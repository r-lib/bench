suite_cols <- c(
  "suite" = "character",
  "benchmark_time" = "POSIXct",
  "expression" = "character",
  "min" = "numeric",
  "1Q" = "numeric",
  "median" = "numeric",
  "3Q" = "numeric",
  "max" = "numeric",
  "n_itr" = "numeric",
  "n_gc" = "numeric",
  "total_time" = "numeric",
  "mem_alloc" = "numeric")

#' @export
suite <- function(name) {
  options(bench.suite = name)
}

suite_file <- function(name) {
  file.path(tempdir(), paste0("bench-", name, ".tsv"))
}

ISO8601_format <- "%Y-%m-%dT%H:%M:%SZ"

get_current_git_ref <- function() {
  withCallingHandlers(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    warning = function(e) {
      # If we can't get the reference convert the warning to an error
      stop(e)
    }
  )
}

get_current_git_branch <- function() {
  # If we aren't on a branch return NA
  suppressWarnings(
    out <- system2("git", c("describe", "--all", "--exact-match", "HEAD"), stdout = TRUE)
  )
  if (length(out) == 0) {
    out <- NA_character_
  }
  out
}

data_list_cols <- c("memory", "time", "gc", "result")

append_file_to_git_notes <- function(suite, file) {
  withCallingHandlers(
    system2("git", c("notes", "--ref", "benchmarks", "append", "-F", file, "HEAD")),
    warning = function(e) {
      stop(e)
    }
  )
}

write_suite <- function(x, suite) {
  x <- summary(x, filter_gc = FALSE)
  times <- x$time
  x$max <- vdapply(times, max)
  x[["1Q"]] <- vdapply(times, stats::quantile, .25)
  x[["3Q"]] <- vdapply(times, stats::quantile, .75)
  x[["expression"]] <- as.character(x[["expression"]])
  x <- x[!colnames(x) %in% data_list_cols]
  x[colnames(x) != "expression"] <- lapply(x[colnames(x) != "expression"], as.numeric)
  x$suite <- suite
  x$benchmark_time <- as.character(Sys.time(), format = ISO8601_format, tz = "UTC")
  x <- x[names(suite_cols)]

  file <- suite_file(suite)
  on.exit(unlink(file))
  write.table(x, sep = "\t", file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
  append_file_to_git_notes(suite, file)
}

plot_benchmark <- function(x) {
  #p1 <- ggplot(x, aes(x = name)) +
    #geom_point(aes(y = median)) +
    #geom_segment(aes(xend = name, y = `1Q`, yend = `3Q`)) +
    #scale_y_bench_time(name = NULL) +
    #scale_x_discrete(name = NULL) +
    #coord_flip() +
    #labs(title = paste0("Execution time - seconds"))

  p1 <- ggplot(x, aes(x = name)) +
    geom_point(aes(y = median)) +
    #geom_step(aes(y = median), group = 1) +
    geom_segment(aes(xend = name, y = `1Q`, yend = `3Q`)) +
    scale_y_bench_time(name = NULL) +
    scale_x_discrete(name = NULL) +
    coord_flip() +
    labs(title = paste0("Execution time"))

  #p2 <- ggplot(x, aes(x = name, y = mem_alloc)) +
    ##geom_bar(stat = "identity") +
    #geom_point() +
    #scale_y_bench_bytes() +
    #coord_flip() +
    #labs(title = "Memory allocations", x = NULL, y = NULL) +
    #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  #p3 <- ggplot(x, aes(x = name, y = n_gc / total_time)) +
    ##geom_bar(stat = "identity") +
    #geom_point() +
    #coord_flip() +
    #labs(title = "GC / second", x = NULL, y = NULL) +
    #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  #library(patchwork)
  #p1 + p2 + p3 + plot_layout(guides = "collect", widths = c(1, 1/6, 1/6)) + plot_annotation(title = x$expression[[1]])
}

read_git_benchmark_note <- function(ref) {
  read.delim(pipe(glue::glue("git cat-file -p {ref}")), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, col.names = names(suite_cols), header = FALSE)
}

read_git_log <- function() {
  x <- read.delim(pipe("git log --notes=benchmarks --pretty=format:'%H|%P|\"%N\"|%s|%D'"), sep = "|", col.names = c("commit_hash", "parent_hashes", "benchmark_notes", "subject", "ref_names"), header = FALSE, stringsAsFactors = FALSE)

  x$benchmark_notes <- lapply(x$benchmark_notes, read_benchmark_note)
  x$parent_hashes <- strsplit(x$parent_hashes, " ")
  x <- tibble::as_tibble(x)
  tidyr::unnest(x, cols = c(benchmark_notes), keep_empty = TRUE)
}

log_to_commit_graph <- function(log) {
  nodes <- log[c("commit_hash", "subject", "ref_names")]
  edges <- tidyr::unnest(log[c("commit_hash", "parent_hashes")], cols = c(parent_hashes))

  # If we have truncated the log the last edge may reference a node we don't
  # have, so remove it
  if (tail(edges$parent_hashes, n = 1) != tail(nodes$commit_hash, n = 1)) {
    edges <- head(edges, n = -1)
  }

  tidygraph::tbl_graph(nodes = nodes, edges = edges)
}

# TODO: get @thomasp85 to make this nice, something like
# https://gitgraphjs.com/#6, also need to somehow align it with the benchmark
# results.
plot_commit_graph <- function(graph) {
  library(ggraph)
  ggraph(graph, layout = 'dendrogram') +
    geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
    geom_node_point(size = 5) +
    geom_node_label(aes(label = commit_hash))
}

read_benchmark_note <- function(data) {
  if (!nzchar(data)) {
    return (tibble::tibble(suite = character(), expression_time = character(), expression = character(), min = numeric(), "1Q" = numeric(), median = numeric(), "3Q" = numeric(), max = numeric(), n_itr = numeric(), n_gc = numeric(), total_time = numeric(), mem_alloc = numeric()))
  }
  tibble::as_tibble(read.delim(text = data, sep = "\t", stringsAsFactors = FALSE, col.names = names(suite_cols), colClasses = unname(suite_cols), header = FALSE))
}

read_notes <- function() {
  read.delim(pipe("git notes --ref benchmarks show HEAD"), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, col.names = suite_cols, header = FALSE)
}

read_git_notes <- function(ref = "HEAD") {
  withCallingHandlers(
    pipe("git", c("notes", "--ref", "benchmarks", "show", ref), stdout = TRUE),
    warning = function(e) {
      stop(e)
    }
  )
}

plot_suite <- function(name) {
  library(ggplot2)

  x <- read_notes()
  x <- x[x$suite == name, ]
  x$ref <- substr(x$ref, 1, 6)
  x$name <- ifelse(is.na(x$branch), x$ref, x$branch)
  x$name <- factor(x$name, levels = unique(x$name))

  plots <- lapply(split(x, x$expression), plot_benchmark)

  plots[[1]]
  #x$datetime <- as.POSIXct(x$datetime, format = ISO8601_format, tz = "UTC")


  #patchwork::align_plots(p1, p2)

  # + theme(legend.position = "bottom")
}
