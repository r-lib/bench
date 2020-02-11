#' Run continuous benchmarks for a package
#'
#' [cb_run()] runs all the benchmark in a package. [cb_run_one()]
#' runs a single benchmark.
#'
#' Benchmark files are any '.R' files in the `bench/` directory. The results of
#' any `bench::mark()` are automatically recorded in [git
#' notes](https://git-scm.com/docs/git-notes) entries for the current commit.
#'
#' @param path For [cb_run()] a path to a package, or within a package. For [cb_run_one()] the path to the benchmark file to be run.
#' @param env Environment in which to execute the benchmarks.
#' @family cb
#' @export
cb_run <- function(path = ".", env = new.env(parent = globalenv())) {
  path <- find_package_root(path)

  old <- getwd()
  on.exit(setwd(old))
  bench_dir <- file.path(path, "bench")
  setwd(bench_dir)
  for (file in list.files(".", pattern = "[.][Rr]$", full.names = TRUE)) {
    cb_run_one(file, env)
  }
}

#' @rdname cb_run
#' @export
cb_run_one <- function(path, env = new.env(parent = globalenv())) {
  filename <- basename(path)
  options(bench.file = filename)

  lines <- read_lines(path)
  exprs <- parse(text = lines, keep.source = FALSE, encoding = "UTF-8")
  eval(exprs, envir = env)
}

git <- function(...) {
  withCallingHandlers(
    system2("git", list(...)),
    warning = function(e) {
      stop(e)
    }
  )
}

#' Interact with the benchmark notes
#'
#' These functions let you interact with the benchmark notes using your git
#' client.
#'
#' By default the git client does not fetch or push notes from remotes,
#' [cb_fetch()] and [cb_push()] can be used to do this.
#'
#' [cb_remove()] is used to remove the notes for the current commit.
#'
#' @param remote The git remote to use, defaults to 'origin'.
#' @family cb
#' @export
cb_fetch <- function(remote = "origin") {
  git("fetch", remote, "refs/notes/benchmarks:refs/notes/benchmarks")
}

#' @rdname cb_fetch
#' @export
cb_push <- function(remote = "origin") {
  git("push", remote, "refs/notes/benchmarks:refs/notes/benchmarks")
}

#' rdname cb_fetch
#' @export
cb_remove <- function() {
  git("notes", "--ref", "benchmarks", "remove")
}

benchmark_cols <- c(
  "file" = "character",
  "name" = "character",
  "time" = "character",
  "os" = "character",
  "mean" = "numeric",
  "sd" = "numeric",
  "p0" = "numeric",
  "p25" = "numeric",
  "p50" = "numeric",
  "p75" = "numeric",
  "p100" = "numeric",
  "n_itr" = "numeric",
  "n_gc" = "numeric",
  "total_time" = "numeric",
  "mem_alloc" = "numeric")

ISO8601_format <- "%Y-%m-%dT%H:%M:%SZ"

# Writing benchmark results

data_list_cols <- c("memory", "time", "gc", "result")

append_file_to_git_notes <- function(file) {
  git("notes", "--ref", "benchmarks", "append", "-F", file, "HEAD")
}

#' @importFrom utils write.table
cb_write <- function(x, file) {
  x <- summary(x, filter_gc = FALSE)
  times <- x$time
  x <- x[!colnames(x) %in% data_list_cols]
  x[["name"]] <- as.character(x[["expression"]])
  x[["expression"]] <- NULL
  x[colnames(x) != "name"] <- lapply(x[colnames(x) != "name"], as.numeric)

  x[["mean"]] <- vdapply(times, mean)
  x[["sd"]] <- vdapply(times, stats::sd)
  x[["p0"]] <- vdapply(times, stats::quantile, 0)
  x[["p25"]] <- vdapply(times, stats::quantile, .25)
  x[["p50"]] <- vdapply(times, stats::quantile, .5)
  x[["p75"]] <- vdapply(times, stats::quantile, .75)
  x[["p100"]] <- vdapply(times, stats::quantile, 1)
  x[["os"]] <- current_os()

  x$file <- file
  x$time <- as.character(Sys.time(), format = ISO8601_format, tz = "UTC")
  x <- x[names(benchmark_cols)]

  file <- tempfile()
  on.exit(unlink(file))
  write.table(x, sep = "\t", file = file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
  append_file_to_git_notes(file)
}

current_os <- function() {
  os <- Sys.info()[["sysname"]]
  switch(tolower(os),
    "sunos" = "Solaris",
    "darwin" = "macOS",
    "windows" = "Windows",
    "linux" = "linux",
    os
  )
}

#' Read continuous benchmark data from the git log
#'
#' Note if the benchmarks were run on a remote system you may need to fetch the
#' data locally first with `cb_fetch()`.
#'
#' @param path A path to a package or within a package.
#' @param additional_columns A named list of additional columns to include. The
#'   names are the names you want the columsn to have. The values are the
#'   placeholder values used in 'git log'.
#' @importFrom utils read.delim
#' @family cb
#' @examples
#' \dontrun{
#' cb_read(additional_columns=c("tree_hash" = "%T", "author_email" = "%ae"))
#' }
#' @export
cb_read <- function(path = ".", additional_columns = NULL) {

  path <- find_package_root(path)

  old <- getwd()
  on.exit(setwd(old))
  setwd(path)

  additional_placeholders <- get_placeholders(additional_columns)
  cmd <- glue::glue("git log --notes=benchmarks --pretty=format:'%H|%h|%P|\"%N\"|%s|%D{additional_placeholders}'")

  x <- read.delim(
    pipe(cmd),
    sep = "|",
    col.names = c(
      "commit_hash",
      "abbrev_commit_hash",
      "parent_hashes",
      "benchmarks",
      "subject",
      "ref_names",
      names(additional_columns)
    ),
    header = FALSE,
    stringsAsFactors = FALSE
  )

  # read the benchmark notes into a df list-cols
  x$benchmarks <- lapply(x$benchmarks, cb_read_benchmark)

  # Split the parents into character list-cols
  x$parent_hashes <- strsplit(x$parent_hashes, " ")

  x$ref_names <- parse_ref_names(x$ref_names)

  tibble::as_tibble(x)
}

get_placeholders <- function(additional_columns) {
  if (length(additional_columns) == 0) {
    return("")
  }
  if (!rlang::is_named(additional_columns)) {
    rlang::abort("`additional_columns` must all be named")
  }
  additional_placeholders <- glue::glue_collapse(unname(additional_columns), "|")
  additional_placeholders <- glue::glue('|{additional_placeholders}')

  additional_placeholders
}

parse_ref_names <- function(x) {
  # TODO: split these into tags and branch types?
  # TODO: special case HEAD?

  strsplit(x, ", ")
}

#tidyr::unnest(x, cols = c(benchmark_notes), keep_empty = TRUE)

cb_read_benchmark <- function(data) {
  if (!nzchar(data)) {
    return (tibble::tibble())
  }
  x <- tibble::as_tibble(read.delim(text = data, sep = "\t", stringsAsFactors = FALSE, col.names = names(benchmark_cols), colClasses = benchmark_cols, header = FALSE, check.names = FALSE))
  x$time <- as.POSIXct(strptime(x$time, format = ISO8601_format, tz = "UTC"))
  x
}

## Plotting the benchmarks

utils::globalVariables(c("benchmarks", "pretty_name", "geom_point", "p0", "p50", "p100", "p25", "p75", "sd"))
cb_plot <- function(x) {
  if (!(requireNamespace("ggplot2") && requireNamespace("tidyr"))) {
    stop("`ggplot2` and `tidyr` must be installed to use `cb_plot()`.", call. = FALSE)
  }

  x <- tidyr::unnest(x, benchmarks)

  x$pretty_name <- get_pretty_names(x)
  x$pretty_name <- factor(x$pretty_name, levels = rev(unique(x$pretty_name)))

  plots <- lapply(split(x, x$name), cb_plot_one)

  plots


  #patchwork::align_plots(p1, p2)

  # + theme(legend.position = "bottom")
}

get_pretty_names <- function(x) {
  out <- character(NROW(x))
  for (i in seq_len(NROW(x))) {
    if (length(x$ref_names[[i]]) > 0) {
      out[[i]] <- x$ref_names[[i]][[1]]
    } else {
      out[[i]] <- x$abbrev_commit_hash[[i]]
    }
  }
  out
}

cb_plot_one <- function(x) {
  aes <- ggplot2::aes
  geom_point <- ggplot2::geom_point

  p1 <- ggplot2::ggplot(x, aes(x = pretty_name)) +
    geom_point(aes(y = p0), color = "red") +
    geom_point(aes(y = p50)) +
    geom_point(aes(y = p100), color = "blue") +
    ggplot2::geom_segment(aes(xend = pretty_name, y = p25, yend = p75)) +
    scale_y_bench_time(name = NULL) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = paste0("Execution time"), subtitle = x$name[[1]]) +
    ggplot2::theme_minimal()

  #p1 <- ggplot(x, aes(x = name)) +
  #geom_point(aes(y = median)) +
  ##geom_step(aes(y = median), group = 1) +
  #geom_segment(aes(xend = name, y = `1Q`, yend = `3Q`)) +
  #scale_y_bench_time(name = NULL) +
  #scale_x_discrete(name = NULL) +
  #coord_flip() +
  #labs(title = paste0("Execution time"))

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
  #p1 + p2 + p3 + plot_layout(guides = "collect", widths = c(1, 1/6, 1/6)) + plot_annotation(title = x$name[[1]])
}
# 
# ## Plotting the commit graph
# 
# #' @importFrom utils tail head
# log_to_commit_graph <- function(log) {
#   nodes <- log[c("commit_hash", "subject", "ref_names")]
#   edges <- tidyr::unnest(log[c("commit_hash", "parent_hashes")], cols = c(parent_hashes))
# 
#   # If we have truncated the log the last edge may reference a node we don't
#   # have, so remove it
#   if (tail(edges$parent_hashes, n = 1) != tail(nodes$commit_hash, n = 1)) {
#     edges <- head(edges, n = -1)
#   }
# 
#   tidygraph::tbl_graph(nodes = nodes, edges = edges)
# }
# 
# # TODO: get @thomasp85 to make this nice, something like
# # https://gitgraphjs.com/#6, also need to somehow align it with the benchmark
# # results.
# plot_commit_graph <- function(graph) {
#   library(ggraph)
#   ggraph(graph, layout = 'dendrogram') +
#     geom_edge_link(arrow = arrow(length = unit(4, 'mm'))) +
#     geom_node_point(size = 5) +
#     geom_node_label(aes(label = commit_hash))
# }
