suite_cols <- c("ref", "branch", "suite", "commit_time", "benchmark_time", "expression", "min", "1Q", "median", "3Q", "max", "n_itr", "n_gc", "total_time", "mem_alloc")

#' @export
suite <- function(name) {
  options(bench.suite = name)
  file <- suite_file(name)
  if (!file.exists(file)) {
    write.table(t(suite_cols), sep = "\t", file = suite_file(name), row.names = FALSE, col.names = FALSE)
  }
}

suite_file <- function(name) {
  paste0("bench-", name, ".tsv")
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

get_current_git_creation_time <- function() {
  suppressWarnings(
    out <- system2("git", c("log", "--format='%cI'", "-n", "1", "HEAD"), stdout = TRUE)
  )
  if (length(out) == 0) {
    out <- NA_character_
  }
  out
}

data_list_cols <- c("memory", "time", "gc", "result")

write_suite <- function(x, name) {
  x <- summary(x, filter_gc = FALSE)
  times <- x$time
  x$max <- vdapply(times, max)
  x[["1Q"]] <- vdapply(times, stats::quantile, .25)
  x[["3Q"]] <- vdapply(times, stats::quantile, .75)
  x[["expression"]] <- as.character(x[["expression"]])
  x <- x[!colnames(x) %in% data_list_cols]
  x[colnames(x) != "expression"] <- lapply(x[colnames(x) != "expression"], as.numeric)
  x$suite <- name
  x$ref <- get_current_git_ref()
  x$branch <- get_current_git_branch()
  x$commit_time <- get_current_git_creation_time()
  x$benchmark_time <- as.character(Sys.time(), format = ISO8601_format, tz = "UTC")
  x <- x[suite_cols]
  write.table(x, sep = "\t", file = suite_file(name), row.names = FALSE, append = TRUE, col.names = FALSE)
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

plot_suite <- function(name) {
  library(ggplot2)

  x <- read.delim(suite_file(name), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  x$ref <- substr(x$ref, 1, 6)
  x$name <- ifelse(is.na(x$branch), x$ref, x$branch)
  x$name <- factor(x$name, levels = unique(x$name))

  plots <- lapply(split(x, x$expression), plot_benchmark)

  plots[[1]]
  #x$datetime <- as.POSIXct(x$datetime, format = ISO8601_format, tz = "UTC")


  #patchwork::align_plots(p1, p2)

  # + theme(legend.position = "bottom")
}

library(ggplot2)
library(ggtext)
labels <- c(
  setosa = "<a href='https://en.wikipedia.org/wiki/Iris_setosa'>*I. setosa*</a>",
  virginica = "<a href='https://en.wikipedia.org/wiki/Iris_virginica'>*I. virginica*</a>",
  setosa = "<a href='https://en.wikipedia.org/wiki/Iris_versicolor'>*I. versicolor*</a>"
)

ggplot(iris, aes(Species, Sepal.Width)) +
  geom_boxplot() +
  scale_x_discrete(
    name = NULL,
    labels = labels
  ) +
  theme(
    axis.text.x = element_markdown(color = "black", size = 11)
  )
