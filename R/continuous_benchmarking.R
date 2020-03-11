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

git <- function(..., stdout = NULL) {
  withCallingHandlers(
    system2("git", list(...), stdout = stdout),
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
#' [cb_merge()] is used to merge local and remote notes.
#'
#' [cb_remove()] is used to remove the notes for the current commit.
#'
#' @param remote The git remote to use, defaults to 'origin'.
#' @family cb
#' @export
cb_fetch <- function(remote = "origin") {
  git("fetch", remote, "refs/notes/benchmarks:refs/notes/benchmarks")
}

git_current_commit <- function() {
  git("rev-parse", "HEAD", stdout = TRUE)
}

#' @rdname cb_fetch
#' @export
cb_merge <- function(remote = "origin") {
  old <- git_current_commit()
  git("checkout", "refs/notes/benchmarks")
  git("pull", remote, "refs/notes/benchmarks")
  git("checkout", old)
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
  x[["nodename"]] <- Sys.info()[["nodename"]]

  load_averages <- bench::bench_load_average()
  x[["load_1_min"]] <- load_average[["load_1_min"]]
  x[["load_5_min"]] <- load_average[["load_5_min"]]
  x[["load_15_min"]] <- load_average[["load_15_min"]]

  x$file <- file
  x$time <- as.character(Sys.time(), format = ISO8601_format, tz = "UTC")
  x <- x[names(benchmark_cols)]

  file <- tempfile()
  on.exit(unlink(file))
  jsonlite::stream_out(x, file(file), verbose = FALSE)
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
  cmd <- glue::glue("git log --notes=benchmarks --pretty=format:\"%H|%h|%P|'%N'|%s|%D{additional_placeholders}\"")

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
    stringsAsFactors = FALSE,
    quote = "'"
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
  x <- tibble::as_tibble(jsonlite::stream_in(textConnection(data), verbose = FALSE))
  x$time <- as.POSIXct(strptime(x$time, format = ISO8601_format, tz = "UTC"))
  x
}

## Plotting the benchmarks

utils::globalVariables(c("benchmarks", "pretty_name", "name", "p0", "p50", "p25", "p75", "sd"))

#' Plot the execution time for continuous benchmarks
#'
#' @param x Continuous benchmark data, as obtained from [cb_read()].
#' @param n The number of previous commits with benchmarks you want to display.
#' @importFrom utils head
#' @export
cb_plot_time <- function(x, n = 25) {
  if (!(requireNamespace("ggplot2") && requireNamespace("tidyr"))) {
    stop("`ggplot2` and `tidyr` must be installed to use `cb_plot()`.", call. = FALSE)
  }

  has_benchmarks <- viapply(x$benchmarks, NROW) > 0
  x <- head(x[has_benchmarks, ], n)
  x <- tidyr::unnest(x, benchmarks)

  x$pretty_name <- get_pretty_names(x)
  x$pretty_name <- factor(x$pretty_name, levels = rev(unique(x$pretty_name)))

  aes <- ggplot2::aes

  ggplot2::ggplot(x, aes(x = pretty_name, y = p50)) +
    ggplot2::geom_ribbon(aes(group = name, ymin = p25, ymax = p75), alpha = 1/2) +
    ggplot2::geom_line(aes(group = name)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(name = NULL) +
    scale_y_bench_time(name = NULL, base = NULL) +
    ggplot2::facet_wrap(ggplot2::vars(file, name), scales = "free_x")
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
