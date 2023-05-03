#' @useDynLib bench, .registration = TRUE
NULL

#' Benchmark a series of functions
#'
#' Benchmark a list of quoted expressions. Each expression will always run at
#' least twice, once to measure the memory allocation and store results and one
#' or more times to measure timing.
#'
#' @param ... Expressions to benchmark, if named the `expression` column will
#'   be the name, otherwise it will be the deparsed expression.
#' @param min_time The minimum number of seconds to run each expression, set to
#'   `Inf` to always run `max_iterations` times instead.
#' @param iterations If not `NULL`, the default, run each expression for
#'   exactly this number of iterations. This overrides both `min_iterations`
#'   and `max_iterations`.
#' @param exprs A list of quoted expressions. If supplied overrides expressions
#'   defined in `...`.
#' @param min_iterations Each expression will be evaluated a minimum of `min_iterations` times.
#' @param max_iterations Each expression will be evaluated a maximum of `max_iterations` times.
#' @param check Check if results are consistent. If `TRUE`, checking is done
#'   with [all.equal()], if `FALSE` checking is disabled and results are not
#'   stored. If `check` is a function that function will be called with each
#'   pair of results to determine consistency.
#' @param memory If `TRUE` (the default when R is compiled with memory
#'   profiling), track memory allocations using [utils::Rprofmem()]. If `FALSE`
#'   disable memory tracking.
#' @param env The environment which to evaluate the expressions
#' @inheritParams summary.bench_mark
#' @inherit summary.bench_mark return
#' @aliases bench_mark
#' @seealso [press()] to run benchmarks across a grid of parameters.
#' @examples
#' dat <- data.frame(x = runif(100, 1, 1000), y=runif(10, 1, 1000))
#' mark(
#'   min_time = .1,
#'
#'   dat[dat$x > 500, ],
#'   dat[which(dat$x > 500), ],
#'   subset(dat, x > 500))
#' @export
mark <- function(..., min_time = .5, iterations = NULL, min_iterations = 1,
                 max_iterations = 10000, check = TRUE, memory = capabilities("profmem"), filter_gc = TRUE,
                 relative = FALSE, time_unit = NULL, exprs = NULL, env = parent.frame()) {

  if (!is.null(iterations)) {
    min_iterations <- iterations
    max_iterations <- iterations
  }

  if (isTRUE(check)) {
    check_fun <- all.equal
  } else if (is.function(check)) {
    check_fun <- check
    check <- TRUE
  } else {
    check <- FALSE
  }

  if (is.null(exprs)) {
    exprs <- dots(...)
  }

  n_exprs <- length(exprs)

  results <- list(expression = new_bench_expr(exprs), time = vector("list", n_exprs), gc = vector("list", n_exprs), memory = vector("list", n_exprs), result = vector("list", n_exprs))

  # Helper for evaluating with memory profiling
  eval_one <- function(e, profile_memory) {
    f <- tempfile()
    on.exit(unlink(f))
    if (profile_memory) {
      utils::Rprofmem(f, threshold = 1)
    }

    res <- eval(e, env)
    if (profile_memory) {
      utils::Rprofmem(NULL)
    }
    list(result = res, memory = parse_allocations(f))
  }

  # We only want to evaluate these first runs if we need to check memory or results.
  if (memory || check) {
    # Run allocation benchmark and check results
    for (i in seq_len(length(exprs))) {
      res <- eval_one(exprs[[i]], memory)
      if (check) {
        if (is.null(res$result)) {
          results$result[i] <- list(res$result)
        } else {
          results$result[[i]] <- res$result
        }
      }
      if (memory) {
        results$memory[[i]] <- res$memory
      }

      if (check && i > 1) {
        comp <- check_fun(results$result[[1]], results$result[[i]])
        if (!isTRUE(comp)) {
          expressions <- as.character(results$expression)

          stop(glue::glue("
              Each result must equal the first result:
              `{first}` does not equal `{current}`
              ",
              first = expressions[[1]],
              current = expressions[[i]]
              ),
            call. = FALSE
          )
        }
      }
    }
  }

  for (i in seq_len(length(exprs))) {
    error <- NULL
    gc_msg <- with_gcinfo({
      tryCatch(error = function(e) { e$call <- NULL; error <<- e},
      res <- .Call(mark_, exprs[[i]], env, min_time, as.integer(min_iterations), as.integer(max_iterations), TRUE)
      )
    })
    if (!is.null(error)) {
      stop(error)
    }

    results$time[[i]] <- as_bench_time(res)
    results$gc[[i]] <- parse_gc(gc_msg)
  }

  out <- summary(bench_mark(tibble::as_tibble(results, .name_repair = "minimal")),
          filter_gc = filter_gc, relative = relative, time_unit = time_unit)

  out
}

bench_mark <- function(x) {
  class(x) <- unique(c("bench_mark", class(x)))
  x
}

#' Coerce to a bench mark object Bench mark objects
#'
#' This is typically needed only if you are performing additional manipulations
#' after calling [bench::mark()].
#' @param x Object to be coerced
#' @export
as_bench_mark <- function(x) {
  bench_mark(tibble::as_tibble(x))
}

summary_cols <- c("min", "median", "itr/sec", "mem_alloc", "gc/sec")
data_cols <- c("n_itr", "n_gc", "total_time", "result", "memory", "time", "gc")
time_cols <- c("min", "median", "total_time")

#' Summarize [bench::mark] results.
#'
#' @param object [bench_mark] object to summarize.
#' @param filter_gc If `TRUE` remove iterations that contained at least one
#'   garbage collection before summarizing. If `TRUE` but an expression had
#'   a garbage collection in every iteration, filtering is disabled, with a warning.
#' @param relative If `TRUE` all summaries are computed relative to the minimum
#'   execution time rather than absolute time.
#' @param time_unit If `NULL` the times are reported in a human readable
#'   fashion depending on each value. If one of 'ns', 'us', 'ms', 's', 'm', 'h',
#'   'd', 'w' the time units are instead expressed as nanoseconds, microseconds,
#'   milliseconds, seconds, hours, minutes, days or weeks respectively.
#' @param ... Additional arguments ignored.
#' @details
#'   If `filter_gc == TRUE` (the default) runs that contain a garbage
#'   collection will be removed before summarizing. This is most useful for fast
#'   expressions when the majority of runs do not contain a gc. Call
#'   `summary(filter_gc = FALSE)` if you would like to compute summaries _with_
#'   these times, such as expressions with lots of allocations when all or most
#'   runs contain a gc.
#' @return A [tibble][tibble::tibble] with the additional summary columns.
#'   The following summary columns are computed
#'   - `expression` - `bench_expr` The deparsed expression that was evaluated
#'     (or its name if one was provided).
#'   - `min` - `bench_time` The minimum execution time.
#'   - `median` - `bench_time` The sample median of execution time.
#'   - `itr/sec` - `double` The estimated number of executions performed per
#'   second.
#'   - `mem_alloc` - `bench_bytes` Total amount of memory allocated by R while
#'     running the expression. Memory allocated *outside* the R heap, e.g. by
#'     `malloc()` or `new` directly is *not* tracked, take care to avoid
#'     misinterpreting the results if running code that may do this.
#'   - `gc/sec` - `double` The number of garbage collections per second.
#'   - `n_itr` - `integer` Total number of iterations after filtering
#'      garbage collections (if `filter_gc == TRUE`).
#'   - `n_gc` - `double` Total number of garbage collections performed over all
#'   iterations. This is a psudo-measure of the pressure on the garbage collector, if
#'   it varies greatly between to alternatives generally the one with fewer
#'   collections will cause fewer allocation in real usage.
#'   - `total_time` - `bench_time` The total time to perform the benchmarks.
#'   - `result` - `list` A list column of the object(s) returned by the
#'     evaluated expression(s).
#'   - `memory` - `list` A list column with results from [Rprofmem()].
#'   - `time` - `list` A list column of `bench_time` vectors for each evaluated
#'     expression.
#'   - `gc` - `list` A list column with tibbles containing the level of
#'     garbage collection (0-2, columns) for each iteration (rows).
#' @examples
#' dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))
#'
#' # `bench::mark()` implicitly calls summary() automatically
#' results <- bench::mark(
#'   dat[dat$x > 500, ],
#'   dat[which(dat$x > 500), ],
#'   subset(dat, x > 500))
#'
#' # However you can also do so explicitly to filter gc differently.
#' summary(results, filter_gc = FALSE)
#'
#' # Or output relative times
#' summary(results, relative = TRUE)
#' @export
summary.bench_mark <- function(object, filter_gc = TRUE, relative = FALSE, time_unit = NULL, ...) {
  nms <- colnames(object)
  parameters <- setdiff(nms, c("expression", summary_cols, data_cols))

  num_gc <- lapply(object$gc,
    function(x) {
      res <- rowSums(x)
      if (length(res) == 0) {
        res <- rep(0, length(x))
      }
      res
    }
  )
  if (isTRUE(filter_gc)) {
    no_gc <- lapply(num_gc, `==`, 0)
    times <- Map(`[`, object$time, no_gc)
  } else {
    times <- object$time
  }

  if (filter_gc && any(lengths(times) == 0)) {
    times <- object$time
    warning(call. = FALSE,
        "Some expressions had a GC in every iteration; so filtering is disabled."
    )
  }

  object$min <- new_bench_time(vdapply(times, min))
  object$median <- new_bench_time(vdapply(times, stats::median))
  object$max <- new_bench_time(vdapply(times, max))
  object$total_time <- new_bench_time(vdapply(times, sum))

  object$n_itr <- viapply(times, length)
  object$`itr/sec` <-  as.numeric(object$n_itr / object$total_time)

  object$n_gc <- vdapply(num_gc, sum)
  object$`gc/sec` <-  as.numeric(object$n_gc / object$total_time)

  object$mem_alloc <-
    bench_bytes(
      vdapply(object$memory, function(x) if (is.null(x)) NA else sum(x$bytes, na.rm = TRUE)))

  if (isTRUE(relative)) {
    object[summary_cols] <- lapply(object[summary_cols], function(x) as.numeric(x / min(x)))
  }

  if (!is.null(time_unit)) {
    time_unit <- match.arg(time_unit, names(time_units()))
    object[time_cols] <- lapply(object[time_cols], function(x) as.numeric(x / time_units()[time_unit]))
  }

  to_keep <- intersect(c("expression", parameters, summary_cols, data_cols), names(object))
  bench_mark(object[to_keep])
}

#' @export
`[.bench_mark` <- function(x, i, j, ...) {
  bench_mark(NextMethod("["))
}

parse_allocations <- function(filename) {

  if (!file.exists(filename)) {
    empty_Rprofmem <- structure(
      list(what = character(),
        bytes = integer(),
        trace = list()),
      class = c("Rprofmem",
        "data.frame"))

    return(empty_Rprofmem)
  }

  # TODO: remove this dependency / simplify parsing
  tryCatch(
    profmem::readRprofmem(filename),
    error = function(e) {
      stop("Memory profiling failed.\n  If you are benchmarking parallel code you must set `memory = FALSE`.", call. = FALSE)
    }
  )
}

#nocov start

#' Custom printing function for `bench_mark` objects in knitr documents
#'
#' By default, data columns (`result`, `memory`, `time`, `gc`) are omitted when
#' printing in knitr. If you would like to include these columns, set the knitr
#' chunk option `bench.all_columns = TRUE`.
#'
#' @details
#' You can set `bench.all_columns = TRUE` to show all columns of the bench mark
#' object.
#'
#'     ```{r, bench.all_columns = TRUE}
#'     bench::mark(
#'       subset(mtcars, cyl == 3),
#'       mtcars[mtcars$cyl == 3, ]
#'     )
#'     ```
#'
#' @inheritParams knitr::knit_print
#'
#' @param options A list of knitr chunk options set in the currently evaluated
#'   chunk.
knit_print.bench_mark <- function(x, ..., options) {
  if (!isTRUE(options$bench.all_columns)) {
    x <- x[!colnames(x) %in% data_cols]
  }
  NextMethod()
}

#nocov end

parse_gc <- function(x) {
  # \x1E is Record Separator
  x <- strsplit(paste0(x, collapse = ""), "\x1E")[[1]]
  tibble::as_tibble(.Call(parse_gc_, x))
}

utils::globalVariables(c("time", "gc"))

unnest.bench_mark <- function(data, ...) {
  if (inherits(data[["expression"]], "bench_expr")) {
    data[["expression"]] <- as.character(data[["expression"]])
  }

  # suppressWarnings to avoid 'elements may not preserve their attributes'
  # warnings from dplyr::collapse
  if (tidyr_new_interface()) {
    data <- suppressWarnings(NextMethod(.Generic, data, ...))
  } else {
    data <- suppressWarnings(NextMethod(.Generic, data, time, gc, .drop = FALSE))
  }

  # Add bench_time class back to the time column
  data$time <- as_bench_time(data$time)

  # Add a gc column, a factor with the highest gc performed for each expression.
  data$gc <-
    dplyr::case_when(
      data$level2 > 0 ~ "level2",
      data$level1 > 0 ~ "level1",
      data$level0 > 0 ~ "level0",
      TRUE ~ "none")
  data$gc <- factor(data$gc, c("none", "level0", "level1", "level2"))

  data
}

#' @export
rbind.bench_mark <- function(..., deparse.level = 1) {
  args <- list(...)
  desc <- unlist(lapply(args, function(x) as.character(x$expression)))
  res <- rbind.data.frame(...)
  attr(res$expression, "description") <- desc
  res
}

filter.bench_mark <- function(.data, ...) {
  dots <- rlang::quos(...)
  idx <- Reduce(`&`, lapply(dots, rlang::eval_tidy, data = .data))
  .data[idx, ]
}

group_by.bench_mark <- function(.data, ...) {
  bench_mark(NextMethod())
}
