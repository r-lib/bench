#' @useDynLib bench, .registration = TRUE
NULL

#' Benchmark a series of functions
#'
#' Benchmark a list of quoted expressions. Each expression will always run at
#' least twice, once to measure the memory allocation and store results and one
#' or more times to measure timing.
#'
#' @param ... Expressions to benchmark
#' @param setup code to evaluated before _each_ benchmark group, this code will
#'   be reevaluated for each row in parameters.
#' @param parameters Variable values to assign, all values will be enumerated
#'   by `expand.grid()`.
#' @param env The environment which to evaluate the expressions
#' @param min_time The minimum number of seconds to run each expression, set to
#'   `Inf` to always run `max_iterations` times instead.
#' @param min_iterations Each expression will be evaluated a minimum of `min_iterations` times.
#' @param max_iterations Each expression will be evaluated a maximum of `max_iterations` times.
#' @param check Check if results are consistent. If `TRUE`, checking is done
#'   with [all.equal()], if `FALSE` checking is disabled. If `check` is a
#'   function that function will be called with each pair of results to
#'   determine consistency.
#' @inherit summary.bench_mark return
#' @aliases bench_mark
#' @examples
#' mark(
#'   setup = dat <- data.frame(x = runif(num_x, 1, 1000), y=runif(num_y, 1, 1000)),
#'   parameters = list(num_x = c(1000, 10000), num_y = c(1000, 10000)),
#'   min_time = .1,
#'
#'   dat[dat$x > 500, ],
#'   dat[which(dat$x > 500), ],
#'   subset(dat, x > 500))
#' @export
mark <- function(..., setup = NULL, parameters = list(),
  env = parent.frame(), min_time = .5, min_iterations = 1, max_iterations = 10000, check = TRUE) {

  # Only use expand.grid if not already a data.frame
  is_simple_list <- is.list(parameters) && !is.object(parameters)
  if (is_simple_list) {
    parameters <- expand.grid(parameters)
  }

  setup <- substitute(setup)

  if (nrow(parameters) == 0) {
    e <- new.env(parent = env)
    res <- mark_internal(
      ...,
      setup = setup,
      env = e,
      min_time = min_time,
      min_iterations = min_iterations,
      max_iterations = max_iterations,
      check = check)

  } else {
    out <- list()
    p_out <- format(tibble::as_tibble(parameters), n = Inf)

    # Output a status message
    message("Running benchmark with:\n",
      paste0(p_out[[2]], collapse = "\n"), sep = "")
    for (i in seq_len(nrow(parameters))) {

      # Assign parameters in the execution environment
      e <- new.env(parent = env)
      for (j in seq_along(parameters)) {
        var <- names(parameters)[[j]]
        value <- parameters[i, j]
        assign(var, value, envir = e)
      }

      message(p_out[[i + 3]])
      out[[i]] <- mark_internal(
        ...,
        setup = setup,
        env = e,
        min_time = min_time,
        min_iterations = min_iterations,
        max_iterations = max_iterations,
        check = check)

      # Add parameters to the output result
      for (j in seq_along(parameters)) {
        var <- names(parameters)[[j]]
        value <- parameters[i, j]
        out[[i]][[var]] <- value
      }
    }
    res <- do.call(rbind, out)
  }

  summary(bench_mark(res))
}

bench_mark <- function(x) {
  class(x) <- unique(c("bench_mark", class(x)))
  x
}

mark_internal <- function(..., setup, env, min_time, min_iterations, max_iterations, check) {

  if (isTRUE(check)) {
    check_fun <- all.equal
  } else if (is.function(check)) {
    check_fun <- check
    check <- TRUE
  } else {
    check <- FALSE
  }

  exprs <- dots(...)

  results <- list(expression = auto_name(exprs), result = list(), memory = list(), time = list(), gc = list())

  # Run setup code
  if (!is.null(setup)) {
    eval(setup, env)
  }

  # Helper for evaluating with memory profiling
  eval_one <- function(e) {
    f <- tempfile()
    on.exit(unlink(f))
    can_profile_memory <- capabilities("profmem")
    if (can_profile_memory) {
      utils::Rprofmem(f, threshold = 1)
    }

    res <- eval(e, env)
    if (can_profile_memory) {
      utils::Rprofmem(NULL)
    }
    list(result = res, memory = parse_allocations(f))
  }

  # Run allocation benchmark and check results
  for (i in seq_len(length(exprs))) {
    res <- eval_one(exprs[[i]])
    results$result[[i]] <- res$result
    results$memory[[i]] <- res$memory

    if (isTRUE(check) && i > 1) {
      comp <- check_fun(results$result[[1]], results$result[[i]])
      if (!isTRUE(comp)) {
        stop(glue::glue("
            All results must equal the first result:
              `{first}` does not equal `{current}`
            ",
            first = deparse(exprs[[1]]),
            current = deparse(exprs[[2]])),
          call. = FALSE)
      }
    }
  }

  for (i in seq_len(length(exprs))) {
    # Do an explicit gc, to minimize counting a gc against a prior expression.
    gc()

    gc_msg <- with_gcinfo({
      res <- .Call(mark_, exprs[[i]], env, min_time, as.integer(min_iterations), as.integer(max_iterations))
    })
    results$time[[i]] <- as_bench_time(res)
    results$gc[[i]] <- parse_gc(gc_msg)
  }

  tibble::as_tibble(results)
}

summary_cols <- c("min", "mean", "median", "max", "itr/sec", "mem_alloc", "total_time", "n_itr", "n_gc")
data_cols <- c("result", "memory", "time", "gc")

#' Summarize [bench::mark] results.
#'
#' @param object [bench_mark] object to summarize.
#' @param filter_gc If `TRUE` filter iterations that contained at least one
#'   garbage collection before summarizing.
#' @param relative If `TRUE` all summaries are computed relative to the minimum
#'   execution time rather than absolute time.
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
#'   - `min` - `bench_time` The minimum execution time.
#'   - `mean` - `bench_time` The arithmetic mean of execution time
#'   - `median` - `bench_time` The sample median of execution time.
#'   - `max` - `bench_time` The maximum execution time.
#'   - `mem_alloc` - `bench_bytes` Total amount of memory allocated by running the expression.
#'   - `itr/sec` - `integer` The estimated number of executions performed per second.
#'   - `n_itr` - `integer` Total number of iterations after filtering
#'      garbage collections (if `filter_gc == TRUE`).
#'   - `n_gc` - `integer` Total number of garbage collections performed over all runs.
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
summary.bench_mark <- function(object, filter_gc = TRUE, relative = FALSE, ...) {
  nms <- colnames(object)
  parameters <- setdiff(nms, c("expression", summary_cols, data_cols))

  num_gc <- lapply(object$gc, function(x) rowSums(x))
  if (isTRUE(filter_gc)) {
    no_gc <- lapply(num_gc, `==`, 0)
    times <- Map(`[`, object$time, no_gc)
  } else {
    times <- object$time
  }

  object$mean <- new_bench_time(vdapply(times, mean))
  object$min <- new_bench_time(vdapply(times, min))
  object$median <- new_bench_time(vdapply(times, stats::median))
  object$max <- new_bench_time(vdapply(times, max))
  object$total_time <- new_bench_time(vdapply(times, sum))
  object$n_itr <- viapply(times, length)
  object$`itr/sec` <-  as.numeric(object$n_itr / object$total_time)

  object$mem_alloc <-
    bench_bytes(
      vdapply(object$memory, function(x) if (is.null(x)) NA else sum(x$bytes, na.rm = TRUE)))

  object$n_gc <- vdapply(num_gc, sum)

  if (isTRUE(relative)) {
    object[summary_cols] <- lapply(object[summary_cols], function(x) as.numeric(x / min(x)))
  }

  bench_mark(object[c("expression", parameters, summary_cols, data_cols)])
}

#' @export
`[.bench_mark` <- function(x, i, j, ...) {
  bench_mark(NextMethod("["))
}

#' @export
`[[.bench_mark` <- function(x, i, ...) {
  bench_mark(NextMethod("[["))
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
  profmem::readRprofmem(filename)
}

auto_name <- function(exprs) {
  nms <- names(exprs)

  if (is.null(nms)) {
    nms <- rep("", length(exprs))
  }
  is_missing <- nms == ""
  nms[is_missing] <- vapply(exprs[is_missing], deparse_trunc, character(1))

  nms
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

#' Custom printing function for bench_mark objects in knitr documents
#'
#' By default data columns ('result', 'memory', 'time', 'gc') are omitted when
#' printing in knitr. If you would like to include these columns set the knitr
#' chunk option 'bench.all_columns = TRUE'.
#' @param options A list of knitr chunk options set in the currently evaluated chunk.
#' @inheritParams knitr::knit_print
#' @details
#' You can set `bench.all_columns = TRUE` to show all columns of the bench mark
#' object.
#'
#'     ```{r bench.all_columns = TRUE}
#'     bench::mark(
#'       subset(mtcars, cyl == 3),
#'       mtcars[mtcars$cyl == 3, ])
#'     ```
#' @export
knit_print.bench_mark <- function(x, ..., options) {
  if (isTRUE(options$bench.all_columns)) {
    print(x)
  } else {
    print(x[!colnames(x) %in% data_cols])
  }
}

parse_gc <- function(x) {
  # \x1E is Record Seperator 
  x <- strsplit(glue::glue_collapse(x, ""), "\x1E")[[1]]
  tibble::as_tibble(.Call(parse_gc_, x))
}

#' @export
`[.bench_mark` <- function(x, ...) {
  bench_mark(NextMethod())
}

unnest.bench_mark <- function(data, ...) {
  # remove columns which don't make sense to unnest
  data[c("result", "memory")] <- list(NULL)

  # suppressWarnings to avoid 'elements may not preserve their attributes'
  # warnings from dplyr::collapse
  data <- suppressWarnings(NextMethod(.Generic))

  # Add bench_time class back to the time column
  data$time <- bench_time(data$time)

  data
}
