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
  env = parent.frame(), min_time = .5, min_iterations = 1, max_iterations = 1e6, check = TRUE) {

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

  results <- list(expression = auto_name(exprs))

  # Run setup code
  if (!is.null(setup)) {
    eval(setup, env)
  }

  # Helper for evaluating with memory profiling
  eval_one <- function(e) {
    f <- tempfile()
    on.exit(unlink(f))
    if (capabilities("profmem")) {
      utils::Rprofmem(f, threshold = 1)
    }

    res <- eval(e, env)
    utils::Rprofmem(NULL)
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

  # Run timing benchmark
  results$time <- list()
  results$gc <- list()

  for (i in seq_len(length(exprs))) {
    with_gcinfo({
      res <- .Call(mark_, exprs[[i]], env, min_time, as.integer(min_iterations), as.integer(max_iterations), tempfile())
    })
    results$time[[i]] <- as_bench_time(res[[1]])
    results$gc[[i]] <- parse_gc(res[[2]])

    # Do an explicit gc, to minimize counting a gc against a prior expression.
    gc()
  }

  tibble::as_tibble(results)
}

summary_cols <- c("rel", "min", "mean", "median", "max", "itr/sec", "mem_alloc", "num_gc")
data_cols <- c("result", "memory", "time", "gc")

# TODO: document return values
#' @export
summary.bench_mark <- function(object, ..., filter_gc = TRUE, relative_within = TRUE) {
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
  object$`itr/sec` <- viapply(times, length) / unclass(object$total_time)

  object$mem_alloc <-
    bench_bytes(
      vdapply(object$memory, function(objectobject) if (is.null(objectobject)) NA else sum(objectobject$bytes, na.rm = TRUE)))

  object$num_gc <- vdapply(num_gc, sum)

  if (isTRUE(relative_within) && length(parameters)) {
    grp <- interaction(object[parameters])
    object$rel <- unsplit(
      lapply(split(object$median, grp), function(x) unclass(x) / unclass(min(x))),
      grp)
  } else {
    object$rel <- unclass(object$median) / unclass(min(object$median))
  }

  bench_mark(
    object[do.call(order, cbind(object[parameters], -object$rel)),
      c("expression", parameters, summary_cols, data_cols)])
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
    return(NULL)
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
  tibble::tibble(
    level0 = lengths(regmatches(x, gregexpr("Garbage collection [^(]+[(](level 0)", x))),
    level1 = lengths(regmatches(x, gregexpr("Garbage collection [^(]+[(](level 1)", x))),
    level2 = lengths(regmatches(x, gregexpr("Garbage collection [^(]+[(](level 2)", x))))
}
