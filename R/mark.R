#' @useDynLib bench, .registration = TRUE
NULL

#' Benchmark a series of functions
#'
#' Benchmark a list of quoted expressions. Each expression will always run at
#' least twice, once to measure the memory allocation and store results and one
#' or more times to measure timing.
#'
#' @param ... Expressions to benchmark
#' @param exprs A list of quoted expressions to benchmark
#' @param setup code to evaluated before _each_ benchmark group, this code will
#'   be reevaluated for each row in parameters.
#' @param parameters Variable values to assign, all values will be enumerated
#'   by `expand.grid()`.
#' @param env The environment which to evaluate the expressions
#' @param min_time The minimum number of seconds to run each expression, set to
#'   `0` to disable and always run `num_iterations` times instead.
#' @param num_iterations Expressions will be a maximum of `num_iterations` times.
#' @param check_results Should results be checked with [testthat::expect_equal]
#'   for consistency?
#' @export
#' @examples
#' mark(list(quote(1 + 1), quote(4 - 2)))
#' dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))
#' mark(
#'   dat[dat$x > 500, ],
#'   dat[which(dat$x > 500), ],
#'   subset(dat, x > 500))

mark <- function(..., exprs = NULL, setup = NULL, parameters = list(),
  env = parent.frame(), min_time = .5, num_iterations = 1e6,
  check_results = TRUE) {

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
      exprs = exprs,
      setup = setup,
      env = e,
      min_time = min_time,
      num_iterations = num_iterations,
      check_results = check_results)

  } else {
    out <- list()
    p_out <- format(tibble::as_tibble(parameters), n = Inf)

    # Output a status message
    message(paste0(p_out[[2]], collapse = "\n"))
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
        exprs = exprs,
        setup = setup,
        env = e,
        min_time = min_time,
        num_iterations = num_iterations,
        check_results = check_results)

      # Add parameters to the output result
      for (j in seq_along(parameters)) {
        var <- names(parameters)[[j]]
        value <- parameters[i, j]
        out[[i]][[var]] <- value
      }
    }
    res <- dplyr::bind_rows(out)
  }

  res <- res[c("name", names(parameters), "relative", "n", "mean", "min",
    "median", "max", "n/sec", "allocated_memory", "gc", "memory", "result",
    "timing")]

  res[order(-res$relative), ]
}

mark_internal <- function(..., exprs, setup, env, min_time, num_iterations, check_results) {

  # Run setup code
  if (!is.null(setup)) {
    eval(setup, env)
  }

  exprs <- c(dots(...), exprs)

  results <- vector("list", length(exprs))

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
  results[[1]] <- eval_one(exprs[[1]])
  for (i in seq_len(length(exprs) - 1)) {

    results[[i + 1]] <- eval_one(exprs[[i + 1]])

    if (isTRUE(check_results)) {
      comp <- testthat::compare(results[[1]]$result, results[[i + 1]]$result)
      if (!comp$equal) {
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
  for (i in seq_len(length(exprs))) {
    gcs <- count_gc()
    results[[i]]$timing <- .Call(mark_, exprs[[i]], env, min_time, as.integer(num_iterations))
    results[[i]]$gc <- gcs()

    # Do an explicit gc, to minimize counting a gc against a prior expression.
    gc()
  }

  # TODO remove purrr dependency probably?
  res <- tibble::as_tibble(purrr::transpose(results))

  res$name <- auto_name(exprs)
  res$n <- purrr::map_int(res$timing, length)
  res$mean <- purrr::map_dbl(res$timing, mean)
  res$min <- purrr::map_dbl(res$timing, min)
  res$median <- purrr::map_dbl(res$timing, stats::median)
  res$max <- purrr::map_dbl(res$timing, max)
  total_time <- purrr::map_dbl(res$timing, sum)
  res$`n/sec` <- res$n / total_time

  res$relative <- res$n / min(res$n)
  res$allocated_memory <- prettyunits::pretty_bytes(purrr::map_dbl(res$memory, ~ if (is.null(.)) NA else sum(.$bytes, na.rm = TRUE)))
  res$gc <- unlist(res$gc)
  res
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
  nms[is_missing] <- vapply(exprs[is_missing], deparse, character(1), width.cutoff = 500)

  nms
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

count_gc <- function() {
  complete <- FALSE
  reg.finalizer(environment(), function(e) complete <<- TRUE)

  i <- 0
  fin <- function(e) {
    i <<- i + 1
    if (!identical(complete, TRUE)) {
      reg.finalizer(environment(), fin)
    }
  }
  fin()

  function() i
}
