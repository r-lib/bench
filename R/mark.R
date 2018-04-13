#' @useDynLib bench, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Benchmark a series of functions
#'
#' Benchmark a list of quoted expressions. Each expression will always run at
#' least twice, once to measure the memory allocation and store results and one
#' or more times to measure timing.
#'
#' @param ... Expressions to benchmark
#' @param exprs A list of quoted expressions to benchmark
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
mark <- function(..., exprs = NULL, env = parent.frame(), min_time = .5, num_iterations = 1e6, check_results = TRUE) {
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
      testthat::expect_equal(!!results[[1]]$result, !!results[[i + 1]]$result)
    }
  }

  # Run timing benchmark
  timing <- mark_(exprs, env, min_time, num_iterations)

  # Add timings to results
  for (i in seq_along(results)) {
    results[[i]]$timing <- timing[[i]]
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

  res <- res[c("name", "relative", "n", "mean", "min", "median", "max", "n/sec", "memory", "result", "timing")]
  res[order(-res$relative), ]
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

