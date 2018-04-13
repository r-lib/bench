#' @useDynLib bench, .registration = TRUE
NULL

#' Benchmark a list of quoted expressions
#' @export
#' @examples
#' mark(list(quote(1 + 1), quote(2 == 2)))
mark <- function(exprs, env = parent.frame(), n = 10, check_results = TRUE) {
  eval_one <- function(e) {
    f <- tempfile()
    on.exit(unlink(f))
    Rprofmem(f, threshold = 1)
    res <- eval(e, env)
    Rprofmem(NULL)
    list(result = res, memory = parse_allocations(f))
  }
  results <- vector("list", length(exprs))

  results[[1]] <- eval_one(exprs[[1]])
  for (i in seq_len(length(exprs) - 1)) {
    results[[i + 1]] <- eval_one(exprs[[i + 1]])
    if (isTRUE(check_results)) {
      testthat::expect_equal(!!results[[1]]$result, !!results[[i + 1]]$result)
    }
  }
  timing <- mark_(exprs, env, n)

  for (i in seq_along(results)) {
    results[[i]]$timing <- timing[[i]]
  }
  results
}

parse_allocations <- function(filename) {
  # TODO: remove this dependency / simplify parsing
  profmem::readRprofmem(filename)
}
