#' @useDynLib bench, .registration = TRUE
NULL

#' Benchmark a list of quoted expressions
#' @export
#' @examples
#' mark(list(quote(1 + 1), quote(2 == 2)))
mark <- function(exprs, env = parent.frame(), n = 10) {
  mark_(exprs, env, n)
}
