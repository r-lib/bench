#' Workout a group of expressions individually
#'
#' Given an block of expressions in `{}` [workout()] individually times each
#' expression in the group.
#'
#' @param expr one or more expressions to workout, use `{}` to pass multiple
#'   expressions.
#' @param description A name to label each expression, if not supplied the
#'   deparsed expression will be used.
#' @export
#' @examples
#' workout({
#'   x <- 1:1000
#'   evens <- x %% 2 == 0
#'   y <- x[evens]
#'   length(y)
#'   length(which(evens))
#'   sum(evens)
#' })
workout <- function(expr, description = NULL) {
  expr <- rlang::enquo(expr)
  env <- rlang::quo_get_env(expr)
  exprs <- as.list(rlang::quo_get_expr(expr)[-1])

  if (is.null(description)) {
    description <- names(exprs)
  }

  out <- list(
    exprs = new_bench_expr(exprs, description),
    process = numeric(length(exprs)),
    real = numeric(length(exprs))
  )

  for (i in seq_along(exprs)) {
    res <- as_bench_time(.Call(system_time_, exprs[[i]], env))
    out[[2]][[i]] <- res[[1]]
    out[[3]][[i]] <- res[[2]]
  }

  out[[2]] <- new_bench_time(out[[2]])
  out[[3]] <- new_bench_time(out[[3]])

  tibble::as_tibble(out)
}
