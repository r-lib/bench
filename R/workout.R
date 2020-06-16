#' Workout a group of expressions individually
#'
#' Given an block of expressions in `{}` [workout()] individually times each
#' expression in the group. [workout_expressions()] is a lower level function most
#' useful when reading lists of calls from a file.
#'
#' @param expr one or more expressions to workout, use `{}` to pass multiple
#'   expressions.
#' @param exprs A list of calls to measure.
#' @param description A name to label each expression, if not supplied the
#'   deparsed expression will be used.
#' @param env The environment in which the expressions should be evaluated.
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
#'
#' # The equivalent to the above, reading the code from a file
#' workout_expressions(as.list(parse(system.file("examples/exprs.R", package = "bench"))))
workout <- function(expr, description = NULL) {
  expr <- substitute(expr)
  env <- parent.frame()
  if (rlang::is_call(expr, "{")) {
    exprs <- as.list(expr[-1])
  } else {
    exprs <- list(expr)
  }
  workout_expressions(exprs, env, description)
}

#' @rdname workout
#' @export
workout_expressions <- function(exprs, env = parent.frame(), description = NULL) {
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
