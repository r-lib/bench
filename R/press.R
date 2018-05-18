#' Run setup code and benchmarks across a grid of parameters
#'
#' @description
#' `press()` is used to run [bench::mark()] across a grid of parameters and
#' then _press_ the results together.
#'
#' The parameters you want to set are given as named arguments and a grid of
#' all possible combinations is automatically created.
#'
#' The code to setup and benchmark is given by one unnamed expression (often
#' delimited by `\{`).
#'
#' If replicates are desired a dummy variable can be used, e.g. `rep = 1:5` for
#' replicates.
#'
#' @param ... If named, parameters to define, if unnamed the expression to run.
#'   Only one unnamed expression is permitted.
#' @param .grid A pre-build grid of values to use, typically a [data.frame] or
#'   [tibble]. This is useful if you only want to use a subset of all possible combinations.
#' @export
#' @examples
#' # Helper function to create a simple data.frame of the specified dimensions
#' create_df <- function(rows, cols) {
#'   as.data.frame(setNames(
#'     replicate(cols, runif(rows, 1, 1000), simplify = FALSE),
#'     rep_len(c("x", letters), cols)))
#' }
#'
#' # Run 4 data sizes across 3 samples with 2 replicates (24 total benchmarks)
#' press(
#'   rows = c(1000, 10000),
#'   cols = c(10, 100),
#'   rep = 1:2,
#'   {
#'     dat <- create_df(rows, cols)
#'     bench::mark(
#'       min_time = .05,
#'       bracket = dat[dat$x > 500, ],
#'       which = dat[which(dat$x > 500), ],
#'       subset = subset(dat, x > 500)
#'     )
#'   }
#' )
press <- function(..., .grid = NULL) {
  args <- rlang::quos(...)

  unnamed <- names(args) == ""

  if (sum(unnamed) < 1) {
    stop("Must supply one unnamed argument", call. = FALSE)
  }

  if (sum(unnamed) > 1) {
    stop("Must supply no more than one unnamed argument", call. = FALSE)
  }

  if (!is.null(.grid)) {
    if (any(!unnamed)) {
      stop("Must supply either `.grid` or named arguments, not both", call. = FALSE)
    }
    parameters <- .grid
  } else {
    parameters <- expand.grid(lapply(args[!unnamed], rlang::eval_tidy))
  }

  status <- format(tibble::as_tibble(parameters), n = Inf)
  message(glue::glue("
      Running with:
      {status[[2]]}"))
  eval_one <- function(row) {
    e <- new.env(parent = emptyenv())
    for (col in seq_along(parameters)) {
      var <- names(parameters)[[col]]
      value <- parameters[row, col]
      assign(var, value, envir = e)
    }
    message(status[[row + 3L]])
    rlang::eval_tidy(args[[which(unnamed)]], data = e)
  }

  res <- lapply(seq_len(nrow(parameters)), eval_one)
  rows <- vapply(res, NROW, integer(1))

  if (!all(rows == rows[[1]])) {
    stop("Results must have equal rows", call. = FALSE)
    # TODO: print parameters / results that are unequal?
  }
  res <- do.call(rbind, res)
  parameters <- parameters[rep(seq_len(nrow(parameters)), each = rows[[1]]), , drop = FALSE]
  bench_mark(tibble::as_tibble(cbind(res[1], parameters, res[-1])))
}
