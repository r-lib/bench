new_bench_expr <- function(x, description = names(x)) {
  if (is.null(description)) {
    description <- rep("", length(x))
  }
  names(x) <- description

  structure(x, class = c("bench_expr", "list"))
}

#' @export
format.bench_expr <- function(x, ...) {
  desc <- names(x)
  is_missing <- desc == ""
  desc[is_missing] <- vapply(x[is_missing], deparse_trunc, character(1))
  desc
}

#' @export
as.character.bench_expr <- format.bench_expr

#' @export
print.bench_expr <- function(x, ...) {
  x <- unclass(x)
  NextMethod()
}

type_sum.bench_expr <- function(x) {
  "bch:expr"
}

#' @export
`[.bench_expr` <- function(x, i, ...) {
  new_x <- NextMethod("[")
  new_bench_expr(new_x)
}

pillar_shaft.bench_expr <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.bench_expr(x), align = "left", ...)
}

scale_type.bench_expr <- function(x) {
  "bench_expr"
}

setOldClass(c("bench_expr", "list"), list())

#' Position and color scales for bench_expr data
#'
#' Default scales for the `bench_expr` class, these are added to plots using
#' `bench_expr` objects automatically.
#' @name scale_bench_expr
#' @keywords internal
#' @export
scale_x_bench_expr <- function(...) {
  sc <- ggplot2::scale_x_discrete(...)
  sc$transform <- as.character
  sc
}

#' @rdname scale_bench_expr
#' @keywords internal
#' @export
scale_y_bench_expr <- function(...) {
  sc <- ggplot2::scale_y_discrete(...)
  sc$transform <- as.character
  sc
}

#' @rdname scale_bench_expr
#' @keywords internal
#' @export
scale_colour_bench_expr <- function(palette = scales::hue_pal(...), ..., aesthetics = "colour") {
  sc <- ggplot2::discrete_scale(aesthetics, "bench_expr", palette, ...)
  sc$transform <- as.character
  sc
}

#' @rdname scale_bench_expr
#' @keywords internal
#' @export
scale_color_bench_expr <- scale_colour_bench_expr
