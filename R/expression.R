new_bench_expr <- function(x, description = names(x)) {
  if (is.null(description)) {
    description <- rep("", length(x))
  }
  names(x) <- description
  structure(x, class = c("bench_expr", "list"), description = description)
}

#' @export
format.bench_expr <- function(x, ...) {
  desc <- attr(x, "description")
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

#' @export
type_sum.bench_expr <- function(x) {
  "bch:expr"
}

#' @export
`[.bench_expr` <- function(x, i, ...) {
  new_x <- NextMethod("[")
  new_bench_expr(new_x)
}

# Lazily registered in `.onLoad()`
vec_proxy.bench_expr <- function(x, ...) {
  desc <- attr(x, "description")
  attributes(x) <- NULL
  out <- list(x = x, desc = desc)
  vctrs::new_data_frame(out, n = length(x))
}

# Lazily registered in `.onLoad()`
vec_restore.bench_expr <- function(x, to, ...) {
  new_bench_expr(x$x, x$desc)
}

#' @export
pillar_shaft.bench_expr <- function(x, ...) {
  # We format bench expressions exactly like character vectors. This ensures
  # they are truncated as needed, which is useful for long unnamed expressions
  # (#94). This is the same logic as `pillar:::pillar_shaft.factor()`.
  pillar_shaft(as.character(x), ...)
}

# Lazily registered in `.onLoad()`
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
scale_colour_bench_expr <- function(
  palette = scales::hue_pal(...),
  ...,
  aesthetics = "colour"
) {
  sc <- ggplot2::discrete_scale(aesthetics, "bench_expr", palette, ...)
  sc$transform <- as.character
  sc
}

#' @rdname scale_bench_expr
#' @keywords internal
#' @export
scale_color_bench_expr <- scale_colour_bench_expr
