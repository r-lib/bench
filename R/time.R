time_units <- function() c(
  'ns' = 1e-9,
  'us' = 1e-6,
  if (is_utf8_output()) c('\U00B5s' = 1e-6),
  'ms' = 1e-3,
  's' = 1,
  'm' = 60,
  'h' = 60 * 60,
  'd' = 60 * 60 * 24,
  'w' = 60 * 60 * 24 * 7)

#' Human readable times
#'
#' Construct, manipulate and display vectors of elapsed times in seconds. These
#' are numeric vectors, so you can compare them numerically, but they can also
#' be compared to human readable values such as '10ms'.
#'
#' @param x A numeric or character vector. Character representations can use
#'   shorthand sizes (see examples).
#' @examples
#' as_bench_time("1ns")
#' as_bench_time("1")
#' as_bench_time("1us")
#' as_bench_time("1ms")
#' as_bench_time("1s")
#'
#' as_bench_time("100ns") < "1ms"
#'
#' sum(as_bench_time(c("1MB", "5MB", "500KB")))
#' @export
as_bench_time <- function(x) {
  UseMethod("as_bench_time")
}

new_bench_time <- function(x) {
  structure(x, class = c("bench_time", "numeric"))
}
setOldClass(c("bench_time", "numeric"), numeric())

#' @export
as_bench_time.default <- function(x) {
  x <- as.character(x)
  re <- glue::glue("
      ^(?<size>[[:digit:].]+)\\s*(?<unit>{nms}?)$
      ", nms = paste0(names(time_units()), collapse = "|"))

  m <- captures(x, regexpr(re, x, perl = TRUE))
  m$unit[m$unit == ""] <- "s"
  new_bench_time(unname(as.numeric(m$size) * time_units()[m$unit]))
}

#' @export
as_bench_time.bench_time <- function(x) {
  return(x)
}

#' @export
as_bench_time.numeric <- function(x) {
  is_small <- x < 1e-9 & !is.infinite(x) & x != 0
  x[is_small] <- 1e-9

  new_bench_time(x)
}
tolerance <- sqrt(.Machine$double.eps)
find_unit <- function(x, units) {
  if (is.na(x) || is.nan(x) || x <= 0 || is.infinite(x)) {
    return(NA_character_)
  }
  epsilon <- 1 - (x * (1 / units))
  names(
    utils::tail(n = 1,
      which(epsilon < tolerance)))
}

# Adapted from https://github.com/gaborcsardi/prettyunits
# Aims to be consistent with ls -lh, so uses 1024 KiB units, 3 or less digits etc.
#' @export
format.bench_time <- function(x, scientific = FALSE, digits = 3, drop0trailing = TRUE, ...) {
  nms <- names(x)

  # convert negative times to 1ns, this can happen if the minimum calculated
  # overhead is higher than the time.
  x[x < 1e-9 & !is.infinite(x) & x != 0] <- 1e-9

  seconds <- unclass(x)

  unit <- vcapply(x, find_unit, time_units())
  res <- round(seconds / time_units()[unit], digits = digits)

  ## Zero seconds
  res[seconds == 0] <- 0
  unit[seconds == 0] <- ""

  ## NA, NaN, Inf, -Inf, seconds
  res[is.na(seconds)] <- NA_real_
  res[is.nan(seconds)] <- NaN
  res[is.infinite(seconds)] <- Inf
  res[is.infinite(seconds) & seconds < 0] <- -Inf
  unit[is.na(seconds) | is.infinite(seconds)] <- ""

  res <- format(res, scientific = scientific, digits = digits, drop0trailing = drop0trailing, ...)

  stats::setNames(paste0(res, unit), nms)
}

#' @export
as.character.bench_time <- format.bench_time

#' @export
print.bench_time <- function(x, ...) {
  print(format.bench_time(x, ...), quote = FALSE)
}

#' @export
sum.bench_time <- function(x, ...) {
  new_bench_time(NextMethod())
}

#' @export
min.bench_time <- function(x, ...) {
  new_bench_time(NextMethod())
}

#' @export
max.bench_time <- function(x, ...) {
  new_bench_time(NextMethod())
}

#' @export
`[.bench_time` <- function(x, i, ...) {
  new_bench_time(NextMethod("["))
}

#' @export
`[[.bench_time` <- function(x, i, ...) {
  new_bench_time(NextMethod("[["))
}

#' @export
# Adapted from Ops.numeric_version
Ops.bench_time <- function(e1, e2, ...) {
  if (nargs() == 1L) {
    stop(sprintf("unary '%s' not defined for \"bench_time\" objects", .Generic),
      call. = FALSE)
  }

  boolean <- switch(.Generic,
    `+` = TRUE,
    `-` = TRUE,
    `*` = TRUE,
    `/` = TRUE,
    `^` = TRUE,
    `<` = TRUE,
    `>` = TRUE,
    `==` = TRUE,
    `!=` = TRUE,
    `<=` = TRUE,
    `>=` = TRUE,
  FALSE)
  if (!boolean) {
    stop(sprintf("'%s' not defined for \"bench_time\" objects", .Generic),
      call. = FALSE)
  }
  e1 <- as_bench_time(e1)
  e2 <- as_bench_time(e2)
  NextMethod(.Generic)
}

#' @export
Summary.bench_time <- function(..., na.rm = FALSE) {
  new_bench_time(NextMethod(.Generic))
}

#' @export
mean.bench_time <- function(x, ...) {
  new_bench_time(NextMethod(.Generic))
}

pillar_shaft.bench_time <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.bench_time(x), align = "right", ...)
}

type_sum.bench_time <- function(x) {
  "bch:tm"
}

#' Benchmark time transformation
#'
#' This both log transforms the times and formats the labels as a `bench_time`
#' object.
#' @inheritParams scales::log_trans
#' @keywords internal
#' @export
bench_time_trans <- function(base = 10) {
  if (is.null(base)) {
    return(
      scales::trans_new("bch:tm", as.numeric, as_bench_time,
        scales::pretty_breaks(), domain = c(1e-100, Inf)
      )
    )
  }

  trans <- function(x) log(as.numeric(x), base)
  inv <- function(x) as_bench_time(base ^ as.numeric(x))

  scales::trans_new(paste0("bch:tm-", format(base)), trans, inv,
    scales::log_breaks(base = base), domain = c(1e-100, Inf))
}

scale_type.bench_time <- function(x) "bench_time"

#' Position scales for bench_time data
#'
#' Default scales for the `bench_time` class, these are added to plots using
#' `bench_time` objects automatically.
#' @name scale_bench_time
#' @param base The base of the logarithm, if `NULL` instead use a
#'   non-logarithmic scale.
#' @keywords internal
#' @export
scale_x_bench_time <- function(base = 10, ...) {
  ggplot2::scale_x_continuous(..., trans = bench_time_trans(base = base))
}

#' @rdname scale_bench_time
#' @keywords internal
#' @export
scale_y_bench_time <- function(base = 10, ...) {
  ggplot2::scale_y_continuous(..., trans = bench_time_trans(base = base))
}
