units <- c('ns' = 1e-9, 'us' = 1e-6, '\U00B5s' = 1e-6, 'ms' = 1e-3, 's' = 1, 'm' = 60, 'h' = 60 * 60, 'd' = 60 * 60 * 24, 'w' = 60 * 60 * 24 * 7)

#' Human readable times
#'
#' Construct, manipulate and display vectors of elapsed times in seconds. These
#' are numeric vectors, so you can compare them numerically, but they can also
#' be compared to human readable values such as '10ms'.
#'
#' @param x A numeric or character vector. Character representations can use
#'   shorthand sizes (see examples).
#' @examples
#' bench_time("1")
#' bench_time("1ns")
#' bench_time("1us")
#' bench_time("1ms")
#' bench_time("1s")
#'
#' bench_time("100ns") < "1ms"
#'
#' sum(bench_time(c("1MB", "5MB", "500KB")))
#' @name bench_time
#' @export
as_bench_time <- function(x) {
  UseMethod("as_bench_time")
}

#' @export
#' @rdname bench_time
bench_time <- as_bench_time

new_bench_time <- function(x) {
  structure(x, class = c("bench_time", "numeric"))
}
setOldClass(c("bench_time", "numeric"), numeric())

#' @export
as_bench_time.default <- function(x) {
  x <- as.character(x)
  re <- glue::glue("
      ^(?<size>[[:digit:].]+)\\s*(?<unit>{nms}?)$
      ", nms = glue::glue_collapse(names(units), "|"))

  m <- captures(x, regexpr(re, x, perl = TRUE))
  m$unit[m$unit == ""] <- "s"
  new_bench_time(unname(as.numeric(m$size) * units[m$unit]))
}

#' @export
as_bench_time.bench_time <- function(x) {
  return(x)
}

#' @export
as_bench_time.numeric <- function(x) {
  new_bench_time(x)
}

# Adapted from https://github.com/gaborcsardi/prettyunits
# Aims to be consistent with ls -lh, so uses 1024 KiB units, 3 or less digits etc.
#' @export
format.bench_time <- function(x, scientific = FALSE, digits = 3, ...) {
  find_unit <- function(x) {
    if (is.na(x) || x == 0) {
      return(NA_character_)
    }
    epsilon <- 1 - (x * (1 / units))
    tolerance <- sqrt(.Machine$double.eps)
    names(
      utils::tail(n = 1,
        which(epsilon < tolerance)))
  }
  seconds <- unclass(x)

  unit <- vcapply(x, find_unit)
  res <- round(seconds / units[unit], digits = digits)

  ## Zero seconds
  res[seconds == 0] <- 0
  unit[seconds == 0] <- ""

  ## NA and NaN seconds
  res[is.na(seconds)] <- NA_real_
  res[is.nan(seconds)] <- NaN
  unit[is.na(seconds)] <- ""            # Includes NaN as well

  res <- format(res, scientific = scientific, digits = digits, drop0trailing = TRUE, ...)

  paste0(res, unit)
}

#' @export
as.character.bench_time <- format.bench_time

#' @export
print.bench_time <- function(x, ...) {
  cat(format.bench_time(x, ...), sep = " ")
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
`[.bench_time` <- function(x, i) {
  new_bench_time(NextMethod("["))
}

#' @export
`[[.bench_time` <- function(x, i) {
  new_bench_time(NextMethod("["))
}

#' @export
# Adapted from Ops.numeric_version
Ops.bench_time <- function(e1, e2) {
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

unnest.bench_time <- function(x, ...) {
  browser()
  x <- unclass(x)
  NextMethod(.Generic)
}

#' Benchmark time transformation
#'
#' @inheritParams scales::log_trans
#' @export
bench_time_trans <- function(base = 10) {
  trans <- function(x) log(x, base)
  inv <- function(x) as_bench_time(base ^ x)

  scales::trans_new(paste0("bch:tm-", format(base)), trans, inv,
    scales::log_breaks(base = base), domain = c(1e-100, Inf))
}
