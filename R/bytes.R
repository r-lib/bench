# This is mostly a copy of https://github.com/r-lib/fs/blob/0f5b6191935fe4c862d2e5003655e6c1669f4afd/R/fs_bytes.R
# If I end up needing this in a third package it should probably live in a package somewhere, maybe prettyunits?

byte_units <- c('B' = 1, 'K' = 1000, 'M' = 1000 ^ 2, 'G' = 1000 ^ 3, 'T' = 1000 ^ 4, 'P' = 1000 ^ 5, 'E' = 1000 ^ 6, 'Z' = 1000 ^ 7, 'Y' = 1000 ^ 8)

#' Human readable memory sizes
#'
#' Construct, manipulate and display vectors of byte sizes. These are numeric
#' vectors, so you can compare them numerically, but they can also be compared
#' to human readable values such as '10MB'.
#'
#' @param x A numeric or character vector. Character representations can use
#'   shorthand sizes (see examples).
#' @examples
#' bench_bytes("1")
#' bench_bytes("1K")
#' bench_bytes("1Kb")
#' bench_bytes("1Kib")
#' bench_bytes("1MB")
#'
#' bench_bytes("1KB") < "1MB"
#'
#' sum(bench_bytes(c("1MB", "5MB", "500KB")))
#' @name bench_bytes
#' @export
as_bench_bytes <- function(x) {
  UseMethod("as_bench_bytes")
}

#' @export
#' @rdname bench_bytes
bench_bytes <- as_bench_bytes

new_bench_bytes <- function(x) {
  structure(x, class = c("bench_bytes", "numeric"))
}
methods::setOldClass(c("bench_bytes", "numeric"), numeric())

#' @export
as_bench_bytes.default <- function(x) {
  x <- as.character(x)
  m <- captures(x, regexpr("^(?<size>[[:digit:].]+)\\s*(?<unit>[KMGTPEZY]?)i?[Bb]?$", x, perl = TRUE))
  m$unit[m$unit == ""] <- "B"
  new_bench_bytes(unname(as.numeric(m$size) * byte_units[m$unit]))
}

#' @export
as_bench_bytes.bench_bytes <- function(x) {
  return(x)
}

#' @export
as_bench_bytes.numeric <- function(x) {
  new_bench_bytes(x)
}

# Adapted from https://github.com/gaborcsardi/prettyunits
#' @export
format.bench_bytes <- function(x, scientific = FALSE, digits = 3, ...) {
  bytes <- unclass(x)

  exponent <- pmin(floor(log(bytes, 1000)), length(byte_units) - 1)
  res <- round(bytes / 1000 ^ exponent, 2)
  unit <- ifelse (exponent == 0, "", names(byte_units)[exponent + 1])

  ## Zero bytes
  res[bytes == 0] <- 0
  unit[bytes == 0] <- ""

  ## NA and NaN bytes
  res[is.na(bytes)] <- NA_real_
  res[is.nan(bytes)] <- NaN
  unit[is.na(bytes)] <- ""            # Includes NaN as well

  res <- format(res, scientific = scientific, digits = digits, drop0trailing = TRUE, ...)

  paste0(res, unit)
}

#' @export
as.character.bench_bytes <- format.bench_bytes

#' @export
print.bench_bytes <- function(x, ...) {
  cat(format.bench_bytes(x, ...))
}

#' @export
sum.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
min.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
max.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
`[.bench_bytes` <- function(x, i) {
  new_bench_bytes(NextMethod("["))
}

#' @export
# Adapted from Ops.numeric_version
Ops.bench_bytes <- function (e1, e2) {
  if (nargs() == 1L) {
    stop(sprintf("unary '%s' not defined for \"bench_bytes\" objects", .Generic),
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
    stop(sprintf("'%s' not defined for \"bench_bytes\" objects", .Generic),
      call. = FALSE)
  }
  e1 <- as_bench_bytes(e1)
  e2 <- as_bench_bytes(e2)
  NextMethod(.Generic)
}

pillar_shaft.bench_bytes <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.bench_bytes(x), align = "right", ...)
}

type_sum.bench_bytes <- function(x) {
  "bch:byt"
}
