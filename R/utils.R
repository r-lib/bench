viapply <- function(x, f, ...) vapply(x, f, integer(1), ...)
vdapply <- function(x, f, ...) vapply(x, f, double(1), ...)
vcapply <- function(x, f, ...) vapply(x, f, character(1), ...)
vlapply <- function(x, f, ...) vapply(x, f, logical(1), ...)

captures <- function(x, m) {
  assert("`x` must be a character", is.character(x))
  assert("`m` must be a match object from `regexpr()`",
    inherits(m, "integer") &&
    all(c("match.length", "capture.start", "capture.length", "capture.names") %in% names(attributes(m))))

  starts <- attr(m, "capture.start")
  strings <- substring(x, starts, starts + attr(m, "capture.length") - 1L)
  res <- data.frame(matrix(strings, ncol = NCOL(starts)), stringsAsFactors = FALSE)
  colnames(res) <- auto_name_vec(attr(m, "capture.names"))
  res[is.na(m) | m == -1, ] <- NA_character_
  res
}

assert <- function(msg, ..., class = "invalid_argument") {
  tests <- unlist(list(...))

  if (!all(tests)) {
    stop(bench_error(msg, class = class))
  }
}

bench_error <- function(msg, class = "invalid_argument") {
  structure(class = c(class, "bench_error", "error", "condition"), list(message = msg))
}

auto_name_vec <- function(names) {
  missing <- names == ""
  if (all(!missing)) {
    return(names)
  }
  names[missing] <- seq_along(names)[missing]
  names
}

with_gcinfo <- function(expr) {
  old <- gcinfo(TRUE)
  on.exit(gcinfo(old))
  force(expr)
}

deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}
