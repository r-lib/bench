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
  tf <- tempfile()
  con <- file(tf, "wb")
  sink(con, type = "message")
  {
    old <- gcinfo(TRUE)
    on.exit({
      gcinfo(old)
      sink(NULL, type = "message")
      close(con)
      output <- readLines(tf, warn = FALSE)
      unlink(tf)
      return(output)
    })
    force(expr)
  }
}

deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  # Remove any leading spaces
  text <- sub("^[[:space:]]*", "", text)

  # Collapse all together
  glue::glue_collapse(text, " ", width = width)
}

# inlined from https://github.com/r-lib/cli/blob/master/R/utf8.R
is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (! is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}

lengths <- function(x, use.names = TRUE) {
  viapply(x, length, USE.NAMES = use.names)
}

# check if the new interface is being used
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

dots <- function(...) {
  dots <- as.list(substitute(...()))

  n <- length(dots)
  if (n && rlang::is_missing(dots[[n]])) {
    dots <- dots[-n]
  }

  dots
}
