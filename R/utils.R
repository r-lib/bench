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
  capture_output(type = "message", {
    old <- gcinfo(TRUE)
    on.exit(gcinfo(old))
    force(expr)
    }
  )
}

deparse_trunc <- function(x, width = getOption("width")) {
  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
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

#nocov start
# This is capture.output from R-3.4.3, 3.1 does not support type = "message"
capture_output <- function(..., file = NULL, append = FALSE,
  type = c("output", "message"), split = FALSE) {
    args <- substitute(list(...))[-1L]
    type <- match.arg(type)
    rval <- NULL
    closeit <- TRUE
    if (is.null(file))
        file <- textConnection("rval", "w", local = TRUE)
    else if (is.character(file))
        file <- file(file, if (append)
            "a"
        else "w")
    else if (inherits(file, "connection")) {
        if (!isOpen(file))
            open(file, if (append)
                "a"
            else "w")
        else closeit <- FALSE
    }
    else stop("'file' must be NULL, a character string or a connection")
    sink(file, type = type, split = split)
    on.exit({
        sink(type = type, split = split)
        if (closeit) close(file)
    })
    pf <- parent.frame()
    evalVis <- function(expr) withVisible(eval(expr, pf))
    for (i in seq_along(args)) {
        expr <- args[[i]]
        tmp <- switch(mode(expr), expression = lapply(expr, evalVis),
            call = , name = list(evalVis(expr)), stop("bad argument"))
        for (item in tmp) if (item$visible)
            print(item$value)
    }
    on.exit()
    sink(type = type, split = split)
    if (closeit)
        close(file)
    if (is.null(rval))
        invisible(NULL)
    else rval
}
#nocov end

collapse <- function(x, sep) {
  paste0(x, collapse = sep)
}

lengths <- function(x, use.names = TRUE) {
  viapply(x, length, USE.NAMES = use.names)
}
