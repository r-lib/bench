#nocov start
.onLoad <- function(...) {
  register_s3_method("tidyr", "unnest", "bench_mark")
  register_s3_method("ggplot2", "autoplot", "bench_mark")

  register_s3_method("pillar", "pillar_shaft", "bench_time")
  register_s3_method("pillar", "type_sum", "bench_time")
  register_s3_method("ggplot2", "scale_type", "bench_time")

  register_s3_method("pillar", "pillar_shaft", "bench_bytes")
  register_s3_method("pillar", "type_sum", "bench_bytes")
  register_s3_method("ggplot2", "scale_type", "bench_bytes")

  register_s3_method("knitr", "knit_print", "bench_mark")
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

#nocov end
