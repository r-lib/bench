#' Autoplot method for bench_mark objects
#'
#' @param object A `bench_mark` object.
#' @param type The type of plot. Plotting geoms used for each type are
#' - beeswarm - [ggbeeswarm::geom_quasirandom()]
#' - jitter - [ggplot2::geom_jitter()]
#' - ridge - [ggridges::geom_density_ridges()]
#' - boxplot - [ggplot2::geom_boxplot()]
#' - violin - [ggplot2::geom_violin()]
#' @param ... Additional arguments passed to the plotting geom.
#' @details This function requires some optional dependencies. [ggplot2][ggplot2::ggplot2-package],
#' [tidyr][tidyr::tidyr-package], and depending on the plot type
#' [ggbeeswarm][ggbeeswarm::ggbeeswarm], [ggridges][ggridges::ggridges-package].
#'
#' For `type` of `beeswarm` and `jitter` the points are colored by the highest
#' level garbage collection performed during each iteration.
#'
#' For plots with 2 parameters `ggplot2::facet_grid()` is used to construct a
#' 2d facet. For other numbers of parameters `ggplot2::facet_wrap()` is used
#' instead.
#'
#' @examples
#' dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))
#'
#' res <- bench::mark(
#'   dat[dat$x > 500, ],
#'   dat[which(dat$x > 500), ],
#'   subset(dat, x > 500))
#'
#' if (require(ggplot2) && require(tidyr) && require(ggbeeswarm)) {
#'
#'   # Beeswarm plot
#'   autoplot(res)
#'
#'   # ridge (joyplot)
#'   autoplot(res, "ridge")
#'
#'   # If you want to have the plots ordered by execution time you can do so by
#'   # ordering factor levels in the expressions.
#'   if (require(dplyr) && require(forcats)) {
#'
#'     res %>%
#'       mutate(expression = forcats::fct_reorder(as.character(expression), min, .desc = TRUE)) %>%
#'       as_bench_mark() %>%
#'       autoplot("violin")
#'   }
#' }
# Lazily registered in `.onLoad()`
autoplot.bench_mark <- function(
  object,
  type = c("beeswarm", "jitter", "ridge", "boxplot", "violin"),
  ...
) {
  rlang::check_installed(c("ggplot2", "tidyr (>= 1.0.0)"), "for `autoplot()`.")

  type <- match.arg(type)

  if (type == "beeswarm") {
    rlang::check_installed("ggbeeswarm", "to use `type = \"beeswarm\".")
  }

  # Just convert bench_expr to characters
  if (inherits(object$expression, "bench_expr")) {
    object$expression <- as.character(object$expression)
  }

  res <- tidyr::unnest(object, c(time, gc))
  p <- ggplot2::ggplot(res)

  switch(
    type,
    beeswarm = p <- p +
      ggplot2::aes(.data$time, .data$expression, color = .data$gc) +
      ggbeeswarm::geom_quasirandom(..., orientation = "y"),

    jitter = p <- p +
      ggplot2::aes(.data$time, .data$expression, color = .data$gc) +
      ggplot2::geom_jitter(...),

    ridge = p <- p +
      ggplot2::aes(.data$time, .data$expression) +
      ggridges::geom_density_ridges(...),

    boxplot = p <- p +
      ggplot2::aes(.data$time, .data$expression) +
      ggplot2::geom_boxplot(...),

    violin = p <- p +
      ggplot2::aes(.data$time, .data$expression) +
      ggplot2::geom_violin(...)
  )

  parameters <- setdiff(
    colnames(object),
    c("expression", summary_cols, data_cols, c("level0", "level1", "level2"))
  )

  if (length(parameters) == 0) {
    return(p)
  }

  if (length(parameters) == 2) {
    return(
      p +
        ggplot2::facet_grid(
          paste0(parameters[[1]], "~", parameters[[2]]),
          labeller = ggplot2::label_both
        )
    )
  }

  p + ggplot2::facet_wrap(parameters, labeller = ggplot2::label_both)
}

#' @rdname autoplot.bench_mark
#' @param x A `bench_mark` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.bench_mark <- function(
  x,
  ...,
  type = c("beeswarm", "jitter", "ridge", "boxplot", "violin"),
  y
) {
  type <- match.arg(type)
  ggplot2::autoplot(x, type = type, ...)
}
