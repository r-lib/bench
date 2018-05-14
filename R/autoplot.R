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
#' [ggbeeswarm][ggbeeswarm::ggbeeswarm], [ggridges][ggridges::ggridges].
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
#' if (require(ggplot2)) {
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
#'       mutate(expression = forcats::fct_reorder(expression, min, .desc = TRUE)) %>%
#'       as_bench_mark() %>%
#'       autoplot("violin")
#'   }
#' }
autoplot.bench_mark <- function(object,
  type = c("beeswarm", "jitter", "ridge", "boxplot", "violin"),...) {

  res <- tidyr::unnest(object)
  p <- ggplot2::ggplot(res)

  type <- match.arg(type)

  switch(type,
    beeswarm = p <- p +
      ggplot2::aes_string("expression", "time", color = "gc") +
      ggbeeswarm::geom_quasirandom(...) +
      ggplot2::coord_flip(),

    jitter = p <- p +
      ggplot2::aes_string("expression", "time", color = "gc") +
      ggplot2::geom_jitter(...) +
      ggplot2::coord_flip(),

    ridge = p <- p +
      ggplot2::aes_string("time", "expression") +
      ggridges::geom_density_ridges(...),

    boxplot = p <- p +
      ggplot2::aes_string("expression", "time") +
      ggplot2::geom_boxplot(...) +
      ggplot2::coord_flip(),

    violin = p <- p +
      ggplot2::aes_string("expression", "time") +
      ggplot2::geom_violin(...) +
      ggplot2::coord_flip())

  parameters <- setdiff(
    colnames(object),
    c("expression", summary_cols, data_cols, c("level0", "level1", "level2")))

  if (length(parameters) == 0) {
    return(p)
  }

  if (length(parameters) == 2) {
    return(p +
      ggplot2::facet_grid(
        paste0(parameters[[1]], "~", parameters[[2]]),
        labeller = ggplot2::label_both))
  }

  p + ggplot2::facet_wrap(parameters, labeller = ggplot2::label_both)
}

#' @rdname autoplot.bench_mark
#' @param x A `bench_mark` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.bench_mark <- function(x, ..., type = c("beeswarm", "jitter", "ridge", "boxplot", "violin"), y) {
  type <- match.arg(type)
  ggplot2::autoplot(x, type = type, ...)
}
