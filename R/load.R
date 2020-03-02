
#' Get system load averages
#'
#' Uses OS system APIs to return the load average for the past 1, 5 and 15 minutes.
#' @export
bench_load_average <- function() {
  stats::setNames(
    .Call(bench_load_average_),
    c("load_1_min", "load_5_min", "load_15_min")
  )
}
