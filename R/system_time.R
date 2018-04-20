#' Return Process CPU and real time that an expression used.
#'
#' @param expr A expression to be timed.
#' @return A [bench_time] object with two values.
#' - `process` - The process CPU usage of the expression evaluation.
#' - `real` - The wallclock time of the expression evaluation.
#' @details On some systems (such as macOS) the process clock has lower
#'   precision than the realtime clock, as a result there may be cases where the
#'   process time is larger than the real time for fast expressions.
#' @examples
#' # This will use ~.5 seconds of real time, but very little process time.
#' system_time(Sys.sleep(.5))
#' @export
system_time <- function(expr) {
  stats::setNames(
    bench_time(.Call(system_time_, substitute(expr), parent.frame())),
    c("process", "real"))
}
