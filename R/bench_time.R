#' Measure Process CPU and real time that an expression used.
#'
#' @param expr A expression to be timed.
#' @return A `bench_time` object with two values.
#' - `process` - The process CPU usage of the expression evaluation.
#' - `real` - The wallclock time of the expression evaluation.
#' @details On some systems (such as macOS) the process clock has lower
#'   precision than the realtime clock, as a result there may be cases where the
#'   process time is larger than the real time for fast expressions.
#' @examples
#' # This will use ~.5 seconds of real time, but very little process time.
#' bench_time(Sys.sleep(.5))
#' @seealso [bench_memory()] To measure memory allocations for a given expression.
#' @aliases system_time
#' @export
bench_time <- function(expr) {
  stats::setNames(
    as_bench_time(.Call(system_time_, substitute(expr), parent.frame())),
    c("process", "real"))
}

#' @export
system_time <- bench_time

#' Measure memory that an expression used.
#'
#' @param expr A expression to be measured.
#' @return A tibble with two columns
#' - The total amount of memory allocated
#' - The raw memory allocations as parsed by [profmem::readRprofmem()]
#' @examples
#' if (capabilities("profmem")) {
#'   bench_memory(1 + 1:10000)
#' }
#' @export
bench_memory <- function(expr) {
  can_profile_memory <- capabilities("profmem")
  if (!can_profile_memory) {
    stop("Memory profiling not available in this version of R", call. = FALSE)
  }
  f <- tempfile()
  on.exit(unlink(f))
  utils::Rprofmem(f, threshold = 1)
  force(expr)
  utils::Rprofmem(NULL)

  memory <- parse_allocations(f)

  tibble::tibble(mem_alloc = bench_bytes(sum(memory$bytes, na.rm = TRUE)), memory = list(memory))
}
