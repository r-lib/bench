#' Return the current high-resolution real time.
#'
#' Time is expressed as seconds since some arbitrary time in the past; it
#' is not correlated in any way to the time of day, and thus is not subject to
#' resetting or drifting. The hi-res
#' timer is ideally suited to performance measurement tasks, where cheap,
#' accurate interval timing is required.
#' @export
#' @examples
#' hires_time()
#'
#' # R rounds doubles to 7 digits by default, see greater precision by setting
#' # the digits argument when printing
#' print(hires_time(), digits = 20)
#'
#' # Generally used by recording two times and then subtracting them
#' start <- hires_time()
#' end <- hires_time()
#' elapsed <- end - start
#' elapsed
hires_time <- function() {
  .Call(hires_time_)
}
