#' Retrieve the current and maximum memory from the R process
#'
#' The memory reported here will likely differ from that reported by `gc()`, as
#' this includes all memory from the R process, including any child processes
#' and memory allocated outside R's garbage collector heap.
#'
#' The OS APIs used are as follows
#'
#' ## Windows
#' - [PROCESS_MEMORY_COUNTERS.WorkingSetSize](https://docs.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters)
#' - [PROCESS_MEMORY_COUNTERS.PeakWorkingSetSize](https://docs.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters)
#' ## macOS
#' - [task_info(TASK_BASIC_INFO)](https://developer.apple.com/documentation/kernel/1537934-task_info?language=occ)
#' - [rusage.ru_maxrss](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/getrusage.2.html)
#' ## linux
#' - [/proc/pid/status VmSize](https://man7.org/linux/man-pages/man5/proc.5.html)
#' - [/proc/pid/status VmPeak](https://man7.org/linux/man-pages/man5/proc.5.html)
#' and on Windows
#' [PROCESS_MEMORY_COUNTERS.PeakWorkingSetSize](https://docs.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters)
#' @export
bench_process_memory <- function() {
  stats::setNames(
    as_bench_bytes(.Call(bench_process_memory_)),
    c("current", "max")
  )
}
