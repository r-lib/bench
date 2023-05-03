#include "nanotime.h"
#include "os.h"

#if OS_WINDOWS
#include <windows.h>
#elif OS_MACOS
#include <mach/mach_time.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#else
#define __EXTENSIONS__
#include <time.h>
#include <sys/time.h>
#define NSEC_PER_SEC	1000000000	/* nanoseconds per second */
#endif


#if OS_WINDOWS
long double real_time(void) {
  // https://msdn.microsoft.com/en-us/library/windows/desktop/ms644904(v=vs.85).aspx
  static LARGE_INTEGER frequency;
  frequency.QuadPart = 0;
  if (frequency.QuadPart == 0) {
    if (QueryPerformanceFrequency(&frequency) == FALSE) {
      Rf_error("QueryPerformanceFrequency(...) failed");
    }
  }
  LARGE_INTEGER count;
  if (QueryPerformanceCounter(&count) == FALSE) {
    Rf_error("QueryPerformanceCounter(...) failed");
  }
  return (long double) count.QuadPart / frequency.QuadPart;
}
#elif OS_MACOS
long double real_time(void) {

  // https://developer.apple.com/library/content/qa/qa1398/_index.html
  //static mach_timebase_info_data_t info;
  static uint64_t ratio = 0;

  if (ratio == 0) {
    mach_timebase_info_data_t info;
    if (mach_timebase_info(&info) != KERN_SUCCESS) {
      Rf_error("mach_timebase_info(...) failed");
    }
    ratio = info.numer / info.denom;
  }

  uint64_t time = mach_absolute_time();
  uint64_t nanos = time * ratio;
  return (long double)nanos / NSEC_PER_SEC;
}
#elif OS_SOLARIS
long double real_time(void) {
  hrtime_t time = gethrtime();
  // The man page doesn't mention any error return values

  return (long double)time / NSEC_PER_SEC;
}
#else
long double real_time(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_REALTIME, ...) failed");
  }

  return ts.tv_sec + (long double)ts.tv_nsec / NSEC_PER_SEC;
}
#endif

long double process_cpu_time(void) {
#if OS_WINDOWS
  HANDLE proc = GetCurrentProcess();
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  if (GetProcessTimes(proc, &creation_time, &exit_time, &kernel_time,
        &user_time) == FALSE) {
    Rf_error("GetProcessTimes(...) failed");
  }
  ULARGE_INTEGER kernel;
  ULARGE_INTEGER user;
  kernel.HighPart = kernel_time.dwHighDateTime;
  kernel.LowPart = kernel_time.dwLowDateTime;
  user.HighPart = user_time.dwHighDateTime;
  user.LowPart = user_time.dwLowDateTime;
  return (((long double)kernel.QuadPart + (long double)user.QuadPart) * 1e-7);
#elif OS_SOLARIS
  hrtime_t time = gethrvtime();
  // The man page doesn't mention any error return values

  return (long double)time / NSEC_PER_SEC;
#elif defined(CLOCK_PROCESS_CPUTIME_ID)
  // Modern macOS and Linux
  struct timespec ts;
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_PROCESS_CPUTIME_ID, ...) failed");
  }
  return ts.tv_sec + (long double)ts.tv_nsec / NSEC_PER_SEC;
#else
  // macOS before 10.12 didn't define `CLOCK_PROCESS_CPUTIME_ID`
  // https://github.com/r-lib/bench/commit/cfd4e2392f980e29d833f4df42a43ea2ba131aaf
  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru) != 0) {
    Rf_error("getrusage(RUSAGE_SELF, ...) failed");
  }
  return ru.ru_utime.tv_sec + (long double) ru.ru_utime.tv_usec * 1e-6 +
    ru.ru_stime.tv_sec + (long double) ru.ru_stime.tv_usec * 1e-6;
#endif
}

long double expr_elapsed_time(SEXP expr, SEXP env) {
  long double start = real_time();

  // Actually evaluate the R code
  Rf_eval(expr, env);

  long double end = real_time();

  return end - start;
}
