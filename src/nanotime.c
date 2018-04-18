#include "nanotime.h"

#ifdef __MACH__
#include <mach/mach_time.h>
#include <time.h>
#include <sys/time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#if __MACH__
long double real_time() {

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
#else
long double real_time() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_REALTIME, ...) failed");
  }

  return ts.tv_sec * NSEC_PER_SEC + (long double)ts.tv_nsec;
}
#endif

long double process_cpu_time() {
  struct timespec ts;
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_PROCESS_CPUTIME_ID, ...) failed");
  }
  return ts.tv_sec + (long double)ts.tv_nsec / NSEC_PER_SEC;
}

long double expr_elapsed_time(SEXP expr, SEXP env) {
  long double start = real_time();

  // Actually evaluate the R code
  Rf_eval(expr, env);

  long double end = real_time();

  return end - start;
}
