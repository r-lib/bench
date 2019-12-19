#include <Rinternals.h>

#ifdef __WIN32
#include <windows.h>
#define PSAPI_VERSION 1
#include <psapi.h>
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

SEXP bench_max_memory_() {

  SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));

#if defined(_WIN32) || defined(_WIN64)
  PROCESS_MEMORY_COUNTERS pmc;
  if (!GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc))) {
    Rf_error("GetProcessMemoryInfo(...) failed");
  }

  REAL(out)[0] = pmc.PeakWorkingSetSize;
#else
  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru) != 0) {
    Rf_error("getrusage(RUSAGE_SELF, ...) failed");
  }

  REAL(out)[0] = ru.ru_maxrss;
#endif

  UNPROTECT(1);
  return out;
}
