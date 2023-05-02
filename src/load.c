#include "os.h"
#include <Rinternals.h>

#if OS_WINDOWS
// Currently does nothing, there is an example of emulating linux style load on
// Windows at https://github.com/giampaolo/psutil/pull/1485
#elif OS_MACOS || OS_LINUX
#include <stdlib.h>
#endif

SEXP bench_load_average_(void) {

  SEXP out = PROTECT(Rf_allocVector(REALSXP, 3));
  REAL(out)[0] = NA_REAL;
  REAL(out)[1] = NA_REAL;
  REAL(out)[2] = NA_REAL;

#if OS_MACOS
  double loadavg[3];
  int num_load = getloadavg(loadavg, 3);
  if (num_load <= 0) {
    Rf_error("getloadavg() failed");
  }

  for (int i = 0; i < num_load; ++i) {
    REAL(out)[i] = loadavg[i];
  }
#endif

  UNPROTECT(1);
  return out;
}
