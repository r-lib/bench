#include <Rinternals.h>
#include "nanotime.h"

SEXP mark_(SEXP expr, SEXP env, SEXP min_time, SEXP num_iterations) {
  R_xlen_t n = INTEGER(num_iterations)[0];
  double min = REAL(min_time)[0];

  SEXP times = PROTECT(Rf_allocVector(REALSXP, n));

  long double begin;
  if (NANO_UNEXPECTED(nano_time(&begin))) {
    Rf_error("Failed to get begin time");
  }
  R_xlen_t i = 0;
  for (; i < n; ++i) {
    long double start, end;
    if (NANO_UNEXPECTED(nano_time(&start))) {
      Rf_error("Failed to get start time iteration: %i", i);
    }

    Rf_eval(expr, env);
    if (NANO_UNEXPECTED(nano_time(&end))) {
      Rf_error("Failed to get end time iteration: %i", i);
    }

    // If we are over our time threshold for this expression break
    if (min != 0 && end - begin > min) { break; }

    REAL(times)[i] = end - start;
  }

  times = Rf_lengthgets(times, i);

  UNPROTECT(1);

  return times;
}

static const R_CallMethodDef CallEntries[] = {
    {"mark_", (DL_FUNC) &mark_, 4},
    {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
