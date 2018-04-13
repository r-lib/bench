#include <Rinternals.h>
#include "nanotime.h"

SEXP mark_(SEXP expressions, SEXP env, SEXP min_time, SEXP num_iterations) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, Rf_xlength(expressions)));

  R_xlen_t n = INTEGER(num_iterations)[0];
  double min = REAL(min_time)[0];

  for (R_xlen_t i = 0; i < Rf_xlength(expressions); ++i) {
    SEXP expr = VECTOR_ELT(expressions, i);
    SEXP times = PROTECT(Rf_allocVector(REALSXP, n));

    long double begin;
    if (NANO_UNEXPECTED(nano_time(&begin))) {
      Rf_error("Failed to get begin time expression: %i", i);
    }
    R_xlen_t j = 0;
    for (; j < n; ++j) {
      long double start, end;
      if (NANO_UNEXPECTED(nano_time(&start))) {
        Rf_error("Failed to get start time expression: %i iteration: %i", i, j);
      }

      Rf_eval(expr, env);
      if (NANO_UNEXPECTED(nano_time(&end))) {
        Rf_error("Failed to get end time expression: %i iteration: %i", i, j);
      }

      // If we are over our time threshold for this expression break
      if (min != 0 && end - begin > min) { break; }

      REAL(times)[j] = end - start;
    }

    UNPROTECT(1);
    SET_VECTOR_ELT(out, i, Rf_lengthgets(times, j));
  }

  UNPROTECT(1);

  return out;
}


static const R_CallMethodDef CallEntries[] = {
    {"mark_", (DL_FUNC) &mark_, 4},
    {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
