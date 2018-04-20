#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include "nanotime.h"
#include <unistd.h>
#include <errno.h>
#include <string.h>

double get_overhead(SEXP env) {
  long double overhead = 100.0;
  for (int i = 0; i < 10; ++i) {
    long double diff = expr_elapsed_time(R_NilValue, env);
    if (diff > 0 && diff < overhead) {
      overhead = diff;
    }
  }

  if (overhead == 100.0) {
    overhead = 0.0;
  }

  return overhead;
}

SEXP mark_(SEXP expr, SEXP env, SEXP min_time, SEXP min_itr, SEXP max_itr) {
  R_xlen_t min_itr_ = INTEGER(min_itr)[0];
  R_xlen_t max_itr_ = INTEGER(max_itr)[0];
  double min_time_ = REAL(min_time)[0];

  SEXP out = PROTECT(Rf_allocVector(REALSXP, max_itr_));

  long double total = 0;

  double overhead = get_overhead(env);

  R_xlen_t i = 0;
  for (; i < max_itr_ && ( (total < min_time_) || i < min_itr_); ++i) {

    long double elapsed = expr_elapsed_time(expr, env);

    // 1E is record separator 
    REprintf("\x1E");
    REAL(out)[i] = elapsed - overhead;
    total+=elapsed;
  }

  out = Rf_xlengthgets(out, i);

  UNPROTECT(1);

  return out;
}

SEXP system_time_(SEXP expr, SEXP env) {
  double process_begin = process_cpu_time();
  double real_begin = real_time();
  Rf_eval(expr, env);
  double real_end = real_time();
  double process_end = process_cpu_time();

  SEXP out = PROTECT(Rf_allocVector(REALSXP, 2));
  REAL(out)[0] = process_end - process_begin;
  REAL(out)[1] = real_end - real_begin;

  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
    {"mark_", (DL_FUNC) &mark_, 5},
    {"system_time_", (DL_FUNC) &system_time_, 2},
    {NULL, NULL, 0}
};

void R_init_bench(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
