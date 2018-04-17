#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include "nanotime.h"
#include <unistd.h>

SEXP mark_(SEXP expr, SEXP env, SEXP min_time, SEXP num_iterations, SEXP logfile) {
  R_xlen_t n = INTEGER(num_iterations)[0];
  double min = REAL(min_time)[0];

  const char* log = CHAR(STRING_ELT(logfile, 0));
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, Rf_allocVector(REALSXP, n));
  SET_VECTOR_ELT(out, 1, Rf_allocVector(STRSXP, n));

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

    freopen(log, "w", stderr);

    // Actually evaluate the R code
    Rf_eval(expr, env);

    FILE* fp = fopen(log, "r");
    char* buffer = NULL;
    size_t len;
    ssize_t bytes_read = getdelim( &buffer, &len, '\0', fp);
    if ( bytes_read != -1) {
      SET_STRING_ELT(VECTOR_ELT(out, 1), i, Rf_mkChar(buffer));
      free(buffer);
    }
    fclose(fp);

    if (NANO_UNEXPECTED(nano_time(&end))) {
      Rf_error("Failed to get end time iteration: %i", i);
    }

    // If we are over our time threshold for this expression break
    if (min != 0 && end - begin > min) { break; }

    REAL(VECTOR_ELT(out, 0))[i] = end - start;
  }

  SET_VECTOR_ELT(out, 0, Rf_lengthgets(VECTOR_ELT(out, 0), i));
  SET_VECTOR_ELT(out, 1, Rf_lengthgets(VECTOR_ELT(out, 1), i));

  freopen("/dev/tty", "a", stderr);

  UNPROTECT(1);

  return out;
}

double real_time() {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_REALTIME, ...) failed");
  }
  return ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

double process_cpu_time() {
  struct timespec ts;
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) != 0) {
    Rf_error("clock_gettime(CLOCK_PROCESS_CPUTIME_ID, ...) failed");
  }
  return ts.tv_sec + (double)ts.tv_nsec * 1e-9;
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
