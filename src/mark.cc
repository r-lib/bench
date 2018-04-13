#include <Rcpp.h>
#include <Rinternals.h>
#include "nanotime.h"

using namespace Rcpp;

// [[Rcpp::export]]
List mark_(List expressions, Environment env, R_xlen_t n = 1) {
  List out(expressions.length());

  for (R_xlen_t i = 0; i < Rf_xlength(expressions); ++i) {
    SEXP expr = VECTOR_ELT(expressions, i);
    NumericVector times(n);
    for (R_xlen_t j = 0; j < n; ++j) {
      long double start, end;
      if (NANO_UNEXPECTED(nano_time(&start))) {
        Rcpp::stop("Failed to get start time expression: %i iteration: %i", i, j);
      }
      Rf_eval(expr, env);
      if (NANO_UNEXPECTED(nano_time(&end))) {
        Rcpp::stop("Failed to get end time expression: %i iteration: %i", i, j);
      }
      times[j] = end - start;
    }
    out[i] = times;
  }
  return out;
}
