#include <Rcpp.h>
#include <Rinternals.h>
#include "nanotime.h"

using namespace Rcpp;

// [[Rcpp::export]]
List mark_(List expressions, Environment env, double min_time, R_xlen_t num_iterations) {
  List out(expressions.length());

  for (R_xlen_t i = 0; i < Rf_xlength(expressions); ++i) {
    SEXP expr = VECTOR_ELT(expressions, i);
    NumericVector times(num_iterations);

    long double begin;
    if (NANO_UNEXPECTED(nano_time(&begin))) {
      Rcpp::stop("Failed to get begin time expression: %i", i);
    }
    R_xlen_t j = 0;
    for (; j < num_iterations; ++j) {
      long double start, end;
      if (NANO_UNEXPECTED(nano_time(&start))) {
        Rcpp::stop("Failed to get start time expression: %i iteration: %i", i, j);
      }

      Rf_eval(expr, env);
      if (NANO_UNEXPECTED(nano_time(&end))) {
        Rcpp::stop("Failed to get end time expression: %i iteration: %i", i, j);
      }

      // If we are over our time threshold for this expression break
      if (min_time != 0 && end - begin > min_time) { break; }

      times[j] = end - start;
    }

    out[i] = Rf_lengthgets(times, j);
  }
  return out;
}
