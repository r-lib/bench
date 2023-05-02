#ifndef NANOTIME_H
#define NANOTIME_H

#include "Rinternals.h"

long double real_time(void);
long double process_cpu_time(void);
long double expr_elapsed_time(SEXP expr, SEXP env);

#endif
