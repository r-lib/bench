#ifndef NANOTIME_H
#define NANOTIME_H

#ifdef __cplusplus
extern "C" {
#endif

#include <time.h>
#ifdef __MACH__
#define NANO extern
#else
#define NANO
#endif

typedef enum {
	NANO_FAILURE = -1,
	NANO_SUCCESS = 0
} nano_return_t;

#define NANO_EXPECTED(X) (X) == NANO_SUCCESS
#define NANO_UNEXPECTED(X) (X) != NANO_SUCCESS

NANO nano_return_t nano_second(unsigned long *second);
NANO nano_return_t nano_time(long double *time);
NANO nano_return_t nano_timespec(struct timespec *now);
#endif

#ifdef __cplusplus
}
#endif
