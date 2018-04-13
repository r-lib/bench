#include "nanotime.h"
#include <stdio.h>

#ifdef __MACH__
#include <mach/mach.h>
#include <mach/clock.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

NANO nano_return_t nano_timespec(struct timespec *now)
{
#ifdef __MACH__
	clock_serv_t clock_service;
	mach_timespec_t mach_time;

	if (host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &clock_service) != 0) {
		return NANO_FAILURE;
	}

	if (clock_get_time(clock_service, &mach_time) != 0) {
		return NANO_FAILURE;
	}

	if (mach_port_deallocate(mach_task_self(), clock_service) != 0) {
		return NANO_FAILURE;
	}

	now->tv_sec = mach_time.tv_sec;
	now->tv_nsec = mach_time.tv_nsec;
#else
	if (clock_gettime(CLOCK_REALTIME, now) != 0) {
		return NANO_FAILURE;
	}
#endif

	return NANO_SUCCESS;
}

NANO nano_return_t nano_second(unsigned long *second)
{
	struct timespec now;

	if (NANO_UNEXPECTED(nano_timespec(&now))) {
		return NANO_FAILURE;
	}

	*second = now.tv_sec * 1E9 + now.tv_nsec;

	return NANO_SUCCESS;
}

NANO nano_return_t nano_time(long double *time)
{
	struct timespec now;

	if (NANO_UNEXPECTED(nano_timespec(&now))) {
		return NANO_FAILURE;
	}

	*time = (long double) now.tv_sec + now.tv_nsec / 1E9;

	return NANO_SUCCESS;
}
