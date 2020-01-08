#include "os.h"

#include <Rinternals.h>

#if OS_WINDOWS
#include <windows.h>
#define PSAPI_VERSION 1
#include <psapi.h>
#elif OS_MACOS
#include <sys/time.h>
#include <sys/resource.h>
#include <mach/mach.h>
#include <mach/mach_init.h>
#elif OS_LINUX
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#endif

#define FAILURE -1

#if OS_LINUX
/*  read_proc_file is derived from https://github.com/cran/memuse/blob/f2be8bc6f6af3771161c6e58ea5b6c1dd0eafcd7/src/meminfo/src/platform.c#L44
 *  Copyright (c) 2014-2017 Drew Schmidt
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
int read_proc_file(const char *file, uint64_t *val, char *field, int fieldlen) {
  size_t len = 0;
  char *tmp = NULL;
  uint64_t value = FAILURE;

  *val = 0L;

  FILE* fp = fopen(file, "r");

  if (fp != NULL) {
    while (getline(&tmp, &len, fp) >= 0) {
      if (strncmp(tmp, field, fieldlen) == 0) {
        sscanf(tmp, "%*s%" SCNu64, &value);
        break;
      }
    }

    fclose(fp);
    free(tmp);

    if (value != (uint64_t)FAILURE) {
      *val = value;
      return 0;
    }
  }

  return FAILURE;
}
#endif

SEXP bench_process_memory_() {

  SEXP out = PROTECT(Rf_allocVector(REALSXP, 2));
  REAL(out)[0] = NA_REAL;
  REAL(out)[1] = NA_REAL;

#if OS_LINUX
  uint64_t current_size = 0;
  uint64_t peak_size = 0;

  if(read_proc_file("/proc/self/status", &current_size, "VmSize:", 7) != 0) {
    Rf_error("read_proc_file(...) failed");
  }

  if(read_proc_file("/proc/self/status", &peak_size, "VmPeak:", 7) != 0) {
    Rf_error("read_proc_file(...) failed");
  }

  REAL(out)[0] = current_size * 1024;
  REAL(out)[1] = peak_size * 1024;

#elif OS_WINDOWS
  PROCESS_MEMORY_COUNTERS pmc;
  if (!GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc))) {
    Rf_error("GetProcessMemoryInfo(...) failed");
  }

  REAL(out)[0] = pmc.WorkingSetSize;
  REAL(out)[1] = pmc.PeakWorkingSetSize;

#elif OS_MACOS

  struct task_basic_info info;
  mach_msg_type_number_t info_count = TASK_BASIC_INFO_COUNT;

  if (task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&info, &info_count) != 0) {
    Rf_error("task_info(TASK_BASIC_INFO, ...) failed");
  }

  struct rusage ru;
  if (getrusage(RUSAGE_SELF, &ru) != 0) {
    Rf_error("getrusage(RUSAGE_SELF, ...) failed");
  }

  REAL(out)[0] = info.resident_size;
  REAL(out)[1] = ru.ru_maxrss;
#endif

  UNPROTECT(1);
  return out;
}
