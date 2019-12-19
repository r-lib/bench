#ifndef __OS__
#define __OS__

#ifdef _WIN32
#define OS_WINDOWS 1
#else
#define OS_WINDOWS 0
#endif

#ifdef __APPLE__
#define OS_MACOS 1
#else
#define OS_MACOS 0
#endif

#ifdef __linux__
#define OS_LINUX 1
#else
#define OS_LINUX 0
#endif

#endif
