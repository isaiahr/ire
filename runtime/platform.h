#ifndef __PLATFORM_H__
#define __PLATFORM_H__

/**
 platform.h - header file resolves to the target platform the compilation is for
*/

#ifdef LINUX_AARCH64
#include "linux_aarch64.h"
#endif

#ifdef LINUX_AMD64
#include "linux_amd64.h"
#endif

#endif
