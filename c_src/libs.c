#define _GNU_SOURCE
#define SOKOL_IMPL
#include "sokol_time.h"

#define BLIB_IMPLEMENTATION
#include "barg.h"

#define CUTE_NET_IMPLEMENTATION
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wstrict-prototypes"
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#endif
#include "cute_net.h"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

#define CKIT_IMPLEMENTATION
#include "ckit.h"
