#ifndef _RE_MACROS_H
#define _RE_MACROS_H
#include <re-config.h>

#ifdef RE_DEBUG_BUILD
#include <stdio.h>

#define dprintf(...) fprintf(stderr, __VA_ARGS__)

#else // RE_DEBUG_BUILD

#define dprintf(...) do {} while (0)

#endif // RE_DEBUG_BUILD

#define STRLEN(x) ((sizeof(x) / sizeof(char)) - 1)

#endif // _RE_MACROS_H
