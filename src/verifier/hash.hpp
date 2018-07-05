#ifndef _HASH_H
#define _HASH_H

#include<stdint.h>

typedef uint32_t Hash;

/* #define WEAK_HASH */

#ifdef WEAK_HASH
#define FNV_PRIME (0UL)
#define OFFSET_BASIS (0UL)
#define ADD_INIT (0UL)
#define MUL_INIT (0UL)
#define IS_DIFFERENCE_APPLICATION_MODE (FALSE)
#else
#define FNV_PRIME (16777619UL)
#define OFFSET_BASIS (2166136261UL)
#define ADD_INIT (2166136261UL)
#define MUL_INIT (2166136261UL)
#define IS_DIFFERENCE_APPLICATION_MODE (TRUE)
#endif

extern const Hash BITS_INVERSE_TABLE[65536];

#define INVERSE(N,M) {                    \
  M = BITS_INVERSE_TABLE[N & 0x0000ffff]; \
                                          \
  M |= -(~(-N*M))*M;                      \
}                                         \

#endif
