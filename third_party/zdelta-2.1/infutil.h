/* infutil.h -- types and macros common to blocks and codes
 * Copyright (C) 1995-1998 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change. Applications should only use zdlib.h.

   file modified by Dimitre Trendafilov (2002)
 */

/* zdelta:
 *
 * modified:
 *          struct inflate_block_state
 * added:
 *          --
 * removed:
 *          --
 */
#ifndef _INFUTIL_H
#define _INFUTIL_H

#include <stdio.h>

typedef enum {
  TYPE,   /* get type bits (3, including end bit) */
  LENS,   /* get lengths for stored */
  STORED, /* processing stored block */
  TABLE,  /* get table lengths */
  BTREE,  /* get bit lengths tree for a dynamic block */
  DTREE,  /* get length, distance trees for a dynamic block */
  CODES,  /* processing fixed or dynamic block */
  DRY,    /* output remaining window bytes */
  DONE,   /* finished last block, done */
  BAD
} /* got a data error--stuck here */
inflate_block_mode;

/* inflate blocks semi-private state */
struct inflate_blocks_state {
  /* mode */
  inflate_block_mode mode; /* current inflate_block mode */

  /* mode dependent information */
  union {
    uInt left; /* if STORED, bytes left to copy */
    struct {
      uInt table;       /* table lengths (14 bits) */
      uInt index;       /* index into blens (or border) */
      uIntf *blens;     /* bit lengths of codes */
      uInt bb;          /* bit length tree depth */
      inflate_huft *tb; /* bit length decoding tree */
    } trees;            /* if DTREE, decoding info for trees */
    struct {
      inflate_codes_statef *codes;
    } decode; /* if CODES, current state */
  } sub;      /* submode */
  uInt last;  /* true if this block is the last block */

  /* mode independent information */
  uInt bitk;           /* bits in bit buffer */
  uLong bitb;          /* bit buffer */
  inflate_huft *hufts; /* single malloc for tree space */
  Bytef *window;       /* sliding window */
  Bytef *end;          /* one byte after sliding window */
  Bytef *read;         /* window read pointer */
  Bytef *write;        /* window write pointer */
  check_func checkfn;  /* check function */
  uLong check;         /* check on output */

  /* zdelta data */
  unsigned long rwptr[ZD_RPN * REFNUM];
  unsigned long stable[ZD_RPN * REFNUM];
};

/* defines for inflate input/output */
/*   update pointers and return */
#ifdef ZDEBUG
#define ZD_WRAP \
  { printf("m=0, WRAP\n"); }
#define ZD_FLUSH \
  { printf("still m=0, FLUSH\n"); }
#define ZD_LEAVE \
  { printf("still m=0, LEAVE\n"); }
#define ZD_RETIF \
  { printf("return inflate_flush();\n"); }
#else
#define ZD_WRAP
#define ZD_FLUSH
#define ZD_LEAVE
#define ZD_RETIF
#endif

#define UPDBITS  \
  {              \
    s->bitb = b; \
    s->bitk = k; \
  }
#define UPDIN                      \
  {                                \
    z->avail_in = n;               \
    z->total_in += p - z->next_in; \
    z->next_in = p;                \
  }
#define UPDOUT \
  { s->write = q; }
#define UPDATE \
  { UPDBITS UPDIN UPDOUT }
#define LEAVE \
  { UPDATE ZD_RETIF return inflate_flush(s, z, r); }
/*   get bytes and bits */
#define LOADIN       \
  {                  \
    p = z->next_in;  \
    n = z->avail_in; \
    b = s->bitb;     \
    k = s->bitk;     \
  }
#define NEEDBYTE \
  {              \
    if (n)       \
      r = ZD_OK; \
    else         \
      LEAVE      \
  }
#define NEXTBYTE (n--, *p++)
#define NEEDBITS(j)                \
  {                                \
    while (k < (j)) {              \
      NEEDBYTE;                    \
      b |= ((uLong)NEXTBYTE) << k; \
      k += 8;                      \
    }                              \
  }
#define DUMPBITS(j) \
  {                 \
    b >>= (j);      \
    k -= (j);       \
  }
/*   output bytes */
#define WAVAIL (uInt)(q < s->read ? s->read - q - 1 : s->end - q)
#define LOADOUT       \
  {                   \
    q = s->write;     \
    m = (uInt)WAVAIL; \
  }
#define WRAP                                   \
  {                                            \
    if (q == s->end && s->read != s->window) { \
      q = s->window;                           \
      m = (uInt)WAVAIL;                        \
    }                                          \
  }
#define FLUSH                          \
  {                                    \
    UPDOUT r = inflate_flush(s, z, r); \
    LOADOUT                            \
  }
#define NEEDOUT                           \
  {                                       \
    if (m == 0) {                         \
      ZD_WRAP WRAP if (m == 0) {          \
        ZD_FLUSH FLUSH WRAP if (m == 0) { \
          ZD_LEAVE LEAVE                  \
        }                                 \
      }                                   \
    }                                     \
    r = ZD_OK;                            \
  }
#define OUTBYTE(a)    \
  {                   \
    *q++ = (Byte)(a); \
    m--;              \
  }
/*   load local pointers */
#define LOAD \
  { LOADIN LOADOUT }

/* masks for lower bits (size given to avoid silly warnings with Visual C++) */
extern uInt inflate_mask[17];

/* copy as much as possible from the sliding window to the output area */
extern int inflate_flush OF((inflate_blocks_statef *, zd_streamp, int));

struct zd_internal_state {
  int dummy;
}; /* for buggy compilers */

#endif
