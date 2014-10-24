/*
 * util.h - common utility functions and macros
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$
 */

#ifndef LMN_UTIL_H
#define LMN_UTIL_H

#include "lmntal.h"
#include "error.h"


/** ----------------------
 *  ASCII code for printer
 */
enum ESC_CODE {
  CODE__HIGH_LIGHT           =  0x01,
  CODE__UNDER_LINE           =  0x04,
  CODE__DASH_LINE            =  0x05,
  CODE__REVERSAL             =  0x07,
  CODE__FORECOLOR_BLACK      =  0x1e,
  CODE__FORECOLOR_RED        =  0x1f,
  CODE__FORECOLOR_GREEN      =  0x20,
  CODE__FORECOLOR_YELLOW     =  0x21,
  CODE__FORECOLOR_DEEPBLUE   =  0x22,
  CODE__FORECOLOR_PURPLE     =  0x23,
  CODE__FORECOLOR_LIGHTBLUE  =  0x24,
  CODE__FORECOLOR_WHITE      =  0x25,
  CODE__BACKCOLOR_BLACK      =  0x28,
  CODE__BACKCOLOR_RED        =  0x29,
  CODE__BACKCOLOR_GREEN      =  0x2a,
  CODE__BACKCOLOR_YELLOW     =  0x2b,
  CODE__BACKCOLOR_DEEPBLUE   =  0x2c,
  CODE__BACKCOLOR_PURPLE     =  0x2d,
  CODE__BACKCOLOR_LIGHTBLUE  =  0x2e,
  CODE__BACKCOLOR_GRAY       =  0x2f,
};

#define __ESC_START__  "\x1b["
#define __ESC_END__    "m"

static inline void esc_code_clear(){
  printf("%s%s", __ESC_START__, __ESC_END__);
  return;
}

static inline void esc_code_add(int code)
{
  printf("%s%d%s", __ESC_START__, code, __ESC_END__);
}

static inline void esc_code_clear_f(FILE *f){
  fprintf(f,"%s%s", __ESC_START__, __ESC_END__);
  return;
}

static inline void esc_code_add_f(FILE *f,int code)
{
  fprintf(f,"%s%d%s", __ESC_START__, code, __ESC_END__);
}



/** ----------------------
 *  byte operation
 */

/* See http://isthe.com/chongo/tech/comp/fnv/ */
#if SIZEOF_LONG == 4
#  define FNV_PRIME 16777619UL
#  define FNV_BASIS 2166136261UL
#elif SIZEOF_LONG == 8
#  define FNV_PRIME  1099511628211UL
#  define FNV_BASIS 14695981039346656037UL
#else
#  error "not supported"
#endif

static inline unsigned long lmn_byte_hash(const unsigned char *str, long i) {
  unsigned long hval;
  /*
   * FNV-1a hash each octet in the buffer
   */
  hval = FNV_BASIS;
  while (--i >= 0) {
    /* xor the bottom with the current octet */
    hval ^= (unsigned int)str[i];
    /* multiply by the FNV magic prime mod 2^32 or 2^64 */
    hval *= FNV_PRIME;
  }

  return hval;
}

/* 正: a ＞ b
 * ０: a = b
 * 負: a ＜ b */
static inline int lmn_byte_cmp(const unsigned char *a, long alen,
                               const unsigned char *b, long blen)
{
  if (alen != blen) {
    return alen - blen;
  } else {
    return memcmp(a, b, alen);
  }
}




/** ----------------------
 *  else
 */

/* 配列の要素数 */
#define ARY_SIZEOF(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))

char *int_to_str(long n);
int comp_int_f(const void *a, const void *b);
int comp_int_greater_f(const void *a_, const void *b_);

/* n以上で最小の2の倍数を返す */
static inline unsigned long round2up(unsigned long n) {
  unsigned int v = 1;
  while (v && v < n) {
    v <<= 1;
  }
  if (v == 0) {
    lmn_fatal("too large size");
  }
  return v;
}


#endif /* !LMN_UTIL_H */
