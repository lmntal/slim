/*
 * vector.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: vector.c,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#include "vector.h"

/* init */
Vector *vec_init(Vector *vec, unsigned int init_size) {
  vec->tbl = LMN_NALLOC(LmnWord, init_size);
  vec->num = 0;
  vec->cap = init_size;
  return vec;
}

/* make */
Vector *vec_make(unsigned int init_size) {
  LMN_ASSERT(init_size > 0);
  Vector* vec = LMN_MALLOC(Vector);
  return vec_init(vec, init_size);
}

/* extend (static) */
static inline void vec_extend(Vector *vec) {
  vec->cap *= 2;
  vec->tbl = LMN_REALLOC(LmnWord, vec->tbl, vec->cap);
}

/* push */
void vec_push(Vector *vec, LmnWord keyp) {
  if(vec->num == vec->cap) {
    vec_extend(vec);
  }
  (vec->tbl)[vec->num] = keyp;
  vec->num++;
}

/* reduce (static) */
static inline void vec_reduce(Vector *vec) {
  vec->cap /= 2;
  vec->tbl = LMN_REALLOC(LmnWord, vec->tbl, vec->cap);
}

/* pop */
LmnWord vec_pop(Vector *vec) {
  LmnWord ret;
  LMN_ASSERT(vec->num > 0);
  if(vec->num <= vec->cap/2) {
    vec_reduce(vec);
  }
  ret = vec_get(vec, (vec->num-1));
  vec->num--;
  return ret;
}

/* pop Nth element */
LmnWord vec_pop_n(Vector *vec, unsigned int n) {
  unsigned int i;
  LmnWord ret;
  LMN_ASSERT(vec->num > 0);
  assert(n >= 0 && n < vec->num);
  if (vec->num <= vec->cap/2) {
    vec_reduce(vec);
  }
  ret = vec_get(vec, n);
  for (i = n; i < vec->num-1; ++i) {
    vec_set(vec, i, vec_get(vec, i+1));      
  }
  vec->num--;
  return ret;
}

/* peek */
inline LmnWord vec_peek(Vector *vec) {
  return vec_get(vec, vec->num-1);
}

/* set */
inline void vec_set(Vector *vec, unsigned int index, LmnWord keyp) {
  assert(index < vec->num);
  (vec->tbl)[index] = keyp;
}

/* get */
inline LmnWord vec_get(Vector *vec, unsigned int index) {
  LMN_ASSERT(index < vec->num);
  return(vec->tbl[index]);
}

/* contains */
BOOL vec_contains(Vector *vec, LmnWord keyp) {
  unsigned int i = 0;
  while (i < vec_num(vec)) {
    if (vec_get(vec, i++) == (LmnWord)keyp) {
      return TRUE;
    }
  }
  return FALSE;
}

/* pop all elements from vec */
inline void vec_clear(Vector *vec) {
  vec->num = 0;
}

/* destroy */
inline void vec_destroy(Vector *vec) {
  free(vec->tbl);
}

/* free */
inline void vec_free(Vector *vec) {
  free(vec->tbl);
  free(vec);
}

