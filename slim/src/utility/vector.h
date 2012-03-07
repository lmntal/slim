/*
 * vector.h
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
 * $Id: vector.h,v 1.9 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_VECTOR_H
#define LMN_VECTOR_H

#include "lmntal.h"

struct Vector {
  LmnWord* tbl;
  unsigned int num, cap;
};


typedef struct Vector *PVector;
typedef LmnWord vec_data_t;

#define vec_cap(V)      ((V)->cap)
#define vec_num(V)      ((V)->num)
#define vec_is_empty(V) ((V)->num == 0)

static inline Vector *vec_init(Vector *vec, unsigned int init_size);
static inline Vector *vec_make(unsigned int init_size);
static inline void    vec_push(Vector *vec, LmnWord keyp);
static inline LmnWord vec_pop(Vector *vec);
static inline LmnWord vec_peek(const Vector *vec);
static inline void    vec_set(Vector *vec, unsigned int index, LmnWord keyp);
static inline LmnWord vec_get(const Vector *vec, unsigned int index);
static inline LmnWord vec_last(Vector *vec);
static inline void    vec_clear(Vector *vec);
static inline void    vec_destroy(Vector *vec);
static inline void    vec_free(Vector *vec);
static inline unsigned long vec_space(Vector *v);
static inline unsigned long vec_space_inner(Vector *v);

LmnWord vec_pop_n(Vector *vec, unsigned int n);
BOOL    vec_contains(const Vector *vec, LmnWord keyp);
Vector *vec_copy(Vector *vec);
void    vec_reverse(Vector *vec);
void    vec_resize(Vector *vec, unsigned int size, vec_data_t val);
void    vec_sort(const Vector *vec,
                 int (*compare)(const void*, const void*));


/* init */
static inline Vector *vec_init(Vector *vec, unsigned int init_size) {
  vec->tbl = LMN_NALLOC(LmnWord, init_size);
  vec->num = 0;
  vec->cap = init_size;
  return vec;
}

/* make */
static inline Vector *vec_make(unsigned int init_size) {
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
static inline void vec_push(Vector *vec, LmnWord keyp) {
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
static inline LmnWord vec_pop(Vector *vec) {
  LmnWord ret;
  LMN_ASSERT(vec->num > 0);
  /* Stackとして利用する場合, reallocが頻繁に発生してしまう.
   * Stackなのでサイズの増減は頻繁に発生するものだが, 頻繁なreallocはパフォーマンスに影響する.
   * >>とりあえず<< サイズの下限値を設ける. LmnStackなる構造を別に作るべきかも. (gocho) */
  if (vec->num <= vec->cap/2 && vec->cap > 1024) {
    vec_reduce(vec);
  }
  ret = vec_get(vec, (vec->num-1));
  vec->num--;
  return ret;
}

/* peek */
static inline LmnWord vec_peek(const Vector *vec) {
  return vec_get(vec, vec->num - 1);
}

/* set */
static inline void vec_set(Vector *vec, unsigned int index, LmnWord keyp) {
  LMN_ASSERT(index < vec->cap);
  (vec->tbl)[index] = keyp;
}

/* get */
static inline LmnWord vec_get(const Vector *vec, unsigned int index) {
  LMN_ASSERT(index < vec->num);
  return(vec->tbl[index]);
}

static inline LmnWord vec_last(Vector *vec)
{
  return vec->tbl[vec->num-1];
}

/* pop all elements from vec */
static inline void vec_clear(Vector *vec) {
  vec->num = 0;
}

/* destroy */
static inline void vec_destroy(Vector *vec) {
  LMN_FREE(vec->tbl);
}

/* free */
static inline void vec_free(Vector *vec) {
  LMN_FREE(vec->tbl);
  LMN_FREE(vec);
}

static inline unsigned long vec_space_inner(Vector *v) {
  return vec_cap(v) * sizeof(vec_data_t);
}

static inline unsigned long vec_space(Vector *v) {
  return sizeof(struct Vector) + vec_space_inner(v);
}

#endif /* LMN_VECTOR_H */
