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

/* pop Nth element */
LmnWord vec_pop_n(Vector *vec, unsigned int n) {
  unsigned int i;
  LmnWord ret;

  LMN_ASSERT(vec->num > 0 && n >= 0 && n < vec->num);

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


/* contains */
BOOL vec_contains(const Vector *vec, LmnWord keyp) {
  unsigned int i = 0;
  while (i < vec_num(vec)) {
    if (vec_get(vec, i++) == (LmnWord)keyp) {
      return TRUE;
    }
  }
  return FALSE;
}

/* ベクタのサイズを size に変更し、新規に追加された項目を val に設定する*/
void vec_resize(Vector *vec, unsigned int size, vec_data_t val)
{
  unsigned int i;

  while (size > vec->cap) {
    vec_extend(vec);
  }

  /* 追加された項目を val に設定 */
  for (i=vec->num; i<size; i++) {
    vec->tbl[i] = val;
  }
  vec->num = size;
}

void vec_sort(const Vector *vec,
              int (*compare)(const void*, const void*))
{
  qsort(vec->tbl, vec->num, sizeof(vec_data_t), compare);
}

/* Vectorに詰んだ要素を逆順に並べ直す */
void vec_reverse(Vector *vec)
{
  unsigned int r, l;

  r = 0;
  l = vec_num(vec) - 1;

  while (r < l) {
    vec_data_t tmp = vec->tbl[r];
    vec_set(vec, r, vec->tbl[l]);
    vec_set(vec, l, tmp);
    r++;
    l--;
  }
}

Vector *vec_copy(Vector *vec)
{
  int i;
  Vector *new_vec;

  new_vec = vec_make(vec->num > 0 ? vec->num : 1);

  for (i = 0; i < vec_num(vec); i++) {
    new_vec->tbl[i] = vec->tbl[i];
  }
  new_vec->num = vec->num;
  return new_vec;
}

