/*
 * memstack.c - Membrane Stack implementation
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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

#include "lmntal.h"
#include "membrane.h"
#include "slim_header/memstack.h"

inline LmnMemStack lmn_memstack_make()
{
  return vec_make(64);
}

inline void lmn_memstack_free(LmnMemStack memstack)
{
  vec_free(memstack);
}

inline BOOL lmn_memstack_isempty(LmnMemStack memstack)
{
  return vec_num(memstack) == 0;
}

inline void lmn_memstack_push(LmnMemStack memstack, LmnMembrane *mem)
{
  vec_push(memstack, (LmnWord)mem);
  lmn_mem_set_active(mem, TRUE);
}

inline LmnMembrane *lmn_memstack_pop(LmnMemStack memstack)
{
  LmnMembrane *m = (LmnMembrane *)vec_pop(memstack);
  lmn_mem_set_active(m, FALSE);
  return m;
}

inline LmnMembrane *lmn_memstack_peek(LmnMemStack memstack)
{
  return (LmnMembrane *)vec_get(memstack, vec_num(memstack)-1);
}

/* 実行膜スタックからmemを削除する。外部関数が膜の削除しようとするとき
   に、その膜がスタックに積まれている事がある。そのため、安全な削除を行
   うために、この手続きが必要になる。外部の機能を使わない通常の実行時に
   はこの手続きは必要ない*/
void lmn_memstack_delete(LmnMemStack memstack, LmnMembrane *mem)
{
  unsigned long i, j, n = vec_num(memstack);
  
  if (lmn_env.nd) return;
  
  for (i = n-1; i >= 0; i--) {
    if ((LmnMembrane *)vec_get(memstack, i) == mem) {
      /* 空いた分後ろの要素を前にを詰める */
      for (j = i+1; j < n; j++) {
        vec_set(memstack, j-1, vec_get(memstack, j));
      }
      break;
    }
  }
  vec_pop(memstack);
}

void mem_react_cxt_init(struct ReactCxt *cxt)
{
  RC_SET_MODE(cxt, REACT_MEM_ORIENTED);
  cxt->v = LMN_MALLOC(struct MemReactCxtData);
  ((struct MemReactCxtData *)cxt->v)->memstack = lmn_memstack_make();
}

void mem_react_cxt_destroy(struct ReactCxt *cxt)
{
  lmn_memstack_free(((struct MemReactCxtData *)cxt->v)->memstack);
  LMN_FREE(cxt->v);
}
