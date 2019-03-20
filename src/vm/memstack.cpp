/*
 * memstack.cpp - Membrane Stack implementation
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

#include "memstack.h"

static void memstack_reconstruct(LmnMemStack memstack, LmnMembraneRef mem);

LmnMemStack lmn_memstack_make() { return vec_make(64); }

void lmn_memstack_free(LmnMemStack memstack) { vec_free(memstack); }

BOOL lmn_memstack_isempty(LmnMemStack memstack) {
  return vec_num(memstack) == 0;
}

void lmn_memstack_push(LmnMemStack memstack, LmnMembraneRef mem) {
  vec_push(memstack, (LmnWord)mem);
  mem->set_active(TRUE);
}

LmnMembraneRef lmn_memstack_pop(LmnMemStack memstack) {
  LmnMembraneRef m = (LmnMembraneRef)vec_pop(memstack);
  m->set_active(FALSE);
  return m;
}

LmnMembraneRef lmn_memstack_peek(LmnMemStack memstack) {
  return (LmnMembraneRef)vec_get(memstack, vec_num(memstack) - 1);
}

/* 実行膜スタックからmemを削除する。外部関数が膜の削除しようとするとき
   に、その膜がスタックに積まれている事がある。そのため、安全な削除を行
   うために、この手続きが必要になる。外部の機能を使わない通常の実行時に
   はこの手続きは必要ない*/
void lmn_memstack_delete(LmnMemStack memstack, LmnMembraneRef mem) {
  long i, j, n = (long)vec_num(memstack);

  for (i = n - 1; i >= 0; i--) {
    if ((LmnMembraneRef)vec_get(memstack, i) == mem) {
      /* 空いた分後ろの要素を前にを詰める */
      for (j = i + 1; j < n; j++) {
        vec_set(memstack, j - 1, vec_get(memstack, j));
      }
      break;
    }
  }
  vec_pop(memstack);
}

void lmn_memstack_reconstruct(LmnMemStack memstack, LmnMembraneRef mem) {
  while (!lmn_memstack_isempty(memstack))
    lmn_memstack_pop(memstack);
  memstack_reconstruct(memstack, mem);
}

static void memstack_reconstruct(LmnMemStack memstack, LmnMembraneRef mem) {
  LmnMembraneRef m;

  /* 親膜を子膜よりも先に積む */
  lmn_memstack_push(memstack, mem);
  for (m = mem->mem_child_head(); m; m = m->mem_next()) {
    memstack_reconstruct(memstack, m);
  }
}
