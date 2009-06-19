/*
 * special_atom.h
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
 * $Id$
 */

/* スペシャルアトムはシンボルアトムや整数などのアトムとは異なった、拡張
   可能なアトムである。アトムにLMNtalでは表現できない特別なデータを持た
   せ、そのデータに対して操作を行うなど、LMNtalの機能を拡張することを可
   能にする。

   新しいスペシャルアトムのタイプを定義する際にはコールバック関数を登録
   する。コールバック関数はアトムのコピーや解放、表示、グラウンド検査な
   どを行う際に呼ばれる。その他に、独自のデータを格納する事が出来るよう
   になっている。登録時に、そのタイプのIDが生成され、登録以後はそのIDを
   使用してスペシャルアトムの操作やデータにアクセスをする。*/

#ifndef LMN_SPECIAL_ATOM_H
#define LMN_SPECIAL_ATOM_H

#include "lmntal.h"
#include "slim_header/port.h"

#define LMN_SP_ATOM(atom) ((struct LmnSPAtomHeader *)(atom))

/* アトムのタイプのID */
#define LMN_SP_ATOM_TYPE(X) (LMN_SP_ATOM(X)->type)
#define LMN_SP_ATOM_SET_TYPE(obj, t) (LMN_SP_ATOM((obj))->type = (t))

typedef void *(*f_copy)(void*);
typedef BOOL (*f_eq)(void*, void*);
typedef void (*f_free)(void*);
typedef void (*f_dump)(void *, LmnPort);
typedef BOOL (*f_is_ground)(void*);

struct SpecialAtomCallback {
  lmn_interned_str name;
  f_copy copy;
  f_free free;
  f_dump dump;
  f_eq eq;
  f_is_ground is_ground;
};

void sp_atom_init(void);
void sp_atom_finalize(void);

/* 新しいスペシャルアトムのタイプを登録する。登録されたタイプのIDを返す */
int lmn_sp_atom_register(const char *name,   /* move owner */
                         f_copy f_copy,
                         f_free f_free,
                         f_eq f_eq,
                         f_dump f_dump,
                         f_is_ground f_is_ground);

struct SpecialAtomCallback * sp_atom_get_callback(int id);

#define SP_ATOM_NAME(ATOM)  \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->name)
#define SP_ATOM_COPY(ATOM) \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->copy((void *)(ATOM)))

#define SP_ATOM_FREE(ATOM) \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->free((void *)(ATOM)))
#define SP_ATOM_DUMP(ATOM, PORT) \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->dump((void *)(ATOM), (PORT)))
#define SP_ATOM_IS_GROUND(ATOM) \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->is_ground((void *)(ATOM)))
#define SP_ATOM_EQ(ATOM1, ATOM2)                                       \
  (LMN_SP_ATOM_TYPE(ATOM1) == LMN_SP_ATOM_TYPE(ATOM2) && \
   sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM1))->eq((void *)(ATOM1), (void *)(ATOM2)))

#endif /* LMN_SPECIALATOM_H */

