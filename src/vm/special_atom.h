/*
 * special_atom.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

/* スペシャルアトムはシンボルアトムや整数などのアトムとは異なった、拡張可能なアトムである。
 * アトムにLMNtalでは表現できない特別なデータを持たせ、そのデータに対して操作を行うなど、
 * LMNtalの機能を拡張することを可能にする。
 *
 * 新しいスペシャルアトムのタイプを定義する際にはコールバック関数を登録する。
 * コールバック関数はアトムのコピーや解放、表示、グラウンド検査などを行う際に呼ばれる。
 * その他に、独自のデータを格納する事が出来るようになっている。
 * 登録時に、そのタイプのIDが生成され、登録以後はそのIDを使用して
 * スペシャルアトムの操作やデータにアクセスをする。*/

#ifndef LMN_SPECIAL_ATOM_H
#define LMN_SPECIAL_ATOM_H

/**
 * @ingroup VM
 * @defgroup SpecialAtom
 * @{
 */

#include "element/element.h"
#include "lmntal.h"

#define LMN_SP_ATOM(atom) ((struct LmnSPAtomHeader *)(atom))

/* アトムのタイプのID */
#define LMN_SP_ATOM_TYPE(X) (LMN_SP_ATOM(X)->type)
#define LMN_SP_ATOM_SET_TYPE(obj, t) (LMN_SP_ATOM((obj))->type = (t))

using f_copy = std::function<void *(void *)>;
using f_eq = std::function<BOOL(void *, void *)>;
using f_free = std::function<void(void *)>;
using f_dump = std::function<void(void *, LmnPortRef)>;
using f_is_ground = std::function<BOOL(void *)>;
using f_encode = std::function<std::vector<uint8_t>(void *)>;
using f_decode = std::function<void *(std::vector<uint8_t> const &)>;

struct SpecialAtomCallback {
  lmn_interned_str name;
  f_copy copy;
  f_free free;
  f_dump dump;
  f_eq eq;
  f_is_ground is_ground;
  f_encode encode;
  f_decode decode;
};

void sp_atom_init(void);
void sp_atom_finalize(void);

/* 新しいスペシャルアトムのタイプを登録する。登録されたタイプのIDを返す */
int lmn_sp_atom_register(const char *name, /* move owner */
                         f_copy f_copy, f_free f_free, f_eq f_eq, f_dump f_dump,
                         f_is_ground f_is_ground);
int lmn_sp_atom_register(const char *name, /* move owner */
                         f_copy f_copy, f_free f_free, f_eq f_eq, f_dump f_dump,
                         f_is_ground f_is_ground, f_encode encoder, f_decode decoder);

struct SpecialAtomCallback *sp_atom_get_callback(int id);

#define SP_ATOM_NAME(ATOM) (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->name)
#define SP_ATOM_COPY(ATOM)                                                     \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->copy((void *)(ATOM)))

#define SP_ATOM_FREE(ATOM)                                                     \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->free((void *)(ATOM)))
#define SP_ATOM_DUMP(ATOM, PORT)                                               \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->dump((void *)(ATOM), (PORT)))
#define SP_ATOM_IS_GROUND(ATOM)                                                \
  (sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM))->is_ground((void *)(ATOM)))
#define SP_ATOM_EQ(ATOM1, ATOM2)                                               \
  (LMN_SP_ATOM_TYPE(ATOM1) == LMN_SP_ATOM_TYPE(ATOM2) &&                       \
   sp_atom_get_callback(LMN_SP_ATOM_TYPE(ATOM1))                               \
       ->eq((void *)(ATOM1), (void *)(ATOM2)))
static inline f_encode sp_atom_encoder(void *atom) {
  return sp_atom_get_callback(LMN_SP_ATOM_TYPE(atom))->encode;
}
static inline f_decode sp_atom_decoder(LmnByte type) {
  return sp_atom_get_callback(type)->decode;
}

/* @} */

#endif /* LMN_SPECIALATOM_H */
