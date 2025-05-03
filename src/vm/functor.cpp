/*
 * functor.cpp - functor operations
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
 * $Id: functor.c,v 1.5 2008/09/29 05:23:40 taisuke Exp $
 */

#include "functor.h"
#include "element/element.h"
#include "symbol.h"

struct PredefinedFunctor {
  LmnFunctor id;
  BOOL special;
  const char *name;
  LmnArity arity;
};

/* 予約されたファンクタの定義 */
struct PredefinedFunctor predefined_functors[] = {
    /* プロキシは第三引数を親膜に使用するので三引数として登録する */
    {LMN_IN_PROXY_FUNCTOR, true, IN_PROXY_NAME, 3},
    {LMN_OUT_PROXY_FUNCTOR, true, OUT_PROXY_NAME, 3},
    {LMN_STAR_PROXY_FUNCTOR, true, STAR_PROXY_NAME, 3},
    {LMN_UNIFY_FUNCTOR, false, UNIFY_ATOM_NAME, 2},
    {LMN_LIST_FUNCTOR, false, CONS_ATOM_NAME, 3},
    {LMN_NIL_FUNCTOR, false, NIL_ATOM_NAME, 1},
    {LMN_RESUME_FUNCTOR, true, RESUME_ATOM_NAME, 0},
    {LMN_ARITHMETIC_IADD_FUNCTOR, false, IADD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_ISUB_FUNCTOR, false, ISUB_ATOM_NAME, 3},
    {LMN_ARITHMETIC_IMUL_FUNCTOR, false, IMUL_ATOM_NAME, 3},
    {LMN_ARITHMETIC_IDIV_FUNCTOR, false, IDIV_ATOM_NAME, 3},
    {LMN_ARITHMETIC_MOD_FUNCTOR, false, IMOD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FADD_FUNCTOR, false, FADD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FSUB_FUNCTOR, false, FSUB_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FMUL_FUNCTOR, false, FMUL_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FDIV_FUNCTOR, false, FDIV_ATOM_NAME, 3},
    {LMN_UNARY_PLUS_FUNCTOR, false, UNARY_PLUS_NAME, 1},
    {LMN_UNARY_MINUS_FUNCTOR, false, UNARY_MINUS_NAME, 1},
    {LMN_MEM_EQ_FUNCTOR, false, MEM_EQ_ATOM_NAME, 5},
    {LMN_TRUE_FUNCTOR, false, TRUE_ATOM_NAME, 1},
    {LMN_FALSE_FUNCTOR, false, FALSE_ATOM_NAME, 1},
    /* hyperlinkは第二引数にHyperLink構造体へのポインタを埋め込むため二引数として登録する
     */
    {LMN_EXCLAMATION_FUNCTOR, false, HYPERLINK_NAME, 2},
#ifdef USE_FIRSTCLASS_RULE
    {LMN_COLON_MINUS_FUNCTOR, false, COLON_MINUS_ATOM_NAME, 3},
#endif
};

LmnFunctorTable * lmn_functor_table;

/* for debug */
#ifdef DEBUG

void LmnFunctorTable::print() {
  int i, n;
  fprintf(stdout, "next_id==%u\n", next_id);
  n = this->size;
  for (i = 0; i < n; i++) {
    fprintf(stdout, "entry[%2d]== %s_%d\n", i,
            lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, i)), LMN_FUNCTOR_ARITY(lmn_functor_table, i));
  }
}

void LmnFunctorTable::functor_printer(LmnFunctor f) {
  fprintf(stdout, "fid=%d[ %s_%d ]\n", f,
          lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)), LMN_FUNCTOR_ARITY(lmn_functor_table, f));
}
#endif

LmnFunctorTable::LmnFunctorTable() {
  const int predefined_size = ARY_SIZEOF(predefined_functors);

  this->size = predefined_size;
  this->next_id = predefined_size;
  this->entry = LMN_NALLOC(LmnFunctorEntry, size);

  /* 予約されたファンクタを順番に登録していく */
  for (const auto &f : predefined_functors) {
    LmnFunctorEntry entry {
      .special = f.special,
      .module = ANONYMOUS,
      .name = lmn_intern(f.name),
      .arity = f.arity,
    };

    this->functor_id_map.emplace(entry, f.id);
    this->entry[f.id] = entry;
  }
}

LmnFunctorTable::~LmnFunctorTable() {
  LMN_FREE(entry);
}

/* ファンクタのIDを返す */
LmnFunctor LmnFunctorTable::functor_intern(BOOL special, lmn_interned_str module,
                                 lmn_interned_str name, int arity) {
  LmnFunctorEntry entry = {
    .special = special,
    .module = module,
    .name = name,
    .arity = static_cast<LmnArity>(arity),
  };

  /* すでにテーブル内にあるならそれを返す */
  auto p = this->functor_id_map.find(entry);
  if (p != this->functor_id_map.end()) {
    return p->second;
  }

  /* 必要ならばサイズを拡張 */
  while (this->next_id >= this->size) {
    this->size *= 2;
    this->entry = LMN_REALLOC(LmnFunctorEntry, this->entry, this->size);
  }

  /* idはデータを格納する配列のインデックス */
  auto id = this->next_id++;
  /* idの位置にファンクタのデータをコピー */
  this->entry[id] = entry;

  /* ファンクタとIDの対応をテーブルに格納する */
  this->functor_id_map.emplace(entry, id);

  return id;
}
