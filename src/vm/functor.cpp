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
#include "atom.h"
#include "element/element.h"
#include "lmntal.h"
#include "symbol.h"

struct PredefinedFunctor {
  LmnFunctor  id;
  BOOL        special;
  char const *name;
  LmnArity    arity;
};

/* 予約されたファンクタの定義 */
struct PredefinedFunctor predefined_functors[] = {
    /* プロキシは第三引数を親膜に使用するので三引数として登録する */
    {LMN_IN_PROXY_FUNCTOR, TRUE, IN_PROXY_NAME, 3},
    {LMN_OUT_PROXY_FUNCTOR, TRUE, OUT_PROXY_NAME, 3},
    {LMN_STAR_PROXY_FUNCTOR, TRUE, STAR_PROXY_NAME, 3},
    {LMN_UNIFY_FUNCTOR, FALSE, UNIFY_ATOM_NAME, 2},
    {LMN_LIST_FUNCTOR, FALSE, CONS_ATOM_NAME, 3},
    {LMN_NIL_FUNCTOR, FALSE, NIL_ATOM_NAME, 1},
    {LMN_RESUME_FUNCTOR, TRUE, RESUME_ATOM_NAME, 0},
    {LMN_ARITHMETIC_IADD_FUNCTOR, FALSE, IADD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_ISUB_FUNCTOR, FALSE, ISUB_ATOM_NAME, 3},
    {LMN_ARITHMETIC_IMUL_FUNCTOR, FALSE, IMUL_ATOM_NAME, 3},
    {LMN_ARITHMETIC_IDIV_FUNCTOR, FALSE, IDIV_ATOM_NAME, 3},
    {LMN_ARITHMETIC_MOD_FUNCTOR, FALSE, IMOD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FADD_FUNCTOR, FALSE, FADD_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FSUB_FUNCTOR, FALSE, FSUB_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FMUL_FUNCTOR, FALSE, FMUL_ATOM_NAME, 3},
    {LMN_ARITHMETIC_FDIV_FUNCTOR, FALSE, FDIV_ATOM_NAME, 3},
    {LMN_UNARY_PLUS_FUNCTOR, FALSE, UNARY_PLUS_NAME, 1},
    {LMN_UNARY_MINUS_FUNCTOR, FALSE, UNARY_MINUS_NAME, 1},
    {LMN_MEM_EQ_FUNCTOR, FALSE, MEM_EQ_ATOM_NAME, 5},
    {LMN_TRUE_FUNCTOR, FALSE, TRUE_ATOM_NAME, 1},
    {LMN_FALSE_FUNCTOR, FALSE, FALSE_ATOM_NAME, 1},
    /* hyperlinkは第二引数にHyperLink構造体へのポインタを埋め込むため二引数として登録する
     */
    {LMN_EXCLAMATION_FUNCTOR, FALSE, EXCLAMATION_NAME, 2},
#ifdef USE_FIRSTCLASS_RULE
    {LMN_COLON_MINUS_FUNCTOR, FALSE, COLON_MINUS_ATOM_NAME, 3},
#endif
};

LmnFunctorTable *lmn_functor_table;

LmnFunctorEntry *LmnFunctorTable::get_entry(unsigned int f) { return &entry[f]; }

unsigned int LmnFunctorTable::get_size() { return size; }

unsigned int LmnFunctorTable::get_next_id() { return next_id; }
/* ファンクタの比較 */
int LmnFunctorTable::functor_cmp(LmnFunctorEntry *x, LmnFunctorEntry *y) {
  return !(x->module == y->module && x->name == y->name && x->arity == y->arity);
}
long LmnFunctorTable::functor_hash(LmnFunctorEntry *x) { return x->module * 31 * 31 + x->name * 31 + x->arity; }

static struct st_hash_type type_functorhash = {(st_cmp_func)LmnFunctorTable::functor_cmp,
                                               (st_hash_func)LmnFunctorTable::functor_hash};

/* for debug */
#ifdef DEBUG

void LmnFunctorTable::print() {
  int i, n;
  fprintf(stdout, "next_id==%u\n", next_id);
  n = this->size;
  for (i = 0; i < n; i++) {
    fprintf(stdout, "entry[%2d]== %s_%d\n", i, lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, i)),
            LMN_FUNCTOR_ARITY(lmn_functor_table, i));
  }
}

void LmnFunctorTable::functor_printer(LmnFunctor f) {
  fprintf(stdout, "fid=%d[ %s_%d ]\n", f, lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)),
          LMN_FUNCTOR_ARITY(lmn_functor_table, f));
}
#endif

LmnFunctorTable::LmnFunctorTable() {
  int       i;
  int const predefined_size = ARY_SIZEOF(predefined_functors);

  this->functor_id_tbl = st_init_table(&type_functorhash);

  this->size    = predefined_size;
  this->entry   = LMN_NALLOC<LmnFunctorEntry>(size);
  this->next_id = predefined_size;

  /* 予約されたファンクタを順番に登録していく */
  for (i = 0; i < predefined_size; i++) {
    struct PredefinedFunctor *f = &predefined_functors[i];
    register_functor(f->id, f->special, ANONYMOUS, lmn_intern(f->name), f->arity);
  }
}

int LmnFunctorTable::functor_entry_free(LmnFunctorEntry *e) {
  LMN_FREE(e);
  return ST_DELETE;
}

LmnFunctorTable::~LmnFunctorTable() {
  st_foreach(this->functor_id_tbl, (st_iter_func)&LmnFunctorTable::functor_entry_free, 0);
  st_free_table(this->functor_id_tbl);
  LMN_FREE(entry);
}

LmnFunctorEntry *LmnFunctorTable::lmn_id_to_functor(int functor_id) const {
  LmnFunctorEntry *entry;

  if (st_lookup(this->functor_id_tbl, (st_data_t)functor_id, (st_data_t *)&entry))
    return entry;
  else
    return NULL;
}

void LmnFunctorTable::register_functor(int id, BOOL special, lmn_interned_str module, lmn_interned_str name,
                                       int arity) {
  struct LmnFunctorEntry *entry = LMN_MALLOC<struct LmnFunctorEntry>();

  entry->special = special;
  entry->module  = module;
  entry->name    = name;
  entry->arity   = arity;

  st_insert(this->functor_id_tbl, (st_data_t)entry, (st_data_t)id);
  /* idの位置にファンクタのデータをコピー */
  this->entry[id] = *entry;
}

/* ファンクタのIDを返す */
LmnFunctor LmnFunctorTable::functor_intern(BOOL special, lmn_interned_str module, lmn_interned_str name, int arity) {
  st_data_t       id;
  LmnFunctorEntry entry;

  entry.special = special;
  entry.module  = module;
  entry.name    = name;
  entry.arity   = arity;

  /* すでにテーブル内にあるならそれを返す */
  if (st_lookup(this->functor_id_tbl, (st_data_t)&entry, &id))
    return id;
  else {
    struct LmnFunctorEntry *new_entry;

    /* 必要ならばサイズを拡張 */
    while (this->next_id >= this->size) {
      this->size  *= 2;
      this->entry = LMN_REALLOC<LmnFunctorEntry>(this->entry, this->size);
    }

    /* idはデータを格納する配列のインデックス */
    id = this->next_id++;
    /* idの位置にファンクタのデータをコピー */
    this->entry[id] = entry;

    /* ファンクタとIDの対応をテーブルに格納する */
    new_entry  = LMN_MALLOC<struct LmnFunctorEntry>();
    *new_entry = entry;
    st_insert(this->functor_id_tbl, (st_data_t)new_entry, (st_data_t)id);

    return id;
  }
}

LmnFunctor LmnFunctorTable::intern(lmn_interned_str module, lmn_interned_str name, int arity) {
  return functor_intern(FALSE, module, name, arity);
}
