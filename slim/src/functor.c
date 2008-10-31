/*
 * functor.c - functor operations 
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lang@ueda.info.waseda.ac.jp>
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
 * $Id: functor.c,v 1.5 2008/09/29 05:23:40 taisuke Exp $
 */

#include "lmntal.h"
#include "functor.h"
#include "st.h"
#include "atom.h"
#include "symbol.h"

struct LmnFunctorTable lmn_functor_table;

/* prototypes */

static LmnFunctor functor_intern(BOOL special, lmn_interned_str module, lmn_interned_str name, int arity);
int functor_entry_free(LmnFunctorEntry *e);
const LmnFunctorEntry *lmn_id_to_functor(int functor_id);

/* ファンクタの比較 */
static int functor_cmp(LmnFunctorEntry *x, LmnFunctorEntry *y)
{
  return
    !(x->module == y->module &&
      x->name == y->name &&
      x->arity == y->arity);
}

static int functor_hash(LmnFunctorEntry *x)
{
  return abs((x->module*31*31 + x->name*31 + x->arity));
}

static struct st_hash_type type_functorhash = {
  functor_cmp,
  functor_hash
};

/* ファンクタ構造体からIDへの対応を要素に持つのテーブル */
st_table *functor_id_tbl;

void lmn_functor_tbl_init()
{
  functor_id_tbl = st_init_table(&type_functorhash);

  lmn_functor_table.num_entry = 0;
  lmn_functor_table.size = 128;
  lmn_functor_table.entry = LMN_NALLOC(LmnFunctorEntry, lmn_functor_table.size);

  /* 予約されたファンクタを順番に登録していく */
  /* プロキシは第三引数を親膜に使用するので三引数として登録する */
  functor_intern(TRUE, ANONYMOUS, lmn_intern("$in"), 3);
  functor_intern(TRUE, ANONYMOUS, lmn_intern("$out"), 3);
  functor_intern(TRUE, ANONYMOUS, lmn_intern("$*"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("="), 2);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("."), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("[]"), 1);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("$res"), 1);
  /* for the system ruleset executing arithmetic operations */  
  functor_intern(FALSE, ANONYMOUS, lmn_intern("+"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("-"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("*"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("/"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("mod"), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("+."), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("-."), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("*."), 3);
  functor_intern(FALSE, ANONYMOUS, lmn_intern("/."), 3);
}

int functor_entry_free(LmnFunctorEntry *e)
{
  LMN_FREE(e);
  return ST_DELETE;
}

void lmn_functor_tbl_destroy()
{
  st_foreach(functor_id_tbl, functor_entry_free, 0);
  st_free_table(functor_id_tbl);
  LMN_FREE(lmn_functor_table.entry);
}

const LmnFunctorEntry *lmn_id_to_functor(int functor_id)
{
  LmnFunctorEntry *entry;

  if (st_lookup(functor_id_tbl, (st_data_t)functor_id, (st_data_t *)&entry))
    return entry;
  else return NULL;
}

/* ファンクタのIDを返す */
static LmnFunctor functor_intern(BOOL special, lmn_interned_str module, lmn_interned_str name, int arity)
{
  int id;
  LmnFunctorEntry entry;

  entry.special = special;
  entry.module = module;
  entry.name = name;
  entry.arity = arity;

  /* すでにテーブル内にあるならそれを返す */
  if (st_lookup(functor_id_tbl, &entry, (st_data_t *)&id)) return id;
  else {
    struct LmnFunctorEntry *new_entry;

    /* 必要ならばサイズを拡張 */
    while (lmn_functor_table.num_entry >= lmn_functor_table.size) {
      lmn_functor_table.size *= 2;
      lmn_functor_table.entry = LMN_REALLOC(LmnFunctorEntry,
                                            lmn_functor_table.entry,
                                            lmn_functor_table.size);
    }

    /* idはデータを格納する配列のインデックス */
    id = lmn_functor_table.num_entry++;
    /* idの位置にファンクタのデータをコピー */
    lmn_functor_table.entry[id] = entry;

    /* ファンクタとIDの対応をテーブルに格納する */
    new_entry = LMN_MALLOC(struct LmnFunctorEntry);
    *new_entry = entry;
    st_insert(functor_id_tbl, new_entry, (st_data_t)id);

    return id;
  }
}

LmnFunctor lmn_functor_intern(lmn_interned_str module, lmn_interned_str name, int arity)
{
  return functor_intern(FALSE, module, name, arity);
}

