/*
 * process_table.cpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 */

#include "process_table.h"

#include <algorithm>
#include <limits.h>

namespace slim {
template <> ProcessID process_id<LmnWord>(LmnWord id) { return id; }

template <> ProcessID process_id<LmnSymbolAtomRef>(LmnSymbolAtomRef atom) {
  return atom->get_id();
}

template <> ProcessID process_id<LmnMembraneRef>(LmnMembraneRef mem) {
  return mem->mem_id();
}

template <> ProcessID process_id<HyperLink *>(HyperLink *hl) {
  return LMN_HL_ID(hl);
}
} // namespace slim



void ProcessTbl::tbl_clear() { this->clear(); }
int ProcessTbl::tbl_foreach(int (*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg) {
  this->foreach (func, arg);
  return 0;
}
BOOL ProcessTbl::tbl_eq(ProcessTableRef b) { return this == b; }

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void ProcessTbl::proc_tbl_put(LmnWord key, LmnWord value) {
  this->put(key, value);
}

/* テーブルにアトムを追加 */
void ProcessTbl::proc_tbl_put_atom(LmnSymbolAtomRef atom, LmnWord value) {
  this->put(atom, value);
}

/* テーブルに膜を追加 */
void ProcessTbl::proc_tbl_put_mem(LmnMembraneRef mem, LmnWord value) {
  this->put(mem, value);
}

/* テーブルにハイパーリンクを追加 */
void ProcessTbl::put_new_hlink(HyperLink *hl, LmnWord value) {
  this->put(hl, value);
}
int ProcessTbl::put_new(LmnWord key, LmnWord value) {
  return this->put_if_absent(key, value);
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す
 */
int ProcessTbl::put_new_atom(LmnSymbolAtomRef atom, LmnWord value) {
  return this->put_if_absent(atom, value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
int ProcessTbl::put_new_mem(LmnMembraneRef mem, LmnWord value) {
  return this->put_if_absent(mem, value);
}

/* テーブルからkeyとそれに対応した値を削除する.
 * 通常この関数ではなくunput_atom, unput_memを使用する. */
void ProcessTbl::proc_tbl_unput(LmnWord key) { this->unput(key); }

/* テーブルからアトムとそれに対応した値を削除する */
void ProcessTbl::unput_atom(LmnSymbolAtomRef atom) {
  this->unput(atom);
}

/* テーブルから膜とそれに対応した値を削除する */
void proc_tbl_unput_mem(ProcessTableRef p, LmnMembraneRef mem) {
  p->unput(mem);
}

void ProcessTbl::unput_hlink(HyperLink *hl) {  // extended
  this->unput(hl);
}


/* テーブルのkeyに対応した値をvalueに設定し, 正の値を返す.
 * keyがテーブルに存在しない場合は0を返す. 通常この間数ではなくget_by_atom,
 * get_by_memを使用する./ */
int proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value) {
  if (!p->contains(key))
    return false;

  if (value) *value = (*p)[slim::process_id(key)];
  return true;
}

/* テーブルのアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにatomが存在しない場合は0を返す */
int proc_tbl_get_by_atom(ProcessTableRef p, LmnSymbolAtomRef atom,
                         LmnWord *value) {
  if (!p->contains(atom))
    return false;

  if (value) *value = (*p)[slim::process_id(atom)];
  return true;
}

/* テーブルの膜memに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにmemが存在しない場合は0を返す */
int proc_tbl_get_by_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord *value) {
  if (!p->contains(mem))
    return false;

  if (value) *value = (*p)[slim::process_id(mem)];
  return true;
}

/* テーブルのハイパーリンクhlに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにhlが存在しない場合は0を返す */
int proc_tbl_get_by_hlink(ProcessTableRef p, HyperLink *hl, LmnWord *value) {
  if (!p->contains(hl))
    return false;

  if (value) *value = (*p)[slim::process_id(hl)];
  return true;
}

BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key) {
  return p->contains(key);
}

/* テーブルにアトムatomに対応する値が設定されている場合, 正の値を返す. */
BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSymbolAtomRef atom) {
  return p->contains(atom);
}

/* テーブルの膜memに対応する値が設定されている場合、正の値を返す. */
BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembraneRef mem) {
  return p->contains(mem);
}

/* process table のダンプ，デバッグ用 */
void proc_tbl_dump(const char* name, ProcessTableRef map) {
  fprintf(stderr,"proc_tbl %s items:\n", name);
  map->tbl_foreach([](LmnWord k, LmnWord v, LmnWord _arg) {
		     fprintf(stderr,"  key=%ld, value=%ld\n", k, v);
		     return 1;
		   }, (LmnWord)0);
}

/* シンボルアトムを入れた process table のダンプ，デバッグ用 */
void proc_tbl_symbol_atom_dump(const char* name, ProcessTableRef map) {
  fprintf(stderr,"proc_tbl %s items:\n", name);
  map->tbl_foreach([](LmnWord k, LmnWord v, LmnWord _arg) {
		     LmnFunctor f = ((LmnSymbolAtomRef)v)->get_functor();
		     fprintf(stderr,"  key=%ld, value=%s\n", k, LMN_FUNCTOR_STR(f));
		     return 1;
		   }, (LmnWord)0);
}
