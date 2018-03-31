/*
 * process_table.c
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 */

#include "process_table.h"

#include <limits.h>
#include <algorithm>

#include "process_table.hpp"


struct ProcessTbl : ProcessTable<LmnWord> {
  ProcessTbl(unsigned long size) : ProcessTable<LmnWord>(size) {};
  ProcessTbl() : ProcessTable<LmnWord>() {};
};

ProcessTableRef proc_tbl_make(void)
{
  return new ProcessTbl();
}

ProcessTableRef proc_tbl_make_with_size(unsigned long size)
{
  return new ProcessTbl(size);
}


void proc_tbl_free(ProcessTableRef p)
{
  delete p;
}


void proc_tbl_clear(ProcessTableRef p)
{
  p->clear();
}


int proc_tbl_foreach(ProcessTableRef p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg)
{
  p->foreach(func, arg);
  return 0;
}



BOOL proc_tbl_eq(ProcessTableRef a, ProcessTableRef b)
{
  return a == b;
}


unsigned long proc_tbl_get_size(ProcessTableRef p) {
  return p->size;
}

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void proc_tbl_put(ProcessTableRef p, LmnWord key, LmnWord value) {
  p->put(key, value);
}

/* テーブルにアトムを追加 */
void proc_tbl_put_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value) {
  p->put(LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加 */
void proc_tbl_put_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value) {
  p->put(lmn_mem_id(mem), value);
}

/* テーブルにハイパーリンクを追加 */
void proc_tbl_put_new_hlink(ProcessTableRef p, HyperLink *hl, LmnWord value)
{
  p->put(LMN_HL_ID(hl), value);
}

/* テーブルにkeyを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_new_atom, put_new_memを使用する. */
int proc_tbl_put_new(ProcessTableRef p, LmnWord key, LmnWord value) {
  return p->put_if_absent(key, value);
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す */
int proc_tbl_put_new_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value) {
  return p->put_if_absent(LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
int proc_tbl_put_new_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value) {
  return p->put_if_absent(lmn_mem_id(mem), value);
}

/* テーブルからkeyとそれに対応した値を削除する.
 * 通常この間数ではなくunput_atom, unput_memを使用する. */
void proc_tbl_unput(ProcessTableRef p, LmnWord key) {
  p->unput(key);
}

/* テーブルからアトムとそれに対応した値を削除する */
void proc_tbl_unput_atom(ProcessTableRef p, LmnSymbolAtomRef atom) {
  p->unput(LMN_SATOM_ID(atom));
}

/* テーブルから膜とそれに対応した値を削除する */
void proc_tbl_unput_mem(ProcessTableRef p, LmnMembraneRef mem) {
  p->unput(lmn_mem_id(mem));
}

/* テーブルのkeyに対応した値をvalueに設定し, 正の値を返す. keyがテーブルに存在しない場合は0を返す.
 * 通常この間数ではなくget_by_atom, get_by_memを使用する./ */
int proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value) {
  if (p->contains(key)) {
    if (value) p->get(key, *value);
    return 1;
  } else {
    return 0;
  }
}

/* テーブルのアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにatomが存在しない場合は0を返す */
int proc_tbl_get_by_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord *value) {
  return proc_tbl_get(p, LMN_SATOM_ID(atom), value);
}


/* テーブルの膜memに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにmemが存在しない場合は0を返す */
int proc_tbl_get_by_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord *value) {
  return proc_tbl_get(p, lmn_mem_id(mem), value);
}

/* テーブルのハイパーリンクhlに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにhlが存在しない場合は0を返す */
int proc_tbl_get_by_hlink(ProcessTableRef p, HyperLink *hl, LmnWord *value)
{
  return proc_tbl_get(p, LMN_HL_ID(hl), value);
}

BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key) {
  return p->contains(key);
}

/* テーブルにアトムatomに対応する値が設定されている場合, 正の値を返す. */
BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSymbolAtomRef atom) {
  return proc_tbl_contains(p, LMN_SATOM_ID(atom));
}

/* テーブルの膜memに対応する値が設定されている場合、正の値を返す. */
BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembraneRef mem) {
  return proc_tbl_contains(p, lmn_mem_id(mem));
}
