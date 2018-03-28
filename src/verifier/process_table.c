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


void proc_tbl_init(ProcessTableRef p)
{
  proc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

void proc_tbl_init_with_size(ProcessTableRef p, unsigned long size)
{
  p->n    = 0;
  p->size = size;
  p->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
  p->tbl = LMN_CALLOC(LmnWord *, p->num_buckets);
}

ProcessTableRef proc_tbl_make(void)
{
  return proc_tbl_make_with_size(PROC_TBL_DEFAULT_SIZE);
}

ProcessTableRef proc_tbl_make_with_size(unsigned long size)
{
  ProcessTableRef p = LMN_MALLOC(struct ProcessTbl);
  proc_tbl_init_with_size(p, size);
  return p;
}

void proc_tbl_destroy(ProcessTableRef p)
{
  for (int i = 0; i < p->num_buckets; i++) {
    LMN_FREE(p->tbl[i]);
  }
  LMN_FREE(p->tbl);
}


void proc_tbl_free(ProcessTableRef p)
{
  proc_tbl_destroy(p);
  LMN_FREE(p);
}


void proc_tbl_clear(ProcessTableRef p)
{
  p->n = 0;
  for (int i = 0; i < p->num_buckets; i++) {
    memset(p->tbl[i], 0xff, sizeof(LmnWord) * PROC_TBL_BUCKETS_SIZE);
  }
}


int proc_tbl_foreach(ProcessTableRef p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg)
{
  unsigned long n = 0;

  for (int i = 0; i < p->num_buckets; i++) {
    if (!p->tbl[i]) continue;
    for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && n < process_tbl_entry_num(p); j++) {
      if (p->tbl[i][j] == ULONG_MAX) continue;
      func(i * PROC_TBL_BUCKETS_SIZE + j, p->tbl[i][j], arg);
      n++;
    }
  }
  return 0;
}



BOOL proc_tbl_eq(ProcessTableRef a, ProcessTableRef b)
{
  if (a->n != b->n) return FALSE;
  else {
    unsigned int a_checked = 0;

    for (int i = 0; i < a->num_buckets; i++) {
      if (!a->tbl[i] && !b->tbl[i]) continue;

      for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && a_checked < a->n; j++) {
        LmnWord va = (a->tbl[i]) ? a->tbl[i][j] : ULONG_MAX;
        LmnWord vb = (b->tbl[i]) ? b->tbl[i][j] : ULONG_MAX;
        if (va != vb) return FALSE;
        if (va != ULONG_MAX) a_checked++;
      }
    }

    return TRUE;
  }
}


void proc_tbl_expand_sub(ProcessTableRef p, unsigned long n)
{
  unsigned int org_n = p->num_buckets;
  while (p->size <= n) p->size *= 2;
  p->num_buckets = p->size / PROC_TBL_BUCKETS_SIZE + 1;
  if (org_n < p->num_buckets) {
    p->tbl = LMN_REALLOC(LmnWord *, p->tbl, p->num_buckets);
    memset(p->tbl + org_n, 0, sizeof(LmnWord *) * (p->num_buckets - org_n));
  }

  unsigned int b = n / PROC_TBL_BUCKETS_SIZE;
  if (b < p->num_buckets && p->tbl[b]) return;
  p->tbl[b] = LMN_NALLOC(LmnWord, PROC_TBL_BUCKETS_SIZE);
  memset(p->tbl[b], 0xffU, sizeof(LmnWord) * PROC_TBL_BUCKETS_SIZE);
}


/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void proc_tbl_put(ProcessTableRef p, LmnWord key, LmnWord value) {
  p->n++;
# ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
# endif
  proc_tbl_expand(p, key);
  process_tbl_entry(p, key) = value;
}

/* テーブルにアトムを追加 */
void proc_tbl_put_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value) {
  proc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加 */
void proc_tbl_put_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value) {
  proc_tbl_put(p, lmn_mem_id(mem), value);
}

/* テーブルにハイパーリンクを追加 */
void proc_tbl_put_new_hlink(ProcessTableRef p, HyperLink *hl, LmnWord value)
{
  proc_tbl_put(p, LMN_HL_ID(hl), value);
}

/* テーブルにkeyを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_new_atom, put_new_memを使用する. */
int proc_tbl_put_new(ProcessTableRef p, LmnWord key, LmnWord value) {
  p->n++;
#ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
#endif
  proc_tbl_expand(p, key);
  if (process_tbl_entry(p, key) != ULONG_MAX) return 0;
  process_tbl_entry(p, key) = value;
  return 1;
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す */
int proc_tbl_put_new_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value) {
  return proc_tbl_put_new(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
int proc_tbl_put_new_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value) {
  return proc_tbl_put_new(p, lmn_mem_id(mem), value);
}

/* テーブルからkeyとそれに対応した値を削除する.
 * 通常この間数ではなくunput_atom, unput_memを使用する. */
void proc_tbl_unput(ProcessTableRef p, LmnWord key) {
  p->n--;
  proc_tbl_expand(p, key);
  process_tbl_entry(p, key) = ULONG_MAX;
}

/* テーブルからアトムとそれに対応した値を削除する */
void proc_tbl_unput_atom(ProcessTableRef p, LmnSymbolAtomRef atom) {
  proc_tbl_unput(p, LMN_SATOM_ID(atom));
}

/* テーブルから膜とそれに対応した値を削除する */
void proc_tbl_unput_mem(ProcessTableRef p, LmnMembraneRef mem) {
  proc_tbl_unput(p, lmn_mem_id(mem));
}

/* テーブルのkeyに対応した値をvalueに設定し, 正の値を返す. keyがテーブルに存在しない場合は0を返す.
 * 通常この間数ではなくget_by_atom, get_by_memを使用する./ */
int proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value) {
  if (proc_tbl_contains(p, key)) {
    if (value) *value = process_tbl_entry(p, key);
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
  return key < p->size && p->tbl[key / PROC_TBL_BUCKETS_SIZE] && process_tbl_entry(p, key) != ULONG_MAX;
}

/* テーブルにアトムatomに対応する値が設定されている場合, 正の値を返す. */
BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSymbolAtomRef atom) {
  return proc_tbl_contains(p, LMN_SATOM_ID(atom));
}

/* テーブルの膜memに対応する値が設定されている場合、正の値を返す. */
BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembraneRef mem) {
  return proc_tbl_contains(p, lmn_mem_id(mem));
}
