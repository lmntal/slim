/*
 * simply_process_table.c
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

#include "simply_process_table.h"

#include "vm/process_table.hpp"

struct SimplyProcTbl : ProcessTable<BYTE> {};



void sproc_tbl_init_with_size(SimplyProcessTableRef p, unsigned long size)
{
  p->n   = 0;
  p->size = size;
  p->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
  p->tbl = LMN_CALLOC(BYTE *, p->num_buckets);
}

void sproc_tbl_init(SimplyProcessTableRef p)
{
  sproc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

SimplyProcessTableRef sproc_tbl_make(void)
{
  return sproc_tbl_make_with_size(PROC_TBL_DEFAULT_SIZE);
}

SimplyProcessTableRef sproc_tbl_make_with_size(unsigned long size)
{
  SimplyProcessTableRef p = LMN_MALLOC(struct SimplyProcTbl);
  sproc_tbl_init_with_size(p, size);
  return p;
}

void sproc_tbl_destroy(SimplyProcessTableRef p)
{
  for (int i = 0; i < p->num_buckets; i++) {
    LMN_FREE(p->tbl[i]);
  }
  LMN_FREE(p->tbl);
}

void sproc_tbl_free(SimplyProcessTableRef p)
{
  sproc_tbl_destroy(p);
  LMN_FREE(p);
}


void sproc_tbl_expand(SimplyProcessTableRef p, unsigned long n) {
  unsigned int org_n = p->num_buckets;
  while (p->size <= n) p->size *= 2;
  p->num_buckets = p->size / PROC_TBL_BUCKETS_SIZE + 1;

  if (org_n < p->num_buckets) {
    p->tbl = LMN_REALLOC(BYTE *, p->tbl, p->num_buckets);
    memset(p->tbl + org_n, 0, sizeof(BYTE *) * (p->num_buckets - org_n));
  }

  unsigned int b = n / PROC_TBL_BUCKETS_SIZE;
  if (b < p->num_buckets && p->tbl[b]) return;
  p->tbl[b] = LMN_NALLOC(BYTE, PROC_TBL_BUCKETS_SIZE);
  memset(p->tbl[b], SPROC_TBL_INIT_V, sizeof(BYTE) * PROC_TBL_BUCKETS_SIZE);
}

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void sproc_tbl_put(SimplyProcessTableRef p, LmnWord key, BYTE value) {
#ifdef DEBUG
  if (value == SPROC_TBL_INIT_V) lmn_fatal("i can't put this value");
#endif
  sproc_tbl_expand(p, key);

  if (sproc_tbl_entry(p, key) == SPROC_TBL_INIT_V) {
    p->n++;
  }

  sproc_tbl_entry(p, key) = value;
}

void sproc_tbl_put_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom, BYTE value) {
  sproc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

void sproc_tbl_put_mem(SimplyProcessTableRef p, LmnMembraneRef mem, BYTE value) {
  sproc_tbl_put(p, lmn_mem_id(mem), value);
}

void sproc_tbl_unput(SimplyProcessTableRef p, LmnWord key) {
  if (!sproc_tbl_contains(p, key)) return;
  p->n--;
  sproc_tbl_entry(p, key) = SPROC_TBL_INIT_V;
}

void sproc_tbl_unput_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom) {
  sproc_tbl_unput(p, LMN_SATOM_ID(atom));
}

void sproc_tbl_unput_mem(SimplyProcessTableRef p, LmnMembraneRef mem) {
  sproc_tbl_unput(p, lmn_mem_id(mem));
}

int sproc_tbl_get(SimplyProcessTableRef p, LmnWord key, BYTE *value) {
  if (sproc_tbl_contains(p, key)) {
    if (value) *value = sproc_tbl_entry(p, key);
    return 1;
  } else {
    return 0;
  }
}

int sproc_tbl_get_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom, BYTE *value) {
  return sproc_tbl_get(p, LMN_SATOM_ID(atom), value);
}

int sproc_tbl_get_by_mem(SimplyProcessTableRef p, LmnMembraneRef mem, BYTE *value) {
  return sproc_tbl_get(p, lmn_mem_id(mem), value);
}

BOOL sproc_tbl_contains(SimplyProcessTableRef p, LmnWord key) {
  return key < p->size && p->tbl[key / PROC_TBL_BUCKETS_SIZE] && sproc_tbl_entry(p, key) != SPROC_TBL_INIT_V;
}

BOOL sproc_tbl_contains_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom) {
  return sproc_tbl_contains(p, LMN_SATOM_ID(atom));
}

BOOL sproc_tbl_contains_mem(SimplyProcessTableRef p, LmnMembraneRef mem) {
  return sproc_tbl_contains(p, lmn_mem_id(mem));
}

BOOL sproc_tbl_get_flag(SimplyProcessTableRef p, LmnWord key, BYTE flag) {
  return sproc_tbl_contains(p, key) ? (sproc_tbl_entry(p, key) & flag) : 0;
}

BOOL sproc_tbl_get_flag_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef key, LmnWord flag) {
  return sproc_tbl_get_flag(p, LMN_SATOM_ID(key), flag);
}

BOOL sproc_tbl_get_flag_by_mem(SimplyProcessTableRef p, LmnMembraneRef key, LmnWord flag) {
  return sproc_tbl_get_flag(p, lmn_mem_id(key), flag);
}

void sproc_tbl_unset_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  sproc_tbl_expand(p, key);
  if (sproc_tbl_entry(p, key) != SPROC_TBL_INIT_V) {
    sproc_tbl_entry(p, key) |= ~flag;
  } else {
    p->n++;
    sproc_tbl_entry(p, key) = 0;
  }
}

void sproc_tbl_set_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  sproc_tbl_expand(p, key);
  if (sproc_tbl_entry(p, key) != SPROC_TBL_INIT_V) {
    sproc_tbl_entry(p, key) |= flag;
  } else {
    p->n++;
    sproc_tbl_entry(p, key) = flag;
  }
}

void sproc_tbl_set_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key, LmnWord flag) {
  sproc_tbl_set_flag(p, LMN_SATOM_ID(key), flag);
}

void sproc_tbl_set_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key, LmnWord flag) {
  sproc_tbl_set_flag(p, lmn_mem_id(key), flag);
}

void sproc_tbl_unset_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key, LmnWord flag) {
  sproc_tbl_unset_flag(p, LMN_SATOM_ID(key), flag);
}

void sproc_tbl_unset_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key, LmnWord flag) {
  sproc_tbl_unset_flag(p, lmn_mem_id(key), flag);
}
