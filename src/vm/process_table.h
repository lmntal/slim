/*
 * process_table.h
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

#ifndef PROCESS_TABLE_H
#define PROCESS_TABLE_H

#ifdef __cplusplus
extern "C++" {
/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
template<typename T> struct ProcessTable {
  unsigned long n;
  unsigned long size;
  unsigned long num_buckets;
  T **tbl;
};
}
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "element/element.h"

typedef struct ProcessTbl *ProcessTableRef;

#include "hyperlink.h"

#define PROC_TBL_DEFAULT_SIZE  128U
#define PROC_TBL_BUCKETS_SIZE  (1 << 12) // heuristics
#define process_tbl_entry(P, IDX) (P->tbl[IDX / PROC_TBL_BUCKETS_SIZE][IDX % PROC_TBL_BUCKETS_SIZE])



#define process_tbl_entry_num(P)  ((P)->n)
void proc_tbl_expand_sub(ProcessTableRef p, unsigned long n);
#define proc_tbl_expand(p, n)                                                  \
      proc_tbl_expand_sub(p, n);                                                 


/**
 * Function ProtoTypes
 */

void       proc_tbl_init_with_size(ProcessTableRef p, unsigned long size);
void       proc_tbl_init(ProcessTableRef p);
ProcessTableRef proc_tbl_make_with_size(unsigned long size);
ProcessTableRef proc_tbl_make(void);
void       proc_tbl_destroy(ProcessTableRef p);
void       proc_tbl_free(ProcessTableRef p);
void       proc_tbl_clear(ProcessTableRef p);
int        proc_tbl_foreach(ProcessTableRef p,
                            int(*func)(LmnWord key, LmnWord val, LmnWord arg),
                            LmnWord arg);
BOOL       proc_tbl_eq(ProcessTableRef a, ProcessTableRef b);

unsigned long proc_tbl_get_size(ProcessTableRef p);
void proc_tbl_put(ProcessTableRef p, LmnWord key, LmnWord value);
void proc_tbl_put_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value);
void proc_tbl_put_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value);
int  proc_tbl_put_new(ProcessTableRef p, LmnWord key, LmnWord value);
int  proc_tbl_put_new_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord value);
int  proc_tbl_put_new_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord value);
void proc_tbl_put_new_hlink(ProcessTableRef p, struct HyperLink *hl, LmnWord value);
void proc_tbl_unput(ProcessTableRef p, LmnWord key);
void proc_tbl_unput_atom(ProcessTableRef p, LmnSymbolAtomRef atom);
void proc_tbl_unput_mem(ProcessTableRef p, LmnMembraneRef mem);
int  proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value);
int  proc_tbl_get_by_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord *value);
int  proc_tbl_get_by_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord *value);
int  proc_tbl_get_by_hlink(ProcessTableRef p, struct HyperLink *hl, LmnWord *value);
BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key);
BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSymbolAtomRef atom);
BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembraneRef mem);

#ifdef __cplusplus
}
#endif


#endif /* PROCESS_TABLE_H */
