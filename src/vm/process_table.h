/*
 * process_table.h
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

#ifndef PROCESS_TABLE_H
#define PROCESS_TABLE_H

#include "element/element.h"

typedef struct ProcessTbl *ProcessTableRef;

#include "hyperlink.h"
#include "process_table.hpp"

struct ProcessTbl : ProcessTable<LmnWord> {
  ProcessTbl(unsigned long size) : ProcessTable<LmnWord>(size){};
  ProcessTbl() : ProcessTable<LmnWord>(){};
  void tbl_clear();
  int tbl_foreach(int (*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg);
  BOOL tbl_eq(ProcessTableRef b);
  void proc_tbl_put(LmnWord key, LmnWord value);
  void proc_tbl_put_atom(LmnSymbolAtomRef atom, LmnWord value);
  void proc_tbl_put_mem(LmnMembraneRef mem, LmnWord value);
  void put_new_hlink(struct HyperLink *hl, LmnWord value);
  int put_new(LmnWord key, LmnWord value);
  int put_new_atom(LmnSymbolAtomRef atom, LmnWord value);
  int put_new_mem(LmnMembraneRef mem, LmnWord value);
  void proc_tbl_unput(LmnWord key);
  void unput_atom(LmnSymbolAtomRef atom);
  void unput_hlink(HyperLink *hl);  // extended
};

/**
 * Function ProtoTypes
 */

void proc_tbl_unput_mem(ProcessTableRef p, LmnMembraneRef mem);
int proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value);
int proc_tbl_get_by_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord *value);
int proc_tbl_get_by_mem(ProcessTableRef p, LmnMembraneRef mem, LmnWord *value);
int proc_tbl_get_by_hlink(ProcessTableRef p, struct HyperLink *hl, LmnWord *value);
BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key);
BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSymbolAtomRef atom);
BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembraneRef mem);

void proc_tbl_dump(const char *name, ProcessTableRef map);
void proc_tbl_symbol_atom_dump(const char *name, ProcessTableRef map);

#endif /* PROCESS_TABLE_H */
