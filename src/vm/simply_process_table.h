/*
 * simply_process_table.h
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

#ifndef LMN_SIMPLY_PROCESS_TABLE_H
#define LMN_SIMPLY_PROCESS_TABLE_H

/* --------------
 *  SimpleProcTbl
 *  プロセスIDをkeyにしたBYTEサイズテーブル
 */

typedef struct SimpleProcessTable *SimplyProcessTableRef;

#include "element/element.h"
#include "vm/vm.h"

#include "vm/process_table.hpp"

struct SimpleProcessTable : ProcessTable<BYTE> {
  SimpleProcessTable() : ProcessTable<BYTE>(){};
  SimpleProcessTable(unsigned long size) : ProcessTable<BYTE>(size){};

  bool get_flag(key_type key, value_type flag) {
    return this->contains(key) ? ((*this)[key] & flag) : false;
  }

  void unset_flag(key_type key, value_type flag) {
    value_type v;
    this->get(key, &v);
    this->put(key, v & ~flag);
  }

  void set_flag(key_type key, value_type flag) {
    value_type v;
    this->get(key, &v);
    this->put(key, v | flag);
  }

  bool contains(key_type key) { return ProcessTable<BYTE>::contains(key); }

  bool contains(LmnSymbolAtomRef atom) { return contains(LMN_SATOM_ID(atom)); }

  bool contains(LmnMembraneRef mem) { return contains(lmn_mem_id(mem)); }
};

/**
 * Function Prototypes
 */

void sproc_tbl_free(SimplyProcessTableRef p);

void sproc_tbl_put(SimplyProcessTableRef p, LmnWord key, BYTE value);
void sproc_tbl_put_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom,
                        BYTE value);
void sproc_tbl_put_mem(SimplyProcessTableRef p, LmnMembraneRef mem, BYTE value);
void sproc_tbl_unput(SimplyProcessTableRef p, LmnWord key);
void sproc_tbl_unput_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom);
void sproc_tbl_unput_mem(SimplyProcessTableRef p, LmnMembraneRef mem);
int sproc_tbl_get(SimplyProcessTableRef p, LmnWord key, BYTE *value);
int sproc_tbl_get_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom,
                          BYTE *value);
int sproc_tbl_get_by_mem(SimplyProcessTableRef p, LmnMembraneRef mem,
                         BYTE *value);
BOOL sproc_tbl_contains(SimplyProcessTableRef p, LmnWord key);
BOOL sproc_tbl_contains_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom);
BOOL sproc_tbl_contains_mem(SimplyProcessTableRef p, LmnMembraneRef mem);
BOOL sproc_tbl_get_flag(SimplyProcessTableRef p, LmnWord key, BYTE flag);
BOOL sproc_tbl_get_flag_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                                LmnWord flag);
BOOL sproc_tbl_get_flag_by_mem(SimplyProcessTableRef p, LmnMembraneRef key,
                               LmnWord flag);
void sproc_tbl_unset_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag);
void sproc_tbl_set_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag);
void sproc_tbl_set_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                             LmnWord flag);
void sproc_tbl_set_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key,
                            LmnWord flag);
void sproc_tbl_unset_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                               LmnWord flag);
void sproc_tbl_unset_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key,
                              LmnWord flag);

#endif /* LMN_SIMPLY_PROCESS_TABLE_H */
