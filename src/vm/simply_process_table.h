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
    value_type v = this->contains(key) ? (*this)[key] : 0;
    this->put(key, v & ~flag);
  }

  void set_flag(key_type key, value_type flag) {
    value_type v = this->contains(key) ? (*this)[key] : 0;
    this->put(key, v | flag);
  }

  bool contains(key_type key) { return ProcessTable<BYTE>::contains(key); }

  bool contains(LmnSymbolAtomRef atom) { return contains(atom->get_id()); }

  bool contains(LmnMembraneRef mem) { return contains(mem->mem_id()); }
  void tbl_put(LmnWord key, BYTE value);
  void tbl_put_atom(LmnSymbolAtomRef atom, BYTE value);
  void tbl_put_mem(LmnMembraneRef mem, BYTE value);
  void tbl_unput(LmnWord key);
  void tbl_unput_atom(LmnSymbolAtomRef atom);
  void tbl_unput_mem(LmnMembraneRef mem);
  int tbl_get(LmnWord key, BYTE *value);
  int tbl_get_by_atom(LmnSymbolAtomRef atom, BYTE *value);
  int tbl_get_by_mem(LmnMembraneRef mem, BYTE *value);
  bool tbl_contains(LmnWord key);
  bool tbl_contains_atom(LmnSymbolAtomRef atom);
  bool tbl_contains_mem(LmnMembraneRef mem);
  bool tbl_get_flag(LmnWord key, BYTE flag);
  bool tbl_get_flag_by_atom(LmnSymbolAtomRef key, LmnWord flag);
  bool tbl_get_flag_by_mem(LmnMembraneRef key, LmnWord flag);
  void tbl_unset_flag(LmnWord key, LmnWord flag);
  void tbl_set_flag(LmnWord key, LmnWord flag);
  void tbl_set_atom_flag(LmnSymbolAtomRef key, LmnWord flag);
  void tbl_set_mem_flag(LmnMembraneRef key, LmnWord flag);
  void tbl_unset_atom_flag(LmnSymbolAtomRef key, LmnWord flag);
  void tbl_unset_mem_flag(LmnMembraneRef key, LmnWord flag);
};

/**
 * Function Prototypes
 */

#endif /* LMN_SIMPLY_PROCESS_TABLE_H */
