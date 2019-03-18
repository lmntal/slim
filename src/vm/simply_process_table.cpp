/*
 * simply_process_table.cpp
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

#include "simply_process_table.h"

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void SimpleProcessTable::tbl_put(LmnWord key, BYTE value) {
  this->put(key, value);
}

void SimpleProcessTable::tbl_put_atom(LmnSymbolAtomRef atom, BYTE value) {
  tbl_put(LMN_SATOM_ID(atom), value);
}

void SimpleProcessTable::tbl_put_mem(LmnMembraneRef mem, BYTE value) {
  tbl_put(lmn_mem_id(mem), value);
}

void SimpleProcessTable::tbl_unput(LmnWord key) { this->unput(key); }

void SimpleProcessTable::tbl_unput_atom(LmnSymbolAtomRef atom) {
  tbl_unput(LMN_SATOM_ID(atom));
}

void SimpleProcessTable::tbl_unput_mem(LmnMembraneRef mem) {
  tbl_unput(lmn_mem_id(mem));
}

int SimpleProcessTable::tbl_get(LmnWord key, BYTE *value) {
  BYTE v;
  bool res = this->get(key, &v);
  if (value) *value = v;
  return res;
}

int SimpleProcessTable::tbl_get_by_atom(LmnSymbolAtomRef atom, BYTE *value) {
  return tbl_get(LMN_SATOM_ID(atom), value);
}

int SimpleProcessTable::tbl_get_by_mem(LmnMembraneRef mem, BYTE *value) {
  return tbl_get(lmn_mem_id(mem), value);
}

bool SimpleProcessTable::tbl_contains(LmnWord key) {
  return this->contains(key);
}

bool SimpleProcessTable::tbl_contains_atom(LmnSymbolAtomRef atom) {
  return tbl_contains(LMN_SATOM_ID(atom));
}

bool SimpleProcessTable::tbl_contains_mem(LmnMembraneRef mem) {
  return tbl_contains(lmn_mem_id(mem));
}

bool SimpleProcessTable::tbl_get_flag(LmnWord key, BYTE flag) {
  return this->get_flag(key, flag);
}

bool SimpleProcessTable::tbl_get_flag_by_atom(LmnSymbolAtomRef key,
                                              LmnWord flag) {
  return tbl_get_flag(LMN_SATOM_ID(key), flag);
}

bool SimpleProcessTable::tbl_get_flag_by_mem(LmnMembraneRef key, LmnWord flag) {
  return tbl_get_flag(lmn_mem_id(key), flag);
}

void SimpleProcessTable::tbl_unset_flag(LmnWord key, LmnWord flag) {
  this->unset_flag(key, flag);
}

void SimpleProcessTable::tbl_set_flag(LmnWord key, LmnWord flag) {
  this->set_flag(key, flag);
}

void SimpleProcessTable::tbl_set_atom_flag(LmnSymbolAtomRef key, LmnWord flag) {
  tbl_set_flag(LMN_SATOM_ID(key), flag);
}

void SimpleProcessTable::tbl_set_mem_flag(LmnMembraneRef key, LmnWord flag) {
  tbl_set_flag(lmn_mem_id(key), flag);
}

void SimpleProcessTable::tbl_unset_atom_flag(LmnSymbolAtomRef key,
                                             LmnWord flag) {
  tbl_unset_flag(LMN_SATOM_ID(key), flag);
}

void SimpleProcessTable::tbl_unset_mem_flag(LmnMembraneRef key, LmnWord flag) {
  tbl_unset_flag(lmn_mem_id(key), flag);
}
