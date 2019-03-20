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

SimplyProcessTableRef sproc_tbl_make(void) { return new SimpleProcessTable(); }

SimplyProcessTableRef sproc_tbl_make_with_size(unsigned long size) {
  return new SimpleProcessTable(size);
}

void sproc_tbl_free(SimplyProcessTableRef p) { delete p; }

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
void sproc_tbl_put(SimplyProcessTableRef p, LmnWord key, BYTE value) {
  p->put(key, value);
}

void sproc_tbl_put_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom,
                        BYTE value) {
  sproc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

void sproc_tbl_put_mem(SimplyProcessTableRef p, LmnMembraneRef mem,
                       BYTE value) {
  sproc_tbl_put(p, mem->mem_id(), value);
}

void sproc_tbl_unput(SimplyProcessTableRef p, LmnWord key) { p->unput(key); }

void sproc_tbl_unput_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom) {
  sproc_tbl_unput(p, LMN_SATOM_ID(atom));
}

void sproc_tbl_unput_mem(SimplyProcessTableRef p, LmnMembraneRef mem) {
  sproc_tbl_unput(p, mem->mem_id());
}

int sproc_tbl_get(SimplyProcessTableRef p, LmnWord key, BYTE *value) {
  BYTE v;
  bool res = p->get(key, &v);
  if (value)
    *value = v;
  return res;
}

int sproc_tbl_get_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom,
                          BYTE *value) {
  return sproc_tbl_get(p, LMN_SATOM_ID(atom), value);
}

int sproc_tbl_get_by_mem(SimplyProcessTableRef p, LmnMembraneRef mem,
                         BYTE *value) {
  return sproc_tbl_get(p, mem->mem_id(), value);
}

BOOL sproc_tbl_contains(SimplyProcessTableRef p, LmnWord key) {
  return p->contains(key);
}

BOOL sproc_tbl_contains_atom(SimplyProcessTableRef p, LmnSymbolAtomRef atom) {
  return sproc_tbl_contains(p, LMN_SATOM_ID(atom));
}

BOOL sproc_tbl_contains_mem(SimplyProcessTableRef p, LmnMembraneRef mem) {
  return sproc_tbl_contains(p, mem->mem_id());
}

BOOL sproc_tbl_get_flag(SimplyProcessTableRef p, LmnWord key, BYTE flag) {
  return p->get_flag(key, flag);
}

BOOL sproc_tbl_get_flag_by_atom(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                                LmnWord flag) {
  return sproc_tbl_get_flag(p, LMN_SATOM_ID(key), flag);
}

BOOL sproc_tbl_get_flag_by_mem(SimplyProcessTableRef p, LmnMembraneRef key,
                               LmnWord flag) {
  return sproc_tbl_get_flag(p, key->mem_id(), flag);
}

void sproc_tbl_unset_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  p->unset_flag(key, flag);
}

void sproc_tbl_set_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  p->set_flag(key, flag);
}

void sproc_tbl_set_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                             LmnWord flag) {
  sproc_tbl_set_flag(p, LMN_SATOM_ID(key), flag);
}

void sproc_tbl_set_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key,
                            LmnWord flag) {
  sproc_tbl_set_flag(p, key->mem_id(), flag);
}

void sproc_tbl_unset_atom_flag(SimplyProcessTableRef p, LmnSymbolAtomRef key,
                               LmnWord flag) {
  sproc_tbl_unset_flag(p, LMN_SATOM_ID(key), flag);
}

void sproc_tbl_unset_mem_flag(SimplyProcessTableRef p, LmnMembraneRef key,
                              LmnWord flag) {
  sproc_tbl_unset_flag(p, key->mem_id(), flag);
}
