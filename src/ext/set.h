/*
 * set.h
 *
 *   Copyright (c) 2017, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

#ifndef LMN_SET_H
#define LMN_SET_H

#include "element/element.h"
#include "verifier/verifier.h"
#include "vm/vm.h"
struct LmnSet {
  LMN_SP_ATOM_HEADER;
  st_table_t tbl; /* hash table */
  LmnSet(struct st_hash_type *ht);
  ~LmnSet();
  public://リファクタリング中につき一時的にpublic設定
  //  typedef struct LmnSet *LmnSetRef;
  /* id set */
  static unsigned long id_hash(st_data_t a);
  static int id_cmp(st_data_t a, st_data_t b);
  static unsigned long tuple_hash(LmnSymbolAtomRef cons);
  static int tuple_cmp(LmnSymbolAtomRef cons0, LmnSymbolAtomRef cons1);
  /* mem set */
  static LmnBinStrRef lmn_inner_mem_encode(LmnMembraneRef m);
  static unsigned long mem_hash(LmnMembraneRef m);
  public://本当にpublicなメソッドはこの下に
  static int mem_cmp(LmnMembraneRef m0, LmnMembraneRef m1);

};

//static int LmnSet::mem_cmp(LmnMembraneRef m0, LmnMembraneRef m1);
extern struct st_hash_type type_id_hash;

#endif
