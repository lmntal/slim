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
class LmnSet {
  typedef class LmnSet *LmnSetRef;
  LMN_SP_ATOM_HEADER;
  st_table_t tbl; /* hash table */
  LmnSet(struct st_hash_type *ht);
  ~LmnSet();
  /* id set */
  static unsigned long id_hash(st_data_t a);
  static int id_cmp(st_data_t a, st_data_t b);
  static unsigned long tuple_hash(LmnSymbolAtomRef cons);
  static int tuple_cmp(LmnSymbolAtomRef cons0, LmnSymbolAtomRef cons1);
  /* mem set */
  static LmnBinStrRef lmn_inner_mem_encode(LmnMembraneRef m);
  static unsigned long mem_hash(LmnMembraneRef m);

  static struct st_hash_type type_id_hash;
  static struct st_hash_type type_mem_hash;
  static struct st_hash_type type_tuple_hash;

  static int set_atom_type; /* special atom type */
  static int inner_set_free(st_data_t, st_data_t, st_data_t);
  static void lmn_set_free(LmnSetRef set);
  static void cb_set_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0);

  static void cb_set_insert(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                   LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                   LmnLinkAttr t2);
  static void cb_set_find(LmnReactCxtRef *rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3);


  static void cb_set_to_list(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                    LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1);  
  static int inner_set_to_list(st_data_t, st_data_t, st_data_t);

  static void cb_set_copy(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2);
  static int inner_set_copy(st_data_t, st_data_t, st_data_t);

  static void cb_set_erase(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                  LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                  LmnLinkAttr t2);

  static void cb_set_union(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                  LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                  LmnLinkAttr t2);
  static int inner_set_union(st_data_t, st_data_t, st_data_t);

  static void cb_set_intersect(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                      LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                      LmnAtomRef a2, LmnLinkAttr t2);
  static int inner_set_intersect(st_data_t, st_data_t, st_data_t);

  static void cb_set_diff(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2);
  static int inner_set_diff(st_data_t, st_data_t, st_data_t);

  static void*sp_cb_set_copy(void *data);
  static void sp_cb_set_free(void *data);
  static unsigned char sp_cb_set_eq(void *data1, void *data2);
  static void sp_cb_set_dump(void *set, LmnPortRef port);
  static unsigned char sp_cb_set_is_ground(void *data);

  public:
  static int mem_cmp(LmnMembraneRef m0, LmnMembraneRef m1);
  static void init_set(void);
};
class InnerToList {
  LmnMembraneRef mem_data;
  LmnAtomRef cons_data;
  LmnAtomRef prev_data;
  struct st_hash_type *ht_data;
  public:
  typedef class InnerToList *InnerToListRef;
  LmnMembraneRef mem();
  void set_mem_as(LmnMembraneRef data);
  LmnAtomRef cons();
  void set_cons_as(LmnAtomRef data);
  LmnAtomRef prev();
  void set_prev_as(LmnAtomRef data);
  struct st_hash_type *ht();
  void set_ht_as(struct st_hash_type *data);
};

#endif
