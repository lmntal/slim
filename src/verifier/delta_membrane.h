/*
 * delta_membrane.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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

#ifndef LMN_DELTA_MEMBRANE_H
#define LMN_DELTA_MEMBRANE_H

/**
 * @ingroup  Verifier
 * @defgroup DeltaMembrane
 * @{
 */

#include "../lmntal.h"
#include "vm/vm.h"
#include "element/element.h"
#include "visitlog.h"

struct MemDeltaRoot {
  LmnMembraneRef root_mem;

  struct ProcessTbl    proc_tbl;
  struct SimplyProcTbl flag_tbl;
  struct ProcessTbl    owner_tbl;

  Vector modified_atoms;

  struct Vector new_mems;
  struct Vector mem_deltas;

  unsigned long next_id;

  BOOL committed;

  LmnRuleRef applied_rule;
  lmn_interned_str applied_history;
  unsigned long new_proc_id_lower_limit;
};


struct MemDelta {
  struct MemDeltaRoot *root_d;
  LmnMembraneRef mem;

  Vector del_atoms;
  Vector new_atoms;

  Vector del_mems;
  Vector new_mems;

  Vector *new_rulesets;
  BOOL ruleset_removed;
  Vector *org_rulesets; /* commit, revertの作業用 */

  Vector new_proxies;

  LmnFunctor max_functor;

  int data_atom_diff;

  LmnMembraneRef new_parent;
  lmn_interned_str new_name, org_name;
};


struct NewMemInfo {
  LmnMembraneRef mem;
  Vector new_child_mems;
  Vector removed_child_mems;
};


/* static  inline LmnAtomRef datom_get_link(struct MemDelta *d, LmnSymbolAtomRef atom, int i); */

/* static inline LmnLinkAttr datom_get_attr(struct MemDelta *d, LmnSymbolAtomRef atom, int i); */

/* とりあえず */
#define DMEM_ROOT_MEM(d) ((d)->root_mem)

/*----------------------------------------------------------------------
 * dmem
 */

struct MemDeltaRoot *dmem_root_make(LmnMembraneRef root_mem, LmnRuleRef rule, unsigned long next_id);
void dmem_root_finish(struct MemDeltaRoot *d);
void dmem_root_free(struct MemDeltaRoot *p);
LmnMembraneRef dmem_root_get_root_mem(struct MemDeltaRoot *d);
unsigned long dmem_root_get_next_id(struct MemDeltaRoot *d);
LmnSymbolAtomRef dmem_root_new_atom(struct MemDeltaRoot *d, LmnFunctor f);
void dmem_root_free_atom(struct MemDeltaRoot *d, LmnAtomRef atom, LmnLinkAttr attr);
LmnAtomRef dmem_root_copy_atom(struct MemDeltaRoot *d, LmnAtomRef atom, LmnLinkAttr attr);
LmnMembraneRef dmem_root_new_mem(struct MemDeltaRoot *d);
void dmem_root_push_atom(struct MemDeltaRoot *d,
                         LmnMembraneRef m,
                         LmnAtomRef atom,
                         LmnLinkAttr attr);
void dmem_root_add_new_mem(struct MemDeltaRoot *d, LmnMembraneRef m);
void dmem_root_add_child_mem(struct MemDeltaRoot *d,
                             LmnMembraneRef parent,
                             LmnMembraneRef child);
void dmem_root_newlink(struct MemDeltaRoot *d, LmnMembraneRef m,
                       LmnAtomRef atom0, LmnLinkAttr attr0, int pos0,
                       LmnAtomRef atom1, LmnLinkAttr attr1, int pos1);
void dmem_root_link_data_atoms(struct MemDeltaRoot *d, LmnMembraneRef m,
                               LmnDataAtomRef d1, LmnLinkAttr attr1,
                               LmnDataAtomRef d2,LmnLinkAttr attr2);
void dmem_root_unify_atom_args(struct MemDeltaRoot *d, LmnMembraneRef m,
                               LmnSymbolAtomRef atom1, int pos1,
                               LmnSymbolAtomRef atom2, int pos2);
void dmem_root_unify_links(struct MemDeltaRoot *d, LmnMembraneRef m,
                           LmnAtomRef atom1, LmnLinkAttr attr1,
                           LmnAtomRef atom2, LmnLinkAttr attr2);
void dmem_root_relink(struct MemDeltaRoot *d, LmnMembraneRef m,
                      LmnAtomRef atom1, LmnLinkAttr attr1, int pos1,
                      LmnAtomRef atom2, LmnLinkAttr attr2, int pos2);
void dmem_root_remove_atom(struct MemDeltaRoot *d,
                           LmnMembraneRef m,
                           LmnAtomRef atom,
                           LmnLinkAttr attr);
void dmem_root_remove_mem(struct MemDeltaRoot *d,
                          LmnMembraneRef parent,
                          LmnMembraneRef child);
void dmem_root_move_cells(struct MemDeltaRoot *d,
                          LmnMembraneRef destmem,
                          LmnMembraneRef srcmem);
ProcessTableRef dmem_root_copy_cells(struct MemDeltaRoot *d,
                                LmnMembraneRef destmem,
                                LmnMembraneRef srcmem);
void dmem_root_clear_ruleset(struct MemDeltaRoot *d, LmnMembraneRef m);
void dmem_root_copy_rules(struct MemDeltaRoot *root_d, LmnMembraneRef dest, LmnMembraneRef src);
void dmem_root_drop(struct MemDeltaRoot *root_d, LmnMembraneRef m);
void dmem_root_set_mem_name(struct MemDeltaRoot *root_d, LmnMembraneRef m, lmn_interned_str name);
void dmem_root_insert_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem, LmnMembraneRef child_mem);
void dmem_root_remove_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);
void dmem_root_remove_toplevel_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);
void dmem_root_remove_temporary_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);

void dmem_root_copy_ground(struct MemDeltaRoot *root_d,
                           LmnMembraneRef mem,
                           Vector *srcvec,
                           Vector **ret_dstlovec,
                           ProcessTableRef *ret_atommap);
void dmem_root_free_ground(struct MemDeltaRoot *root_d, Vector *srcvec);
void dmem_root_remove_ground(struct MemDeltaRoot *root_d, LmnMembraneRef mem, Vector *srcvec);

BOOL dmem_root_is_committed(struct MemDeltaRoot *root_d);

void dmem_root_commit(struct MemDeltaRoot *d);
void dmem_root_revert(struct MemDeltaRoot *d);

void dmem_root_dump(struct MemDeltaRoot *d);

LmnAtomRef dmem_root_get_link(struct MemDeltaRoot *d,
                                  LmnSymbolAtomRef atom,
                                  int i);

/* @} */

#endif
