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

#include "lmntal.h"
#include "atom.h"
#include "utility/visitlog.h"
#include "utility/vector.h"
#include "rule.h"

struct MemDeltaRoot {
  LmnMembrane *root_mem;

  struct ProcessTbl    proc_tbl;
  struct SimplyProcTbl flag_tbl;
  struct ProcessTbl    owner_tbl;

  Vector modified_atoms;

  struct Vector new_mems;
  struct Vector mem_deltas;

  unsigned long next_id;

  BOOL committed;

  LmnRule applied_rule;
  lmn_interned_str applied_history;
  unsigned long new_proc_id_lower_limit;
};


struct MemDelta {
  struct MemDeltaRoot *root_d;
  LmnMembrane *mem;

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

  LmnMembrane *new_parent;
  lmn_interned_str new_name, org_name;
};


struct NewMemInfo {
  LmnMembrane *mem;
  Vector new_child_mems;
  Vector removed_child_mems;
};


/* static  inline LmnAtom datom_get_link(struct MemDelta *d, LmnSAtom atom, int i); */

/* static inline LmnLinkAttr datom_get_attr(struct MemDelta *d, LmnSAtom atom, int i); */

/* とりあえず */
#define DMEM_ROOT_MEM(d) ((d)->root_mem)

/*----------------------------------------------------------------------
 * dmem
 */

struct MemDeltaRoot *dmem_root_make(LmnMembrane *root_mem, LmnRule rule, unsigned long next_id);
void dmem_root_finish(struct MemDeltaRoot *d);
void dmem_root_free(struct MemDeltaRoot *p);
LmnMembrane *dmem_root_get_root_mem(struct MemDeltaRoot *d);
unsigned long dmem_root_get_next_id(struct MemDeltaRoot *d);
LmnSAtom dmem_root_new_atom(struct MemDeltaRoot *d, LmnFunctor f);
void dmem_root_free_atom(struct MemDeltaRoot *d, LmnAtom atom, LmnLinkAttr attr);
LmnAtom dmem_root_copy_atom(struct MemDeltaRoot *d, LmnAtom atom, LmnLinkAttr attr);
LmnMembrane *dmem_root_new_mem(struct MemDeltaRoot *d);
void dmem_root_push_atom(struct MemDeltaRoot *d,
                         LmnMembrane *m,
                         LmnAtom atom,
                         LmnLinkAttr attr);
void dmem_root_add_new_mem(struct MemDeltaRoot *d, LmnMembrane *m);
void dmem_root_add_child_mem(struct MemDeltaRoot *d,
                             LmnMembrane *parent,
                             LmnMembrane *child);
void dmem_root_newlink(struct MemDeltaRoot *d, LmnMembrane *m,
                       LmnAtom atom0, LmnLinkAttr attr0, int pos0,
                       LmnAtom atom1, LmnLinkAttr attr1, int pos1);
void dmem_root_link_data_atoms(struct MemDeltaRoot *d, LmnMembrane *m,
                               LmnAtom d1, LmnLinkAttr attr1,
                               LmnAtom d2,LmnLinkAttr attr2);
void dmem_root_unify_atom_args(struct MemDeltaRoot *d, LmnMembrane *m,
                               LmnSAtom atom1, int pos1,
                               LmnSAtom atom2, int pos2);
void dmem_root_unify_links(struct MemDeltaRoot *d, LmnMembrane *m,
                           LmnAtom atom1, LmnLinkAttr attr1,
                           LmnAtom atom2, LmnLinkAttr attr2);
void dmem_root_relink(struct MemDeltaRoot *d, LmnMembrane *m,
                      LmnAtom atom1, LmnLinkAttr attr1, int pos1,
                      LmnAtom atom2, LmnLinkAttr attr2, int pos2);
void dmem_root_remove_atom(struct MemDeltaRoot *d,
                           LmnMembrane *m,
                           LmnAtom atom,
                           LmnLinkAttr attr);
void dmem_root_remove_mem(struct MemDeltaRoot *d,
                          LmnMembrane *parent,
                          LmnMembrane *child);
void dmem_root_move_cells(struct MemDeltaRoot *d,
                          LmnMembrane *destmem,
                          LmnMembrane *srcmem);
ProcessTbl dmem_root_copy_cells(struct MemDeltaRoot *d,
                                LmnMembrane *destmem,
                                LmnMembrane *srcmem);
void dmem_root_clear_ruleset(struct MemDeltaRoot *d, LmnMembrane *m);
void dmem_root_copy_rules(struct MemDeltaRoot *root_d, LmnMembrane *dest, LmnMembrane *src);
void dmem_root_drop(struct MemDeltaRoot *root_d, LmnMembrane *m);
void dmem_root_set_mem_name(struct MemDeltaRoot *root_d, LmnMembrane *m, lmn_interned_str name);
void dmem_root_insert_proxies(struct MemDeltaRoot *root_d, LmnMembrane *mem, LmnMembrane *child_mem);
void dmem_root_remove_proxies(struct MemDeltaRoot *root_d, LmnMembrane *mem);
void dmem_root_remove_toplevel_proxies(struct MemDeltaRoot *root_d, LmnMembrane *mem);
void dmem_root_remove_temporary_proxies(struct MemDeltaRoot *root_d, LmnMembrane *mem);

void dmem_root_copy_ground(struct MemDeltaRoot *root_d,
                           LmnMembrane *mem,
                           Vector *srcvec,
                           Vector **ret_dstlovec,
                           ProcessTbl *ret_atommap);
void dmem_root_free_ground(struct MemDeltaRoot *root_d, Vector *srcvec);
void dmem_root_remove_ground(struct MemDeltaRoot *root_d, LmnMembrane *mem, Vector *srcvec);

BOOL dmem_root_is_committed(struct MemDeltaRoot *root_d);

void dmem_root_commit(struct MemDeltaRoot *d);
void dmem_root_revert(struct MemDeltaRoot *d);

void dmem_root_dump(struct MemDeltaRoot *d);

LmnAtom dmem_root_get_link(struct MemDeltaRoot *d,
                                  LmnSAtom atom,
                                  int i);

#endif
