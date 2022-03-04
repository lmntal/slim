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
#include "element/element.h"
#include "visitlog.h"
#include "vm/vm.h"

#define dmem_get_attr(d, m, atom, i) ((atom)->get_attr(i))

#define DMEM_ORG_EACH_FUNC_ATOM(D, MEM, F, V, CODE)                                    \
  do {                                                                                 \
    AtomListEntryRef __ent = (MEM)->get_atomlist((F));                                 \
    LmnSymbolAtomRef __next;                                                           \
    if (__ent) {                                                                       \
      for ((V) = atomlist_head(__ent); (V) != lmn_atomlist_end(__ent); (V) = __next) { \
        __next = (V)->get_next();                                                      \
        if ((V)->get_functor() != LMN_RESUME_FUNCTOR &&                                \
            !((D) && dmem_is_removed_atom((D), (MEM), (V)))) {                         \
          (CODE);                                                                      \
        }                                                                              \
      }                                                                                \
    }                                                                                  \
  } while (0)

#define DMEM_EACH_FUNC_ATOM(D, MEM, F, V, CODE)                                  \
  do {                                                                           \
    int __i;                                                                     \
    DMEM_ORG_EACH_FUNC_ATOM(D, MEM, F, V, CODE);                                 \
    if ((D)) {                                                                   \
      for (__i = 0; __i < (D)->new_proxies.get_num(); __i++) {                   \
        (V) = (LmnSymbolAtomRef)(D)->new_proxies.get(__i);                       \
        if ((V)->get_functor() == F && !dmem_is_removed_atom((D), (MEM), (V))) { \
          (CODE);                                                                \
        }                                                                        \
      }                                                                          \
    }                                                                            \
  } while (0)

#define DMEM_ALL_ATOMS(D, MEM, V, CODE)                                    \
  do {                                                                     \
    if (!(D)) {                                                            \
      ALL_ATOMS(MEM, V, CODE);                                             \
    } else {                                                               \
      int __i, i_atomlist;                                                 \
      for (i_atomlist = 0; i_atomlist <= (D)->max_functor; i_atomlist++) { \
        DMEM_ORG_EACH_FUNC_ATOM(D, MEM, i_atomlist, V, CODE);              \
      }                                                                    \
      for (__i = 0; __i < (D)->new_atoms.get_num(); __i++) {               \
        (V) = (LmnSymbolAtomRef)(D)->new_atoms.get(__i);                   \
        if (!dmem_is_removed_atom((D), (MEM), (V))) {                      \
          (CODE);                                                          \
        }                                                                  \
      }                                                                    \
    }                                                                      \
  } while (0)

#define DMEM_ALL_MEMS(D, MEM, V, CODE)                     \
  do {                                                     \
    unsigned int i;                                        \
    LmnMembraneRef __next;                                 \
    for ((V) = MEM->mem_child_head(); (V); (V) = __next) { \
      __next = V->mem_next();                              \
      if (!(D) || !dmem_is_removed_mem((D), (MEM), (V))) { \
        (CODE);                                            \
      }                                                    \
    }                                                      \
    if ((D)) {                                             \
      for (i = 0; i < (D)->new_mems.get_num(); i++) {      \
        (V) = (LmnMembraneRef)(D)->new_mems.get(i);        \
        (CODE);                                            \
      }                                                    \
    }                                                      \
  } while (0)

enum {
  TAG_DEL_MEM = 1U /* 0000 0001 */,
  TAG_NEW_ATOM = 2U /* 0000 0010 */,
  TAG_DEL_ATOM = 4U /* 0000 0100 */,
  TAG_NEW_MEM = 8U /* 0000 1000 */,
  TAG_DELTA_MEM = 16U /* 0001 0000 */,
  TAG_MODIFIED_ATOM = 32U /* 0010 0000 */
};

/*----------------------------------------------------------------------
 * dmem
 */

void dmem_root_finish(struct MemDeltaRoot *d);
LmnSymbolAtomRef dmem_root_new_atom(struct MemDeltaRoot *d, LmnFunctor f);
void dmem_root_free_atom(struct MemDeltaRoot *d, LmnAtomRef atom, LmnLinkAttr attr);
LmnAtomRef dmem_root_copy_atom(struct MemDeltaRoot *d, LmnAtomRef atom, LmnLinkAttr attr);
LmnMembraneRef dmem_root_new_mem(struct MemDeltaRoot *d);
void dmem_root_add_new_mem(struct MemDeltaRoot *d, LmnMembraneRef m);
void dmem_root_add_child_mem(struct MemDeltaRoot *d, LmnMembraneRef parent, LmnMembraneRef child);
void dmem_root_remove_mem(struct MemDeltaRoot *d, LmnMembraneRef parent, LmnMembraneRef child);
void dmem_root_clear_ruleset(struct MemDeltaRoot *d, LmnMembraneRef m);
void dmem_root_copy_rules(struct MemDeltaRoot *root_d, LmnMembraneRef dest, LmnMembraneRef src);
void dmem_root_drop(struct MemDeltaRoot *root_d, LmnMembraneRef m);
void dmem_root_set_mem_name(struct MemDeltaRoot *root_d, LmnMembraneRef m, lmn_interned_str name);
void dmem_root_insert_proxies(
    struct MemDeltaRoot *root_d,
    LmnMembraneRef mem,
    LmnMembraneRef child_mem);
void dmem_root_remove_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);
void dmem_root_remove_toplevel_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);
void dmem_root_remove_temporary_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem);

void dmem_root_copy_ground(
    struct MemDeltaRoot *root_d,
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

LmnAtomRef dmem_root_get_link(struct MemDeltaRoot *d, LmnSymbolAtomRef atom, int i);

// ここから移転済み
static LmnSymbolAtomRef dmem_root_alter_functor(
    struct MemDeltaRoot *root_d,
    LmnMembraneRef mem,
    LmnSymbolAtomRef atom,
    LmnFunctor f);
static struct NewMemInfo *dmem_root_get_new_mem_info(struct MemDeltaRoot *d, LmnMembraneRef m);
static LmnMembraneRef dmem_root_get_parent(struct MemDeltaRoot *root_d, LmnMembraneRef m);
static inline LmnSymbolAtomRef dmem_root_copy_satom_with_data(
    struct MemDeltaRoot *d,
    LmnSymbolAtomRef atom);
static inline LmnSymbolAtomRef dmem_root_copy_satom(struct MemDeltaRoot *d, LmnSymbolAtomRef atom);
static inline LmnSymbolAtomRef dmem_root_copy_eqatom_with_data(LmnSymbolAtomRef atom);
static inline void
dmem_root_commit_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef src, LmnSymbolAtomRef atom);
static inline void
dmem_root_revert_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef src, LmnSymbolAtomRef atom);
static inline BOOL dmem_root_is_freed_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef a);
static inline LmnSymbolAtomRef dmem_root_modified_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef a);
static inline void dmem_root_free_satom(struct MemDeltaRoot *d, LmnSymbolAtomRef atom);
static inline BOOL dmem_root_is_new_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef a);
static inline BOOL dmem_root_is_modified_atom(struct MemDeltaRoot *d, LmnSymbolAtomRef a);
static inline LmnMembraneRef dmem_root_atom_mem(struct MemDeltaRoot *d, LmnSymbolAtomRef a)
    LMN_UNUSED;
static struct MemDelta *
mem_delta_make(struct MemDeltaRoot *root_d, LmnMembraneRef m, unsigned long next_id);
static void mem_delta_free(struct MemDelta *p);

static inline BOOL dmem_is_new_atom(struct MemDelta *d, LmnMembraneRef m, LmnSymbolAtomRef a)
    LMN_UNUSED;
static inline BOOL dmem_is_removed_atom(struct MemDelta *d, LmnMembraneRef m, LmnSymbolAtomRef a);
static inline BOOL
dmem_is_removed_mem(struct MemDelta *d, LmnMembraneRef parent, LmnMembraneRef child);
static inline void
dmem_put_atom(struct MemDelta *d, LmnMembraneRef m, LmnAtomRef atom, LmnLinkAttr attr);
static inline void
dmem_put_symbol_atom(struct MemDelta *d, LmnMembraneRef m, LmnSymbolAtomRef atom);
static inline void
dmem_remove_symbol_atom(struct MemDelta *d, LmnMembraneRef m, LmnSymbolAtomRef atom);
static void dmem_link_data_atoms(
    struct MemDelta *d,
    LmnMembraneRef m,
    LmnDataAtomRef d0,
    LmnLinkAttr attr0,
    LmnDataAtomRef d1,
    LmnLinkAttr attr1);
static inline LmnSymbolAtomRef
dmem_modify_atom(struct MemDelta *d, LmnMembraneRef mem, LmnSymbolAtomRef src);
static inline void dmem_modify_link(
    struct MemDelta *d,
    LmnMembraneRef m,
    LmnSymbolAtomRef atom,
    int i,
    LmnAtomRef l,
    LmnLinkAttr attr);

static void dmem_add_ruleset(struct MemDelta *d, LmnMembraneRef m, LmnRuleSetRef ruleset);
static inline void dmem_clear_ruleset(struct MemDelta *d, LmnMembraneRef m);
static void dmem_add_child_mem(struct MemDelta *d, LmnMembraneRef parent, LmnMembraneRef child);
static inline void
dmem_remove_atom(struct MemDelta *d, LmnMembraneRef m, LmnAtomRef atom, LmnLinkAttr attr);
static void modify_free_link(struct MemDeltaRoot *d, LmnMembraneRef m);
static void modify_free_link_sub(struct MemDeltaRoot *d, LmnMembraneRef m, LmnSymbolAtomRef in);
static inline void dmem_copy_rules(struct MemDelta *d, LmnMembraneRef dest, LmnMembraneRef src);
static void dmem_drop(struct MemDelta *d, LmnMembraneRef mem);
static void dmem_commit(struct MemDelta *d);
static inline void dmem_commit_delete_mem(struct MemDelta *d);
static void dmem_revert(struct MemDelta *d);
static inline void dmem_revert_new_mem(struct MemDelta *d);
static void dmem_dump(struct MemDelta *d);
static inline void dmem_relink(
    struct MemDelta *d,
    LmnMembraneRef m,
    LmnSymbolAtomRef atom1,
    int pos1,
    LmnSymbolAtomRef atom2,
    int pos2);
static void dmem_unify_atom_args(
    struct MemDelta *d,
    LmnMembraneRef m,
    LmnSymbolAtomRef atom1,
    int pos1,
    LmnSymbolAtomRef atom2,
    int pos2);

struct MemDeltaRoot {
  LmnMembraneRef root_mem;

  ProcessTableRef proc_tbl;
  SimplyProcessTableRef flag_tbl;
  ProcessTableRef owner_tbl;

  Vector modified_atoms;

  struct Vector new_mems;
  struct Vector mem_deltas;

  unsigned long next_id;

  BOOL committed;

  LmnRuleRef applied_rule;
  lmn_interned_str applied_history;
  unsigned long new_proc_id_lower_limit;

  MemDeltaRoot(LmnMembraneRef root_mem, LmnRuleRef rule, unsigned long next_id);
  ~MemDeltaRoot();

  LmnMembraneRef get_root_mem() const;
  unsigned long get_next_id() const;
  struct MemDelta *get_mem_delta(LmnMembraneRef m);
  BOOL is_delta_mem(LmnMembraneRef m) const;
  BOOL is_new_mem(LmnMembraneRef m) const;
  void push_atom(LmnMembraneRef m, LmnAtomRef atom, LmnLinkAttr attr);
  void remove_atom(LmnMembraneRef m, LmnAtomRef atom, LmnLinkAttr attr);
  void newlink(
      LmnMembraneRef m,
      LmnAtomRef atom0,
      LmnLinkAttr attr0,
      int pos0,
      LmnAtomRef atom1,
      LmnLinkAttr attr1,
      int pos1);
  void link_data_atoms(
      LmnMembraneRef m,
      LmnDataAtomRef d1,
      LmnLinkAttr attr1,
      LmnDataAtomRef d2,
      LmnLinkAttr attr2);
  void unify_atom_args(
      LmnMembraneRef m,
      LmnSymbolAtomRef atom1,
      int pos1,
      LmnSymbolAtomRef atom2,
      int pos2);
  void unify_links(
      LmnMembraneRef m,
      LmnAtomRef atom1,
      LmnLinkAttr attr1,
      LmnAtomRef atom2,
      LmnLinkAttr attr2);
  void relink(
      LmnMembraneRef m,
      LmnAtomRef atom1,
      LmnLinkAttr attr1,
      int pos1,
      LmnAtomRef atom2,
      LmnLinkAttr attr2,
      int pos2);
  void move_satom(LmnWord key, LmnWord dest);
  void move_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem);
  ProcessTableRef copy_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem);
  void copy_cells(
      struct MemDelta *d,
      LmnMembraneRef destmem,
      struct MemDelta *d2,
      LmnMembraneRef srcmem,
      ProcessTableRef atoms);
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

  NewMemInfo(LmnMembraneRef mem);
  ~NewMemInfo();
};

/* static  inline LmnAtomRef datom_get_link(struct MemDelta *d, LmnSymbolAtomRef
 * atom, int i); */

/* static inline LmnLinkAttr datom_get_attr(struct MemDelta *d, LmnSymbolAtomRef
 * atom, int i); */

/* とりあえず */
#define DMEM_ROOT_MEM(d) ((d)->root_mem)

/* @} */

#endif
