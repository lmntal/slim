/*
 * membrane.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 * $Id: membrane.h,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_MEMBRANE_H
#define LMN_MEMBRANE_H

/**
 * @ingroup VM
 * @defgroup Membrane
 * @{
 */

#include "membrane.hpp"

typedef struct AtomListEntry *AtomListEntryRef;
typedef struct LinkObj *LinkObjRef;

#include "atom.h"
#include "element/element.h"
#include "functor.h"
#include "lmntal.h"
#include "process_table.h"
#include "rule.h"

#include <vector>

/*
 * extended ground
 */

// typedef struct ProcessTbl *ProcessTableRef;
// typedef struct SimpleProcessTable *SimplyProcessTableRef;
// typedef struct Hlground_Data Hlground_Data;
// struct Hlground_Data {
//   ProcessTableRef global_hlinks;  // global hlinks
//   ProcessTableRef local_atoms;    // atoms within hlground
// };
//
// Hlground_Data hlground_dat;a

// void init_grounddata();
// void free_grounddata();
void dfs_scope_finder(ProcessTableRef *global_hlinks,
                      ProcessTableRef *local_atoms,
                      LinkObjRef root_link,
                      // Vector *src, Vector *avovec,
                      ProcessTableRef *attr_functors,
                      Vector *attr_dataAtoms,
                      Vector *attr_dataAtom_attrs);

BOOL purecycle_exist(Vector *srcvec, Vector *avovec);
// BOOL cycle_exist (Vector *srcvec, Vector *avovec,
//                   ProcessTableRef  *attr_functors,
//                   Vector   *attr_dataAtoms,
// 		  Vector   *attr_dataAtom_attrs);

void get_neighbours(Vector  *avovec,
                    Vector *neighbours,
                    LmnAtomRef atom,
                    LmnLinkAttr pos,
                    ProcessTableRef  *attr_functors,
                    Vector   *attr_dataAtoms,
                    Vector   *attr_dataAtom_attrs);

BOOL extended_ground_atoms(ProcessTableRef *global_hlinks,
                           ProcessTableRef *local_atoms,
                           Vector *srcvec,
                           Vector *avovec,
                           // ProcessTableRef *atoms,
                           // ProcessTableRef *hlinks,
                           ProcessTableRef *attr_functors,
                           Vector *attr_dataAtoms,
                           Vector *attr_dataAtom_attrs);

/** -----
 *  リンクオブジェクトの代替
 */

LmnAtomRef LinkObjGetAtom(LinkObjRef o);
LmnLinkAttr LinkObjGetPos(LinkObjRef o);
LinkObjRef LinkObj_make(LmnAtomRef ap, LmnLinkAttr pos);

/** -----
 *  膜
 */

extern struct st_hash_type type_memhash;


LmnRuleSetRef lmn_mem_get_ruleset(LmnMembraneRef m, int i);

void lmn_mem_rulesets_destroy(const std::vector<LmnRuleSet *> &rulesets);
void lmn_mem_rulesets_destroy(Vector *rulesets);
void mem_push_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom);
void lmn_mem_add_ruleset_sort(std::vector<LmnRuleSet *> *rulesets, LmnRuleSetRef ruleset);


LmnMembraneRef lmn_mem_copy_with_map_ex(LmnMembraneRef srcmem,
                                        ProcessTableRef *copymap);
LmnMembraneRef lmn_mem_copy_with_map(LmnMembraneRef srcmem,
                                     ProcessTableRef *copymap);

ProcessTableRef lmn_mem_copy_cells_ex(LmnMembraneRef dest, LmnMembraneRef src,
                                      BOOL hl_nd);
ProcessTableRef lmn_mem_copy_cells(LmnMembraneRef dest, LmnMembraneRef srcmem);

BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec);
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
BOOL lmn_mem_is_hlground(Vector *srcvec, Vector *avovec, unsigned long *natoms,
                         ProcessTableRef *attr_functors, Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_copy_ground(LmnMembraneRef mem, Vector *srcvec,
                         Vector **ret_dstlovec, ProcessTableRef *ret_atommap,
                         ProcessTableRef *ret_hlinkmap,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms, Vector *attr_dataAtom_attrs);
void lmn_mem_copy_hlground(LmnMembraneRef mem, Vector *srcvec,
                           Vector **ret_dstlovec, ProcessTableRef *ret_atommap,
                           ProcessTableRef *ret_hlinkmap,
                           ProcessTableRef *attr_functors,
                           Vector *attr_dataAtoms, Vector *attr_dataAtom_attrs);
void lmn_mem_remove_hlground(LmnMembraneRef mem, Vector *srcvec,
                             ProcessTableRef *attr_sym, Vector *attr_data,
                             Vector *attr_data_at);
void lmn_mem_free_ground(Vector *srcvec);
void lmn_mem_free_hlground(Vector *srcvec, ProcessTableRef *attr_sym,
                           Vector *attr_data, Vector *attr_data_at);
BOOL ground_atoms(Vector *srcvec, Vector *avovec, ProcessTableRef *atoms,
                  unsigned long *natoms, ProcessTableRef *hlinks,
                  ProcessTableRef *attr_functors, Vector *attr_dataAtoms,
                  Vector *attr_dataAtom_attrs);
BOOL ground_atoms_old(Vector *srcvec, Vector *avovec, HashSet **atoms,
                      unsigned long *natoms);

void move_symbol_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem);
void move_symbol_atomlist_to_atomlist_tail(LmnSymbolAtomRef a,
                                           LmnMembraneRef mem);
void move_symbol_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                                   LmnMembraneRef mem);
//imagawa2
bool move_symbol_diffatomlist_to_atomlist_tail2(LmnFunctor f, LmnMembraneRef mem);

void mem_push_symbol_diffatom(LmnMembraneRef mem, LmnSymbolAtomRef atom);

bool move_diffatomlist_to_atomlist_tail2(LmnFunctor f, LmnMembraneRef mem);

void lmn_mem_push_diffatom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr);
void add_removeatom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr);
bool del_remove_list(LmnMembraneRef mem);


LmnSymbolAtomRef lmn_mem_newatom(LmnMembraneRef mem, LmnFunctor f);
void lmn_mem_remove_data_atom(LmnMembraneRef mem, LmnDataAtomRef atom,
                              LmnLinkAttr attr);
void mem_remove_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom);
void mem_remove_symbol_atom_with_buddy_data(LmnMembraneRef mem,
                                            LmnSymbolAtomRef atom);
void lmn_mem_remove_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr);
void lmn_mem_delete_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr);
void lmn_mem_push_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr);
void alter_functor(LmnMembraneRef mem, LmnSymbolAtomRef atom, LmnFunctor f);
void lmn_mem_add_ruleset(LmnMembraneRef mem, LmnRuleSetRef ruleset);
void newlink_symbol_and_something(LmnSymbolAtomRef atom0, int pos,
                                  LmnAtomRef atom1, LmnLinkAttr attr);
void lmn_mem_newlink(LmnMembraneRef mem, LmnAtomRef atom0, LmnLinkAttr attr0,
                     int pos0, LmnAtomRef atom1, LmnLinkAttr attr1, int pos1);
void lmn_newlink_in_symbols(LmnSymbolAtomRef atom0, int pos0,
                            LmnSymbolAtomRef atom1, int pos1);
void lmn_newlink_with_ex(LmnMembraneRef mem, LmnSymbolAtomRef atom0,
                         LmnLinkAttr attr0, int pos0, LmnSymbolAtomRef atom1,
                         LmnLinkAttr attr1, int pos1);
void lmn_mem_link_data_atoms(LmnMembraneRef mem, LmnAtomRef d1,
                             LmnLinkAttr attr1, LmnAtomRef d2,
                             LmnLinkAttr attr2);
void lmn_mem_unify_atom_args(LmnMembraneRef mem, LmnSymbolAtomRef atom1,
                             int pos1, LmnSymbolAtomRef atom2, int pos2);
void lmn_mem_unify_symbol_atom_args(LmnSymbolAtomRef atom1, int pos1,
                                    LmnSymbolAtomRef atom2, int pos2);
void lmn_relink_symbols(LmnSymbolAtomRef atom0, int pos0,
                        LmnSymbolAtomRef atom1, int pos1);
void lmn_mem_relink_atom_args(LmnMembraneRef mem, LmnAtomRef atom0,
                              LmnLinkAttr attr0, int pos0, LmnAtomRef atom1,
                              LmnLinkAttr attr1, int pos1);

namespace slim {
namespace vm {
struct membrane_iterator {
  using difference_type = intptr_t;
  using value_type = LmnMembrane;
  using pointer = LmnMembrane *;
  using reference = LmnMembrane &;
  typedef typename std::input_iterator_tag iterator_category;

  membrane_iterator() : mem_(nullptr) {}
  membrane_iterator(LmnMembrane *mem) : mem_(mem) {}
  membrane_iterator(const membrane_iterator &i)
      : mem_(i.mem_) {}

  reference operator*() { return *mem_; }
  membrane_iterator &operator++() {
    mem_ = mem_->mem_next();
    return *this;
  }
  pointer operator->() { return mem_; }
  membrane_iterator operator++(int i_) {
    auto i = *this;
    ++i;
    return i;
  }

  bool operator==(const membrane_iterator &i) const { return i.mem_ == mem_; }
  bool operator!=(const membrane_iterator &i) const { return !(i == *this); }
private:
  LmnMembrane *mem_;
};

struct membrane_children {
  LmnMembrane *mem;
  membrane_children(LmnMembrane *mem) : mem(mem) {}
  membrane_iterator begin() const { return membrane_iterator(mem->mem_child_head()); }
  membrane_iterator end() const { return membrane_iterator(); }
};

} // namespace vm
} // namespace slim

/* @} */

#endif /* LMN_MEMBRANE_H */
