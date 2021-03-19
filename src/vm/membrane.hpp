/*
 * membrane.hpp
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
 */


#ifndef LMN_MEMBRANE_HPP
#define LMN_MEMBRANE_HPP
/*----------------------------------------------------------------------
 * Membrane
 */


#include "lmntal.h"
#include "../element/vector.h"

#include <map>
#include <vector>

struct LmnMembrane;
struct LmnRuleSet;
typedef struct LmnMembrane *LmnMembraneRef;
typedef struct AtomListEntry **AtomSet;

struct LmnMembrane {
  AtomSet atomset;
  ProcessID id;
  unsigned int max_functor;
  unsigned int atomset_size;
  unsigned int atom_symb_num; /* # of symbol atom except proxy */
  unsigned int atom_data_num;
  lmn_interned_str name;
  BOOL is_activated;
  LmnMembraneRef parent;
  LmnMembraneRef child_head;
  LmnMembraneRef prev, next;
  std::vector<LmnRuleSet *> rulesets;
#ifdef USE_FIRSTCLASS_RULE
  std::vector<LmnRuleSet *> firstclass_rulesets;
#endif

  std::map<LmnFunctor, AtomListEntry *> atom_lists() const {
    std::map<LmnFunctor, AtomListEntry *> res;
    for (int i = 0; i < max_functor; i++)
      if (atomset[i])
        res[i] = atomset[i];
    return res;
  }
  LmnMembrane();
  ~LmnMembrane();
  lmn_interned_str NAME_ID() {
    return this->name;
  }
  LmnMembraneRef mem_parent() {
    return this->parent;
  }
  BOOL is_active() {
    return this->is_activated;
  }
  unsigned int mem_max_functor() {
    return this->max_functor;
  }
  void set_parent(LmnMembraneRef parent) {
    this->parent = parent;
  }
  void set_name(lmn_interned_str name) {
    this->name = name;
  }
  void set_active(BOOL is_activated) {
    this->is_activated = is_activated;
  }
  const std::vector<LmnRuleSet *> &get_rulesets() const {
    return this->rulesets;
  }
#ifdef USE_FIRSTCLASS_RULE
  const std::vector<LmnRuleSet *> &get_firstclass_rulesets() const {
    return this->firstclass_rulesets;
  }
#endif
  size_t ruleset_num() const {
    return (this->get_rulesets()).size();
  }
  unsigned int symb_atom_num() const {
    return this->atom_symb_num;
  }
  void symb_atom_set(unsigned int n) {
    this->atom_symb_num = n;
  }
  unsigned int data_atom_num() const {
    return this->atom_data_num;
  }
  void data_atom_set(unsigned int n) {
    this->atom_data_num = n;
  }
  unsigned int atom_num() const {
    return this->symb_atom_num() + this->data_atom_num();
  }
  BOOL natoms(unsigned int n) {
    return this->atom_num() == n;
  }
  void symb_atom_add(int n) {
    this->atom_symb_num += n;
  }
  void symb_atom_sub(int n) {
    this->atom_symb_num -= n;
  }
  void symb_atom_inc() {
    this->atom_symb_num++;
  }
  void symb_atom_dec() {
    this->atom_symb_num--;
  }
  void data_atom_add(int n) {
    this->atom_data_num += n;
  }
  void data_atom_sub(int n) {
    this->atom_data_num -= n;
  }
  void data_atom_inc() {
    this->atom_data_num++;
  }
  void data_atom_dec() {
    this->atom_data_num--;
  }
  LmnMembraneRef mem_child_head() {
    return this->child_head;
  }
  LmnMembraneRef mem_next() {
    return this->next;
  }
  LmnMembraneRef mem_prev() {
    return this->prev;
  }
  ProcessID mem_id() const {
    return this->id;
  }
  void set_id(ProcessID n) {
    this->id = n;
  }
  void natoms_copy(LmnMembraneRef n) {
    this->symb_atom_set(n->symb_atom_num());
    this->data_atom_set(n->data_atom_num());
  }
  void add_child_mem(LmnMembraneRef newmem) {
    newmem->prev = NULL;
    newmem->next = this->mem_child_head();
    newmem->parent = this;
    LMN_ASSERT(this);
    if (this->mem_child_head()) {
      this->child_head->prev = newmem;
    }
    this->child_head = newmem;
  }
  AtomListEntry *get_atomlist(LmnFunctor f) {
    if ((f < this->atomset_size) && this->atomset[f]) {
      return this->atomset[f];
    } else {
      return NULL;
    }
  }
  const AtomListEntry *get_atomlist(LmnFunctor f) const {
    if ((f < this->atomset_size) && this->atomset[f]) {
      return this->atomset[f];
    } else {
      return NULL;
    }
  }
  const char *MEM_NAME();
  void drop();
  unsigned long space();
  unsigned long root_space();
  void move_cells(LmnMembraneRef srcmem);
  void remove_proxies();
  void insert_proxies(LmnMembraneRef child_mem);
  void remove_temporary_proxies();
  void remove_toplevel_proxies();
  LmnMembraneRef copy();
  LmnMembraneRef copy_ex();
  //まだリファクタリングしていない
  void remove_ground(Vector *srcvec);
  void delete_ground(Vector *srcvec);
  BOOL equals(LmnMembraneRef mem2);
  void remove_mem(LmnMembraneRef mem);
  void free_rec();
  void delete_mem(LmnMembraneRef mem);
  void activate_ancestors();
  BOOL nmems(unsigned int count);
  int child_mem_num();
  unsigned int count_children();
  unsigned int count_descendants();
  BOOL nfreelinks(unsigned int count);
  void copy_rules(LmnMembraneRef src);
  void clearrules();
  void delete_ruleset(LmnRulesetId del_id);
};


#endif
