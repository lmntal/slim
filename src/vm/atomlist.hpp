/*
 * atomlist.hpp
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

#ifndef LMN_ATOMLIST_HPP
#define LMN_ATOMLIST_HPP

#include "atom.h"
#include <iterator>
#include <random>
#include <vector>

struct SimpleHashtbl;
struct AtomListEntry;

LmnSymbolAtomRef atomlist_head(AtomListEntry *lst);
LmnSymbolAtomRef lmn_atomlist_end(AtomListEntry *lst);

struct AtomListEntry {
  LmnSymbolAtomRef tail, head;
  //  LmnSymbolAtomRef shuffle_tail, shuffle_head;
  int n;
  int n_record = 0; // 履歴管理用アトムのための変数(nakata)
  struct SimpleHashtbl *record;
  AtomListEntry *d1, *d2;

  bool is_empty() {
    return this->head == reinterpret_cast<LmnSymbolAtomRef>(this);
  }

  void set_empty() {
    reinterpret_cast<LmnSymbolAtomRef>(this)->set_prev(reinterpret_cast<LmnSymbolAtomRef>(this));
    reinterpret_cast<LmnSymbolAtomRef>(this)->set_next(reinterpret_cast<LmnSymbolAtomRef>(this));
    this->n = 0;
  }

  /* アトムリストALからアトムAを削除する.
   * ただし, リストのつなぎ変えだけを行い,
   * 膜からのアトムAのdeleteやatomのfreeはしない */
  void remove(LmnSymbolAtomRef a) {
    a->get_next()->set_prev(a->get_prev());
    a->get_prev()->set_next(a->get_next());
    this->n -= 1;
  }

  /* アトムリストALの末尾にアトムAを追加する. */
  // 後ろにつなげている.
  // ランダムに位置につなげる->楽？
  /*
    先頭と最後尾を保持しておいて
    リング状にする. ランダムな位置をheadにして, shuffle-tailやshuffle-head変数を追加する.
    -> 非決定実行いけるかな？
   */
  void push(LmnSymbolAtomRef a) {
    if(lmn_env.shuffle_atom) {
      std::random_device rnd;
      int startpoint = rnd() % (n+1);
      
      if(startpoint == 0) { // head処理
        a->set_next(this->head);
        a->set_prev(reinterpret_cast<LmnSymbolAtomRef>(this));
        this->head->set_prev(a);
        this->head = a;
      } else if(startpoint == n) { // tail 処理
        a->set_next(reinterpret_cast<LmnSymbolAtomRef>(this));
        a->set_prev(this->tail);
        this->tail->set_next(a);
        this->tail = a;
      } else {
        // 挿入するアトムまで移動するためのアトム
        auto insertpoint_atom = this->head;
        for(int i = 0; i < startpoint; i++) {
          insertpoint_atom = insertpoint_atom->get_next();
        }
        // 挿入した
        a->set_prev(insertpoint_atom);
        a->set_next(insertpoint_atom->get_next());
        insertpoint_atom->set_next(a);
        a->get_next()->set_prev(a);
      }
    } else {
      a->set_next(reinterpret_cast<LmnSymbolAtomRef>(this));
      a->set_prev(this->tail);
      this->tail->set_next(a);
      this->tail = a;
    }
    this->n += 1;
  }

  int size() { return this->n; }

  void append(AtomListEntry *e2) {
    if (e2->is_empty())
      return;

    this->tail->set_next(e2->head);
    e2->head->set_prev(this->tail);
    e2->tail->set_next(reinterpret_cast<LmnSymbolAtomRef>(this));
    this->tail = e2->tail;
    this->n += e2->size();
    e2->set_empty();
  }

  /* return NULL when atomlist doesn't exist. */
  LmnSymbolAtomRef get_record(int findatomid) {
    if (this->record) {
      return (LmnSymbolAtomRef)hashtbl_get_default(this->record, findatomid, 0);
    } else {
      this->record = hashtbl_make(4);
      return NULL;
    }
  }

  void put_record(int id, LmnAtomRef record) {
    hashtbl_put(this->record, id, (HashKeyType)record);
  }

  void move_atom_to_atomlist_tail(LmnSymbolAtomRef a) {
    a->get_next()->set_prev(a->get_prev());
    a->get_prev()->set_next(a->get_next());
    this->tail->set_next(a);
  }

  /* 以下, 履歴管理用アトムのためのコード(nakata) */
  void make_head_record() {
    //insert head atom
    LmnSymbolAtomRef head_atom = new LmnSymbolAtom;
    auto funct = lmn_functor_table->intern(ANONYMOUS, lmn_intern("head"), 0);
    head_atom->procId = -2;
    head_atom->set_functor(funct);
    head_atom->record_flag = true;

    head_atom->set_next(this->head);
    this->head->set_prev(head_atom);
    head_atom->set_prev(this->head->prev);
    this->head = head_atom;

    this->n += 1;
    this->n_record += 1;
  }
  
  LmnSymbolAtomRef make_new_recordatom(bool record_flag, LmnFunctor f) {
    // exception
    if(!this->head->record_flag) {
      fprintf(stdout, "\033[31mthere isn't the head atom for record\33[0m\n");
      make_head_record();
    }

    // make new record atom
    LmnSymbolAtomRef newatom = new LmnSymbolAtom;

    newatom->procId = -1;
    newatom->set_functor(f);
    newatom->record_flag = true;

    // change the pointer
    auto next_head_atom = this->head->get_next();

    next_head_atom->set_prev(newatom);
    newatom->set_next(next_head_atom);

    this->head->set_next(newatom);
    newatom->set_prev(this->head);
    
    this->n += 1;
    this->n_record += 1;

    return newatom;
  }
  /* ここまで(nakata) */
  
  class const_iterator {
    LmnSymbolAtomRef a_index;
    const AtomListEntry *a_ent;

  public:
    using difference_type = intptr_t;
    using value_type = LmnSymbolAtomRef;
    using pointer = LmnSymbolAtomRef *;
    using reference = LmnSymbolAtomRef &;
    typedef typename std::bidirectional_iterator_tag iterator_category;

    const_iterator() : a_ent(nullptr), a_index(nullptr) {}
    const_iterator(const AtomListEntry *ent, LmnSymbolAtomRef index) {
      a_ent = ent;
      a_index = index;
    };

    const_iterator &operator++() {
      a_index = a_index->get_next();
      return *this;
    };
    const_iterator operator++(int) {
      auto ret = *this;
      ++ret;
      return ret;
    };
    const_iterator operator--() {
      a_index = a_index->get_prev();
      return *this;
    }
    const_iterator operator--(int i) {
      auto ret = *this;
      ++ret;
      return ret;
    }
    LmnSymbolAtomRef &operator*() { return this->a_index; };
    const LmnSymbolAtomRef &operator*() const { return this->a_index; };

    bool operator!=(const const_iterator &itr) const {
      return this->a_ent != itr.a_ent || this->a_index != itr.a_index;
    };
    bool operator==(const const_iterator &itr) const {
      return !(*this != itr);
    };
  };
  const_iterator begin() const { return const_iterator(this, head); }
  const_iterator end() const {
    return const_iterator(this, reinterpret_cast<LmnSymbolAtomRef>(
                                    const_cast<AtomListEntry *>(this)));
  }

  const_iterator make_iterator(LmnSymbolAtomRef atom) const {
    return const_iterator(this, atom);
  }
  
  const_iterator insert(int findatomid, LmnSymbolAtomRef record) {
    hashtbl_put(this->record, findatomid, (HashKeyType)record);
    auto start_atom = atomlist_head(this);
    /* 履歴アトムを挿入する */
    ((LmnSymbolAtomRef)this)->set_next(record);
    record->set_prev((LmnSymbolAtomRef)this);
    record->set_next(start_atom);
    start_atom->set_prev(record);
    return const_iterator(this, record);
  }

  const_iterator find_record(int findatomid) {
    if (this->record) {
      return const_iterator(this, (LmnSymbolAtomRef)hashtbl_get_default(
                                      this->record, findatomid, 0));
    } else {
      this->record = hashtbl_make(4);
      return end();
    }
  }

  void splice(const_iterator position, AtomListEntry &x, const_iterator i) {
    (*i)->get_next()->set_prev((*i)->get_prev());
    (*i)->get_prev()->set_next((*i)->get_next());

    auto next = std::next(position, 1);
    (*position)->set_next(*i);
    (*i)->set_prev(*position);
    (*i)->set_next(*next);
    (*next)->set_prev(*i);
  }
};

void move_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembrane *mem);
void move_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembrane *mem);
void move_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                            LmnMembrane *mem);
void move_diffatomlist_to_atomlist_tail2(LmnFunctor f, LmnMembrane *mem);

/* RecordAtom, RecordList クラスは履歴管理用アトムのためのクラス */
class RecordAtom {
  LmnSymbolAtomRef atom_record;
  LmnSymbolAtomRef head_record; // necessary to move atom_record to the beginning(head) after backtracking
public:
  bool first_time;
  RecordAtom(LmnSymbolAtomRef atom_record, LmnSymbolAtomRef head_record, int rule_number) {
    this->atom_record = atom_record;
    this->head_record = head_record;
    this->atom_record->rule_number = rule_number;
    first_time = true;
  }

  void go_head() {
    this->atom_record->swap_to_head(this->head_record);
  }

  void record_forward() {
    this->atom_record->atom_swap_forward();
  }

  LmnSymbolAtomRef get_record() {
    return atom_record;
  }

  LmnSymbolAtomRef get_head() {
    return head_record;
  }

  void set_record(LmnSymbolAtomRef atom_record) {
    this->atom_record = atom_record;
  }

  void set_head(LmnSymbolAtomRef head_record) {
    this->head_record = head_record;
  }
};


class RecordList {
  int latest_rule_number = 0;
  std::vector<bool> delete_flag;
public:
  std::vector<bool> start_flag;
  std::vector<std::vector<int>> rule_reset;
  std::vector<std::vector<bool>> loop_flag;
  std::vector<std::vector<RecordAtom*>> atoms;

  RecordList(int rule_number) {
    latest_rule_number = rule_number;
  }

  int get_rule_number() {
    return latest_rule_number;
  }

  void set_rule_number(int rule_number) {
    latest_rule_number = rule_number;
  }

  void set_delete_flag(bool flag) {
    delete_flag.push_back(flag);
  }
  
  void set_delete_flag(bool flag, int rule_number) {
    if(rule_number >= delete_flag.size()) return;
    delete_flag[rule_number] = flag;
  }

  bool get_delete_flag(int rule_number) {
    return delete_flag[rule_number];
  }

  void push_back_newatom(AtomListEntry* atomlist_ent, int rule_number, size_t reg) {
    if(rule_number >= this->atoms.size()) {
      // size error
      fprintf(stdout, "\033[31mpush_back_newatom is fault : atomlist.hpp\33[0m\n");
      exit(-1);
    }

    auto func_record = lmn_functor_table->intern(ANONYMOUS, lmn_intern("record"), 0);
    RecordAtom* atom = new RecordAtom(atomlist_ent->make_new_recordatom(true, func_record), atomlist_ent->head, rule_number);
    this->atoms[rule_number].resize(reg);
    this->loop_flag[rule_number].resize(reg);
    this->atoms[rule_number][reg-1] = atom;
    this->loop_flag[rule_number][reg-1] = false;
  }

  void loop(AtomListEntry* atomlist_ent, int rule_number, int reg) {
    auto recordatom = atoms[rule_number][reg-1];
    auto atom = recordatom->get_record();
    if(atom == atomlist_ent->tail && loop_flag[rule_number][reg-1]) {
      loop_flag[rule_number][reg-1] = false;
      recordatom->go_head();
    }
    while(atom->next->record_flag && atom != atomlist_ent->tail) {
      atom->atom_swap_forward();
    }
  }

  void loop_back(int rule_number, int reg) {
    auto atom = atoms[rule_number][reg-1]->get_record();
    auto head = atoms[rule_number][reg-1]->get_head();

    if(atom->prev != head) atom->prev->atom_swap_forward();
    while (atom->prev->record_flag && atom->prev != head) {
      atom->prev->atom_swap_forward();
    }
  }

  void push_reset(LmnFunctor f) {
    for(int i = 0; i < atoms.size(); i++) {
      if(rule_reset[i][f]) {
	for(int j = 0; j < atoms[i].size(); j++) {
	  if(atoms[i][j]) {
	    loop_flag[i][j] = true;
	  }
	}
      }
    }
  }

  void push_reset_all() {
    for(int i = 0; i < atoms.size(); i++) {
      for(int j = 0; j < atoms[i].size(); j++) {
	if(atoms[i][j]) loop_flag[i][j] = true;
      }
    }
  }
};

extern RecordList record_list;

#define EACH_ATOMLIST_WITH_FUNC(MEM, ENT, F, CODE)                             \
  do {                                                                         \
    for (auto it : (MEM)->atom_lists()) {                                      \
      (ENT) = it.second;                                                       \
      (F) = it.first;                                                          \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#define EACH_ATOMLIST(MEM, ENT, CODE)                                          \
  do {                                                                         \
    for (auto it : (MEM)->atom_lists()) {                                      \
      (ENT) = it.second;                                                       \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)

/* アトムリストENTのアトムに対してCODEを実行する。
   それぞれのループでCODEを実行する前に、Vにアトムが割り当てられる。
   履歴アトムがアトムリストにある場合は、読み飛ばす */
#define EACH_ATOM(V, ENT, CODE)                                                \
  if ((ENT)) {                                                                 \
    for (auto iter_ : *(ENT)) {                                                \
      (V) = iter_;                                                             \
      if (((LmnSymbolAtomRef)(V))->get_functor() !=                      \
          LMN_RESUME_FUNCTOR) {                                                \
        (CODE);                                                                \
      }                                                                        \
    }                                                                          \
  }

#define EACH_ATOM_THREAD(V, ENT, ID, NUM, CODE)                                \
  int id = (ID);                                                               \
  if ((ENT)) {                                                                 \
    for (auto iter_ : *(ENT)) {                                                \
      (V) = iter_;                                                             \
      if (((LmnSymbolAtomRef)(V))->get_functor() !=                      \
              LMN_RESUME_FUNCTOR &&                                            \
          id == 0) {                                                           \
        (CODE);                                                                \
        id = (NUM);                                                            \
      }                                                                        \
      id--;                                                                    \
    }                                                                          \
  }

#define EACH_ATOM_THREAD_OPT(V, ENT, ID, NUM, START, CODE)                     \
  int id = (ID);                                                               \
  int flag = 1;                                                                \
  if ((ENT)) {                                                                 \
    if ((START) == NULL) {                                                     \
      (V) = atomlist_head((ENT));                                              \
      flag--;                                                                  \
    } else {                                                                   \
      (V) = (START);                                                           \
    }                                                                          \
    for (; (V) != lmn_atomlist_end((ENT)) || flag;                             \
         (V) = ((LmnSymbolAtomRef)(V))->get_next()) {                \
      if ((V) == lmn_atomlist_end((ENT))) {                                    \
        (V) = atomlist_head((ENT));                                            \
        id = (ID);                                                             \
        flag--;                                                                \
      }                                                                        \
      if (((LmnSymbolAtomRef)(V))->get_functor() !=                      \
              LMN_RESUME_FUNCTOR &&                                            \
          id == 0) {                                                           \
        (CODE);                                                                \
        id = (NUM);                                                            \
      }                                                                        \
      id--;                                                                    \
    }                                                                          \
  }

#define EACH_FUNC_ATOM(MEM, F, V, CODE)                                        \
  do {                                                                         \
    AtomListEntry *__ent = (MEM)->get_atomlist((F));			\
    if (__ent) {                                                               \
      for (auto iter_ : *__ent)                                                \
        ;                                                                      \
      {                                                                        \
        (V) = iter_;                                                           \
        if ((V)->get_functor() != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (0)

#define ALL_ATOMS(MEM, V, CODE)                                                \
  do {                                                                         \
    for (auto ent_ : (MEM)->atom_lists()) {                                    \
      for (auto iter_ : *ent_.second) {                                        \
        (V) = iter_;                                                           \
        if ((V)->get_functor() != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    };                                                                         \
  } while (0)

#endif
