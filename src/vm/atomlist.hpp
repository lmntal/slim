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

struct SimpleHashtbl;
struct AtomListEntry;

LmnSymbolAtomRef atomlist_head(AtomListEntry *lst);
LmnSymbolAtomRef lmn_atomlist_end(AtomListEntry *lst);

struct AtomListEntry {
  LmnSymbolAtomRef tail, head;
  int n;
  struct SimpleHashtbl *record;

  bool is_empty() {
    return this->head == reinterpret_cast<LmnSymbolAtomRef>(this);
  }

  void set_empty() {
    LMN_SATOM_SET_PREV(reinterpret_cast<LmnSymbolAtomRef>(this),
                       reinterpret_cast<LmnSymbolAtomRef>(this));
    LMN_SATOM_SET_NEXT(reinterpret_cast<LmnSymbolAtomRef>(this),
                       reinterpret_cast<LmnSymbolAtomRef>(this));
    this->n = 0;
  }

  /* アトムリストALからアトムAを削除する.
   * ただし, リストのつなぎ変えだけを行い,
   * 膜からのアトムAのdeleteやatomのfreeはしない */
  void remove(LmnSymbolAtomRef a) {
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a), LMN_SATOM_GET_NEXT_RAW(a));
    this->n -= 1;
  }

  /* アトムリストALの末尾にアトムAを追加する. */
  void push(LmnSymbolAtomRef a) {
    LMN_SATOM_SET_NEXT(a, reinterpret_cast<LmnSymbolAtomRef>(this));
    LMN_SATOM_SET_PREV(a, this->tail);
    LMN_SATOM_SET_NEXT(this->tail, a);
    this->tail = a;
    this->n += 1;
  }

  int size() { return this->n; }

  void append(AtomListEntry *e2) {
    if (e2->is_empty())
      return;

    LMN_SATOM_SET_NEXT(this->tail, e2->head);
    LMN_SATOM_SET_PREV(e2->head, this->tail);
    LMN_SATOM_SET_NEXT(e2->tail, reinterpret_cast<LmnSymbolAtomRef>(this));
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
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a), LMN_SATOM_GET_NEXT_RAW(a));
    LMN_SATOM_SET_NEXT(this->tail, a);
  }

  class const_iterator {
    LmnSymbolAtomRef a_index;
    const AtomListEntry *a_ent;

  public:
    using difference_type = intptr_t;
    using value_type = LmnSymbolAtomRef;
    using pointer = LmnSymbolAtomRef *;
    using reference = LmnSymbolAtomRef &;
    typedef typename std::bidirectional_iterator_tag iterator_category;

    const_iterator(const AtomListEntry *ent, LmnSymbolAtomRef index) {
      a_ent = ent;
      a_index = index;
    };

    const_iterator &operator++() {
      a_index = LMN_SATOM_GET_NEXT_RAW(a_index);
      return *this;
    };
    const_iterator operator++(int) {
      auto ret = *this;
      ++ret;
      return ret;
    };
    const_iterator operator--() {
      a_index = LMN_SATOM_GET_PREV(a_index);
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

  const_iterator insert(int findatomid, LmnSymbolAtomRef record) {
    hashtbl_put(this->record, findatomid, (HashKeyType)record);
    auto start_atom = atomlist_head(this);
    /* 履歴アトムを挿入する */
    LMN_SATOM_SET_NEXT((LmnSymbolAtomRef)this, record);
    LMN_SATOM_SET_PREV(record, (LmnSymbolAtomRef)this);
    LMN_SATOM_SET_NEXT(record, start_atom);
    LMN_SATOM_SET_PREV(start_atom, record);
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
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(*i), LMN_SATOM_GET_PREV(*i));
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(*i), LMN_SATOM_GET_NEXT_RAW(*i));

    auto next = std::next(position, 1);
    LMN_SATOM_SET_NEXT(*position, *i);
    LMN_SATOM_SET_PREV(*i, *position);
    LMN_SATOM_SET_NEXT(*i, *next);
    LMN_SATOM_SET_PREV(*next, *i);
  }
};

void move_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembrane *mem);
void move_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembrane *mem);
void move_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                            LmnMembrane *mem);

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
      if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)(V)) !=                      \
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
      if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)(V)) !=                      \
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
         (V) = LMN_SATOM_GET_NEXT_RAW((LmnSymbolAtomRef)(V))) {                \
      if ((V) == lmn_atomlist_end((ENT))) {                                    \
        (V) = atomlist_head((ENT));                                            \
        id = (ID);                                                             \
        flag--;                                                                \
      }                                                                        \
      if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)(V)) !=                      \
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
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                \
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
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    };                                                                         \
  } while (0)

#endif
