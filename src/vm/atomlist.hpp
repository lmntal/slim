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
LmnSymbolAtomRef atomlist_head(AtomListEntryRef lst);
LmnSymbolAtomRef lmn_atomlist_end(AtomListEntryRef lst);

struct AtomListIter_;

struct AtomListEntry {
  LmnSymbolAtomRef tail, head;
  int n;
  struct SimpleHashtbl *record;
  typedef struct AtomListIter_ iterator;
  iterator begin();
  iterator end();

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
    if (atomlist_head(e2) !=
        lmn_atomlist_end(e2)) { /* true if e2 is not empty */
      LMN_SATOM_SET_NEXT(this->tail, e2->head);
      LMN_SATOM_SET_PREV(e2->head, this->tail);
      LMN_SATOM_SET_NEXT(e2->tail, (LmnSymbolAtomRef)this);
      this->tail = e2->tail;
      this->n += e2->size();
    }
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
  // void mem_remove_symbol_atom(LmnSymbolAtomRef atom) {
  //   this->remove(atom);
  // }
  void move_atom_to_atomlist_tail(LmnSymbolAtomRef a) {
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a), LMN_SATOM_GET_NEXT_RAW(a));
    LMN_SATOM_SET_NEXT(this->tail, a);
    // ent->remove(a);
    // if (LMN_IS_PROXY_FUNCTOR(f)) {
    //   LMN_PROXY_SET_MEM(a, NULL);
    // } else if (f != LMN_UNIFY_FUNCTOR) {
    //   lmn_mem_symb_atom_dec(this);
    // }
    // mem_push_symbol_atom(this, a);
  }
};

void move_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem);
void move_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem);
void move_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                            LmnMembraneRef mem);

struct AtomListIter_
    : public std::iterator<std::forward_iterator_tag, LmnSymbolAtomRef> {
  LmnSymbolAtomRef a_index;
  AtomListEntryRef a_ent;
  // AtomListIter_(){

  // };
  AtomListIter_(AtomListEntryRef ent, LmnSymbolAtomRef index) {
    a_ent = ent;
    a_index = index;
  };

  // AtomListIter_(const AtomListIter_ &itr);

  AtomListIter_ &operator++() {
    a_index = LMN_SATOM_GET_NEXT_RAW(a_index);
    return *this;
  };
  AtomListIter_ operator++(int) {
    AtomListIter_ ret = *this;
    a_index = LMN_SATOM_GET_NEXT_RAW(a_index);
    return ret;
  };
  LmnSymbolAtomRef &operator*() { return this->a_index; };
  bool operator!=(const AtomListIter_ &itr) {
    return this->a_ent != itr.a_ent || this->a_index != itr.a_index;
  };
  bool operator==(const AtomListIter_ &itr) { return !(*this != itr); };
};

typedef int AtomListIter;
#define atomlist_iter_initializer(AS) (0)
#define atomlist_iter_condition(Mem, Iter) ((Iter) < lmn_mem_max_functor(Mem))
#define atomlist_iter_next(Iter) ((Iter)++)
#define atomlist_iter_get_entry(Mem, Iter) lmn_mem_get_atomlist(Mem, Iter)
#define atomlist_iter_get_functor(Iter) (Iter)

#define EACH_ATOMLIST_WITH_FUNC(MEM, ENT, F, CODE)                             \
  do {                                                                         \
    AtomListIter __iter;                                                       \
    for (__iter = atomlist_iter_initializer((MEM)->atomset);                   \
         atomlist_iter_condition(MEM, __iter); atomlist_iter_next(__iter)) {   \
      (ENT) = atomlist_iter_get_entry(MEM, __iter);                            \
      (F) = atomlist_iter_get_functor(__iter);                                 \
      if (!(ENT))                                                              \
        continue;                                                              \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#define EACH_ATOMLIST(MEM, ENT, CODE)                                          \
  do {                                                                         \
    AtomListIter __iter;                                                       \
    for (__iter = atomlist_iter_initializer((MEM)->atomset);                   \
         atomlist_iter_condition(MEM, __iter); atomlist_iter_next(__iter)) {   \
      (ENT) = atomlist_iter_get_entry(MEM, __iter);                            \
      if (!(ENT))                                                              \
        continue;                                                              \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)

/* アトムリストENTのアトムに対してCODEを実行する。
   それぞれのループでCODEを実行する前に、Vにアトムが割り当てられる。
   履歴アトムがアトムリストにある場合は、読み飛ばす */
#define EACH_ATOM(V, ENT, CODE)                                                \
  if ((ENT)) {                                                                 \
    for ((V) = atomlist_head((ENT)); (V) != lmn_atomlist_end((ENT));           \
         (V) = LMN_SATOM_GET_NEXT_RAW((LmnSymbolAtomRef)(V))) {                \
      if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)(V)) !=                      \
          LMN_RESUME_FUNCTOR) {                                                \
        (CODE);                                                                \
      }                                                                        \
    }                                                                          \
  }

#define EACH_ATOM_THREAD(V, ENT, ID, NUM, CODE)                                \
  int id = (ID);                                                               \
  if ((ENT)) {                                                                 \
    for ((V) = atomlist_head((ENT)); (V) != lmn_atomlist_end((ENT));           \
         (V) = LMN_SATOM_GET_NEXT_RAW((LmnSymbolAtomRef)(V))) {                \
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
    AtomListEntry *__ent = lmn_mem_get_atomlist((MEM), (F));                   \
    if (__ent) {                                                               \
      for ((V) = atomlist_head(__ent); (V) != lmn_atomlist_end(__ent);         \
           (V) = LMN_SATOM_GET_NEXT_RAW((V))) {                                \
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (0)

#define ALL_ATOMS(MEM, V, CODE)                                                \
  do {                                                                         \
    AtomListEntryRef __ent;                                                    \
    EACH_ATOMLIST((MEM), __ent, ({                                             \
                    for ((V) = atomlist_head(__ent);                           \
                         (V) != lmn_atomlist_end(__ent);                       \
                         (V) = LMN_SATOM_GET_NEXT_RAW((V))) {                  \
                      if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {  \
                        (CODE);                                                \
                      }                                                        \
                    }                                                          \
                  }));                                                         \
  } while (0)

#endif
