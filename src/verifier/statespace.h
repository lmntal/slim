/*
 * statespace.h
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

/** @author Masato Gocho
 *  Closed Address Hash Table / Parallel Hash Table for State Management Table
 */

#ifndef LMN_STATESPACE_H
#define LMN_STATESPACE_H

/**
 * @ingroup  Verifier
 * @defgroup StateSpace
 * @{
 */

#include "../lmntal.h"
#include "automata.h"
#include "delta_membrane.h"
#include "element/element.h"
#include "mem_encode.h"
#include "tree_compress.h"
#include "state.hpp"

struct statespace_type {
  int (*compare)(State *, State *); /* 状態の等価性判定を行う関数 */
  LmnBinStrRef (*compress)(State *); /* 状態sの圧縮バイト列を計算して返す関数 */
};

struct StateSpace {
  BYTE tbl_type; /* なんらかの特殊操作を行うためのフラグフィールド */
  BOOL is_formated; /* ハッシュ表の並びを崩した整列を行った場合に真 */
  /* 2bytes alignment */
  unsigned int thread_num; /* 本テーブルの操作スレッド数 */

  FILE *out;          /* dump先 */
  State *init_state;  /* 初期状態 */
  Vector *end_states; /* 最終状態の集合 */
  StateTable *tbl; /* mhash値をkeyに, 状態のアドレスを登録する状態管理表 */
  StateTable
      *memid_tbl; /* memid_hashをkeyに, 状態のアドレスを登録する状態管理表 */
  StateTable *acc_tbl;
  StateTable *acc_memid_tbl;

  AutomataRef property_automata; /* Never Clainへのポインタ */
  Vector *propsyms;              /* 命題記号定義へのポインタ */

#ifdef PROFILE
  HashSet memid_hashes; /* 膜のIDで同型性の判定を行うハッシュ値(mhash)のSet */
#endif
};

#define statespace_has_property(SS) ((SS)->property_automata)
#define statespace_automata(SS) ((SS)->property_automata)
#define statespace_propsyms(SS) ((SS)->propsyms)

/* the member "tbl_type" in struct StateSpace */
#define SS_MEMID_MASK (0x01U)
#define SS_REHASHER_MASK (0x01U << 1)

#define statespace_use_memenc(SS) ((SS)->tbl_type & SS_MEMID_MASK)
#define statespace_set_memenc(SS) ((SS)->tbl_type |= SS_MEMID_MASK)
#define statespace_unset_memenc(SS) ((SS)->tbl_type &= (~SS_MEMID_MASK))
#define statespace_use_rehasher(SS) ((SS)->tbl_type & SS_REHASHER_MASK)
#define statespace_set_rehasher(SS) ((SS)->tbl_type |= SS_REHASHER_MASK)
#define statespace_unset_rehasher(SS) ((SS)->tbl_type &= (~SS_REHASHER_MASK))

struct StateTable {
  /* TODO: テーブルの初期サイズはいくつが適当か.
   * (固定サイズにしているモデル検査器は多い) */
  static constexpr size_t TABLE_DEFAULT_INIT_SIZE = 1U << 15;
  StateTable(int thread_num)
      : StateTable(thread_num, TABLE_DEFAULT_INIT_SIZE) {}
  StateTable(int thread_num, unsigned long size);
  ~StateTable();

  void clear() {
    unsigned long i;

    for (i = 0; i < this->thread_num; i++) {
      this->num[i] = 0;
      this->num_dummy_[i] = 0;
    }

    memset(this->tbl, 0x00, cap_ * (sizeof(State *)));
  }
  unsigned long all_num() const;
  unsigned long num_by_me() const;
  unsigned long space() const;
  unsigned long cap() const { return cap_; }
  void resize(unsigned long);
  void num_increment();
  void num_dummy_increment();
  unsigned long num_dummy(size_t idx) { return num_dummy_[idx]; }
  unsigned long cap_density() const { return cap_density_; }
  void set_rehasher() { use_rehasher_ = true; }
  bool use_rehasher() const;
  LmnBinStrRef compress_state(State *s, LmnBinStrRef bs);
  State *insert(State *ins, unsigned long *col = nullptr);
  void add_direct(State *s);
  void format_states();
  StateTable *rehash_table();
  void set_rehash_table(StateTable *);
  void set_lock(EWLock *);
  void memid_rehash(State *s);
  void memid_rehash(unsigned long hash) {
    for (int i = 0; i < this->cap(); i++) {
      auto ptr = this->tbl[i];

      while (ptr) {
        State *next = ptr->next;
        if (state_hash(ptr) == hash) { /* statespace_mem_encode_f */
          this->memid_rehash(ptr);
        }
        ptr = next;
      }
    }
  }

  class iterator {
    StateTable *table;
    size_t table_index;
    State *ptr;
    State *next;

  public:
    using difference_type = intptr_t;
    using value_type = State *;
    using pointer = State **;
    using reference = State *&;
    using iterator_category = typename std::input_iterator_tag;

    iterator() : ptr(nullptr) {}
    iterator(StateTable *t)
        : table(t), table_index(0), ptr(table->tbl[table_index]) {
      while (!ptr && table_index + 1 < table->cap())
        ptr = table->tbl[++table_index];
      if (ptr)
        next = ptr->next;
    };
    iterator(const iterator &itr)
        : table(itr.table), table_index(itr.table_index), ptr(itr.ptr) {}

    iterator &operator++() {
      ptr = next;
      while (!ptr && table_index + 1 < table->cap())
        ptr = table->tbl[++table_index];
      if (ptr)
        next = ptr->next;
      return *this;
    }
    iterator operator++(int) {
      auto ret = *this;
      ++ret;
      return ret;
    }
    State *&operator*() { return ptr; };

    bool operator==(const iterator &itr) const { return ptr == itr.ptr; };
    bool operator!=(const iterator &itr) const { return !(*this == itr); };
  };
  iterator begin() { return iterator(this); }
  iterator end() { return iterator(); }

private:
  BYTE thread_num;
  BOOL use_rehasher_;
  struct statespace_type *type;
  unsigned long *num;
  unsigned long cap_;
  unsigned long *num_dummy_;
  unsigned long cap_density_;
  State **tbl;
  EWLock *lock;
  StateTable *rehash_tbl_; /* rehashした際に登録するテーブル */
};

/** -----------
 *  StateTable
 */

// void statetable_format_states(StateTable *st);

// void statetable_set_lock(StateTable *st, EWLock *lock);
// void statetable_set_rehasher(StateTable *st);
// BOOL statetable_use_rehasher(StateTable *st);
// unsigned long statetable_num_by_me(StateTable *st);
// unsigned long statetable_num(StateTable *st);
// unsigned long statetable_cap(StateTable *st);
// unsigned long statetable_cap_density(StateTable *st);
// void statetable_num_add(StateTable *st, unsigned long n);
// void statetable_num_sub(StateTable *st, unsigned long n);
// void statetable_dummy_add(StateTable *st, unsigned long n);
// void statetable_dummy_sub(StateTable *st, unsigned long n);
// void statetable_set_rehash_tbl(StateTable *st, StateTable *rehash_tbl);
// StateTable *statetable_rehash_tbl(StateTable *st);
// unsigned long statetable_space(StateTable *tbl);

/** -----------
 *  StateSpace
 */

StateSpaceRef statespace_make(AutomataRef a, Vector *psyms);
StateSpaceRef statespace_make_for_parallel(int thread_num, AutomataRef a,
                                           Vector *psyms);
void statespace_free(StateSpaceRef ss);
void statespace_add_direct(StateSpaceRef ss, State *s);
State *statespace_insert(StateSpaceRef ss, State *s);
State *statespace_insert_delta(StateSpaceRef ss, State *s,
                               struct MemDeltaRoot *d);
void statespace_foreach(StateSpaceRef ss, void (*func)(ANYARGS), LmnWord _arg1);
void statespace_format_states(StateSpaceRef ss);
void statespace_clear(StateSpaceRef ss);
void statespace_ends_dumper(StateSpaceRef ss);
void statespace_dumper(StateSpaceRef ss);

unsigned long statespace_num_raw(StateSpaceRef ss);
unsigned long statespace_num(StateSpaceRef ss);
unsigned long statespace_dummy_num(StateSpaceRef ss);
unsigned long statespace_end_num(StateSpaceRef ss);
State *statespace_init_state(StateSpaceRef ss);
void statespace_set_init_state(StateSpaceRef ss, State *init_state,
                               BOOL enable_compact);
void statespace_add_end_state(StateSpaceRef ss, State *s);
const Vector *statespace_end_states(StateSpaceRef ss);
StateTable *statespace_tbl(StateSpaceRef ss);
StateTable *statespace_memid_tbl(StateSpaceRef ss);
StateTable *statespace_accept_tbl(StateSpaceRef ss);
StateTable *statespace_accept_memid_tbl(StateSpaceRef ss);
unsigned long statespace_space(StateSpaceRef ss);

/* @} */

#endif
