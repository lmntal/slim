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
#include "state.hpp"
#include "tree_compress.h"

struct statespace_type {
  int (*compare)(State *, State *); /* 状態の等価性判定を行う関数 */
  LmnBinStrRef (*compress)(State *); /* 状態sの圧縮バイト列を計算して返す関数 */
};

/* the member "tbl_type" in struct StateSpace */
#define SS_MEMID_MASK (0x01U)
#define SS_REHASHER_MASK (0x01U << 1)

struct StateSpace {
  StateSpace();
  StateSpace(AutomataRef a, Vector *psyms) : StateSpace() {
    this->end_states = vec_make(64);
    this->property_automata = a;
    this->propsyms = psyms;
    this->make_table();
  }
  /* for parallel model checker mode */
  StateSpace(int thread_num, AutomataRef a, Vector *psyms) : StateSpace() {
    this->thread_num = thread_num;
    this->property_automata = a;
    this->propsyms = psyms;
    this->end_states = LMN_NALLOC(struct Vector, thread_num);
    for (int i = 0; i < this->thread_num; i++) {
      vec_init(&this->end_states[i], 48);
    }

    this->make_table();
  }
  ~StateSpace();

  void add_memid_hash(unsigned long hash);
  State *insert(State *s);
  State *insert_delta(State *s, struct MemDeltaRoot *d);
  void add_direct(State *s);
  void set_init_state(State *init_state, BOOL enable_binstr);
  void dump();
  void dump_all_states();
  void dump_all_transitions();
  void dump_all_labels();
  void format_states();
  unsigned long space();
  void dump_ends();
  unsigned long num();
  unsigned long num_raw();
  unsigned long dummy_num();
  unsigned long num_of_ends() const;
  State *initial_state() { return init_state; }
  void mark_as_end(State *);

  StateTable *accept_tbl() { return acc_tbl; }
  StateTable *accept_memid_tbl() { return acc_memid_tbl; }

  void make_table();

  /* hashが膜のIDを計算しているハッシュならば真、そうでなければ偽を返す */
  bool is_memid_hash(unsigned long hash) {
    return hashset_contains(&this->memid_hashes, hash);
  }

  bool use_memenc() const { return ((this)->tbl_type & SS_MEMID_MASK); }
  void set_memenc() { ((this)->tbl_type |= SS_MEMID_MASK); }
  void set_rehasher() { ((this)->tbl_type |= SS_REHASHER_MASK); }
  bool is_formatted() const { return this->is_formated; }
  bool has_property() const { return this->property_automata; }
  AutomataRef automata() { return this->property_automata; }
  Vector *prop_symbols() { return this->propsyms; }

  FILE *output() { return out; }

  std::vector<State *> all_states() const;

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

  unsigned long cap() const { return cap_; }
  unsigned long num_by_me() const;
  unsigned long all_num() const;
  unsigned long num_dummy(size_t idx) const { return num_dummy_[idx]; }
  unsigned long cap_density() const { return cap_density_; }
  unsigned long space() const;
  void resize(unsigned long);
  State *insert(State *ins, unsigned long *col = nullptr);
  void add_direct(State *s);
  void format_states();
  void set_rehash_table(StateTable *);
  void set_lock(EWLock *);
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

private:
  void num_increment();
  void num_dummy_increment();
  void set_rehasher() { use_rehasher_ = true; }
  bool use_rehasher() const;
  LmnBinStrRef compress_state(State *s, LmnBinStrRef bs);

  void memid_rehash(State *s);
  StateTable *rehash_table();

  /* 既に計算済のバイナリストリングbsを状態sに登録する.
   * statetable_{insert/add_direct}内の排他制御ブロック内で呼び出す. */
  static void state_set_compress_for_table(State *s, LmnBinStrRef bs) {
    if (!s->is_encoded() && bs) {
      s->state_set_binstr(bs);
    }
  }

public:
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

/* @} */

#endif
