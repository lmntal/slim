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


#include "../lmntal.h"
#include "element/element.h"

#include "state.hpp"


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

#endif
