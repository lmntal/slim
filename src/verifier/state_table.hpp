/*
 * state_table.hpp
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
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
 */

#ifndef SLIM_VERIFIER_STATE_TABLE_HPP
#define SLIM_VERIFIER_STATE_TABLE_HPP

#include "state.hpp"

#include <cstddef>
#include <cstdint> /* intptr_t */
#include <cstring> /* memset */

#include <iterator>
#include <numeric>

struct State;
struct EWLock;
struct LmnBinStr;

struct statespace_type {
  int (*compare)(State *, State *); /* 状態の等価性判定を行う関数 */
  LmnBinStr *(*compress)(State *); /* 状態sの圧縮バイト列を計算して返す関数 */
};

struct StateTable {
  /* TODO: テーブルの初期サイズはいくつが適当か.
   * (固定サイズにしているモデル検査器は多い) */
  static constexpr size_t TABLE_DEFAULT_INIT_SIZE = 1U << 15;
  /* 1バケットあたりの平均長がこの値を越えた場合にresizeする */
  static constexpr size_t TABLE_DEFAULT_MAX_DENSITY = 5U;

  StateTable(int thread_num)
      : StateTable(thread_num, TABLE_DEFAULT_INIT_SIZE, nullptr) {}
  StateTable(int thread_num, StateTable *rehash_tbl)
      : StateTable(thread_num, TABLE_DEFAULT_INIT_SIZE, rehash_tbl) {}
  StateTable(int thread_num, unsigned long size, StateTable *rehash_tbl);
  ~StateTable();

  void clear() {
    std::fill(num.begin(), num.end(), 0);
    std::fill(num_dummy_.begin(), num_dummy_.end(), 0);
    std::fill(tbl.begin(), tbl.end(), nullptr);
  }

  unsigned long cap() const { return cap_; }
  unsigned long num_by_me() const { return this->num[env_my_thread_id()]; }
  unsigned long all_num() const {
    return std::accumulate(std::begin(num), std::end(num), 0);
  }
  unsigned long all_num_dummy() const {
    return std::accumulate(num_dummy_.begin(), num_dummy_.end(), 0);
  }
  unsigned long cap_density() const { return cap_density_; }
  unsigned long space() const;
  void resize(unsigned long);
  State *insert(State *ins, unsigned long *col = nullptr);
  void add_direct(State *s);
  void format_states();
  void memid_rehash(unsigned long hash);
  bool need_resize() const {
    return ((num_by_me() / cap_density()) > TABLE_DEFAULT_MAX_DENSITY);
  }
  void resize_if_needed() {
    if (this->need_resize()) {
      this->resize(this->cap());
    }
  }

private:
  void num_increment() { num[env_my_thread_id()]++; }
  void num_dummy_increment() { num_dummy_[env_my_thread_id()]++; }
  bool use_rehasher() const { return rehash_tbl_; }
  LmnBinStr *compress_state(State *s, LmnBinStr *bs);

  void memid_rehash(State *s);

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
    iterator(const iterator &itr)
        : table(itr.table), table_index(itr.table_index), ptr(itr.ptr) {}
    iterator(StateTable *t)
        : table(t), table_index(0), ptr(table->tbl[table_index]) {
      while (!ptr && table_index + 1 < table->cap())
        ptr = table->tbl[++table_index];
      if (ptr)
        next = ptr->next;
    };

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
  uint8_t thread_num;
  bool use_rehasher_;
  struct statespace_type *type;
  std::vector<unsigned long> num;
  unsigned long cap_;
  std::vector<unsigned long> num_dummy_;
  unsigned long cap_density_;
  std::vector<State *> tbl;
  EWLock *lock;
  StateTable *rehash_tbl_; /* rehashした際に登録するテーブル */
};

#endif /* SLIM_VERIFIER_STATE_TABLE_HPP */
