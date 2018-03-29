/*
 * process_table.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 */

#ifndef PROCESS_TABLE_HPP
#define PROCESS_TABLE_HPP

#ifndef PROC_TBL_DEFAULT_SIZE
#define PROC_TBL_DEFAULT_SIZE  128U
#endif

#include <limits>
#include <stdexcept>
#include <iterator>
#include <utility>

extern "C" {
#include "lmntal.h"
}

extern "C++" {
/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
template<typename Value> struct ProcessTable {
  using Key = LmnWord;
  using limit = std::numeric_limits<Key>;
  class iterator;
  friend iterator;

  unsigned long n;
  unsigned long size;
  unsigned long num_buckets;
  Value **tbl;

  ProcessTable(unsigned long size) {
    this->n    = 0;
    this->size = size;
    this->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
    this->tbl = LMN_CALLOC(Key *, this->num_buckets);
  }

  ProcessTable() {
    ProcessTable(PROC_TBL_BUCKETS_SIZE);
  }

  ~ProcessTable() {
    for (int i = 0; i < this->num_buckets; i++) {
      LMN_FREE(this->tbl[i]);
    }
    LMN_FREE(this->tbl);
  }

  static void* operator new(std::size_t size) {
    return lmn_malloc(size);
  }
  static void operator delete(void *p) {
    lmn_free(p);
  }

  bool operator==(const ProcessTable<Value> *b) {
    if (this->n != b->n) return false;
    else {
      unsigned int a_checked = 0;

      for (int i = 0; i < this->num_buckets; i++) {
        if (!this->tbl[i] && !b->tbl[i]) continue;

        for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && a_checked < this->n; j++) {
          Key va = (this->tbl[i]) ? this->tbl[i][j] : limit::max();
          Key vb = (b->tbl[i]) ? b->tbl[i][j] : limit::max();
          if (va != vb) return false;
          if (va != limit::max()) a_checked++;
        }
      }

      return true;
    }
  }

  const Value &operator[](Key key) const {
    if (!this->contains(key)) throw new std::out_of_range("accessed with invalid key");
    return this->tbl[key / PROC_TBL_BUCKETS_SIZE][key % PROC_TBL_BUCKETS_SIZE];
  }

  bool contains(Key key) const {
    return key < this->size && this->tbl[key / PROC_TBL_BUCKETS_SIZE] &&
      this->tbl[key / PROC_TBL_BUCKETS_SIZE][key % PROC_TBL_BUCKETS_SIZE] != limit::max();
  }

  void put(Key key, Value value) {
#ifdef DEBUG
    if (value == limit::max()) lmn_fatal("cannot put ULONG_MAX");
#endif
    this->n++;
    this->expand(key);
    this->tbl[key / PROC_TBL_BUCKETS_SIZE][key % PROC_TBL_BUCKETS_SIZE] = value;
  }

  bool put_if_absent(Key key, Value value) {
    if (this->contains(key)) return false;
    this->put(key, value);
    return true;
  }

  void unput(Key key) {
#ifdef DEBUG
    if (!this->contains(key)) throw new std::logic_error("attempted to unput an absent key.");
#endif
    this->n--;
    this->tbl[key / PROC_TBL_BUCKETS_SIZE][key % PROC_TBL_BUCKETS_SIZE] = limit::max();
  }

  bool get(Key key, Value &value) {
    if (this->contains(key)) {
      value = (*this)[key];
      return true;
    } else {
      return false;
    }
  }

  void clear() {
    this->n = 0;
    for (int i = 0; i < this->num_buckets; i++) {
      memset(this->tbl[i], 0xff, sizeof(Value) * PROC_TBL_BUCKETS_SIZE);
    }
  }

  void foreach(int(*func)(Key key, Value val, LmnWord arg), LmnWord arg) {
    unsigned long n = 0;

    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i]) continue;
      for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && n < this->n; j++) {
        if (this->tbl[i][j] == limit::max()) continue;
        func(i * PROC_TBL_BUCKETS_SIZE + j, this->tbl[i][j], arg);
        n++;
      }
    }
  }

  iterator begin() {
    return iterator(*this);
  }

  iterator end() {
    return iterator(*this, 0, 0, std::numeric_limits<unsigned long>::max());
  }

private:
  void expand(unsigned long n) {
    unsigned int org_n = this->num_buckets;
    while (this->size <= n) this->size *= 2;
    this->num_buckets = this->size / PROC_TBL_BUCKETS_SIZE + 1;
    if (org_n < this->num_buckets) {
      this->tbl = LMN_REALLOC(Value *, this->tbl, this->num_buckets);
      memset(this->tbl + org_n, 0, sizeof(Value *) * (this->num_buckets - org_n));
    }

    unsigned int b = n / PROC_TBL_BUCKETS_SIZE;
    if (b < this->num_buckets && this->tbl[b]) return;
    this->tbl[b] = LMN_NALLOC(Value, PROC_TBL_BUCKETS_SIZE);
    memset(this->tbl[b], 0xffU, sizeof(Value) * PROC_TBL_BUCKETS_SIZE);
  }
};
}

#endif /* PROCESS_TABLE_HPP */