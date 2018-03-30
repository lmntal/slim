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


#include <limits>
#include <stdexcept>
#include <iterator>
#include <utility>

extern "C" {
#include "lmntal.h"
}

extern "C++" {
/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
template<typename T> struct ProcessTable {
  using key_type = LmnWord;
  using value_type = T;

  static const key_type not_found = std::numeric_limits<key_type>::max();
  static const value_type unused = std::numeric_limits<value_type>::max();
  static const std::size_t buckets_size = 1 << 12; // heuristics

  unsigned long n;
  unsigned long size;
  unsigned long num_buckets;
  value_type **tbl;

  ProcessTable(unsigned long size) {
    this->n    = 0;
    this->size = size;
    this->num_buckets = size / buckets_size + 1;
    this->tbl = LMN_CALLOC(value_type *, this->num_buckets);
  }

  ProcessTable() {
    ProcessTable(0);
  }

  ~ProcessTable() {
    for (int i = 0; i < this->num_buckets; i++) {
      LMN_FREE(this->tbl[i]);
    }
    LMN_FREE(this->tbl);
  }

  bool operator==(const ProcessTable<value_type> *b) {
    if (this->n != b->n) return false;
    
    unsigned int a_checked = 0;

    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i] && !b->tbl[i]) continue;

      for (int j = 0; j < buckets_size && a_checked < this->n; j++) {
        value_type va = (this->tbl[i]) ? this->tbl[i][j] : unused;
        value_type vb = (b->tbl[i]) ? b->tbl[i][j] : unused;
        if (va != vb) return false;
        if (va != unused) a_checked++;
      }
    }

    return true;
  }

  const value_type &operator[](key_type key) const {
    if (!this->contains(key)) throw new std::out_of_range("accessed with invalid key");
    return this->tbl[key / buckets_size][key % buckets_size];
  }

  bool contains(key_type key) const {
    return key < this->size && this->tbl[key / buckets_size] &&
      this->tbl[key / buckets_size][key % buckets_size] != unused;
  }

  void put(key_type key, value_type value) {
#ifdef DEBUG
    if (value == unused) lmn_fatal("cannot put 'unused' value.");
#endif
    if (!this->contains(key)) {
      this->n++;
      this->expand(key);
    }
    this->tbl[key / buckets_size][key % buckets_size] = value;
  }

  bool put_if_absent(key_type key, value_type value) {
    if (this->contains(key)) return false;
    this->put(key, value);
    return true;
  }

  void unput(key_type key) {
#ifdef DEBUG
    if (!this->contains(key)) throw new std::logic_error("attempted to unput an absent key.");
#endif
    this->n--;
    this->tbl[key / buckets_size][key % buckets_size] = unused;
  }

  bool get(key_type key, value_type &value) {
    if (this->contains(key)) {
      value = (*this)[key];
      return true;
    } else {
      value = unused;
      return false;
    }
  }

  void clear() {
    this->n = 0;
    for (int i = 0; i < this->num_buckets; i++) {
      if (this->tbl[i])
        memset(this->tbl[i], 0xff, sizeof(value_type) * buckets_size);
    }
  }

  void foreach(int(*func)(key_type key, value_type val, LmnWord arg), LmnWord arg) {
    unsigned long n = 0;

    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i]) continue;
      for (int j = 0; j < buckets_size && n < this->n; j++) {
        if (this->tbl[i][j] == not_found) continue;
        func(i * buckets_size + j, this->tbl[i][j], arg);
        n++;
      }
    }
  }

private:
  void expand(unsigned long n) {
    unsigned int org_n = this->num_buckets;
    while (this->size <= n) this->size *= 2;
    this->num_buckets = this->size / buckets_size + 1;
    if (org_n < this->num_buckets) {
      this->tbl = LMN_REALLOC(value_type *, this->tbl, this->num_buckets);
      memset(this->tbl + org_n, 0, sizeof(value_type *) * (this->num_buckets - org_n));
    }

    unsigned int b = n / buckets_size;
    if (b < this->num_buckets && this->tbl[b]) return;
    this->tbl[b] = LMN_NALLOC(value_type, buckets_size);
    memset(this->tbl[b], 0xff, sizeof(value_type) * buckets_size);
  }
};
}

#endif /* PROCESS_TABLE_HPP */