/*
 * process_table.hpp
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

#ifndef PROCESS_TABLE_HPP
#define PROCESS_TABLE_HPP

#include <functional>
#include <iterator>
#include <limits>
#include <stdexcept>
#include <utility>

#include "lmntal.h"

namespace slim {
template <typename T> ProcessID process_id(T);
}

/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
template <typename T> class ProcessTable {
public:
  using key_type   = ProcessID;
  using value_type = T;

  const static key_type        not_found;
  static constexpr std::size_t buckets_size = 1 << 12; // heuristics

private:
  const static value_type unused;
  unsigned long           n;
  unsigned long           size;
  unsigned long           num_buckets;
  value_type            **tbl;

public:
  ProcessTable(unsigned long size = 64) {
    this->n           = 0;
    this->size        = size ? size : 1;
    this->num_buckets = size / buckets_size + 1;
    this->tbl         = LMN_CALLOC<value_type *>(this->num_buckets);
  }

  ~ProcessTable() {
    for (int i = 0; i < this->num_buckets; i++) {
      LMN_FREE(this->tbl[i]);
    }
    LMN_FREE(this->tbl);
  }

  bool operator==(ProcessTable<value_type> const *b) {
    if (this->n != b->n)
      return false;

    unsigned int a_checked = 0;

    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i] && !b->tbl[i])
        continue;

      for (int j = 0; j < buckets_size && a_checked < this->n; j++) {
        value_type va = (this->tbl[i]) ? this->tbl[i][j] : unused;
        value_type vb = (b->tbl[i]) ? b->tbl[i][j] : unused;
        if (va != vb)
          return false;
        if (va != unused)
          a_checked++;
      }
    }

    return true;
  }

  value_type &operator[](key_type key) {
    expand(key);
    return this->tbl[key / buckets_size][key % buckets_size];
  }

  template <typename U> bool contains(U key) const {
    auto const k = slim::process_id(key);
    return k < this->size && this->tbl[k / buckets_size] && this->tbl[k / buckets_size][k % buckets_size] != unused;
  }

  template <typename U> void put(U key_, value_type value) {
    auto const key = slim::process_id(key_);
#ifdef DEBUG
    if (value == unused)
      lmn_fatal("cannot put 'unused' value.");
#endif
    if (!this->contains(key)) {
      this->n++;
      this->expand(key);
    }
    this->tbl[key / buckets_size][key % buckets_size] = value;
  }

  /* テーブルにkeyを追加し, trueを返す. すでにpが存在した場合はfalseを返す. */
  template <typename U> bool put_if_absent(U key, value_type value) {
    if (this->contains(key))
      return false;
    this->put(key, value);
    return true;
  }

  template <typename U> void unput(U key) {
    auto const k = slim::process_id(key);
#ifdef DEBUG
    if (!this->contains(k))
      throw new std::logic_error("attempted to unput an absent key.");
#endif
    this->n--;
    this->tbl[k / buckets_size][k % buckets_size] = unused;
  }

  template <typename U> void erase(U key) { unput(key); }

  void clear() {
    this->n = 0;
    for (int i = 0; i < this->num_buckets; i++) {
      if (this->tbl[i])
        memset(this->tbl[i], 0xff, sizeof(value_type) * buckets_size);
    }
  }

  void foreach (std::function<int(LmnWord key, LmnWord val, LmnWord arg)> const&func, LmnWord arg) {
    unsigned long n = 0;

    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i])
        continue;
      for (int j = 0; j < buckets_size && n < this->n; j++) {
        if (this->tbl[i][j] == unused)
          continue;
        func(i * buckets_size + j, this->tbl[i][j], arg);
        n++;
      }
    }
  }

  class iterator : std::input_iterator_tag {
    ProcessTable<value_type> const *table;
    std::size_t                     bucket_idx, idx;
    std::pair<key_type, value_type> value;

  public:
    iterator(ProcessTable<value_type> const *table) : table(table), bucket_idx(not_found), idx(not_found) {}
    iterator(ProcessTable<value_type> const *table, std::size_t bucket_idx, std::size_t idx)
        : table(table), bucket_idx(bucket_idx), idx(idx),
          value(std::make_pair(bucket_idx * buckets_size + idx, table->tbl[bucket_idx][idx])){};

    std::pair<key_type, value_type> const &operator*() const { return value; }

    std::pair<key_type, value_type> const *operator->() const { return &value; }

    iterator &operator++() {
      for (; bucket_idx < table->num_buckets; bucket_idx++) {
        if (!table->tbl[bucket_idx])
          continue;
        for (idx++; idx < buckets_size; idx++) {
          if (table->tbl[bucket_idx][idx] == unused)
            continue;
          value = std::make_pair(bucket_idx * buckets_size + idx, table->tbl[bucket_idx][idx]);
          return *this;
        }
      }
      bucket_idx = not_found;
      idx        = not_found;
      return *this;
    }

    iterator operator++(int _) {
      iterator it(this->table, bucket_idx, idx);
      ++(*this);
      return it;
    }

    bool operator==(iterator const &it) const {
      return table == it.table && bucket_idx == it.bucket_idx && idx == it.idx;
    }
    bool operator!=(iterator const &it) const { return !(*this == it); }
  };
  friend iterator;

  iterator begin() {
    for (int i = 0; i < this->num_buckets; i++) {
      if (!this->tbl[i])
        continue;
      for (int j = 0; j < buckets_size; j++) {
        if (this->tbl[i][j] == unused)
          continue;
        return iterator(this, i, j);
      }
    }
    return end();
  }

  iterator end() const { return iterator(this); }

  template <typename U> iterator find(U key) const {
    auto const k = slim::process_id(key);
    return (k < this->size && this->tbl[k / buckets_size] && this->tbl[k / buckets_size][k % buckets_size] != unused)
               ? iterator(this, k / buckets_size, k % buckets_size)
               : end();
  }

private:
  void expand(unsigned long n) {
    unsigned int org_n = this->num_buckets;
    while (this->size <= n)
      this->size *= 2;
    this->num_buckets = this->size / buckets_size + 1;
    if (org_n < this->num_buckets) {
      this->tbl = LMN_REALLOC<value_type *>(this->tbl, this->num_buckets);
      memset(this->tbl + org_n, 0, sizeof(value_type *) * (this->num_buckets - org_n));
    }

    unsigned int b = n / buckets_size;
    if (b < this->num_buckets && this->tbl[b])
      return;
    this->tbl[b] = LMN_NALLOC<value_type>(buckets_size);
    for (int i = 0; i < buckets_size; i++)
      this->tbl[b][i] = unused;
  }
};

template <typename T> const LmnWord ProcessTable<T>::not_found = std::numeric_limits<LmnWord>::max();

#endif /* PROCESS_TABLE_HPP */
