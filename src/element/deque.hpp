/*
 * queue.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
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
 *  library for queue / parallel queue
 */

#pragma once
#ifndef LMN_QUEUE_H
#define LMN_QUEUE_H

/**
 * @ingroup Element
 * @defgroup Queue
 * @{
 */

#include "fmt/core.h"

#include "lmntal.h"
#include "lmntal_thread.h"

struct Node {
  LmnWord v;
  Node   *next;
  Node(LmnWord v);
  ~Node();
};

/** ==========
 *  DeQue (KaWaBaTa code)
 */

using deq_data_t = LmnWord;

constexpr auto DEQ_DEC(unsigned int X, unsigned int C) -> unsigned int {
  if (X != 0) {
    return X = X - 1;
  }
  return X = C - 1;
}
constexpr auto DEQ_INC(unsigned int X, unsigned int C) -> unsigned int {
  if (X != C - 1) {
    return X = X + 1;
  }
  return X = 0;
}

struct Deque {
  LmnWord     *tbl;
  unsigned int head{0}, tail{1}, cap;

  Deque(unsigned int init_size) : cap(init_size), tbl(LMN_NALLOC<LmnWord>(init_size)) {}
  ~Deque() { LMN_FREE(this->tbl); }

  // copy constructor
  Deque(const Deque &d) : cap(d.cap), head(d.head), tail(d.tail) {
    this->tbl = LMN_NALLOC<LmnWord>(this->cap);
    auto i    = tail;
    while (i != this->head) {
      DEQ_DEC(i, cap);
      tbl[i] = at(i);
    }
  }

  // move constructor
  Deque(Deque &&d) = delete;

  size_t size() const {
    return this->tail > this->head ? this->tail - this->head - 1 : this->cap - this->head + this->tail - 1;
  }
  bool empty() const { return size() == 0; }

  void extend() {
    unsigned int old  = this->cap;
    this->cap        *= 2;
    this->tbl         = LMN_REALLOC<LmnWord>(this->tbl, this->cap);
    if (this->tail <= this->head) {
      unsigned int i;
      for (i = 0; i < this->tail; i++) {
        this->tbl[i + old] = this->tbl[i];
      }
      this->tail = old + this->tail;
    }
  }

  void push_head(LmnWord keyp) {
    if (this->size() == this->cap - 1) {
      this->extend();
    }
    (this->tbl)[this->head] = keyp;
    DEQ_DEC(this->head, this->cap);
  }

  void push_tail(LmnWord keyp) {
    if (this->size() == this->cap - 1) {
      this->extend();
    }
    (this->tbl)[this->tail] = keyp;
    DEQ_INC(this->tail, this->cap);
  }

  LmnWord pop_head() const {
    LmnWord ret;

    DEQ_INC(this->head, this->cap);
    ret = this->tbl[this->head];
    return ret;
  }

  LmnWord pop_tail() const {
    DEQ_DEC(this->tail, this->cap);
    return this->tbl[this->tail];
  }

  LmnWord peek_head() const {
    unsigned int x = this->head;
    return this->tbl[DEQ_INC(x, this->cap)];
  }

  LmnWord peek_tail() const {
    unsigned int x = this->tail;
    return this->tbl[DEQ_DEC(x, this->cap)];
  }

  LmnWord at(unsigned int i) const { return this->tbl[i]; }

  void clear() {
    this->head = 0;
    this->tail = 1;
  }

  unsigned long space() const { return sizeof(struct Deque) + this->space_inner(); }
  unsigned long space_inner() const { return this->cap * sizeof(deq_data_t); }

  void print() const {
    unsigned int i;
    FILE        *f = stdout;
    fprintf(f, "cap=%u, head=%u, tail=%u, num=%zu\n[", this->cap, this->head, this->tail, this->size());
    for (i = 0; i < this->cap; i++)
      fprintf(f, "%lu, ", this->tbl[i]);
    fprintf(f, "]\n");
  }

  bool contains(LmnWord keyp) const {
    unsigned int i = this->tail;
    while (i != this->head) {
      DEQ_DEC(i, this->cap);
      if (this->at(i) == (LmnWord)keyp) {
        return true;
      }
    }
    return false;
  }
};
/* @} */

template <> class fmt::formatter<Deque> {
public:
  // format a value using stored specification:
  template <typename FmtContext> constexpr auto format(Deque const &p, FmtContext &ctx) const {
    format_to(ctx.out(), "Deque(cap={}, head={}, tail={}, num={})", p.cap, p.head, p.tail, p.size());
    for (auto i = 0; i < p.cap; i++) {
      format_to(ctx.out(), "{}, ", p.tbl[i]);
    }
    format_to(ctx.out(), "]");
  }
};

#endif
