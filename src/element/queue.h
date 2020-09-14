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

#ifndef LMN_QUEUE_H
#define LMN_QUEUE_H

/**
 * @ingroup Element
 * @defgroup Queue
 * @{
 */

#include "../lmntal.h"
#include "error.h"
#include "lmntal_thread.h"

#include <mutex>

namespace slim {
namespace element {

// ビット演算で実装したほうが速くなるかも？
enum class concurrent_mode {
  single_reader_single_writer,
  single_reader_multi_writer,
  multi_reader_single_writer,
  multi_reader_multi_writer,
};

template <class T> class concurrent_queue {
public:
  using value_type = T;

  /*  {head, tail}
   *       ↓
   *       ○ → NULL
   *    sentinel
   */
  concurrent_queue(concurrent_mode lock_type =
                       concurrent_mode::single_reader_single_writer) {
    Node *sentinel = new Node(0);
    this->head = sentinel;
    this->tail = sentinel;
    this->enq_num = 0UL;
    this->deq_num = 0UL;
    this->qlock = lock_type;
  }
  ~concurrent_queue() {
    Node *n, *m;
    for (n = this->head; n; n = m) {
      m = n->next;
      delete n;
    }
  }
  /*{tail, last}
   *     ↓
   *   ..○→NULL
   */
  void enqueue(value_type v) {
    auto scoped_enq_lock = lock_writer_if_needed();
    auto last = this->tail;
    auto node = new Node(v);
    last->next = node;
    this->tail = node;
    this->enq_num++;
  }

  /* Queueから要素をdequeueする.
   * Queueが空の場合, 0を返す */
  value_type dequeue() {
    auto scoped_deq_lock = lock_reader_if_needed();
    value_type ret = value_type();

    if (!this->is_empty()) {
      auto sentinel = this->head;
      auto next = sentinel->next;
      if (next) {
        ret = next->v;
        next->v = 0;
        this->head = next;
        free(sentinel);
        this->deq_num++;
      }
    }

    return ret;
  }
  /* キューqが空なら真を返す.*/
  bool is_empty() const {
    return (this->head == this->tail) && (this->enq_num == this->deq_num);
  }

  void clear() {
    while (this->dequeue())
      ;
  }
  unsigned long entry_num() const { return this->enq_num - this->deq_num; }

  struct Node {
    value_type v;
    Node *next;
    Node(value_type v) {
      this->v = v;
      this->next = NULL;
    }
  };
  Node *head;

private:

  Node *tail;
  concurrent_mode qlock;
  std::mutex enq_mtx, deq_mtx;
  unsigned long enq_num, deq_num;

  std::unique_lock<std::mutex> lock_reader_if_needed() {
    return this->should_lock_reader() ? std::unique_lock<std::mutex>(deq_mtx)
                                      : std::unique_lock<std::mutex>();
  }
  std::unique_lock<std::mutex> lock_writer_if_needed() {
    return this->should_lock_writer() ? std::unique_lock<std::mutex>(enq_mtx)
                                      : std::unique_lock<std::mutex>();
  }

  bool should_lock_reader() const {
    return this->qlock == concurrent_mode::multi_reader_multi_writer ||
           this->qlock == concurrent_mode::multi_reader_single_writer;
  }

  bool should_lock_writer() const {
    return this->qlock == concurrent_mode::multi_reader_multi_writer ||
           this->qlock == concurrent_mode::single_reader_multi_writer;
  }
};

/** ==========
 *  DeQue (KaWaBaTa code)
 */

#define DEQ_DEC(X, C) (X = X != 0 ? X - 1 : C - 1)
#define DEQ_INC(X, C) (X = X != C - 1 ? X + 1 : 0)

template <class T> struct deque {
  using value_type = T;
  value_type *tbl;
  unsigned int head, tail, cap;
  deque(unsigned int init_size) {
    LMN_ASSERT(init_size > 0);
    this->init(init_size);
  }

  ~deque() { this->destroy(); }
  void init(unsigned int init_size) {
    this->tbl = LMN_NALLOC(value_type, init_size);
    this->head = 0;
    this->tail = 1;
    this->cap = init_size;
  }
  int num() {
    return this->tail > this->head ? this->tail - this->head - 1
                                   : this->cap - this->head + this->tail - 1;
  }
  BOOL is_empty() { return (this->num() == 0); }
  void extend() {
    unsigned int old = this->cap;
    this->cap *= 2;
    this->tbl = LMN_REALLOC(value_type, this->tbl, this->cap);
    if (this->tail <= this->head) {
      unsigned int i;
      for (i = 0; i < this->tail; i++) {
        this->tbl[i + old] = this->tbl[i];
      }
      this->tail = old + this->tail;
    }
  }
  void push_head(value_type keyp) {
    if (this->num() == this->cap - 1) {
      this->extend();
    }
    (this->tbl)[this->head] = keyp;
    DEQ_DEC(this->head, this->cap);
  }
  void push_tail(value_type keyp) {
    if (this->num() == this->cap - 1) {
      this->extend();
    }
    (this->tbl)[this->tail] = keyp;
    DEQ_INC(this->tail, this->cap);
  }
  value_type pop_head() {
    value_type ret;
    LMN_ASSERT(this->num() > 0);

    DEQ_INC(this->head, this->cap);
    ret = this->tbl[this->head];
    return ret;
  }
  value_type pop_tail() {
    LMN_ASSERT(this->num() > 0);
    DEQ_DEC(this->tail, this->cap);
    return this->tbl[this->tail];
  }
  value_type peek_head() const {
    unsigned int x = this->head;
    return this->tbl[DEQ_INC(x, this->cap)];
  }
  value_type peek_tail() const {
    unsigned int x = this->tail;
    return this->tbl[DEQ_DEC(x, this->cap)];
  }
  value_type get(unsigned int i) const { return this->tbl[i]; }
  void clear() {
    this->head = 0;
    this->tail = 1;
  }
  void destroy() { LMN_FREE(this->tbl); }
  unsigned long space() { return sizeof(struct deque) + this->space_inner(); }
  unsigned long space_inner() { return this->cap * sizeof(value_type); }
  void print() {
    unsigned int i;
    FILE *f = stdout;
    fprintf(f, "cap=%u, head=%u, tail=%u, num=%u\n[", this->cap, this->head,
            this->tail, this->num());
    for (i = 0; i < this->cap; i++)
      fprintf(f, "%lu, ", this->tbl[i]);
    fprintf(f, "]\n");
  }
  BOOL contains(value_type keyp) const {
    unsigned int i = this->tail;
    while (i != this->head) {
      DEQ_DEC(i, this->cap);
      if (this->get(i) == (value_type)keyp) {
        return TRUE;
      }
    }
    return FALSE;
  }
  deque *copy() {
    unsigned int i;
    deque *new_deq;

    i = this->tail;
    new_deq = new deque(this->num() > 0 ? this->num() : 1);

    while (i != this->head) {
      DEQ_DEC(i, this->cap);
      new_deq->tbl[i] = this->get(i);
    }

    new_deq->head = this->head;
    new_deq->head = this->tail;
    return new_deq;
  }
};
} // namespace element
} // namespace slim

using Queue = slim::element::concurrent_queue<LmnWord>;
using Deque = slim::element::deque<LmnWord>;

/* @} */

#endif
