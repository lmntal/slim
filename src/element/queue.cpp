/*
 * queue.c
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

#include "queue.h"
#include "error.h"
#include "lmntal.h"
#include <cerrno>

/**=====================
 *  DeQue
 */

/*init*/
void Deque::init(unsigned int init_size) {
  this->tbl  = LMN_NALLOC<LmnWord>(init_size);
  this->head = 0;
  this->tail = 1;
  this->cap  = init_size;
}

Deque::Deque(unsigned int init_size) {
  LMN_ASSERT(init_size > 0);
  this->init(init_size);
}

/* destroy */
void Deque::destroy() { LMN_FREE(this->tbl); }

/* free */
Deque::~Deque() { this->destroy(); }

/* num */
int Deque::num() {
  return this->tail > this->head ? this->tail - this->head - 1 : this->cap - this->head + this->tail - 1;
}

/* is_empty */
BOOL Deque::is_empty() { return (this->num() == 0); }

/* extend (static) */
void Deque::extend() {
  unsigned int old = this->cap;
  this->cap        *= 2;
  this->tbl        = LMN_REALLOC<LmnWord>(this->tbl, this->cap);
  if (this->tail <= this->head) {
    unsigned int i;
    for (i = 0; i < this->tail; i++) {
      this->tbl[i + old] = this->tbl[i];
    }
    this->tail = old + this->tail;
  }
}

/* push */
void Deque::push_head(LmnWord keyp) {
  if (this->num() == this->cap - 1) {
    this->extend();
  }
  (this->tbl)[this->head] = keyp;
  DEQ_DEC(this->head, this->cap);
}

/*  */
void Deque::push_tail(LmnWord keyp) {
  if (this->num() == this->cap - 1) {
    this->extend();
  }
  (this->tbl)[this->tail] = keyp;
  DEQ_INC(this->tail, this->cap);
}

/* pop */
LmnWord Deque::pop_head() {
  LmnWord ret;
  LMN_ASSERT(this->num() > 0);

  DEQ_INC(this->head, this->cap);
  ret = this->tbl[this->head];
  return ret;
}

/* */
LmnWord Deque::pop_tail() {
  LMN_ASSERT(this->num() > 0);
  DEQ_DEC(this->tail, this->cap);
  return this->tbl[this->tail];
}

/* peek */
LmnWord Deque::peek_head() const {
  unsigned int x = this->head;
  return this->tbl[DEQ_INC(x, this->cap)];
}

/* */
LmnWord Deque::peek_tail() const {
  unsigned int x = this->tail;
  return this->tbl[DEQ_DEC(x, this->cap)];
}

/* peek (no assertion) */
LmnWord Deque::get(unsigned int i) const { return this->tbl[i]; }

/* pop all elements from deq */
void Deque::clear() {
  this->head = 0;
  this->tail = 1;
}

unsigned long Deque::space_inner() { return this->cap * sizeof(deq_data_t); }

unsigned long Deque::space() { return sizeof(struct Deque) + this->space_inner(); }

void Deque::print() {
  unsigned int i;
  FILE        *f = stdout;
  fprintf(f, "cap=%u, head=%u, tail=%u, num=%u\n[", this->cap, this->head, this->tail, this->num());
  for (i = 0; i < this->cap; i++)
    fprintf(f, "%lu, ", this->tbl[i]);
  fprintf(f, "]\n");
}

/* contains */
BOOL Deque::contains(LmnWord keyp) const {
  unsigned int i = this->tail;
  while (i != this->head) {
    DEQ_DEC(i, this->cap);
    if (this->get(i) == (LmnWord)keyp) {
      return TRUE;
    }
  }
  return FALSE;
}

Deque *Deque::copy() {
  unsigned int i;
  Deque       *new_deq;

  i       = this->tail;
  new_deq = new Deque(this->num() > 0 ? this->num() : 1);

  while (i != this->head) {
    DEQ_DEC(i, this->cap);
    new_deq->tbl[i] = this->get(i);
  }

  new_deq->head = this->head;
  new_deq->head = this->tail;
  return new_deq;
}
