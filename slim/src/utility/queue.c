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

#define Q_DEQ 0
#define Q_ENQ 1

inline static Node *node_make(LmnWord v);
inline static void node_free(Node *node);
inline static void q_lock(Queue *q, BOOL rw);
inline static void q_unlock(Queue *q, BOOL rw);


Queue *make_parallel_queue(BOOL lock_type)
{
  Queue *q = new_queue();

  switch(lock_type) {
  case LMN_Q_SRSW:
    break;
  case LMN_Q_MRMW:
    lmn_mutex_init(&(q->deq_mtx));
    /* fall through */
  case LMN_Q_SRMW:
    lmn_mutex_init(&(q->enq_mtx));
    break;
  case LMN_Q_MRSW:
    lmn_mutex_init(&(q->deq_mtx));
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }
  q->lock = lock_type;
  return q;
}

/*  {head, tail}
 *       ↓
 *       ○ → NULL
 *    sentinel
 */
Queue *new_queue(void)
{
  Queue *q = LMN_MALLOC(Queue);
  Node *sentinel = node_make(0);
  q->head     = sentinel;
  q->tail     = sentinel;
  q->lock     = FALSE;
  q->enq_num  = 0UL;
  q->deq_num  = 0UL;
  return q;
}

void q_free(Queue *q)
{
  Node *n, *m;
  for (n = q->head; n; n = m) {
    m = n->next;
    node_free(n);
  }

  switch(q->lock) {
  case LMN_Q_MRMW:
    lmn_mutex_destroy(&(q->deq_mtx));
    /* FALL THROUGH */
  case LMN_Q_SRMW:
    lmn_mutex_destroy(&(q->enq_mtx));
    break;
  case LMN_Q_MRSW:
    lmn_mutex_destroy(&(q->deq_mtx));
    break;
  default:
    /* nothing to do */
    break;
  }
  LMN_FREE(q);
}

/*{tail, last}
 *     ↓
 *   ..○→NULL
 */

void enqueue(Queue *q, LmnWord v)
{
  Node *last, *new;
  if (q->lock) q_lock(q, Q_ENQ);
  last = q->tail;
  new = node_make(v);
  last->next = new;
  q->tail = new;
  q->enq_num++;

  if (q->lock) q_unlock(q, Q_ENQ);
}

/* Queueから要素をdequeueする.
 * Queueが空の場合, 0を返す */
LmnWord dequeue(Queue *q)
{
  LmnWord ret = 0;
  if (q->lock) q_lock(q, Q_DEQ);
  if (!is_empty_queue(q)) {
    Node *sentinel, *next;
    sentinel = q->head;
    next = sentinel->next;
    if (next) {
      ret  = next->v;
      next->v = 0;
      q->head = next;
      free(sentinel);
      q->deq_num++;
    }
  }
  if (q->lock) q_unlock(q, Q_DEQ);
  return ret;
}

/* キューqが空なら真を返す.*/
inline BOOL is_empty_queue(Queue *q)
{
  return (q->head == q->tail) && (q->enq_num == q->deq_num);
}


/** ----
 *  static functions
 */

inline static Node *node_make(LmnWord v)
{
  Node *n = LMN_MALLOC(Node);
  n->v = v;
  n->next = NULL;
  return n;
}

inline static void node_free(Node *node)
{
  LMN_FREE(node);
}

inline static void q_lock(Queue *q, BOOL is_enq)
{
  if (is_enq) { /* for enqueue */
    switch(q->lock) {
    case LMN_Q_MRMW:
    case LMN_Q_SRMW:
      lmn_mutex_lock(&(q->enq_mtx));
      break;
    default:
      break;
    }
  }
  else {       /* for dequeue */
    switch(q->lock) {
    case LMN_Q_MRMW:
    case LMN_Q_MRSW:
      lmn_mutex_lock(&(q->deq_mtx));
      break;
    default:
      break;
    }
  }
}

inline static void q_unlock(Queue *q, BOOL is_enq)
{
  if (is_enq) { /* for enqueue */
    switch(q->lock) {
    case LMN_Q_MRMW:
    case LMN_Q_SRMW:
      lmn_mutex_unlock(&(q->enq_mtx));
      break;
    default:
      break;
    }
  }
  else {       /* for dequeue */
    switch(q->lock) {
    case LMN_Q_MRMW:
    case LMN_Q_MRSW:
      lmn_mutex_unlock(&(q->deq_mtx));
      break;
    default:
      break;
    }
  }
}
