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

#include "lmntal.h"
#include "lmntal_thread.h"

typedef struct Node  Node;
typedef struct Queue Queue;
struct Node {
  LmnWord v;
  Node *next;
};

struct Queue {
  Node *head;
  Node *tail;
  BOOL  lock;
  unsigned long enq_num, deq_num;
  pthread_mutex_t enq_mtx, deq_mtx;
};

/* single dequeue(reader), single enqueue(writer) */
#define LMN_Q_SRSW 0
/* single dequeue(reader), multiple enqueue(writer) */
#define LMN_Q_SRMW 1
/* multiple dequeue(reader), single enqueue(writer) */
#define LMN_Q_MRSW 2
/* multiple dequeue(reader), multiple enqueue(writer) */
#define LMN_Q_MRMW 4

inline static unsigned long queue_entry_num(Queue *q) {
  return q->enq_num - q->deq_num;
}

Queue *new_queue(void);
Queue *make_parallel_queue(BOOL lock_type);
void q_free(Queue *q);
inline BOOL is_empty_queue(Queue *q);
inline void enqueue(Queue *q, LmnWord v);
inline LmnWord dequeue(Queue *q);

#endif
