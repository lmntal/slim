/*
 * parallel.h
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
 *  utility for Parallel Execution
 */

#ifndef LMN_PARALLEL_H
#define LMN_PARALLEL_H

#include "lmntal.h"
#include "statespace.h"
#include "nd.h"
#include "state.h"
#include "queue.h"
#include "lmntal_thread.h"
#include <signal.h>

typedef struct LmnWorker      LmnWorker;
typedef struct LmnWorkerFuncs LmnWorkerFuncs;

struct LmnWorkerFuncs {
  void (*start)( );
  void (*init)( );
  void (*finalize)( );
};

struct LmnWorker {
  lmn_thread_t pth;        /* pthread_id */
  unsigned long id;        /* Natural integer id (lmn_thread_id) */
  BOOL flags;              /* for flags of non-deterministic execution */
  BOOL attr;               /* Attribute 1     for Termination Detection */
  BOOL attr2;              /* Attribute 2     for Termination Detection */
  BOOL attr3;              /* Attribute 3     for Termination Detection */
  BOOL end;
  Queue *wq;               /* Work Queue */
  LmnWorkerFuncs funcs;    /* Abstract Methods */
  StateSpace states;       /* Pointer to StateSpace */
  LmnWorker *next;         /* Pointer to Neighbor Worker */
};

/** Macros
 */
#define WORKER_ID(W)               ((W)->id)
#define WORKER_PID(W)              ((W)->pth)
#define WORKER_FLAGS(W)            ((W)->flags)
#define WORKER_STATESPACE(W)       ((W)->states)
#define WORKER_NEXT_WORKER(W)      ((W)->next)

/* MTでbit fieldは使えないのでなんとなく用意したマスク */
#define WORKER_ACTIVE_MASK         (0x01U)
#define WORKER_WHITE_MASK          (0x01U)
#define WORKER_STOLEN_MASK         (0x01U)
#define WORKER_END_MASK            (0x01U)

#define WORKER_SET_FLAGS(W, F)     ((W)->flags |= (F))
#define WORKER_UNSET_ACTIVE(W)     ((W)->attr  &= (~WORKER_ACTIVE_MASK))
#define WORKER_SET_ACTIVE(W)       ((W)->attr  |= WORKER_ACTIVE_MASK)
#define WORKER_IS_ACTIVE(W)        ((W)->attr  &  WORKER_ACTIVE_MASK)
#define WORKER_SET_BLACK(W)        ((W)->attr2 &= (~WORKER_WHITE_MASK))
#define WORKER_SET_WHITE(W)        ((W)->attr2 |= WORKER_WHITE_MASK)
#define WORKER_IS_WHITE(W)         ((W)->attr2 &  WORKER_WHITE_MASK)
#define WORKER_SET_STOLEN(W)       ((W)->attr3 |= WORKER_STOLEN_MASK)
#define WORKER_UNSET_STOLEN(W)     ((W)->attr3 &= (~WORKER_STOLEN_MASK))
#define WORKER_IS_STOLEN(W)        ((W)->attr3 &  WORKER_STOLEN_MASK)
#define WORKER_SET_END(W)          ((W)->end |= WORKER_END_MASK)
#define WORKER_UNSET_END(W)        ((W)->end &= (~WORKER_END_MASK))
#define WORKER_IS_END(W)           ((W)->end &  WORKER_END_MASK)
#define WORKER_START(W)            ((*(W)->funcs.start)())
#define WORKER_INIT(W)             ((*(W)->funcs.init)(W))
#define WORKER_FINALIZE(W)         ((*(W)->funcs.init)(W))
#define WORKER_QUEUE(W)            ((W)->wq)

/** ProtoTypes
 */
StateSpace do_nd_parallel(LmnMembrane *mem, BOOL flags);
BOOL p_representative_termination_detection(LmnWorker *root);
BOOL p_simple_termination_detection(void);
void p_sigset_handler(struct sigaction *s, int sig, void (*handler)( ));
inline LmnWorker *new_worker_minimal(void);
LmnWorker *new_worker(StateSpace     ss,
                      unsigned long  id,
                      LmnWorkerFuncs *f,
                      BOOL           flags);
void worker_free(LmnWorker *w);
void worker_start(void *arg);
LmnWorker *workerpool_get_my_worker(void);
inline LmnWorker *workerpool_get_worker(unsigned long id);

#endif
