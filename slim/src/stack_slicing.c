/*
 * stack_slicing.c
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
 *  Parallel Depth First Search using A Stack-Slicing Algorithm
 */

#include "stack_slicing.h"
#include "nd.h"
#include "mc.h"
#include "state.h"
#include "vector.h"
#include "error.h"
#include "lmntal_thread.h"
#include "runtime_status.h"

/** prototypes
 */
inline static LmnWord ss_work_stealing(LmnWorker *w);
inline static void    ss_handoff_all_task(LmnWorker *me, Vector *tasks);
inline static void    ss_handoff_task(LmnWorker *me,  LmnWord task);
       static void    ss_nd_loop(LmnWorker *w, Vector *stack, Vector *succ, struct ReactCxt *cxt);


/** global vars
 */
static unsigned int ss_cutoff_depth = 7;


void ss_set_cutoff_depth(unsigned int d)
{
  ss_cutoff_depth = d;
}

/** public functions
 */

void ss_set_funcs(LmnWorkerFuncs *f)
{
  f->start    = ss_start;
  f->init     = ss_worker_queue_init;
  f->finalize = ss_worker_queue_finalize;
}



void ss_worker_queue_init(LmnWorker *w)
{
  Queue *q;
  if (lmn_env.core_num == 1) {
    q = new_queue();
  } else if (lmn_env.optimize_loadbalancing) {
    q = make_parallel_queue(MRSW);
  } else {
    q = make_parallel_queue(SRSW);
  }
  w->wq = q;
}


void ss_worker_queue_finalize(LmnWorker *w)
{
  q_free(WORKER_QUEUE(w));
}

void ss_start() {
  struct ReactCxt cxt;
  Vector *my_stack;
  Vector *my_expand_trans;
  LmnWorker *w;
  State  *s;

  /** preliminary */
  s = NULL;
  w = workerpool_get_my_worker();
  if (WORKER_ID(w) == 0) {
    s = state_space_init_state(WORKER_STATESPACE(w));
  }

  nd_react_cxt_init(&cxt, DEFAULT_STATE_ID);
  my_stack        = vec_make(SS_STACK_SIZE);
  my_expand_trans = vec_make(64);

  /** start */
  while (!mc_data.mc_exit) {
    if (!s && is_empty_queue(WORKER_QUEUE(w))) {
      WORKER_UNSET_ACTIVE(w);
      if(p_representative_termination_detection(w)) {
        /* termination is detected! */
        break;
      }
      else if (lmn_env.optimize_loadbalancing){
        /* 職探しの旅 */
        s = (State *)ss_work_stealing(w);
      }
    }
    else {
      WORKER_SET_ACTIVE(w);
      if (s || (s = (State *)dequeue(WORKER_QUEUE(w)))) {
#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node));
          profile_start_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);
        }
#endif
        vec_push(my_stack, (LmnWord)s);
        ss_nd_loop(w, my_stack, my_expand_trans, &cxt);
        s = NULL;

#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_finish_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);
        }
#endif
        vec_clear(my_stack);
        vec_clear(my_expand_trans);
      }
    }
  }

  nd_react_cxt_destroy(&cxt);
  vec_free(my_stack);
  vec_free(my_expand_trans);
}

/** static functions
 */
inline static LmnWord ss_work_stealing(LmnWorker *w)
{
  LmnWorker *dst;
  dst = WORKER_NEXT_WORKER(w);

  while (w != dst) {
    if (WORKER_IS_ACTIVE(dst) && !is_empty_queue(WORKER_QUEUE(dst))) {
      WORKER_SET_ACTIVE(w);
      WORKER_SET_STOLEN(dst);
      return dequeue(WORKER_QUEUE(dst));
    }
    else {
      dst = WORKER_NEXT_WORKER(dst);
    }
  }
  return (LmnWord)NULL;
}

inline static void ss_handoff_all_task(LmnWorker *me, Vector *expands)
{
  unsigned long i, n;
  LmnWorker *rn = WORKER_NEXT_WORKER(me);
  if (WORKER_ID(me) > WORKER_ID(rn)) {
    WORKER_SET_BLACK(me);
  }

  n = vec_num(expands);
  for (i = 0; i < n; i++) {
    enqueue(WORKER_QUEUE(rn), vec_get(expands, i));
  }
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node) * n);
  }
#endif
}

inline static void ss_handoff_task(LmnWorker *me, LmnWord task)
{
  LmnWorker *rn = WORKER_NEXT_WORKER(me);
  if (WORKER_ID(me) > WORKER_ID(rn)) {
    WORKER_SET_BLACK(me);
  }
  enqueue(WORKER_QUEUE(rn), task);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node));
  }
#endif
}

/** 状態通信の条件式 */
#define SS_LOAD_BALANCING(Stack, W, I, N)    \
 (vec_num(Stack) >= CUTOFF_DEPTH) ||         \
 (lmn_env.optimize_loadbalancing             \
     && ((I + 1) < (N))                      \
     && !WORKER_IS_ACTIVE(WORKER_NEXT_WORKER(W)))


static void ss_nd_loop(LmnWorker       *w,
                       Vector          *stack,
                       Vector          *new_states,
                       struct ReactCxt *cxt) {
  State *s;
  AutomataState prop_atm_s;
  unsigned int i, n;
  BOOL flags = WORKER_FLAGS(w);

  while (vec_num(stack) != 0) {
    /** 展開元の状態の取得 */
    s = (State *)vec_peek(stack);
    if (mc_data.mc_exit) break;
    prop_atm_s = !has_property(flags) ? DEFAULT_PROP_AUTOMATA
                                      : automata_get_state(mc_data.property_automata,
                                                           state_property_state(s));
    if (is_expanded(s)) {
      vec_pop(stack);
      continue;
    }
    else if (mc_data.do_search && atmstate_is_end(prop_atm_s)) {
      mc_violate(WORKER_STATESPACE(w), s, NULL, flags);
      vec_pop(stack);
      continue;
    }

    /* サクセッサを展開 */
    expand(WORKER_STATESPACE(w), s, prop_atm_s, cxt, new_states, flags);

    n = vec_num(new_states);
    if (vec_num(stack) >= CUTOFF_DEPTH) {
      ss_handoff_all_task(w, new_states);
    }
    else {
      for (i = 0; i < n; i++) {
        State *new_s = (State *)vec_get(new_states, i);

        if (SS_LOAD_BALANCING(stack, w, i, n)) {
          ss_handoff_task(w, (LmnWord)new_s);
        } else {
          vec_push(stack, (vec_data_t)new_s);
        }
      }
    }
    vec_clear(new_states);
  }
}
