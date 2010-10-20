/*
 * work_pool.c
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
 *  Parallel Breadth First Search
 */

#include "work_pool.h"
#include "lmntal_thread.h"
#include "nd.h"
#include "mc.h"
#include "state.h"
#include "queue.h"
#include "vector.h"
#include "runtime_status.h"

static Queue *wp_queue;
static void wp_nd(LmnWorker *w, State *s, Vector *succ, struct ReactCxt *cxt);

void wp_set_funcs(LmnWorkerFuncs *f)
{
  f->start    = wp_start;
  f->init     = wp_worker_queue_init;
  f->finalize = wp_worker_queue_finalize;
}


#define WP_PRIMARY 0
void wp_worker_queue_init(LmnWorker *w)
{
  if (WORKER_ID(w) == WP_PRIMARY) {
    if (lmn_env.core_num > 1) {
      wp_queue = make_parallel_queue(MRMW);
    } else {
      wp_queue = new_queue();
    }
  }

  w->wq = wp_queue;
}

void wp_worker_queue_finalize(LmnWorker *w)
{
  if (WORKER_ID(w) == WP_PRIMARY) {
    q_free(wp_queue);
  }
}

void wp_start() {
  struct ReactCxt cxt;
  State *s;
  LmnWorker *w;
  Vector *my_expand_trans;

  s  = NULL;
  w = workerpool_get_my_worker();
  if (WORKER_ID(w) == 0) {
    s = state_space_init_state(WORKER_STATESPACE(w));
  }

  nd_react_cxt_init(&cxt, DEFAULT_STATE_ID);
  my_expand_trans = vec_make(64);

  /* start bfs  */
  while (!mc_data.mc_exit) {
    if (!s && is_empty_queue(wp_queue)) {
      WORKER_UNSET_ACTIVE(w);
      if (p_representative_termination_detection(w)) {
        /* termination is detected! */
        break;
      }
    }
    else {
      WORKER_SET_ACTIVE(w);
      if (s || (s = (State *)dequeue(wp_queue))) {
#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node));
          profile_start_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);
        }
        wp_nd(w, s, my_expand_trans, &cxt);
        if (lmn_env.profile_level >= 3) {
          profile_finish_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);
        }
#else
        wp_nd(w, s, my_expand_trans, &cxt);
#endif
        s = NULL;
        vec_clear(my_expand_trans);
      }
    }
  }

  nd_react_cxt_destroy(&cxt);
  vec_free(my_expand_trans);
}


static void wp_nd(LmnWorker *w, State *s, Vector *new_states, struct ReactCxt *cxt) {
  unsigned int i, n;
  AutomataState prop_atm_s;
  BOOL flags = WORKER_FLAGS(w);

  prop_atm_s = !has_property(flags) ? DEFAULT_PROP_AUTOMATA
                                    : automata_get_state(mc_data.property_automata, state_property_state(s));

  if (is_expanded(s)) {
    return;
  }
  else if (mc_data.do_search && atmstate_is_end(prop_atm_s)) {
    mc_violate(WORKER_STATESPACE(w), s, NULL, flags);
    return;
  }

  /* サクセッサを展開 */
  expand(WORKER_STATESPACE(w), s, prop_atm_s, cxt, new_states, flags);
  n = vec_num(new_states);

  if (n > 0) WORKER_SET_BLACK(w);
  for (i = 0; i < n; i++) {
    State *open_s = (State *)vec_get(new_states, i);
    enqueue(wp_queue, (LmnWord)open_s);
  }
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node) * n);
  }
#endif
  vec_clear(new_states);
}

