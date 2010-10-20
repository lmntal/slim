/*
 * parallel.c
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

#include "mc.h"
#include "state.h"
#include "statespace.h"
#include "stack_slicing.h"
#include "work_pool.h"
#include "parallel.h"
#include "error.h"
#include "runtime_status.h"

/** prototypes
 */
static void init_parallel_env(StateSpace states, BOOL flags);
static void finalize_parallel_env(void);
static void worker_set_tls_id(unsigned long id);
static void workerpool_make(StateSpace     states,
                            LmnWorkerFuncs *f,
                            BOOL           flags);
static void workerpool_free(void);
static void workerpool_set_ring(void);

static LmnWorker **p_workers;  /* ワーカープール */
static BOOL      p_terminated; /* 終了検知フラグ */

/** -----------------------------------------------
 *  Start / Finalize for parallel model checking
 */

StateSpace do_nd_parallel(LmnMembrane *world_mem_org, BOOL flags)
{
  unsigned long i, core_num;
  StateSpace states;
  State *init_state;
  BYTE prop_label;

  states     = state_space_make_for_parallel();
  init_parallel_env(states, flags);
  prop_label = !has_property(flags) ? DEFAULT_STATE_ID
                                    : automata_get_init_state(mc_data.property_automata);
  init_state = state_make(lmn_mem_copy(world_mem_org), prop_label, lmn_env.mem_enc && lmn_env.enable_compress_mem);
  state_space_set_init_state(states, init_state, use_compact(flags));

  /************** START: STATE SPACE SEARCH ***************/
  core_num = lmn_env.core_num;
  for (i = 1; i < core_num; i++) { /** start */
    lmn_thread_create(&WORKER_PID(p_workers[i]), worker_start, i);
  }
  if (lmn_env.profile_level >= 1) profile_start_exec();
  worker_start((void *)0UL);

  if (lmn_env.profile_level >= 1) profile_finish_exec();
  for (i = 1; i < core_num; i++) {
    lmn_thread_join(WORKER_PID(p_workers[i]));
  }
  /************** FINISH: STATE SPACE SEARCH **************/

  finalize_parallel_env();
  return states;
}

/* parallel workersの初期設定 */
static void init_parallel_env(StateSpace states, BOOL flags)
{
  LmnWorkerFuncs f;

  /* アルゴリズムの割り当て */
  if (lmn_env.bfs) {
    wp_set_funcs(&f);
  } else {
    ss_set_funcs(&f);
  }

  /* worker pool の構築 */
  p_terminated = FALSE;
  workerpool_make(states, &f, flags);
  workerpool_set_ring();
}

/* 後始末 */
static void finalize_parallel_env(void)
{
  workerpool_free();
  worker_set_tls_id(0);  /* primary_threadのTLS IDを元に戻す */
}



#define TERMINATION_CONDITION(W)  !WORKER_IS_ACTIVE(W) && \
                                   WORKER_IS_WHITE(W)  && \
                                  !WORKER_IS_STOLEN(W) && \
                                   is_empty_queue(WORKER_QUEUE(W))

/* TODO: work stealing時の不正終了問題 */
BOOL p_representative_termination_detection(LmnWorker *root)
{
  /** @see dijkstra's token termination detection (in parallel computing 2nd edition)
   *
   *  スレッドは, "white/black" "idle/active"を示すフラグを持つ.
   *  リングに沿ってトークンを回していく. 不正終了の恐れがある場合, blackとなっている.
   *  dynamic loadbalancerが存在する場合, そのまま導入しただけでは不正終了する.
   *  共有メモリ向けに処理を最適化している
   */
  if (WORKER_ID(root) == 0 && !p_terminated) {
    int i, n;
    BOOL ret;

    ret = TRUE;
    n   = lmn_env.core_num;
    for (i = 0; i < n; i++) {
      LmnWorker *w = p_workers[i];
      ret = ret && TERMINATION_CONDITION(w);
      WORKER_SET_WHITE(w); /* 検査したらwhiteに戻す */
      WORKER_UNSET_STOLEN(w);
      if (!ret) return FALSE;
    }
    p_terminated = ret && TERMINATION_CONDITION(root);
  }

  return p_terminated;
}

BOOL p_simple_termination_detection()
{
  unsigned int i;
  static unsigned int fail_num = 0;
  BOOL ret = TRUE;
  for (i = 0; i < lmn_env.core_num; i++) {
    ret = ret && WORKER_IS_END(p_workers[i]);
  }

  if (!ret) {
    fail_num++;
    if (fail_num > 1000) return TRUE;
  }
  return ret;
}




/** -------------------------------------
 *  LmnWorker
 */


inline LmnWorker *new_worker_minimal()
{
  LmnWorker *w      = LMN_MALLOC(LmnWorker);
  w->id             = 0;
  w->flags          = 0x00U;
  w->attr           = 0x00U;
  w->attr2          = 0x00U;
  w->attr3          = 0x00U;
  w->end            = 0x00U;
  w->states         = NULL;
  w->wq             = NULL;
  w->next           = NULL;
  w->funcs.start    = NULL;
  w->funcs.init     = NULL;
  w->funcs.finalize = NULL;
  return w;
}

LmnWorker *new_worker(StateSpace     ss,
                      unsigned long  id,
                      LmnWorkerFuncs *f,
                      BOOL           flags)
{
  LmnWorker *w = new_worker_minimal();
  w->states         = ss;
  w->id             = id;
  w->funcs.start    = f->start;
  w->funcs.init     = f->init;
  w->funcs.finalize = f->finalize;
  WORKER_SET_FLAGS(w, flags);
  WORKER_SET_ACTIVE(w);
  WORKER_SET_WHITE(w);
  return w;
}

void worker_free(LmnWorker *w)
{
  LMN_FREE(w);
}

/* 並列アルゴリズム実行前の初期設定を行うためのwrapper */
void worker_start(void *arg)
{
  LmnWorker *w;
  unsigned long id;

  id = (unsigned long)arg;
  worker_set_tls_id(id);
  thread_set_cpu_affinity(id);
  w  = workerpool_get_my_worker();

  if (lmn_env.profile_level >= 1) profile_start_exec_thread();
  WORKER_START(w);
  if (lmn_env.profile_level >= 1) profile_finish_exec_thread();
}

static void worker_set_tls_id(unsigned long id)
{
#ifdef ENABLE_PARALLEL
  lmn_thread_id = id;
  env_reset_proc_ids();
#endif
}


/** -----------------------------------------------------------
 *  Worker Pool
 */


/* 必ずp_set_worker_idを呼び出した後で使用すること */
LmnWorker *workerpool_get_my_worker()
{
  return workerpool_get_worker(lmn_thread_id);
}

/* workerpoolのid番目のLmnWorkerオブジェクトを返す */
inline LmnWorker *workerpool_get_worker(unsigned long id)
{
  return p_workers[id];
}

static void workerpool_make(StateSpace states, LmnWorkerFuncs *f, BOOL flags)
{
  /* init static global worker pool */
  unsigned int i, n;
  n = lmn_env.core_num;
  p_workers = LMN_NALLOC(LmnWorker *, n);
  for (i = 0; i < n; i++) {
    LmnWorker *w = new_worker(states, i, f, flags);
    WORKER_INIT(w);
    p_workers[i] = w;
  }
}

static void workerpool_free()
{
  unsigned int i, n;
  n = lmn_env.core_num;
  for (i = 0; i < n; i++) {
    LmnWorker *w = p_workers[i];
    WORKER_FINALIZE(w);
    worker_free(w);
  }
  LMN_FREE(p_workers);
}

static void workerpool_set_ring()
{
  unsigned int i, n;
  n = lmn_env.core_num;
  for (i = 0; i < n; i++) {
    LmnWorker *w, *next;
    w    = workerpool_get_worker(i);
    next = workerpool_get_worker((i + 1) % n);
    w->next = next;
  }
}

