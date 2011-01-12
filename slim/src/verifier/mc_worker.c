/*
 * mc_worker.c
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
 *  utility for Paral*/

#include "mc.h"
#include "mc_worker.h"
#include "mc_generator.h"
#include "mc_explorer.h"
#include "state.h"
#include "statespace.h"
#include "error.h"
#include "runtime_status.h"

/** prototypes
 */
static void worker_start(void *arg);
static void worker_set_tls_id(unsigned long id);
static void worker_set_env(LmnWorker *w);
static void workerpool_make(StateSpace states, BOOL flags);
static void workerpool_free(void);
static void workerpool_set_ring(void);


/* TODO:
 *  struct LmnWorkerPoolみたいな構造を作って,
 *  それをいじくりまわしてあげるように変更した方がよいかも.. */
static LmnWorker   **p_workers;
static unsigned int  p_worker_num;
static BOOL          p_terminated;
static lmn_barrier_t p_barrier;

/* parallel workersの初期設定 (MT-Unsafe) */
void lmn_workers_env_init(StateSpace states, BOOL flags)
{
  /* worker pool の構築と初期設定 */
  p_terminated = FALSE;
  p_worker_num = lmn_env.core_num;
  lmn_barrier_init(&p_barrier, p_worker_num);
  workerpool_make(states, flags);
  workerpool_set_ring();
}


/* 後始末 */
void lmn_workers_env_finalize(void)
{
  lmn_barrier_destroy(&p_barrier);
  workerpool_free();
  worker_set_tls_id(0);  /* primary_threadのTLS IDを元に戻す */
}


/* スレッドの起動 (MT-unsafe) */
void launch_lmn_workers()
{
  unsigned long i, core_num;

  core_num = p_worker_num;
  for (i = 1; i < core_num; i++) { /** start */
    lmn_thread_create(&WORKER_PID(workerpool_get_worker(i)), worker_start, i);
  }
  if (lmn_env.profile_level >= 1) profile_start_exec();
  worker_start((void *)0UL);
  if (lmn_env.profile_level >= 1) profile_finish_exec();
  for (i = 1; i < core_num; i++) {
    lmn_thread_join(WORKER_PID(workerpool_get_worker(i)));
  }
}



#define TERMINATION_CONDITION(W)  !WORKER_IS_ACTIVE(W) && \
                                   WORKER_IS_WHITE(W)  && \
                                   WORKER_CHECK(W)

/* 全てのWorkerオブジェクトが実行を停止している場合に真を返す. */
BOOL lmn_workers_termination_detection_for_rings(LmnWorker *root)
{
  /** 概要:
   *  LmnWorkerは論理的に輪を形成しており, フラグチェックを輪に沿って実施する.
   *  is_activeかどうかをチェックする他にis_stealer, is_whiteもチェックする.
   *    is_white  : 自idより小さいidのworkerに状態を送信した場合の不正終了を判定するフラグ
   *                フラグチェック済みのworkerの仕事を投げた場合に不正終了するため
   *                @see Dijkstra's Token Termination Detection (parallel computing)
   *    is_steaker: 盗みを働いたことを示すフラグ.
   *                フラグチェック済のworkerがフラグ未チェックworkerからWork Stealingした場合に不正終了するため
   *
   *  Dijkstraの手法を一部参考にしてはいるが, 実装はまったくDijkstraの手法ではないため注意.
   *  (共有メモリなので, Tokenを輪の方向に回し, Tokenが戻ってきたら判定終了, という同期が必要な処理は必要ない)
   *  Primary Worker(id==0)が判定した検知結果を他のWorkerは読み出す, という実装にしている.
   */
  if (WORKER_ID(root) == 0 && !p_terminated) {
    int i, n;
    BOOL ret;

    ret = TRUE;
    n   = p_worker_num;

    for (i = 0; i < n; i++) {
      LmnWorker *w = workerpool_get_worker(i);
      ret = ret && TERMINATION_CONDITION(w);
      WORKER_SET_WHITE(w); /* 検査したらwhiteに戻す */
      if (!ret) return FALSE;
    }

    for (i = 0; i < n; i++) {
      LmnWorker *w = workerpool_get_worker(i);
      ret = ret && !WORKER_IS_STEALER(w);
      WORKER_UNSET_STEALER(w);
      if (!ret) return FALSE;
    }

    p_terminated = ret;
  }

  return p_terminated;
}


/* 全てのWorkerオブジェクトで同期を取り, id番のWorkerが関数funcを実行する.
 * 全てのWorkerが本関数に入った場合, 関数を抜ける.
 * 関数名がいまいち合ってない気がしてならない */
void lmn_workers_synchronization(LmnWorker    *me,
                                 unsigned int id,
                                 void (*func)(LmnWorker *w))
{
  lmn_barrier_wait(&p_barrier);

  if (func) {
    if (WORKER_ID(me) == id) {
      (*func)(me);
    }
    lmn_barrier_wait(&p_barrier);
  }
}


/** -------------------------------------
 *  LmnWorker
 */

/* まっさらなLmnWorkerオブジェクトをmallocして返す */
inline LmnWorker *lmn_worker_make_minimal()
{
  LmnWorker *w = LMN_MALLOC(LmnWorker);

  w->id        = 0;
  w->f_safe    = 0x00U;
  w->f_exec    = 0x00U;
  w->f_end     = FALSE;
  w->f_end2    = FALSE;
  w->states    = NULL;
  w->next      = NULL;
  w->type      = 0x00U;
  w->type2     = 0x00U;
  w->obj       = NULL;
  w->obj2      = NULL;
  w->start     = NULL;
  w->check     = NULL;
  w->init      = NULL;
  w->init2     = NULL;
  w->finalize  = NULL;
  w->finalize2 = NULL;

  return w;
}

/* LmnWorkerオブジェクトを初期化生成して返す */
LmnWorker *lmn_worker_make(StateSpace     ss,
                           unsigned long  id,
                           BOOL           flags)
{
  LmnWorker *w = lmn_worker_make_minimal();
  w->states = ss;
  w->id     = id;
  WORKER_SET_FLAGS(w, flags);
  WORKER_SET_ACTIVE(w);
  WORKER_SET_WHITE(w);

  return w;
}


void lmn_worker_free(LmnWorker *w)
{
  LMN_FREE(w);
}


/* Start Verification:
 * >>>> ここから先の処理は全てMT-safeでなければならない <<<< */
static void worker_start(void *arg)
{
  LmnWorker *w;
  unsigned long id;

  id = (unsigned long)arg;
  worker_set_tls_id(id);          /* 1. Thread Local Storageの参照に用いるIDを設定 */
  thread_set_cpu_affinity(id);    /* 2. ThreadをCPUに貼り付ける */
  w = workerpool_get_my_worker(); /* 3. 1で取得したIDから実行スレッド用のLmnWorkerオブジェクトへの参照を張る */

  mc_react_cxt_init(&WORKER_RC(w), DEFAULT_STATE_ID);
  if (lmn_env.profile_level >= 1) profile_start_exec_thread();
  WORKER_START(w);

  if (!mc_data.mc_exit && !mc_data.error_exist) {
    if (worker_use_owcty(w)) {
      owcty_start(w);
    } else if (worker_use_map(w) && !worker_use_weak_map(w)) {
      map_iteration_start(w);
    }
  }

  if (lmn_env.profile_level >= 1) profile_finish_exec_thread();
  mc_react_cxt_destroy(&WORKER_RC(w));
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

/* 呼び出したスレッドのTLS IDに応じたLmnWorkerオブジェクトをWorkerPoolから返す */
LmnWorker *workerpool_get_my_worker()
{
  /* >>>>>>>> worker_set_tls_idでTLS IDを割り当ててから使用すること <<<<<<< */
  return workerpool_get_worker(lmn_thread_id);
}


/* WorkerPoolのid番目のLmnWorkerオブジェクトを返す */
inline LmnWorker *workerpool_get_worker(unsigned long id)
{
  return (id >= p_worker_num) ? NULL : p_workers[id];
}


static void workerpool_make(StateSpace states, BOOL flags)
{
  unsigned int i, n;
  n = p_worker_num;
  p_workers = LMN_NALLOC(LmnWorker *, n);

  for (i = 0; i < n; i++) {
    LmnWorker *w = lmn_worker_make(states, i, flags);
    p_workers[i] = w;

    /* アルゴリズムの割り当てと初期化 */
    worker_set_env(w);
    WORKER_INIT(w);
  }
}


static void workerpool_free()
{
  unsigned int i, n;
  n = p_worker_num;
  for (i = 0; i < n; i++) {
    LmnWorker *w = workerpool_get_worker(i);
    WORKER_FINALIZE(w);
    lmn_worker_free(w);
  }
  LMN_FREE(p_workers);
}


static void workerpool_set_ring()
{
  unsigned int i, n;
  n = p_worker_num;
  for (i = 0; i < n; i++) {
    LmnWorker *w, *next;
    w    = workerpool_get_worker(i);
    next = workerpool_get_worker((i + 1) % n);
    w->next = next;
  }
}



/* workerの実行アルゴリズムの割当を行う */
static void worker_set_env(LmnWorker *w)
{
  if (!lmn_env.nd) {
    lmn_fatal("UnExepcted Yet.");
  }

  if (lmn_env.enable_parallel)        worker_set_parallel(w);
  if (lmn_env.optimize_loadbalancing) worker_set_dynamic_lb(w);

  if (!lmn_env.bfs) { /* Depth First Search */
    dfs_env_set(w);
  } else {            /* Breadth First Search */
    bfs_env_set(w);
  }

  if (lmn_env.ltl) {
    if (lmn_env.enable_owcty) {
      owcty_env_set(w);
    } else if (lmn_env.enable_map) {
      map_env_set(w);
    } else if (lmn_env.enable_bledge || worker_use_lsync(w) || worker_on_mc_bfs(w)) {
      bledge_env_set(w);
    }

    /* 特定のアルゴリズムが指定されていない場合のデフォルト動作 */
    if (worker_ltl_none(w)) {
      if (worker_on_parallel(w) || worker_on_mc_bfs(w)) {
        /* 並列アルゴリズムを使用している or BFSの場合のデフォルト */
        owcty_env_set(w);
      } else {
        ndfs_env_set(w);
      }
    }


    if (worker_use_owcty(w)) {
      lmn_env.enable_owcty = TRUE;
    } else if (worker_use_ble(w)) {
      lmn_env.enable_bledge = TRUE;
    }

    if (!worker_use_opt_scc(w)) {
      lmn_env.prop_scc_driven = FALSE;
    }
  }
}


