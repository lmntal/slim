/*
 * mc_worker.cpp
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
 *  utilities for parallel/concurrent execution */
#include "mc_worker.h"
#include "mc.h"
#include "mc_explorer.h"
#include "mc_generator.h"
#include "runtime_status.h"
#include "state.h"
#include "state.hpp"
#include "state_dumper.h"
#include "statespace.h"
#include <limits.h>

namespace c14 = slim::element;

/** -------------------------------------
 *  MC object
 */

LmnWorkerStrategy::LmnWorkerStrategy(LmnWorker *w)
    : start(nullptr), check(nullptr) {
  explorer = c14::make_unique<LmnMCObj>(w);
}

/** -------------------------------------
 *  LmnWorker
 */

static void lmn_worker_start(void *arg);
static void worker_TLS_init(unsigned int id);
static void worker_TLS_finalize(void);

/* まっさらなLmnWorkerオブジェクトをmallocして返す */
inline LmnWorker *lmn_worker_make_minimal() {
  LmnWorker *w = new (LmnWorker);

  w->id = 0;
  w->f_safe = 0x00U;
  w->f_exec = 0x00U;
  w->f_end = FALSE;
  w->f_end2 = FALSE;
  w->wait = FALSE;
  w->states = NULL;
  w->_next = NULL;
  w->invalid_seeds = NULL;
  w->cycles = NULL;
  w->expand = 0;
  w->red = 0;

  return w;
}

/* LmnWorkerオブジェクトを初期化生成して返す */
LmnWorker *lmn_worker_make(LmnWorkerGroup *wg, StateSpaceRef ss,
                           unsigned long id, BOOL flags) {
  LmnWorker *w = lmn_worker_make_minimal();
  w->states = ss;
  w->id = id;
  w->cxt = c14::make_unique<MCReactContext>(nullptr);
  worker_flags_set(w, flags);
  worker_set_active(w);
  worker_set_white(w);
  w->group = wg;
  w->invalid_seeds = new Vector(4);
  w->cycles = new Vector(4);

  /* アルゴリズムの割り当てと初期化 */
  w->set_env();
  worker_init(w);

  return w;
}

void lmn_worker_free(LmnWorker *w) { delete (w); }

/* Verification start */
void LmnWorkerGroup::start(LmnWorker *w) {
  worker_TLS_init(worker_id(w));

  worker_rc(w) = c14::make_unique<MCReactContext>(nullptr);

  if (worker_id(w) == LMN_PRIMARY_ID && mc_is_dump(worker_flags(w))) {
    StateSpaceRef ss = worker_states(w);
    StateDumper::from_env(ss->output())->dump(ss->initial_state());
  }

  if (lmn_env.profile_level >= 1)
    profile_start_exec_thread();
  worker_start(w);

  if (!this->workers_are_exit() && !this->workers_have_error()) {
    if (worker_use_owcty(w)) {
      owcty_start(w);
    } else if (worker_use_map(w) && !worker_use_weak_map(w)) {
      map_iteration_start(w);
    }
    /* else: ND-mode etc */
  }

  if (lmn_env.profile_level >= 1)
    profile_finish_exec_thread();
  worker_TLS_finalize();
}

static void worker_TLS_init(unsigned int inc_id) {
  /* 各スレッド毎に, 自分のTLS idを設定する */
  env_my_TLS_init(inc_id);
  lmn_thread_set_CPU_affinity(inc_id);
}

static void worker_TLS_finalize() { env_my_TLS_finalize(); }

/** -----------------------------------------------------------
 *  Worker Group
 */
LmnWorkerGroup::LmnWorkerGroup() {
  workers = NULL;
  ewlock = NULL;
}
LmnWorkerGroup::LmnWorkerGroup(AutomataRef a, Vector *psyms, int thread_num) {

  BOOL flags;
  /* worker pool の構築と初期設定 */
  workers_unset_terminated();
  workers_set_entried_num(thread_num);
  workers_unset_stop();

  workers_unset_do_search();
  workers_unset_do_exhaustive();
  workers_unset_do_palgorithm();
  workers_unset_exit();
  workers_unfound_error();

  workers_set_opt_end_state(NULL);

#ifdef KWBT_OPT
  if (thread_num >= 2 && lmn_env.opt_mode != OPT_NONE) {
    workers_set_ewlock(ewlock_make(1U, DEFAULT_WLOCK_NUM));
  } else
#endif
    workers_set_ewlock(NULL);

  flags = flags_init(a);
#ifdef OPT_WORKERS_SYNC
  workers_set_synchronizer(thread_num);
#else
  lmn_barrier_init(workers_synchronizer(), workers_get_entried_num());
#endif
  workers_gen(workers_get_entried_num(), a, psyms, flags);
  ring_alignment();
}
LmnWorkerGroup::~LmnWorkerGroup() {
#ifndef OPT_WORKERS_SYNC
  lmn_barrier_destroy(workers_synchronizer());
#endif
  workers_free(workers_get_entried_num());

  if (workers_get_ewlock()) {
    ewlock_free(workers_get_ewlock());
  }
}

LmnWorkerGroup::LmnWorkerGroup(const LmnWorkerGroup &lwg) {
  // copy soshi
}

volatile BOOL LmnWorkerGroup::workers_are_exit() { return mc_exit; }

void LmnWorkerGroup::workers_set_exit() { mc_exit = TRUE; }

void LmnWorkerGroup::workers_unset_exit() { mc_exit = FALSE; }

BOOL LmnWorkerGroup::workers_have_error() { return error_exist; }

void LmnWorkerGroup::workers_found_error() { error_exist = TRUE; }

void LmnWorkerGroup::workers_unfound_error() { error_exist = FALSE; }

BOOL LmnWorkerGroup::workers_get_do_palgorithm() { return do_para_algo; }

void LmnWorkerGroup::workers_set_do_palgorithm() { do_para_algo = TRUE; }

void LmnWorkerGroup::workers_unset_do_palgorithm() { do_para_algo = FALSE; }

State *LmnWorkerGroup::workers_get_opt_end_state() { return opt_end_state; }

void LmnWorkerGroup::workers_set_opt_end_state(State *s) { opt_end_state = s; }

EWLock *LmnWorkerGroup::workers_get_ewlock() { return ewlock; }

void LmnWorkerGroup::workers_set_ewlock(EWLock *e) { ewlock = e; }

void LmnWorkerGroup::workers_opt_end_lock() {
  ewlock_acquire_enter(ewlock, 0U);
}

void LmnWorkerGroup::workers_opt_end_unlock() {
  ewlock_release_enter(ewlock, 0U);
}

void LmnWorkerGroup::workers_state_lock(mtx_data_t id) {
  ewlock_acquire_write(ewlock, id);
}

void LmnWorkerGroup::workers_state_unlock(mtx_data_t id) {
  ewlock_release_write(ewlock, id);
}

BOOL LmnWorkerGroup::workers_are_terminated() { return terminated; }

void LmnWorkerGroup::workers_set_terminated() { terminated = TRUE; }

void LmnWorkerGroup::workers_unset_terminated() { terminated = FALSE; }

volatile BOOL LmnWorkerGroup::workers_are_stop() { return stop; }

void LmnWorkerGroup::workers_set_stop() { stop = TRUE; }

void LmnWorkerGroup::workers_unset_stop() { stop = FALSE; }

BOOL LmnWorkerGroup::workers_are_do_search() { return do_search; }

void LmnWorkerGroup::workers_set_do_search() { do_search = TRUE; }

void LmnWorkerGroup::workers_unset_do_search() { do_search = FALSE; }

BOOL LmnWorkerGroup::workers_are_do_exhaustive() { return do_exhaustive; }

void LmnWorkerGroup::workers_set_do_exhaustive() { do_exhaustive = TRUE; }

void LmnWorkerGroup::workers_unset_do_exhaustive() { do_exhaustive = FALSE; }

unsigned int LmnWorkerGroup::workers_get_entried_num() { return worker_num; }

void LmnWorkerGroup::workers_set_entried_num(unsigned int i) { worker_num = i; }
#ifdef OPT_WORKERS_SYNC
volatile unsigned int LmnWorkerGroup::workers_synchronizer() {
  return synchronizer;
}

void LmnWorkerGroup::workers_set_synchronizer(unsigned int i) {
  synchronizer = i;
}
#else
lmn_barrier_t *LmnWorkerGroup::workers_synchronizer() { return &synchronizer; }

void LmnWorkerGroup::workers_set_synchronizer(lmn_barrier_t t) {
  synchronizer = t;
}
#endif

void LmnWorkerGroup::workers_free(unsigned int worker_num) {
  unsigned int i, j;
  for (i = 0; i < worker_num; i++) {
    LmnWorker *w = workers[i];
    delete w->invalid_seeds;

    for (j = 0; j < w->cycles->get_num(); j++) {
      delete (Vector *)w->cycles->get(j);
    }
    delete w->cycles;

    if (i == 0) {
      delete (worker_states(w));
    }

    worker_finalize(w);
    lmn_worker_free(w);
  }
  LMN_FREE(workers);
}

void LmnWorkerGroup::workers_gen(unsigned int worker_num, AutomataRef a,
                                 Vector *psyms, BOOL flags) {
  unsigned int i;
  workers = LMN_NALLOC(LmnWorker *, worker_num);
  for (i = 0; i < worker_num; i++) {
    StateSpaceRef states;

    if (i == 0) {
      states = worker_num > 1 ? new StateSpace(worker_num, a, psyms)
                              : new StateSpace(a, psyms);
    } else {
      states = worker_states(get_worker(0));
    }

    LmnWorker *w = lmn_worker_make(this, states, i, flags);

    workers[i] = w;
  }
}

/* 実行時オプションが増えたため,
 * Worker起動以前のこの時点で,
 * 組み合わせをサポートしていないオプションの整合性を取るなどを行う.
 * 実際に使用するオプションフラグを, lmn_env構造体からworker構造体にコピーする.
 */
BOOL LmnWorkerGroup::flags_init(
    AutomataRef property_a) { // this should be in LmnEnv class
  BOOL flags = 0x00U;

  /* === 1. 出力フォーマット === */

  /* --- 1-1. GraphViz形式の使用 --- */
  if (lmn_env.output_format == DOT) {
    lmn_env.output_format = DEFAULT;
  }

  /* --- 1-2. インクリメンタルな状態の標準出力 --- */
  if (lmn_env.sp_dump_format == INCREMENTAL) {
    mc_set_dump(flags);
    lmn_env.dump = TRUE;
    if (lmn_env.mc_dump_format != CUI && lmn_env.mc_dump_format != LaViT) {
      lmn_fatal("unsupported incremental output format");
    }
    fprintf(stdout, "States\n");
  }

  /* --- 1-3. 並列処理中のサポート外オプション --- */
  if (lmn_env.core_num >= 2) {
    if (lmn_env.sp_dump_format == INCREMENTAL) {
#ifndef DEBUG
      lmn_fatal("unsupported combination incremental state dumper & "
                "parallelization.");
#endif
    }
#ifdef PROFILE
    if (lmn_env.optimize_hash_old) {
      lmn_fatal(
          "unsupported combination optimized hash (old) & parallelization.");
    }
#endif
  }

  /* === 2. 状態空間構築オプション === */

  /* --- 2-1. 遷移オブジェクトの使用 --- */
  if (lmn_env.show_transition) {
    mc_set_trans(flags);
  }

  /* --- 2-2. バイト列エンコードを使用しない場合 -----
   * 対応していない全てのオプションフラグを下げる. */
  if (lmn_env.enable_compress_mem) {
    if (lmn_env.delta_mem) {
      mc_set_delta(flags);
    }
    mc_set_compress(flags);
  } else {
    lmn_env.delta_mem = FALSE;
    lmn_env.optimize_hash = FALSE;
#ifdef PROFILE
    lmn_env.optimize_hash_old = FALSE;
#endif

    if (lmn_env.hyperlink) {
      lmn_fatal("no support: membrane isomorphis tester for hyper graph model");
    }
  }

  /* --- 2-3. バイト列エンコードと階層グラフ構造の一対一対応 --- */
  if (lmn_env.mem_enc) {
    mc_set_canonical(flags);
  }

  /* --- 2-4. Partial Order Reduction --- */
  if (lmn_env.enable_por || lmn_env.enable_por_old) {
    mc_set_por(flags);
  }

  /* --- 2-5. binstr compressor --- */
  if (lmn_env.z_compress) {
    lmn_env.d_compress = FALSE;
    lmn_env.tree_compress = FALSE;
    lmn_env.hash_compaction = FALSE;
  }

  /* --- 2-6. hash compaction --- */
  if (lmn_env.hash_compaction) {
    lmn_env.z_compress = FALSE;
    lmn_env.d_compress = FALSE;
    lmn_env.tree_compress = FALSE;
  }

  /* --- 2-7. tree compression --- */
  if (lmn_env.tree_compress) {
    lmn_env.z_compress = FALSE;
    lmn_env.d_compress = FALSE;
    lmn_env.hash_compaction = FALSE;
    lmn_env.mem_enc = FALSE;
#ifdef PROFILE
    lmn_env.optimize_hash_old = FALSE;
#endif
  }

  /* === 3. 状態空間探索(LTLモデル検査)オプション === */
  if (lmn_env.ltl) {
    if (!property_a) {
      lmn_env.ltl = FALSE;
      lmn_env.ltl_all = FALSE;
      workers_unset_do_search();
      workers_unset_do_exhaustive();
    } else {
      if (lmn_env.ltl_all) {
        workers_set_do_exhaustive();
      }
      workers_set_do_search();
    }

    if (lmn_env.enable_parallel) {
      workers_set_do_palgorithm();
    }
  }

  /* === 4. hypergraph ==== */
  if (lmn_env.hyperlink) {
    if (lmn_env.optimize_hash) {
      lmn_env.optimize_hash = FALSE;
    }

    if (lmn_env.delta_mem || lmn_env.mem_enc || !lmn_env.enable_compress_mem) {
      lmn_fatal("under constructions: delta/canonical membrane methods for "
                "hyper graph model");
    }
  }

  return flags;
}

struct lmn_worker_start_arg {
  LmnWorkerGroup *grp;
  LmnWorker *worker;
};
static void lmn_worker_start(void *args_) {
  auto args = (lmn_worker_start_arg *)args_;
  args->grp->start(args->worker);
  delete args;
}

/* スレッドの起動 (MT-unsafe) */
void LmnWorkerGroup::launch_lmn_workers() {
  unsigned long i, core_num;

  core_num = workers_get_entried_num();
  for (i = 0; i < core_num; i++) { /** start */
    if (i == LMN_PRIMARY_ID)
      continue;
    auto args = new lmn_worker_start_arg();
    args->grp = this;
    args->worker = get_worker(i);
    lmn_thread_create(&worker_pid(get_worker(i)), lmn_worker_start, args);
  }
  if (lmn_env.profile_level >= 1)
    profile_start_exec();

  this->start(get_worker(LMN_PRIMARY_ID));

  if (lmn_env.profile_level >= 1)
    profile_finish_exec();
  for (i = 0; i < core_num; i++) {
    if (i == LMN_PRIMARY_ID)
      continue;
    lmn_thread_join(worker_pid(get_worker(i)));
  }
}

#define TERMINATION_CONDITION(W)                                               \
  (worker_is_idle(W) && worker_is_white(W) && worker_check(w))

/* 全てのWorkerオブジェクトが実行を停止している場合に真を返す. */
BOOL lmn_workers_termination_detection_for_rings(LmnWorker *root) {
  // when someone calls this function, the group which root is included is
  // unknown so this function should be a LmnWorker member method. But after
  // finding the group, LmnWorkerGroup should handle.
  /** 概要:
   *  LmnWorkerは論理的に輪を形成しており, フラグチェックを輪に沿って実施する.
   *  is_activeかどうかをチェックする他にis_stealer, is_whiteもチェックする.
   *    is_white  :
   *       自idより小さいidのworkerにタスクを送信した場合の不正終了を判定するフラグ.
   *       (チェック済みのworkerに対するタスク送信が発生することで不正終了する)
   *       @see Dijkstra's Token Termination Detection (parallel computing)
   *    is_steaker:
   *       異なるWorkerからタスクを横取りしたこを示すフラグ.
   *       (チェック済のworkerが未チェックのworkerからタスクを横取りした場合に不正終了する)
   *
   *  Dijkstraの手法を一部参考にしてはいるが, Dijkstraの手法ではないため注意.
   */

  /* Primary
   * Worker(id==LMN_PRIMARY_ID)が判定した検知結果をグローバル変数に書込む.
   * 他のWorkerはグローバル変数に書き込まれた終了フラグを読み出す. */
  return worker_group(root)->termination_detection(worker_id(root));
  // this is all.
}

BOOL LmnWorkerGroup::termination_detection(int id) {
  if (id == LMN_PRIMARY_ID && !workers_are_terminated()) {
    int i, n;
    BOOL ret;

    ret = TRUE;
    n = workers_get_entried_num();
    for (i = 0; i < n; i++) {
      LmnWorker *w = get_worker(i);
      ret = ret && TERMINATION_CONDITION(w);
      worker_set_white(w);
      if (!ret)
        return FALSE;
    }

    for (i = 0; i < n; i++) {
      LmnWorker *w = get_worker(i);
      ret = ret && !worker_is_stealer(w);
      worker_unset_stealer(w);
      if (!ret)
        return FALSE;
    }
    workers_set_terminated();
  }

  return workers_are_terminated();
}

/* 全てのWorkerオブジェクトで同期を取り, Primary Workerが関数funcを実行する.
 * 全てのWorkerがbarrierに到達したとき処理を再開する. */
void lmn_workers_synchronization(LmnWorker *me, void (*func)(LmnWorker *w)) {
  /*この関数を呼ぶ時点ではどのgroupを呼ぶか不明なのでLmnWorkerクラスに入るべき*/
  worker_group(me)->lmn_workers_synchronization(worker_id(me), (*func));
}

void LmnWorkerGroup::lmn_workers_synchronization(unsigned long id,
                                                 void (*func)(LmnWorker *w)) {
  lmn_barrier_wait(workers_synchronizer());
  if (id == LMN_PRIMARY_ID && func) {
    (*func)(get_worker(id));
  }
  lmn_barrier_wait(workers_synchronizer());
}

/* 呼び出したスレッドのTLS IDに応じたLmnWorkerオブジェクトをWorkerPoolから返す
 */
LmnWorker *LmnWorkerGroup::workers_get_my_worker() {
  /* >>>>>>>> worker_TLS_init以前に使用してはならない <<<<<<< */
  return get_worker(env_my_thread_id());
}

/* WorkerPoolのid番目のLmnWorkerオブジェクトを返す */
LmnWorker *LmnWorkerGroup::get_worker(unsigned long id) {
  if (id >= workers_get_entried_num()) {
    return NULL;
  } else {
    return workers[id];
  }
}

void LmnWorkerGroup::ring_alignment() {
  unsigned int i, n;
  n = workers_get_entried_num();
  for (i = 0; i < n; i++) {
    get_worker(i)->_next = get_worker((i + 1) % n);
  }
}

LmnWorkerStrategyOption LmnWorkerStrategyOption::from_env() {
  LmnWorkerStrategyOption option;

  if (!lmn_env.nd) {
    lmn_fatal("UnExepcted Yet.");
  }

  if (!lmn_env.bfs) { /* Depth First Search */
    option.construction = strategy::Construction::DepthFirst;
  } else { /* Breadth First Search */
    option.construction = strategy::Construction::BreadthFirst;
    if (lmn_env.bfs_layer_sync) {
      option.does_sync_layers_in_bfs = true;
    }
  }

  if (lmn_env.enable_parallel)
    option.is_parallel = true;

  if (lmn_env.optimize_loadbalancing)
    option.does_loadbalancing = true;

  if (lmn_env.ltl) {
    if (lmn_env.enable_owcty) {
      option.is_parallel = true;
      option.modelcheck = strategy::LTLModelCheck::OneWayCatchThemYoung;
      option.prop_scc_driven = lmn_env.prop_scc_driven;
      option.enable_map_heuristic = lmn_env.enable_map_heuristic;
    } else if (lmn_env.enable_map) {
      option.is_parallel = true;
      option.modelcheck = strategy::LTLModelCheck::MaximalAcceptingPredecessors;
      option.prop_scc_driven = lmn_env.prop_scc_driven;
    } else if (lmn_env.enable_mapndfs) {
      option.modelcheck =
          strategy::LTLModelCheck::MaximalAcceptingPredecessors_NestedDFS;
      option.is_parallel = true;
      option.prop_scc_driven = lmn_env.prop_scc_driven;
      if (lmn_env.core_num == 1) {
        option.modelcheck = strategy::LTLModelCheck::NestedDepthFirstSearch;
        option.is_parallel = false;
      }
      option.enable_map_heuristic = lmn_env.enable_map_heuristic;
#ifndef MINIMAL_STATE
    } else if (lmn_env.enable_mcndfs) {
      option.modelcheck = strategy::LTLModelCheck::MulticoreNestedDFS;
      option.prop_scc_driven = lmn_env.prop_scc_driven;
      option.is_parallel = true;
      if (lmn_env.core_num == 1) {
        option.modelcheck = strategy::LTLModelCheck::NestedDepthFirstSearch;
        option.is_parallel = false;
      }

      option.enable_map_heuristic = lmn_env.enable_map_heuristic;
#endif
    } else if (lmn_env.enable_bledge ||
               option.construction == strategy::Construction::BreadthFirst ||
               lmn_env.bfs_layer_sync) {
      // 条件が間違ってそう
      option.modelcheck = strategy::LTLModelCheck::BackLevelEdges;
      option.is_parallel = true;
      option.prop_scc_driven = lmn_env.prop_scc_driven;
    } else if (option.is_parallel) {
      /* 並列アルゴリズムを使用している場合のデフォルト */
      option.modelcheck = strategy::LTLModelCheck::OneWayCatchThemYoung;
    } else {
      option.modelcheck = strategy::LTLModelCheck::NestedDepthFirstSearch;
    }

    // 選択されたオプションに合うように環境設定に書き戻す
    // ここでやるべきなのかはよくわからない

    if (option.modelcheck == strategy::LTLModelCheck::OneWayCatchThemYoung) {
      lmn_env.enable_owcty = true;
    } else if (option.modelcheck == strategy::LTLModelCheck::BackLevelEdges) {
      lmn_env.enable_bledge = true;
    }

    if (option.modelcheck != strategy::LTLModelCheck::OneWayCatchThemYoung &&
        option.modelcheck !=
            strategy::LTLModelCheck::MaximalAcceptingPredecessors &&
        option.modelcheck != strategy::LTLModelCheck::BackLevelEdges &&
#ifndef MINIMAL_STATE
        option.modelcheck != strategy::LTLModelCheck::MulticoreNestedDFS &&
#endif
        option.modelcheck !=
            strategy::LTLModelCheck::MaximalAcceptingPredecessors_NestedDFS) {
      lmn_env.prop_scc_driven = false;
    }
  }

  return option;
}

LmnWorkerStrategy::LmnWorkerStrategy(LmnWorker *w,
                                     LmnWorkerStrategyOption const &option)
    : LmnWorkerStrategy(w) {
  switch (option.construction) {
  case strategy::Construction::DepthFirst:
    if (option.is_parallel)
      this->generator =
          c14::make_unique<slim::verifier::tactics::DFS_Parallel>(w);
    else {
      if (slim::config::kwbt_opt && lmn_env.opt_mode != OPT_NONE)
        this->generator =
            c14::make_unique<slim::verifier::tactics::DFS_kwbtopt>(w);
      else
        this->generator = c14::make_unique<slim::verifier::tactics::DFS>(w);
    }
    break;
  case strategy::Construction::BreadthFirst:
    this->generator = c14::make_unique<slim::verifier::tactics::BFS>(w);
    break;
  }

  switch (option.modelcheck) {
  case strategy::LTLModelCheck::None:
    // does nothing
    break;
  case strategy::LTLModelCheck::NestedDepthFirstSearch:
    this->explorer = c14::make_unique<slim::verifier::tactics::NDFS>(w);
    break;
  case strategy::LTLModelCheck::OneWayCatchThemYoung:
    this->explorer = c14::make_unique<slim::verifier::tactics::OWCTY>(w);

    if (option.prop_scc_driven) {
      this->generator->type |= WORKER_F1_MC_OPT_SCC_MASK;
    }

    if (option.enable_map_heuristic) {
      this->explorer->type |= WORKER_F2_MC_MAP_WEAK_MASK;
      this->explorer->type |= WORKER_F2_MC_MAP_MASK;
    }
    break;
  case strategy::LTLModelCheck::MaximalAcceptingPredecessors:
    this->explorer = c14::make_unique<slim::verifier::tactics::MAP>(w);
    if (option.prop_scc_driven) {
      this->generator->type |= WORKER_F1_MC_OPT_SCC_MASK;
    }
    break;
  case strategy::LTLModelCheck::MaximalAcceptingPredecessors_NestedDFS:
    this->explorer = c14::make_unique<slim::verifier::tactics::MAP_NDFS>(w);

    if (option.prop_scc_driven) {
      this->generator->type |= WORKER_F1_MC_OPT_SCC_MASK;
    }
#ifdef MAPNDFS_USE_MAP
    if (option.enable_map_heuristic) {
      this->explorer->type |= WORKER_F2_MC_MAP_MASK;
    }
#endif
    break;
#ifndef MINIMAL_STATE
  case strategy::LTLModelCheck::MulticoreNestedDFS:
    this->explorer = c14::make_unique<slim::verifier::tactics::MultiNDFS>(w);
    this->is_explorer = false;

    if (option.prop_scc_driven) {
      this->generator->type |= WORKER_F1_MC_OPT_SCC_MASK;
    }

    if (option.enable_map_heuristic) {
      this->explorer->type |= WORKER_F2_MC_MAP_MASK;
    }
    break;
#endif
  case strategy::LTLModelCheck::BackLevelEdges:
    this->explorer = c14::make_unique<slim::verifier::tactics::BLE>(w);

    if (option.prop_scc_driven) {
      this->generator->type |= WORKER_F1_MC_OPT_SCC_MASK;
    }
    break;
  }

  if (option.is_parallel)
    this->generator->type |= WORKER_F1_PARALLEL_MASK;

  if (option.does_loadbalancing)
    this->generator->type |= WORKER_F1_DYNAMIC_LB_MASK;

  if (option.does_sync_layers_in_bfs)
    this->generator->type |= WORKER_F1_MC_BFS_LSYNC_MASK;
}

/* workerの実行アルゴリズムの割当を行う */
void LmnWorker::set_env() {
  auto option = LmnWorkerStrategyOption::from_env();
  this->strategy = LmnWorkerStrategy(this, option);

  if (option.modelcheck == strategy::LTLModelCheck::MaximalAcceptingPredecessors_NestedDFS) {
    // thread0のみexplorer
    this->strategy.is_explorer = (this->id == lmn_env.core_num - 1);
  }
}

LmnCost LmnWorkerGroup::opt_cost(){
  State *opt = workers_get_opt_end_state();
  if(!opt){
    return lmn_env.opt_mode == OPT_MINIMIZE ? ULONG_MAX : 0;
  } else {
    return state_cost(opt);
  }
}

/* 最適コストを可能ならば更新する
 * f==true: minimize
 * f==false: maximize */
void LmnWorkerGroup::update_opt_cost(State *new_s, BOOL f){
  if(env_threads_num() >= 2)
    workers_opt_end_lock();
  if((f && opt_cost() > state_cost(new_s)) ||
    (!f && opt_cost() < state_cost(new_s))){
    workers_set_opt_end_state(new_s);
  }
  if(env_threads_num() >= 2)
    workers_opt_end_unlock();
}
