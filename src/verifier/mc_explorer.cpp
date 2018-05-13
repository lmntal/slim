/*
 * mc_explorer.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
#include "mc_explorer.h"
#include "element/element.h"
#include "mc.h"
#include "mc_worker.h"
#include "state.h"
#include "state.hpp"

#ifdef PROFILE
#include "runtime_status.h"

#define START_CYCLE_SEARCH()                                                   \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_start_timer(PROFILE_TIME__CYCLE_EXPLORE);                          \
  }
#define FINISH_CYCLE_SEARCH()                                                  \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_finish_timer(PROFILE_TIME__CYCLE_EXPLORE);                         \
  }
#else
#define START_CYCLE_SEARCH()
#define FINISH_CYCLE_SEARCH()
#endif

#define TRANS_BETWEEN_DIFF_SCCs(W, U, T, A)                                    \
  (worker_use_opt_scc(W) && (state_scc_id(A, U) != state_scc_id(A, T)))
#define STATE_PROP_SCC_N(W, S, A)                                              \
  (worker_use_opt_scc(W) && (state_scc_id(A, S) == SCC_TYPE_NON_ACCEPT))

/* DFSにより状態seedから状態goalへのパスをVector pathに積む.
 * ただし, 既にon_cycle_flagが立っている状態はpathに含まない. */
static BOOL state_to_state_path(State *seed, State *goal, Vector *search,
                                Vector *path, st_table_t traversed) {
  vec_push(search, (vec_data_t)seed);

  while (vec_num(search) > 0) {
    State *s;
    unsigned int i;

    s = (State *)vec_peek(search);
    if (st_contains(traversed, (st_data_t)s)) {
      State *s_pop = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == s_pop) {
        vec_pop(path);
      }
    } else {
      st_add_direct(traversed, (st_data_t)s, (st_data_t)s);
      vec_push(path, (vec_data_t)s);

      for (i = 0; i < s->successor_num; i++) {
        State *succ = state_succ_state(s, i);

        if (succ->is_on_cycle() || !s->is_expanded())
          continue;
        else if (succ == goal) {
          return TRUE;
        } else {
          vec_push(search, (vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}

/** ==================================
 *  =========== Nested-DFS ===========
 *  ==================================
 */

static BOOL ndfs_loop(State *seed, Vector *search, Vector *postordered);
static void ndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                       Vector *cycle_path);

typedef struct McSearchNDFS McSearchNDFS;
struct McSearchNDFS {
  Vector *open;
  Vector *path;
};

#define NDFS_WORKER_OBJ(W) ((McSearchNDFS *)worker_explorer_obj(W))
#define NDFS_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define NDFS_WORKER_OPEN_VEC(W) (NDFS_WORKER_OBJ(W)->open)
#define NDFS_WORKER_PATH_VEC(W) (NDFS_WORKER_OBJ(W)->path)
#define NDFS_WORKER_OBJ_CLEAR(W)                                               \
  do {                                                                         \
    vec_clear(NDFS_WORKER_OPEN_VEC(W));                                        \
    vec_clear(NDFS_WORKER_PATH_VEC(W));                                        \
  } while (0)

void ndfs_worker_init(LmnWorker *w) {
  McSearchNDFS *mc = LMN_MALLOC(McSearchNDFS);
  mc->open = vec_make(1024);
  mc->path = vec_make(512);
  NDFS_WORKER_OBJ_SET(w, mc);
}

void ndfs_worker_finalize(LmnWorker *w) {
  vec_free(NDFS_WORKER_OPEN_VEC(w));
  vec_free(NDFS_WORKER_PATH_VEC(w));
  LMN_FREE(NDFS_WORKER_OBJ(w));
}

void ndfs_env_set(LmnWorker *w) {
  worker_set_ndfs(w);
  worker_explorer_init_f_set(w, ndfs_worker_init);
  worker_explorer_finalize_f_set(w, ndfs_worker_finalize);
}

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void ndfs_start(LmnWorker *w, State *seed) {
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  vec_push(NDFS_WORKER_OPEN_VEC(w), (vec_data_t)seed);
  has_error = ndfs_loop(seed, NDFS_WORKER_OPEN_VEC(w), NDFS_WORKER_PATH_VEC(w));

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    ndfs_found_accepting_cycle(w, seed, NDFS_WORKER_PATH_VEC(w));
  }

  NDFS_WORKER_OBJ_CLEAR(w);
}

void ndfs_found_accepting_cycle(LmnWorker *w, State *seed, Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  workers_found_error(wp);

  gen_counter_example = lmn_env.dump;
 seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
   s->set_on_cycle();

    if (gen_counter_example)
      vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->do_exhaustive) {
    workers_set_exit(wp);
  }
}

static BOOL ndfs_loop(State *seed, Vector *search, Vector *path) {
  while (vec_num(search) > 0) {
    State *s = (State *)vec_peek(search);

    if (s->is_snd()) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == s_pop) {
        vec_pop(path);
      }
    } else {
      unsigned int i;
      vec_push(path, (vec_data_t)s);
     s->set_snd();
      for (i = 0; i < s->successor_num; i++) {
        State *succ = state_succ_state(s, i);

        if (succ->is_on_cycle()) {
          return FALSE;
        } else if (!succ->is_expanded()) {
          continue;
        } else if (succ == seed /* || succ->is_on_stack() */) {
          return TRUE; /* 同一のseedから探索する閉路が1つ見つかったならば探索を打ち切る
                        */
        } else {
          vec_push(search, (vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}

/** ==================================
 *  === One Way Catch Them Young =====
 *  ==================================
 */

typedef struct McSearchOWCTY McSearchOWCTY;
struct McSearchOWCTY {
  Queue *accepts1;
  Queue *accepts2;
  unsigned long old;
  unsigned long iteration;
  st_table_t traversed; /* 反例生成用 */
};

#define OWCTY_WORKER_OBJ(W) ((McSearchOWCTY *)worker_explorer_obj(W))
#define OWCTY_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define OWCTY_WORKER_AQ1(W) (OWCTY_WORKER_OBJ(W)->accepts1)
#define OWCTY_WORKER_AQ2(W) (OWCTY_WORKER_OBJ(W)->accepts2)
#define OWCTY_WORKER_HASHSET(W) (OWCTY_WORKER_OBJ(W)->traversed)

static inline void owcty_reachability(LmnWorker *w, Queue *primary,
                                      Queue *secondary, BOOL set_flag,
                                      BOOL is_up);
static inline BOOL owcty_traversed_owner_is_me(State *succ, BOOL set_flag,
                                               BOOL is_up);
static inline void owcty_report_midterm(LmnWorker *w);
static inline void owcty_termination_detection(LmnWorker *w);
static void owcty_found_accepting_cycle(LmnWorker *w, AutomataRef a);

void owcty_worker_init(LmnWorker *w) {
  McSearchOWCTY *mc = LMN_MALLOC(McSearchOWCTY);

  /* 全ワーカでオブジェクトを共有 */

  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (workers_entried_num(worker_group(w)) > 1) {
      mc->accepts1 = make_parallel_queue(LMN_Q_MRMW);
      mc->accepts2 = make_parallel_queue(LMN_Q_MRMW);
    } else {
      mc->accepts1 = new_queue();
      mc->accepts2 = new_queue();
    }
  } else {
    LmnWorker *primary = workers_get_worker(worker_group(w), LMN_PRIMARY_ID);
    mc->accepts1 = OWCTY_WORKER_AQ1(primary);
    mc->accepts2 = OWCTY_WORKER_AQ2(primary);
  }

  mc->traversed = st_init_ptrtable();
  mc->old = 0;
  mc->iteration = 0;

  OWCTY_WORKER_OBJ_SET(w, mc);
}

void owcty_worker_finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    q_free(OWCTY_WORKER_AQ1(w));
    q_free(OWCTY_WORKER_AQ2(w));
  }
  st_free_table(OWCTY_WORKER_HASHSET(w));
  LMN_FREE(OWCTY_WORKER_OBJ(w));
}

void owcty_env_set(LmnWorker *w) {
  worker_set_owcty(w);

  worker_explorer_init_f_set(w, owcty_worker_init);
  worker_explorer_finalize_f_set(w, owcty_worker_finalize);

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }

  if (lmn_env.enable_map_heuristic) {
    worker_set_weak_map(w);
    worker_set_map(w);
  }
}

static inline void statetable_enqueue_f(State *s, LmnWord _q) {
  enqueue((Queue *)_q, (LmnWord)s);
}

void statetable_to_state_queue(StateTable *st, Queue *q) {
  statetable_foreach(st, (void (*)(ANYARGS))statetable_enqueue_f, (LmnWord)q,
                     DEFAULT_ARGS);
}

static void owcty_env_init(LmnWorker *w) {
  statetable_to_state_queue(statespace_accept_tbl(worker_states(w)),
                            OWCTY_WORKER_AQ1(w));
  statetable_to_state_queue(statespace_accept_memid_tbl(worker_states(w)),
                            OWCTY_WORKER_AQ2(w));

  MC_DEBUG(printf("acceptance queue init, num=%lu\n",
                  queue_entry_num(OWCTY_WORKER_AQ1(w))));
}

struct DegreeCnt {
  unsigned int in, out;
  LmnWord *ins, *outs;
};

void owcty_start(LmnWorker *w) {
  /* st_tableにも, elock, wlockを組込む. */
  if (worker_id(w) == LMN_PRIMARY_ID) {
  }

  /* 1枚のハッシュ表.
   * スレッド固有Queue (SRMW)
   * state id partition
   * 受信したら入力変数を増やす */

  lmn_workers_synchronization(w, owcty_env_init);

  while (!worker_group(w)->mc_exit) {
    START_CYCLE_SEARCH();

    /* OWCTYはReachabilityの結果から不動点への到達を判定するため,
     * reachability2回の実行を1セットとして考える.
     * 1st Reachabilityで訪問した頂点にFOR_MC_MASKフラグをTRUEにし,
     *     訪問した受理頂点を起点に2nd Reachabilityを行う
     * 2nd
     * Reachabilityで訪問した頂点にはFOR_MC_MASKフラグが既にTRUEになっているため
     *     フラグをFALSEにしていく. */
#define ORG_MY_OWCTY
#ifdef ORG_MY_OWCTY
    owcty_reachability(w, OWCTY_WORKER_AQ1(w), OWCTY_WORKER_AQ2(w), FOR_MC_MASK,
                       TRUE);
    lmn_workers_synchronization(w, owcty_report_midterm);
    owcty_reachability(w, OWCTY_WORKER_AQ2(w), OWCTY_WORKER_AQ1(w), FOR_MC_MASK,
                       FALSE);
    lmn_workers_synchronization(w, owcty_termination_detection);
#else

#endif
    FINISH_CYCLE_SEARCH();
  }

  if (!is_empty_queue(OWCTY_WORKER_AQ1(w))) {
    owcty_found_accepting_cycle(w, statespace_automata(worker_states(w)));
  }
}

static inline void owcty_report_midterm(LmnWorker *w) {
  McSearchOWCTY *mc = OWCTY_WORKER_OBJ(w);
  mc->old = queue_entry_num(mc->accepts2);
}

/* owctyアルゴリズムの停止を判定した場合, mc_exitフラグを真にする.
 * (owctyはon-the-flyアルゴリズムではないため,
 *  generator側の終了検知ではなく, 専用の終了検知を用意した.) */
static inline void owcty_termination_detection(LmnWorker *w) {
  McSearchOWCTY *mc;
  unsigned long q_num;

  mc = OWCTY_WORKER_OBJ(w);
  mc->iteration++;
  MC_DEBUG(fprintf(stderr, "iter%3lu[S=%10lu, old=%10lu]  %s", mc->iteration,
                   queue_entry_num(mc->accepts1), mc->old,
                   (mc->iteration % 3 == 0) ? "\n" : ""));

  q_num = queue_entry_num(mc->accepts1);
  if (q_num == 0 || q_num == mc->old) {
    MC_DEBUG(fprintf(stderr, "\n"));
    worker_group(w)->mc_exit = TRUE;
  }
}

/* primary Queueに積まれた頂点を起点に到達可能な受理頂点の集合を
 * secondary QueueがNULLでなければsecondary Queueに積む.
 * 訪問した頂点は,
 *   is_upが真ならset_flagを真に
 *   is_upが偽ならset_flagを偽に
 * 設定する */
static inline void owcty_reachability(LmnWorker *w, Queue *primary,
                                      Queue *secondary, BOOL set_flag,
                                      BOOL is_up) {
  StateSpaceRef ss = worker_states(w);
  while (!is_empty_queue(primary)) {
    State *s;
    unsigned int i, cnt;

    s = (State *)dequeue(primary);
    if (!s) {
      continue;
    } else if (STATE_PROP_SCC_N(w, s, statespace_automata(ss)) ||
               smap_is_deleted(s) || (MAP_COND(w) && !s->map)) {
      /* A. 性質オートマトン上でSCCを跨ぐ遷移ならば受理サイクルを形成しない
       * B. 既に削除マーキング済
       * C. map
       * iteration1週目でなんらかのMAP値が設定されていない頂点は受理頂点から到達不可能
       * A〜Cのいずれかが真ならば, 削除マークをつける */
      smap_set_deleted(s);
      continue;
    }

    for (i = 0, cnt = 0; i < s->successor_num; i++) {
      State *succ = state_succ_state(s, i);

      if (TRANS_BETWEEN_DIFF_SCCs(w, s, succ, statespace_automata(ss)))
        continue;

      if (!owcty_traversed_owner_is_me(succ, set_flag, is_up)) {
        if (state_is_accept(statespace_automata(ss), succ) &&
            !smap_is_deleted(succ)) {
          enqueue(secondary, (LmnWord)succ);
          cnt++;
        }
      } else {
        cnt++;
      }
    }

    if (cnt == 0) {
      backward_elimination(w, s);
    }
  }
}

/* 状態succのフラグset_flagの更新に成功した場合、FALSEを返す.
 * フラグは,
 *   is_upが真-->真
 *   is_upが偽-->偽
 * に設定する. */
static inline BOOL owcty_traversed_owner_is_me(State *succ, BOOL set_flag,
                                               BOOL is_up) {
  BOOL flags_fetch, flags_update;

  flags_fetch = succ->flags;

  if (is_up) {
    if (flags_fetch & set_flag) {
      return TRUE;
    } else {
      flags_update = flags_fetch | set_flag;
      return !CAS(succ->flags, flags_fetch, flags_update);
    }
  } else {
    if (flags_fetch & set_flag) {
      flags_update = flags_fetch & ~(set_flag);
      return !CAS(succ->flags, flags_fetch, flags_update);
    } else {
      return TRUE;
    }
  }
}

/* 受理サイクルを発見した場合に呼び出す.
 * 反例出力が必要な場合は, 不動点の状態集合からサイクルパスを求めて登録する. */
static void owcty_found_accepting_cycle(LmnWorker *w, AutomataRef a) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    LmnWorkerGroup *wp = worker_group(w);
    wp->error_exist = TRUE;

    if (lmn_env.dump) {
      Vector search, path;

      vec_init(&search, 64);
      vec_init(&path, 32);
      while (!is_empty_queue(OWCTY_WORKER_AQ1(w))) {
        State *seed = (State *)dequeue(OWCTY_WORKER_AQ1(w));

        if (state_is_end(a, seed)) {
          mc_found_invalid_state(wp, seed);
        } else if (!seed->is_on_cycle()) {
          if (state_to_state_path(seed, seed, &search, &path,
                                  OWCTY_WORKER_HASHSET(w))) {
            Vector *v;
            unsigned int i;

            v = vec_make(vec_num(&path));
            for (i = 0; i < vec_num(&path); i++) {
              State *tmp = (State *)vec_get(&path, i);
             tmp->set_on_cycle();
              vec_push(v, (vec_data_t)tmp);
            }

            mc_found_invalid_path(wp, v);
          }
          st_clear(OWCTY_WORKER_HASHSET(w));
          vec_clear(&search);
          vec_clear(&path);
        }
      }

      vec_destroy(&search);
      vec_destroy(&path);
    }
  }
}

/** =======================================
 *  === Maximal Accepting Predecessors ====
 *  =======================================
 */
#include <stdarg.h>

typedef struct McSearchMAP McSearchMAP;
struct McSearchMAP {
  Queue *propagate;
  Queue *waitingSeed;
  st_table_t traversed;
};

#define MAP_WORKER_OBJ(W) ((McSearchMAP *)worker_explorer_obj(W))
#define MAP_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define MAP_WORKER_PROPAG_G(W) (MAP_WORKER_OBJ(W)->propagate)
#define MAP_WORKER_DEL_G(W) (MAP_WORKER_OBJ(W)->waitingSeed)
#define MAP_WORKER_HASHSET(W) (MAP_WORKER_OBJ(W)->traversed)

State *map_ordering_states(AutomataRef a, unsigned int num, ...);
static inline State *map_ordering(State *s1, State *s2, AutomataRef a);
static inline BOOL map_entry_state(State *t, State *propag, AutomataRef a);
static void map_found_accepting_cycle(LmnWorker *w, State *s);
static void map_propagate(LmnWorker *w, State *s, State *t, State *propag,
                          AutomataRef a);
static State *map_ordering_propagate_state(LmnWorker *w, State *u,
                                           AutomataRef a);

void map_worker_init(LmnWorker *w) {
  McSearchMAP *mc = LMN_MALLOC(McSearchMAP);

  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (workers_entried_num(worker_group(w)) > 1) {
      mc->propagate = make_parallel_queue(LMN_Q_MRMW);
      mc->waitingSeed = make_parallel_queue(LMN_Q_MRMW);
    } else {
      mc->propagate = new_queue();
      mc->waitingSeed = new_queue();
    }
  } else {
    LmnWorker *prim = workers_get_worker(worker_group(w), LMN_PRIMARY_ID);
    mc->propagate = MAP_WORKER_PROPAG_G(prim);
    mc->waitingSeed = MAP_WORKER_DEL_G(prim);
  }

  mc->traversed = st_init_ptrtable();

  MAP_WORKER_OBJ_SET(w, mc);
}

void map_worker_finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    q_free(MAP_WORKER_PROPAG_G(w));
    q_free(MAP_WORKER_DEL_G(w));
  }
  st_free_table(MAP_WORKER_HASHSET(w));
  LMN_FREE(MAP_WORKER_OBJ(w));
}

void map_env_set(LmnWorker *w) {
  worker_set_map(w);

  worker_explorer_init_f_set(w, map_worker_init);
  worker_explorer_finalize_f_set(w, map_worker_finalize);

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }
}

/* 頂点uからuのサクセッサにMAPを伝搬する.
 * MAPを更新できる限り, MAPの再伝搬を行う */
void map_start(LmnWorker *w, State *u) {
  AutomataRef a;

  START_CYCLE_SEARCH();

  if (u->successor_num == 0) {
    backward_elimination(w, u);
  }

  a = statespace_automata(worker_states(w));

  do {
    State *propag;
    unsigned int i;

    propag = map_ordering_propagate_state(w, u, a);
    for (i = 0; i < u->successor_num; i++) {
      State *t = state_succ_state(u, i);
      map_propagate(w, u, t, propag, a);
    }

    if (worker_use_weak_map(w)) {
      u = NULL;
    } else {
      u = (State *)dequeue(MAP_WORKER_PROPAG_G(w));
    }
  } while (u);

  FINISH_CYCLE_SEARCH();
}

/* MAPアルゴリズムの2週目以降.
 * on-the-flyなMAPの伝搬でDELと判定した頂点を起点に
 * MAP値の再伝搬を行う */
void map_iteration_start(LmnWorker *w) {
  lmn_workers_synchronization(w, NULL);

  while (!is_empty_queue(MAP_WORKER_DEL_G(w))) {
    State *seed = (State *)dequeue(MAP_WORKER_DEL_G(w));
    if (seed && !smap_is_not_delete(seed)) {
      smap_set_deleted(seed);
      map_start(w, seed);
    }
  }
}

/* 状態uからサクセサへ伝搬するMAP値を決定し, 返す.
 * MAP値は, 伝搬元となる状態uと,
 * uに伝搬されて設定されているMAP値が指す状態とで決定する */
static State *map_ordering_propagate_state(LmnWorker *w, State *u,
                                           AutomataRef a) {
  State *propag;

  if (!state_is_accept(a, u) || smap_is_deleted(u)) {
    propag = u->map;
  } else { /* u ∈ A */
    propag = map_ordering(u, u->map, a);

    if (!worker_use_weak_map(w)) {
      if (propag == u) {
        smap_unset_not_delete(u);
        enqueue(MAP_WORKER_DEL_G(w), (LmnWord)u);
      } else {
        smap_set_not_delete(u);
        smap_unset_deleted(u);
      }
    }
  }

  return propag;
}

static void map_propagate(LmnWorker *w, State *s, State *t, State *propag,
                          AutomataRef a) {
  if (TRANS_BETWEEN_DIFF_SCCs(w, s, t, a))
    return;

  if (propag == t || (state_is_accept(a, t) && t == s)) {
    map_found_accepting_cycle(w, t);
  } else if (propag == map_ordering(propag, t->map, a)) {
    if (map_entry_state(t, propag, a) && !worker_use_weak_map(w) &&
        t->is_expanded()) {
      enqueue(MAP_WORKER_PROPAG_G(w), (LmnWord)t);
    }
  }
}

/* 状態tのMAP値に, propagが優先するMAP値なら伝搬する.
 * 伝搬に成功した場合, 真を返す. */
static inline BOOL map_entry_state(State *t, State *propag, AutomataRef a) {
  BOOL ret = FALSE;

  do {
    State *fetch = t->map;

    if (fetch == propag) {
      break;
    } else if (CAS(t->map, fetch, propag)) {
      ret = TRUE;
      break;
    } else if (propag != map_ordering(t->map, propag, a)) {
      ret = FALSE;
      break;
    }
  } while (1);

  return ret;
}

static inline State *map_ordering(State *s1, State *s2, AutomataRef a) {
  BOOL s1_valid, s2_valid;

  /* MAP値を比較するべき頂点として有効なものは, 未削除の受理頂点のみ */
  s1_valid =
      s1 && state_is_accept(a, s1) && !smap_is_deleted(s1) && state_id(s1) != 0;
  s2_valid =
      s2 && state_is_accept(a, s2) && !smap_is_deleted(s2) && state_id(s2) != 0;

  if (s1_valid && s2_valid) {
    return state_id(s1) < state_id(s2) ? s1 : s2;
  } else if (!s1_valid) {
    return s2_valid ? s2 : NULL;
  } else {
    return s1_valid ? s1 : NULL;
  }
}

/* N個の状態を入力として受け, MAP順序が最も高い状態を返す.
 * なんとなく書いてみたけど, 使わなかった */
State *map_ordering_states(AutomataRef a, unsigned int num, ...) {
  State *ptr, *ret;
  va_list states;
  unsigned int i;

  va_start(states, num);

  ret = NULL;
  for (i = 0; i < num; i++) {
    ptr = va_arg(states, State *);
    ret = map_ordering(ptr, ret, a);
  }

  va_end(states);

  return ret;
}

/* 受理状態でサクセッサがない状態sを探索対象外とすることをparent側に伝搬させていく.
 */
void backward_elimination(LmnWorker *w, State *s) {
  while (s) {
    unsigned int i;

    smap_set_deleted(s);
    s = state_get_parent(s);

    if (!s || smap_is_deleted(s) || !s->is_expanded()) {
      return;
    } else if (s->successor_num == 1) {
      continue;
    }

    /* state_succ_num > 1 */

    for (i = 0; i < s->successor_num; i++) {
      if (!smap_is_deleted(state_succ_state(s, i))) {
        return;
      }
    }
  }
}

/* 受理サイクルを発見した場合に呼び出す.
 * 反例出力が必要な場合は, 起点となる頂点と同じMAP値の状態を辿るように
 * self-reachability testを行う. */
static void map_found_accepting_cycle(LmnWorker *w, State *s) {
  LmnWorkerGroup *wp = worker_group(w);

  workers_found_error(wp);
  if (!wp->do_exhaustive) {
    workers_set_exit(wp);
  }

  if (lmn_env.dump && !s->is_on_cycle()) {
    /* TODO: とりあえず単純なself reachability testで実装しているので,
     *        MAP値の等しい状態だけを辿るようにしたself reachabilityに直す */
    Vector search, path;
    st_table_t traversed;

    vec_init(&search, 64);
    vec_init(&path, 32);
    if (worker_use_weak_map(w)) {
      traversed = st_init_ptrtable();
    } else {
      traversed = MAP_WORKER_HASHSET(w);
    }

    if (state_to_state_path(s, s, &search, &path, traversed)) {
      Vector *v;
      unsigned int i;

      v = vec_make(vec_num(&path));
      for (i = 0; i < vec_num(&path); i++) {
        State *tmp = (State *)vec_get(&path, i);
       tmp->set_on_cycle();
        vec_push(v, (vec_data_t)tmp);
      }
      mc_found_invalid_path(wp, v);
    }

    if (worker_use_weak_map(w)) {
      st_free_table(traversed);
    } else {
      st_clear(MAP_WORKER_HASHSET(w));
    }
    vec_destroy(&search);
    vec_destroy(&path);
  }
}

/** ==================================
 *  === Back Level Edges =============
 *  ==================================
 */

typedef struct McSearchBLE McSearchBLE;
struct McSearchBLE {
  Queue *layer;
  Vector *path;
  Vector *search;
  st_table_t traversed;
};

#define BLE_WORKER_OBJ(W) ((McSearchBLE *)worker_explorer_obj(W))
#define BLE_WORKER_OBJ_SET(W, O) (worker_explorer_obj_set(W, O))
#define BLE_WORKER_LAYER_Q(W) (BLE_WORKER_OBJ(W)->layer)
#define BLE_WORKER_PATH_VEC(W) (BLE_WORKER_OBJ(W)->path)
#define BLE_WORKER_SEARCH_VEC(W) (BLE_WORKER_OBJ(W)->search)
#define BLE_WORKER_HASHSET(W) (BLE_WORKER_OBJ(W)->traversed)

static void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path);

void bledge_env_set(LmnWorker *w) {
  worker_set_ble(w);
  worker_explorer_init_f_set(w, bledge_worker_init);
  worker_explorer_finalize_f_set(w, bledge_worker_finalize);

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }
}

void bledge_worker_init(LmnWorker *w) {
  McSearchBLE *mc = LMN_MALLOC(McSearchBLE);
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (workers_entried_num(worker_group(w)) > 1) {
      mc->layer = make_parallel_queue(LMN_Q_MRMW);
    } else {
      mc->layer = new_queue();
    }
  } else {
    mc->layer =
        BLE_WORKER_LAYER_Q(workers_get_worker(worker_group(w), LMN_PRIMARY_ID));
  }

  mc->path = vec_make(32);
  mc->search = vec_make(64);
  mc->traversed = st_init_ptrtable();

  BLE_WORKER_OBJ_SET(w, mc);
}

void bledge_worker_finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    q_free(BLE_WORKER_LAYER_Q(w));
  }
  vec_free(BLE_WORKER_PATH_VEC(w));
  vec_free(BLE_WORKER_SEARCH_VEC(w));
  st_free_table(BLE_WORKER_HASHSET(w));
}

/* State Vectorの各状態に受理頂点が含まれていたら真を返す */
static BOOL bledge_path_accepting(Vector *v, AutomataRef a) {
  unsigned int i;

  for (i = 0; i < vec_num(v); i++) {
    State *t = (State *)vec_get(v, i);
    if (state_is_accept(a, t))
      return TRUE;
  }

  return FALSE;
}

static BOOL bledge_explorer_accepting_cycle(LmnWorker *w, State *u, State *v) {
  AutomataRef a = statespace_automata(worker_states(w));
  return state_to_state_path(v, u, BLE_WORKER_SEARCH_VEC(w),
                             BLE_WORKER_PATH_VEC(w), BLE_WORKER_HASHSET(w)) &&
         bledge_path_accepting(BLE_WORKER_PATH_VEC(w), a);
}

void bledge_start(LmnWorker *w) {
  lmn_workers_synchronization(w, NULL);

  START_CYCLE_SEARCH();

  while (!is_empty_queue(BLE_WORKER_LAYER_Q(w))) {
    State *u;
    unsigned int i;

    if (!(u = (State *)dequeue(BLE_WORKER_LAYER_Q(w))))
      continue;
    for (i = 0; i < u->successor_num; i++) {
      State *v = state_succ_state(u, i);

      if (v->is_expanded()) { /* detected back level edge:t where [u]--t-->[v] */
        if (!u->is_on_cycle() && !v->is_on_cycle() &&
            bledge_explorer_accepting_cycle(w, u, v)) {
          vec_push(BLE_WORKER_PATH_VEC(w), (vec_data_t)u);
          bledge_found_accepting_cycle(w, BLE_WORKER_PATH_VEC(w));
        }

        st_clear(BLE_WORKER_HASHSET(w));
        vec_clear(BLE_WORKER_PATH_VEC(w));
        vec_clear(BLE_WORKER_SEARCH_VEC(w));
      }
    }
  }

  FINISH_CYCLE_SEARCH();
}

void bledge_store_layer(LmnWorker *w, State *s) {
  if (s->successor_num > 0) {
    enqueue(BLE_WORKER_LAYER_Q(w), (LmnWord)s);
  }
}

static void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  wp->error_exist = TRUE;

  gen_counter_example = lmn_env.dump;
  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
   s->set_on_cycle();
    if (gen_counter_example)
      vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->do_exhaustive) {
    wp->mc_exit = TRUE;
  }
}

/** ==================================
 *  === MAP + Nested-DFS ========
 *  ==================================
 */

/* NDFSとMAPのハイブリッドなアルゴリズム */
static BOOL mapndfs_loop(State *seed, Vector *search, Vector *postordered);
static void mapndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                          Vector *cycle_path);

#define MAPNDFS_USE_MAP
/* MAP_WORKER_~
 * 系のマクロでアクセスするため、最初にMcSearchMapを持ってくる必要がある */
/* McSearchMapを変更したらこっちも変更する必要あり */
/* MAP系関数・マクロを使い回したかったので、こんな構造にしたが、色々と問題がありそうな構造なので要修正
 */
typedef struct McSearchMAPNDFS McSearchMAPNDFS;
struct McSearchMAPNDFS {
#ifdef MAPNDFS_USE_MAP
  Queue *propagate;
  Queue *waitingSeed;
  st_table_t traversed;
#endif
  Vector *open;
  Vector *path;
};

#define MAPNDFS_WORKER_OBJ(W) ((McSearchMAPNDFS *)worker_explorer_obj(W))
#define MAPNDFS_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define MAPNDFS_WORKER_OPEN_VEC(W) (MAPNDFS_WORKER_OBJ(W)->open)
#define MAPNDFS_WORKER_PATH_VEC(W) (MAPNDFS_WORKER_OBJ(W)->path)
#define MAPNDFS_WORKER_OBJ_CLEAR(W)                                            \
  do {                                                                         \
    vec_clear(MAPNDFS_WORKER_OPEN_VEC(W));                                     \
    vec_clear(MAPNDFS_WORKER_PATH_VEC(W));                                     \
  } while (0)

void mapndfs_worker_init(LmnWorker *w) {
  McSearchMAPNDFS *mc = LMN_MALLOC(McSearchMAPNDFS);
  mc->open = vec_make(1024);
  mc->path = vec_make(512);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (workers_entried_num(worker_group(w)) > 1) {
      mc->propagate = make_parallel_queue(LMN_Q_MRMW);
      mc->waitingSeed = make_parallel_queue(LMN_Q_MRMW);
    } else {
      mc->propagate = new_queue();
      mc->waitingSeed = new_queue();
    }
  } else {
    LmnWorker *prim = workers_get_worker(worker_group(w), LMN_PRIMARY_ID);
    mc->propagate = MAP_WORKER_PROPAG_G(prim);
    mc->waitingSeed = MAP_WORKER_DEL_G(prim);
  }
  mc->traversed = NULL;
#endif

  MAPNDFS_WORKER_OBJ_SET(w, mc);
}

void mapndfs_worker_finalize(LmnWorker *w) {
  vec_free(MAPNDFS_WORKER_OPEN_VEC(w));
  vec_free(MAPNDFS_WORKER_PATH_VEC(w));

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    q_free(MAP_WORKER_PROPAG_G(w));
    q_free(MAP_WORKER_DEL_G(w));
  }
  if (MAP_WORKER_HASHSET(w))
    st_free_table(MAP_WORKER_HASHSET(w));
#endif

  LMN_FREE(MAPNDFS_WORKER_OBJ(w));
}

void mapndfs_env_set(LmnWorker *w) {
  if (lmn_env.core_num == 1)
    ndfs_env_set(w);
  else {
    worker_set_mapndfs(w);
    worker_explorer_init_f_set(w, mapndfs_worker_init);
    worker_explorer_finalize_f_set(w, mapndfs_worker_finalize);

    // thread0のみexplorer
    if (worker_id(w) == lmn_env.core_num - 1)
      worker_explorer_set(w);
    else
      worker_generator_set(w);
    if (lmn_env.prop_scc_driven) {
      worker_set_opt_scc(w);
    }
#ifdef MAPNDFS_USE_MAP
    if (lmn_env.enable_map_heuristic) {
      worker_set_map(w);
    }
#endif
  }
}

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void mapndfs_start(LmnWorker *w, State *seed) {
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  vec_push(MAPNDFS_WORKER_OPEN_VEC(w), (vec_data_t)seed);
  has_error = mapndfs_loop(seed, MAPNDFS_WORKER_OPEN_VEC(w),
                           MAPNDFS_WORKER_PATH_VEC(w));

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    mapndfs_found_accepting_cycle(w, seed, MAPNDFS_WORKER_PATH_VEC(w));
  }

  MAPNDFS_WORKER_OBJ_CLEAR(w);
}

void mapndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                   Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  workers_found_error(wp);

  gen_counter_example = lmn_env.dump;
 seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
   s->set_on_cycle();

    if (gen_counter_example)
      vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->do_exhaustive) {
    workers_set_exit(wp);
  }
}

static BOOL mapndfs_loop(State *seed, Vector *search, Vector *path) {
  while (vec_num(search) > 0) {
    State *s = (State *)vec_peek(search);

    if (s->is_snd()) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == s_pop) {
        vec_pop(path);
      }
    } else {
      unsigned int i;
      vec_push(path, (vec_data_t)s);
     s->set_snd();
      for (i = 0; i < s->successor_num; i++) {
        State *succ = state_succ_state(s, i);

        if (succ->is_on_cycle()) {
          return FALSE;
        } else if (!succ->is_expanded()) {
          continue;
        } else if (succ == seed /* || succ->is_on_stack() */) {
          return TRUE; /* 同一のseedから探索する閉路が1つ見つかったならば探索を打ち切る
                        */
        } else {
          vec_push(search, (vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}

#ifndef MINIMAL_STATE
/** ==================================
 *  === Multicore Nested-DFS ========
 *  ==================================
 */
static BOOL mcndfs_loop(LmnWorker *w, State *seed, Vector *search,
                        Vector *postordered, Vector *red_states);
static void mcndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                         Vector *cycle_path) LMN_UNUSED;

void mcndfs_worker_init(LmnWorker *w) {
  McSearchMAPNDFS *mc = LMN_MALLOC(McSearchMAPNDFS);
  mc->open = vec_make(1024);
  mc->path = vec_make(512);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (workers_entried_num(worker_group(w)) > 1) {
      mc->propagate = make_parallel_queue(LMN_Q_MRMW);
      mc->waitingSeed = make_parallel_queue(LMN_Q_MRMW);
    } else {
      mc->propagate = new_queue();
      mc->waitingSeed = new_queue();
    }
  } else {
    LmnWorker *prim = workers_get_worker(worker_group(w), LMN_PRIMARY_ID);
    mc->propagate = MAP_WORKER_PROPAG_G(prim);
    mc->waitingSeed = MAP_WORKER_DEL_G(prim);
  }
  mc->traversed = NULL;
#endif

  MAPNDFS_WORKER_OBJ_SET(w, mc);
}

void mcndfs_worker_finalize(LmnWorker *w) {
  vec_free(MAPNDFS_WORKER_OPEN_VEC(w));
  vec_free(MAPNDFS_WORKER_PATH_VEC(w));

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    q_free(MAP_WORKER_PROPAG_G(w));
    q_free(MAP_WORKER_DEL_G(w));
  }
  if (MAP_WORKER_HASHSET(w))
    st_free_table(MAP_WORKER_HASHSET(w));
#endif

  LMN_FREE(MAPNDFS_WORKER_OBJ(w));
}

void mcndfs_env_set(LmnWorker *w) {
  if (lmn_env.core_num == 1)
    ndfs_env_set(w);
  else {
    worker_set_mcndfs(w);
    worker_explorer_init_f_set(w, mapndfs_worker_init);
    worker_explorer_finalize_f_set(w, mapndfs_worker_finalize);
    w->is_explorer = FALSE;

    if (lmn_env.prop_scc_driven) {
      worker_set_opt_scc(w);
    }

    if (lmn_env.enable_map_heuristic) {
      worker_set_map(w);
    }
  }
}

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void mcndfs_start(LmnWorker *w, State *seed, Vector *red_states) {
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  vec_push(MAPNDFS_WORKER_OPEN_VEC(w), (vec_data_t)seed);
  has_error = mcndfs_loop(w, seed, MAPNDFS_WORKER_OPEN_VEC(w),
                          MAPNDFS_WORKER_PATH_VEC(w), red_states);

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    mapndfs_found_accepting_cycle(w, seed, MAPNDFS_WORKER_PATH_VEC(w));
  }

  MAPNDFS_WORKER_OBJ_CLEAR(w);
}

void mcndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                  Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  workers_found_error(wp);

  gen_counter_example = lmn_env.dump;
 seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
   s->set_on_cycle();

    if (gen_counter_example)
      vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->do_exhaustive) {
    workers_set_exit(wp);
  }
}

static BOOL mcndfs_loop(LmnWorker *w, State *seed, Vector *search, Vector *path,
                        Vector *red_states) {
  unsigned int i, j, n, m;
  State *t, *succ;
  BOOL contained;

  while (vec_num(search) > 0) {
    State *s = (State *)vec_peek(search);

    if (s->is_snd()) {
      t = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == t) {
        vec_pop(path);
      }
      continue;
    }

    put_stack(red_states, s);
   s->set_snd();

    n = s->successor_num;
    for (i = 0; i < n; i++) {
      succ = state_succ_state(s, i);
      if (succ->s_is_cyan(worker_id(w))) {
        return TRUE;
      } else if (!succ->s_is_red()) {
        m = vec_num(red_states);
        contained = FALSE;
        for (j = 0; j < m; j++) {
          t = (State *)vec_get(red_states, j);
          if (state_id(t) == state_id(succ)) {
            contained = TRUE;
            break;
          }
        }
        if (!contained)
          put_stack(search, succ);
      }
    }

#if 0
    if (s->is_snd()) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == s_pop) {
        vec_pop(path);
      }
    }
    else {
      unsigned int i;
      vec_push(path, (vec_data_t)s);
     s->set_snd();
      for (i = 0; i < s->successor_num; i++) {
        State *succ = state_succ_state(s, i);

        if (succ->is_on_cycle()) {
          return FALSE;
        } else if (!succ->is_expanded()) {
          continue;
        } else if (s->s_is_cyan(worker_id(w))/*succ == seed*/ /* || succ->is_on_stack() */) {
          return TRUE; /* 同一のseedから探索する閉路が1つ見つかったならば探索を打ち切る */
        } else {
          vec_push(search, (vec_data_t)succ);
        }
      }
    }
#endif
  }

  return FALSE;
}
#endif
