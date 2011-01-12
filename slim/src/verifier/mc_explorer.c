/*
 * mc_explorer.c
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

#include "mc.h"
#include "mc_worker.h"
#include "mc_explorer.h"
#include "state.h"
#include "vector.h"
#include "error.h"
#include "lmntal_thread.h"

#ifdef PROFILE
#  include "runtime_status.h"

#  define START_CYCLE_SEARCH()                                             \
    if (lmn_env.profile_level >= 3) {                                      \
      profile_start_timer(PROFILE_TIME__CYCLE_EXPLORE);                    \
    }
#  define FINISH_CYCLE_SEARCH()                                            \
    if (lmn_env.profile_level >= 3) {                                      \
      profile_finish_timer(PROFILE_TIME__CYCLE_EXPLORE);                   \
    }
#else
#  define START_CYCLE_SEARCH()
#  define FINISH_CYCLE_SEARCH()
#endif


#define TRANS_BETWEEN_DIFF_SCCs(W, U, T)  (worker_use_opt_scc(W) && \
                                           (state_scc_id(U) != state_scc_id(T)))
#define STATE_PROP_SCC_N(W, S)            (worker_use_opt_scc(W) && \
                                           (state_scc_id(S) == SCC_TYPE_NON_ACCEPT))


/* DFSにより状態seedから状態goalへのパスをVector pathに積む.
 * ただし, 既にon_cycle_flagが立っている状態はpathに含まない. */
static BOOL state_to_state_path(State      *seed,
                                State      *goal,
                                Vector     *search,
                                Vector     *path,
                                st_table_t traversed)
{
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
    }
    else {
      st_add_direct(traversed, (st_data_t)s, (st_data_t)s);
      vec_push(path, (vec_data_t)s);

      MC_DEBUG(
        if (!mc_vec_states_valid(path)) {
          state_space_dumper(workerpool_get_my_worker()->states, stdout);
          fprintf(stdout, "PATH:\n");
          mc_print_vec_states(stdout, path, NULL);
          fprintf(stdout, "SEARCH:\n");
          mc_print_vec_states(stdout, search, NULL);
          lmn_fatal("unexpected.");
        }
      );

      for (i = 0; i < state_succ_num(s); i++) {
        State *succ = state_succ_state(s, i);

        if (is_on_cycle(succ) || !is_expanded(s)) continue;
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
static void ndfs_found_accepting_cycle(LmnWorker *w, State *seed, Vector *cycle_path);

typedef struct McSearchNDFS McSearchNDFS;
struct McSearchNDFS {
  Vector *open;
  Vector *path;
};

void ndfs_worker_init(LmnWorker *w)
{
  McSearchNDFS *mc = LMN_MALLOC(McSearchNDFS);
  mc->open = vec_make(1024);
  mc->path = vec_make(512);
  WORKER_SET_OBJ_SUB(w, mc);
}

#define W_NDFS_OBJ(w)       ((McSearchNDFS *)WORKER_OBJ_SUB(w))
#define NDFS_OPEN(MC)       (((McSearchNDFS *)(MC))->open)
#define NDFS_PATH(MC)       (((McSearchNDFS *)(MC))->path)
#define NDFS_OBJ_CLEAR(MC) do { \
  vec_clear(NDFS_OPEN(MC)); \
  vec_clear(NDFS_PATH(MC)); \
} while (0)


void ndfs_worker_finalize(LmnWorker *w)
{
  McSearchNDFS *mc = W_NDFS_OBJ(w);
  vec_free(NDFS_OPEN(mc));
  vec_free(NDFS_PATH(mc));
  LMN_FREE(mc);
}

void ndfs_env_set(LmnWorker *w)
{
  worker_set_ndfs(w);
  w->init2     = ndfs_worker_init;
  w->finalize2 = ndfs_worker_finalize;
}


/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に, 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void ndfs_start(LmnWorker *w, State *seed)
{
  McSearchNDFS *mc;
  BOOL has_error;

  START_CYCLE_SEARCH();

  mc = W_NDFS_OBJ(w);
  has_error = FALSE;
  vec_push(NDFS_OPEN(mc), (vec_data_t)seed);
  has_error = ndfs_loop(seed, NDFS_OPEN(mc), NDFS_PATH(mc));

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    ndfs_found_accepting_cycle(w, seed, NDFS_PATH(mc));
  }

  NDFS_OBJ_CLEAR(mc);
}


void ndfs_found_accepting_cycle(LmnWorker *w, State *seed, Vector *cycle_path)
{
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  mc_data.error_exist = TRUE;

  gen_counter_example = lmn_env.dump;
  set_on_cycle(seed); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
    set_on_cycle(s);

    if (gen_counter_example) vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(v);
  } else if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }
}


static BOOL ndfs_loop(State  *seed,
                      Vector *search,
                      Vector *path)
{
  while(vec_num(search) > 0) {
    State *s = (State *)vec_peek(search);

    if (is_snd(s)) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)vec_pop(search);
      if (vec_num(path) > 0 && (State *)vec_peek(path) == s_pop) {
        vec_pop(path);
      }
    }
    else {
      unsigned int i;
      vec_push(path, (vec_data_t)s);
      MC_DEBUG(
        if (!mc_vec_states_valid(path)) {
          state_space_dumper(workerpool_get_my_worker()->states, stdout);
          fprintf(stdout, "PATH:\n");
          mc_print_vec_states(stdout, path, NULL);
          fprintf(stdout, "SEARCH:\n");
          mc_print_vec_states(stdout, search, NULL);
          lmn_fatal("unexpected.");
        }
      );
      set_snd(s);
      for (i = 0; i < state_succ_num(s); i++) {
        State *succ = state_succ_state(s, i);

        if (is_on_cycle(succ)) {
          return FALSE;
        } else if (!is_expanded(succ)) {
          continue;
        } else if (succ == seed /* || is_on_stack(succ) */) {
          return TRUE; /* 同一のseedから探索する閉路が1つ見つかったならば探索を打ち切る */
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
  Queue  *accepts1;
  Queue  *accepts2;
  unsigned long old;
  unsigned long iteration;
  st_table_t traversed; /* 反例生成用 */
};

#define W_OWCTY_OBJ(W)     ((McSearchOWCTY *)WORKER_OBJ_SUB(W))
#define W_OWCTY_ACC_Q1(W)  (W_OWCTY_OBJ(W)->accepts1)
#define W_OWCTY_ACC_Q2(W)  (W_OWCTY_OBJ(W)->accepts2)
#define W_OWCTY_HASHSET(W) (W_OWCTY_OBJ(W)->traversed)

inline static void owcty_reachability(LmnWorker *w,
                                      Queue *primary,
                                      Queue *secondary,
                                      BOOL  set_flag,
                                      BOOL  is_up);
inline static BOOL owcty_traversed_owner_is_me(State *succ,
                                               BOOL  set_flag,
                                               BOOL  is_up);
inline static void owcty_report_midterm(LmnWorker *w);
inline static void owcty_termination_detection(LmnWorker *w);
static void owcty_found_accepting_cycle(LmnWorker *w);


void owcty_worker_init(LmnWorker *w)
{
  McSearchOWCTY *mc = LMN_MALLOC(McSearchOWCTY);

  /* 全ワーカでオブジェクトを共有 */

  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    mc->accepts1 = lmn_env.core_num > 1 ? make_parallel_queue(LMN_Q_MRMW)
                                        : new_queue();
    mc->accepts2 = lmn_env.core_num > 1 ? make_parallel_queue(LMN_Q_MRMW)
                                        : new_queue();
  } else {
    LmnWorker *w = workerpool_get_worker(WORKER_PRIMARY_ID);
    mc->accepts1 = W_OWCTY_ACC_Q1(w);
    mc->accepts2 = W_OWCTY_ACC_Q2(w);
  }

  mc->traversed = st_init_ptrtable();
  mc->old       = 0;
  mc->iteration = 0;

  WORKER_SET_OBJ_SUB(w, mc);
}


void owcty_worker_finalize(LmnWorker *w)
{
  McSearchOWCTY *mc = W_OWCTY_OBJ(w);
  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    q_free(mc->accepts1);
    q_free(mc->accepts2);
  }
  st_free_table(mc->traversed);
  LMN_FREE(mc);
}


void owcty_env_set(LmnWorker *w)
{
  worker_set_owcty(w);

  w->init2     = owcty_worker_init;
  w->finalize2 = owcty_worker_finalize;

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }

  if (lmn_env.enable_map_heuristic) {
    worker_set_weak_map(w);
    worker_set_map(w);
  }
}


static void owcty_env_init(LmnWorker *w)
{
  McSearchOWCTY *mc = W_OWCTY_OBJ(w);
  state_table_to_state_queue(state_space_accept_tbl(WORKER_STATESPACE(w)),
                             mc->accepts1);
  state_table_to_state_queue(state_space_accept_memid_tbl(WORKER_STATESPACE(w)),
                             mc->accepts1);

  MC_DEBUG(printf("acceptance queue init, num=%lu\n"
                            , queue_entry_num(mc->accepts1)));
}


void owcty_start(LmnWorker *w)
{
  McSearchOWCTY *mc;

  mc = W_OWCTY_OBJ(w);
  lmn_workers_synchronization(w, WORKER_PRIMARY_ID, owcty_env_init);

  while (!mc_data.mc_exit) {
    START_CYCLE_SEARCH();

    /* OWCTYはReachabilityの結果から不動点への到達を判定するため,
     * reachability2回の実行を1セットとして考える.
     * 1st Reachabilityで訪問した頂点にFOR_MC_MASKフラグをTRUEにし,
     *     訪問した受理頂点を起点に2nd Reachabilityを行う
     * 2nd Reachabilityで訪問した頂点にはFOR_MC_MASKフラグが既にTRUEになっているため
     *     フラグをFALSEにしていく. */

    owcty_reachability(w, mc->accepts1, mc->accepts2, FOR_MC_MASK, TRUE);
    lmn_workers_synchronization(w, WORKER_PRIMARY_ID, owcty_report_midterm);
    owcty_reachability(w, mc->accepts2, mc->accepts1, FOR_MC_MASK, FALSE);
    lmn_workers_synchronization(w, WORKER_PRIMARY_ID, owcty_termination_detection);

    FINISH_CYCLE_SEARCH();
  }

  if (!is_empty_queue(mc->accepts1)) {
    owcty_found_accepting_cycle(w);
  }
}


inline static void owcty_report_midterm(LmnWorker *w)
{
  McSearchOWCTY *mc = W_OWCTY_OBJ(w);
  mc->old = queue_entry_num(mc->accepts2);
}


/* owctyアルゴリズムの停止を判定した場合, mc_data.mc_exitフラグを真にする.
 * (owctyはon-the-flyアルゴリズムではないため,
 *  generator側の終了検知ではなく, 専用の終了検知を用意した.) */
inline static void owcty_termination_detection(LmnWorker *w)
{
  McSearchOWCTY *mc;
  unsigned long q_num;

  mc = W_OWCTY_OBJ(w);

  mc->iteration++;
  MC_DEBUG(fprintf(stderr, "iter%3lu[S=%10lu, old=%10lu]  %s"
                         , mc->iteration
                         , queue_entry_num(mc->accepts1)
                         , mc->old
                         , (mc->iteration % 5 == 0) ? "\n" : ""));

  q_num = queue_entry_num(mc->accepts1);
  if (q_num == 0 || q_num == mc->old) {
    MC_DEBUG(fprintf(stderr, "\n"));
    mc_data.mc_exit = TRUE;
  }
}


/* primary Queueに積まれた頂点を起点に到達可能な受理頂点の集合を
 * secondary QueueがNULLでなければsecondary Queueに積む.
 * 訪問した頂点は,
 *   is_upが真ならset_flagを真に
 *   is_upが偽ならset_flagを偽に
 * 設定する */
inline static void owcty_reachability(LmnWorker *w,
                                      Queue *primary,
                                      Queue *secondary,
                                      BOOL  set_flag,
                                      BOOL  is_up)
{
  while (!is_empty_queue(primary)) {
    State *s;
    unsigned int i;

    s = (State *)dequeue(primary);
    if (!s || STATE_PROP_SCC_N(w, s)) {
      continue;
    }

    for (i = 0; i < state_succ_num(s); i++) {
      State *succ = state_succ_state(s, i);

      if (TRANS_BETWEEN_DIFF_SCCs(w, s, succ)) continue;

      if (!owcty_traversed_owner_is_me(succ, set_flag, is_up)) {
        if (secondary && state_is_accept(succ)) {
          enqueue(secondary, (LmnWord)succ);
        } else {
          enqueue(primary, (LmnWord)succ);
        }
      }
    }
  }
}


/* 状態succのフラグset_flagの更新に成功した場合、FALSEを返す.
 * フラグは, is_upが真なら, 真に
 *           is_upが偽なら, 偽に
 * 設定する. */
inline static BOOL owcty_traversed_owner_is_me(State *succ,
                                               BOOL  set_flag,
                                               BOOL  is_up)
{
  BOOL flags_fetch, flags_update;

  flags_fetch  = succ->flags;

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
static void owcty_found_accepting_cycle(LmnWorker *w)
{
  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    mc_data.error_exist = TRUE;

    if (lmn_env.dump) {
      McSearchOWCTY *mc;
      Vector search, path;

      mc = W_OWCTY_OBJ(w);
      vec_init(&search, 64);
      vec_init(&path, 32);

      while (!is_empty_queue(mc->accepts1)) {
        State *seed = (State *)dequeue(mc->accepts1);

        if (state_is_end(seed)) {
          mc_found_invalid_state(seed);
        }
        else if (!is_on_cycle(seed)) {
          if (state_to_state_path(seed, seed, &search, &path, W_OWCTY_HASHSET(w))) {
            Vector *v;
            unsigned int i;

            v = vec_make(vec_num(&path));
            for (i = 0; i < vec_num(&path); i++) {
              State *tmp = (State *)vec_get(&path, i);
              set_on_cycle(tmp);
              vec_push(v, (vec_data_t)tmp);
            }

            mc_found_invalid_path(v);
          }
          st_clear(W_OWCTY_HASHSET(w));
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

#define W_MAP_OBJ(W)           ((McSearchMAP *)WORKER_OBJ_SUB(W))
#define W_MAP_PROPAG_G(W)      (W_MAP_OBJ(W)->propagate)
#define W_MAP_DEL_G(W)         (W_MAP_OBJ(W)->waitingSeed)
#define W_MAP_HASHSET(W)       (W_MAP_OBJ(W)->traversed)

State *map_ordering_states(unsigned int num, ...);
inline static State *map_ordering(State *s1, State *s2);
inline static BOOL map_entry_state(State *t, State *propag);
static void map_found_accepting_cycle(LmnWorker *w, State *s);
static void map_propagate(LmnWorker *w, State *s, State *t, State *propag);
static State *map_ordering_propagate_state(LmnWorker *w, State *u);


void map_worker_init(LmnWorker *w)
{
  McSearchMAP *mc = LMN_MALLOC(McSearchMAP);

  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    mc->propagate   = lmn_env.core_num > 1 ? make_parallel_queue(LMN_Q_MRMW)
                                           : new_queue();
    mc->waitingSeed = lmn_env.core_num > 1 ? make_parallel_queue(LMN_Q_MRMW)
                                           : new_queue();
  } else {
    LmnWorker *w = workerpool_get_worker(WORKER_PRIMARY_ID);
    mc->propagate   = W_MAP_PROPAG_G(w);
    mc->waitingSeed = W_MAP_DEL_G(w);
  }

  mc->traversed = st_init_ptrtable();

  WORKER_SET_OBJ_SUB(w, mc);
}


void map_worker_finalize(LmnWorker *w)
{
  McSearchMAP *mc = W_MAP_OBJ(w);
  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    q_free(mc->propagate);
    q_free(mc->waitingSeed);
  }
  st_free_table(mc->traversed);
  LMN_FREE(mc);
}


void map_env_set(LmnWorker *w)
{
  worker_set_map(w);

  w->init2     = map_worker_init;
  w->finalize2 = map_worker_finalize;

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }
}


/* 頂点uからuのサクセッサにMAPを伝搬する.
 * MAPを更新できる限り, MAPの再伝搬を行う */
void map_start(LmnWorker *w, State *u)
{
  START_CYCLE_SEARCH();

  do {
    State *propag;
    unsigned int i;

    propag = map_ordering_propagate_state(w, u);

    for (i = 0; i < state_succ_num(u); i++) {
      State *t = state_succ_state(u, i);
      map_propagate(w, u, t, propag);
    }

    u = worker_use_weak_map(w) ? NULL : (State *)dequeue(W_MAP_PROPAG_G(w));

  } while (u);


  FINISH_CYCLE_SEARCH();
}


/* MAPアルゴリズムの2週目以降.
 * on-the-flyなMAPの伝搬でDELと判定した頂点を起点に
 * MAP値の再伝搬を行う */
void map_iteration_start(LmnWorker *w)
{
  lmn_workers_synchronization(w, WORKER_PRIMARY_ID, NULL);

  while (!is_empty_queue(W_MAP_DEL_G(w))) {
    State *seed = (State *)dequeue(W_MAP_DEL_G(w));
    if (seed && !smap_is_not_delete(seed)) {
      smap_set_deleted(seed);
      map_start(w, seed);
    }
  }
}


/* 状態uからサクセサへ伝搬するMAP値を決定し, 返す.
 * MAP値は, 伝搬元となる状態uと, uに伝搬されて設定されているMAP値が指す状態とで決定する */
static State *map_ordering_propagate_state(LmnWorker *w, State *u)
{
  State *propag;

  if (!state_is_accept(u) || smap_is_deleted(u)) {
    propag = state_map(u);
  }
  else { /* u ∈ A */
    propag = map_ordering(u, state_map(u));

    if (!worker_use_weak_map(w)) {
      if (propag == u) {
        smap_unset_not_delete(u);
        enqueue(W_MAP_DEL_G(w), (LmnWord)u);
      } else {
        smap_set_not_delete(u);
        smap_unset_deleted(u);
      }
    }
  }

  return propag;
}


static void map_propagate(LmnWorker *w, State *s, State *t, State *propag)
{
  if (TRANS_BETWEEN_DIFF_SCCs(w, s, t)) return;

  if (propag == t || (state_is_accept(t) && t == s)) {
    map_found_accepting_cycle(w, t);
  }
  else if (propag == map_ordering(propag, state_map(t))) {
    if (map_entry_state(t, propag) && !worker_use_weak_map(w)
                                   && is_expanded(t)) {
      enqueue(W_MAP_PROPAG_G(w), (LmnWord)t);
    }
  }
}


/* 状態tのMAP値に, propagが優先するMAP値なら伝搬する.
 * 伝搬に成功した場合, 真を返す. */
inline static BOOL map_entry_state(State *t, State *propag)
{
  BOOL ret = FALSE;

  do {
    State *fetch = state_map(t);

    if (fetch == propag) {
      break;
    } else if (CAS(state_map(t), fetch, propag)) {
      ret = TRUE;
      break;
    } else if (propag != map_ordering(state_map(t), propag)) {
      ret = FALSE;
      break;
    }
  } while (1);

  return ret;
}


inline static State *map_ordering(State *s1, State *s2)
{
  BOOL s1_valid, s2_valid;

  /* MAP値を比較するべき頂点として有効なものは, 未削除の受理頂点のみ */
  s1_valid = s1 && state_is_accept(s1)
                && !smap_is_deleted(s1)
                && state_id(s1) != 0;
  s2_valid = s2 && state_is_accept(s2)
                && !smap_is_deleted(s2)
                && state_id(s2) != 0;

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
State *map_ordering_states(unsigned int num, ...)
{
  State *ptr, *ret;
  va_list states;
  unsigned int i;

  va_start(states, num);

  ret = NULL;
  for (i = 0; i < num; i++) {
    ptr = va_arg(states, State *);
    ret = map_ordering(ptr, ret);
  }

  va_end(states);

  return ret;
}



/* 受理サイクルを発見した場合に呼び出す.
 * 反例出力が必要な場合は, 起点となる頂点と同じMAP値の状態を辿るように
 * self-reachability testを行う. */
static void map_found_accepting_cycle(LmnWorker *w, State *s)
{
  mc_data.error_exist = TRUE;

  if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }

  if (lmn_env.dump && !is_on_cycle(s)) {
    /* TODO: とりあえず単純なself reachability testで実装しているので,
     *        MAP値の等しい状態だけを辿るようにしたself reachabilityに直す */
    Vector search, path;
    st_table_t traversed;

    vec_init(&search, 64);
    vec_init(&path, 32);
    traversed = !worker_use_weak_map(w) ? W_MAP_HASHSET(w)
                                        : st_init_ptrtable();
    if (state_to_state_path(s, s, &search, &path, traversed)) {
      Vector *v;
      unsigned int i;

      v = vec_make(vec_num(&path));
      for (i = 0; i < vec_num(&path); i++) {
        State *tmp = (State *)vec_get(&path, i);
        set_on_cycle(tmp);
        vec_push(v, (vec_data_t)tmp);
      }

      mc_found_invalid_path(v);
    }

    if (!worker_use_weak_map(w)) {
      st_clear(W_MAP_HASHSET(w));
    } else {
      st_free_table(traversed);
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

#define W_BLE_OBJ(W)             ((McSearchBLE *)WORKER_OBJ_SUB(W))
#define W_BLE_LAYER_Q(W)         (W_BLE_OBJ(W)->layer)
#define W_BLE_PATH_V(W)          (W_BLE_OBJ(W)->path)
#define W_BLE_SEARCH_V(W)        (W_BLE_OBJ(W)->search)
#define W_BLE_HASHSET(W)         (W_BLE_OBJ(W)->traversed)


static void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path);

void bledge_env_set(LmnWorker *w)
{
  worker_set_ble(w);
  w->init2     = bledge_worker_init;
  w->finalize2 = bledge_worker_finalize;

  if (lmn_env.prop_scc_driven) {
    worker_set_opt_scc(w);
  }
}


void bledge_worker_init(LmnWorker *w)
{
  McSearchBLE *mc = LMN_MALLOC(McSearchBLE);

  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    mc->layer = lmn_env.core_num ? make_parallel_queue(LMN_Q_MRMW)
                                 : new_queue();
  } else {
    mc->layer = W_BLE_LAYER_Q(workerpool_get_worker(WORKER_PRIMARY_ID));
  }

  mc->path = vec_make(32);
  mc->search = vec_make(64);
  mc->traversed = st_init_ptrtable();

  WORKER_SET_OBJ_SUB(w, mc);
}


void bledge_worker_finalize(LmnWorker *w)
{
  McSearchBLE *mc = W_BLE_OBJ(w);
  if (WORKER_ID(w) == WORKER_PRIMARY_ID) {
    q_free(mc->layer);
  }
  vec_free(mc->path);
  vec_free(mc->search);
  st_free_table(mc->traversed);
}


/* State Vectorの各状態に受理頂点が含まれていたら真を返す */
static BOOL bledge_path_accepting(Vector *v)
{
  unsigned int i;

  for (i = 0; i < vec_num(v); i++) {
    State *t = (State *)vec_get(v, i);
    if (state_is_accept(t)) return TRUE;
  }

  return FALSE;
}


static BOOL bledge_explorer_accepting_cycle(LmnWorker *w, State *u, State *v)
{
  Vector *v1, *v2;
  v1 = W_BLE_SEARCH_V(w);
  v2 = W_BLE_PATH_V(w);
  return state_to_state_path(v, u, v1, v2, W_BLE_HASHSET(w))
       && bledge_path_accepting(v2);
}


void bledge_start(LmnWorker *w)
{
  lmn_workers_synchronization(w, WORKER_PRIMARY_ID, NULL);

  START_CYCLE_SEARCH();

  while (!is_empty_queue(W_BLE_LAYER_Q(w))) {
    State *u;
    unsigned int i;

    if (!(u = (State *)dequeue(W_BLE_LAYER_Q(w)))) continue;
    for (i = 0; i < state_succ_num(u); i++) {
      State *v = state_succ_state(u, i);

      if (is_expanded(v)) { /* detected back level edge:t where [u]--t-->[v] */
        if (!is_on_cycle(u) && !is_on_cycle(v) &&
            bledge_explorer_accepting_cycle(w, u, v)) {
          vec_push(W_BLE_PATH_V(w), (vec_data_t)u);
          bledge_found_accepting_cycle(w, W_BLE_PATH_V(w));
        }

        st_clear(W_BLE_HASHSET(w));
        vec_clear(W_BLE_PATH_V(w));
        vec_clear(W_BLE_SEARCH_V(w));
      }
    }
  }

  FINISH_CYCLE_SEARCH();
}


void bledge_store_layer(LmnWorker *w, State *s)
{
  if (state_succ_num(s) > 0) {
    enqueue(W_BLE_LAYER_Q(w), (LmnWord)s);
  }
}


static void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path)
{
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  mc_data.error_exist = TRUE;

  gen_counter_example = lmn_env.dump;
  v = gen_counter_example ? vec_make(vec_num(cycle_path)) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < vec_num(cycle_path); i++) {
    State *s = (State *)vec_get(cycle_path, i);
    set_on_cycle(s);
    if (gen_counter_example) vec_push(v, (vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(v);
  } else if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }
}


/** ==================================
 *  === MultiCore Nested-DFS ========
 *  ==================================
 */


