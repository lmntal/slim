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
#include "state_table.hpp"

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
  search->push((vec_data_t)seed);

  while (search->get_num() > 0) {
    State *s;
    unsigned int i;

    s = (State *)search->peek();
    if (st_contains(traversed, (st_data_t)s)) {
      State *s_pop = (State *)search->pop();
      if (path->get_num() > 0 && (State *)path->peek() == s_pop) {
        path->pop();
      }
    } else {
      st_add_direct(traversed, (st_data_t)s, (st_data_t)s);
      path->push((vec_data_t)s);

      for (i = 0; i < s->successor_num; i++) {
        State *succ = state_succ_state(s, i);

        if (succ->is_on_cycle() || !s->is_expanded())
          continue;
        else if (succ == goal) {
          return TRUE;
        } else {
          search->push((vec_data_t)succ);
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

#define NDFS_WORKER_OBJ(W)                                                     \
  ((slim::verifier::tactics::NDFS *)&worker_explorer(W))
#define NDFS_WORKER_OPEN_VEC(W) (NDFS_WORKER_OBJ(W)->open)
#define NDFS_WORKER_PATH_VEC(W) (NDFS_WORKER_OBJ(W)->path)
#define NDFS_WORKER_OBJ_CLEAR(W)                                               \
  do {                                                                         \
    NDFS_WORKER_OPEN_VEC(W)->clear();                                          \
    NDFS_WORKER_PATH_VEC(W)->clear();                                          \
  } while (0)

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void ndfs_start(LmnWorker *w, State *seed) {
  auto t = (slim::verifier::tactics::NDFS *)w->strategy.explorer.get();
  t->start(seed);
}

namespace slim {
namespace verifier {
namespace tactics {
void NDFS::initialize(LmnWorker *w) {
  this->open = new Vector(1024);
  this->path = new Vector(512);
}
void NDFS::finalize(LmnWorker *w) {
  delete NDFS_WORKER_OPEN_VEC(w);
  delete NDFS_WORKER_PATH_VEC(w);
}
void NDFS::start(State *seed) {
  auto w = this->owner;
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  NDFS_WORKER_OPEN_VEC(w)->push((vec_data_t)seed);
  has_error = ndfs_loop(seed, NDFS_WORKER_OPEN_VEC(w), NDFS_WORKER_PATH_VEC(w));

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    ndfs_found_accepting_cycle(w, seed, NDFS_WORKER_PATH_VEC(w));
  }

  NDFS_WORKER_OBJ_CLEAR(w);
}

void NDFS::ndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                      Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  wp->workers_found_error();

  gen_counter_example = lmn_env.dump;
  seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? new Vector(cycle_path->get_num()) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < cycle_path->get_num(); i++) {
    State *s = (State *)cycle_path->get(i);
    s->set_on_cycle();

    if (gen_counter_example)
      v->push((vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}

BOOL NDFS::ndfs_loop(State *seed, Vector *search, Vector *path) {
  while (search->get_num() > 0) {
    State *s = (State *)search->peek();

    if (s->is_snd()) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)search->pop();
      if (path->get_num() > 0 && (State *)path->peek() == s_pop) {
        path->pop();
      }
    } else {
      unsigned int i;
      path->push((vec_data_t)s);
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
          search->push((vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}

} // namespace tactics
} // namespace verifier
} // namespace slim

/** ==================================
 *  === One Way Catch Them Young =====
 *  ==================================
 */

#define OWCTY_WORKER_OBJ(W)                                                    \
  ((slim::verifier::tactics::OWCTY *)&worker_explorer(W))
#define OWCTY_WORKER_AQ1(W) (OWCTY_WORKER_OBJ(W)->accepts1)
#define OWCTY_WORKER_AQ2(W) (OWCTY_WORKER_OBJ(W)->accepts2)
#define OWCTY_WORKER_HASHSET(W) (OWCTY_WORKER_OBJ(W)->traversed)

void owcty_worker_init(LmnWorker *w) {
  auto t = (slim::verifier::tactics::OWCTY *)w->strategy.explorer.get();
  t->initialize(w);
}

void owcty_worker_finalize(LmnWorker *w) {
  auto t = (slim::verifier::tactics::OWCTY *)w->strategy.explorer.get();
  t->finalize(w);
}

// struct DegreeCnt {
//   unsigned int in, out;
//   LmnWord *ins, *outs;
// };

void owcty_start(LmnWorker *w) {
  auto t = (slim::verifier::tactics::OWCTY *)w->strategy.explorer.get();
  t->start(w);
}

namespace slim {
namespace verifier {
namespace tactics {

void OWCTY::initialize(LmnWorker *w) {
  /* 全ワーカでオブジェクトを共有 */

  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (worker_group(w)->workers_get_entried_num() > 1) {
      this->accepts1 = new Queue(element::concurrent_mode::multi_reader_multi_writer);
      this->accepts2 = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    } else {
      this->accepts1 = new Queue();
      this->accepts2 = new Queue();
    }
  } else {
    LmnWorker *primary = worker_group(w)->get_worker(LMN_PRIMARY_ID);
    this->accepts1 = OWCTY_WORKER_AQ1(primary);
    this->accepts2 = OWCTY_WORKER_AQ2(primary);
  }

  this->traversed = st_init_ptrtable();
  this->old = 0;
  this->iteration = 0;
}

void OWCTY::finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    delete OWCTY_WORKER_AQ1(w);
    delete OWCTY_WORKER_AQ2(w);
  }
  st_free_table(OWCTY_WORKER_HASHSET(w));
}

void OWCTY::start(LmnWorker *w) {
  /* st_tableにも, elock, wlockを組込む. */
  if (worker_id(w) == LMN_PRIMARY_ID) {
  }

  /* 1枚のハッシュ表.
   * スレッド固有Queue (SRMW)
   * state id partition
   * 受信したら入力変数を増やす */

  lmn_workers_synchronization(w, owcty_env_init);

  while (!worker_group(w)->workers_are_exit()) {
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

  if (!OWCTY_WORKER_AQ1(w)->is_empty()) {
    owcty_found_accepting_cycle(w, worker_states(w)->automata());
  }
}

void OWCTY::owcty_report_midterm(LmnWorker *w) {
  auto mc = OWCTY_WORKER_OBJ(w);
  mc->old = mc->accepts2->entry_num();
}

/* owctyアルゴリズムの停止を判定した場合, mc_exitフラグを真にする.
 * (owctyはon-the-flyアルゴリズムではないため,
 *  generator側の終了検知ではなく, 専用の終了検知を用意した.) */
void OWCTY::owcty_termination_detection(LmnWorker *w) {
  unsigned long q_num;

  auto mc = OWCTY_WORKER_OBJ(w);
  mc->iteration++;
  MC_DEBUG(fprintf(stderr, "iter%3lu[S=%10lu, old=%10lu]  %s", mc->iteration,
                   mc->accepts1->entry_num(), mc->old,
                   (mc->iteration % 3 == 0) ? "\n" : ""));

  q_num = mc->accepts1->entry_num();
  if (q_num == 0 || q_num == mc->old) {
    MC_DEBUG(fprintf(stderr, "\n"));
    worker_group(w)->workers_set_exit();
  }
}

void OWCTY::owcty_env_init(LmnWorker *w) {
  for (auto &ptr : worker_states(w)->accept_tbl())
    OWCTY_WORKER_AQ1(w)->enqueue((LmnWord)ptr);
  for (auto &ptr : worker_states(w)->accept_memid_tbl())
    OWCTY_WORKER_AQ2(w)->enqueue((LmnWord)ptr);

  MC_DEBUG(printf("acceptance queue init, num=%lu\n",
                  OWCTY_WORKER_AQ1(w)->entry_num()));
}

/* primary Queueに積まれた頂点を起点に到達可能な受理頂点の集合を
 * secondary QueueがNULLでなければsecondary Queueに積む.
 * 訪問した頂点は,
 *   is_upが真ならset_flagを真に
 *   is_upが偽ならset_flagを偽に
 * 設定する */
void OWCTY::owcty_reachability(LmnWorker *w, Queue *primary, Queue *secondary,
                               BOOL set_flag, BOOL is_up) {
  StateSpaceRef ss = worker_states(w);
  while (!primary->is_empty()) {
    State *s;
    unsigned int i, cnt;

    s = (State *)primary->dequeue();
    if (!s) {
      continue;
    } else if (STATE_PROP_SCC_N(w, s, ss->automata()) || smap_is_deleted(s) ||
               (MAP_COND(w) && !s->map)) {
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

      if (TRANS_BETWEEN_DIFF_SCCs(w, s, succ, ss->automata()))
        continue;

      if (!owcty_traversed_owner_is_me(succ, set_flag, is_up)) {
        if (state_is_accept(ss->automata(), succ) && !smap_is_deleted(succ)) {
          secondary->enqueue((LmnWord)succ);
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
BOOL OWCTY::owcty_traversed_owner_is_me(State *succ, BOOL set_flag,
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
void OWCTY::owcty_found_accepting_cycle(LmnWorker *w, AutomataRef a) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    LmnWorkerGroup *wp = worker_group(w);
    wp->workers_found_error();

    if (lmn_env.dump) {
      Vector search, path;

      search.init(64);
      path.init(32);
      while (!OWCTY_WORKER_AQ1(w)->is_empty()) {
        State *seed = (State *)OWCTY_WORKER_AQ1(w)->dequeue();

        if (state_is_end(a, seed)) {
          mc_found_invalid_state(wp, seed);
        } else if (!seed->is_on_cycle()) {
          if (state_to_state_path(seed, seed, &search, &path,
                                  OWCTY_WORKER_HASHSET(w))) {
            Vector *v;
            unsigned int i;

            v = new Vector(path.get_num());
            for (i = 0; i < path.get_num(); i++) {
              State *tmp = (State *)path.get(i);
              tmp->set_on_cycle();
              v->push((vec_data_t)tmp);
            }

            mc_found_invalid_path(wp, v);
          }
          st_clear(OWCTY_WORKER_HASHSET(w));
          search.clear();
          path.clear();
        }
      }

      search.destroy();
      path.destroy();
    }
  }
}

} // namespace tactics
} // namespace verifier
} // namespace slim

/** =======================================
 *  === Maximal Accepting Predecessors ====
 *  =======================================
 */
#include <stdarg.h>

#define MAP_WORKER_OBJ(W) ((slim::verifier::tactics::MAP *)&worker_explorer(W))
#define MAP_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define MAP_WORKER_PROPAG_G(W) (MAP_WORKER_OBJ(W)->propagate)
#define MAP_WORKER_DEL_G(W) (MAP_WORKER_OBJ(W)->waitingSeed)
#define MAP_WORKER_HASHSET(W) (MAP_WORKER_OBJ(W)->traversed)

/* 頂点uからuのサクセッサにMAPを伝搬する.
 * MAPを更新できる限り, MAPの再伝搬を行う */
void map_start(LmnWorker *w, State *u) {
  auto t = (slim::verifier::tactics::MAP *)w->strategy.explorer.get();
  t->start(u);
}

/* MAPアルゴリズムの2週目以降.
 * on-the-flyなMAPの伝搬でDELと判定した頂点を起点に
 * MAP値の再伝搬を行う */
void map_iteration_start(LmnWorker *w) {
  lmn_workers_synchronization(w, NULL);

  while (!MAP_WORKER_DEL_G(w)->is_empty()) {
    State *seed = (State *)MAP_WORKER_DEL_G(w)->dequeue();
    if (seed && !smap_is_not_delete(seed)) {
      smap_set_deleted(seed);
      map_start(w, seed);
    }
  }
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

namespace slim {
namespace verifier {
namespace tactics {

void MAP::initialize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (worker_group(w)->workers_get_entried_num() > 1) {
      this->propagate = new Queue(element::concurrent_mode::multi_reader_multi_writer);
      this->waitingSeed = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    } else {
      this->propagate = new Queue();
      this->waitingSeed = new Queue();
    }
  } else {
    LmnWorker *prim = worker_group(w)->get_worker(LMN_PRIMARY_ID);
    this->propagate = MAP_WORKER_PROPAG_G(prim);
    this->waitingSeed = MAP_WORKER_DEL_G(prim);
  }

  this->traversed = st_init_ptrtable();
}
void MAP::finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    delete MAP_WORKER_PROPAG_G(w);
    delete MAP_WORKER_DEL_G(w);
  }
  st_free_table(MAP_WORKER_HASHSET(w));
}

void MAP::start(State *u) {
  AutomataRef a;

  START_CYCLE_SEARCH();

  if (u->successor_num == 0) {
    backward_elimination(this->owner, u);
  }

  a = worker_states(this->owner)->automata();

  do {
    State *propag;
    unsigned int i;

    propag = map_ordering_propagate_state(this->owner, u, a);
    for (i = 0; i < u->successor_num; i++) {
      State *t = state_succ_state(u, i);
      map_propagate(this->owner, u, t, propag, a);
    }

    if (worker_use_weak_map(this->owner)) {
      u = NULL;
    } else {
      u = (State *)MAP_WORKER_PROPAG_G(this->owner)->dequeue();
    }
  } while (u);

  FINISH_CYCLE_SEARCH();
}

/* 状態uからサクセサへ伝搬するMAP値を決定し, 返す.
 * MAP値は, 伝搬元となる状態uと,
 * uに伝搬されて設定されているMAP値が指す状態とで決定する */
State *MAP::map_ordering_propagate_state(LmnWorker *w, State *u,
                                         AutomataRef a) {
  State *propag;

  if (!state_is_accept(a, u) || smap_is_deleted(u)) {
    propag = u->map;
  } else { /* u ∈ A */
    propag = map_ordering(u, u->map, a);

    if (!worker_use_weak_map(w)) {
      if (propag == u) {
        smap_unset_not_delete(u);
        MAP_WORKER_DEL_G(w)->enqueue((LmnWord)u);
      } else {
        smap_set_not_delete(u);
        smap_unset_deleted(u);
      }
    }
  }

  return propag;
}

void MAP::map_propagate(LmnWorker *w, State *s, State *t, State *propag,
                        AutomataRef a) {
  if (TRANS_BETWEEN_DIFF_SCCs(w, s, t, a))
    return;

  if (propag == t || (state_is_accept(a, t) && t == s)) {
    map_found_accepting_cycle(w, t);
  } else if (propag == map_ordering(propag, t->map, a)) {
    if (map_entry_state(t, propag, a) && !worker_use_weak_map(w) &&
        t->is_expanded()) {
      MAP_WORKER_PROPAG_G(w)->enqueue((LmnWord)t);
    }
  }
}

/* 状態tのMAP値に, propagが優先するMAP値なら伝搬する.
 * 伝搬に成功した場合, 真を返す. */
BOOL MAP::map_entry_state(State *t, State *propag, AutomataRef a) {
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

State *MAP::map_ordering(State *s1, State *s2, AutomataRef a) {
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
State *MAP::map_ordering_states(AutomataRef a, unsigned int num, ...) {
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

/* 受理サイクルを発見した場合に呼び出す.
 * 反例出力が必要な場合は, 起点となる頂点と同じMAP値の状態を辿るように
 * self-reachability testを行う. */
void MAP::map_found_accepting_cycle(LmnWorker *w, State *s) {
  LmnWorkerGroup *wp = worker_group(w);

  wp->workers_found_error();
  if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }

  if (lmn_env.dump && !s->is_on_cycle()) {
    /* TODO: とりあえず単純なself reachability testで実装しているので,
     *        MAP値の等しい状態だけを辿るようにしたself reachabilityに直す */
    Vector search, path;
    st_table_t traversed;

    search.init(64);
    path.init(32);
    if (worker_use_weak_map(w)) {
      traversed = st_init_ptrtable();
    } else {
      traversed = MAP_WORKER_HASHSET(w);
    }

    if (state_to_state_path(s, s, &search, &path, traversed)) {
      Vector *v;
      unsigned int i;

      v = new Vector(path.get_num());
      for (i = 0; i < path.get_num(); i++) {
        State *tmp = (State *)path.get(i);
        tmp->set_on_cycle();
        v->push((vec_data_t)tmp);
      }
      mc_found_invalid_path(wp, v);
    }

    if (worker_use_weak_map(w)) {
      st_free_table(traversed);
    } else {
      st_clear(MAP_WORKER_HASHSET(w));
    }
    search.destroy();
    path.destroy();
  }
}

} // namespace tactics
} // namespace verifier
} // namespace slim

/** ==================================
 *  === Back Level Edges =============
 *  ==================================
 */

#define BLE_WORKER_OBJ(W) ((slim::verifier::tactics::BLE *)&worker_explorer(W))
#define BLE_WORKER_LAYER_Q(W) (BLE_WORKER_OBJ(W)->layer)
#define BLE_WORKER_PATH_VEC(W) (BLE_WORKER_OBJ(W)->path)
#define BLE_WORKER_SEARCH_VEC(W) (BLE_WORKER_OBJ(W)->search)
#define BLE_WORKER_HASHSET(W) (BLE_WORKER_OBJ(W)->traversed)

static void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path);

void bledge_start(LmnWorker *w) {
  auto t = (slim::verifier::tactics::BLE *)w->strategy.explorer.get();
  t->start();
}

void bledge_store_layer(LmnWorker *w, State *s) {
  auto t = (slim::verifier::tactics::BLE *)w->strategy.explorer.get();
  t->store_layer(w, s);
}

namespace slim {
namespace verifier {
namespace tactics {
void BLE::initialize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (worker_group(w)->workers_get_entried_num() > 1) {
      this->layer = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    } else {
      this->layer = new Queue();
    }
  } else {
    this->layer =
        BLE_WORKER_LAYER_Q(worker_group(w)->get_worker(LMN_PRIMARY_ID));
  }

  this->path = new Vector(32);
  this->search = new Vector(64);
  this->traversed = st_init_ptrtable();
}
void BLE::finalize(LmnWorker *w) {
  if (worker_id(w) == LMN_PRIMARY_ID) {
    delete BLE_WORKER_LAYER_Q(w);
  }
  delete BLE_WORKER_PATH_VEC(w);
  delete BLE_WORKER_SEARCH_VEC(w);
  st_free_table(BLE_WORKER_HASHSET(w));
}

void BLE::start() {
  lmn_workers_synchronization(this->owner, NULL);

  START_CYCLE_SEARCH();

  while (!BLE_WORKER_LAYER_Q(this->owner)->is_empty()) {
    State *u;
    unsigned int i;

    if (!(u = (State *)BLE_WORKER_LAYER_Q(this->owner)->dequeue()))
      continue;
    for (i = 0; i < u->successor_num; i++) {
      State *v = state_succ_state(u, i);

      if (v->is_expanded()) { /* detected back level edge:t where [u]--t-->[v]
                               */
        if (!u->is_on_cycle() && !v->is_on_cycle() &&
            bledge_explorer_accepting_cycle(this->owner, u, v)) {
          BLE_WORKER_PATH_VEC(this->owner)->push((vec_data_t)u);
          bledge_found_accepting_cycle(this->owner,
                                       BLE_WORKER_PATH_VEC(this->owner));
        }

        st_clear(BLE_WORKER_HASHSET(this->owner));
        BLE_WORKER_PATH_VEC(this->owner)->clear();
        BLE_WORKER_SEARCH_VEC(this->owner)->clear();
      }
    }
  }

  FINISH_CYCLE_SEARCH();
}

void BLE::store_layer(LmnWorker *w, State *s) {
  if (s->successor_num > 0) {
    BLE_WORKER_LAYER_Q(w)->enqueue((LmnWord)s);
  }
}

void BLE::bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  wp->workers_found_error();

  gen_counter_example = lmn_env.dump;
  v = gen_counter_example ? new Vector(cycle_path->get_num()) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < cycle_path->get_num(); i++) {
    State *s = (State *)cycle_path->get(i);
    s->set_on_cycle();
    if (gen_counter_example)
      v->push((vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}

BOOL BLE::bledge_explorer_accepting_cycle(LmnWorker *w, State *u, State *v) {
  AutomataRef a = worker_states(w)->automata();
  return state_to_state_path(v, u, BLE_WORKER_SEARCH_VEC(w),
                             BLE_WORKER_PATH_VEC(w), BLE_WORKER_HASHSET(w)) &&
         bledge_path_accepting(BLE_WORKER_PATH_VEC(w), a);
}

/* State Vectorの各状態に受理頂点が含まれていたら真を返す */
BOOL BLE::bledge_path_accepting(Vector *v, AutomataRef a) {
  unsigned int i;

  for (i = 0; i < v->get_num(); i++) {
    State *t = (State *)v->get(i);
    if (state_is_accept(a, t))
      return TRUE;
  }

  return FALSE;
}

} // namespace tactics
} // namespace verifier
} // namespace slim

/** ==================================
 *  === MAP + Nested-DFS ========
 *  ==================================
 */

/* MAP_WORKER_~
 * 系のマクロでアクセスするため、最初にMcSearchMapを持ってくる必要がある */
/* McSearchMapを変更したらこっちも変更する必要あり */
/* MAP系関数・マクロを使い回したかったので、こんな構造にしたが、色々と問題がありそうな構造なので要修正
 */

#define MAPNDFS_WORKER_OBJ(W)                                                  \
  ((slim::verifier::tactics::MAP_NDFS *)&worker_explorer(W))
#define MAPNDFS_WORKER_OBJ_SET(W, O) worker_explorer_obj_set(W, O)
#define MAPNDFS_WORKER_OPEN_VEC(W) (MAPNDFS_WORKER_OBJ(W)->open)
#define MAPNDFS_WORKER_PATH_VEC(W) (MAPNDFS_WORKER_OBJ(W)->path)
#define MAPNDFS_WORKER_OBJ_CLEAR(W)                                            \
  do {                                                                         \
    MAPNDFS_WORKER_OPEN_VEC(W)->clear();                                       \
    MAPNDFS_WORKER_PATH_VEC(W)->clear();                                       \
  } while (0)

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void mapndfs_start(LmnWorker *w, State *seed) {
  auto t = (slim::verifier::tactics::MAP_NDFS *)w->strategy.explorer.get();
  t->start(seed);
}

void mapndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                   Vector *cycle_path) {
  auto t = (slim::verifier::tactics::MAP_NDFS *)w->strategy.explorer.get();
  t->mapndfs_found_accepting_cycle(w, seed, cycle_path);
}

namespace slim {
namespace verifier {
namespace tactics {
void MAP_NDFS::initialize(LmnWorker *w) {
  this->open = new Vector(1024);
  this->path = new Vector(512);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (worker_group(w)->workers_get_entried_num() > 1) {
      this->propagate = new Queue(element::concurrent_mode::multi_reader_multi_writer);
      this->waitingSeed = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    } else {
      this->propagate = new Queue();
      this->waitingSeed = new Queue();
    }
  } else {
    LmnWorker *prim = worker_group(w)->get_worker(LMN_PRIMARY_ID);
    this->propagate = MAP_WORKER_PROPAG_G(prim);
    this->waitingSeed = MAP_WORKER_DEL_G(prim);
  }
  this->traversed = NULL;
#endif
}
void MAP_NDFS::finalize(LmnWorker *w) {
  delete MAPNDFS_WORKER_OPEN_VEC(w);
  delete MAPNDFS_WORKER_PATH_VEC(w);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    delete MAP_WORKER_PROPAG_G(w);
    delete MAP_WORKER_DEL_G(w);
  }
  if (MAP_WORKER_HASHSET(w))
    st_free_table(MAP_WORKER_HASHSET(w));
#endif
}

void MAP_NDFS::start(State *seed) {
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  MAPNDFS_WORKER_OPEN_VEC(this->owner)->push((vec_data_t)seed);
  has_error = mapndfs_loop(seed, MAPNDFS_WORKER_OPEN_VEC(this->owner),
                           MAPNDFS_WORKER_PATH_VEC(this->owner));

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    mapndfs_found_accepting_cycle(this->owner, seed,
                                  MAPNDFS_WORKER_PATH_VEC(this->owner));
  }

  MAPNDFS_WORKER_OBJ_CLEAR(this->owner);
}

BOOL MAP_NDFS::mapndfs_loop(State *seed, Vector *search, Vector *path) {
  while (search->get_num() > 0) {
    State *s = (State *)search->peek();

    if (s->is_snd()) { /* 訪問済み */
      /** DFS2 BackTracking */
      State *s_pop = (State *)search->pop();
      if (path->get_num() > 0 && (State *)path->peek() == s_pop) {
        path->pop();
      }
    } else {
      unsigned int i;
      path->push((vec_data_t)s);
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
          search->push((vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}

void MAP_NDFS::mapndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                             Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  wp->workers_found_error();

  gen_counter_example = lmn_env.dump;
  seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? new Vector(cycle_path->get_num()) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < cycle_path->get_num(); i++) {
    State *s = (State *)cycle_path->get(i);
    s->set_on_cycle();

    if (gen_counter_example)
      v->push((vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}
} // namespace tactics
} // namespace verifier
} // namespace slim

#ifndef MINIMAL_STATE
/** ==================================
 *  === Multicore Nested-DFS ========
 *  ==================================
 */

/* Nested-DFS, Double-DFS, Red-DFS:
 * 1段階目のDFSで求めたpostorder順に,
 * 受理頂点seedから自身に戻る閉路(受理サイクル)を探索する. */
void mcndfs_start(LmnWorker *w, State *seed, Vector *red_states) {
  auto t = (slim::verifier::tactics::MultiNDFS *)w->strategy.explorer.get();
  t->start(w, seed, red_states);
}

namespace slim {
namespace verifier {
namespace tactics {
void MultiNDFS::initialize(LmnWorker *w) {
  this->open = new Vector(1024);
  this->path = new Vector(512);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    if (worker_group(w)->workers_get_entried_num() > 1) {
      this->propagate = new Queue(element::concurrent_mode::multi_reader_multi_writer);
      this->waitingSeed = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    } else {
      this->propagate = new Queue();
      this->waitingSeed = new Queue();
    }
  } else {
    LmnWorker *prim = worker_group(w)->get_worker(LMN_PRIMARY_ID);
    this->propagate = MAP_WORKER_PROPAG_G(prim);
    this->waitingSeed = MAP_WORKER_DEL_G(prim);
  }
  this->traversed = NULL;
#endif
}

void MultiNDFS::finalize(LmnWorker *w) {
  delete MAPNDFS_WORKER_OPEN_VEC(w);
  delete MAPNDFS_WORKER_PATH_VEC(w);

#ifdef MAPNDFS_USE_MAP
  if (worker_id(w) == LMN_PRIMARY_ID) {
    delete MAP_WORKER_PROPAG_G(w);
    delete MAP_WORKER_DEL_G(w);
  }
  if (MAP_WORKER_HASHSET(w))
    st_free_table(MAP_WORKER_HASHSET(w));
#endif
}

void MultiNDFS::start(LmnWorker *w, State *seed, Vector *red_states) {
  BOOL has_error;
  START_CYCLE_SEARCH();

  has_error = FALSE;
  MAPNDFS_WORKER_OPEN_VEC(w)->push((vec_data_t)seed);
  has_error = mcndfs_loop(w, seed, MAPNDFS_WORKER_OPEN_VEC(w),
                          MAPNDFS_WORKER_PATH_VEC(w), red_states);

  FINISH_CYCLE_SEARCH();

  if (has_error) {
    mapndfs_found_accepting_cycle(w, seed, MAPNDFS_WORKER_PATH_VEC(w));
  }

  MAPNDFS_WORKER_OBJ_CLEAR(w);
}

void MultiNDFS::mcndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                             Vector *cycle_path) {
  LmnWorkerGroup *wp;
  Vector *v;
  unsigned long i;
  BOOL gen_counter_example;

  wp = worker_group(w);
  wp->workers_found_error();

  gen_counter_example = lmn_env.dump;
  seed->set_on_cycle(); /* 受理サイクルに含まれるフラグを立てる */

  v = gen_counter_example ? new Vector(cycle_path->get_num()) : NULL;

  /* 受理サイクル上の状態にフラグを立てていく */
  for (i = 0; i < cycle_path->get_num(); i++) {
    State *s = (State *)cycle_path->get(i);
    s->set_on_cycle();

    if (gen_counter_example)
      v->push((vec_data_t)s);
  }

  /* サイクルを登録 */
  if (gen_counter_example) {
    mc_found_invalid_path(wp, v);
  } else if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}

BOOL MultiNDFS::mcndfs_loop(LmnWorker *w, State *seed, Vector *search, Vector *path,
                        Vector *red_states) {
  unsigned int i, j, n, m;
  State *t, *succ;
  BOOL contained;

  while (search->get_num() > 0) {
    State *s = (State *)search->peek();

    if (s->is_snd()) {
      t = (State *)search->pop();
      if (path->get_num() > 0 && (State *)path->peek() == t) {
        path->pop();
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
        m = red_states->get_num();
        contained = FALSE;
        for (j = 0; j < m; j++) {
          t = (State *)red_states->get(j);
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
      State *s_pop = (State *)search->pop();
      if (path->get_num() > 0 && (State *)path->peek() == s_pop) {
        path->pop();
      }
    }
    else {
      unsigned int i;
      path->push((vec_data_t)s);
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
          search->push((vec_data_t)succ);
        }
      }
    }
#endif
  }

  return FALSE;
}

} // namespace tactics
} // namespace verifier
} // namespace slim

#endif
