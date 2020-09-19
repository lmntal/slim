/*
 * mc_generator.cpp
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
#include "mc_generator.h"
#include "element/element.h"
#include "mc.h"
#include "mc_explorer.h"
#include "mc_worker.h"
#include "runtime_status.h"
#include <unistd.h>

#include "mc_visualizer.h"
#include "state.h"
#include "state.hpp"

namespace c14 = slim::element;

/* TODO: C++ template関数で書き直した方がよい */

/* 邪魔なので上に持ってきた */
#ifdef PROFILE
#include "runtime_status.h"

#define START_LOCK()                                                           \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_start_timer(PROFILE_TIME__LOCK);                                   \
  }
#define FINISH_LOCK()                                                          \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_finish_timer(PROFILE_TIME__LOCK);                                  \
  }

#define START_REPAIR_PHASE()                                                   \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_start_timer(PROFILE_TIME__REPAIR);                                 \
  }
#define FINISH_REPAIR_PHASE()                                                  \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_finish_timer(PROFILE_TIME__REPAIR);                                \
  }

#else
#define START_LOCK()
#define FINISH_LOCK()
#define START_REPAIR_PHASE()
#define FINISH_REPAIR_PHASE()
#endif

/** ------------------------------------------------------------------
 *  Depth First Search
 *  Parallel Depth First Search (Stack-Slicing Algorithm)
 */
#define DFS_WORKER_OBJ(W) ((slim::verifier::tactics::DFS *)&worker_generator(W))
#define DFS_WORKER_QUEUE(W) (this->q)
#define DFS_CUTOFF_DEPTH(W) (this->cutoff_depth)
#define DFS_WORKER_STACK(W) (DFS_WORKER_OBJ(W)->stack)
#define DFS_WORKER_DEQUE(W) (DFS_WORKER_OBJ(W)->deq)

/* DFS Stackを静的に分割する条件 */
#define DFS_HANDOFF_COND_STATIC(W, Stack) (Stack.size() >= DFS_CUTOFF_DEPTH(W))
#define DFS_HANDOFF_COND_STATIC_DEQ(W, Deq) (Deq.num() >= DFS_CUTOFF_DEPTH(W))

/* DFS Stackを動的に分割するためのWork Sharingの条件 */
#define DFS_HANDOFF_COND_DYNAMIC(I, N, W)                                      \
  (worker_on_dynamic_lb(W) && ((I + 1) < (N)) &&                               \
   !worker_is_active(begin(neighbors(W))))
/* 分割条件 */
#define DFS_LOAD_BALANCING(Stack, W, I, N)                                     \
  (DFS_HANDOFF_COND_STATIC(W, Stack) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))
#define DFS_LOAD_BALANCING_DEQ(Deq, W, I, N)                                   \
  (DFS_HANDOFF_COND_STATIC_DEQ(W, Deq) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))

/* 既に到達した状態であるかを判定するための条件 */
#define MAPNDFS_ALREADY_VISITED(w, s)                                          \
  ((s->s_is_visited_by_explorer() && worker_is_explorer(w)) ||                 \
   (s->s_is_visited_by_generator() && worker_is_generator(w)))

namespace slim {
namespace verifier {

void profiling_stack::pop() {
  auto &pop = c.top();
  if (pop->is_on_stack())
    pop->unset_on_stack();
  c.pop();

  if (slim::config::profile && lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(value_type));
  }
}

namespace tactics {
void DFS::initialize(LmnWorker *w) { profiling_stack().swap(this->stack); }
void DFS::finalize(LmnWorker *w) {}

void DFS::start() {
  auto ss = worker_states(owner);
  auto wp = worker_group(owner);
  auto s = ss->initial_state();

  this->stack.push(s);
  this->dfs_loop(owner, ss->automata(), ss->prop_symbols());

  if (slim::config::debug)
    fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(owner),
            owner->expand);

  if (!slim::config::minimal_state && lmn_env.enable_visualize) {
    if (worker_id(owner) == 0) {
      dump_dot(ss, wp->workers_get_entried_num());
    }
  }
}

bool DFS::check() { return true; }

void DFS::dfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  Vector new_ss;
  new_ss.init(32);
  while (!stack.empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (!(worker_group(w)))
      break;
    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = this->stack.top();
    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      if (NDFS_COND(w, s, p_s)) {
        /** entering second DFS */
        w->red++;
        ndfs_start(w, s);
      }

      this->stack.pop();
      continue;
    } else if (!worker_ltl_none(w) && p_s->get_is_end()) {
      mc_found_invalid_state(worker_group(w), s);
      this->stack.pop();
      continue;
    }

    /* サクセッサを展開 */
    mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
              worker_flags(w));

    if (!slim::config::minimal_state)
      s->state_set_expander_id(worker_id(w));

    /* Nested-DFS:
       postorder順を求めるDFS(再度到達した未展開状態がStackに積み直される)
     */
    s->set_on_stack();
    n = s->successor_num;
    for (i = 0; i < n; i++) {
      State *succ = state_succ_state(s, i);

      if (!succ->is_expanded()) {
        this->stack.push(succ);
      }
    }

    new_ss.clear();
  }
}

/* TODO: DequeとStackが異なるだけでdfs_loopと同じ.
 *   C++ template関数として記述するなど, 保守性向上のための修正が必要 */
void DFS_kwbtopt::costed_dfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  Vector new_ss;
  new_ss.init(32);
  while (!deq.is_empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = (State *)(deq.peek_tail());
    p_s = MC_GET_PROPERTY(s, a);

    if ((lmn_env.opt_mode == OPT_MINIMIZE &&
         worker_group(w)->opt_cost() < state_cost(s)) ||
        (s->is_expanded() && !s->s_is_update()) ||
        (!worker_ltl_none(w) && p_s->get_is_end())) {
      pop_deq(deq, TRUE);
      continue;
    }

    if (!s->is_expanded()) {
      /* サクセッサを展開 */
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
                worker_flags(w));
      mc_update_cost(s, &new_ss, worker_group(w)->workers_get_ewlock());
    } else if (s->s_is_update()) {
      mc_update_cost(s, &new_ss, worker_group(w)->workers_get_ewlock());
    }

    if (state_is_accept(a, s)) {
      worker_group(w)->update_opt_cost(s, (lmn_env.opt_mode == OPT_MINIMIZE));
    }

    n = s->successor_num;
    for (i = 0; i < n; i++) {
      State *succ = state_succ_state(s, i);
      if (!succ->is_expanded()) {
        push_deq(deq, succ, TRUE);
      } else if (succ->s_is_update()) {
        push_deq(deq, succ, FALSE);
      }
    }

    new_ss.clear();
  }
}

void DFS_kwbtopt::initialize(LmnWorker *w) { this->deq.init(8192); }
void DFS_kwbtopt::finalize(LmnWorker *w) { this->deq.destroy(); }

void DFS_kwbtopt::start() {
  auto ss = worker_states(owner);
  auto wp = worker_group(owner);
  auto s = ss->initial_state();

  push_deq(this->deq, s, TRUE);
  this->costed_dfs_loop(owner, ss->automata(), ss->prop_symbols());

  if (slim::config::debug)
    fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(owner),
            owner->expand);

  if (!slim::config::minimal_state && lmn_env.enable_visualize) {
    if (worker_id(owner) == 0) {
      dump_dot(ss, wp->workers_get_entried_num());
    }
  }
}

void DFS_Parallel::initialize(LmnWorker *w) {
  this->cutoff_depth = lmn_env.cutoff_depth;

  if (slim::config::kwbt_opt && lmn_env.opt_mode != OPT_NONE) {
    this->deq.init(this->cutoff_depth + 1);
  } else {
    profiling_stack().swap(this->stack);
  }

  if (lmn_env.core_num == 1) {
    this->q = c14::make_unique<slim::element::concurrent_queue<State *>>();
  } else if (worker_on_dynamic_lb(owner)) {
    if (worker_use_mapndfs(owner))
      this->q = c14::make_unique<slim::element::concurrent_queue<State *>>(
          element::concurrent_mode::multi_reader_multi_writer);
    else
      this->q = c14::make_unique<slim::element::concurrent_queue<State *>>(
          element::concurrent_mode::multi_reader_single_writer);
  } else {
    this->q = c14::make_unique<slim::element::concurrent_queue<State *>>(
        element::concurrent_mode::single_reader_single_writer);
  }
}
void DFS_Parallel::finalize(LmnWorker *w) {
  if (slim::config::kwbt_opt && lmn_env.opt_mode != OPT_NONE) {
    this->deq.destroy();
  }
}

void DFS_Parallel::start() {
  if (!slim::config::minimal_state && worker_use_mcndfs(owner)) {
    return mcdfs_start(owner);
  }

  auto ss = worker_states(owner);
  auto wp = worker_group(owner);
  auto s = this->is_assigned_to_initial_state() ? ss->initial_state() : nullptr;

  /* Stack-Slicing */
  while (!wp->workers_are_exit()) {
    if (!s && this->q->is_empty()) {
      worker_set_idle(owner);
      if (lmn_workers_termination_detection_for_rings(owner)) {
        /* termination is detected! */
        break;
      } else if (worker_on_dynamic_lb(owner)) {
        /* 職探しの旅 */
        if (!worker_use_mapndfs(owner))
          s = steal_unexpand_state(neighbors(owner));
        else if (worker_is_generator(owner))
          s = steal_unexpand_state(neighbors(this->owner).generators());
        else {
          // explorerの仕事無くなったら終了
          if (slim::config::debug) {
            printf("\n\n\nexplorer have no work\n\n\n");
          }
          wp->workers_set_exit();
        }
      }
    } else {
      worker_set_active(owner);
      if (slim::config::debug && !this->q->is_empty() && !this->q->head->next) {
        printf("%d : queue is not empty? %d\n", worker_id(owner),
               this->q->entry_num());
      }

      if (s || (s = this->q->dequeue())) {
        EXECUTE_PROFILE_START();
        if (slim::config::kwbt_opt && lmn_env.opt_mode != OPT_NONE) {
          push_deq(this->deq, s, TRUE);
          this->costed_dfs_loop(owner, ss->automata(), ss->prop_symbols());
          s = NULL;
          this->deq.clear();
        } else {
          this->stack.push(s);
          if (worker_use_mapndfs(owner))
            this->mapdfs_loop(owner, ss->automata(), ss->prop_symbols());
          else
            this->dfs_loop(owner, ss->automata(), ss->prop_symbols());
          s = NULL;
          profiling_stack().swap(this->stack);
        }

        EXECUTE_PROFILE_FINISH();
      }
    }
  }

  if (slim::config::debug)
    fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(owner),
            owner->expand);

  if (!slim::config::minimal_state && lmn_env.enable_visualize) {
    if (worker_id(owner) == 0) {
      dump_dot(ss, wp->workers_get_entried_num());
    }
  }
}

bool DFS_Parallel::check() { return this->q->is_empty(); }

void DFS_Parallel::dfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  Vector new_ss;
  new_ss.init(32);
  while (!stack.empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (!(worker_group(w)))
      break;
    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = this->stack.top();
    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      if (NDFS_COND(w, s, p_s)) {
        /** entering second DFS */
        w->red++;
        ndfs_start(w, s);
      } else if (MAPNDFS_COND(w, s, p_s)) {
        mapndfs_start(w, s);
      }
      this->stack.pop();
      continue;
    } else if (!worker_ltl_none(w) && p_s->get_is_end()) {
      mc_found_invalid_state(worker_group(w), s);
      this->stack.pop();
      continue;
    }

    /* サクセッサを展開 */
    mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
              worker_flags(w));

    if (!slim::config::minimal_state)
      s->state_set_expander_id(worker_id(w));

    if (MAP_COND(w))
      map_start(w, s);

    if (DFS_HANDOFF_COND_STATIC(w, stack)) {
      handoff_all_tasks(&new_ss, *begin(neighbors(this->owner)));
    } else {
      n = new_ss.get_num();
      for (i = 0; i < n; i++) {
        State *new_s = (State *)new_ss.get(i);

        if (DFS_LOAD_BALANCING(stack, w, i, n)) {
          handoff_task(new_s, *begin(neighbors(this->owner)));
        } else {
          this->stack.push(new_s);
        }
      }
    }

    new_ss.clear();
  }
}

void DFS_Parallel::mcdfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  LMN_ASSERT(!slim::config::minimal_state);

  Vector new_ss;
  new_ss.init(32);
  while (!stack.empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n, start;
    BOOL repaired;

    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = (State *)stack.top();
    p_s = MC_GET_PROPERTY(s, a);

    // backtrack
    if (s->s_is_cyan(worker_id(w))) {
      if (state_is_accept(a, s)) {
        Vector red_states;
        red_states.init(8192);

        // launch red dfs
        mcndfs_start(w, s, &red_states);

        // repair phase
        START_REPAIR_PHASE();
        do {
          repaired = TRUE;
          n = red_states.get_num();
          for (i = 0; i < n; i++) {
            State *r = (State *)red_states.get(i);

            if (state_id(r) != state_id(s) && state_is_accept(a, r)) {
              if (!r->s_is_red()) {
                repaired = FALSE;
                usleep(1);
                break;
              }
            }
          }
        } while (!repaired);
        FINISH_REPAIR_PHASE();

        // set red
        n = red_states.get_num();
        for (i = 0; i < n; i++) {
          State *r = (State *)red_states.get(i);
          r->s_set_red();
        }
      }

      s->s_set_blue();
      s->s_unset_cyan(worker_id(w));

      this->stack.pop();
      continue;
    }

    // cyan flag用の領域を確保
    s->prepare_local_flags(worker_group(w)->workers_get_entried_num());

    // cyanに着色
    s->s_set_cyan(worker_id(w));

    // 同時に状態を展開すると問題が起こるのでロック
    START_LOCK();
    s->state_expand_lock();
    FINISH_LOCK();
    if (!s->is_expanded()) {
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
                worker_flags(w));
      w->expand++;
      s->state_set_expander_id(worker_id(w));
    }
    s->state_expand_unlock();

#if 0
    // workerごとにsuccessorをずらして積む
    n = s->successor_num;

    if (n > 0) {
      start = worker_id(w) % n;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, (start + i) % n);

        if (!succ->s_is_blue() && !succ->s_is_cyan(worker_id(w))) {
          stack.push(succ);
        }
      }
    }
#else
    // fresh successor heuristics
    n = s->successor_num;
    Vector *fresh = NULL;

    if (n > 0) {
      fresh = new Vector(n);
      start = worker_id(w) % n;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, (start + i) % n);

        if (!succ->s_is_blue() && !succ->s_is_cyan(worker_id(w))) {
          if (!succ->is_expanded() && succ->s_is_fresh())
            fresh->push((LmnWord)succ);
          else
            this->stack.push(succ);
        }
      }
    }

    // freshな状態をスタックの上位に持ってくる
    if (fresh) {
      n = fresh->get_num();
      if (n > 0) {
        for (i = 0; i < n; i++) {
          State *fs = (State *)fresh->get(i);
          fs->s_unset_fresh();
          this->stack.push(fs);
        }
      }
    }
#endif
  }
}

void DFS_Parallel::mcdfs_start(LmnWorker *w) {
  LMN_ASSERT(!slim::config::minimal_state);

  LmnWorkerGroup *wp;
  struct Vector new_ss;
  State *s;
  StateSpaceRef ss;

  ss = worker_states(w);
  wp = worker_group(w);
  new_ss.init(32);

  /*
  if(this->is_assigned_to_initial_state()) {
    s = ss->initial_state();
  } else {
    s = NULL;
  }
  */
  s = ss->initial_state();

  while (!wp->workers_are_exit()) {
    if (!s && DFS_WORKER_QUEUE(w)->is_empty()) {
      break;
      /*
      if (lmn_workers_termination_detection_for_rings(w)) {
        break;
      }
      */
    } else {
      // worker_set_active(w);
      if (s || (s = (State *)DFS_WORKER_QUEUE(w)->dequeue())) {
        EXECUTE_PROFILE_START();
        {
          this->stack.push(s);
          this->mcdfs_loop(w, ss->automata(), ss->prop_symbols());
          s = NULL;
          profiling_stack().swap(this->stack);
        }
        new_ss.clear();

        EXECUTE_PROFILE_FINISH();
      }
    }
  }

  fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(w), w->expand);
  // fflush(stdout);
  if (lmn_env.enable_visualize) {
    if (worker_id(w) == 0) {
      dump_dot(ss, wp->workers_get_entried_num());
    }
  }

  new_ss.destroy();
}

/* MAP+NDFS : 基本的にndfs_loopと同じ。安定したら合併させます。 */
void DFS_Parallel::mapdfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  Vector new_ss;
  new_ss.init(32);
  while (!stack.empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = (State *)stack.top();
    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      if (MAPNDFS_COND(w, s, p_s)) {
        /** entering red DFS **/
        w->red++;
        mapndfs_start(w, s);
      }
      if (MAPNDFS_ALREADY_VISITED(w, s)) {
        this->stack.pop();
        continue;
      }
    } else if (!worker_ltl_none(w) && p_s->get_is_end()) {
      mc_found_invalid_state(worker_group(w), s);
      this->stack.pop();
      continue;
    }

    /* サクセッサを展開 */
    if (!s->is_expanded()) {
      w->expand++;
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
                worker_flags(w));
      if (!slim::config::minimal_state)
        s->state_set_expander_id(worker_id(w));
    }

    if (MAP_COND(w))
      map_start(w, s);

    /* explorerが到達した状態はblueに着色 */
    if (worker_is_explorer(w))
      s->s_set_visited_by_explorer();
    // generatorが到達した状態はredに着色
    else if (worker_is_generator(w))
      s->s_set_visited_by_generator();

    /* Nested-DFS:
     * postorder順を求めるDFS(explorerから未到達の状態がStackに積み直される) */
    if (worker_is_explorer(w)) {
      s->set_on_stack();
      n = s->successor_num;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, i);
        if (!succ->s_is_visited_by_explorer()) {
          this->stack.push(succ);
        }
      }
    }
    /* 並列アルゴリズム使用時 MAPNDFS使ってる時点で並列前提だけど一応 */
    if (DFS_HANDOFF_COND_STATIC(w, stack) /*|| worker_is_explorer(w)*/) {
      handoff_all_tasks(&new_ss, *begin(neighbors(this->owner).generators()));
    } else {
      n = new_ss.get_num();
      for (i = 0; i < n; i++) {
        State *new_s = (State *)new_ss.get(i);

        if (DFS_LOAD_BALANCING(stack, w, i, n)) {
          handoff_task(new_s, *begin(neighbors(this->owner).generators()));
        } else {
          this->stack.push(new_s);
        }
      }
    }

    new_ss.clear();
  }
}

/* TODO: DequeとStackが異なるだけでdfs_loopと同じ.
 *   C++ template関数として記述するなど, 保守性向上のための修正が必要 */
void DFS_Parallel::costed_dfs_loop(LmnWorker *w, AutomataRef a, Vector *psyms) {
  Vector new_ss;
  new_ss.init(32);
  while (!deq.is_empty()) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (worker_group(w)->workers_are_exit())
      break;

    /** 展開元の状態の取得 */
    s = (State *)(deq.peek_tail());
    p_s = MC_GET_PROPERTY(s, a);

    if ((lmn_env.opt_mode == OPT_MINIMIZE &&
         worker_group(w)->opt_cost() < state_cost(s)) ||
        (s->is_expanded() && !s->s_is_update()) ||
        (!worker_ltl_none(w) && p_s->get_is_end())) {
      pop_deq(deq, TRUE);
      continue;
    }

    if (!s->is_expanded()) {
      /* サクセッサを展開 */
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), &new_ss, psyms,
                worker_flags(w));
      mc_update_cost(s, &new_ss, worker_group(w)->workers_get_ewlock());
    } else if (s->s_is_update()) {
      mc_update_cost(s, &new_ss, worker_group(w)->workers_get_ewlock());
    }

    if (state_is_accept(a, s)) {
      worker_group(w)->update_opt_cost(s, (lmn_env.opt_mode == OPT_MINIMIZE));
    }

    if (DFS_HANDOFF_COND_STATIC_DEQ(w, deq)) {
      handoff_all_tasks(&new_ss, *begin(neighbors(this->owner)));
    } else {
      n = new_ss.get_num();
      for (i = 0; i < n; i++) {
        State *new_s = (State *)new_ss.get(i);

        if (DFS_LOAD_BALANCING_DEQ(deq, w, i, n)) {
          handoff_task(new_s, *begin(neighbors(this->owner)));
        } else {
          if (!new_s->is_expanded()) {
            push_deq(deq, new_s, TRUE);
          } else if (new_s->s_is_update()) {
            push_deq(deq, new_s, FALSE);
          }
        }
      }
    }

    new_ss.clear();
  }
}

/* ベクタexpandsに積まれたタスクをワーカーmeの隣接ワーカーに全てハンドオフする
 */
void DFS_Parallel::handoff_all_tasks(Vector *expands, LmnWorker &rn) {
  if (this->owner->id > rn.id) {
    worker_set_black(this->owner);
  }

  auto gen = (DFS_Parallel *)rn.strategy.generator.get();
  for (int i = 0; i < expands->get_num(); i++) {
    gen->q->enqueue((State *)expands->get(i));
  }

  ADD_OPEN_PROFILE(sizeof(Node) * expands->get_num());
}

/* タスクtaskをワーカーmeの隣接ワーカーにハンドオフする */
void DFS_Parallel::handoff_task(State *task, LmnWorker &rn) {
  if (this->owner->id > rn.id) {
    worker_set_black(this->owner);
  }
  auto gen = (DFS_Parallel *)rn.strategy.generator.get();
  gen->q->enqueue(task);

  ADD_OPEN_PROFILE(sizeof(Node));
}

/** -----------------------------------------------------------
 *  Breadth First Search
 *  Layer Synchronized Breadth First Search
 */

#define BFS_WORKER_OBJ(W) ((BFS *)&worker_generator(W))
#define BFS_WORKER_Q_CUR(W) (BFS_WORKER_OBJ(W)->cur)
#define BFS_WORKER_Q_NXT(W) (BFS_WORKER_OBJ(W)->nxt)
#define BFS_WORKER_Q_SWAP(W)                                                                              \
  do {                                                                                                    \
    Queue *_swap = BFS_WORKER_Q_NXT(W);                                                                   \
    BFS_WORKER_Q_NXT(W) = BFS_WORKER_Q_CUR(W);                                                            \
    BFS_WORKER_Q_CUR(W) = _swap;                                                                          \
  } while (                                                                                               \
      0) /* ポインタの付け替えはatomicに処理されないので並列処理の際には注意 \
          */

void BFS::bfs_loop(LmnWorker *w, Vector *new_ss, AutomataRef a, Vector *psyms) {
  LmnWorkerGroup *wp = worker_group(w);
  while (!BFS_WORKER_Q_CUR(w)->is_empty()) {
    /* # of states@current layer > 0 */
    State *s;
    AutomataStateRef p_s;
    unsigned int i;

    if (wp->workers_are_exit())
      return;

    s = (State *)BFS_WORKER_Q_CUR(w)->dequeue();

    if (!s)
      return; /* dequeueはNULLを返すことがある */

    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      continue;
    } else if (!worker_ltl_none(w) &&
               p_s->get_is_end()) { /* safety property analysis */
      mc_found_invalid_state(wp, s);
      continue;
    }

    mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), new_ss, psyms,
              worker_flags(w));

    if (MAP_COND(w))
      map_start(w, s);
    else if (BLEDGE_COND(w))
      bledge_store_layer(w, s);

    if (s->successor_num == 0) {
      if (lmn_env.nd_search_end) {
        /* 最終状態探索モードの場合, 発見次第探索を打ち切る */
        wp->workers_set_exit();
        return;
      }
    } else {
      /* 展開した状態をnext layer queueに登録する */
      for (i = 0; i < new_ss->get_num(); i++) {
        BFS_WORKER_Q_NXT(w)->enqueue((LmnWord)new_ss->get(i));
      }
    }

    new_ss->clear();
  }
}

void BFS::initialize(LmnWorker *w) {
  if (!worker_on_parallel(w)) {
    this->cur = new Queue();
    this->nxt = new Queue();
  }
  /* MT */
  else if (worker_id(w) == 0) {
    this->cur = new Queue(element::concurrent_mode::multi_reader_multi_writer);
    this->nxt =
        worker_use_lsync(w)
            ? new Queue(element::concurrent_mode::multi_reader_multi_writer)
            : this->cur;
  } else {
    /* Layer Queueは全スレッドで共有 */
    this->cur = BFS_WORKER_Q_CUR(worker_group(w)->get_worker(0));
    this->nxt = BFS_WORKER_Q_NXT(worker_group(w)->get_worker(0));
  }
}

void BFS::finalize(LmnWorker *w) {
  if (!worker_on_parallel(w)) {
    delete this->cur;
    delete this->nxt;
  } else if (worker_id(w) == 0) {
    delete this->cur;
    if (worker_use_lsync(w))
      delete this->nxt;
  }
}

void BFS::start() {
  LmnWorker *w = this->owner;
  LmnWorkerGroup *wp;
  unsigned long d, d_lim;
  Vector *new_ss;
  StateSpaceRef ss;

  ss = worker_states(w);
  wp = worker_group(w);
  d = 1;
  d_lim = lmn_env.depth_limits; /* ローカル変数に */
  new_ss = new Vector(32);

  if (!worker_on_parallel(w) ||
      worker_id(w) == 0) { /* 重複して初期状態をenqしないようにするための条件 */
    BFS_WORKER_Q_CUR(w)->enqueue((LmnWord)ss->initial_state());
  }

  /* start bfs  */
  if (!worker_on_parallel(w)) {
    /** >>>> 逐次 >>>> */
    while (!wp->workers_are_exit()) {
      /* 1step展開 */
      bfs_loop(w, new_ss, ss->automata(), ss->prop_symbols());

      if (BLEDGE_COND(w))
        bledge_start(w);

      if (d_lim < ++d || BFS_WORKER_Q_NXT(w)->is_empty()) {
        /* 次のLayerが空の場合は探索終了 */
        /* 指定した制限の深さに到達した場合も探索を打ち切る */
        break;
      }

      BFS_WORKER_Q_SWAP(w); /* Layer Queueをswap */
    }
  } else if (!worker_use_lsync(w)) {
    /** >>>> 並列(Layer非同期) >>>> */
    while (!wp->workers_are_exit()) {
      if (!BFS_WORKER_Q_CUR(w)->is_empty()) {
        EXECUTE_PROFILE_START();
        worker_set_active(w);
        bfs_loop(w, new_ss, ss->automata(), ss->prop_symbols());
        worker_set_idle(w);

        new_ss->clear();
        EXECUTE_PROFILE_FINISH();
      } else {
        worker_set_idle(w);
        if (lmn_workers_termination_detection_for_rings(w)) {
          /* termination is detected! */
          break;
        }
      }
    }
  } else {
    /** >>>> 並列(Layer同期) >>>> */
    worker_set_idle(w);
    while (TRUE) {
      if (!BFS_WORKER_Q_CUR(w)->is_empty()) {
        /**/ EXECUTE_PROFILE_START();
        worker_set_active(w);
        bfs_loop(w, new_ss, ss->automata(), ss->prop_symbols());
        worker_set_idle(w);
        new_ss->clear();
        /**/ EXECUTE_PROFILE_FINISH();
      }

      if (BLEDGE_COND(w))
        bledge_start(w);

      BFS_WORKER_Q_SWAP(w);
      lmn_workers_synchronization(
          w,
          (void (*)(LmnWorker *))lmn_workers_termination_detection_for_rings);
      if (d_lim < ++d || wp->workers_are_exit() ||
          lmn_workers_termination_detection_for_rings(w)) {
        break;
      }
    }
  }

  delete new_ss;
}

bool BFS::check() {
  return BFS_WORKER_Q_CUR(owner)->is_empty() && BFS_WORKER_Q_NXT(owner)->is_empty();
}

} // namespace tactics
} // namespace verifier
} // namespace slim
