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

#ifndef MINIMAL_STATE
#include "mc_visualizer.h"
#endif
#include "state.h"
#include "state.hpp"
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
#define DFS_WORKER_OBJ(W) ((McExpandDFS *)worker_generator_obj(W))
#define DFS_WORKER_OBJ_SET(W, O) (worker_generator_obj_set(W, O))
#define DFS_WORKER_QUEUE(W) (DFS_WORKER_OBJ(W)->q)
#define DFS_CUTOFF_DEPTH(W) (DFS_WORKER_OBJ(W)->cutoff_depth)
#define DFS_WORKER_STACK(W) (DFS_WORKER_OBJ(W)->stack)
#define DFS_WORKER_DEQUE(W) (DFS_WORKER_OBJ(W)->deq)

/* DFS Stackを静的に分割する条件 */
#define DFS_HANDOFF_COND_STATIC(W, Stack)                                      \
  (vec_num(Stack) >= DFS_CUTOFF_DEPTH(W))
#define DFS_HANDOFF_COND_STATIC_DEQ(W, Deq)                                    \
  (deq_num(Deq) >= DFS_CUTOFF_DEPTH(W))

/* DFS Stackを動的に分割するためのWork Sharingの条件 */
#define DFS_HANDOFF_COND_DYNAMIC(I, N, W)                                      \
  (worker_on_dynamic_lb(W) && ((I + 1) < (N)) &&                               \
   !worker_is_active(worker_next(W)))
/* 分割条件 */
#define DFS_LOAD_BALANCING(Stack, W, I, N)                                     \
  (DFS_HANDOFF_COND_STATIC(W, Stack) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))
#define DFS_LOAD_BALANCING_DEQ(Deq, W, I, N)                                   \
  (DFS_HANDOFF_COND_STATIC_DEQ(W, Deq) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))

/* 既に到達した状態であるかを判定するための条件 */
#define MAPNDFS_ALREADY_VISITED(w, s)                                          \
  ((s->s_is_visited_by_explorer() && worker_is_explorer(w)) ||                   \
   (s->s_is_visited_by_generator() && worker_is_generator(w)))

/* 初期状態を割り当てるワーカーの条件 */
#define WORKER_FOR_INIT_STATE(w, s)                                            \
  ((worker_use_mapndfs(w) && worker_is_explorer(w)) ||                         \
   (!worker_use_mapndfs(w) && worker_id(w) == 0) || !worker_on_parallel(w))

static inline void dfs_loop(LmnWorker *w, Vector *stack, Vector *new_states,
                            AutomataRef a, Vector *psyms);
static inline void mapdfs_loop(LmnWorker *w, Vector *stack, Vector *new_states,
                               AutomataRef a, Vector *psyms);
static inline void mcdfs_loop(LmnWorker *w, Vector *stack, Vector *new_states,
                              AutomataRef a, Vector *psyms);

void costed_dfs_loop(LmnWorker *w, Deque *deq, Vector *new_states,
                     AutomataRef a, Vector *psyms);
static inline LmnWord dfs_work_stealing(LmnWorker *w);
static inline void dfs_handoff_all_task(LmnWorker *me, Vector *tasks);
static inline void dfs_handoff_task(LmnWorker *me, LmnWord task);

typedef struct McExpandDFS {
  struct Vector stack;
  Deque deq;
  unsigned int cutoff_depth;
  Queue *q;
} McExpandDFS;

/* LmnWorker wにDFSのためのデータを割り当てる */
void dfs_worker_init(LmnWorker *w) {
  McExpandDFS *mc = LMN_MALLOC(McExpandDFS);
  mc->cutoff_depth = lmn_env.cutoff_depth;

  if (!worker_on_parallel(w)) {
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      deq_init(&mc->deq, 8192);
    } else
#endif
      vec_init(&mc->stack, 8192);
  } else {
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      deq_init(&mc->deq, mc->cutoff_depth + 1);
    } else
#endif
      vec_init(&mc->stack, mc->cutoff_depth + 1);

    if (lmn_env.core_num == 1) {
      mc->q = new Queue();
    } else if (worker_on_dynamic_lb(w)) {
      if (worker_use_mapndfs(w))
        mc->q = new Queue(LMN_Q_MRMW);
      else
        mc->q = new Queue(LMN_Q_MRSW);
    } else {
      mc->q = new Queue(LMN_Q_SRSW);
    }
  }

  DFS_WORKER_OBJ_SET(w, mc);
}

/* LmnWorkerのDFS固有データを破棄する */
void dfs_worker_finalize(LmnWorker *w) {
  if (worker_on_parallel(w)) {
    delete DFS_WORKER_QUEUE(w);
  }
#ifdef KWBT_OPT
  if (lmn_env.opt_mode != OPT_NONE) {
    deq_destroy(&DFS_WORKER_DEQUE(w));
  } else
#endif
    vec_destroy(&DFS_WORKER_STACK(w));
  LMN_FREE(DFS_WORKER_OBJ(w));
}

/* DFS Worker Queueが空の場合に真を返す */
BOOL dfs_worker_check(LmnWorker *w) {
  return DFS_WORKER_QUEUE(w) ? DFS_WORKER_QUEUE(w)->is_empty() : TRUE;
}

/* WorkerにDFSを割り当てる */
void dfs_env_set(LmnWorker *w) {
  worker_set_mc_dfs(w);
  w->start = dfs_start;
  w->check = dfs_worker_check;
  worker_generator_init_f_set(w, dfs_worker_init);
  worker_generator_finalize_f_set(w, dfs_worker_finalize);
}

/* ワーカーwが輪の方向に沿って, 他のワーカーから未展開状態を奪いに巡回する.
 * 未展開状態を発見した場合, そのワーカーのキューからdequeueして返す.
 * 発見できなかった場合, NULLを返す */
static inline LmnWord dfs_work_stealing(LmnWorker *w) {
  LmnWorker *dst;
  dst = worker_next(w);

  while (w != dst) {
    if (worker_is_active(dst) && !DFS_WORKER_QUEUE(dst)->is_empty()) {
      worker_set_active(w);
      worker_set_stealer(w);
      return DFS_WORKER_QUEUE(dst)->dequeue();
    } else {
      dst = worker_next(dst);
    }
  }
  return (LmnWord)NULL;
}

/* ベクタexpandsに積まれたタスクをワーカーmeの隣接ワーカーに全てハンドオフする
 */
static inline void dfs_handoff_all_task(LmnWorker *me, Vector *expands) {
  unsigned long i, n;
  LmnWorker *rn = worker_next(me);
  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }

  n = vec_num(expands);
  for (i = 0; i < n; i++) {
    DFS_WORKER_QUEUE(rn)->enqueue(vec_get(expands, i));
  }

  ADD_OPEN_PROFILE(sizeof(Node) * n);
}

/* タスクtaskをワーカーmeの隣接ワーカーにハンドオフする */
static inline void dfs_handoff_task(LmnWorker *me, LmnWord task) {
  LmnWorker *rn = worker_next(me);
  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }
  DFS_WORKER_QUEUE(rn)->enqueue(task);

  ADD_OPEN_PROFILE(sizeof(Node));
}

/* ワーカーwが輪の方向に沿って, 他のワーカーから未展開状態を奪いに巡回する.
 * 未展開状態を発見した場合, そのワーカーのキューからdequeueして返す.
 * 発見できなかった場合, NULLを返す */
static inline LmnWord mapdfs_work_stealing(LmnWorker *w) {
  LmnWorker *dst;

  if (worker_is_explorer(w))
    return (LmnWord)NULL;
  dst = worker_next_generator(w);

  while (w != dst) {
    if (worker_is_active(dst) && !DFS_WORKER_QUEUE(dst)->is_empty()) {
      worker_set_active(w);
      worker_set_stealer(w);
      return DFS_WORKER_QUEUE(dst)->dequeue();
    } else {
      dst = worker_next_generator(dst);
    }
  }
  return (LmnWord)NULL;
}

/* ベクタexpandsに積まれたタスクをワーカーmeの隣接ワーカーに全てハンドオフする
 */
static inline void mapdfs_handoff_all_task(LmnWorker *me, Vector *expands) {
  unsigned long i, n;
  LmnWorker *rn = worker_next_generator(me);

  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }

  n = vec_num(expands);
  for (i = 0; i < n; i++) {
    DFS_WORKER_QUEUE(rn)->enqueue(vec_get(expands, i));
    // DFS_WORKER_QUEUE(rn)->enqueue_push_head(vec_get(expands, i));
  }

  ADD_OPEN_PROFILE(sizeof(Node) * n);
}

/* タスクtaskをワーカーmeの隣接ワーカーにハンドオフする */
static inline void mcdfs_handoff_task(LmnWorker *me, LmnWord task) {
  LmnWorker *rn = worker_next_generator(me);

  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }
  DFS_WORKER_QUEUE(rn)->enqueue(task);
  // DFS_WORKER_QUEUE(rn)->enqueue_push_head(task);

  ADD_OPEN_PROFILE(sizeof(Node));
}

#ifndef MINIMAL_STATE
void mcdfs_start(LmnWorker *w) {
  LmnWorkerGroup *wp;
  struct Vector new_ss;
  State *s;
  StateSpaceRef ss;

  ss = worker_states(w);
  wp = worker_group(w);
  vec_init(&new_ss, 32);

  /*
  if(WORKER_FOR_INIT_STATE(w, s)) {
    s = ss->initial_state();
  } else {
    s = NULL;
  }
  */
  s = ss->initial_state();

  if (!worker_on_parallel(w)) { /* DFS */
    {
      put_stack(&DFS_WORKER_STACK(w), s);
      dfs_loop(w, &DFS_WORKER_STACK(w), &new_ss, ss->automata(),
               ss->prop_symbols());
    }
  } else {
    while (!wp->mc_exit) {
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
            put_stack(&DFS_WORKER_STACK(w), s);
            mcdfs_loop(w, &DFS_WORKER_STACK(w), &new_ss,
                       ss->automata(), ss->prop_symbols());
            s = NULL;
            vec_clear(&DFS_WORKER_STACK(w));
          }
          vec_clear(&new_ss);

          EXECUTE_PROFILE_FINISH();
        }
      }
    }
  }

  fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(w), w->expand);
  // fflush(stdout);
  if (lmn_env.enable_visualize) {
    if (worker_id(w) == 0) {
      dump_dot(ss, wp->worker_num);
    }
  }

  vec_destroy(&new_ss);
}
#endif

void dfs_start(LmnWorker *w) {
#ifndef MINIMAL_STATE
  if (worker_use_mcndfs(w)) {
    return mcdfs_start(w);
  }
#endif

  LmnWorkerGroup *wp;
  struct Vector new_ss;
  State *s;
  StateSpaceRef ss;

  ss = worker_states(w);
  wp = worker_group(w);
  vec_init(&new_ss, 32);

  if (WORKER_FOR_INIT_STATE(w, s)) {
    s = ss->initial_state();
  } else {
    s = NULL;
  }

  if (!worker_on_parallel(w)) { /* DFS */
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      push_deq(&DFS_WORKER_DEQUE(w), s, TRUE);
      costed_dfs_loop(w, &DFS_WORKER_DEQUE(w), &new_ss, ss->automata(),
                      ss->prop_symbols());
    } else
#endif
    {
      put_stack(&DFS_WORKER_STACK(w), s);
      dfs_loop(w, &DFS_WORKER_STACK(w), &new_ss, ss->automata(),
               ss->prop_symbols());
    }
  } else { /* Stack-Slicing */
    while (!wp->mc_exit) {
      if (!s && DFS_WORKER_QUEUE(w)->is_empty()) {
        worker_set_idle(w);
        if (lmn_workers_termination_detection_for_rings(w)) {
          /* termination is detected! */
          break;
        } else if (worker_on_dynamic_lb(w)) {
          /* 職探しの旅 */
          if (!worker_use_mapndfs(w))
            s = (State *)dfs_work_stealing(w);
          else if (worker_is_generator(w))
            s = (State *)mapdfs_work_stealing(w);
          else {
#ifdef DEBUG
            // explorerの仕事無くなったら終了
            printf("\n\n\nexplorer have no work\n\n\n");
#endif
            wp->mc_exit = TRUE;
          }
        }
      } else {
        worker_set_active(w);
#ifdef DEBUG
        if (!DFS_WORKER_QUEUE(w)->is_empty() &&
            !((Queue *)DFS_WORKER_QUEUE(w))->head->next) {
          printf("%d : queue is not empty? %d\n", worker_id(w),
                 DFS_WORKER_QUEUE(w)->entry_num());
        }
#endif
        if (s || (s = (State *)DFS_WORKER_QUEUE(w)->dequeue())) {
          EXECUTE_PROFILE_START();
#ifdef KWBT_OPT
          if (lmn_env.opt_mode != OPT_NONE) {
            push_deq(&DFS_WORKER_DEQUE(w), s, TRUE);
            costed_dfs_loop(w, &DFS_WORKER_DEQUE(w), &new_ss,
                            ss->automata(), ss->prop_symbols());
            s = NULL;
            deq_clear(&DFS_WORKER_DEQUE(w));
          } else
#endif
          {
            put_stack(&DFS_WORKER_STACK(w), s);
            if (worker_use_mapndfs(w))
              mapdfs_loop(w, &DFS_WORKER_STACK(w), &new_ss,
                          ss->automata(), ss->prop_symbols());
            else
              dfs_loop(w, &DFS_WORKER_STACK(w), &new_ss,
                       ss->automata(), ss->prop_symbols());
            s = NULL;
            vec_clear(&DFS_WORKER_STACK(w));
          }
          vec_clear(&new_ss);

          EXECUTE_PROFILE_FINISH();
        }
      }
    }
  }

#ifdef DEBUG
  fprintf(stderr, "%d : exit (total expand=%d)\n", worker_id(w), w->expand);
#endif
#ifndef MINIMAL_STATE
  if (lmn_env.enable_visualize) {
    if (worker_id(w) == 0) {
      dump_dot(ss, wp->worker_num);
    }
  }
#endif

  vec_destroy(&new_ss);
}

static inline void dfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss,
                            AutomataRef a, Vector *psyms) {
  while (!vec_is_empty(stack)) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (!(worker_group(w)))
      break;
    if (workers_are_exit(worker_group(w)))
      break;

    /** 展開元の状態の取得 */
    s = (State *)vec_peek(stack);
    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      if (NDFS_COND(w, s, p_s)) {
        /** entering second DFS */
        w->red++;
        ndfs_start(w, s);
      } else if (MAPNDFS_COND(w, s, p_s)) {
        mapndfs_start(w, s);
      }
      pop_stack(stack);
      continue;
    } else if (!worker_ltl_none(w) && p_s->get_is_end()) {
      mc_found_invalid_state(worker_group(w), s);
      pop_stack(stack);
      continue;
    }

    /* サクセッサを展開 */
    mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), new_ss, psyms,
              worker_flags(w));
#ifndef MINIMAL_STATE
    s->state_set_expander_id(worker_id(w));
#endif

    if (MAP_COND(w))
      map_start(w, s);

    if (!worker_on_parallel(w)) { /* Nested-DFS:
                                     postorder順を求めるDFS(再度到達した未展開状態がStackに積み直される)
                                   */
     s->set_on_stack();
      n = s->successor_num;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, i);

        if (!succ->is_expanded()) {
          put_stack(stack, succ);
        }
      }
    } else { /* 並列アルゴリズム使用時 */
      if (DFS_HANDOFF_COND_STATIC(w, stack)) {
        dfs_handoff_all_task(w, new_ss);
      } else {
        n = vec_num(new_ss);
        for (i = 0; i < n; i++) {
          State *new_s = (State *)vec_get(new_ss, i);

          if (DFS_LOAD_BALANCING(stack, w, i, n)) {
            dfs_handoff_task(w, (LmnWord)new_s);
          } else {
            put_stack(stack, new_s);
          }
        }
      }
    }

    vec_clear(new_ss);
  }
}

/* MAP+NDFS : 基本的にndfs_loopと同じ。安定したら合併させます。 */
static inline void mapdfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss,
                               AutomataRef a, Vector *psyms) {
  while (!vec_is_empty(stack)) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (workers_are_exit(worker_group(w)))
      break;

    /** 展開元の状態の取得 */
    s = (State *)vec_peek(stack);
    p_s = MC_GET_PROPERTY(s, a);
    if (s->is_expanded()) {
      if (MAPNDFS_COND(w, s, p_s)) {
        /** entering red DFS **/
        w->red++;
        mapndfs_start(w, s);
      }
      if (MAPNDFS_ALREADY_VISITED(w, s)) {
        pop_stack(stack);
        continue;
      }
    } else if (!worker_ltl_none(w) && p_s->get_is_end()) {
      mc_found_invalid_state(worker_group(w), s);
      pop_stack(stack);
      continue;
    }

    /* サクセッサを展開 */
    if (!s->is_expanded()) {
      w->expand++;
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), new_ss, psyms,
                worker_flags(w));
#ifndef MINIMAL_STATE
      s->state_set_expander_id(worker_id(w));
#endif
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
          put_stack(stack, succ);
        }
      }
    }
    /* 並列アルゴリズム使用時 MAPNDFS使ってる時点で並列前提だけど一応 */
    if (worker_on_parallel(w)) {
      if (DFS_HANDOFF_COND_STATIC(w, stack) /*|| worker_is_explorer(w)*/) {
        mapdfs_handoff_all_task(w, new_ss);
      } else {
        n = vec_num(new_ss);
        for (i = 0; i < n; i++) {
          State *new_s = (State *)vec_get(new_ss, i);

          if (DFS_LOAD_BALANCING(stack, w, i, n)) {
            mcdfs_handoff_task(w, (LmnWord)new_s);
          } else {
            put_stack(stack, new_s);
          }
        }
      }
    }

    vec_clear(new_ss);
  }
}

#ifndef MINIMAL_STATE
static inline void mcdfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss,
                              AutomataRef a, Vector *psyms) {
  while (!vec_is_empty(stack)) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n, start;
    BOOL repaired;

    if (workers_are_exit(worker_group(w)))
      break;

    /** 展開元の状態の取得 */
    s = (State *)vec_peek(stack);
    p_s = MC_GET_PROPERTY(s, a);

    // backtrack
    if (s->s_is_cyan(worker_id(w))) {
      if (state_is_accept(a, s)) {
        Vector red_states;
        vec_init(&red_states, 8192);

        // launch red dfs
        mcndfs_start(w, s, &red_states);

        // repair phase
        START_REPAIR_PHASE();
        do {
          repaired = TRUE;
          n = vec_num(&red_states);
          for (i = 0; i < n; i++) {
            State *r = (State *)vec_get(&red_states, i);

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
        n = vec_num(&red_states);
        for (i = 0; i < n; i++) {
          State *r = (State *)vec_get(&red_states, i);
          r->s_set_red();
        }
      }

      s->s_set_blue();
      s->s_unset_cyan(worker_id(w));

      pop_stack(stack);
      continue;
    }

    // cyan flag用の領域を確保
    if (!s->local_flags) {
      n = workers_entried_num((worker_group(w)));
      s->local_flags = LMN_NALLOC(BYTE, n);
      memset(s->local_flags, 0, sizeof(BYTE) * n);
    }

    // cyanに着色
    s->s_set_cyan(worker_id(w));

    // 同時に状態を展開すると問題が起こるのでロック
    START_LOCK();
    s->state_expand_lock();
    FINISH_LOCK();
    if (!s->is_expanded()) {
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), new_ss, psyms,
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
          put_stack(stack, succ);
        }
      }
    }
#else
    // fresh successor heuristics
    n = s->successor_num;
    Vector *fresh = NULL;

    if (n > 0) {
      fresh = vec_make(n);
      start = worker_id(w) % n;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, (start + i) % n);

        if (!succ->s_is_blue() && !succ->s_is_cyan(worker_id(w))) {
          if (!succ->is_expanded() && succ->s_is_fresh())
            put_stack(fresh, succ);
          else
            put_stack(stack, succ);
        }
      }
    }

    // freshな状態をスタックの上位に持ってくる
    if (fresh) {
      n = vec_num(fresh);
      if (n > 0) {
        for (i = 0; i < n; i++) {
          State *fs = (State *)vec_get(fresh, i);
          fs->s_unset_fresh();
          put_stack(stack, fs);
        }
      }
    }
#endif
  }
}
#endif

/* TODO: DequeとStackが異なるだけでdfs_loopと同じ.
 *   C++ template関数として記述するなど, 保守性向上のための修正が必要 */
void costed_dfs_loop(LmnWorker *w, Deque *deq, Vector *new_ss, AutomataRef a,
                     Vector *psyms) {
  while (!deq_is_empty(deq)) {
    State *s;
    AutomataStateRef p_s;
    unsigned int i, n;

    if (workers_are_exit(worker_group(w)))
      break;

    /** 展開元の状態の取得 */
    s = (State *)deq_peek_tail(deq);
    p_s = MC_GET_PROPERTY(s, a);

    if ((lmn_env.opt_mode == OPT_MINIMIZE &&
         workers_opt_cost(worker_group(w)) < state_cost(s)) ||
        (s->is_expanded() && !s->s_is_update()) ||
        (!worker_ltl_none(w) && p_s->get_is_end())) {
      pop_deq(deq, TRUE);
      continue;
    }

    if (!s->is_expanded()) {
      /* サクセッサを展開 */
      mc_expand(worker_states(w), s, p_s, worker_rc(w).get(), new_ss, psyms,
                worker_flags(w));
      mc_update_cost(s, new_ss, workers_ewlock(worker_group(w)));
    } else if (s->s_is_update()) {
      mc_update_cost(s, new_ss, workers_ewlock(worker_group(w)));
    }

    if (state_is_accept(a, s)) {
      lmn_update_opt_cost(worker_group(w), s,
                          (lmn_env.opt_mode == OPT_MINIMIZE));
    }

    if (!worker_on_parallel(w)) {
      n = s->successor_num;
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, i);
        if (!succ->is_expanded()) {
          push_deq(deq, succ, TRUE);
        } else if (succ->s_is_update()) {
          push_deq(deq, succ, FALSE);
        }
      }
    } else { /* 並列アルゴリズム使用時 */
      if (DFS_HANDOFF_COND_STATIC_DEQ(w, deq)) {
        dfs_handoff_all_task(w, new_ss);
      } else {
        n = vec_num(new_ss);
        for (i = 0; i < n; i++) {
          State *new_s = (State *)vec_get(new_ss, i);

          if (DFS_LOAD_BALANCING_DEQ(deq, w, i, n)) {
            dfs_handoff_task(w, (LmnWord)new_s);
          } else {
            if (!new_s->is_expanded()) {
              push_deq(deq, new_s, TRUE);
            } else if (new_s->s_is_update()) {
              push_deq(deq, new_s, FALSE);
            }
          }
        }
      }
    }

    vec_clear(new_ss);
  }
}

/** -----------------------------------------------------------
 *  Breadth First Search
 *  Layer Synchronized Breadth First Search
 */

typedef struct McExpandBFS {
  Queue *cur; /* 現在のFront Layer */
  Queue *nxt; /* 次のLayer */
} McExpandBFS;

#define BFS_WORKER_OBJ(W) ((McExpandBFS *)worker_generator_obj(W))
#define BFS_WORKER_OBJ_SET(W, O) (worker_generator_obj_set(W, O))
#define BFS_WORKER_Q_CUR(W) (BFS_WORKER_OBJ(W)->cur)
#define BFS_WORKER_Q_NXT(W) (BFS_WORKER_OBJ(W)->nxt)
#define BFS_WORKER_Q_SWAP(W)                                                                                   \
  do {                                                                                                         \
    Queue *_swap = BFS_WORKER_Q_NXT(W);                                                                        \
    BFS_WORKER_Q_NXT(W) = BFS_WORKER_Q_CUR(W);                                                                 \
    BFS_WORKER_Q_CUR(W) = _swap;                                                                               \
  } while (0) /* ポインタの付け替えはatomicに処理されないので並列処理の際には注意 \
               */

static inline void bfs_loop(LmnWorker *w, Vector *new_states, AutomataRef a,
                            Vector *psyms);

/* LmnWorker wにBFSのためのデータを割り当てる */
void bfs_worker_init(LmnWorker *w) {
  McExpandBFS *mc = LMN_MALLOC(McExpandBFS);

  if (!worker_on_parallel(w)) {
    mc->cur = new Queue();
    mc->nxt = new Queue();
  }
  /* MT */
  else if (worker_id(w) == 0) {
    mc->cur = new Queue(LMN_Q_MRMW);
    mc->nxt = worker_use_lsync(w) ? new Queue(LMN_Q_MRMW) : mc->cur;
  } else {
    /* Layer Queueは全スレッドで共有 */
    mc->cur = BFS_WORKER_Q_CUR(workers_get_worker(worker_group(w), 0));
    mc->nxt = BFS_WORKER_Q_NXT(workers_get_worker(worker_group(w), 0));
  }

  BFS_WORKER_OBJ_SET(w, mc);
}

/* LmnWorkerのBFS固有データを破棄する */
void bfs_worker_finalize(LmnWorker *w) {
  McExpandBFS *mc = (McExpandBFS *)BFS_WORKER_OBJ(w);
  if (!worker_on_parallel(w)) {
    delete mc->cur;
    delete mc->nxt;
  } else if (worker_id(w) == 0) {
    delete mc->cur;
    if (worker_use_lsync(w))
      delete mc->nxt;
  }
  LMN_FREE(mc);
}

/* BFS Queueが空の場合に真を返す */
BOOL bfs_worker_check(LmnWorker *w) {
  return BFS_WORKER_Q_CUR(w)->is_empty() &&
         BFS_WORKER_Q_NXT(w)->is_empty();
}

/* WorkerにBFSを割り当てる */
void bfs_env_set(LmnWorker *w) {
  worker_set_mc_bfs(w);
  w->start = bfs_start;
  w->check = bfs_worker_check;
  worker_generator_init_f_set(w, bfs_worker_init);
  worker_generator_finalize_f_set(w, bfs_worker_finalize);

  if (lmn_env.bfs_layer_sync) {
    worker_set_lsync(w);
  }
}

/* 幅優先探索で状態空間を構築する */
void bfs_start(LmnWorker *w) {
  LmnWorkerGroup *wp;
  unsigned long d, d_lim;
  Vector *new_ss;
  StateSpaceRef ss;

  ss = worker_states(w);
  wp = worker_group(w);
  d = 1;
  d_lim = lmn_env.depth_limits; /* ローカル変数に */
  new_ss = vec_make(32);

  if (!worker_on_parallel(w) ||
      worker_id(w) == 0) { /* 重複して初期状態をenqしないようにするための条件 */
    BFS_WORKER_Q_CUR(w)->enqueue((LmnWord)ss->initial_state());
  }

  /* start bfs  */
  if (!worker_on_parallel(w)) {
    /** >>>> 逐次 >>>> */
    while (!wp->mc_exit) {
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
    while (!wp->mc_exit) {
      if (!BFS_WORKER_Q_CUR(w)->is_empty()) {
        EXECUTE_PROFILE_START();
        worker_set_active(w);
        bfs_loop(w, new_ss, ss->automata(), ss->prop_symbols());
        worker_set_idle(w);

        vec_clear(new_ss);
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
        vec_clear(new_ss);
        /**/ EXECUTE_PROFILE_FINISH();
      }

      if (BLEDGE_COND(w))
        bledge_start(w);

      BFS_WORKER_Q_SWAP(w);
      lmn_workers_synchronization(
          w,
          (void (*)(LmnWorker *))lmn_workers_termination_detection_for_rings);
      if (d_lim < ++d || wp->mc_exit ||
          lmn_workers_termination_detection_for_rings(w)) {
        break;
      }
    }
  }

  vec_free(new_ss);
}

static inline void bfs_loop(LmnWorker *w, Vector *new_ss, AutomataRef a,
                            Vector *psyms) {
  LmnWorkerGroup *wp = worker_group(w);
  while (!BFS_WORKER_Q_CUR(w)->is_empty()) {
                 /* # of states@current layer > 0 */
    State *s;
    AutomataStateRef p_s;
    unsigned int i;

    if (workers_are_exit(wp))
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
        workers_set_exit(wp);
        return;
      }
    } else {
      /* 展開した状態をnext layer queueに登録する */
      for (i = 0; i < vec_num(new_ss); i++) {
        BFS_WORKER_Q_NXT(w)->enqueue((LmnWord)vec_get(new_ss, i));
      }
    }

    vec_clear(new_ss);
  }
}
