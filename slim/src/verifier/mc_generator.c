/*
 * mc_generator.c
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
#include "mc_generator.h"
#include "mc_explorer.h"
#include "state.h"
#include "vector.h"
#include "queue.h"
#include "error.h"
#include "lmntal_thread.h"
#include "runtime_status.h"

/* TODO: C++ template関数で書き直した方がよい */

/* 邪魔なので上に持ってきた */
#ifdef PROFILE
# define pop_stack(List)                                                       \
  do {                                                                         \
    State *pop = (State *)vec_pop(List);                                       \
    if (is_on_stack(pop)) unset_on_stack(pop);                                 \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));         \
    }                                                                          \
  } while (0)
# define put_stack(List, St)                                                   \
  do {                                                                         \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));            \
    }                                                                          \
    vec_push((List), (vec_data_t)(St));                                        \
  } while (0)
# define pop_deq(Deq, Dir)                                                     \
  do {                                                                         \
    if (Dir) {                                                                  \
      deq_pop_tail(Deq);                                                       \
    } else {                                                                   \
      deq_pop_head(Deq);                                                       \
    }                                                                          \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));         \
    }                                                                          \
  } while (0)
# define push_deq(List, St, Dir)                                               \
  do {                                                                         \
    if (lmn_env.profile_level >= 3) {                                          \
      profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));            \
    }                                                                          \
    if (Dir) {                                                                 \
      deq_push_tail((List), (vec_data_t)(St));                                 \
    } else {                                                                   \
      deq_push_head((List), (vec_data_t)(St));                                 \
    }                                                                          \
} while (0)
# define EXECUTE_PROFILE_START()                                               \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(Node));              \
    profile_start_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);                   \
  }
# define EXECUTE_PROFILE_FINISH()                                              \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_finish_timer(PROFILE_TIME__ACTIVE_FOR_IDLE_PROF);                  \
  }
# define ADD_OPEN_PROFILE(M)                                                   \
  if (lmn_env.profile_level >= 3) {                                            \
    profile_add_space(PROFILE_SPACE__OPEN_LIST, M);                            \
  }
#else
# define EXECUTE_PROFILE_START()
# define EXECUTE_PROFILE_FINISH()
# define ADD_OPEN_PROFILE(M)
# define pop_stack(List)                                                       \
  do {                                                                         \
    State *pop = (State *)vec_pop(List);                                       \
    if (is_on_stack(pop)) unset_on_stack(pop);                                 \
  } while (0)
# define put_stack(List, St)  vec_push((List), (vec_data_t)(St))
# define pop_deq(Deq, Dir)                                                     \
  do {                                                                         \
    if (Dir) {                                                                 \
      deq_pop_tail(Deq);                                                       \
    } else {                                                                   \
      (State *)deq_pop_head(Deq);                                              \
    }                                                                          \
  } while (0)
# define push_deq(List, St, Dir)                                               \
  do {                                                                         \
    if (Dir) {                                                                 \
      deq_push_tail((List), (vec_data_t)(St));                                 \
    } else {                                                                   \
      deq_push_head((List), (vec_data_t)(St));                                 \
    }                                                                          \
} while (0)

#endif



/** ------------------------------------------------------------------
 *  Depth First Search
 *  Parallel Depth First Search (Stack-Slicing Algorithm)
 */
#define DFS_WORKER_OBJ(W)                  ((McExpandDFS *)worker_generator_obj(W))
#define DFS_WORKER_OBJ_SET(W, O)           (worker_generator_obj_set(W, O))
#define DFS_WORKER_QUEUE(W)                (DFS_WORKER_OBJ(W)->q)
#define DFS_CUTOFF_DEPTH(W)                (DFS_WORKER_OBJ(W)->cutoff_depth)
#define DFS_WORKER_STACK(W)                (DFS_WORKER_OBJ(W)->stack)
#define DFS_WORKER_DEQUE(W)                (DFS_WORKER_OBJ(W)->deq)

/* DFS Stackを静的に分割する条件 */
#define DFS_HANDOFF_COND_STATIC(W, Stack)                                      \
  (vec_num(Stack) >= DFS_CUTOFF_DEPTH(W))
#define DFS_HANDOFF_COND_STATIC_DEQ(W, Deq)                                    \
  (deq_num(Deq)   >= DFS_CUTOFF_DEPTH(W))

/* DFS Stackを動的に分割するためのWork Sharingの条件 */
#define DFS_HANDOFF_COND_DYNAMIC(I, N, W)                                      \
  (worker_on_dynamic_lb(W) && ((I + 1) < (N)) && !worker_is_active(worker_next(W)))
/* 分割条件 */
#define DFS_LOAD_BALANCING(Stack, W, I, N)                                     \
  (DFS_HANDOFF_COND_STATIC(W, Stack) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))
#define DFS_LOAD_BALANCING_DEQ(Deq, W, I, N)                                   \
  (DFS_HANDOFF_COND_STATIC_DEQ(W, Deq) || DFS_HANDOFF_COND_DYNAMIC(I, N, W))

/* 既に到達した状態であるかを判定するための条件 */
#define MCNDFS_ALREADY_VISITED(w, s) ( (s_is_blue(s) && worker_is_explorer(w)) \
                                   ||  (s_is_red(s) && worker_is_generator(w)) ) 

/* 初期状態を割り当てるワーカーの条件 */
#define WORKER_FOR_INIT_STATE(w, s) ( (worker_use_mcndfs(w)  && worker_is_explorer(w) ) \
                                   || (!worker_use_mcndfs(w) &&  worker_id(w) == 0)     \
                                   || !worker_on_parallel(w) )

static inline void    dfs_loop(LmnWorker *w,
                               Vector    *stack,
                               Vector    *new_states,
                               Automata  a,
                               Vector    *psyms);
static inline void    mcdfs_loop(LmnWorker *w,
                               Vector    *stack,
                               Vector    *new_states,
                               Automata  a,
                               Vector    *psyms);
void    costed_dfs_loop(LmnWorker *w,
                        Deque    *deq,
                        Vector    *new_states,
                        Automata  a,
                        Vector    *psyms);
static inline LmnWord dfs_work_stealing(LmnWorker *w);
static inline void    dfs_handoff_all_task(LmnWorker *me, Vector *tasks);
static inline void    dfs_handoff_task(LmnWorker *me,  LmnWord task);

typedef struct McExpandDFS {
  struct Vector stack;
  Deque deq;
  unsigned int cutoff_depth;
  Queue *q;
} McExpandDFS;


/* LmnWorker wにDFSのためのデータを割り当てる */
void dfs_worker_init(LmnWorker *w)
{
  McExpandDFS *mc = LMN_MALLOC(McExpandDFS);
  mc->cutoff_depth = lmn_env.cutoff_depth;

  if (!worker_on_parallel(w)) {
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      deq_init(&mc->deq, 8192);
    } else
#endif
      vec_init(&mc->stack, 8192);
  }
  else {
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      deq_init(&mc->deq, mc->cutoff_depth + 1);
    } else
#endif
      vec_init(&mc->stack, mc->cutoff_depth + 1);

    if (lmn_env.core_num == 1) {
      mc->q = new_queue();
    } else if (worker_on_dynamic_lb(w)) {
      if(worker_use_mcndfs(w)) mc->q = make_parallel_queue(LMN_Q_MRMW);
      else mc->q = make_parallel_queue(LMN_Q_MRSW);
    } else {
      mc->q = make_parallel_queue(LMN_Q_SRSW);
    }
  }

  DFS_WORKER_OBJ_SET(w, mc);
}


/* LmnWorkerのDFS固有データを破棄する */
void dfs_worker_finalize(LmnWorker *w)
{
  if (worker_on_parallel(w)) {
    q_free(DFS_WORKER_QUEUE(w));
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
BOOL dfs_worker_check(LmnWorker *w)
{
  return DFS_WORKER_QUEUE(w) ? is_empty_queue(DFS_WORKER_QUEUE(w))
                             : TRUE;
}

/* WorkerにDFSを割り当てる */
void dfs_env_set(LmnWorker *w)
{
  worker_set_mc_dfs(w);
  w->start    = dfs_start;
  w->check    = dfs_worker_check;
  worker_generator_init_f_set(w, dfs_worker_init);
  worker_generator_finalize_f_set(w, dfs_worker_finalize);
}


/* ワーカーwが輪の方向に沿って, 他のワーカーから未展開状態を奪いに巡回する.
 * 未展開状態を発見した場合, そのワーカーのキューからdequeueして返す.
 * 発見できなかった場合, NULLを返す */
static inline LmnWord dfs_work_stealing(LmnWorker *w)
{
  LmnWorker *dst;
  dst = worker_next(w);

  while (w != dst) {
    if (worker_is_active(dst) && !is_empty_queue(DFS_WORKER_QUEUE(dst))) {
      worker_set_active(w);
      worker_set_stealer(w);
      return dequeue(DFS_WORKER_QUEUE(dst));
    }
    else {
      dst = worker_next(dst);
    }
  }
  return (LmnWord)NULL;
}


/* ベクタexpandsに積まれたタスクをワーカーmeの隣接ワーカーに全てハンドオフする */
static inline void dfs_handoff_all_task(LmnWorker *me, Vector *expands)
{
  unsigned long i, n;
  LmnWorker *rn = worker_next(me);
  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }

  n = vec_num(expands);
  for (i = 0; i < n; i++) {
    enqueue(DFS_WORKER_QUEUE(rn), vec_get(expands, i));
  }

  ADD_OPEN_PROFILE(sizeof(Node) * n);
}


/* タスクtaskをワーカーmeの隣接ワーカーにハンドオフする */
static inline void dfs_handoff_task(LmnWorker *me, LmnWord task)
{
  LmnWorker *rn = worker_next(me);
  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }
  enqueue(DFS_WORKER_QUEUE(rn), task);

  ADD_OPEN_PROFILE(sizeof(Node));
}

/* ワーカーwが輪の方向に沿って, 他のワーカーから未展開状態を奪いに巡回する.
 * 未展開状態を発見した場合, そのワーカーのキューからdequeueして返す.
 * 発見できなかった場合, NULLを返す */
static inline LmnWord mcdfs_work_stealing(LmnWorker *w)
{
  LmnWorker *dst;

  if(worker_is_explorer(w)) return (LmnWord)NULL;
  dst = worker_next_generator(w);

  while (w != dst) {
    if (worker_is_active(dst) && !is_empty_queue(DFS_WORKER_QUEUE(dst))) {
      worker_set_active(w);
      worker_set_stealer(w);
      return  dequeue(DFS_WORKER_QUEUE(dst));
    }
    else {
      dst = worker_next_generator(dst);
   }
  }
  return (LmnWord)NULL;
}


/* ベクタexpandsに積まれたタスクをワーカーmeの隣接ワーカーに全てハンドオフする */
static inline void mcdfs_handoff_all_task(LmnWorker *me, Vector *expands)
{
  unsigned long i, n;
  LmnWorker *rn = worker_next_generator(me);
   
  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }

  n = vec_num(expands);
  for (i = 0; i < n; i++) {
    enqueue(DFS_WORKER_QUEUE(rn), vec_get(expands, i));
    //enqueue_push_head(DFS_WORKER_QUEUE(rn), vec_get(expands, i));
  }

  ADD_OPEN_PROFILE(sizeof(Node) * n);
}


/* タスクtaskをワーカーmeの隣接ワーカーにハンドオフする */
static inline void mcdfs_handoff_task(LmnWorker *me, LmnWord task)
{
  LmnWorker *rn = worker_next_generator(me);

  if (worker_id(me) > worker_id(rn)) {
    worker_set_black(me);
  }
  enqueue(DFS_WORKER_QUEUE(rn), task);
  //enqueue_push_head(DFS_WORKER_QUEUE(rn), task);

  ADD_OPEN_PROFILE(sizeof(Node));
}


void dfs_start(LmnWorker *w)
{
  LmnWorkerGroup *wp;
  struct Vector new_ss;
  State  *s;
  StateSpace ss;
  
  ss = worker_states(w);
  wp = worker_group(w);
  vec_init(&new_ss, 32);

  if(WORKER_FOR_INIT_STATE(w, s)) {
    s = statespace_init_state(ss);
  } else {
    s = NULL;
  }

  if (!worker_on_parallel(w)) { /* DFS */
#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      push_deq(&DFS_WORKER_DEQUE(w), s, TRUE);
      costed_dfs_loop(w, &DFS_WORKER_DEQUE(w), &new_ss, statespace_automata(ss), statespace_propsyms(ss));
    } else
#endif
    {
      put_stack(&DFS_WORKER_STACK(w), s);
      dfs_loop(w, &DFS_WORKER_STACK(w), &new_ss, statespace_automata(ss), statespace_propsyms(ss));
    }
  }
  else {                        /* Stack-Slicing */
    while (!wp->mc_exit) {
      if (!s && is_empty_queue(DFS_WORKER_QUEUE(w))) {
        worker_set_idle(w);
        if (lmn_workers_termination_detection_for_rings(w)) {
          /* termination is detected! */
          break;
        } else if (worker_on_dynamic_lb(w) ){
          /* 職探しの旅 */
          if(!worker_use_mcndfs(w)) s = (State *)dfs_work_stealing(w);
          else if(worker_is_generator(w)) s = (State *)mcdfs_work_stealing(w);
          else {
              // explorerの仕事無くなったら終了
              printf("\n\n\nexplorer have no work\n\n\n");
              wp->mc_exit=TRUE;
          } 
        }
      } else {
        worker_set_active(w);
        if(!is_empty_queue(DFS_WORKER_QUEUE(w)) &&  !((Queue*)DFS_WORKER_QUEUE(w))->head->next) printf("%u : queue is not empty? %lu\n", worker_id(w), queue_entry_num(DFS_WORKER_QUEUE(w)));
        if (s || (s = (State *)dequeue(DFS_WORKER_QUEUE(w)))) {
          EXECUTE_PROFILE_START();
#ifdef KWBT_OPT
          if (lmn_env.opt_mode != OPT_NONE) {
            push_deq(&DFS_WORKER_DEQUE(w), s, TRUE);
            costed_dfs_loop(w, &DFS_WORKER_DEQUE(w), &new_ss, statespace_automata(ss), statespace_propsyms(ss));
            s = NULL;
            deq_clear(&DFS_WORKER_DEQUE(w));
          } else
#endif
          {
            put_stack(&DFS_WORKER_STACK(w), s);
            if(worker_use_mcndfs(w)) mcdfs_loop(w, &DFS_WORKER_STACK(w), &new_ss, statespace_automata(ss), statespace_propsyms(ss));
            else dfs_loop(w, &DFS_WORKER_STACK(w), &new_ss, statespace_automata(ss), statespace_propsyms(ss));
            s = NULL;
            vec_clear(&DFS_WORKER_STACK(w));
          }
          vec_clear(&new_ss);

          EXECUTE_PROFILE_FINISH();
        }
      }
    }

  }


  printf("%d : exit (total expand=%d)\n", worker_id(w), w->expand);
  if(worker_is_explorer(w) || worker_use_ndfs(w)) printf("red dfs count : %d\n", w->red);


  vec_destroy(&new_ss);
}

static inline void dfs_loop(LmnWorker *w,
                            Vector    *stack,
                            Vector    *new_ss,
                            Automata  a,
                            Vector    *psyms)
{
  while (!vec_is_empty(stack)) {
    State *s;
    AutomataState p_s;
    unsigned int i, n;

    if (workers_are_exit(worker_group(w))) break;

    /** 展開元の状態の取得 */
    s   = (State *)vec_peek(stack);
    p_s = MC_GET_PROPERTY(s, a);
    if (is_expanded(s)) {
      if (NDFS_COND(w, s, p_s)) {
        /** entering second DFS */
          w->red++;
        ndfs_start(w, s);
      }
      else if (MCNDFS_COND(w, s, p_s)) {
        mcndfs_start(w,s);
      }
      pop_stack(stack);
      continue;
    } else if (!worker_ltl_none(w) && atmstate_is_end(p_s)) {
      mc_found_invalid_state(worker_group(w), s);
      pop_stack(stack);
      continue;
    }

    /* サクセッサを展開 */
    mc_expand(worker_states(w), s, p_s, &worker_rc(w), new_ss, psyms, worker_flags(w));

    if (MAP_COND(w)) map_start(w, s);

    if (!worker_on_parallel(w)) { /* Nested-DFS: postorder順を求めるDFS(再度到達した未展開状態がStackに積み直される) */
      set_on_stack(s);
      n = state_succ_num(s);
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, i);

        if (!is_expanded(succ)) {
          put_stack(stack, succ);
        }
      }
    }
    else {/* 並列アルゴリズム使用時 */
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

/* MC-NDFS : 基本的にndfs_loopと同じ。安定したら合併させます。 */
static inline void mcdfs_loop(LmnWorker *w,
                            Vector    *stack,
                            Vector    *new_ss,
                            Automata  a,
                            Vector    *psyms)
{
  while (!vec_is_empty(stack)) {
    State *s;
    AutomataState p_s;
    unsigned int i, n;
    
    if (workers_are_exit(worker_group(w))) break;    

    /** 展開元の状態の取得 */
    s   = (State *)vec_peek(stack);
    p_s = MC_GET_PROPERTY(s, a);
    if (is_expanded(s)) {
      if (MCNDFS_COND(w, s, p_s)) {
         /** entering red DFS **/
         w->red++;
         mcndfs_start(w,s);
      }          
      if(MCNDFS_ALREADY_VISITED(w, s)) {
        pop_stack(stack);
        continue;
      }
    } else if (!worker_ltl_none(w) && atmstate_is_end(p_s)) {       
      mc_found_invalid_state(worker_group(w), s);
      pop_stack(stack);
      continue;
    }

    /* サクセッサを展開 */    
    if(!is_expanded(s)) {
        w->expand++;
        mc_expand(worker_states(w), s, p_s, &worker_rc(w), new_ss, psyms, worker_flags(w));
    }
    
    if (MAP_COND(w)) map_start(w, s);

    /* explorerが到達した状態はblueに着色 */
    if(worker_is_explorer(w)) s_set_blue(s);
    // generatorが到達した状態はredに着色
    else if(worker_is_generator(w)) s_set_red(s);


   /* Nested-DFS: postorder順を求めるDFS(explorerから未到達の状態がStackに積み直される) */
   if (worker_is_explorer(w)) {       
      set_on_stack(s);
      n = state_succ_num(s);
      for (i = 0; i < n; i++) {
        State* succ = state_succ_state(s, i);
        if (!s_is_blue(succ) ) {
          put_stack(stack, succ);
        }
      }
    }
    /* 並列アルゴリズム使用時 MCNDFS使ってる時点で並列前提だけど一応 */
    if(worker_on_parallel(w)) {
      if (DFS_HANDOFF_COND_STATIC(w, stack) /*|| worker_is_explorer(w)*/) {
        mcdfs_handoff_all_task(w, new_ss);
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

/* TODO: DequeとStackが異なるだけでdfs_loopと同じ.
 *   C++ template関数として記述するなど, 保守性向上のための修正が必要 */
void costed_dfs_loop(LmnWorker *w,
                     Deque     *deq,
                     Vector    *new_ss,
                     Automata  a,
                     Vector    *psyms)
{
  while (!deq_is_empty(deq)) {
    State *s;
    AutomataState p_s;
    unsigned int i, n;

    if (workers_are_exit(worker_group(w))) break;

    /** 展開元の状態の取得 */
    s   = (State *)deq_peek_tail(deq);
    p_s = MC_GET_PROPERTY(s, a);

    if ((lmn_env.opt_mode == OPT_MINIMIZE && workers_opt_cost(worker_group(w)) < state_cost(s)) ||
        (is_expanded(s)      && !s_is_update(s)) ||
        (!worker_ltl_none(w) && atmstate_is_end(p_s))) {
      pop_deq(deq, TRUE);
      continue;
    }

    if (!is_expanded(s)) {
      /* サクセッサを展開 */
      mc_expand(worker_states(w), s, p_s, &worker_rc(w), new_ss, psyms, worker_flags(w));
      mc_update_cost(s, new_ss, workers_ewlock(worker_group(w)));
    } else if (s_is_update(s)) {
      mc_update_cost(s, new_ss, workers_ewlock(worker_group(w)));
    }

    if (state_is_accept(a, s)) {
      lmn_update_opt_cost(worker_group(w), s, (lmn_env.opt_mode == OPT_MINIMIZE));
    }

    if (!worker_on_parallel(w)) {
      n = state_succ_num(s);
      for (i = 0; i < n; i++) {
        State *succ = state_succ_state(s, i);
        if (!is_expanded(succ)) {
          push_deq(deq, succ, TRUE);
        } else if (s_is_update(succ)) {
          push_deq(deq, succ, FALSE);
        }
      }
    }
    else {/* 並列アルゴリズム使用時 */
      if (DFS_HANDOFF_COND_STATIC_DEQ(w, deq)) {
        dfs_handoff_all_task(w, new_ss);
      } else {
        n = vec_num(new_ss);
        for (i = 0; i < n; i++) {
          State *new_s = (State *)vec_get(new_ss, i);

          if (DFS_LOAD_BALANCING_DEQ(deq, w, i, n)) {
            dfs_handoff_task(w, (LmnWord)new_s);
          } else {
            if (!is_expanded(new_s)) {
              push_deq(deq, new_s, TRUE);
            } else if (s_is_update(new_s)) {
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


#define BFS_WORKER_OBJ(W)             ((McExpandBFS *)worker_generator_obj(W))
#define BFS_WORKER_OBJ_SET(W, O)      (worker_generator_obj_set(W, O))
#define BFS_WORKER_Q_CUR(W)           (BFS_WORKER_OBJ(W)->cur)
#define BFS_WORKER_Q_NXT(W)           (BFS_WORKER_OBJ(W)->nxt)
#define BFS_WORKER_Q_SWAP(W) do {             \
  Queue *_swap = BFS_WORKER_Q_NXT(W);         \
  BFS_WORKER_Q_NXT(W) = BFS_WORKER_Q_CUR(W);  \
  BFS_WORKER_Q_CUR(W) = _swap;                \
} while (0) /* ポインタの付け替えはatomicに処理されないので並列処理の際には注意 */

static inline void bfs_loop(LmnWorker *w, Vector *new_states, Automata a, Vector *psyms);


/* LmnWorker wにBFSのためのデータを割り当てる */
void bfs_worker_init(LmnWorker *w)
{
  McExpandBFS *mc = LMN_MALLOC(McExpandBFS);

  if (!worker_on_parallel(w)) {
    mc->cur = new_queue();
    mc->nxt = new_queue();
  }
  /* MT */
  else if (worker_id(w) == 0) {
    mc->cur = make_parallel_queue(LMN_Q_MRMW);
    mc->nxt = worker_use_lsync(w) ? make_parallel_queue(LMN_Q_MRMW)
                                  : mc->cur;
  } else {
    /* Layer Queueは全スレッドで共有 */
    mc->cur = BFS_WORKER_Q_CUR(workers_get_worker(worker_group(w), 0));
    mc->nxt = BFS_WORKER_Q_NXT(workers_get_worker(worker_group(w), 0));
  }

  BFS_WORKER_OBJ_SET(w, mc);
}

/* LmnWorkerのBFS固有データを破棄する */
void bfs_worker_finalize(LmnWorker *w)
{
  McExpandBFS *mc = (McExpandBFS *)BFS_WORKER_OBJ(w);
  if (!worker_on_parallel(w)) {
    q_free(mc->cur);
    q_free(mc->nxt);
  } else if (worker_id(w) == 0){
    q_free(mc->cur);
    if (worker_use_lsync(w)) q_free(mc->nxt);
  }
  LMN_FREE(mc);
}


/* BFS Queueが空の場合に真を返す */
BOOL bfs_worker_check(LmnWorker *w)
{
  return is_empty_queue(BFS_WORKER_Q_CUR(w))
      && is_empty_queue(BFS_WORKER_Q_NXT(w));
}


/* WorkerにBFSを割り当てる */
void bfs_env_set(LmnWorker *w)
{
  worker_set_mc_bfs(w);
  w->start    = bfs_start;
  w->check    = bfs_worker_check;
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
  StateSpace ss;

  ss = worker_states(w);
  wp = worker_group(w);
  d = 1;
  d_lim = lmn_env.depth_limits; /* ローカル変数に */
  new_ss = vec_make(32);

  if (!worker_on_parallel(w) || worker_id(w) == 0) { /* 重複して初期状態をenqしないようにするための条件 */
    enqueue(BFS_WORKER_Q_CUR(w),
            (LmnWord)statespace_init_state(ss));
  }

  /* start bfs  */
  if (!worker_on_parallel(w)) {
    /** >>>> 逐次 >>>> */
    while (!wp->mc_exit) {
      /* 1step展開 */
      bfs_loop(w, new_ss, statespace_automata(ss), statespace_propsyms(ss));

      if (BLEDGE_COND(w)) bledge_start(w);

      if (d_lim < ++d || is_empty_queue(BFS_WORKER_Q_NXT(w))) {
        /* 次のLayerが空の場合は探索終了 */
        /* 指定した制限の深さに到達した場合も探索を打ち切る */
        break;
      }

      BFS_WORKER_Q_SWAP(w); /* Layer Queueをswap */
    }
  }
  else if (!worker_use_lsync(w)) {
    /** >>>> 並列(Layer非同期) >>>> */
    while (!wp->mc_exit) {
      if (!is_empty_queue(BFS_WORKER_Q_CUR(w))) {
        EXECUTE_PROFILE_START();
        worker_set_active(w);
        bfs_loop(w, new_ss, statespace_automata(ss), statespace_propsyms(ss));
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
      if (!is_empty_queue(BFS_WORKER_Q_CUR(w))) {
        /**/EXECUTE_PROFILE_START();
        worker_set_active(w);
        bfs_loop(w, new_ss, statespace_automata(ss), statespace_propsyms(ss));
        worker_set_idle(w);
        vec_clear(new_ss);
        /**/EXECUTE_PROFILE_FINISH();
      }

      if (BLEDGE_COND(w)) bledge_start(w);

      BFS_WORKER_Q_SWAP(w);
      lmn_workers_synchronization(w, (void *)lmn_workers_termination_detection_for_rings);
      if (d_lim < ++d || wp->mc_exit || lmn_workers_termination_detection_for_rings(w)) {
        break;
      }
    }
  }

  vec_free(new_ss);
}


static inline void bfs_loop(LmnWorker *w, Vector *new_ss, Automata a, Vector *psyms)
{
  LmnWorkerGroup *wp = worker_group(w);
  while (!is_empty_queue(BFS_WORKER_Q_CUR(w))) { /* # of states@current layer > 0 */
    State *s;
    AutomataState p_s;
    unsigned int i;

    if (workers_are_exit(wp)) return;

    s = (State *)dequeue(BFS_WORKER_Q_CUR(w));

    if (!s) return; /* dequeueはNULLを返すことがある */

    p_s = MC_GET_PROPERTY(s, a);
    if (is_expanded(s)) {
      continue;
    } else if (!worker_ltl_none(w) && atmstate_is_end(p_s)) { /* safety property analysis */
      mc_found_invalid_state(wp, s);
      continue;
    }


    mc_expand(worker_states(w), s, p_s, &worker_rc(w), new_ss, psyms, worker_flags(w));

    if (MAP_COND(w)) map_start(w, s);
    else if (BLEDGE_COND(w)) bledge_store_layer(w, s);

    if (state_succ_num(s) == 0) {
      if (lmn_env.nd_search_end) {
        /* 最終状態探索モードの場合, 発見次第探索を打ち切る */
        workers_set_exit(wp);
        return;
      }
    } else {
      /* 展開した状態をnext layer queueに登録する */
      for (i = 0; i < vec_num(new_ss); i++) {
        enqueue(BFS_WORKER_Q_NXT(w), (LmnWord)vec_get(new_ss, i));
      }
    }

    vec_clear(new_ss);
  }
}
