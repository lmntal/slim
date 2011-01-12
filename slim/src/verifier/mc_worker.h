/*
 * parallel.h
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

#ifndef LMN_MC_WORKER_H
#define LMN_MC_WORKER_H

#include "lmntal.h"
#include "statespace.h"
#include "state.h"
#include "queue.h"
#include "lmntal_thread.h"


typedef struct LmnWorkerPool *LmnWorkerPool;
typedef struct LmnWorker     LmnWorker;

struct LmnWorkerPool {
  BOOL         terminated;
  unsigned int worker_num;
  LmnWorker    **workers;
};

struct LmnWorker {
  lmn_thread_t    pth;           /* スレッド識別子(pthread_t) */
  unsigned long   id;            /* Natural integer id (lmn_thread_id) */

  BOOL            f_end;         /* Workerの終了検知判定用フラグ. 任意のWorkerが操作可能 */
  BOOL            f_end2;        /* Workerの終了検知判定用フラグ. 任意のWorkerが操作可能 */
  BYTE            f_safe;        /* Workerに割り当てられたスレッドのみWritableなフラグ */
  BYTE            f_exec;        /* 実行時オプションをローカルに記録 */
  BYTE            type;
  BYTE            type2;
  /* 隙間が2BYTE */

  void           *obj, *obj2;    /* 任意のオブジェクト */
  void          (*init)( ),
                (*init2)( );     /* objの初期化関数 */
  void          (*finalize)( ),
                (*finalize2)( ); /* objの後始末関数 */

  void          (*start)( );     /* 実行関数 */
  BOOL          (*check)( );     /* 終了検知関数 */

  StateSpace      states;        /* Pointer to StateSpace */
  struct ReactCxt cxt;           /* ReactContext Object */
  LmnWorker *next;               /* Pointer to Neighbor Worker */
};


/** Macros
 */
#define WORKER_ACTIVE_MASK            (0x01U)
#define WORKER_WAITING_MASK           (0x01U << 1)

#define WORKER_UNSET_ACTIVE(W)        ((W)->f_safe &= (~WORKER_ACTIVE_MASK))
#define WORKER_SET_IDLE(W)            (WORKER_UNSET_ACTIVE(W))
#define WORKER_SET_ACTIVE(W)          ((W)->f_safe |= WORKER_ACTIVE_MASK)
#define WORKER_IS_ACTIVE(W)           ((W)->f_safe &  WORKER_ACTIVE_MASK)
#define WORKER_IS_IDLE(W)             (!(WORKER_IS_ACTIVE(W)))

#define WORKER_SET_BLACK(W)           ((W)->f_end = FALSE)
#define WORKER_SET_WHITE(W)           ((W)->f_end = TRUE)
#define WORKER_IS_WHITE(W)            ((W)->f_end)

#define WORKER_SET_STEALER(W)         ((W)->f_end2 = TRUE)
#define WORKER_UNSET_STEALER(W)       ((W)->f_end2 = FALSE)
#define WORKER_IS_STEALER(W)          ((W)->f_end2)

#define WORKER_PRIMARY_ID             (0U)

#define WORKER_ID(W)                  ((W)->id)
#define WORKER_PID(W)                 ((W)->pth)
#define WORKER_FLAGS(W)               ((W)->f_exec)
#define WORKER_SET_FLAGS(W, F)        ((W)->f_exec |= (F))
#define WORKER_STATESPACE(W)          ((W)->states)
#define WORKER_NEXT_WORKER(W)         ((W)->next)
#define WORKER_RC(W)                  ((W)->cxt)
#define WORKER_SET_OBJ(W, O)          ((W)->obj  = (void *)(O))
#define WORKER_SET_OBJ_SUB(W, O)      ((W)->obj2 = (void *)(O))
#define WORKER_OBJ(W)                 ((W)->obj)
#define WORKER_OBJ_SUB(W)             ((W)->obj2)
#define WORKER_OBJ_TYPE(W)            ((W)->type)
#define WORKER_OBJ_SUB_TYPE(W)        ((W)->type2)

#define WORKER_INIT(W)              do {                                       \
                                     if ((W)->init)  (*(W)->init)(W);          \
                                     if ((W)->init2) (*(W)->init2)(W);         \
                                    } while (0)
#define WORKER_FINALIZE(W)          do {                                       \
                                     if ((W)->finalize)  (*(W)->finalize)(W);  \
                                     if ((W)->finalize2) (*(W)->finalize2)(W); \
                                    } while (0)

#define WORKER_START(W)               if ((W)->start) (*(W)->start)(W)
#define WORKER_CHECK(W)               ((W)->check ? (*(W)->check)(W) : TRUE)


/* LmnWorker: 状態空間構築に関するオプション */
#define WORKER_F0_MC_DUMP_MASK        (0x01U)
#define WORKER_F0_MC_POR_MASK         (0x01U << 1)
#define WORKER_F0_MC_PROP_MASK        (0x01U << 2)
#define WORKER_F0_MC_TRANS_MASK       (0x01U << 3)
#define WORKER_F0_MC_COMPRESS_MASK    (0x01U << 4)
#define WORKER_F0_MC_COMPACT_MASK     (0x01U << 5)
#define WORKER_F0_MC_DELTA_MASK       (0x01U << 6)

#define mc_is_dump(F)                 ((F) &   WORKER_F0_MC_DUMP_MASK)
#define mc_set_dump(F)                ((F) |=  WORKER_F0_MC_DUMP_MASK)
#define mc_enable_por(F)              ((F) &   WORKER_F0_MC_POR_MASK)
#define mc_set_por(F)                 ((F) |=  WORKER_F0_MC_POR_MASK)
#define mc_has_property(F)            ((F) &   WORKER_F0_MC_PROP_MASK)
#define mc_set_property(F)            ((F) |=  WORKER_F0_MC_PROP_MASK)
#define mc_has_trans(F)               ((F) &   WORKER_F0_MC_TRANS_MASK)
#define mc_set_trans(F)               ((F) |=  WORKER_F0_MC_TRANS_MASK)
#define mc_use_delta(F)               ((F) &   WORKER_F0_MC_DELTA_MASK)
#define mc_set_delta(F)               ((F) |=  WORKER_F0_MC_DELTA_MASK)
#define mc_use_compress(F)            ((F) &   WORKER_F0_MC_COMPRESS_MASK)
#define mc_set_compress(F)            ((F) |=  WORKER_F0_MC_COMPRESS_MASK)
#define mc_use_compact(F)             ((F) &   WORKER_F0_MC_COMPACT_MASK)
#define mc_set_compact(F)             ((F) |=  WORKER_F0_MC_COMPACT_MASK)


#define WORKER_F1_PARALLEL_MASK       (0x01U)
#define WORKER_F1_DYNAMIC_LB_MASK     (0x01U << 1)
#define WORKER_F1_MC_DFS_MASK         (0x01U << 2)
#define WORKER_F1_MC_BFS_MASK         (0x01U << 3)
#define WORKER_F1_MC_BFS_LSYNC_MASK   (0x01U << 4)
#define WORKER_F1_MC_OPT_SCC_MASK     (0x01U << 5)

#define mc_on_parallel(F)             ((F) &  WORKER_F1_PARALLEL_MASK)
#define mc_set_parallel(F)            ((F) |= WORKER_F1_PARALLEL_MASK)
#define mc_on_dynamic_lb(F)           ((F) &  WORKER_F1_DYNAMIC_LB_MASK)
#define mc_set_dynamic_lb(F)          ((F) |= WORKER_F1_DYNAMIC_LB_MASK)
#define mc_on_dfs(F)                  ((F) &  WORKER_F1_MC_DFS_MASK)
#define mc_set_dfs(F)                 ((F) |= WORKER_F1_MC_DFS_MASK)
#define mc_on_bfs(F)                  ((F) &  WORKER_F1_MC_BFS_MASK)
#define mc_set_bfs(F)                 ((F) |= WORKER_F1_MC_BFS_MASK)
#define mc_use_lsync(F)               ((F) &  WORKER_F1_MC_BFS_LSYNC_MASK)
#define mc_set_lsync(F)               ((F) |= WORKER_F1_MC_BFS_LSYNC_MASK)
#define mc_use_opt_scc(F)             ((F) &  WORKER_F1_MC_OPT_SCC_MASK)
#define mc_set_opt_scc(F)             ((F) |= WORKER_F1_MC_OPT_SCC_MASK)

#define worker_on_parallel(W)         (mc_on_parallel(WORKER_OBJ_TYPE(W)))
#define worker_set_parallel(W)        (mc_set_parallel(WORKER_OBJ_TYPE(W)))
#define worker_on_dynamic_lb(W)       (mc_on_dynamic_lb(WORKER_OBJ_TYPE(W)))
#define worker_set_dynamic_lb(W)      (mc_set_dynamic_lb(WORKER_OBJ_TYPE(W)))
#define worker_on_mc_dfs(W)           (mc_on_dfs(WORKER_OBJ_TYPE(W)))
#define worker_set_mc_dfs(W)          (mc_set_dfs(WORKER_OBJ_TYPE(W)))
#define worker_on_mc_bfs(W)           (mc_on_bfs(WORKER_OBJ_TYPE(W)))
#define worker_set_mc_bfs(W)          (mc_set_bfs(WORKER_OBJ_TYPE(W)))
#define worker_use_lsync(W)           (mc_use_lsync(WORKER_OBJ_TYPE(W)))
#define worker_set_lsync(W)           (mc_set_lsync(WORKER_OBJ_TYPE(W)))
#define worker_use_opt_scc(W)         (mc_use_opt_scc(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_opt_scc(W)         (mc_set_opt_scc(WORKER_OBJ_SUB_TYPE(W)))


#define WORKER_F2_MC_NDFS_MASK        (0x01U)
#define WORKER_F2_MC_OWCTY_MASK       (0x01U << 1)
#define WORKER_F2_MC_MAP_MASK         (0x01U << 2)
#define WORKER_F2_MC_MAP_WEAK_MASK    (0x01U << 3)
#define WORKER_F2_MC_BLE_MASK         (0x01U << 4)
#define WORKER_F2_MC_MCNDFS_MASK      (0x01U << 5)
#define WORKER_F2_MC_MCNDFS_WEAK_MASK (0x01U << 6)

#define mc_ltl_none(F)                (F == 0x00U)
#define mc_use_ndfs(F)                ((F) &  WORKER_F2_MC_NDFS_MASK)
#define mc_set_ndfs(F)                ((F) |= WORKER_F2_MC_NDFS_MASK)
#define mc_use_owcty(F)               ((F) &  WORKER_F2_MC_OWCTY_MASK)
#define mc_set_owcty(F)               ((F) |= WORKER_F2_MC_OWCTY_MASK)
#define mc_use_map(F)                 ((F) &  WORKER_F2_MC_MAP_MASK)
#define mc_set_map(F)                 ((F) |= WORKER_F2_MC_MAP_MASK)
#define mc_use_weak_map(F)            ((F) &  WORKER_F2_MC_MAP_WEAK_MASK)
#define mc_set_weak_map(F)            ((F) |= WORKER_F2_MC_MAP_WEAK_MASK)
#define mc_use_ble(F)                 ((F) &  WORKER_F2_MC_BLE_MASK)
#define mc_set_ble(F)                 ((F) |= WORKER_F2_MC_BLE_MASK)
#define mc_use_mcndfs(F)              ((F) &  WORKER_F2_MC_MCNDFS_MASK)
#define mc_set_mcndfs(F)              ((F) |= WORKER_F2_MC_MCNDFS_MASK)
#define mc_use_mcndfs_weak(F)         ((F) &  WORKER_F2_MC_MCNDFS_WEAK_MASK)
#define mc_set_mcndfs_weak(F)         ((F) |= WORKER_F2_MC_MCNDFS_WEAK_MASK)

#define worker_ltl_none(W)            (mc_ltl_none(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_ndfs(W)            (mc_use_ndfs(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_ndfs(W)            (mc_set_ndfs(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_owcty(W)           (mc_use_owcty(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_owcty(W)           (mc_set_owcty(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_map(W)             (mc_use_map(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_map(W)             (mc_set_map(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_weak_map(W)        (mc_use_weak_map(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_weak_map(W)        (mc_set_weak_map(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_ble(W)             (mc_use_ble(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_ble(W)             (mc_set_ble(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_mcndfs(W)          (mc_use_mcndfs(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_mcndfs(W)          (mc_set_mcndfs(WORKER_OBJ_SUB_TYPE(W)))
#define worker_use_mcndfs_weak(W)     (mc_use_mcndfs_weak(WORKER_OBJ_SUB_TYPE(W)))
#define worker_set_mcndfs_weak(W)     (mc_set_mcndfs_weak(WORKER_OBJ_SUB_TYPE(W)))


/** ProtoTypes
 */
void lmn_workers_env_init(StateSpace states, BOOL flags);
void lmn_workers_env_finalize(void);
void launch_lmn_workers(void);
BOOL lmn_workers_termination_detection_for_rings(LmnWorker *root);
void lmn_workers_synchronization(LmnWorker *root,
                                 unsigned int id,
                                 void (*func)(LmnWorker *w));
inline LmnWorker *lmn_worker_make_minimal(void);
LmnWorker *lmn_worker_make(StateSpace     ss,
                           unsigned long  id,
                           BOOL           flags);
void lmn_worker_free(LmnWorker *w);
LmnWorker *workerpool_get_my_worker(void);
inline LmnWorker *workerpool_get_worker(unsigned long id);

#endif
