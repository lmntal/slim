/*
 * runtime_status.h
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

#ifndef RUNTIME_STATUS_H
#define RUNTIME_STATUS_H

/**
 * @ingroup  Verifier
 * @defgroup Runtime
 * @{
 */

#include "element/element.h"
#include "lmntal.h"
#include "mc_worker.h"
#include "vm/vm.h"

typedef struct MCProfiler2 MCProfiler2;
typedef struct MCProfiler3 MCProfiler3;
typedef struct MemoryProfiler MemoryProfiler;
typedef struct TimeProfiler TimeProfiler;
typedef struct RuleProfiler RuleProfiler;
typedef struct PeakCounter PeakCounter;

enum PROFILE_SPACE {
  PROFILE_SPACE__TOTAL,          /* トータル */
  PROFILE_SPACE__STATE_BINSTR,   /* バイナリストリング */
  PROFILE_SPACE__STATE_OBJECT,   /* struct State */
  PROFILE_SPACE__TRANS_OBJECT,   /* struct Transition + sizeof(succ_data_t) *
                                    successors_num */
  PROFILE_SPACE__STATE_MEMBRANE, /* 膜 */
  PROFILE_SPACE__OPEN_LIST,      /* オープンノードリスト */
  PROFILE_SPACE__REDUCED_MEMSET, /* バイト列圧縮により状態管理表から削除した階層グラフの総メモリ量
                                  */
  PROFILE_SPACE__REDUCED_BINSTR, /* 削減したバイナリストリングの総メモリ量 */
  PSPACE_TAIL,                   /* dummy */
};

enum PROFILE_TIME {
  PROFILE_TIME__ACTIVE_FOR_IDLE_PROF = 0, /* アイドルCPU時間を計測するためのトータルアクティブCPU時間
                                             (並列処理用) */
  PROFILE_TIME__STATE_HASH_MEM,           /* 膜のハッシュ関数 */
  PROFILE_TIME__STATE_HASH_MID,           /* 膜のIDのハッシュ関数 */
  PROFILE_TIME__STATE_COMPARE_MEQ, /* グラフ同型性判定による状態の等価性判定 */
  PROFILE_TIME__STATE_COMPARE_CL,  /* canonical labelの比較 */
  PROFILE_TIME__TRIEMCKAY,	   /* trieMckay */
  PROFILE_TIME__LISTMCKAY,
  PROFILE_TIME__LISTMCKAY_INNER,
  PROFILE_TIME__INSERTDP,
  PROFILE_TIME__REFINE,
  PROFILE_TIME__PUTLABELS,
  PROFILE_TIME__CLASSIFY,
  PROFILE_TIME__MAKELABEL,
  PROFILE_TIME__TRIEPROPAGATE,	   /* triePropagate */
  PROFILE_TIME__STATE_COMPARE_MID, /* 膜のID比較による状態の等価性判定 */
  PROFILE_TIME__STATE_COPY,           /* 膜の複製(INSTR_COMMITを除く) */
  PROFILE_TIME__STATE_COPY_IN_COMMIT, /* 膜の複製(INSTR_COMMIT命令内部) */
  PROFILE_TIME__TRANS_RULE, /* ルール適用 (INSTR_COMMIT命令を*含む*) */
  PROFILE_TIME__MENC_DUMP, /* 膜からバイナリストリングへのエンコード */
  PROFILE_TIME__MENC_RESTORE, /* バイナリストリングから膜への復元 */
  PROFILE_TIME__MENC_CANONICAL, /* 膜に対して一意なIDへのエンコード */
  PROFILE_TIME__DMEM_COMMIT,     /* 差分情報の適用 */
  PROFILE_TIME__DMEM_REVERT,     /* 差分情報適用の取り消し */
  PROFILE_TIME__CYCLE_EXPLORE,   /* 受理サイクル探索 */
  PROFILE_TIME__Z_COMPRESS,      /* compression using z library */
  PROFILE_TIME__Z_UNCOMPRESS,    /* uncompression using z library */
  PROFILE_TIME__D_COMPRESS,      /* compression using zdelta lib */
  PROFILE_TIME__D_UNCOMPRESS,    /* uncompression using zdelta lib */
  PROFILE_TIME__TREE_COMPRESS,   /* tree compression */
  PROFILE_TIME__TREE_UNCOMPRESS, /* tree uncompression */
  PROFILE_TIME__COST_UPDATE,     /* 最適化実行 */
  PROFILE_TIME__LOCK,            /* ロックされている時間 */
  PROFILE_TIME__REPAIR,          /* (MCNDFS)Red DFSのRepair Phaseの時間 */
  PTIME_TAIL,                    /* dummy */
};

enum PROFILE_HTABLE_COUNT {
  PROFILE_COUNT__HASH_CONFLICT_ENTRY = 0, /* 衝突したエントリ数 */
  PROFILE_COUNT__HASH_CONFLICT_HASHV,     /* エントリが衝突し,
                                             更にハッシュ値が衝突した回数 */
  PROFILE_COUNT__HASH_RESIZE_TRIAL,   /* テーブル拡張の試行回数 */
  PROFILE_COUNT__HASH_RESIZE_APPLY,   /* テーブル拡張の適用回数 */
  PROFILE_COUNT__HASH_FAIL_TO_INSERT, /* 並列時,
                                         競合によってエントリの追加に失敗した回数
                                       */
  PCOUNT_TAIL,                        /* dummy */
};

struct PeakCounter {
  long cur;  /* 現在の数 */
  long peak; /* ピーク値 */
};

struct MemoryProfiler {
  PeakCounter num;   /* オブジェクト数 */
  PeakCounter space; /* 利用メモリ量 */
};

struct TimeProfiler {
  unsigned long called_num; /* # of calls */
  double total_time;        /* total time */
  double tmp_start;
};

struct MCProfiler2 {
  unsigned long invalid_end_num, accept_num, transition_num, mhash_num,
      rehashed_num, midhash_num;
  unsigned long statespace_space, transition_space, state_space, binstr_space,
      membrane_space;
  st_table_t hashes;
};

struct MCProfiler3 {
  TimeProfiler times[PTIME_TAIL];     /* 処理毎の所用時間の調査 */
  MemoryProfiler spaces[PSPACE_TAIL]; /* メモリ使用量の調査 */
  unsigned long
      counters[PCOUNT_TAIL]; /* カウンタ群, 主に状態管理票の調査に使う */
};

struct LmnProfiler {
  BOOL valid;
  BOOL has_property; /* プロファイル出力用にこちらの領域にもメモしておく */
  BOOL found_err;
  unsigned int thread_num;

  /** 全体の実行時間と, 全体から前処理と後処理を除いた実行時間 */
  double start_wall_time, end_wall_time;
  double start_cpu_time, end_cpu_time;
  double start_wall_time_main, end_wall_time_main;
  double *start_cpu_time_main, *end_cpu_time_main;
  double *thread_cpu_time_main;

  /* TODO: 以下のデータ群はMCProfilerの中に移し,
           実行時間, RTPProfiler, MCProfilerをメンバとした方が分かりやすい  */

  /** 状態空間のプロファイル用データ群
   */

  /* for profile level1
   * 実行終了後(開放前)にStateSpaceから情報を受け取る. */
  unsigned long state_num_stored, state_num_end, error_num;

  /* for profile level2
   * 実行終了後(開放前)にStateSpace内の各状態から情報収集する */
  MCProfiler2 *lv2;

  /* for profile level3 (configure --enable-profile)
   * 実行中に, 詳細に各状態から情報収集する.
   * (実行性能に影響するためベンチマークテストの際には使用しない) */
  MCProfiler3 *lv3; /* for verifier only */
  struct RuleProfiler *cur;
  st_table_t prules; /* Set of Rule Profiler */
};

/* RuleProfiler Interface */
void ruleprofiler_incr_backtrack(struct RuleProfiler *p);
void ruleprofiler_add_backtrack(struct RuleProfiler *p, int num);
void ruleprofiler_incr_apply(struct RuleProfiler *p);
TimeProfiler *ruleprofiler_trial(struct RuleProfiler *p);

extern struct LmnProfiler lmn_prof;

void lmn_profiler_init(unsigned int threads_num);
void lmn_profiler_finalize(void);
void profile_start_slim(void);
void profile_finish_slim(void);
void profile_start_exec(void);
void profile_finish_exec(void);
void profile_start_exec_thread(void);
void profile_finish_exec_thread(void);
void dump_profile_data(FILE *f);
void profile_statespace(LmnWorkerGroup *wp);
void profile_total_space_update(StateSpaceRef ss);

void profile_finish_timer(int type);
void profile_start_timer(int type);
void profile_remove_space(int type, unsigned long size);
void profile_add_space(int type, unsigned long size);
void profile_countup(int type);
void profile_peakcounter_pop(PeakCounter *p, unsigned long size);
void profile_rule_obj_set(LmnRuleSetRef src, LmnRuleRef r);

void time_profiler_start(TimeProfiler *p);
void time_profiler_finish(TimeProfiler *p);

#ifdef PROFILE
#define profile_backtrack()                                                    \
  if (lmn_prof.cur && !lmn_env.findatom_parallel_mode)                         \
  (ruleprofiler_incr_backtrack(lmn_prof.cur))
#define profile_backtrack_add(NUM)                                             \
  if (lmn_prof.cur)                                                            \
  (ruleprofiler_add_backtrack(lmn_prof.cur, NUM))
#define profile_start_trial()                                                  \
  if (lmn_prof.cur)                                                            \
  time_profiler_start(ruleprofiler_trial(lmn_prof.cur))
#define profile_finish_trial()                                                 \
  if (lmn_prof.cur)                                                            \
  time_profiler_finish(ruleprofiler_trial(lmn_prof.cur))
#define profile_apply()                                                        \
  if (lmn_prof.cur)                                                            \
  (ruleprofiler_incr_apply(lmn_prof.cur))
#else
#define profile_backtrack()
#define profile_backtrack_add(NUM)
#define profile_start_trial()
#define profile_finish_trial()
#define profile_apply()
#endif

/* @} */

#endif
