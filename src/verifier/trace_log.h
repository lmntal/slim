/*
 * trace_log.h
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 */

#ifndef LMN_TRACE_LOG_H
#define LMN_TRACE_LOG_H

#ifdef __cplusplus
extern "C" {
#endif


#include "vm/vm.h"


#ifndef PROC_TBL_DEFAULT_SIZE
#define PROC_TBL_DEFAULT_SIZE  128U
#endif

#ifndef PROC_TBL_BUCKETS_SIZE
#define PROC_TBL_BUCKETS_SIZE  (1 << 12) // heuristics
#endif


/*----------------------------------------------------------------------
 * TraceLog
 * --------
 * - struct LogTracker
 * - struct TraceLog
 * - struct SimpleTraceLog
 *
 * VisitLogの改良版.
 * データ構造をグラフ同形成判定に特化させたため, VisitLogと比較すると汎用性は低め.
 */


/* ---
 * Tracker周り
 * 訪問済みテーブルの状態をバックトラックさせるためのデータ構造
 */

struct LogTracker {
  struct Vector traced_ids, btp_idx;
};

/* 前回のトレースプロセス数を基に, ログを巻き戻す. */
#define LogTracker_REVERT(Tracker, UnputFunc, Tbl)                             \
  do {                                                                         \
    unsigned long __bt_idx;                                                    \
    LMN_ASSERT(!vec_is_empty(&((Tracker)->btp_idx)));                          \
                                                                               \
    /* 前回セットしたトレース数のメモを取り出す */                                     \
    __bt_idx = vec_pop(&((Tracker)->btp_idx));                                 \
    /* bt_idxより上のidxに積まれたデータをログ上からunputしていく */                    \
    while (vec_num(&((Tracker)->traced_ids)) > __bt_idx) {                     \
      LmnWord __key = vec_pop(&((Tracker)->traced_ids));                       \
      UnputFunc(Tbl, __key);                                                   \
    }                                                                          \
  } while (0)

/* 現在のトレースプロセス数をログlにメモしておく */
#define LogTracker_PUSH(TR) (vec_push(&(TR)->btp_idx, vec_num(&(TR)->traced_ids)))
/* 最も最近にメモしたトレースプロセス数の記録を削除する */
#define LogTracker_POP(TR)  (vec_pop(&(TR)->btp_idx))
/* プロセスID集合にIDを追加する */
#define LogTracker_TRACE(TR, ID) (vec_push(&(TR)->traced_ids, (ID)))

/* ----
 * TraceLog
 * グラフ同形成判定用データ構造本体
 */

/** MEMO:
 *  非TIME-OPT版を実装していない
 */

/* 各{シンボルアトム, inside proxyアトム, 膜}に対して1つずつ対応させるデータ構造
 * outside proxyアトムは含めない */
struct TraceData { /* 64bit: 24Bytes (32bit: 16Bytes) */
  BYTE flag;                   /* 対応させているデータの種類を示すフラグ */
  unsigned int traversed_proc; /* 膜に対応させている場合は, その膜内で訪問した
                                   {シンボルアトム, inside proxyアトム, 子膜}の総数
                                  を記録している.
                                  他のデータに対応させている場合は0のまま */
  ProcessID owner_id;          /* 対応させているデータ構造の所属膜のID.
                                * プロセスIDは1から始まるため,
                                * 所属膜がない(例えばグローバルルート膜の)場合は, 0 */
  ProcessID matched;           /* 対応させているプロセスとマッチさせたプロセスのID.
                                * in-proxyアトムはBS encode時の訪問順序に数えないため,
                                * in-proxyアトムへの対応としては0をセット */
};

struct TraceLog {
  int cap, num;
  int num_buckets;
  struct TraceData **tbl;
  struct LogTracker tracker;
};

typedef struct TraceLog *TraceLogRef;

#define TLOG_MATCHED_ID_NONE       (0U)

void tracker_init(struct LogTracker *track);
void tracker_destroy(struct LogTracker *track);

/**
 * Function ProtoTypes
 */

TraceLogRef tracelog_make(void);
void tracelog_free(TraceLogRef trc);
void tracelog_init(TraceLogRef trc);
void tracelog_init_with_size(TraceLogRef trc, unsigned long size);
void tracelog_destroy(TraceLogRef trc);

BOOL tracelog_eq_traversed_proc_num(TraceLogRef      l,
                                    LmnMembraneRef   owner,
                                    AtomListEntryRef in_ent,
                                    AtomListEntryRef avoid);
void tracelog_tbl_expand(TraceLogRef l, unsigned long new_size);
int  tracelog_put(TraceLogRef l, LmnWord key, LmnWord matched_id,
                  LmnMembraneRef owner);
int  tracelog_put_atom(TraceLogRef l, LmnSymbolAtomRef atom1, LmnWord atom2_id,
                       LmnMembraneRef owner1);
int  tracelog_put_mem(TraceLogRef l, LmnMembraneRef mem1, LmnWord mem2_id);
int  tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id);
void tracelog_unput(TraceLogRef l, LmnWord key);
BOOL tracelog_contains(TraceLogRef l, LmnWord key);
BOOL tracelog_contains_atom(TraceLogRef l, LmnSymbolAtomRef atom);
BOOL tracelog_contains_mem(TraceLogRef l, LmnMembraneRef mem);
BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) ;
LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key);
LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSymbolAtomRef atom);
LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembraneRef mem);
LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl);
void tracelog_backtrack(TraceLogRef l);
void tracelog_set_btpoint(TraceLogRef l);
void tracelog_continue_trace(TraceLogRef l);
BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key);

#ifdef __cplusplus
}
#endif


#endif /* LMN_TRACE_LOG_H */
