/*
 * trace_log.c
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

#include "trace_log.h"



#define TLOG_FLAG(V)               ((V).flag)
#define TLOG_NUM(V)                ((V).traversed_proc)
#define TLOG_OWNER(V)              ((V).owner_id)
#define TLOG_MATCHED(V)            ((V).matched)
#define TLOG_TRV_INC(V)            (TLOG_NUM(V)++)
#define TLOG_TRV_DEC(V)            (TLOG_NUM(V)--)
#define TLOG_SET_OWNER(V, N)       (TLOG_OWNER(V) = lmn_mem_id(N))
#define TLOG_SET_MATCHED(V, ID)    (TLOG_MATCHED(V) = (ID))

#define TLOG_DATA_CLEAR(V)                                                     \
    do {                                                                       \
      tracelog_entry(l, key).flag = TLOG_INIT_DATA;                                       \
      tracelog_entry(l, key).owner_id = 0;                                                \
      tracelog_entry(l, key).matched = 0;                                                 \
    } while (0)


#define TLOG_INIT_DATA             (0x0U)
#define TLOG_TRAVERSED_ATOM        (0x1U)
#define TLOG_TRAVERSED_MEM         (0x2U)
#define TLOG_TRAVERSED_HLINK       (0x3U)
#define TLOG_TRAVERSED_OTHERS      (0xfU)

#define TLOG_SET_TRV_ATOM(F)       (F = TLOG_TRAVERSED_ATOM)
#define TLOG_SET_TRV_MEM(F)        (F = TLOG_TRAVERSED_MEM)
#define TLOG_SET_TRV_HLINK(F)      (F = TLOG_TRAVERSED_HLINK)
#define TLOG_SET_TRV_SOME(F)       (F = TLOG_TRAVERSED_OTHERS)
#define TLOG_IS_TRV(F)             (F != TLOG_INIT_DATA)
#define TLOG_UNSET_TRV(F)          (F = TLOG_INIT_DATA)

#define tracelog_bucket(T, K) ((T)->tbl[(K) / PROC_TBL_BUCKETS_SIZE])
#define tracelog_idx_in_bucket(T, K) ((K) % PROC_TBL_BUCKETS_SIZE)
#define tracelog_entry(T, K) (tracelog_bucket(T, K)[tracelog_idx_in_bucket(T, K)])



/*------------
 * TraceLog
 */

TraceLogRef tracelog_make(void)
{
  struct TraceLog *l = LMN_MALLOC(struct TraceLog);
  tracelog_init(l);
  return l;
}


void tracelog_init(TraceLogRef l)
{
  tracelog_init_with_size(l, PROC_TBL_DEFAULT_SIZE);
}


void tracelog_init_with_size(TraceLogRef l, unsigned long size)
{
  l->cap = size;
  l->num = 0;
  l->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
  l->tbl = LMN_CALLOC(struct TraceData *, l->num_buckets);
  tracker_init(&l->tracker);
}


void tracelog_free(TraceLogRef l)
{
  tracelog_destroy(l);
  LMN_FREE(l);
}

void tracelog_destroy(TraceLogRef l)
{
  for (int i = 0; i < l->num_buckets; i++) {
    LMN_FREE(l->tbl[i]);
  }
  LMN_FREE(l->tbl);
  tracker_destroy(&l->tracker);
}

/*----------------
 * Tracker
 */

void tracker_init(struct LogTracker *track)
{
  vec_init(&track->traced_ids, PROC_TBL_DEFAULT_SIZE);
  vec_init(&track->btp_idx, PROC_TBL_DEFAULT_SIZE);
}

void tracker_destroy(struct LogTracker *track)
{
  vec_destroy(&track->traced_ids);
  vec_destroy(&track->btp_idx);
}



/* 膜ownerを対象として訪問済みにしたプロセス (シンボルアトム + 子膜 + inside proxies) の数が
 * 膜ownerのそれと一致しているか否かを返す */
BOOL tracelog_eq_traversed_proc_num(TraceLogRef      l,
                                                  LmnMembraneRef   owner,
                                                  AtomListEntryRef in_ent,
                                                  AtomListEntryRef avoid)
{
  return
      (tracelog_bucket(l, lmn_mem_id(owner)) ? TLOG_NUM(tracelog_entry(l, lmn_mem_id(owner))) : 0) ==
          (lmn_mem_symb_atom_num(owner)
              + lmn_mem_child_mem_num(owner)
              + atomlist_get_entries_num(in_ent)
              - atomlist_get_entries_num(avoid));
}

/* トレースログlのテーブルサイズをnew_size以上の大きさに拡張する. */
void tracelog_tbl_expand(TraceLogRef l, unsigned long new_size)
{
  unsigned int org_n = l->num_buckets;

  while (l->cap <= new_size) l->cap *= 2;
  l->num_buckets = l->cap / PROC_TBL_BUCKETS_SIZE + 1;
  if (org_n < l->num_buckets) {
    l->tbl = LMN_REALLOC(struct TraceData *, l->tbl, l->num_buckets);
    memset(l->tbl + org_n, 0, sizeof(struct TraceData *) * (l->num_buckets - org_n));
  }

  unsigned int b = new_size / PROC_TBL_BUCKETS_SIZE;
  if (b < l->num_buckets && l->tbl[b]) return;
  l->tbl[b] = LMN_NALLOC(struct TraceData, PROC_TBL_BUCKETS_SIZE);
  memset(l->tbl[b], TLOG_INIT_DATA, sizeof(struct TraceData) * PROC_TBL_BUCKETS_SIZE);
}

/* ログl上のキーkey位置にあるTraceLogに対して, 訪問を記録する.
 * matched_idはマッチしたプロセスのID, ownerは所属膜.
 * 所属膜がNULLでない場合は, 所属膜の情報を記録し, 所属膜側のプロセス訪問カウンタを回す.
 * tracelog_put_atom, tracelog_put_memから呼び出す関数であり, 直接callしないこと.
 * (呼出しコスト削減のために公開関数としているだけ) */
int tracelog_put(TraceLogRef l, LmnWord key, LmnWord matched_id,
                               LmnMembraneRef owner) {
  tracelog_tbl_expand(l, key);

  if (TLOG_IS_TRV(TLOG_FLAG(tracelog_entry(l, key)))) {
    return 0;
  }

  l->num++;
  TLOG_SET_MATCHED(tracelog_entry(l, key), matched_id);

  if (owner) {
    TLOG_SET_OWNER(tracelog_entry(l, key), owner);
    LMN_ASSERT(l->cap > lmn_mem_id(owner));
    tracelog_tbl_expand(l, lmn_mem_id(owner));
    TLOG_TRV_INC(tracelog_entry(l, lmn_mem_id(owner)));
  }

  LogTracker_TRACE(&l->tracker, key);

  return 1;
}

/* ログlに, 所属膜owner1のアトムatom1への訪問を記録する.
 * atom1にマッチしたアトムのプロセスIDもしくは訪問番号atom2_idを併せて記録する. */
int tracelog_put_atom(TraceLogRef l, LmnSymbolAtomRef atom1, LmnWord  atom2_id,
                                    LmnMembraneRef owner1) {
  int ret = tracelog_put(l, LMN_SATOM_ID(atom1), atom2_id, owner1);
  TLOG_SET_TRV_ATOM(TLOG_FLAG(tracelog_entry(l, LMN_SATOM_ID(atom1))));
  return ret;
}

/* ログlに, 膜mem1への訪問を記録する. (所属膜はmem1のメンバから参照するため不要)
 * mem1にマッチした膜のプロセスIDもしくは訪問番号mem2_idを併せて記録する */
int tracelog_put_mem(TraceLogRef l, LmnMembraneRef mem1, LmnWord mem2_id) {
  int ret = tracelog_put(l, lmn_mem_id(mem1), mem2_id, lmn_mem_parent(mem1));
  TLOG_SET_TRV_MEM(TLOG_FLAG(tracelog_entry(l, lmn_mem_id(mem1))));
  return ret;
}

/* ログlに, ハイパーグラフのルートオブジェクトhl1への訪問を記録する.
 * (ハイパーグラフ構造には所属膜の概念がなく, 膜オブジェクトからの参照もできないため,
 *  所属膜に対する一切の操作は不要)
 * hl1にマッチしたハイパリンクオブジェクトIDもしくは訪問番号hl2_idを併せて記録する */
int tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id) {
  int ret = tracelog_put(l, LMN_HL_ID(hl1), hl2_id, NULL);
  TLOG_SET_TRV_HLINK(TLOG_FLAG(tracelog_entry(l, LMN_HL_ID(hl1))));
  return ret;
}

void tracelog_unput(TraceLogRef l, LmnWord key) {
  LMN_ASSERT (TLOG_IS_TRV(TLOG_FLAG(tracelog_entry(l, key)))); /* 訪問済みでもないないのにunputするとnumの値が不正になり得る */
  if (l->cap > key && tracelog_bucket(l, key)) {
    l->num--;

    ProcessID owner_id = TLOG_OWNER(tracelog_entry(l, key));
    if (tracelog_bucket(l, owner_id)) {
      TLOG_TRV_DEC(tracelog_entry(l, owner_id));
    }

    TLOG_DATA_CLEAR(tracelog_entry(l, key));
  }
}

BOOL tracelog_contains(TraceLogRef l, LmnWord key) {
  return (l->cap > key) && tracelog_bucket(l, key) && TLOG_IS_TRV(TLOG_FLAG(tracelog_entry(l, key)));
}

BOOL tracelog_contains_atom(TraceLogRef l, LmnSymbolAtomRef atom) {
  return tracelog_contains(l, LMN_SATOM_ID(atom));
}

BOOL tracelog_contains_mem(TraceLogRef l, LmnMembraneRef mem) {
  return tracelog_contains(l, lmn_mem_id(mem));
}

BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) {
  return tracelog_contains(l, LMN_HL_ID(hl));
}

LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key) {
  return tracelog_bucket(l, key) ? TLOG_MATCHED(tracelog_entry(l, key)) : 0;
}

LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSymbolAtomRef atom) {
  return tracelog_get_matched(l, LMN_SATOM_ID(atom));
}

LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembraneRef mem) {
  return tracelog_get_matched(l, lmn_mem_id(mem));
}

LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl) {
  return tracelog_get_matched(l, LMN_HL_ID(hl));
}

BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key) {
  return tracelog_bucket(l, key) ? TLOG_FLAG(tracelog_entry(l, key)) : 0;
}

void tracelog_backtrack(TraceLogRef l) {
  LogTracker_REVERT(&l->tracker, tracelog_unput, l);
}

void tracelog_set_btpoint(TraceLogRef l) {
  LogTracker_PUSH(&l->tracker);
}

void tracelog_continue_trace(TraceLogRef l) {
  LogTracker_POP(&l->tracker);
}
