/*
 * visitlog.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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

#ifndef LMN_VISITLOG_H
#define LMN_VISITLOG_H

/* cldoc:begin-category(VisitLog) */

#include "../lmntal.h"
#include "vector.h"
#include "../atom.h"
#include "../membrane.h"
#include "../error.h"
#include "util.h"
#include <limits.h>


#define VISITLOG_INIT_N       (1)

/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
struct ProcessTbl {
  unsigned long n;
  unsigned long size;
  LmnWord *tbl;
};

#define process_tbl_entry_num(P)  ((P)->n)
void proc_tbl_expand_sub(ProcessTableRef p, unsigned long n);
#define proc_tbl_expand(p, n)                                                  \
  if ((p)->size <= (n)) {                                                      \
    proc_tbl_expand_sub(p, n);                                                 \
  }


/**
 * Function ProtoTypes
 */

void       proc_tbl_init_with_size(ProcessTableRef p, unsigned long size);
void       proc_tbl_init(ProcessTableRef p);
ProcessTableRef proc_tbl_make_with_size(unsigned long size);
ProcessTableRef proc_tbl_make(void);
void       proc_tbl_destroy(ProcessTableRef p);
void       proc_tbl_free(ProcessTableRef p);
void       proc_tbl_clear(ProcessTableRef p);
int        proc_tbl_foreach(ProcessTableRef p,
                            int(*func)(LmnWord key, LmnWord val, LmnWord arg),
                            LmnWord arg);
BOOL       proc_tbl_eq(ProcessTableRef a, ProcessTableRef b);

static inline void proc_tbl_put(ProcessTableRef p, LmnWord key, LmnWord value);
static inline void proc_tbl_put_atom(ProcessTableRef p, LmnSAtom atom, LmnWord value);
static inline void proc_tbl_put_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord value);
static inline int  proc_tbl_put_new(ProcessTableRef p, LmnWord key, LmnWord value);
static inline int  proc_tbl_put_new_atom(ProcessTableRef p, LmnSAtom atom, LmnWord value);
static inline int  proc_tbl_put_new_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord value);
static inline void proc_tbl_put_new_hlink(ProcessTableRef p, HyperLink *hl, LmnWord value);
static inline void proc_tbl_unput(ProcessTableRef p, LmnWord key);
static inline void proc_tbl_unput_atom(ProcessTableRef p, LmnSAtom atom);
static inline void proc_tbl_unput_mem(ProcessTableRef p, LmnMembrane *mem);
static inline int  proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value);
static inline int  proc_tbl_get_by_atom(ProcessTableRef p, LmnSAtom atom, LmnWord *value);
static inline int  proc_tbl_get_by_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord *value);
static inline int  proc_tbl_get_by_hlink(ProcessTableRef p, HyperLink *hl, LmnWord *value);
static inline BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key);
static inline BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSAtom atom);
static inline BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembrane *mem);



/**
 * Inline Functions
 */


/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
static inline void proc_tbl_put(ProcessTableRef p, LmnWord key, LmnWord value) {
  p->n++;
# ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
# endif
  proc_tbl_expand(p, key);
  p->tbl[key] = value;
}

/* テーブルにアトムを追加 */
static inline void proc_tbl_put_atom(ProcessTableRef p, LmnSAtom atom, LmnWord value) {
  proc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加 */
static inline void proc_tbl_put_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord value) {
  proc_tbl_put(p, lmn_mem_id(mem), value);
}

/* テーブルにハイパーリンクを追加 */
static inline void proc_tbl_put_new_hlink(ProcessTableRef p, HyperLink *hl, LmnWord value)
{
  proc_tbl_put(p, LMN_HL_ID(hl), value);
}

/* テーブルにkeyを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_new_atom, put_new_memを使用する. */
static inline int proc_tbl_put_new(ProcessTableRef p, LmnWord key, LmnWord value) {
  p->n++;
#ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
#endif
  proc_tbl_expand(p, key);
  if (p->tbl[key] != ULONG_MAX) return 0;
  p->tbl[key] = value;
  return 1;
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す */
static inline int proc_tbl_put_new_atom(ProcessTableRef p, LmnSAtom atom, LmnWord value) {
  return proc_tbl_put_new(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
static inline int proc_tbl_put_new_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord value) {
  return proc_tbl_put_new(p, lmn_mem_id(mem), value);
}

/* テーブルからkeyとそれに対応した値を削除する.
 * 通常この間数ではなくunput_atom, unput_memを使用する. */
static inline void proc_tbl_unput(ProcessTableRef p, LmnWord key) {
  p->n--;
  proc_tbl_expand(p, key);
  p->tbl[key] = ULONG_MAX;
}

/* テーブルからアトムとそれに対応した値を削除する */
static inline void proc_tbl_unput_atom(ProcessTableRef p, LmnSAtom atom) {
  proc_tbl_unput(p, LMN_SATOM_ID(atom));
}

/* テーブルから膜とそれに対応した値を削除する */
static inline void proc_tbl_unput_mem(ProcessTableRef p, LmnMembrane *mem) {
  proc_tbl_unput(p, lmn_mem_id(mem));
}

/* テーブルのkeyに対応した値をvalueに設定し, 正の値を返す. keyがテーブルに存在しない場合は0を返す.
 * 通常この間数ではなくget_by_atom, get_by_memを使用する./ */
static inline int proc_tbl_get(ProcessTableRef p, LmnWord key, LmnWord *value) {
  if (p->size > key && p->tbl[key] != ULONG_MAX) {
    if (value) *value = p->tbl[key];
    return 1;
  } else {
    return 0;
  }
}

/* テーブルのアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにatomが存在しない場合は0を返す */
static inline int proc_tbl_get_by_atom(ProcessTableRef p, LmnSAtom atom, LmnWord *value) {
  return proc_tbl_get(p, LMN_SATOM_ID(atom), value);
}

/* テーブルの膜memに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにmemが存在しない場合は0を返す */
static inline int proc_tbl_get_by_mem(ProcessTableRef p, LmnMembrane *mem, LmnWord *value) {
  return proc_tbl_get(p, lmn_mem_id(mem), value);
}

/* テーブルのハイパーリンクhlに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにhlが存在しない場合は0を返す */
static inline int proc_tbl_get_by_hlink(ProcessTableRef p, HyperLink *hl, LmnWord *value)
{
  return proc_tbl_get(p, LMN_HL_ID(hl), value);
}

static inline BOOL proc_tbl_contains(ProcessTableRef p, LmnWord key) {
  return key < p->size && p->tbl[key] != ULONG_MAX;
}

/* テーブルにアトムatomに対応する値が設定されている場合, 正の値を返す. */
static inline BOOL proc_tbl_contains_atom(ProcessTableRef p, LmnSAtom atom) {
  return proc_tbl_contains(p, LMN_SATOM_ID(atom));
}

/* テーブルの膜memに対応する値が設定されている場合、正の値を返す. */
static inline BOOL proc_tbl_contains_mem(ProcessTableRef p, LmnMembrane *mem) {
  return proc_tbl_contains(p, lmn_mem_id(mem));
}


/* --------------
 *  SimplyProcTbl
 *  プロセスIDをkeyにしたBYTEサイズテーブル
 */
struct SimplyProcTbl {
  unsigned long n, cap;
  BYTE *tbl;
};

#define SPROC_TBL_INIT_V        (0xfU)
#define sproc_tbl_entry_num(P)  ((P)->n)

/**
 * Function Prototypes
 */


void sproc_tbl_init_with_size(SimplyProcessTableRef p, unsigned long size);
void sproc_tbl_init(SimplyProcessTableRef p);
void sproc_tbl_destroy(SimplyProcessTableRef p);

static inline void sproc_tbl_expand(SimplyProcessTableRef p, unsigned long n);
static inline void sproc_tbl_put(SimplyProcessTableRef p, LmnWord key, BYTE value);
static inline void sproc_tbl_put_atom(SimplyProcessTableRef p, LmnSAtom atom, BYTE value);
static inline void sproc_tbl_put_mem(SimplyProcessTableRef p, LmnMembrane *mem, BYTE value);
static inline void sproc_tbl_unput(SimplyProcessTableRef p, LmnWord key);
static inline void sproc_tbl_unput_atom(SimplyProcessTableRef p, LmnSAtom atom);
static inline void sproc_tbl_unput_mem(SimplyProcessTableRef p, LmnMembrane *mem);
static inline int  sproc_tbl_get(SimplyProcessTableRef p, LmnWord key, BYTE *value);
static inline int  sproc_tbl_get_by_atom(SimplyProcessTableRef p, LmnSAtom atom, BYTE *value);
static inline int  sproc_tbl_get_by_mem(SimplyProcessTableRef p, LmnMembrane *mem, BYTE *value);
static inline BOOL sproc_tbl_contains(SimplyProcessTableRef p, LmnWord key);
static inline BOOL sproc_tbl_contains_atom(SimplyProcessTableRef p, LmnSAtom atom);
static inline BOOL sproc_tbl_contains_mem(SimplyProcessTableRef p, LmnMembrane *mem);
static inline BOOL sproc_tbl_get_flag(SimplyProcessTableRef p, LmnWord key, BYTE flag);
static inline BOOL sproc_tbl_get_flag_by_atom(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag);
static inline BOOL sproc_tbl_get_flag_by_mem(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag);
static inline void sproc_tbl_unset_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag);
static inline void sproc_tbl_set_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag);
static inline void sproc_tbl_set_atom_flag(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag);
static inline void sproc_tbl_set_mem_flag(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag);
static inline void sproc_tbl_unset_atom_flag(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag);
static inline void sproc_tbl_unset_mem_flag(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag);


/**
 * Inline Functions
 */

static inline void sproc_tbl_expand(SimplyProcessTableRef p, unsigned long n) {
  unsigned long org_size = p->cap;
  while (p->cap <= n) p->cap *= 2;
  p->tbl = LMN_REALLOC(BYTE, p->tbl, p->cap);
  memset(p->tbl + org_size,
         SPROC_TBL_INIT_V,
         sizeof(BYTE) * (p->cap - org_size));
}

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
static inline void sproc_tbl_put(SimplyProcessTableRef p, LmnWord key, BYTE value) {
#ifdef DEBUG
  if (value == SPROC_TBL_INIT_V) lmn_fatal("i can't put this value");
#endif
  if (p->cap <= key) {
    sproc_tbl_expand(p, key);
  }

  if (p->tbl[key] == SPROC_TBL_INIT_V) {
    p->n++;
  }

  p->tbl[key] = value;
}

static inline void sproc_tbl_put_atom(SimplyProcessTableRef p, LmnSAtom atom, BYTE value) {
  sproc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

static inline void sproc_tbl_put_mem(SimplyProcessTableRef p, LmnMembrane *mem, BYTE value) {
  sproc_tbl_put(p, lmn_mem_id(mem), value);
}

static inline void sproc_tbl_unput(SimplyProcessTableRef p, LmnWord key) {
  if (p->cap < key || p->tbl[key] == SPROC_TBL_INIT_V) return;
  p->n--;
  p->tbl[key] = SPROC_TBL_INIT_V;
}

static inline void sproc_tbl_unput_atom(SimplyProcessTableRef p, LmnSAtom atom) {
  sproc_tbl_unput(p, LMN_SATOM_ID(atom));
}

static inline void sproc_tbl_unput_mem(SimplyProcessTableRef p, LmnMembrane *mem) {
  sproc_tbl_unput(p, lmn_mem_id(mem));
}

static inline int sproc_tbl_get(SimplyProcessTableRef p, LmnWord key, BYTE *value) {
  if (p->cap > key && p->tbl[key] != SPROC_TBL_INIT_V) {
    if (value) *value = p->tbl[key];
    return 1;
  } else {
    return 0;
  }
}

static inline int sproc_tbl_get_by_atom(SimplyProcessTableRef p, LmnSAtom atom, BYTE *value) {
  return sproc_tbl_get(p, LMN_SATOM_ID(atom), value);
}

static inline int sproc_tbl_get_by_mem(SimplyProcessTableRef p, LmnMembrane *mem, BYTE *value) {
  return sproc_tbl_get(p, lmn_mem_id(mem), value);
}

static inline BOOL sproc_tbl_contains(SimplyProcessTableRef p, LmnWord key) {
  return key < p->cap && p->tbl[key] != SPROC_TBL_INIT_V;
}

static inline BOOL sproc_tbl_contains_atom(SimplyProcessTableRef p, LmnSAtom atom) {
  return sproc_tbl_contains(p, LMN_SATOM_ID(atom));
}

static inline BOOL sproc_tbl_contains_mem(SimplyProcessTableRef p, LmnMembrane *mem) {
  return sproc_tbl_contains(p, lmn_mem_id(mem));
}

static inline BOOL sproc_tbl_get_flag(SimplyProcessTableRef p, LmnWord key, BYTE flag) {
  if (p->cap > key && p->tbl[key] != SPROC_TBL_INIT_V) return p->tbl[key] & flag;
  else return 0;
}

static inline BOOL sproc_tbl_get_flag_by_atom(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag) {
  return sproc_tbl_get_flag(p, LMN_SATOM_ID(key), flag);
}

static inline BOOL sproc_tbl_get_flag_by_mem(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag) {
  return sproc_tbl_get_flag(p, lmn_mem_id(key), flag);
}

static inline void sproc_tbl_unset_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  if (p->cap <= key) {
    sproc_tbl_expand(p, key);
  }
  if (p->tbl[key] != SPROC_TBL_INIT_V) {
    p->tbl[key] |= ~flag;
  } else {
    p->n++;
    p->tbl[key] = 0;
  }
}

static inline void sproc_tbl_set_flag(SimplyProcessTableRef p, LmnWord key, LmnWord flag) {
  if (p->cap <= key) {
    sproc_tbl_expand(p, key);
  }
  if (p->tbl[key] != SPROC_TBL_INIT_V) {
    p->tbl[key] |= flag;
  } else {
    p->n++;
    p->tbl[key] = flag;
  }
}

static inline void sproc_tbl_set_atom_flag(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag) {
  sproc_tbl_set_flag(p, LMN_SATOM_ID(key), flag);
}

static inline void sproc_tbl_set_mem_flag(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag) {
  sproc_tbl_set_flag(p, lmn_mem_id(key), flag);
}

static inline void sproc_tbl_unset_atom_flag(SimplyProcessTableRef p, LmnSAtom key, LmnWord flag) {
  sproc_tbl_unset_flag(p, LMN_SATOM_ID(key), flag);
}

static inline void sproc_tbl_unset_mem_flag(SimplyProcessTableRef p, LmnMembrane *key, LmnWord flag) {
  sproc_tbl_unset_flag(p, lmn_mem_id(key), flag);
}

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
  struct TraceData *tbl;
  struct LogTracker tracker;
};

typedef struct TraceLog *TraceLogRef;


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
      l->tbl[key].flag = TLOG_INIT_DATA;                                       \
      l->tbl[key].owner_id = 0;                                                \
      l->tbl[key].matched = 0;                                                 \
    } while (0)

#define TLOG_MATCHED_ID_NONE       (0U)

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

/**
 * Function ProtoTypes
 */

TraceLogRef tracelog_make(void);
void tracelog_free(TraceLogRef trc);
void tracelog_init(TraceLogRef trc);
void tracelog_init_with_size(TraceLogRef trc, unsigned long size);
void tracelog_destroy(TraceLogRef trc);

static inline BOOL tracelog_eq_traversed_proc_num(TraceLogRef      l,
                                                  LmnMembrane   *owner,
                                                  AtomListEntryRef in_ent,
                                                  AtomListEntryRef avoid);
static inline void tracelog_tbl_expand(TraceLogRef l, unsigned long new_size);
static inline int  tracelog_put(TraceLogRef l, LmnWord key, LmnWord matched_id,
                                LmnMembrane *owner);
static inline int  tracelog_put_atom(TraceLogRef l, LmnSAtom atom1, LmnWord atom2_id,
                                     LmnMembrane *owner1);
static inline int  tracelog_put_mem(TraceLogRef l, LmnMembrane *mem1, LmnWord mem2_id);
static inline int  tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id);
static inline void tracelog_unput(TraceLogRef l, LmnWord key);
static inline BOOL tracelog_contains(TraceLogRef l, LmnWord key);
static inline BOOL tracelog_contains_atom(TraceLogRef l, LmnSAtom atom);
static inline BOOL tracelog_contains_mem(TraceLogRef l, LmnMembrane *mem);
static inline BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) ;
static inline LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key);
static inline LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSAtom atom);
static inline LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembrane *mem);
static inline LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl);
static inline void tracelog_backtrack(TraceLogRef l);
static inline void tracelog_set_btpoint(TraceLogRef l);
static inline void tracelog_continue_trace(TraceLogRef l);
static inline BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key);

/**
 * Inline Functions
 */

/* 膜ownerを対象として訪問済みにしたプロセス (シンボルアトム + 子膜 + inside proxies) の数が
 * 膜ownerのそれと一致しているか否かを返す */
static inline BOOL tracelog_eq_traversed_proc_num(TraceLogRef      l,
                                                  LmnMembrane   *owner,
                                                  AtomListEntryRef in_ent,
                                                  AtomListEntryRef avoid)
{
  return
      TLOG_NUM(l->tbl[lmn_mem_id(owner)]) ==
          (lmn_mem_symb_atom_num(owner)
              + lmn_mem_child_mem_num(owner)
              + atomlist_get_entries_num(in_ent)
              - atomlist_get_entries_num(avoid));
}

/* トレースログlのテーブルサイズをnew_size以上の大きさに拡張する. */
static inline void tracelog_tbl_expand(TraceLogRef l, unsigned long new_size)
{
  unsigned long org_size = l->cap;
  while (l->cap <= new_size) l->cap *= 2;
  l->tbl = LMN_REALLOC(struct TraceData, l->tbl, l->cap);
  memset(l->tbl + org_size, TLOG_INIT_DATA, sizeof(struct TraceData) * (l->cap - org_size));
}

/* ログl上のキーkey位置にあるTraceLogに対して, 訪問を記録する.
 * matched_idはマッチしたプロセスのID, ownerは所属膜.
 * 所属膜がNULLでない場合は, 所属膜の情報を記録し, 所属膜側のプロセス訪問カウンタを回す.
 * tracelog_put_atom, tracelog_put_memから呼び出す関数であり, 直接callしないこと.
 * (呼出しコスト削減のために公開関数としているだけ) */
static inline int tracelog_put(TraceLogRef l, LmnWord key, LmnWord matched_id,
                               LmnMembrane *owner) {
  if (l->cap <= key) {
    tracelog_tbl_expand(l, key);
  } else if (TLOG_IS_TRV(TLOG_FLAG(l->tbl[key]))) {
    return 0;
  }

  l->num++;
  TLOG_SET_MATCHED(l->tbl[key], matched_id);

  if (owner) {
    TLOG_SET_OWNER(l->tbl[key], owner);
    LMN_ASSERT(l->cap > lmn_mem_id(owner));
    TLOG_TRV_INC(l->tbl[lmn_mem_id(owner)]);
  }

  LogTracker_TRACE(&l->tracker, key);

  return 1;
}

/* ログlに, 所属膜owner1のアトムatom1への訪問を記録する.
 * atom1にマッチしたアトムのプロセスIDもしくは訪問番号atom2_idを併せて記録する. */
static inline int tracelog_put_atom(TraceLogRef l, LmnSAtom atom1, LmnWord  atom2_id,
                                    LmnMembrane *owner1) {
  int ret = tracelog_put(l, LMN_SATOM_ID(atom1), atom2_id, owner1);
  TLOG_SET_TRV_ATOM(TLOG_FLAG(l->tbl[LMN_SATOM_ID(atom1)]));
  return ret;
}

/* ログlに, 膜mem1への訪問を記録する. (所属膜はmem1のメンバから参照するため不要)
 * mem1にマッチした膜のプロセスIDもしくは訪問番号mem2_idを併せて記録する */
static inline int tracelog_put_mem(TraceLogRef l, LmnMembrane *mem1, LmnWord mem2_id) {
  int ret = tracelog_put(l, lmn_mem_id(mem1), mem2_id, lmn_mem_parent(mem1));
  TLOG_SET_TRV_MEM(TLOG_FLAG(l->tbl[lmn_mem_id(mem1)]));
  return ret;
}

/* ログlに, ハイパーグラフのルートオブジェクトhl1への訪問を記録する.
 * (ハイパーグラフ構造には所属膜の概念がなく, 膜オブジェクトからの参照もできないため,
 *  所属膜に対する一切の操作は不要)
 * hl1にマッチしたハイパリンクオブジェクトIDもしくは訪問番号hl2_idを併せて記録する */
static inline int tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id) {
  int ret = tracelog_put(l, LMN_HL_ID(hl1), hl2_id, NULL);
  TLOG_SET_TRV_HLINK(TLOG_FLAG(l->tbl[LMN_HL_ID(hl1)]));
  return ret;
}

static inline void tracelog_unput(TraceLogRef l, LmnWord key) {
  LMN_ASSERT (TLOG_IS_TRV(TLOG_FLAG(l->tbl[key]))); /* 訪問済みでもないないのにunputするとnumの値が不正になり得る */
  if (l->cap > key) {
    l->num--;
    TLOG_TRV_DEC(l->tbl[TLOG_OWNER(l->tbl[key])]);
    TLOG_DATA_CLEAR(l->tbl[key]);
  }
}

static inline BOOL tracelog_contains(TraceLogRef l, LmnWord key) {
  return (l->cap > key) && TLOG_IS_TRV(TLOG_FLAG(l->tbl[key]));
}

static inline BOOL tracelog_contains_atom(TraceLogRef l, LmnSAtom atom) {
  return tracelog_contains(l, LMN_SATOM_ID(atom));
}

static inline BOOL tracelog_contains_mem(TraceLogRef l, LmnMembrane *mem) {
  return tracelog_contains(l, lmn_mem_id(mem));
}

static inline BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) {
  return tracelog_contains(l, LMN_HL_ID(hl));
}

static inline LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key) {
  return TLOG_MATCHED(l->tbl[key]);
}

static inline LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSAtom atom) {
  return tracelog_get_matched(l, LMN_SATOM_ID(atom));
}

static inline LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembrane *mem) {
  return tracelog_get_matched(l, lmn_mem_id(mem));
}

static inline LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl) {
  return tracelog_get_matched(l, LMN_HL_ID(hl));
}

static inline BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key) {
  return TLOG_FLAG(l->tbl[key]);
}

static inline void tracelog_backtrack(TraceLogRef l) {
  LogTracker_REVERT(&l->tracker, tracelog_unput, l);
}

static inline void tracelog_set_btpoint(TraceLogRef l) {
  LogTracker_PUSH(&l->tracker);
}

static inline void tracelog_continue_trace(TraceLogRef l) {
  LogTracker_POP(&l->tracker);
}


/** ------
 *  SimpleTraceLog
 */

struct SimplyTraceLog {
  struct SimplyProcTbl tbl; /* Process IDをkey, 訪問済みか否かの真偽値をvalueとしたテーブル */
  struct LogTracker tracker;
};

#define STRACE_TRUE   (!SPROC_TBL_INIT_V)

typedef struct SimplyTraceLog *SimplyLog;

/**
 * Function ProtoTypes
 */

void simplylog_init(SimplyLog trc);
inline void simplylog_init_with_size(SimplyLog trc, unsigned long size);
void simplylog_destroy(SimplyLog trc);

static inline void simplylog_put(SimplyLog l, LmnWord key);
static inline void simplylog_put_atom(SimplyLog l, LmnSAtom atom);
static inline void simplylog_put_mem(SimplyLog l, LmnMembrane *mem);
static inline BOOL simplylog_contains_atom(SimplyLog l, LmnSAtom atom);
static inline BOOL simplylog_contains_mem(SimplyLog l, LmnMembrane *mem);
static inline void simplylog_backtrack(SimplyLog l);
static inline void simplylog_set_btpoint(SimplyLog l);
static inline void simplylog_continue_trace(SimplyLog l);

/**
 * Inline Functions
 */
static inline void simplylog_put(SimplyLog l, LmnWord key)
{
  LogTracker_TRACE(&l->tracker, key);
  sproc_tbl_put(&l->tbl, key, STRACE_TRUE);
}

static inline void simplylog_put_atom(SimplyLog l, LmnSAtom atom) {
  simplylog_put(l, LMN_SATOM_ID(atom));
}

static inline void simplylog_put_mem(SimplyLog l, LmnMembrane *mem) {
  simplylog_put(l, lmn_mem_id(mem));
}

static inline BOOL simplylog_contains_atom(SimplyLog l, LmnSAtom atom) {
  return sproc_tbl_contains_atom(&l->tbl, atom);
}

static inline BOOL simplylog_contains_mem(SimplyLog l, LmnMembrane *mem) {
  return sproc_tbl_contains_mem(&l->tbl, mem);
}

static inline void simplylog_backtrack(SimplyLog l) {
  LogTracker_REVERT(&l->tracker, sproc_tbl_unput, &l->tbl);
}

static inline void simplylog_set_btpoint(SimplyLog l) {
  LogTracker_PUSH(&l->tracker);
}

static inline void simplylog_continue_trace(SimplyLog l) {
  LogTracker_POP(&l->tracker);
}


/*----------------------------------------------------------------------
 * Visit Log
 */

/* VisitLog - 同型性判定や、IDなどバックトラックをしながらグラフを探索する場合に用いる.
 * アトムや膜のログへの追加時には追加順に自動的に番号を付加する.
 * チェックポイントを使うことで, バックトラック時にログをバックトラック前の状態に元に戻すことができる.
 */

/* VisitLogに記録された変更のスナップショット */
struct Checkpoint {
  int n_data_atom;
  Vector elements;
};

/* 訪問済みのアトムや膜の記録 */
struct VisitLog {
  struct ProcessTbl tbl;         /* プロセスIDをkeyにした訪問表 */
  int               ref_n,       /* バイト列から読み出したプロセスに再訪問が発生した場合のための参照番号割当カウンタ */
                    element_num; /* 訪問したプロセス数のカウンタ */
  Vector            checkpoints; /* Checkpointオブジェクトの配列 */
};

typedef struct VisitLog    *VisitLogRef;
typedef struct Checkpoint  *CheckpointRef;


/**
 * Function ProtoTypes
 */

void checkpoint_free(CheckpointRef cp);

void visitlog_init_with_size(VisitLogRef p, unsigned long tbl_size);
void visitlog_destroy(VisitLogRef p);
void visitlog_set_checkpoint(VisitLogRef visitlog);
CheckpointRef visitlog_pop_checkpoint(VisitLogRef visitlog);
void visitlog_revert_checkpoint(VisitLogRef visitlog);
void visitlog_commit_checkpoint(VisitLogRef visitlog);
void visitlog_push_checkpoint(VisitLogRef visitlog, CheckpointRef cp);

static inline int  visitlog_put(VisitLogRef visitlog, LmnWord p);
static inline int  visitlog_put_atom(VisitLogRef visitlog, LmnSAtom atom);
static inline int  visitlog_put_mem(VisitLogRef visitlog, LmnMembrane *mem);
static inline int  visitlog_put_hlink(VisitLogRef visitlog, HyperLink *hl);
static inline void visitlog_put_data(VisitLogRef visitlog);
static inline int  visitlog_get_atom(VisitLogRef visitlog, LmnSAtom atom, LmnWord *value);
static inline int  visitlog_get_mem(VisitLogRef visitlog, LmnMembrane *mem, LmnWord *value);
static inline int  visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value);
static inline int  visitlog_element_num(VisitLogRef visitlog);


/**
 * Inline Functions
 */

/* ログにpを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_atom, put_memを使用する. */
static inline int visitlog_put(VisitLogRef visitlog, LmnWord p) {
  if (proc_tbl_put_new(&visitlog->tbl, p, visitlog->ref_n++)) {
    if (vec_num(&visitlog->checkpoints) > 0) {
      CheckpointRef checkpoint = (CheckpointRef)vec_last(&visitlog->checkpoints);
      vec_push(&checkpoint->elements, p);
    }
    visitlog->element_num++;
    return 1;
  } else {
    return 0;
  }
}

/* ログにアトムを追加し, 正の値を返す. すでにアトムが存在した場合は0を返す */
static inline int visitlog_put_atom(VisitLogRef visitlog, LmnSAtom atom) {
  return visitlog_put(visitlog, LMN_SATOM_ID(atom));
}

/* ログに膜を追加し, 正の値を返す. すでに膜が存在した場合は0を返す */
static inline int visitlog_put_mem(VisitLogRef visitlog, LmnMembrane *mem) {
  return visitlog_put(visitlog, lmn_mem_id(mem));
}

/* ログにハイパーリンクを追加し, 正の値を返す. すでにハイパーリンクが存在した場合は0を返す */
static inline int visitlog_put_hlink(VisitLogRef visitlog, HyperLink *hl)
{
  return visitlog_put(visitlog, LMN_HL_ID(hl));
}

/* ログにデータアトムを追加する.
 * （引数がログしか無いことから分かるように, 単に訪問したアトムを数えるために使用する） */
static inline void visitlog_put_data(VisitLogRef visitlog) {
  if (vec_num(&visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint = (struct Checkpoint *)vec_last(&visitlog->checkpoints);
    checkpoint->n_data_atom++;
  }
  visitlog->element_num++;
}

/* ログに記録されたアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * ログにatomが存在しない場合は, 0を返す. */
static inline int visitlog_get_atom(VisitLogRef visitlog, LmnSAtom atom, LmnWord *value) {
  return proc_tbl_get_by_atom(&visitlog->tbl, atom, value);
}

/* ログに記録された膜memに対応する値をvalueに設定, 正の値を返す.
 * ログにmemが存在しない場合は, 0を返す. */
static inline int visitlog_get_mem(VisitLogRef visitlog, LmnMembrane *mem, LmnWord *value) {
  return proc_tbl_get_by_mem(&visitlog->tbl, mem, value);
}

/* ログに記録されたhlに対応する値をvalueに設定し, 正の値を返す.
 * ログにhlが存在しない場合は, 0を返す. */
static inline int visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value)
{
  return proc_tbl_get_by_hlink(&visitlog->tbl, hl, value);
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
static inline int visitlog_element_num(VisitLogRef visitlog) {
  return visitlog->element_num;
}

/* cldoc:end-category() */

#endif
