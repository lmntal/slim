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

#include "lmntal.h"
#include "vector.h"
#include "atom.h"
#include "membrane.h"
#include "error.h"
#include "util.h"
#ifndef TIME_OPT
#  include "st.h"
#endif
#include <limits.h>

#define VISITLOG_INIT_N       (1)

/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
struct ProcessTbl {
  unsigned long n;
  unsigned long size;
#ifdef TIME_OPT
  LmnWord *tbl;
#else
  st_table_t tbl;
#endif
};

void proc_tbl_init_with_size(ProcessTbl p, unsigned long size);
void proc_tbl_init(ProcessTbl p);
ProcessTbl proc_tbl_make_with_size(unsigned long size);
ProcessTbl proc_tbl_make(void);
void proc_tbl_destroy(ProcessTbl p);
void proc_tbl_free(ProcessTbl p);
static inline unsigned long proc_tgl_num(ProcessTbl p) { return p->n; }
int proc_tbl_foreach(ProcessTbl p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg);
#ifdef TIME_OPT
void proc_tbl_expand_sub(ProcessTbl p, unsigned long n);
#  define proc_tbl_expand(p, n) if ((p)->size <= (n)) proc_tbl_expand_sub(p, n)
#endif

/* テーブルにkeyを追加。put_atom,put_memを使用する。 */
static inline void proc_tbl_put(ProcessTbl p, LmnWord key, LmnWord value)
{
  p->n++;

#ifdef TIME_OPT
#  ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
#  endif
  proc_tbl_expand(p, key);
  p->tbl[key] = value;
#else
  st_insert(p->tbl, (st_data_t)key, value);
#endif
}

/* テーブルにアトムを追加 */
static inline void proc_tbl_put_atom(ProcessTbl p, LmnSAtom atom, LmnWord value)
{
  proc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加 */
static inline void proc_tbl_put_mem(ProcessTbl p, LmnMembrane *mem, LmnWord value)
{
  proc_tbl_put(p, lmn_mem_id(mem), value);
}

/* テーブルにkeyを追加し、正の値を返す。すでにpが存在した場合は0を返す。
   通常この関数ではなく、put_new_atom,put_new_memを使用する。 */
static inline int proc_tbl_put_new(ProcessTbl p, LmnWord key, LmnWord value)
{
  p->n++;
#ifndef TIME_OPT
  return st_insert_safe(p->tbl, (st_data_t)key, value);
#else
#ifdef DEBUG
  if (value == ULONG_MAX) lmn_fatal("cannot put ULONG_MAX");
#endif
  proc_tbl_expand(p, key);
  if (p->tbl[key] != ULONG_MAX) return 0;
  p->tbl[key] = value;
  return 1;
#endif
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す */
static inline int proc_tbl_put_new_atom(ProcessTbl p, LmnSAtom atom, LmnWord value)
{
  return proc_tbl_put_new(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
static inline int proc_tbl_put_new_mem(ProcessTbl p, LmnMembrane *mem, LmnWord value)
{
  return proc_tbl_put_new(p, lmn_mem_id(mem), value);
}

/* テーブルからkeyとそれに対応した値を削除する。通常この間数ではなく、
   unput_atom, unput_memを使用する */
static inline void proc_tbl_unput(ProcessTbl p, LmnWord key)
{
  p->n--;
#ifdef TIME_OPT
  proc_tbl_expand(p, key);
  p->tbl[key] = ULONG_MAX;
#else
  st_delete(p->tbl, (st_data_t)key, NULL);
#endif
}

/* テーブルからアトムとそれに対応した値を削除する */
static inline void proc_tbl_unput_atom(ProcessTbl p, LmnSAtom atom)
{
  proc_tbl_unput(p, LMN_SATOM_ID(atom));
}

/* テーブルから膜とそれに対応した値を削除する */
static inline void proc_tbl_unput_mem(ProcessTbl p, LmnMembrane *mem)
{
  proc_tbl_unput(p, lmn_mem_id(mem));
}

static inline void proc_tbl_unset_flag(ProcessTbl p, LmnWord key, LmnWord flag)
{
#ifdef TIME_OPT
  proc_tbl_expand(p, key);
  if (p->tbl[key] != ULONG_MAX) p->tbl[key] |= ~flag;
  else {
    p->n++;
    p->tbl[key] = 0;
  }
#else
  st_data_t t;
  if (st_lookup(p->tbl, (st_data_t)key, &t)) st_insert(p->tbl, (st_data_t)key, ((LmnWord)t) | flag);
  else st_insert(p->tbl, (st_data_t)key, flag);
#endif
}

static inline void proc_tbl_set_flag(ProcessTbl p, LmnWord key, LmnWord flag)
{
#ifdef TIME_OPT
  proc_tbl_expand(p, key);
  if (p->tbl[key] != ULONG_MAX) p->tbl[key] |= flag;
  else {
    p->n++;
    p->tbl[key] = flag;
  }
#else
  st_data_t t;
  if (st_lookup(p->tbl, (st_data_t)key, &t)) st_insert(p->tbl, (st_data_t)key, ((LmnWord)t) | flag);
  else st_insert(p->tbl, (st_data_t)key, flag);
#endif
}

static inline void proc_tbl_set_atom_flag(ProcessTbl p, LmnSAtom key, LmnWord flag)
{
  proc_tbl_set_flag(p, LMN_SATOM_ID(key), flag);
}

static inline void proc_tbl_set_mem_flag(ProcessTbl p, LmnMembrane *key, LmnWord flag)
{
  proc_tbl_set_flag(p, lmn_mem_id(key), flag);
}

static inline void proc_tbl_unset_atom_flag(ProcessTbl p, LmnSAtom key, LmnWord flag)
{
  proc_tbl_unset_flag(p, LMN_SATOM_ID(key), flag);
}

static inline void proc_tbl_unset_mem_flag(ProcessTbl p, LmnMembrane *key, LmnWord flag)
{
  proc_tbl_unset_flag(p, lmn_mem_id(key), flag);
}

/* テーブルのkeyに対応した値をvalueに設定し、正の値を返す。keyがテー
   ブルに存在しない場合は0を返す。通常この間数ではなく、get_by_atom,
   get_by_memを使用する */
static inline int proc_tbl_get(ProcessTbl p, LmnWord key, LmnWord *value)
{
#ifdef TIME_OPT
  if (p->size > key && p->tbl[key] != ULONG_MAX) {
    if (value) *value = p->tbl[key];
    return 1;
  } else {
    return 0;
  }
#else
  return st_lookup(p->tbl, key, (st_data_t *)value);
#endif
}

/* テーブルのアトムatomに対応する値をvalueに設定し、正の値を返す。テー
   ブルにatomが存在しない場合は0を返す */
static inline int proc_tbl_get_by_atom(ProcessTbl p, LmnSAtom atom, LmnWord *value)
{
  return proc_tbl_get(p, LMN_SATOM_ID(atom), value);
}

/* テーブルの膜memに対応する値をvalueに設定し、正の値を返す。テーブルに
   memが存在しない場合は0を返す */
static inline int proc_tbl_get_by_mem(ProcessTbl p, LmnMembrane *mem, LmnWord *value)
{
  return proc_tbl_get(p, lmn_mem_id(mem), value);
}

static inline BOOL proc_tbl_get_flag(ProcessTbl p, LmnWord key, LmnWord flag)
{
#ifdef TIME_OPT
  if (p->size > key && p->tbl[key] != ULONG_MAX) return p->tbl[key] & flag;
  else return 0;
#else
  LmnWord t;
  return proc_tbl_get(p, key, &t) && (t & flag);
#endif

}

static inline BOOL proc_tbl_get_flag_by_atom(ProcessTbl p, LmnSAtom key, LmnWord flag)
{
  return proc_tbl_get_flag(p, LMN_SATOM_ID(key), flag);
}

static inline BOOL proc_tbl_get_flag_by_mem(ProcessTbl p, LmnMembrane *key, LmnWord flag)
{
  return proc_tbl_get_flag(p, lmn_mem_id(key), flag);
}

static inline BOOL proc_tbl_contains(ProcessTbl p, LmnWord key)
{
#ifdef TIME_OPT
  return key < p->size && p->tbl[key] != ULONG_MAX;
#else
  LmnWord t;
  return proc_tbl_get(p, key, &t);
#endif
}

/* テーブルのアトムatomに対応する値をvalueに設定し、正の値を返す。テー
   ブルにatomが存在しない場合は0を返す */
static inline BOOL proc_tbl_contains_atom(ProcessTbl p, LmnSAtom atom)
{
  return proc_tbl_contains(p, LMN_SATOM_ID(atom));
}

/* テーブルの膜memに対応する値をvalueに設定し、正の値を返す。テーブルに
   memが存在しない場合は0を返す */
static inline BOOL proc_tbl_contains_mem(ProcessTbl p, LmnMembrane *mem)
{
  return proc_tbl_contains(p, lmn_mem_id(mem));
}


/*----------------------------------------------------------------------
 * Visit Log
 */

/* VisitLog - 同型性判定や、IDなどバックトラックをしながらグラフを探索
 * する場合に用いる。アトムや膜のログへの追加時には追加順に自動的に番号
 * を付加する。チェックポイントを使うことで、バックトラック時にログをバッ
 * クトラック前の状態に元に戻すことができる
 */

/* VisitLogに記録された変更のスナップショット */
struct Checkpoint {
  int n_data_atom;
  Vector elements;
};

/* 訪問済みのアトムや膜の記録。 */
struct VisitLog {
  struct ProcessTbl tbl;
  int ref_n, element_num;
  Vector checkpoints;
};

typedef struct VisitLog *VisitLog;
typedef struct Checkpoint *Checkpoint;

void checkpoint_free(Checkpoint cp);

void visitlog_init_with_size(VisitLog p, unsigned long tbl_size);
void visitlog_destroy(VisitLog p);
void visitlog_set_checkpoint(VisitLog visitlog);
Checkpoint visitlog_pop_checkpoint(VisitLog visitlog);
void visitlog_revert_checkpoint(VisitLog visitlog);
void visitlog_commit_checkpoint(VisitLog visitlog);
void visitlog_push_checkpoint(VisitLog visitlog, Checkpoint cp);

/*----------------------------------------------------------------------
 * Visit Log
 */

/* ログにpを追加し、正の値を返す。すでにpが存在した場合は0を返す。通常
   この関数ではなく、put_atom, put_memを使用する。 */
static inline int visitlog_put(VisitLog visitlog, LmnWord p)
{
  if (proc_tbl_put_new(&visitlog->tbl, p, visitlog->ref_n++)) {
    if (vec_num(&visitlog->checkpoints) > 0) {
      Checkpoint checkpoint =
        (Checkpoint)vec_last(&visitlog->checkpoints);
      vec_push(&checkpoint->elements, p);
    }
    visitlog->element_num++;
    return 1;
  } else {
    return 0;
  }
}

/* ログにアトムを追加し、正の値を返す。すでにアトムが存在した場合は0を返す */
static inline int visitlog_put_atom(VisitLog visitlog, LmnSAtom atom)
{
  return visitlog_put(visitlog, LMN_SATOM_ID(atom));
}

/* ログに膜を追加し、正の値を返す。すでに膜が存在した場合は0を返す */
static inline int visitlog_put_mem(VisitLog visitlog, LmnMembrane *mem)
{
  return visitlog_put(visitlog, lmn_mem_id(mem));
}

/* ログにデータアトム膜を追加する。（引数がログしか無いことから分かるよ
   うに、単に訪問したアトムを数えるために使用する） */
static inline void visitlog_put_data(VisitLog visitlog)
{
  if (vec_num(&visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint = (struct Checkpoint *)vec_last(&visitlog->checkpoints);
    checkpoint->n_data_atom++;
  }
  visitlog->element_num++;
}

/* ログに記録されたアトムatomに対応する値をvalueに設定し、正の値を返す。
   ログにatomが存在しない場合は、0を返す */
static inline int visitlog_get_atom(VisitLog visitlog, LmnSAtom atom, LmnWord *value)
{
  return proc_tbl_get_by_atom(&visitlog->tbl, atom, value);
}

/* ログに記録された膜memに対応する値をvalueに設定し、正の値を返す。ログ
   にmemが存在しない場合は、0を返す */
static inline int visitlog_get_mem(VisitLog visitlog, LmnMembrane *mem, LmnWord *value)
{
  return proc_tbl_get_by_mem(&visitlog->tbl, mem, value);
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
static inline int visitlog_element_num(VisitLog visitlog)
{
  return visitlog->element_num;
}

#endif
