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
#include "st.h"
#include "vector.h"
#include "atom.h"
#include "membrane.h"
#include "error.h"

#define VISITLOG_INIT_N       1

/* LMNtalのプロセス（アトム、膜）をキーにもちいるテーブル */
struct ProcessTbl {
  st_table_t tbl;
};

static inline void proc_tbl_init(ProcessTbl p)
{
  p->tbl = st_init_ptrtable();
}

static inline ProcessTbl proc_tbl_make(void)
{
  ProcessTbl p = LMN_MALLOC(struct ProcessTbl);
  proc_tbl_init(p);
  return p;
}

static inline void proc_tbl_destroy(ProcessTbl p)
{
  st_free_table(p->tbl);
}

static inline void proc_tbl_free(ProcessTbl p)
{
  proc_tbl_destroy(p);
  LMN_FREE(p);
}


/* テーブルにkeyを追加し、正の値を返す。すでにpが存在した場合は0を返す。
   通常この関数ではなく、put_atom,put_memを使用する。 */
static inline int proc_tbl_put(ProcessTbl p, LmnWord key, LmnWord value)
{
  return st_insert_safe(p->tbl, (st_data_t)key, value);
}

/* テーブルにアトムを追加し、正の値を返す。すでに同じアトムが存在した場合は0を返す */
static inline int proc_tbl_put_atom(ProcessTbl p, LmnSAtom atom, LmnWord value)
{
  return proc_tbl_put(p, LMN_SATOM_ID(atom), value);
}

/* テーブルに膜を追加し、正の値を返す。すでに同じ膜が存在した場合は0を返す */
static inline int proc_tbl_put_mem(ProcessTbl p, LmnMembrane *mem, LmnWord value)
{
  return proc_tbl_put(p, lmn_mem_id(mem), value);
}

/* テーブルからkeyとそれに対応した値を削除する。通常この間数ではなく、
   unput_atom, unput_memを使用する */
static inline void proc_tbl_unput(ProcessTbl p, LmnWord key)
{
  st_delete(p->tbl, (st_data_t)key, NULL);
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

/* テーブルのkeyに対応した値をvalueに設定し、正の値を返す。keyがテー
   ブルに存在しない場合は0を返す。通常この間数ではなく、get_by_atom,
   get_by_memを使用する */
static inline int proc_tbl_get(ProcessTbl p, LmnWord key, LmnWord *value)
{
  return st_lookup(p->tbl, key, (st_data_t *)value);
}

/* テーブルのアトムatomに対応する値をvalueに設定し、正の値を返す。テー
   ブルにatomが存在しない場合は0を返す */
static inline int proc_tbl_get_by_atom(ProcessTbl p, LmnSAtom atom, LmnWord *value)
{
  return proc_tbl_get(p, LMN_SATOM_ID(atom), (st_data_t *)value); 
}

/* テーブルの膜memに対応する値をvalueに設定し、正の値を返す。テーブルに
   memが存在しない場合は0を返す */
static inline int proc_tbl_get_by_mem(ProcessTbl p, LmnMembrane *mem, LmnWord *value)
{
  return proc_tbl_get(p, lmn_mem_id(mem), value);
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

void visitlog_init(VisitLog p);
void visitlog_destroy(VisitLog p);
void visitlog_set_checkpoint(VisitLog visitlog);
Checkpoint visitlog_pop_checkpoint(VisitLog visitlog);
void visitlog_revert_checkpoint(VisitLog visitlog);
void visitlog_commit_checkpoint(VisitLog visitlog);
void visitlog_push_checkpoint(VisitLog visitlog, Checkpoint cp);

/* ログにpを追加し、正の値を返す。すでにpが存在した場合は0を返す。通常
   この関数ではなく、put_atom, put_memを使用する。 */
static inline int visitlog_put(VisitLog visitlog, LmnWord p)
{
  if (proc_tbl_put(&visitlog->tbl, p, visitlog->ref_n++)) {
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
