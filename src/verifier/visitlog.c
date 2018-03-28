/*
 * visitlog.c
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

#include "visitlog.h"


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




void sproc_tbl_init_with_size(SimplyProcessTableRef p, unsigned long size)
{
  p->n   = 0;
  p->cap = size;
  p->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
  p->tbl = LMN_CALLOC(BYTE *, p->num_buckets);
}

void sproc_tbl_init(SimplyProcessTableRef p)
{
  sproc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

void sproc_tbl_destroy(SimplyProcessTableRef p)
{
  for (int i = 0; i < p->num_buckets; i++) {
    LMN_FREE(p->tbl[i]);
  }
  LMN_FREE(p->tbl);
}

/*------------
 * TraceLog
 */

static inline void tracker_init(struct LogTracker *track);
static inline void tracker_destroy(struct LogTracker *track);

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

static inline void tracker_init(struct LogTracker *track)
{
  vec_init(&track->traced_ids, PROC_TBL_DEFAULT_SIZE);
  vec_init(&track->btp_idx, PROC_TBL_DEFAULT_SIZE);
}

static inline void tracker_destroy(struct LogTracker *track)
{
  vec_destroy(&track->traced_ids);
  vec_destroy(&track->btp_idx);
}


/*------------
 * SimplyTraceLog
 */

void simplylog_init(SimplyLog s)
{
  simplylog_init_with_size(s, PROC_TBL_DEFAULT_SIZE);
}

void simplylog_init_with_size(SimplyLog s, unsigned long size)
{
  sproc_tbl_init_with_size(&s->tbl, size);
  tracker_init(&s->tracker);
}

void simplylog_destroy(SimplyLog s)
{
  sproc_tbl_destroy(&s->tbl);
  tracker_destroy(&s->tracker);
}


/*----------------------------------------------------------------------
 * VisitLog: アトムや膜への訪問の記録
 */

/* VisitLogのログ変更の記録 */
static inline struct Checkpoint *checkpoint_make()
{
  struct Checkpoint *p = LMN_MALLOC(struct Checkpoint);

  vec_init(&p->elements, PROC_TBL_DEFAULT_SIZE);
  p->n_data_atom = 0;
  return p;
}

void checkpoint_free(struct Checkpoint *cp)
{
  vec_destroy(&cp->elements);
  LMN_FREE(cp);
}

void visitlog_init_with_size(VisitLogRef p, unsigned long tbl_size)
{
  if (tbl_size != 0) {
    proc_tbl_init_with_size(&p->tbl, tbl_size);
  } else {
    proc_tbl_init(&p->tbl);
  }
/*   printf("size = %lu\n", tbl_size); */
  p->ref_n = VISITLOG_INIT_N;
  p->element_num = 0;
  vec_init(&p->checkpoints, PROC_TBL_DEFAULT_SIZE);
}

VisitLogRef visitlog_create() {
  return LMN_CALLOC(struct VisitLog, 1);
}

void visitlog_init(struct VisitLog *p)
{
  visitlog_init_with_size(p, 0);
}

void visitlog_destroy(struct VisitLog *p)
{
  int i;

  proc_tbl_destroy(&p->tbl);

  for (i = 0; i < vec_num(&p->checkpoints); i++) {
    vec_free((Vector *)vec_get(&p->checkpoints, i));
  }
  vec_destroy(&p->checkpoints);

  LMN_FREE(p);
}

/* チェックポイントを設定する。 */
void visitlog_set_checkpoint(VisitLogRef visitlog)
{
  vec_push(&visitlog->checkpoints, (vec_data_t)checkpoint_make());
}

/* もっとも最近のチェックポイントを返し、ログの状態をチェックポイントが設定された時点にもどす */
struct Checkpoint *visitlog_pop_checkpoint(VisitLogRef visitlog)
{
  int i;
  struct Checkpoint *checkpoint;

  checkpoint = (struct Checkpoint *)vec_pop(&visitlog->checkpoints);
  for (i = 0; i < vec_num(&checkpoint->elements); i++) {
    proc_tbl_unput(&visitlog->tbl, vec_get(&checkpoint->elements, i));
    visitlog->element_num--;
    visitlog->ref_n--;
  }
  visitlog->element_num -= checkpoint->n_data_atom;
  return checkpoint;
}


/* もっとも最近のチェックポイントを消し、ログの状態をチェックポイントが設定された時点にもどす */
void visitlog_revert_checkpoint(VisitLogRef visitlog)
{
  checkpoint_free(visitlog_pop_checkpoint(visitlog));
}

/* ログの状態はそのままに、もっとも最近に設定したチェックポイントを消す */
 void visitlog_commit_checkpoint(VisitLogRef visitlog)
{
  struct Checkpoint *last = (struct Checkpoint *)vec_pop(&visitlog->checkpoints);

  if (vec_num(&visitlog->checkpoints) > 0) {
    int i;
    struct Checkpoint *new_last =
        (struct Checkpoint *)vec_last(&visitlog->checkpoints);

    for (i = 0; i < vec_num(&last->elements); i++) {
      vec_push(&new_last->elements, vec_get(&last->elements, i));
    }
    new_last->n_data_atom += last->n_data_atom;
  }

  checkpoint_free(last);
}

/* チェックポイントをログに追加する */
void visitlog_push_checkpoint(VisitLogRef visitlog, struct Checkpoint *cp)
{
  int i;

  vec_push(&visitlog->checkpoints, (vec_data_t)cp);
  for (i = 0; i < vec_num(&cp->elements); i++) {
    proc_tbl_put(&visitlog->tbl, vec_get(&cp->elements, i), visitlog->ref_n++);
    visitlog->element_num++;
  }
  visitlog->element_num += cp->n_data_atom;
}


/* ログにpを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_atom, put_memを使用する. */
int visitlog_put(VisitLogRef visitlog, LmnWord p) {
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
int visitlog_put_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom) {
  return visitlog_put(visitlog, LMN_SATOM_ID(atom));
}

/* ログに膜を追加し, 正の値を返す. すでに膜が存在した場合は0を返す */
int visitlog_put_mem(VisitLogRef visitlog, LmnMembraneRef mem) {
  return visitlog_put(visitlog, lmn_mem_id(mem));
}

/* ログにハイパーリンクを追加し, 正の値を返す. すでにハイパーリンクが存在した場合は0を返す */
int visitlog_put_hlink(VisitLogRef visitlog, HyperLink *hl)
{
  return visitlog_put(visitlog, LMN_HL_ID(hl));
}

/* ログにデータアトムを追加する.
 * （引数がログしか無いことから分かるように, 単に訪問したアトムを数えるために使用する） */
void visitlog_put_data(VisitLogRef visitlog) {
  if (vec_num(&visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint = (struct Checkpoint *)vec_last(&visitlog->checkpoints);
    checkpoint->n_data_atom++;
  }
  visitlog->element_num++;
}

/* ログに記録されたアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * ログにatomが存在しない場合は, 0を返す. */
int visitlog_get_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom, LmnWord *value) {
  return proc_tbl_get_by_atom(&visitlog->tbl, atom, value);
}

/* ログに記録された膜memに対応する値をvalueに設定, 正の値を返す.
 * ログにmemが存在しない場合は, 0を返す. */
int visitlog_get_mem(VisitLogRef visitlog, LmnMembraneRef mem, LmnWord *value) {
  return proc_tbl_get_by_mem(&visitlog->tbl, mem, value);
}

/* ログに記録されたhlに対応する値をvalueに設定し, 正の値を返す.
 * ログにhlが存在しない場合は, 0を返す. */
int visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value)
{
  return proc_tbl_get_by_hlink(&visitlog->tbl, hl, value);
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
int visitlog_element_num(VisitLogRef visitlog) {
  return visitlog->element_num;
}
