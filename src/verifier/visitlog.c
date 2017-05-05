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

#define PROC_TBL_DEFAULT_SIZE  128U

void proc_tbl_init(ProcessTableRef p)
{
  proc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

void proc_tbl_init_with_size(ProcessTableRef p, unsigned long size)
{
  p->n    = 0;
  p->size = size;
  p->num_buckets = size / PROC_TBL_BUCKETS_SIZE + 1;
  p->tbl = LMN_CALLOC(LmnWord *, p->num_buckets);
}

ProcessTableRef proc_tbl_make(void)
{
  return proc_tbl_make_with_size(PROC_TBL_DEFAULT_SIZE);
}

ProcessTableRef proc_tbl_make_with_size(unsigned long size)
{
  ProcessTableRef p = LMN_MALLOC(struct ProcessTbl);
  proc_tbl_init_with_size(p, size);
  return p;
}

void proc_tbl_destroy(ProcessTableRef p)
{
  for (int i = 0; i < p->num_buckets; i++) {
    LMN_FREE(p->tbl[i]);
  }
  LMN_FREE(p->tbl);
}


void proc_tbl_free(ProcessTableRef p)
{
  proc_tbl_destroy(p);
  LMN_FREE(p);
}


void proc_tbl_clear(ProcessTableRef p)
{
  p->n = 0;
  for (int i = 0; i < p->num_buckets; i++) {
    memset(p->tbl[i], 0xff, sizeof(LmnWord) * PROC_TBL_BUCKETS_SIZE);
  }
}


int proc_tbl_foreach(ProcessTableRef p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg)
{
  unsigned long n = 0;

  for (int i = 0; i < p->num_buckets; i++) {
    if (!p->tbl[i]) continue;
    for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && n < process_tbl_entry_num(p); j++) {
      if (p->tbl[i][j] == ULONG_MAX) continue;
      func(i * PROC_TBL_BUCKETS_SIZE + j, p->tbl[i][j], arg);
      n++;
    }
  }
  return 0;
}



BOOL proc_tbl_eq(ProcessTableRef a, ProcessTableRef b)
{
  if (a->n != b->n) return FALSE;
  else {
    unsigned int a_checked = 0;

    for (int i = 0; i < a->num_buckets; i++) {
      if (!a->tbl[i] && !b->tbl[i]) continue;

      for (int j = 0; j < PROC_TBL_BUCKETS_SIZE && a_checked < a->n; j++) {
        LmnWord va = (a->tbl[i]) ? a->tbl[i][j] : ULONG_MAX;
        LmnWord vb = (b->tbl[i]) ? b->tbl[i][j] : ULONG_MAX;
        if (va != vb) return FALSE;
        if (va != ULONG_MAX) a_checked++;
      }
    }

    return TRUE;
  }
}


void proc_tbl_expand_sub(ProcessTableRef p, unsigned long n)
{
  unsigned int org_n = p->num_buckets;
  while (p->size <= n) p->size *= 2;
  p->num_buckets = p->size / PROC_TBL_BUCKETS_SIZE + 1;
  if (org_n < p->num_buckets) {
    p->tbl = LMN_REALLOC(LmnWord *, p->tbl, p->num_buckets);
    memset(p->tbl + org_n, 0, sizeof(LmnWord *) * (p->num_buckets - org_n));
  }

  unsigned int b = n / PROC_TBL_BUCKETS_SIZE;
  if (b < p->num_buckets && p->tbl[b]) return;
  p->tbl[b] = LMN_NALLOC(LmnWord, PROC_TBL_BUCKETS_SIZE);
  memset(p->tbl[b], 0xffU, sizeof(LmnWord) * PROC_TBL_BUCKETS_SIZE);
}


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

/* テーブルのアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * テーブルにatomが存在しない場合は0を返す */
int proc_tbl_get_by_atom(ProcessTableRef p, LmnSymbolAtomRef atom, LmnWord *value) {
  return proc_tbl_get(p, LMN_SATOM_ID(atom), value);
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
  l->tbl = LMN_NALLOC(struct TraceData, l->cap);
  memset(l->tbl, 0U, sizeof(struct TraceData) * l->cap);
  tracker_init(&l->tracker);
}


void tracelog_free(TraceLogRef l)
{
  tracelog_destroy(l);
  LMN_FREE(l);
}

void tracelog_destroy(TraceLogRef l)
{
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
