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
#include "../membrane.h"

#define PROC_TBL_DEFAULT_SIZE  128U

void proc_tbl_init(ProcessTableRef p)
{
  proc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

void proc_tbl_init_with_size(ProcessTableRef p, unsigned long size)
{
  p->n    = 0;
  p->size = size;
#ifdef TIME_OPT
  p->tbl  = LMN_NALLOC(LmnWord, p->size);
  memset(p->tbl, 0xffU, sizeof(LmnWord) * p->size);
#else
  p->tbl = st_init_ptrtable();
#endif
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
#ifdef TIME_OPT
  LMN_FREE(p->tbl);
#else
  st_free_table(p->tbl);
#endif
}


void proc_tbl_free(ProcessTableRef p)
{
  proc_tbl_destroy(p);
  LMN_FREE(p);
}


void proc_tbl_clear(ProcessTableRef p)
{
  p->n = 0;
#ifdef TIME_OPT
  memset(p->tbl, 0xff, sizeof(LmnWord) * p->size);
#else
  st_clear(p->tbl);
#endif
}


int proc_tbl_foreach(ProcessTableRef p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg)
{
#ifdef TIME_OPT
  unsigned long i, n;

  n = 0;
  for (i = 0; i < p->size && n < process_tbl_entry_num(p); i++) {
    if (p->tbl[i] != ULONG_MAX) {
      func(i, p->tbl[i], arg);
      n++;
    }
  }
  return 0;
#else
  return st_foreach(p->tbl, func, arg);
#endif
}



BOOL proc_tbl_eq(ProcessTableRef a, ProcessTableRef b)
{
#ifdef TIME_OPT
  if (a->n != b->n) return FALSE;
  else {
    unsigned int i, a_checked;
    a_checked = 0;
    for (i = 0; i < a->size && a_checked < a->n; i++) {
      if (a->tbl[i] != b->tbl[i]) {
 //       printf("diff tbl[%lu]=, a=%lu, b=%lu\n", i, a->tbl[i], b->tbl[i]);
        return FALSE;
      }
      else if (a->tbl[i] != ULONG_MAX) {
        a_checked++;
      }
    }

    return TRUE;
  }


#else
  return (a->n == b->n) && st_equals(a->tbl, b->tbl);
#endif
}


void proc_tbl_expand_sub(ProcessTableRef p, unsigned long n)
{
  unsigned long org_size = p->size;
  while (p->size <= n) p->size *= 2;
  p->tbl = LMN_REALLOC(LmnWord, p->tbl, p->size);
  memset(p->tbl + org_size, 0xffU, sizeof(LmnWord) * (p->size - org_size));
}


void sproc_tbl_init_with_size(SimplyProcessTableRef p, unsigned long size)
{
  p->n   = 0;
  p->cap = size;
#ifdef TIME_OPT
  p->tbl = LMN_NALLOC(BYTE, p->cap);
  memset(p->tbl, SPROC_TBL_INIT_V, sizeof(BYTE) * p->cap);
#else
  p->tbl = st_init_ptrtable();
#endif
}

void sproc_tbl_init(SimplyProcessTableRef p)
{
  sproc_tbl_init_with_size(p, PROC_TBL_DEFAULT_SIZE);
}

void sproc_tbl_destroy(SimplyProcessTableRef p)
{
#ifdef TIME_OPT
  LMN_FREE(p->tbl);
#else
  st_free_table(p->tbl);
#endif
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

inline void simplylog_init_with_size(SimplyLog s, unsigned long size)
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
