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
#include "membrane.h"

void proc_tbl_init(ProcessTbl p)
{
//  proc_tbl_init_with_size(p, env_max_id());
  proc_tbl_init_with_size(p, 1024);
}

void proc_tbl_init_with_size(ProcessTbl p, unsigned long size)
{
  p->n    = 0;
  p->size = size;
#ifdef TIME_OPT
  p->tbl  = LMN_NALLOC(LmnWord, p->size);
  memset(p->tbl, 0xff, sizeof(LmnWord) * p->size);
#else
  p->tbl = st_init_ptrtable();
#endif
}

ProcessTbl proc_tbl_make(void)
{
//  return proc_tbl_make_with_size(env_max_id());
  return proc_tbl_make_with_size(1024);
}

ProcessTbl proc_tbl_make_with_size(unsigned long size)
{
  ProcessTbl p = LMN_MALLOC(struct ProcessTbl);
  proc_tbl_init_with_size(p, size);
  return p;
}

void proc_tbl_destroy(ProcessTbl p)
{
#ifdef TIME_OPT
  LMN_FREE(p->tbl);
#else
  st_free_table(p->tbl);
#endif
}

void proc_tbl_free(ProcessTbl p)
{
  proc_tbl_destroy(p);
  LMN_FREE(p);
}

int proc_tbl_foreach(ProcessTbl p, int(*func)(LmnWord key, LmnWord val, LmnWord arg), LmnWord arg)
{
#ifdef TIME_OPT
  LmnWord i;

  for (i = 0; i < p->size; i++) {
    if (p->tbl[i] != ULONG_MAX) func(i, p->tbl[i], arg);
  }
  return 0;
#else
  return st_foreach(p->tbl, func, arg);
#endif
}

#ifdef TIME_OPT
void proc_tbl_expand_sub(ProcessTbl p, unsigned long n)
{
  unsigned long org_size = p->size;
  while (p->size <= n) p->size *= 2;
  p->tbl = LMN_REALLOC(LmnWord, p->tbl, p->size);
  memset(p->tbl + org_size, 0xff, sizeof(LmnWord) * (p->size - org_size));
}
#endif

/*----------------------------------------------------------------------
 * VisitLog: アトムや膜への訪問の記録
 */

/* VisitLogのログ変更の記録 */
static inline struct Checkpoint *checkpoint_make()
{
  struct Checkpoint *p = LMN_MALLOC(struct Checkpoint);

  vec_init(&p->elements, 128);
  p->n_data_atom = 0;
  return p;
}

void checkpoint_free(struct Checkpoint *cp)
{
  vec_destroy(&cp->elements);
  LMN_FREE(cp);
}

void visitlog_init_with_size(VisitLog p, unsigned long tbl_size)
{
  if (tbl_size != 0) proc_tbl_init_with_size(&p->tbl, tbl_size);
  else proc_tbl_init(&p->tbl);
/*   printf("size = %lu\n", tbl_size); */
  p->ref_n = VISITLOG_INIT_N;
  p->element_num = 0;
  vec_init(&p->checkpoints, 128);
}

void visitlog_init(struct VisitLog *p)
{
  proc_tbl_init(&p->tbl);
  p->ref_n = VISITLOG_INIT_N;
  p->element_num = 0;
  vec_init(&p->checkpoints, 128);
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
void visitlog_set_checkpoint(VisitLog visitlog)
{
  vec_push(&visitlog->checkpoints, (vec_data_t)checkpoint_make());
}

/* もっとも最近のチェックポイントを返し、ログの状態をチェックポイントが設定された時点にもどす */
struct Checkpoint *visitlog_pop_checkpoint(VisitLog visitlog)
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
void visitlog_revert_checkpoint(VisitLog visitlog)
{
  checkpoint_free(visitlog_pop_checkpoint(visitlog));
}

/* ログの状態はそのままに、もっとも最近に設定したチェックポイントを消す */
 void visitlog_commit_checkpoint(VisitLog visitlog)
{
  struct Checkpoint *last = (struct Checkpoint *)vec_pop(&visitlog->checkpoints);

  if (vec_num(&visitlog->checkpoints) > 0) {
    int i;
    struct Checkpoint *new_last = (struct Checkpoint *)vec_last(&visitlog->checkpoints);

    for (i = 0; i < vec_num(&last->elements); i++) {
      vec_push(&new_last->elements, vec_get(&last->elements, i));
    }
    new_last->n_data_atom += last->n_data_atom;
  }

  checkpoint_free(last);
}

/* チェックポイントをログに追加する */
void visitlog_push_checkpoint(VisitLog visitlog, struct Checkpoint *cp)
{
  int i;

  vec_push(&visitlog->checkpoints, (vec_data_t)cp);
  for (i = 0; i < vec_num(&cp->elements); i++) {
    proc_tbl_put(&visitlog->tbl, vec_get(&cp->elements, i), visitlog->ref_n++);
    visitlog->element_num++;
  }

  visitlog->element_num += cp->n_data_atom;
}

