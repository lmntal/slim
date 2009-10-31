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

void visitlog_init(struct VisitLog *p)
{
  p->log = st_init_ptrtable(); 
  p->ref_n = VISITLOG_INIT_N;
  p->element_num = 0;
  vec_init(&p->checkpoints, 128);
}

void visitlog_destroy(struct VisitLog *p)
{
  int i;
  
  st_free_table(p->log);

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
    st_delete(visitlog->log, (st_data_t)vec_get(&checkpoint->elements, i), NULL);
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
    st_insert(visitlog->log, (st_data_t)vec_get(&cp->elements, i), visitlog->ref_n++);
    visitlog->element_num++;
  }

  visitlog->element_num += cp->n_data_atom;
}

