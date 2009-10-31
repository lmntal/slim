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

/** VisitLog - 膜のコピーや、エンコード、ハッシュの計算などを行う際に、
 * アトムや膜の訪問履歴を記録するために使用する
 */

#ifndef LMN_VISITLOG_H
#define LMN_VISITLOG_H

#include "lmntal.h"
#include "st.h"
#include "vector.h"
#include "atom.h"

#define VISITLOG_INIT_N       1

/* VisitLogに記録された変更のスナップショット */
struct Checkpoint {
  int n_data_atom;
  Vector elements;
};

/* 訪問済みのアトムや膜の記録 */
struct VisitLog {
  st_table_t log;
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

static inline void visitlog_put(VisitLog visitlog, LmnWord p)
{
  st_insert(visitlog->log, (st_data_t)p, visitlog->ref_n++);
  if (vec_num(&visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint = (struct Checkpoint *)vec_last(&visitlog->checkpoints);
    vec_push(&checkpoint->elements, p);
  }
  visitlog->element_num++;
}

static inline void visitlog_put_data(VisitLog visitlog)
{
  if (vec_num(&visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint = (struct Checkpoint *)vec_last(&visitlog->checkpoints);
    checkpoint->n_data_atom++;
  }
  visitlog->element_num++;
}

static inline int visitlog_get(VisitLog visitlog, LmnWord p)
{
  st_data_t t;
  
  if (st_lookup(visitlog->log, (st_data_t)p, &t)) return t;
  else return -1;
}

static inline BOOL visitlog_contains(VisitLog visitlog, LmnWord p)
{
  return visitlog_get(visitlog, p) >= VISITLOG_INIT_N;
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
static inline int visitlog_element_num(VisitLog visitlog)
{
  return visitlog->element_num;
}

#endif
