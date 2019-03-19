/*
 * visitlog.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

/**
 * @ingroup  Verifier
 * @defgroup VisitLog
 * @{
 */

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"
#include <limits.h>

#define VISITLOG_INIT_N (1)

#ifndef PROC_TBL_DEFAULT_SIZE
#define PROC_TBL_DEFAULT_SIZE 128U
#endif

#ifndef PROC_TBL_BUCKETS_SIZE
#define PROC_TBL_BUCKETS_SIZE (1 << 12) // heuristics
#endif

/*----------------------------------------------------------------------
 * Visit Log
 */

/* VisitLog -
 * 同型性判定や、IDなどバックトラックをしながらグラフを探索する場合に用いる.
 * アトムや膜のログへの追加時には追加順に自動的に番号を付加する.
 * チェックポイントを使うことで,
 * バックトラック時にログをバックトラック前の状態に元に戻すことができる.
 */

typedef struct VisitLog *VisitLogRef;
typedef struct Checkpoint *CheckpointRef;

/* VisitLogに記録された変更のスナップショット */
struct Checkpoint {
  int n_data_atom;
  Vector* elements;
  Checkpoint():n_data_atom(0) {
    vec_init(this->elements, PROC_TBL_DEFAULT_SIZE);
  }

  ~Checkpoint() {
    vec_destroy(this->elements);
  }
};

/* 訪問済みのアトムや膜の記録 */
struct VisitLog {
  ProcessTableRef tbl; /* プロセスIDをkeyにした訪問表 */
  int ref_n, /* バイト列から読み出したプロセスに再訪問が発生した場合のための参照番号割当カウンタ
              */
      element_num;    /* 訪問したプロセス数のカウンタ */
  Vector* checkpoints; /* Checkpointオブジェクトの配列 */

  VisitLog():tbl(0),ref_n(0),element_num(0),checkpoints(0){}
  ~VisitLog(){
    proc_tbl_free(this->tbl);

    for (int i = 0; i < vec_num(this->checkpoints); i++) {
      vec_free((Vector *)vec_get(this->checkpoints, i));
    }
    vec_destroy(this->checkpoints);
  }

  void init() { this->init_with_size(0); }
  void init_with_size(unsigned long tbl_size) {
    if (tbl_size != 0) {
      this->tbl = proc_tbl_make_with_size(tbl_size);
    } else {
      this->tbl = proc_tbl_make();
    }
    /*   printf("size = %lu\n", tbl_size); */
    this->ref_n = VISITLOG_INIT_N;
    this->element_num = 0;
    vec_init(this->checkpoints, PROC_TBL_DEFAULT_SIZE);
  }

  /* チェックポイントを設定する。 */
  void set_checkpoint() {
    vec_push(this->checkpoints, (vec_data_t) new Checkpoint());
  }

  /* もっとも最近のチェックポイントを返し、ログの状態をチェックポイントが設定された時点にもどす */
  struct Checkpoint *pop_checkpoint() {
    int i;
    struct Checkpoint *checkpoint;

    checkpoint = (struct Checkpoint *)vec_pop(this->checkpoints);
    for (i = 0; i < vec_num(checkpoint->elements); i++) {
      proc_tbl_unput(this->tbl, vec_get(checkpoint->elements, i));
      this->element_num--;
      this->ref_n--;
    }
    this->element_num -= checkpoint->n_data_atom;
    return checkpoint;
  }
  /* もっとも最近のチェックポイントを消し、ログの状態をチェックポイントが設定された時点にもどす */
  void revert_checkpoint() {
    delete this->pop_checkpoint();
  }
  /* ログの状態はそのままに、もっとも最近に設定したチェックポイントを消す */
  void commit_checkpoint() {
    struct Checkpoint *last =
        (struct Checkpoint *)vec_pop(this->checkpoints);

    if (vec_num(this->checkpoints) > 0) {
      int i;
      struct Checkpoint *new_last =
          (struct Checkpoint *)vec_last(this->checkpoints);

      for (i = 0; i < vec_num(last->elements); i++) {
        vec_push(new_last->elements, vec_get(last->elements, i));
      }
      new_last->n_data_atom += last->n_data_atom;
    }
    delete last;
  }
};

/**
 * Function ProtoTypes
 */

void visitlog_push_checkpoint(VisitLogRef visitlog, CheckpointRef cp);

int visitlog_put(VisitLogRef visitlog, LmnWord p);
int visitlog_put_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom);
int visitlog_put_mem(VisitLogRef visitlog, LmnMembraneRef mem);
int visitlog_put_hlink(VisitLogRef visitlog, HyperLink *hl);
void visitlog_put_data(VisitLogRef visitlog);
int visitlog_get_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom,
                      LmnWord *value);
int visitlog_get_mem(VisitLogRef visitlog, LmnMembraneRef mem, LmnWord *value);
int visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value);
int visitlog_element_num(VisitLogRef visitlog);

/* @} */

#endif /** LMN_VISITLOG_H */
