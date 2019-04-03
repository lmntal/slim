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
  Vector elements;
  Checkpoint():n_data_atom(0) {
    this->elements.init(PROC_TBL_DEFAULT_SIZE);
  }

  ~Checkpoint() {
    this->elements.destroy();
  }
};

/* 訪問済みのアトムや膜の記録 */
struct VisitLog {
  ProcessTableRef tbl; /* プロセスIDをkeyにした訪問表 */
  int ref_n, /* バイト列から読み出したプロセスに再訪問が発生した場合のための参照番号割当カウンタ
              */
      element_num;    /* 訪問したプロセス数のカウンタ */
  Vector checkpoints; /* Checkpointオブジェクトの配列 */

  VisitLog():tbl(0),ref_n(0),element_num(0){}
  ~VisitLog(){
    proc_tbl_free(this->tbl);

    for (int i = 0; i < this->checkpoints.get_num(); i++) {
      delete (Vector *)this->checkpoints.get(i);
    }
    this->checkpoints.destroy();
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
    this->checkpoints.init(PROC_TBL_DEFAULT_SIZE);
  }

  /* チェックポイントを設定する。 */
  void set_checkpoint() {
    this->checkpoints.push((vec_data_t) new Checkpoint());
  }

  /* もっとも最近のチェックポイントを返し、ログの状態をチェックポイントが設定された時点にもどす */
  struct Checkpoint *pop_checkpoint() {
    int i;
    struct Checkpoint *checkpoint;

    checkpoint = (struct Checkpoint *)this->checkpoints.pop();
    for (i = 0; i < checkpoint->elements.get_num(); i++) {
      proc_tbl_unput(this->tbl, checkpoint->elements.get(i));
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
        (struct Checkpoint *)this->checkpoints.pop();

    if (this->checkpoints.get_num() > 0) {
      int i;
      struct Checkpoint *new_last =
          (struct Checkpoint *)this->checkpoints.last();

      for (i = 0; i < last->elements.get_num(); i++) {
        new_last->elements.push(last->elements.get(i));
      }
      new_last->n_data_atom += last->n_data_atom;
    }
    delete last;
  }
  /* チェックポイントをログに追加する */
  void push_checkpoint(struct Checkpoint *cp) {
    this->checkpoints.push((vec_data_t)cp);
    for (int i = 0; i < cp->elements.get_num(); i++) {
      proc_tbl_put(this->tbl, cp->elements.get(i), this->ref_n++);
      this->element_num++;
    }
    this->element_num += cp->n_data_atom;
  }

  /* ログにpを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
   * 通常この関数ではなくput_atom, put_memを使用する. */
  int put(LmnWord p) {
    if (proc_tbl_put_new(this->tbl, p, this->ref_n++)) {
      if (this->checkpoints.get_num() > 0) {
        CheckpointRef checkpoint =
            (CheckpointRef)this->checkpoints.last();
        checkpoint->elements.push(p);
      }
      this->element_num++;
      return 1;
    } else {
      return 0;
    }
  }
  /* ログにアトムを追加し, 正の値を返す. すでにアトムが存在した場合は0を返す */
  int put_atom(LmnSymbolAtomRef atom) {
    return this->put(atom->get_id());
  }
  /* ログに膜を追加し, 正の値を返す. すでに膜が存在した場合は0を返す */
  int put_mem(LmnMembraneRef mem) {
    return this->put(mem->mem_id());
  }
  /* ログにハイパーリンクを追加し, 正の値を返す.
   * すでにハイパーリンクが存在した場合は0を返す */
  int put_hlink(HyperLink *hl) {
    return this->put(LMN_HL_ID(hl));
  }
  /* ログにデータアトムを追加する.
   * （引数がログしか無いことから分かるように,
   * 単に訪問したアトムを数えるために使用する） */
  void put_data() {
    if (this->checkpoints.get_num() > 0) {
      struct Checkpoint *checkpoint =
          (struct Checkpoint *)this->checkpoints.last();
      checkpoint->n_data_atom++;
    }
    this->element_num++;
  }

  /* ログに記録されたアトムatomに対応する値をvalueに設定し, 正の値を返す.
   * ログにatomが存在しない場合は, 0を返す. */
  int get_atom(LmnSymbolAtomRef atom,
                        LmnWord *value) {
    return proc_tbl_get_by_atom(this->tbl, atom, value);
  }
  /* ログに記録された膜memに対応する値をvalueに設定, 正の値を返す.
   * ログにmemが存在しない場合は, 0を返す. */
  int get_mem(LmnMembraneRef mem, LmnWord *value) {
    return proc_tbl_get_by_mem(this->tbl, mem, value);
  }
  /* ログに記録されたhlに対応する値をvalueに設定し, 正の値を返す.
   * ログにhlが存在しない場合は, 0を返す. */
  int get_hlink(HyperLink *hl, LmnWord *value) {
    return proc_tbl_get_by_hlink(this->tbl, hl, value);
  }
  /* visitlogに記録した要素（膜、アトム）の数を返す */
  int get_element_num() {
    return this->element_num;
  }
};

/* @} */

#endif /** LMN_VISITLOG_H */
