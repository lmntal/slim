/*
 * visitlog.cpp
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
#include "visitlog.h"



/* チェックポイントをログに追加する */
void visitlog_push_checkpoint(VisitLogRef visitlog, struct Checkpoint *cp) {
  int i;

  vec_push(visitlog->checkpoints, (vec_data_t)cp);
  for (i = 0; i < vec_num(cp->elements); i++) {
    proc_tbl_put(visitlog->tbl, vec_get(cp->elements, i), visitlog->ref_n++);
    visitlog->element_num++;
  }
  visitlog->element_num += cp->n_data_atom;
}

/* ログにpを追加し, 正の値を返す. すでにpが存在した場合は0を返す.
 * 通常この関数ではなくput_atom, put_memを使用する. */
int visitlog_put(VisitLogRef visitlog, LmnWord p) {
  if (proc_tbl_put_new(visitlog->tbl, p, visitlog->ref_n++)) {
    if (vec_num(visitlog->checkpoints) > 0) {
      CheckpointRef checkpoint =
          (CheckpointRef)vec_last(visitlog->checkpoints);
      vec_push(checkpoint->elements, p);
    }
    visitlog->element_num++;
    return 1;
  } else {
    return 0;
  }
}

/* ログにアトムを追加し, 正の値を返す. すでにアトムが存在した場合は0を返す */
int visitlog_put_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom) {
  return visitlog_put(visitlog, atom->get_id());
}

/* ログに膜を追加し, 正の値を返す. すでに膜が存在した場合は0を返す */
int visitlog_put_mem(VisitLogRef visitlog, LmnMembraneRef mem) {
  return visitlog_put(visitlog, lmn_mem_id(mem));
}

/* ログにハイパーリンクを追加し, 正の値を返す.
 * すでにハイパーリンクが存在した場合は0を返す */
int visitlog_put_hlink(VisitLogRef visitlog, HyperLink *hl) {
  return visitlog_put(visitlog, LMN_HL_ID(hl));
}

/* ログにデータアトムを追加する.
 * （引数がログしか無いことから分かるように,
 * 単に訪問したアトムを数えるために使用する） */
void visitlog_put_data(VisitLogRef visitlog) {
  if (vec_num(visitlog->checkpoints) > 0) {
    struct Checkpoint *checkpoint =
        (struct Checkpoint *)vec_last(visitlog->checkpoints);
    checkpoint->n_data_atom++;
  }
  visitlog->element_num++;
}

/* ログに記録されたアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * ログにatomが存在しない場合は, 0を返す. */
int visitlog_get_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom,
                      LmnWord *value) {
  return proc_tbl_get_by_atom(visitlog->tbl, atom, value);
}

/* ログに記録された膜memに対応する値をvalueに設定, 正の値を返す.
 * ログにmemが存在しない場合は, 0を返す. */
int visitlog_get_mem(VisitLogRef visitlog, LmnMembraneRef mem, LmnWord *value) {
  return proc_tbl_get_by_mem(visitlog->tbl, mem, value);
}

/* ログに記録されたhlに対応する値をvalueに設定し, 正の値を返す.
 * ログにhlが存在しない場合は, 0を返す. */
int visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value) {
  return proc_tbl_get_by_hlink(visitlog->tbl, hl, value);
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
int visitlog_element_num(VisitLogRef visitlog) { return visitlog->element_num; }
