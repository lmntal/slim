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

/**
 * Function ProtoTypes
 */

void checkpoint_free(CheckpointRef cp);

VisitLogRef visitlog_create();
void visitlog_init_with_size(VisitLogRef p, unsigned long tbl_size);
void visitlog_destroy(VisitLogRef p);
void visitlog_set_checkpoint(VisitLogRef visitlog);
CheckpointRef visitlog_pop_checkpoint(VisitLogRef visitlog);
void visitlog_revert_checkpoint(VisitLogRef visitlog);
void visitlog_commit_checkpoint(VisitLogRef visitlog);
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

#endif
