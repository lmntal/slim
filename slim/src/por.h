/*
 * por.h
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
 * $Id$
 */

#ifndef POR_H
#define POR_H

#include "lmntal.h"
#include "mc.h"
#include "vector.h"
#include "st.h"

st_table *strans_independency; /* 独立性情報テーブル: 構造体StateTransitionのidをキーとし，bins[id]は高々1個のエントリーを持つ．
                                *   エントリーの値はVectorであり，キーとなったidを持つ遷移と独立な関係にある遷移のidがpushされたものとなる．*/
st_table *States_POR;    /* ample(s)計算中のみ使用．展開されたすべてのStateを管理． */
Vector *Stack_POR;       /* C1のチェックにあたってstate graphを展開するする際に使用 */
Vector *succ_strans;     /* ある状態sから可能な遷移の集合を管理 */
Vector *ample_candidate; /* ample(s)の候補を管理するVector．本Vector内のすべての遷移が，C0〜C3のチェック対象となる */

extern unsigned long next_strans_id;

LMN_EXTERN Vector *ample(const StateSpace states, State *s);

void init_por_vars(void);
void free_por_vars(void);

#endif
