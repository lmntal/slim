/*
 * react_context.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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

/* ルールの適用時に使用するデータ */

#ifndef LMN_REACT_CONTEXT_H
#define LMN_REACT_CONTEXT_H

#include "lmntal.h"

struct ReactCxt {
  LmnWord mode;
  LmnMembrane *global_root; /* グローバルルート膜 */
  void *v;
};

#define RC_SET_MODE(rc, m) ((rc)->mode = (m))
/* #define RC_UNSET_MODE(rc, m) ((rc)->mode = ~(m)) */
#define RC_GET_MODE(rc, m) ((rc)->mode == (m))

#define RC_GROOT_MEM(rc)  ((rc)->global_root)
#define RC_SET_GROOT_MEM(rc, mem)  ((rc)->global_root = (mem))

#define  REACT_MEM_ORIENTED  1  /* 膜主導テスト */
#define  REACT_STAND_ALONE   2  /* 特別な処理を行わない */
#define  REACT_ND            4  /* 状態の展開(非決定実行) */

inline void stand_alone_react_cxt_init(struct ReactCxt *cxt);
inline void stand_alone_react_cxt_destroy(struct ReactCxt *cxt);

#endif
