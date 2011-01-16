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
#include "rule.h"
#include "st.h"
#include "delta_membrane.h"
#include "state.h"

struct ReactCxt {
  LmnMembrane *global_root; /* グローバルルート膜 */
  unsigned int work_vec_size;
  LmnRulesetId atomic_id;   /* atomic step中: atomic set id(signed int), default:-1 */
  BYTE mode;
  BOOL flag;
  void *v;                  /* 各mode毎に固有の持ち物 */
};

#define REACT_MEM_ORIENTED  (0x01U)       /* 膜主導テスト */
#define REACT_ND            (0x01U << 1)  /* 非決定実行: 状態の展開 */
#define REACT_STAND_ALONE   (0x01U << 2)  /* 非決定実行: 状態は展開しない */
#define REACT_PROPERTY      (0x01U << 3)  /* LTLモデル検査: 性質ルールのマッチングのみ */
#define REACT_ATOMIC        (0x01U << 4)  /* Atomic Step中: インタリーブの抑制 */

#define RC_SET_MODE(rc, m)             ((rc)->mode |= (m))
#define RC_UNSET_MODE(rc, m)           ((rc)->mode &= ~(m))
#define RC_GET_MODE(rc, m)             ((rc)->mode & (m))
#define RC_WORK_VEC_SIZE(rc)           ((rc)->work_vec_size)
#define RC_SET_WORK_VEC_SIZE(rc, n)    ((rc)->work_vec_size = (n))
#define RC_GROOT_MEM(rc)               ((rc)->global_root)
#define RC_SET_GROOT_MEM(rc, mem)      ((rc)->global_root = (mem))
#define RC_UNSET_GROOT_MEM(rc)         ((rc)->global_root = NULL)
#define RC_START_ATOMIC_STEP(rc, id)   ((rc)->atomic_id  = (id))
#define RC_IS_ATOMIC_STEP(rc)          ((rc)->atomic_id >= 0)
#define RC_FINISH_ATOMIC_STEP(rc)      ((rc)->atomic_id = -1)

/*----------------------------------------------------------------------
 * ND React Context
 */
struct NDReactCxtData {
  st_table_t succ_tbl;
  Vector *roots;        /* 通常時: struct LmnMembrane  差分時: struct MemDeltaRoot */
  Vector *rules;
  Vector *mem_deltas;
  struct MemDeltaRoot *mem_delta_root; /* commit命令でmallocしたデータをここに置き,
                                        * BODY命令をここに適用する.
                                        * 適用を終えたMemDeltaRootオブジェクトは,
                                        * Vector *mem_deltasにpushする.
                                        * delta時には, Vector *rootsには,
                                        * mallocされた空の状態データを積み,
                                        * 状態管理表へ登録する際にデータを記録する */
  BYTE property_state;
  unsigned int next_id;
};


#define RC_ND_DATA(rc)                  ((struct NDReactCxtData *)(rc)->v)
#define RC_SUCC_TBL(rc)                 ((RC_ND_DATA(rc))->succ_tbl)
#define RC_EXPANDED(rc)                 ((RC_ND_DATA(rc))->roots)
#define RC_EXPANDED_RULES(rc)           ((RC_ND_DATA(rc))->rules)
#define RC_MEM_DELTAS(rc)               ((RC_ND_DATA(rc))->mem_deltas)
#define RC_PROPERTY_STATE(rc)           ((RC_ND_DATA(rc))->property_state)
#define RC_SET_PROPERTY(rc, prop)       ((RC_ND_DATA(rc))->property_state = (prop))
#define RC_ND_SET_MEM_DELTA_ROOT(rc, d) ((RC_ND_DATA(rc))->mem_delta_root = (d))
#define RC_ND_MEM_DELTA_ROOT(rc)        ((RC_ND_DATA(rc))->mem_delta_root)
#define RC_ND_DELTA_ENABLE(rc)          RC_MEM_DELTAS(rc)
#define RC_CLEAR_DATA(rc) do {                                                 \
  RC_SET_GROOT_MEM(rc, NULL);                                                  \
  st_clear(RC_SUCC_TBL(rc));                                                   \
  vec_clear(RC_EXPANDED_RULES(rc));                                            \
  vec_clear(RC_EXPANDED(rc));                                                  \
  if (RC_MEM_DELTAS(rc)) {                                                     \
    int _d_i;                                                                  \
    for (_d_i = 0; _d_i < vec_num(RC_MEM_DELTAS(rc)); _d_i++) {                \
      dmem_root_free((struct MemDeltaRoot *)vec_get(RC_MEM_DELTAS(rc), _d_i)); \
    }                                                                          \
    vec_clear(RC_MEM_DELTAS(rc));                                              \
  }                                                                            \
  RC_SET_PROPERTY(rc, DEFAULT_STATE_ID);                                       \
} while (0)


/*----------------------------------------------------------------------
 * Mem React Context
 */
struct MemReactCxtData {
  LmnMemStack memstack; /* 膜主導実行時に使用 */
};

#define RC_MEMSTACK(rc)  (((struct MemReactCxtData *)(rc)->v)->memstack)


inline void react_context_init(struct ReactCxt *rc, BYTE mode);
inline void react_context_destroy(struct ReactCxt *rc);
inline void stand_alone_react_cxt_init(struct ReactCxt *cxt);
inline void stand_alone_react_cxt_destroy(struct ReactCxt *cxt);
inline void property_react_cxt_init(struct ReactCxt *cxt);
inline void property_react_cxt_destroy(struct ReactCxt *cxt);
inline void mem_react_cxt_init(struct ReactCxt *cxt);
inline void mem_react_cxt_destroy(struct ReactCxt *cxt);
inline void mc_react_cxt_init(struct ReactCxt *cxt, BYTE prop_state_id);
inline void mc_react_cxt_destroy(struct ReactCxt *cxt);
inline void mc_react_cxt_add_expanded(struct ReactCxt *cxt,
                                      LmnMembrane *mem,
                                      LmnRule rule);
inline void mc_react_cxt_add_mem_delta(struct ReactCxt *cxt,
                                       struct MemDeltaRoot *d,
                                       LmnRule rule);

static inline LmnWord mc_react_cxt_expanded_pop(struct ReactCxt *cxt) {
  vec_pop(RC_EXPANDED_RULES(cxt));
  return vec_pop(RC_ND_DELTA_ENABLE(cxt) ? RC_MEM_DELTAS(cxt)
                                         : RC_EXPANDED(cxt));
}

static inline unsigned int mc_react_cxt_expanded_num(struct ReactCxt *cxt) {
  return RC_ND_DELTA_ENABLE(cxt) ? vec_num(RC_MEM_DELTAS(cxt))
                                 : vec_num(RC_EXPANDED(cxt));
}

#endif
