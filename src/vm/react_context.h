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

/* cldoc:begin-category(Lmntal::ReactContext) */

typedef struct LmnRegister *LmnRegisterRef;
typedef struct __LmnRegisterArray *LmnRegisterArray;

typedef struct LmnReactCxt *LmnReactCxtRef;


#include "lmntal.h"
#include "rule.h"
#include "memstack.h"
#include "element/element.h"

LmnWord lmn_register_wt(LmnRegisterRef r);
LmnByte lmn_register_at(LmnRegisterRef r);
LmnByte lmn_register_tt(LmnRegisterRef r);
void lmn_register_set_wt(LmnRegisterRef r, LmnWord wt);
void lmn_register_set_at(LmnRegisterRef r, LmnByte at);
void lmn_register_set_tt(LmnRegisterRef r, LmnByte tt);
LmnRegisterRef lmn_register_array_get(LmnRegisterArray array, int idx);


#define REACT_MEM_ORIENTED  (0x01U)       /* 膜主導テスト */
#define REACT_ND            (0x01U << 1)  /* 非決定実行: 状態の展開 */
#define REACT_STAND_ALONE   (0x01U << 2)  /* 非決定実行: 状態は展開しない */
#define REACT_PROPERTY      (0x01U << 3)  /* LTLモデル検査: 性質ルールのマッチングのみ */
#define REACT_ATOMIC        (0x01U << 4)  /* Atomic Step中: インタリーブの抑制 */
#define REACT_ND_MERGE_STS  (0x01U << 5 | REACT_ND)  /* 非決定実行: 別々の状態のグローバルルート膜がマージされ得る（react_rule_nd用） */
#define REACT_ZEROSTEP      (0x01U << 6)  /**< 0step rule application */

BYTE RC_MODE(LmnReactCxtRef cxt);
void RC_SET_MODE(LmnReactCxtRef cxt, BYTE mode);
void RC_ADD_MODE(LmnReactCxtRef cxt, BYTE mode);
BOOL RC_GET_MODE(LmnReactCxtRef cxt, BYTE mode);

unsigned int warry_size(LmnReactCxtRef cxt);
void warry_size_set(LmnReactCxtRef cxt, unsigned int n);
unsigned int warry_use_size(LmnReactCxtRef cxt);
void warry_use_size_set(LmnReactCxtRef cxt, unsigned int n);
unsigned int warry_cur_size(LmnReactCxtRef cxt);
void warry_cur_size_set(LmnReactCxtRef cxt, unsigned int n);
void warry_cur_update(LmnReactCxtRef cxt, unsigned int i);

#define WARRY_DEF_SIZE                 (1024U)
LmnRegisterArray rc_warry(LmnReactCxtRef cxt);
void rc_warry_set(LmnReactCxtRef cxt, LmnRegisterArray arry);
LmnWord wt(LmnReactCxtRef cxt, unsigned int i);
void wt_set(LmnReactCxtRef cxt, unsigned int i, LmnWord o);
LmnByte at(LmnReactCxtRef cxt, unsigned int i);
void at_set(LmnReactCxtRef cxt, unsigned int i, LmnByte o);
LmnByte tt(LmnReactCxtRef cxt, unsigned int i);
void tt_set(LmnReactCxtRef cxt, unsigned int i, LmnByte o);
void warry_set(LmnReactCxtRef cxt, unsigned int i, LmnWord w, LmnByte a, LmnByte t);

unsigned int RC_TRACE_NUM(LmnReactCxtRef cxt);
unsigned int RC_TRACE_NUM_INC(LmnReactCxtRef cxt);

LmnMembraneRef RC_GROOT_MEM(LmnReactCxtRef cxt);
void RC_SET_GROOT_MEM(LmnReactCxtRef cxt, LmnMembraneRef mem);
void RC_START_ATOMIC_STEP(LmnReactCxtRef cxt, LmnRulesetId id);
BOOL RC_IS_ATOMIC_STEP(LmnReactCxtRef cxt);
void RC_FINISH_ATOMIC_STEP(LmnReactCxtRef cxt);
ProcessID RC_PROC_ORG_ID(LmnReactCxtRef cxt);
void RC_SET_PROC_ORG_ID(LmnReactCxtRef cxt, ProcessID id);
ProcessID RC_PROC_NEXT_ID(LmnReactCxtRef cxt);
void RC_SET_PROC_NEXT_ID(LmnReactCxtRef cxt, ProcessID id);
LmnMembraneRef RC_CUR_MEM(LmnReactCxtRef cxt);
void RC_SET_CUR_MEM(LmnReactCxtRef cxt, LmnMembraneRef mem);

SimpleHashtbl *RC_HLINK_SPC(LmnReactCxtRef cxt);
void RC_SET_HLINK_SPC(LmnReactCxtRef cxt, SimpleHashtbl *spc);

struct McReactCxtData *RC_ND_DATA(LmnReactCxtRef cxt);

BOOL rc_hlink_opt(LmnInstrVar atomi, LmnReactCxtRef rc);


LmnRegisterArray lmn_register_make(unsigned int size);
void lmn_register_copy(LmnRegisterArray to, LmnRegisterArray from, unsigned int size);
void lmn_register_free(LmnRegisterArray v);
void lmn_register_extend(LmnReactCxtRef rc, unsigned int new_size);
void react_context_copy(LmnReactCxtRef to, LmnReactCxtRef from);

#ifdef USE_FIRSTCLASS_RULE
/**
 * @brief Post an insertion event of a first-class rulesets.
 */
void lmn_rc_push_insertion(LmnReactCxtRef rc, LmnSymbolAtomRef satom, LmnMembraneRef mem);
/**
 * @brief Execute posted insertion events.
 */
void lmn_rc_execute_insertion_events(LmnReactCxtRef rc);
#endif

/*----------------------------------------------------------------------
 * MC React Context
 */
struct McReactCxtData {
  st_table_t   succ_tbl;       /* 多重辺除去用 */
  Vector       *roots;         /* 1. 遷移先計算中
                                *    通常: struct LmnMembrane
                                *    差分: 空
                                * 2. 遷移先計算後 (mc_gen_successor@mc.c以降)
                                * 　　通常: struct LmnMembraneへの参照を設定したstruct State
                                *    差分: 初期化設定のみを行ったstruct State　*/
  Vector       *rules;
  Vector       *props;
  Vector       *mem_deltas;    /* BODY命令の適用を終えたMemDeltaRootオブジェクトを置く */
  MemDeltaRoot *mem_delta_tmp; /* commit命令でmallocした差分オブジェクトを一旦ここに置く.
                                * BODY命令はこのMemDeltaRootオブジェクトへ適用する. */
  BYTE         opt_mode;       /* 最適化のモードを記録 */
  BYTE         d_cur;
  unsigned int org_succ_num;
  McDporData   *por;
};

#define RC_MC_DREC_MAX                  (3)

#define RC_MC_DMEM_MASK                 (0x01U)
#define RC_MC_DPOR_MASK                 (0x01U << 1)
#define RC_MC_DPOR_NAIVE_MASK           (0x01U << 2)
#define RC_MC_D_MASK                    (0x01U << 3)

#define RC_MC_OPT_FLAG(RC)              ((RC_ND_DATA(RC))->opt_mode)
#define RC_MC_USE_DMEM(RC)              (RC_MC_OPT_FLAG(RC) &   RC_MC_DMEM_MASK)
#define RC_MC_SET_DMEM(RC)              (RC_MC_OPT_FLAG(RC) |=  RC_MC_DMEM_MASK)
#define RC_MC_UNSET_DMEM(RC)            (RC_MC_OPT_FLAG(RC) &=(~RC_MC_DMEM_MASK))
#define RC_MC_USE_DPOR(RC)              (RC_MC_OPT_FLAG(RC) &   RC_MC_DPOR_MASK)
#define RC_MC_SET_DPOR(RC)              (RC_MC_OPT_FLAG(RC) |=  RC_MC_DPOR_MASK)
#define RC_MC_UNSET_DPOR(RC)            (RC_MC_OPT_FLAG(RC) &=(~RC_MC_DPOR_MASK))
#define RC_MC_USE_DPOR_NAIVE(RC)        (RC_MC_OPT_FLAG(RC) &   RC_MC_DPOR_NAIVE_MASK)
#define RC_MC_SET_DPOR_NAIVE(RC)        (RC_MC_OPT_FLAG(RC) |=  RC_MC_DPOR_NAIVE_MASK)
#define RC_MC_UNSET_DPOR_NAIVE(RC)      (RC_MC_OPT_FLAG(RC) &=(~RC_MC_DPOR_NAIVE_MASK))
#define RC_MC_USE_D(RC)                 (RC_MC_OPT_FLAG(RC) &   RC_MC_D_MASK)
#define RC_MC_SET_D(RC)                 (RC_MC_OPT_FLAG(RC) |=  RC_MC_D_MASK)
#define RC_MC_UNSET_D(RC)               (RC_MC_OPT_FLAG(RC) &=(~RC_MC_D_MASK))

//#define RC_ND_DATA(RC)                  ((struct McReactCxtData *)(RC)->v)
#define RC_SUCC_TBL(RC)                 ((RC_ND_DATA(RC))->succ_tbl)
#define RC_EXPANDED(RC)                 ((RC_ND_DATA(RC))->roots)
#define RC_EXPANDED_RULES(RC)           ((RC_ND_DATA(RC))->rules)
#define RC_EXPANDED_PROPS(RC)           ((RC_ND_DATA(RC))->props)
#define RC_MEM_DELTAS(RC)               ((RC_ND_DATA(RC))->mem_deltas)
#define RC_ND_SET_MEM_DELTA_ROOT(RC, D) ((RC_ND_DATA(RC))->mem_delta_tmp = (D))
#define RC_ND_MEM_DELTA_ROOT(RC)        ((RC_ND_DATA(RC))->mem_delta_tmp)
#define RC_ND_ORG_SUCC_NUM(RC)          ((RC_ND_DATA(RC))->org_succ_num)
#define RC_ND_SET_ORG_SUCC_NUM(RC, N)   ((RC_ND_DATA(RC))->org_succ_num = (N))
#define RC_POR_DATA(RC)                 ((RC_ND_DATA(RC))->por)
#define RC_D_CUR(RC)                    ((RC_ND_DATA(RC))->d_cur)
#define RC_D_COND(RC)                   (RC_D_CUR(RC) > 0)
#define RC_D_PROGRESS(RC)                                                      \
  do {                                                                         \
    if (RC_MC_USE_D(RC)) {                                                     \
      (RC_D_CUR(RC) = (RC_D_CUR(RC) + 1) % RC_MC_DREC_MAX);                    \
    }                                                                          \
  } while (0)
#define RC_CLEAR_DATA(RC) do {                                                 \
  RC_SET_GROOT_MEM(RC, NULL);                                                  \
  st_clear(RC_SUCC_TBL(RC));                                                   \
  vec_clear(RC_EXPANDED_RULES(RC));                                            \
  vec_clear(RC_EXPANDED(RC));                                                  \
  vec_clear(RC_EXPANDED_PROPS(RC));                                            \
  if (RC_MC_USE_DMEM(RC)) {                                                    \
    unsigned int _fr_;                                                         \
    for (_fr_ = 0;                                                             \
         _fr_ < RC_ND_ORG_SUCC_NUM(RC) && _fr_ < vec_num(RC_MEM_DELTAS(RC));   \
         _fr_++) {                                                             \
      MemDeltaRoot *_d_ = (MemDeltaRoot *)vec_get(RC_MEM_DELTAS(RC), _fr_);    \
      if (_d_) dmem_root_free(_d_);                                            \
    }                                                                          \
    vec_clear(RC_MEM_DELTAS(RC));                                              \
  }                                                                            \
  RC_ND_SET_ORG_SUCC_NUM(RC, 0);                                               \
} while (0)


/*----------------------------------------------------------------------
 * Mem React Context
 */
struct MemReactCxtData {
  LmnMemStack memstack; /* 膜主導実行時に使用 */
};

LmnMemStack RC_MEMSTACK(LmnReactCxtRef cxt);
void RC_MEMSTACK_SET(LmnReactCxtRef cxt, LmnMemStack s);

LmnReactCxtRef react_context_alloc();
void react_context_dealloc(LmnReactCxtRef cxt);
void react_context_init(LmnReactCxtRef rc, BYTE mode);
void react_context_destroy(LmnReactCxtRef rc);
void stand_alone_react_cxt_init(LmnReactCxtRef cxt);
void stand_alone_react_cxt_destroy(LmnReactCxtRef cxt);
void property_react_cxt_init(LmnReactCxtRef cxt);
void property_react_cxt_destroy(LmnReactCxtRef cxt);
void mem_react_cxt_init(LmnReactCxtRef cxt);
void mem_react_cxt_destroy(LmnReactCxtRef cxt);
void mc_react_cxt_init(LmnReactCxtRef cxt);
void mc_react_cxt_destroy(LmnReactCxtRef cxt);
void mc_react_cxt_add_expanded(LmnReactCxtRef cxt,
                               LmnMembraneRef mem,
                               LmnRuleRef rule);
void mc_react_cxt_add_mem_delta(LmnReactCxtRef cxt,
                                       struct MemDeltaRoot *d,
                                       LmnRuleRef rule);

LmnWord mc_react_cxt_expanded_pop(LmnReactCxtRef cxt);

LmnWord mc_react_cxt_expanded_get(LmnReactCxtRef cxt, unsigned int i);

unsigned int mc_react_cxt_succ_num_org(LmnReactCxtRef cxt);

unsigned int mc_react_cxt_expanded_num(LmnReactCxtRef cxt);

/* cldoc:end-category() */

#endif
