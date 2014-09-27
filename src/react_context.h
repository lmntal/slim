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
#include "internal_hash.h"

struct LmnRegister {
  LmnWord wt;
  LmnByte at;
  LmnByte tt;
};

struct LmnReactCxt {
  LmnMembrane *global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
  LmnRegister *work_arry;   /* ルール適用レジスタ */
  unsigned int warry_cur;   /* work_arryの現在の使用サイズ */
  unsigned int warry_num;   /* work_arryの最大使用サイズ(SPEC命令指定) */
  unsigned int warry_cap;   /* work_arryのキャパシティ */
  unsigned int trace_num;   /* ルール適用回数 (通常実行用トレース実行で使用)  */
  LmnRulesetId atomic_id;   /* atomic step中: atomic set id(signed int), default:-1 */
  ProcessID proc_org_id;    /* atomic step終了時に Process ID をこの値に復帰 */
  ProcessID proc_next_id;   /* atomic step継続時に Process ID をこの値に設定 */
  LmnMembrane *cur_mem;     /* atomic step継続時に現在膜をこの値に設定 */
  BYTE mode;
  BOOL flag;                /* mode以外に指定するフラグ */
  void *v;                  /* 各mode毎に固有の持ち物 */
  SimpleHashtbl *hl_sameproccxt; /* findatom 時のアトム番号と、同名型付きプロセス文脈を持つアトム引数との対応関係を保持 */
};

#define REACT_MEM_ORIENTED  (0x01U)       /* 膜主導テスト */
#define REACT_ND            (0x01U << 1)  /* 非決定実行: 状態の展開 */
#define REACT_STAND_ALONE   (0x01U << 2)  /* 非決定実行: 状態は展開しない */
#define REACT_PROPERTY      (0x01U << 3)  /* LTLモデル検査: 性質ルールのマッチングのみ */
#define REACT_ATOMIC        (0x01U << 4)  /* Atomic Step中: インタリーブの抑制 */

#define RC_MODE(RC)                    ((RC)->mode)
#define RC_SET_MODE(RC, M)             ((RC)->mode = (M))
#define RC_ADD_MODE(RC, M)             ((RC)->mode |= (M))
#define RC_UNPUT_MODE(RC, M)           ((RC)->mode &= ~(M))
#define RC_GET_MODE(RC, M)             ((RC)->mode & (M))


#define warry_size(RC)                 ((RC)->warry_cap)
#define warry_size_set(RC, N)          ((RC)->warry_cap = (N))
#define warry_use_size(RC)             ((RC)->warry_num)
#define warry_use_size_set(RC, N)      ((RC)->warry_num = (N))
#define warry_cur_size(RC)             ((RC)->warry_cur)
#define warry_cur_size_set(RC, N)      ((RC)->warry_cur = (N))
#define warry_cur_update(RC, I)                                                \
  if (warry_cur_size(RC) <= (I)) {                                             \
    warry_cur_size_set((RC), (I) + 1);                                         \
  }

#define WARRY_DEF_SIZE                 (1024U)
#define rc_warry(RC)                   ((RC)->work_arry)
#define rc_warry_set(RC, V)            ((RC)->work_arry = (V))
#define wt(RC, I)                      ((RC)->work_arry[I].wt)
#define wt_set(RC, I, O)                                                       \
  do {                                                                         \
    (RC)->work_arry[I].wt = (LmnWord)(O);                                      \
    warry_cur_update(RC, I);                                                   \
  } while (0)
#define at(RC, I)                      ((RC)->work_arry[I].at)
#define at_set(RC, I, O)                                                       \
  do {                                                                         \
    (RC)->work_arry[I].at = (LmnByte)(O);                                      \
    warry_cur_update(RC, I);                                                   \
  } while (0)
#define tt(RC, I)                      ((RC)->work_arry[I].tt)
#define tt_set(RC, I, O)                                                       \
  do {                                                                         \
    (RC)->work_arry[I].tt = (LmnByte)(O);                                      \
    warry_cur_update(RC, I);                                                   \
  } while (0)
#define warry_set(RC, I, W, A, T) do {                                         \
    (RC)->work_arry[I].wt = (LmnWord)(W);                                      \
    (RC)->work_arry[I].at = (LmnByte)(A);                                      \
    (RC)->work_arry[I].tt = (LmnByte)(T);                                      \
    warry_cur_update(RC, I);                                                   \
  } while (0)

#define RC_TRACE_NUM(RC)               ((RC)->trace_num)
#define RC_TRACE_NUM_INC(RC)           ((RC)->trace_num++)

#define RC_GROOT_MEM(RC)               ((RC)->global_root)
#define RC_SET_GROOT_MEM(RC, MEM)      ((RC)->global_root = (MEM))
#define RC_UNSET_GROOT_MEM(RC)         ((RC)->global_root = NULL)
#define RC_START_ATOMIC_STEP(RC, ID)   ((RC)->atomic_id  = (ID))
#define RC_IS_ATOMIC_STEP(RC)          ((RC)->atomic_id >= 0)
#define RC_FINISH_ATOMIC_STEP(RC)      ((RC)->atomic_id = -1)
#define RC_PROC_ORG_ID(RC)             ((RC)->proc_org_id)
#define RC_SET_PROC_ORG_ID(RC, ID)     ((RC)->proc_org_id = (ID)) 
#define RC_PROC_NEXT_ID(RC)            ((RC)->proc_next_id)
#define RC_SET_PROC_NEXT_ID(RC, ID)    ((RC)->proc_next_id = (ID)) 
#define RC_CUR_MEM(RC)                 ((RC)->cur_mem)
#define RC_SET_CUR_MEM(RC, MEM)        ((RC)->cur_mem = (MEM))

#define RC_SET_HLINK_SPC(RC, SPC)      ((RC)->hl_sameproccxt = (SPC))
#define RC_HLINK_SPC(RC)               ((RC)->hl_sameproccxt)

static inline BOOL rc_hlink_opt(LmnInstrVar atomi, LmnReactCxt *rc) {
  /*  return hl_sameproccxtが初期化済み && atomiは同名プロセス文脈を持つアトム */
  return RC_HLINK_SPC(rc) &&
         hashtbl_contains(RC_HLINK_SPC(rc), (HashKeyType)atomi);
}


LmnRegister *lmn_register_make(unsigned int size);
void lmn_register_free(LmnRegister *v);
void lmn_register_extend(LmnReactCxt *rc, unsigned int new_size);

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

#define RC_ND_DATA(RC)                  ((struct McReactCxtData *)(RC)->v)
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

#define RC_MEMSTACK(RC)  (((struct MemReactCxtData *)(RC)->v)->memstack)


void react_context_init(LmnReactCxt *rc, BYTE mode);
void react_context_destroy(LmnReactCxt *rc);
void stand_alone_react_cxt_init(LmnReactCxt *cxt);
void stand_alone_react_cxt_destroy(LmnReactCxt *cxt);
void property_react_cxt_init(LmnReactCxt *cxt);
void property_react_cxt_destroy(LmnReactCxt *cxt);
void mem_react_cxt_init(LmnReactCxt *cxt);
void mem_react_cxt_destroy(LmnReactCxt *cxt);
void mc_react_cxt_init(LmnReactCxt *cxt);
void mc_react_cxt_destroy(LmnReactCxt *cxt);
void mc_react_cxt_add_expanded(LmnReactCxt *cxt,
                               LmnMembrane *mem,
                               LmnRule rule);
void mc_react_cxt_add_mem_delta(LmnReactCxt *cxt,
                                       struct MemDeltaRoot *d,
                                       LmnRule rule);

static inline LmnWord mc_react_cxt_expanded_pop(LmnReactCxt *cxt) {
  vec_pop(RC_EXPANDED_RULES(cxt));
  if (RC_MC_USE_DMEM(cxt)) {
    return vec_pop(RC_MEM_DELTAS(cxt));
  } else {
    return vec_pop(RC_EXPANDED(cxt));
  }
}

static inline LmnWord mc_react_cxt_expanded_get(LmnReactCxt *cxt, unsigned int i) {
  if (RC_MC_USE_DMEM(cxt)) {
    return vec_get(RC_MEM_DELTAS(cxt), i);
  } else {
    return vec_get(RC_EXPANDED(cxt), i);
  }
}

static inline unsigned int mc_react_cxt_succ_num_org(LmnReactCxt *cxt) {
  return RC_ND_ORG_SUCC_NUM(cxt);
}

static inline unsigned int mc_react_cxt_expanded_num(LmnReactCxt *cxt) {
  return RC_MC_USE_DMEM(cxt) ? vec_num(RC_MEM_DELTAS(cxt))
                             : vec_num(RC_EXPANDED(cxt));
}

#endif
