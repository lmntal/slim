/*
 * react_context.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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

#ifndef LMN_REACT_CONTEXT_HPP
#define LMN_REACT_CONTEXT_HPP

#include <cstdint>

typedef struct LmnRegister *LmnRegisterRef;

struct LmnReactCxt;
using LmnReactCxtRef = LmnReactCxt *;

#include "element/element.h"
#include "hyperlink.h"
#include "lmntal.h"
#include "memstack.h"
#include "rule.h"

struct LmnMembrane;
struct SimpleHashtbl;

struct LmnRegister {
  LmnWord wt;
  LmnByte at;
  LmnByte tt;
  LmnWord register_wt() { return this->wt; }
  LmnByte register_at() { return this->at; }
  LmnByte register_tt() { return this->tt; }
  void register_set_wt(LmnWord wt) { this->wt = wt; }
  void register_set_at(LmnByte at) { this->at = at; }
  void register_set_tt(LmnByte tt) { this->tt = tt; }
};

using LmnRegisterArray = std::vector<LmnRegister>;

namespace slim {
namespace vm {

/**
 *  ルール適用中に使用する情報を保持する.
 */
struct RuleContext {
  LmnRegisterArray work_array; /* ルール適用レジスタ */
  SimpleHashtbl *
      hl_sameproccxt; /* findatom
                         時のアトム番号と、同名型付きプロセス文脈を持つアトム引数との対応関係を保持
                       */

  RuleContext() { work_array = LmnRegisterArray(1024); }
  virtual ~RuleContext() {}

  RuleContext &operator=(const RuleContext &cxt) {
    this->work_array = cxt.work_array;
    return *this;
  }

#ifdef USE_FIRSTCLASS_RULE
  Vector *insertion_events;
#endif

  size_t capacity() const { return work_array.size(); }
  void warray_set(LmnRegisterArray &&array) { work_array = std::move(array); }
  void resize(size_t s) { work_array.resize(s); }

  LmnRegister &reg(unsigned int i) { return work_array.at(i); }

  LmnWord &wt(unsigned int i) { return reg(i).wt; }
  LmnByte &at(unsigned int i) { return reg(i).at; }
  LmnByte &tt(unsigned int i) { return reg(i).tt; }
};
} // namespace vm
} // namespace slim

void lmn_sameproccxt_clear(LmnReactCxtRef rc);

#define REACT_MEM_ORIENTED (0x01U) /* 膜主導テスト */
#define REACT_ND (0x01U << 1)      /* 非決定実行: 状態の展開 */
#define REACT_STAND_ALONE (0x01U << 2) /* 非決定実行: 状態は展開しない */
#define REACT_PROPERTY                                                         \
  (0x01U << 3) /* LTLモデル検査: 性質ルールのマッチングのみ */

struct LmnReactCxt : slim::vm::RuleContext {
  LmnMembrane
      *global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
  unsigned int trace_num; /* ルール適用回数 (通常実行用トレース実行で使用)  */
  BYTE mode;
  bool is_zerostep;
  /* 非決定実行:
   * 別々の状態のグローバルルート膜がマージされ得る（react_rule_nd用）*/
  bool keep_process_id_in_nd_mode;

  constexpr static size_t warray_DEF_SIZE = 1024U;

  LmnReactCxt() : is_zerostep(false), keep_process_id_in_nd_mode(false) {}
  LmnReactCxt(BYTE mode)
      : is_zerostep(false), keep_process_id_in_nd_mode(false) {
    this->mode = mode;
    global_root = NULL;
    hl_sameproccxt = NULL;
    trace_num = 0;
#ifdef USE_FIRSTCLASS_RULE
    insertion_events = vec_make(4);
#endif
  }

  LmnReactCxt &operator=(const LmnReactCxt &from) {
    this->RuleContext::operator=(from);
    this->mode = from.mode;
    this->global_root = from.global_root;
#ifdef USE_FIRSTCLASS_RULE
    vec_free(this->insertion_events);
    this->insertion_events = vec_copy(from.insertion_events);
#endif
    return *this;
  }

  virtual ~LmnReactCxt() {
    if (hl_sameproccxt) {
      lmn_sameproccxt_clear(this);
    }
#ifdef USE_FIRSTCLASS_RULE
    if (this->insertion_events) {
      vec_free(this->insertion_events);
    }
#endif
  }
};

/*----------------------------------------------------------------------
 * Mem React Context
 */

LmnMemStack lmn_memstack_make(void);
void lmn_memstack_free(LmnMemStack memstack);

struct MemReactContext;

LmnMemStack RC_MEMSTACK(MemReactContext *cxt);
void RC_MEMSTACK_SET(MemReactContext *cxt, LmnMemStack s);

struct MemReactContext : LmnReactCxt {
  MemReactContext() : LmnReactCxt(REACT_MEM_ORIENTED) {
    RC_MEMSTACK_SET(this, lmn_memstack_make());
  }

  ~MemReactContext() { lmn_memstack_free(RC_MEMSTACK(this)); }

  LmnMemStack memstack; /* 膜主導実行時に使用 */
};

BOOL RC_GET_MODE(LmnReactCxtRef cxt, BYTE mode);

unsigned int RC_TRACE_NUM(LmnReactCxtRef cxt);
unsigned int RC_TRACE_NUM_INC(LmnReactCxtRef cxt);

LmnMembraneRef RC_GROOT_MEM(LmnReactCxtRef cxt);
void RC_SET_GROOT_MEM(LmnReactCxtRef cxt, LmnMembraneRef mem);

SimpleHashtbl *RC_HLINK_SPC(LmnReactCxtRef cxt);
void RC_SET_HLINK_SPC(LmnReactCxtRef cxt, SimpleHashtbl *spc);

struct MCReactContext;
struct McReactCxtData *RC_ND_DATA(MCReactContext *cxt);

BOOL rc_hlink_opt(LmnInstrVar atomi, LmnReactCxtRef rc);

void react_context_copy(LmnReactCxtRef to, LmnReactCxtRef from);

#ifdef USE_FIRSTCLASS_RULE
/**
 * @brief Post an insertion event of a first-class rulesets.
 */
void lmn_rc_push_insertion(LmnReactCxtRef rc, LmnSymbolAtomRef satom,
                           LmnMembraneRef mem);
/**
 * @brief Execute posted insertion events.
 */
void lmn_rc_execute_insertion_events(LmnReactCxtRef rc);
#endif

/*----------------------------------------------------------------------
 * MC React Context
 */
struct McReactCxtData {
  st_table_t succ_tbl; /* 多重辺除去用 */
  Vector *roots;       /* 1. 遷移先計算中
                        *    通常: struct LmnMembrane
                        *    差分: 空
                        * 2. 遷移先計算後 (mc_gen_successor@mc.c以降)
                        * 　　通常: struct LmnMembraneへの参照を設定したstruct State
                        *    差分: 初期化設定のみを行ったstruct State　*/
  Vector *rules;
  Vector *props;
  Vector *mem_deltas; /* BODY命令の適用を終えたMemDeltaRootオブジェクトを置く */
  MemDeltaRoot
      *mem_delta_tmp; /* commit命令でmallocした差分オブジェクトを一旦ここに置く.
                       * BODY命令はこのMemDeltaRootオブジェクトへ適用する. */
  BYTE opt_mode; /* 最適化のモードを記録 */
  BYTE d_cur;
  unsigned int org_succ_num;
  McDporData *por;

  McReactCxtData();

  ~McReactCxtData() {
    st_free_table(succ_tbl);
    vec_free(roots);
    vec_free(rules);
    vec_free(props);
    if (mem_deltas) {
      vec_free(mem_deltas);
    }
  }
};

#define RC_MC_DREC_MAX (3)

#define RC_MC_DMEM_MASK (0x01U)
#define RC_MC_DPOR_MASK (0x01U << 1)
#define RC_MC_DPOR_NAIVE_MASK (0x01U << 2)
#define RC_MC_D_MASK (0x01U << 3)

#define RC_MC_OPT_FLAG(RC) ((RC_ND_DATA((MCReactContext *)RC))->opt_mode)
#define RC_MC_USE_DMEM(RC) (RC_MC_OPT_FLAG(RC) & RC_MC_DMEM_MASK)
#define RC_MC_SET_DMEM(RC) (RC_MC_OPT_FLAG(RC) |= RC_MC_DMEM_MASK)
#define RC_MC_UNSET_DMEM(RC) (RC_MC_OPT_FLAG(RC) &= (~RC_MC_DMEM_MASK))
#define RC_MC_USE_DPOR(RC) (RC_MC_OPT_FLAG(RC) & RC_MC_DPOR_MASK)
#define RC_MC_SET_DPOR(RC) (RC_MC_OPT_FLAG(RC) |= RC_MC_DPOR_MASK)
#define RC_MC_UNSET_DPOR(RC) (RC_MC_OPT_FLAG(RC) &= (~RC_MC_DPOR_MASK))
#define RC_MC_USE_DPOR_NAIVE(RC) (RC_MC_OPT_FLAG(RC) & RC_MC_DPOR_NAIVE_MASK)
#define RC_MC_SET_DPOR_NAIVE(RC) (RC_MC_OPT_FLAG(RC) |= RC_MC_DPOR_NAIVE_MASK)
#define RC_MC_UNSET_DPOR_NAIVE(RC)                                             \
  (RC_MC_OPT_FLAG(RC) &= (~RC_MC_DPOR_NAIVE_MASK))
#define RC_MC_USE_D(RC) (RC_MC_OPT_FLAG(RC) & RC_MC_D_MASK)
#define RC_MC_SET_D(RC) (RC_MC_OPT_FLAG(RC) |= RC_MC_D_MASK)
#define RC_MC_UNSET_D(RC) (RC_MC_OPT_FLAG(RC) &= (~RC_MC_D_MASK))

//#define RC_ND_DATA(RC)                  ((struct McReactCxtData *)(RC)->v)
#define RC_SUCC_TBL(RC) ((RC_ND_DATA((MCReactContext *)RC))->succ_tbl)
#define RC_EXPANDED(RC) ((RC_ND_DATA((MCReactContext *)RC))->roots)
#define RC_EXPANDED_RULES(RC) ((RC_ND_DATA((MCReactContext *)RC))->rules)
#define RC_EXPANDED_PROPS(RC) ((RC_ND_DATA((MCReactContext *)RC))->props)
#define RC_MEM_DELTAS(RC) ((RC_ND_DATA((MCReactContext *)RC))->mem_deltas)
#define RC_ND_SET_MEM_DELTA_ROOT(RC, D)                                        \
  ((RC_ND_DATA((MCReactContext *)RC))->mem_delta_tmp = (D))
#define RC_ND_MEM_DELTA_ROOT(RC)                                               \
  ((RC_ND_DATA((MCReactContext *)RC))->mem_delta_tmp)
#define RC_ND_ORG_SUCC_NUM(RC)                                                 \
  ((RC_ND_DATA((MCReactContext *)RC))->org_succ_num)
#define RC_ND_SET_ORG_SUCC_NUM(RC, N)                                          \
  ((RC_ND_DATA((MCReactContext *)RC))->org_succ_num = (N))
#define RC_POR_DATA(RC) ((RC_ND_DATA((MCReactContext *)RC))->por)
#define RC_D_CUR(RC) ((RC_ND_DATA((MCReactContext *)RC))->d_cur)
#define RC_D_COND(RC) (RC_D_CUR(RC) > 0)
#define RC_D_PROGRESS(RC)                                                      \
  do {                                                                         \
    if (RC_MC_USE_D(RC)) {                                                     \
      (RC_D_CUR(RC) = (RC_D_CUR(RC) + 1) % RC_MC_DREC_MAX);                    \
    }                                                                          \
  } while (0)
#define RC_CLEAR_DATA(RC)                                                      \
  do {                                                                         \
    RC_SET_GROOT_MEM(RC, NULL);                                                \
    st_clear(RC_SUCC_TBL(RC));                                                 \
    vec_clear(RC_EXPANDED_RULES(RC));                                          \
    vec_clear(RC_EXPANDED(RC));                                                \
    vec_clear(RC_EXPANDED_PROPS(RC));                                          \
    if (RC_MC_USE_DMEM(RC)) {                                                  \
      unsigned int _fr_;                                                       \
      for (_fr_ = 0;                                                           \
           _fr_ < RC_ND_ORG_SUCC_NUM(RC) && _fr_ < vec_num(RC_MEM_DELTAS(RC)); \
           _fr_++) {                                                           \
        MemDeltaRoot *_d_ = (MemDeltaRoot *)vec_get(RC_MEM_DELTAS(RC), _fr_);  \
        if (_d_)                                                               \
          dmem_root_free(_d_);                                                 \
      }                                                                        \
      vec_clear(RC_MEM_DELTAS(RC));                                            \
    }                                                                          \
    RC_ND_SET_ORG_SUCC_NUM(RC, 0);                                             \
  } while (0)

struct MCReactContext : LmnReactCxt {
  MCReactContext() : LmnReactCxt(REACT_ND) {
    if (data.mem_deltas) {
      RC_MC_SET_DMEM(this);
    }

    if (lmn_env.enable_por_old) {
      RC_MC_SET_DPOR_NAIVE(this);
    } else if (lmn_env.enable_por) {
      RC_MC_SET_DPOR(this);
    }

    if (lmn_env.d_compress) {
      RC_MC_SET_D(this);
    }
  }

  McReactCxtData data;
};

void mc_react_cxt_add_expanded(LmnReactCxtRef cxt, LmnMembraneRef mem,
                               LmnRuleRef rule);
void mc_react_cxt_add_mem_delta(LmnReactCxtRef cxt, struct MemDeltaRoot *d,
                                LmnRuleRef rule);

LmnWord mc_react_cxt_expanded_pop(LmnReactCxtRef cxt);

LmnWord mc_react_cxt_expanded_get(LmnReactCxtRef cxt, unsigned int i);

unsigned int mc_react_cxt_succ_num_org(LmnReactCxtRef cxt);

unsigned int mc_react_cxt_expanded_num(LmnReactCxtRef cxt);


#endif
