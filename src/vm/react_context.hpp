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
#include <vector>
#include <algorithm>
#include <bitset>
#include <unordered_map>
#include <atomic>

typedef struct LmnRegister *LmnRegisterRef;

struct LmnReactCxt;
using LmnReactCxtRef = LmnReactCxt *;

#include "element/element.h"
#include "hyperlink.h"
#include "lmntal.h"
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

  RuleContext() : hl_sameproccxt(nullptr), work_array(1024) {
#ifdef USE_FIRSTCLASS_RULE
    insertion_events = new Vector(4);
#endif
  }
  virtual ~RuleContext() {
    if (hl_sameproccxt) {
      clear_hl_spc();
    }
#ifdef USE_FIRSTCLASS_RULE
    if (this->insertion_events) {
      delete this->insertion_events;
    }
#endif
  }

  RuleContext &operator=(const RuleContext &cxt) {
    // work array を deep copy するように変更
    //this->work_array = new Vector(cxt.work_array);
    //this->work_array = std::vector<LmnRegister>(std::begin(cxt.work_array), std::end(cxt.work_array));
    this->work_array = cxt.work_array;
#ifdef USE_FIRSTCLASS_RULE
    delete this->insertion_events;
    this->insertion_events = new Vector(*from.insertion_events);
#endif
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

  void prepare_hl_spc() {
    hl_sameproccxt = hashtbl_make(2);
  }
  void clear_hl_spc();
};
} // namespace vm
} // namespace slim

#define REACT_MEM_ORIENTED (0x01U) /* 膜主導テスト */
#define REACT_ND (0x01U << 1)      /* 非決定実行: 状態の展開 */
#define REACT_STAND_ALONE (0x01U << 2) /* 非決定実行: 状態は展開しない */
#define REACT_PROPERTY                                                         \
  (0x01U << 3) /* LTLモデル検査: 性質ルールのマッチングのみ */

struct LmnReactCxt : slim::vm::RuleContext {
  LmnMembrane
      *global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
private:
  unsigned int trace_num; /* ルール適用回数 (通常実行用トレース実行で使用)  */
public:
  BYTE mode;
  bool is_zerostep;
  /* 非決定実行:
   * 別々の状態のグローバルルート膜がマージされ得る（react_rule_nd用）*/
  bool keep_process_id_in_nd_mode;

  constexpr static size_t warray_DEF_SIZE = 1024U;

  LmnReactCxt() : is_zerostep(false), keep_process_id_in_nd_mode(false), trace_num(0) {}
  LmnReactCxt(LmnMembrane *groot, BYTE mode)
      : is_zerostep(false), keep_process_id_in_nd_mode(false), trace_num(0),
        mode(mode), global_root(groot) {
  }

  LmnReactCxt &operator=(const LmnReactCxt &from) {
    this->RuleContext::operator=(from);
    this->mode = from.mode;
    this->global_root = from.global_root;
    return *this;
  }

  unsigned int get_reaction_count() const { return trace_num; }
  void increment_reaction_count() { trace_num++; }

  bool has_mode(BYTE mode) const {
    return (this->mode & mode) != 0;
  }

  LmnMembrane *get_global_root() { return global_root; }
  SimpleHashtbl *get_hl_sameproccxt() { return hl_sameproccxt; }
};

/*----------------------------------------------------------------------
 * Mem React Context
 * 膜主導実行時に使用
 */

class MemReactContext : public LmnReactCxt {

public:
  std::vector<LmnMembrane *> memstack;
  // MemReactContext& operator=(const MemReactContext &mrc) {
  //   *this = new LmnReactCxt(mrc.global_root, REACT_MEM_ORIENTED);
  //   this->memstack = mrc.memstack;
  //   return *this;
  // }

  // LmnReactCxt &operator=(const LmnReactCxt &from) {
  //   this->RuleContext::operator=(from);
  //   this->mode = from.mode;
  //   this->global_root = from.global_root;
  //   return *this;
  // }

  MemReactContext(LmnMembrane *mem) :
      LmnReactCxt(mem, REACT_MEM_ORIENTED) {}

  bool memstack_isempty() const {
    return memstack.empty();
  }
  void memstack_push_front(LmnMembrane *mem){
    memstack.insert(memstack.begin(), mem);
  }
  void memstack_push(LmnMembrane *mem) {
    memstack.push_back(mem);
    mem->set_active(true);
  }
  LmnMembrane *memstack_pop() {
    auto result = memstack.back();
    memstack.pop_back();
    // result->set_active(false);
    return result;
  }
  LmnMembrane *memstack_peek() {
    return memstack.back();
  }
  LmnMembrane *memstack_first() {
    return memstack.front();
  }
  // auto memstack_begin() {
  //   return memstack.begin();
  // }
  // auto memstack_end() {
  //   return memstack.end();
  // }
  // void memstack_clear(){
  //   memstack.clear();
  // }
  // auto get_ith_mem(int i){
  //   return memstack[i];
  // }
  // int get_size(){
  //   return memstack.size();
  // }

  /* 実行膜スタックからmemを削除する。外部関数が膜の削除しようとするとき
   に、その膜がスタックに積まれている事がある。そのため、安全な削除を行
   うために、この手続きが必要になる。外部の機能を使わない通常の実行時に
   はこの手続きは必要ない*/
  void memstack_remove(LmnMembrane *mem) {
    memstack.erase(std::remove(memstack.begin(), memstack.end(), mem), memstack.end());
  }
  void memstack_reconstruct(LmnMembrane *mem) {
    memstack.clear();
    memstack_reconstruct_rec(mem);
  }

private:
  /* 親膜を子膜よりも先に積む */
  void memstack_reconstruct_rec(LmnMembrane *parent) {
    memstack_push(parent);
    for (auto m = parent->mem_child_head(); m; m = m->mem_next()) {
      memstack_reconstruct_rec(m);
    }
  }
};

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

#define RC_MC_DREC_MAX (3)

enum ModelCheckerOptimazeMode {
  DeltaMembrane,
  DynamicPartialOrderReduction,
  DynamicPartialOrderReduction_Naive,
  BinaryStringDeltaCompress,
};
const unsigned int ModelCheckerOptimazeModeSize = 4;

#define RC_ND_SET_MEM_DELTA_ROOT(RC, D)                                        \
  ((((MCReactContext *)RC))->mem_delta_tmp = (D))
#define RC_ND_MEM_DELTA_ROOT(RC)                                               \
  ((((MCReactContext *)RC))->mem_delta_tmp)
#define RC_ND_ORG_SUCC_NUM(RC)                                                 \
  ((((MCReactContext *)RC))->org_succ_num)
#define RC_ND_SET_ORG_SUCC_NUM(RC, N)                                          \
  ((((MCReactContext *)RC))->org_succ_num = (N))
#define RC_POR_DATA(RC) ((((MCReactContext *)RC))->por)
#define RC_D_CUR(RC) ((((MCReactContext *)RC))->d_cur)
#define RC_D_COND(RC) (RC_D_CUR(RC) > 0)
#define RC_D_PROGRESS(RC)                                                      \
  do {                                                                         \
    if ((RC)->has_optmode(BinaryStringDeltaCompress)) {                                                     \
      (RC_D_CUR(RC) = (RC_D_CUR(RC) + 1) % RC_MC_DREC_MAX);                    \
    }                                                                          \
  } while (0)
#define RC_CLEAR_DATA(RC)                                                      \
  do {                                                                         \
    (RC)->set_global_root(nullptr);                                              \
    (RC)->clear_successor_table();                                                 \
    (RC)->clear_expanded_rules();                                          \
    (RC)->clear_expanded_states();                                                \
    (RC)->clear_expanded_properties();                                          \
    if ((RC)->has_optmode(DeltaMembrane)) {                                                  \
      unsigned int _fr_;                                                       \
      for (_fr_ = 0;                                                           \
           _fr_ < RC_ND_ORG_SUCC_NUM(RC) && _fr_ < (RC)->get_mem_delta_roots().size(); \
           _fr_++) {                                                           \
        MemDeltaRoot *_d_ = (RC)->get_mem_delta_roots().at(_fr_);  \
        if (_d_)                                                               \
          delete (_d_);                                                 \
      }                                                                        \
      (RC)->clear_mem_delta_roots();                                            \
    }                                                                          \
    RC_ND_SET_ORG_SUCC_NUM(RC, 0);                                             \
  } while (0)

struct MCReactContext : LmnReactCxt {
  MCReactContext(LmnMembrane *mem);

  void set_global_root(LmnMembrane *mem) {
    global_root = mem;
  }
    
  MemDeltaRoot
      *mem_delta_tmp; /* commit命令でmallocした差分オブジェクトを一旦ここに置く.
                       * BODY命令はこのMemDeltaRootオブジェクトへ適用する. */
  BYTE d_cur;
  unsigned int org_succ_num;
  McDporData *por;

  bool has_optmode(ModelCheckerOptimazeMode mode) const {
    return opt_mode.test(mode);
  }
  void turnon_optmode(ModelCheckerOptimazeMode mode) {
    opt_mode.set(mode);
  }
  void turnoff_optmode(ModelCheckerOptimazeMode mode) {
    opt_mode.reset(mode);
  }

  void clear_successor_table() {
    succ_tbl.clear();
  }
  void *get_transition_to(State *succ) {
    auto it = succ_tbl.find(succ);
    return it == succ_tbl.end() ? nullptr : it->second;
  }
  void set_transition_to(State *state, void *succ) {
    succ_tbl[state] = succ;
  }

  std::vector<void *> const &expanded_states() const {
    return roots;
  }
  void *expanded_states(int i) const {
    return roots[i];
  }

  void set_expanded_state(int idx, void *s) {
    roots[idx] = s;
  }
  void resize_expanded_states(int size) {
    roots.resize(size);
  }
  void clear_expanded_states() {
    roots.clear();
  }
  void push_expanded_state(void *s) {
    roots.push_back(s);
  }

  LmnRule *get_expanded_rule(int i) {
    return rules.at(i);
  }
  void resize_expanded_rules(int size) {
    rules.resize(size);
  }
  void push_expanded_rule(LmnRule *rule) {
    rules.push_back(rule);
  }
  void clear_expanded_rules() {
    rules.clear();
  }

  std::vector<BYTE> const &get_expanded_properties() const {
    return props;
  }
  void push_expanded_property(BYTE prop) {
    props.push_back(prop);
  }
  void clear_expanded_properties() {
    props.clear();
  }

  void clear_mem_delta_roots() {
    mem_deltas.clear();
  }
  std::vector<MemDeltaRoot *> const &get_mem_delta_roots() const {
    return mem_deltas;
  }
  void set_mem_delta_root(int idx, MemDeltaRoot *root) {
    mem_deltas[idx] = root;
  }
  void resize_mem_delta_roots(int size) {
    mem_deltas.resize(size);
  }
  void push_mem_delta_root(MemDeltaRoot *root) {
    mem_deltas.push_back(root);
  }

private:
  /* 1. 遷移先計算中
   *    通常: struct LmnMembrane
   *    差分: 空
   * 2. 遷移先計算後 (mc_gen_successor@mc.c以降)
   * 　　通常: struct LmnMembraneへの参照を設定したstruct State
   *    差分: 初期化設定のみを行ったstruct State
   */
  std::vector<void *> roots;   

  /* 最適化のモードを記録 */
  std::bitset<ModelCheckerOptimazeModeSize> opt_mode;
  /* 多重辺除去用 */
  /* key: 展開中のsuccessor */
  /* value: successorに対応するTransition *か、keyと同じ値*/
  std::unordered_map<State *, void *> succ_tbl;

  std::vector<LmnRule *> rules;

  std::vector<BYTE> props;

  /* BODY命令の適用を終えたMemDeltaRootオブジェクトを置く */
  std::vector<MemDeltaRoot *> mem_deltas;
};

void mc_react_cxt_add_expanded(MCReactContext *cxt, LmnMembraneRef mem,
                               LmnRuleRef rule);
void mc_react_cxt_add_mem_delta(MCReactContext *cxt, struct MemDeltaRoot *d,
                                LmnRuleRef rule);

unsigned int mc_react_cxt_succ_num_org(LmnReactCxtRef cxt);

unsigned int mc_react_cxt_expanded_num(MCReactContext *cxt);

#endif
