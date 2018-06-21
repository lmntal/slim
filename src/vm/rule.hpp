/*
 * rule.hpp - types and functions about rule, rule set, module
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
 * $Id: rule.hpp,v 1.6 2008/09/29 05:23:40 taisuke Exp $
 */
#ifndef LMN_RULE_HPP
#define LMN_RULE_HPP

#include "lmntal.h"

#include <vector>

#include "membrane.h"
#include "react_context.h"

typedef BOOL (*LmnTranslated)(LmnReactCxtRef, LmnMembraneRef, LmnRuleRef);

struct LmnRule {
  BYTE *inst_seq;
  int inst_seq_len;
  LmnTranslated translated;
  lmn_interned_str name;
  BOOL is_invisible;
  st_table_t history_tbl;
  lmn_interned_str pre_id;

  /* コストを動的に変えたい場合, このcostに一時的に値を入れておく or
   * costの計算式を入れる */
  LmnCost cost;

  /* 実行時のルールの表現。ルールの処理は中間語命令列を変換したバイナリ表
     現をinst_seqに持つか、関数をtranslatedに持つ。関数は,トランスレータ
     により、ルールを変換して生成された関数を想定している。*/
  LmnRule(LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated,
          lmn_interned_str name)
      : inst_seq(inst_seq), inst_seq_len(inst_seq_len), translated(translated),
        name(name), is_invisible(FALSE), pre_id(ANONYMOUS), history_tbl(NULL) {}

  /* 関数によるルールの処理の表現。トランスレータにより、ルールを変換して
     生成された関数を想定している。戻り値は適用に成功した場合TRUE,失敗し
     た場合FALSEを返す */
  LmnRule(LmnTranslated translated, lmn_interned_str name)
      : LmnRule(NULL, 0, translated, name) {}

  LmnRule(const LmnRule &rule)
      : is_invisible(false), pre_id(ANONYMOUS), history_tbl(NULL) {
    if (rule.inst_seq) {
      inst_seq = LMN_NALLOC(BYTE, rule.inst_seq_len);
      inst_seq = (BYTE *)memcpy(inst_seq, rule.inst_seq, rule.inst_seq_len);
    } else {
      inst_seq = NULL;
    }

    inst_seq_len = rule.inst_seq_len;
    translated = rule.translated;
    name = rule.name;
    if (rule.history_tbl) {
      this->history_tbl = st_copy(rule.history_tbl);
      this->pre_id = rule.pre_id;
    }
  }

  LmnRule() : name(lmn_intern("")) {}

  ~LmnRule() {
    delete (this->inst_seq);
    if (this->history_tbl) {
      st_free_table(this->history_tbl);
    }
  }

  void init_uniq_table() { history_tbl = st_init_numtable(); }
};

enum AtomicType {
  ATOMIC_NONE = 0,
  ATOMIC_ALL_EXHAUSTIVE,
  ATOMIC_SIMULATION,
  ATOMIC_SYNC_STEP,
};

/* structure of RuleSet */
class LmnRuleSet {
  bool is_copied;
  bool has_uniqrule;
  bool is_0step;
  std::vector<LmnRule *> rules;

  bool ruleset_cp_is_need_object() const {
    return has_unique() || (atomic != ATOMIC_NONE);
  }

public:
  LmnRulesetId id;
  AtomicType
      atomic; /* 本ルールセットの適用をatomicに実行するか否かを示すフラグ */
  bool is_atomic_valid; /* atomic step中であることを主張するフラグ */

  LmnRuleSet(LmnRulesetId id, int init_size)
      : id(id), atomic(ATOMIC_NONE), is_atomic_valid(false), is_copied(false),
        has_uniqrule(false), is_0step(false) {}

  ~LmnRuleSet() {
    for (auto r : rules)
      delete r;
  }

  size_t size() const { return rules.size(); }

  /* Adds rule into ruleset */
  void put(LmnRule *rule) {
    rules.push_back(rule);

    /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
    if (!has_unique() && rule->history_tbl) {
      has_uniqrule = true;
    }
  }

  void put(std::unique_ptr<LmnRule> rule) {
    put(rule.get());
  	rule.release();
  }

  LmnRuleSet *duplicate_object() {
    auto result = new LmnRuleSet(id, rules.capacity());
    result->is_copied = true;
    result->atomic = atomic;
    result->is_atomic_valid = is_atomic_valid;

    /* ルール単位のオブジェクト複製 */
    for (auto r : rules)
      result->put(new LmnRule(*r));
    return result;
  }

  /* ルールセットsrcを複製して返す.
   * 基本的にはオリジナルのobjectへの参照を返すのみ.
   * ただし, uniqルールセットである場合(現在はuniq使用時のみ),
   * ルールセットオブジェクトを独立に扱うため新たにmallocして複製したオブジェクトを返す.
   * uniqルールの場合, ルール単位でobjectを複製する */
  LmnRuleSet *duplicate() {
    return (ruleset_cp_is_need_object()) ? duplicate_object() : this;
  }

  /* 2つのrulesetが同じruleを持つか判定する.
   * (ruleの順序はソースコード依存) */
  bool operator==(const LmnRuleSet &set2) const {
    /* rulesetの種類をチェック */
    if (id != set2.id)
      return false;

    bool t1 = has_uniqrule;
    bool t2 = set2.has_uniqrule;

    /* 互いにuniq rulsetでなければruleset idの比較でok */
    if (!t1 && !t2)
      return true;

    /* uniq ruleset同士ではなければ当然FALSE */
    if (t1 ^ t2)
      return false;

    /* uniq ruleset同士の場合:
     *   ruleの適用ヒストリまで比較 */
    if (rules.size() != set2.rules.size())
      return false;

    for (int i = 0; i < rules.size(); i++) {
      LmnRule *rule1 = rules[i];
      LmnRule *rule2 = set2.rules[i];
      st_table_t hist1 = rule1->history_tbl;
      st_table_t hist2 = rule2->history_tbl;

      if (!hist1 && !hist2)
        continue;

      if (!hist1 || !hist2)
        return false;
      if (!st_equals(hist1, hist2))
        return false;
    }

    return true;
  }

  bool operator!=(const LmnRuleSet &set) const { return !(*this == set); }

  unsigned long space() const {
    if (!has_unique() && !is_copy())
      return 0;

    unsigned long ret = 0;
    ret += sizeof(struct LmnRuleSet);
    ret += sizeof(struct LmnRule *) * rules.size();
    for (auto r : rules)
      if (r->history_tbl)
        st_table_space(r->history_tbl);
    return ret;
  }

  void validate_atomic() { is_atomic_valid = true; }
  void invalidate_atomic() { is_atomic_valid = false; }
  void validate_zerostep() { is_0step = true; }
  bool is_zerostep() const { return is_0step; }

  bool is_atomic() const { return is_atomic_valid; }
  bool is_copy() const { return is_copied; }
  bool has_unique() const { return has_uniqrule; }

  /* Returns the ith rule in ruleset */
  LmnRule *get_rule(int i) const { return rules[i]; }

  std::vector<LmnRule *>::const_iterator begin() const { return rules.begin(); }
  std::vector<LmnRule *>::const_iterator end() const { return rules.end(); }
};

/* table, mapping RuleSet ID to RuleSet */
class LmnRuleSetTable {
  std::vector<LmnRuleSet *> entry;
  int next_id;

  static LmnRuleSetTable &instance() {
    static LmnRuleSetTable instance_(64);
    return instance_;
  }
  LmnRuleSetTable(unsigned int size) : entry(size) {}

  ~LmnRuleSetTable() {
    for (auto rs : entry) delete(rs);
  }

public:
  /* Returns RuleSet associated with id. If nothing is, returns NULL */
  static LmnRuleSet *at(int id) {
    return instance().entry[id];
  }

  /* Associates id with ruleset */
  static void add(LmnRuleSet *ruleset, int id) {
    auto &tbl = LmnRuleSetTable::instance();
    if (tbl.entry.size() <= id)
      tbl.entry.resize(id + 1);
    tbl.entry[id] = ruleset;
  }

  /* Generates and returns new RuleSet id */
  static int gen_id() {
    return instance().next_id++;
  }
};

#endif