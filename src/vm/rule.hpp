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
#include <set>

#include "membrane.h"
#include "react_context.hpp"

#include <random>

using LmnTranslated = std::function<BOOL(LmnReactCxtRef, LmnMembraneRef, LmnRuleRef)>;

/* 実行時のルールの表現。ルールの処理は中間語命令列を変換したバイナリ表
   現をinst_seqに持つか、関数をtranslatedに持つ。関数は,トランスレータ
   により、ルールを変換して生成された関数を想定している。*/
class LmnRule {
  std::set<lmn_interned_str> history_tbl;
  bool is_unique_;
  bool is_subrule_;
  lmn_interned_str latest_history_;
public:
  BYTE *inst_seq;
  int inst_seq_len;
  LmnTranslated translated;
  lmn_interned_str name;
  int rule_number = -1; // 履歴管理用アトムのための変数

  /* コストを動的に変えたい場合, このcostに一時的に値を入れておく or
   * costの計算式を入れる */
  LmnCost cost;

private:
  LmnRule(LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated,
          lmn_interned_str name, bool is_unique_, bool is_subrule = false)
      : inst_seq(inst_seq), inst_seq_len(inst_seq_len), translated(translated),
        name(name), latest_history_(ANONYMOUS), is_unique_(is_unique_), is_subrule_(is_subrule) {}

public:
  /* 関数によるルールの処理の表現。トランスレータにより、ルールを変換して
     生成された関数を想定している。戻り値は適用に成功した場合TRUE,失敗し
     た場合FALSEを返す */
  LmnRule(LmnTranslated translated, lmn_interned_str name)
      : LmnRule(nullptr, 0, translated, name, false) {}

  LmnRule(LmnRuleInstr inst_seq, int inst_seq_len, lmn_interned_str name, bool is_unique = false, bool is_subrule = false)
      : LmnRule(inst_seq, inst_seq_len, nullptr, name, is_unique, is_subrule) {}

  LmnRule(const LmnRule &rule)
      : latest_history_(ANONYMOUS), is_unique_(false) {
    if (rule.inst_seq) {
      inst_seq = LMN_NALLOC<BYTE>(rule.inst_seq_len);
      inst_seq = (BYTE *)memcpy(inst_seq, rule.inst_seq, rule.inst_seq_len);
    } else {
      inst_seq = nullptr;
    }

    inst_seq_len = rule.inst_seq_len;
    translated = rule.translated;
    name = rule.name;
    if (rule.is_unique()) {
      history_tbl = rule.history_tbl;
      latest_history_ = rule.latest_history_;
      is_unique_ = true;
    }
    is_subrule_ = rule.is_subrule();
  }

  LmnRule() : name(lmn_intern("")) {}

  ~LmnRule() {
    // delete (this->inst_seq);
    lmn_free(this->inst_seq);
  }

  void init_uniq_table() { is_unique_ = true; }

  lmn_interned_str latest_history() const {
    return is_unique() ? latest_history_ : ANONYMOUS;
  }

  void add_history(lmn_interned_str history) {
    history_tbl.insert(history);
    latest_history_ = history;
  }

  void undo_history() {
    if (is_unique() && latest_history_ != 0) {
      history_tbl.erase(latest_history_);
      latest_history_ = ANONYMOUS;
    }
  }

  void delete_history(lmn_interned_str history) {
    history_tbl.erase(history);
  }

  bool has_history(lmn_interned_str history) {
    return history_tbl.find(history) != history_tbl.end();
  }

  bool history_equals(const LmnRule &rule) const {
    auto hist1 = history_tbl;
    auto hist2 = rule.history_tbl;

    if (!is_unique() && !rule.is_unique())
      return true;

    return history_tbl == rule.history_tbl;
  }

  size_t history_table_size() const {
    // NOTE: this value may not be correct.
    return (is_unique()) ? history_tbl.max_size() * sizeof(lmn_interned_str) : 0;
  }

  const std::set<lmn_interned_str> &history() const {
    return history_tbl;
  }

  bool is_unique() const {
    return is_unique_;
  }

  bool is_subrule() const {
    return is_subrule_;
  }
};

/* structure of RuleSet */
class LmnRuleSet {
  bool is_copied;
  bool has_uniqrule;
  bool is_0step;
  std::vector<LmnRule *> rules;
  std::vector<LmnRule *> subrules;

  bool ruleset_cp_is_need_object() const {
    return has_unique();
  }

public:
  LmnRulesetId id;

  LmnRuleSet(LmnRulesetId id, int init_size)
      : id(id), is_copied(false),
        has_uniqrule(false), is_0step(false) {}

  LmnRuleSet(const LmnRuleSet &rs) : LmnRuleSet(rs.id, rs.rules.capacity()) {
    is_copied = true;
    is_0step = rs.is_0step;

    /* ルール単位のオブジェクト複製 */
    for (auto r : rs.rules)
      put(new LmnRule(*r));
    for (auto r : rs.subrules)
      put(new LmnRule(*r));
  }

  ~LmnRuleSet() {
    for (auto r : rules)
      delete r;
    for (auto r : subrules)
      delete r;
  }

  size_t size() const { return rules.size(); }

  /* Adds rule into ruleset */
  void put(LmnRule *rule) {
    if (rule->is_subrule()) {
      subrules.push_back(rule);
    } else {
      rules.push_back(rule);
    }

    /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
    has_uniqrule |= rule->is_unique();
  }

  void put(std::unique_ptr<LmnRule> rule) {
    put(rule.get());
    rule.release();
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

    for (int i = 0; i < rules.size(); i++)
      if (!rules[i]->history_equals(*set2.rules[i]))
        return false;

    return true;
  }

  bool operator!=(const LmnRuleSet &set) const { return !(*this == set); }

  unsigned long space() const {
    if (!has_unique() && !is_copy())
      return 0;

    unsigned long ret = 0;
    ret += sizeof(struct LmnRuleSet);
    ret += sizeof(struct LmnRule *) * (rules.size() + subrules.size());
    for (auto r : rules)
      ret += r->history_table_size();
    return ret;
  }

  void validate_zerostep() { is_0step = true; }
  bool is_zerostep() const { return is_0step; }

  bool is_copy() const { return is_copied; }
  bool has_unique() const { return has_uniqrule; }

  /* Returns the ith rule in ruleset */
  LmnRule *get_rule(int i) const { return rules[i]; }

  /* query subrule in the ruleset. */
  // TODO: more efficient search.
  LmnRule *get_subrule(lmn_interned_str name) {
    for (auto r : subrules) {
      if (r->name == name)
        return r;
    }
    return nullptr;
  }

  std::vector<LmnRule *>::const_iterator begin() const { return rules.begin(); }
  std::vector<LmnRule *>::const_iterator end() const { return rules.end(); }

  //ruleの順序をシャッフルをする関数
  void shuffle(){
    std::random_device rd;
    auto rng = std::default_random_engine{rd()};
    std::shuffle(std::begin(rules),std::end(rules),rng);
  }

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
