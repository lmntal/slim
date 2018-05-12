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

  LmnRule(LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated,
          lmn_interned_str name)
      : inst_seq(inst_seq),
        inst_seq_len(inst_seq_len),
        translated(translated),
        name(name),
        is_invisible(FALSE),
        pre_id(ANONYMOUS),
        history_tbl(NULL) {}

  LmnRule() {}

  ~LmnRule() {
    delete (this->inst_seq);
    if (lmn_rule_get_history_tbl(this)) {
      st_free_table(lmn_rule_get_history_tbl(this));
    }
  }
};

/* structure of RuleSet */
struct LmnRuleSet {
  LmnRule **rules; /* ルールのリスト */
  int num, cap;    /* # of rules, and # of capacity */
  LmnRulesetId id; /* RuleSet ID */
  AtomicType
      atomic; /* 本ルールセットの適用をatomicに実行するか否かを示すフラグ */
  BOOL is_atomic_valid; /* atomic step中であることを主張するフラグ */
  BOOL is_copy;
  BOOL is_0step;
  BOOL has_uniqrule;
  LmnRuleSet(LmnRulesetId id, int init_size)
      : id(id),
        cap(init_size),
        rules(LMN_CALLOC(LmnRule *, init_size)),
        num(0),
        atomic(ATOMIC_NONE),
        is_atomic_valid(FALSE),
        is_copy(FALSE),
        has_uniqrule(FALSE),
        is_0step(FALSE) {}

  ~LmnRuleSet() {
    for (int i = 0; i < this->num; i++) delete this->rules[i];
    LMN_FREE(this->rules);
  }

  /* Adds rule into ruleset */
  void put(LmnRule *rule) {
    if (this->num == this->cap) {
      this->cap = (this->cap * 2);
      this->rules = LMN_REALLOC(LmnRule *, this->rules, this->cap);
    }
    this->rules[this->num++] = rule;

    /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
    if (!lmn_ruleset_has_uniqrule(this) && lmn_rule_get_history_tbl(rule)) {
      this->has_uniqrule = TRUE;
    }
  }

  LmnRuleSet *duplicate_object() {
    LmnRuleSet *result;
    unsigned int i, r_n;

    r_n = this->num;
    result = new LmnRuleSet(lmn_ruleset_get_id(this), r_n);
    result->is_copy = TRUE;
    result->atomic = this->atomic;
    result->is_atomic_valid = this->is_atomic_valid;

    /* ルール単位のオブジェクト複製 */
    for (i = 0; i < r_n; i++) {
      LmnRule* r = this->get_rule(i);
      result->put(lmn_rule_copy(r));
    }
    return result;
  }

  bool ruleset_cp_is_need_object() {
    return lmn_ruleset_has_uniqrule(this) ||
           (lmn_ruleset_atomic_type(this) != ATOMIC_NONE);
  }

  /* ルールセットsrcを複製して返す.
   * 基本的にはオリジナルのobjectへの参照を返すのみ.
   * ただし, uniqルールセットである場合(現在はuniq使用時のみ),
   * ルールセットオブジェクトを独立に扱うため新たにmallocして複製したオブジェクトを返す.
   * uniqルールの場合, ルール単位でobjectを複製する */
  LmnRuleSet *duplicate() {
    if (ruleset_cp_is_need_object()) {
      return duplicate_object();
    } else {
      return this;
    }
  }

  /* 2つのrulesetが同じruleを持つか判定する.
   * (ruleの順序はソースコード依存) */
  bool operator==(const LmnRuleSet &set2) {
    /* rulesetの種類をチェック */
    if (this->id != set2.id)
      return false;
    
    bool t1 = this->has_uniqrule;
    bool t2 = set2.has_uniqrule;

    /* 互いにuniq rulsetでなければruleset idの比較でok */
    if (!t1 && !t2)
      return true;

    /* uniq ruleset同士ではなければ当然FALSE */
    if (t1 ^ t2)
      return false;

    /* uniq ruleset同士の場合:
     *   ruleの適用ヒストリまで比較 */
    if (this->num != set2.num)
      return false;
    
    for (int i = 0; i < this->num; i++) {
      LmnRule *rule1 = this->rules[i];
      LmnRule *rule2 = set2.rules[i];
      st_table_t hist1 = lmn_rule_get_history_tbl(rule1);
      st_table_t hist2 = lmn_rule_get_history_tbl(rule2);

      if (!hist1 && !hist2)
        continue;

      if (!hist1 || !hist2)
        return false;
      if (!st_equals(hist1, hist2))
        return false;
    }

    return true;
  }

  bool operator!=(const LmnRuleSet &set) { return !(*this == set); }

  unsigned long space() {
    unsigned long ret = 0;
    if (lmn_ruleset_has_uniqrule(this) || lmn_ruleset_is_copy(this)) {
      unsigned int i, n;

      n = this->num;
      ret += sizeof(struct LmnRuleSet);
      ret += sizeof(struct LmnRule *) * n;
      for (i = 0; i < n; i++) {
        LmnRuleRef r = this->get_rule(i);
        if (lmn_rule_get_history_tbl(r)) { /* 履歴表を持っている場合  */
          st_table_space(lmn_rule_get_history_tbl(r));
        }
      }
    }
    return ret;
  }

  void validate_atomic() {
    this->is_atomic_valid = TRUE;
  }

  void invalidate_atomic() {
    this->is_atomic_valid = FALSE;
  }
  bool is_atomic(){
    return this->is_atomic_valid;
  }

  /* Returns the ith rule in ruleset */
  LmnRuleRef get_rule(int i) {
    return this->rules[i];
  }
};

/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSet **entry;

  LmnRuleSetTable(unsigned int size) : size(size) {
    this->entry = LMN_NALLOC(LmnRuleSetRef, this->size);
  }

  ~LmnRuleSetTable() {
    unsigned int i;
    for (i = 0; i < this->size; i++) {
      if (this->entry[i]) {
        delete(this->entry[i]);
      }
    }
    delete(this->entry);
  }

/* Associates id with ruleset */
void register_ruleset(LmnRuleSet *ruleset, int id) {
  /* 必要ならば容量を拡張 */
  while (this->size <= (unsigned int)id) {
    int old_size = this->size;
    this->size *= 2;
    this->entry =
        LMN_REALLOC(LmnRuleSetRef, this->entry, this->size);
    /* 安全なメモリ解放の為ゼロクリア */
    memset(this->entry + old_size, 0,
           (this->size - old_size) * sizeof(LmnRuleSetRef));
  }

  this->entry[id] = ruleset;
}
};

#endif