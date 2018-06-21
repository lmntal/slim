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

  LmnRule() : name(lmn_intern("")) {}

  ~LmnRule() {
    delete (this->inst_seq);
    if (this->history_tbl) {
      st_free_table(this->history_tbl);
    }
  }

  void init_uniq_table() { history_tbl = st_init_numtable(); }
};

/* structure of RuleSet */
struct LmnRuleSet {
private:
  BOOL is_copied;
  BOOL has_uniqrule;
  BOOL is_0step;

public:
  LmnRule **rules; /* ルールのリスト */
  int num, cap;    /* # of rules, and # of capacity */
  LmnRulesetId id; /* RuleSet ID */
  AtomicType
      atomic; /* 本ルールセットの適用をatomicに実行するか否かを示すフラグ */
  BOOL is_atomic_valid; /* atomic step中であることを主張するフラグ */
  LmnRuleSet(LmnRulesetId id, int init_size)
      : id(id), cap(init_size), rules(LMN_CALLOC(LmnRule *, init_size)), num(0),
        atomic(ATOMIC_NONE), is_atomic_valid(FALSE), is_copied(FALSE),
        has_uniqrule(FALSE), is_0step(FALSE) {}

  ~LmnRuleSet() {
    for (int i = 0; i < this->num; i++)
      delete this->rules[i];
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
    if (!this->has_unique() && rule->history_tbl) {
      this->has_uniqrule = TRUE;
    }
  }

  LmnRuleSet *duplicate_object() {
    LmnRuleSet *result;
    unsigned int i, r_n;

    r_n = this->num;
    result = new LmnRuleSet(this->id, r_n);
    result->is_copied = TRUE;
    result->atomic = this->atomic;
    result->is_atomic_valid = this->is_atomic_valid;

    /* ルール単位のオブジェクト複製 */
    for (i = 0; i < r_n; i++) {
      LmnRule *r = this->get_rule(i);
      result->put(lmn_rule_copy(r));
    }
    return result;
  }

  bool ruleset_cp_is_need_object() {
    return this->has_unique() || (this->atomic != ATOMIC_NONE);
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

  bool operator!=(const LmnRuleSet &set) { return !(*this == set); }

  unsigned long space() {
    unsigned long ret = 0;
    if (this->has_unique() || is_copy()) {
      unsigned int i, n;

      n = this->num;
      ret += sizeof(struct LmnRuleSet);
      ret += sizeof(struct LmnRule *) * n;
      for (i = 0; i < n; i++) {
        LmnRule *r = this->get_rule(i);
        if (r->history_tbl) { /* 履歴表を持っている場合  */
          st_table_space(r->history_tbl);
        }
      }
    }
    return ret;
  }

  void validate_atomic() { this->is_atomic_valid = TRUE; }
  void invalidate_atomic() { this->is_atomic_valid = FALSE; }
  void validate_zerostep() { this->is_0step = TRUE; }
  bool is_zerostep() { return this->is_0step; }

  bool is_atomic() { return this->is_atomic_valid; }
  bool is_copy() { return this->is_copied; }
  bool has_unique() { return this->has_uniqrule; }

  /* Returns the ith rule in ruleset */
  LmnRule *get_rule(int i) { return this->rules[i]; }
};

/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSet **entry;

  LmnRuleSetTable(unsigned int size) : size(size) {
    this->entry = LMN_NALLOC(LmnRuleSet *, this->size);
  }

  ~LmnRuleSetTable() {
    for (unsigned int i = 0; i < this->size; i++)
      if (this->entry[i])
        delete (this->entry[i]);
    delete (this->entry);
  }

  /* Associates id with ruleset */
  void register_ruleset(LmnRuleSet *ruleset, int id) {
    /* 必要ならば容量を拡張 */
    while (this->size <= (unsigned int)id) {
      int old_size = this->size;
      this->size *= 2;
      this->entry =
          LMN_REALLOC(LmnRuleSet *, this->entry, this->size);
      /* 安全なメモリ解放の為ゼロクリア */
      memset(this->entry + old_size, 0,
             (this->size - old_size) * sizeof(LmnRuleSet *));
    }

    this->entry[id] = ruleset;
  }

  /* Returns RuleSet associated with id. If nothing is, returns NULL */
  LmnRuleSet * get(int id) {
    if (ruleset_table->size <= (unsigned int)id)
      return NULL;
    else
      return ruleset_table->entry[id];
  }
};

#endif