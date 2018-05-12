/*
 * rule.hpp - types and functions about rule, rule set, module
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: rule.hpp,v 1.6 2008/09/29 05:23:40 taisuke Exp $
 */
#ifndef LMN_RULE_HPP
#define LMN_RULE_HPP

struct LmnRule {
  BYTE             *inst_seq;
  int              inst_seq_len;
  LmnTranslated    translated;
  lmn_interned_str name;
  BOOL             is_invisible;
  st_table_t       history_tbl;
  lmn_interned_str pre_id;

  /* コストを動的に変えたい場合, このcostに一時的に値を入れておく or costの計算式を入れる */
  LmnCost          cost;

  LmnRule (LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated, lmn_interned_str name):
    inst_seq(inst_seq), inst_seq_len(inst_seq_len), translated(translated), name(name), is_invisible(FALSE), pre_id(ANONYMOUS), history_tbl(NULL) {}

  LmnRule () {}

  ~LmnRule () {
    delete(this->inst_seq);
    if (lmn_rule_get_history_tbl(this)) {
      st_free_table(lmn_rule_get_history_tbl(this));
    }
  }
};

/* structure of RuleSet */
struct LmnRuleSet {
  LmnRuleRef *rules; /* ルールのリスト */
  int num, cap;      /* # of rules, and # of capacity */
  LmnRulesetId id;   /* RuleSet ID */
  AtomicType
      atomic; /* 本ルールセットの適用をatomicに実行するか否かを示すフラグ */
  BOOL is_atomic_valid; /* atomic step中であることを主張するフラグ */
  BOOL is_copy;
  BOOL is_0step;
  BOOL has_uniqrule;
  LmnRuleSet(LmnRulesetId id,int init_size):
  	id(id), cap(init_size), rules(LMN_CALLOC(LmnRuleRef, init_size)), num(0), atomic(ATOMIC_NONE), is_atomic_valid(FALSE), is_copy(FALSE), has_uniqrule(FALSE), is_0step(FALSE) {}

  /* Adds rule into ruleset */
  void put(LmnRuleRef rule) {
   if (this->num == this->cap) {
     this->cap = (this->cap * 2);
     this->rules = LMN_REALLOC(LmnRuleRef, this->rules, this->cap);
   }
   this->rules[this->num++] = rule;

   /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
   if (!lmn_ruleset_has_uniqrule(this) && lmn_rule_get_history_tbl(rule)) {
     this->has_uniqrule = TRUE;
   }
  }

};

/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSetRef *entry;

  LmnRuleSetTable (unsigned int size):
    size(size) {
      this->entry = LMN_NALLOC(LmnRuleSetRef, this->size);
    }
  
  ~LmnRuleSetTable () {
    unsigned int i;
    for (i = 0; i < this->size; i++) {
      if (this->entry[i]) {
        delete(this->entry[i]);
      }
    }
    delete(this->entry);
  }
};

#endif