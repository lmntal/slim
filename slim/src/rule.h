/*
 * rule.h - types and functions about rule, rule set, module
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
 * $Id: rule.h,v 1.6 2008/09/29 05:23:40 taisuke Exp $
 */

#ifndef LMN_RULE_H
#define LMN_RULE_H

#include "lmntal.h"
#include "vector.h"
#include "st.h"
#include "symbol.h"

/*----------------------------------------------------------------------
 * Rule
 */
/* 関数によるルールの処理の表現。トランスレータにより、ルールを変換して
   生成された関数を想定している。戻り値は適用に成功した場合TRUE,失敗し
   た場合FALSEを返す */
typedef struct LmnRule *LmnRule;
typedef BOOL (*LmnTranslated)(struct ReactCxt*, LmnMembrane *, LmnRule);

/* 実行時のルールの表現。ルールの処理は中間語命令列を変換したバイナリ表
   現をinst_seqに持つか、関数をtranslatedに持つ。関数は,トランスレータ
   により、ルールを変換して生成された関数を想定している。*/
struct LmnRule {
  BYTE *inst_seq;
  int inst_seq_len;
  LmnTranslated translated;
  lmn_interned_str name;
  BOOL is_invisible;
  st_table_t history_tbl;
  lmn_interned_str pre_id;
};

LmnRule lmn_rule_make(LmnRuleInstr instr, int instr_len, lmn_interned_str name);
LmnRule dummy_rule(void);
LmnRule lmn_rule_make_translated(LmnTranslated translated, lmn_interned_str name);
LmnRule lmn_rule_copy(LmnRule rule);
void lmn_rule_free(LmnRule rule);

inline static st_table_t lmn_rule_get_history_tbl(LmnRule rule) {
  return rule->history_tbl;
}

inline static lmn_interned_str lmn_rule_get_pre_id(LmnRule rule) {
  return rule->pre_id;
}

inline static void lmn_rule_set_pre_id(LmnRule rule, lmn_interned_str t) {
  rule->pre_id = t;
}

/* ルールの処理を行う関数を返す。ルールが関数を持たなければNULLを返す */
inline static LmnTranslated lmn_rule_get_translated(LmnRule rule) {
  return rule->translated;
}

/* ルールの処理を行う中間語命令列を変換したバイト列を返す。ルールが列を
   持たなければNULLを返す。*/
inline static BYTE *lmn_rule_get_inst_seq(LmnRule rule) {
  return rule->inst_seq;
}

/* ルールの名前を返す */
inline static lmn_interned_str lmn_rule_get_name(LmnRule rule) {
  return rule->name;
}

/* ルール名のセット */
inline static void lmn_rule_set_name(LmnRule rule, lmn_interned_str rule_name) {
  rule->name = rule_name;
}

inline static BOOL lmn_rule_is_invisible(LmnRule rule) {
  return rule->is_invisible == TRUE;
}

inline static void lmn_rule_init_uniq_rule(LmnRule rule) {
  rule->history_tbl = st_init_numtable();
}


/*----------------------------------------------------------------------
 * Rule Set
 */

typedef struct LmnRuleSet *LmnRuleSet;
typedef enum AtomicType{
  ATOMIC_NONE = 0,
  ATOMIC_ALL_EXHAUSTIVE,
  ATOMIC_EACH_SYNC,
} AtomicType;

/* structure of RuleSet */
struct LmnRuleSet {
  LmnRule *rules;         /* ルールのリスト */
  int num, cap;           /* # of rules, and # of capacity */
  LmnRulesetId id;        /* RuleSet ID */
  AtomicType atomic;      /* 本ルールセットの適用をatomicに実行するか否かを示すフラグ */
  BOOL is_atomic_valid;   /* atomic step中であることを主張するフラグ */
  BOOL is_copy;
  BOOL has_uniqrule;
};

/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSet *entry;
};

extern struct LmnRuleSetTable *ruleset_table;

int lmn_gen_ruleset_id(void);
LmnRuleSet lmn_ruleset_make(LmnRulesetId id, int init_size);
void lmn_ruleset_free(LmnRuleSet ruleset);
void lmn_ruleset_put(LmnRuleSet ruleset, LmnRule rule);
inline void lmn_set_ruleset(LmnRuleSet ruleset, int id);
inline long lmn_ruleset_history_num(LmnRuleSet ruleset);
LmnRuleSet lmn_ruleset_copy(LmnRuleSet ruleset);
void lmn_ruleset_copied_free(LmnRuleSet rs);
BOOL lmn_ruleset_equals(LmnRuleSet set1, LmnRuleSet set2);
BOOL lmn_rulesets_contains(Vector *rulesets, LmnRuleSet set1);
BOOL lmn_rulesets_equals(Vector *rulesets1, Vector *rulesets2);
unsigned long lmn_ruleset_space(LmnRuleSet rs);

inline static void lmn_ruleset_validate_atomic(LmnRuleSet rs) {
  rs->is_atomic_valid = TRUE;
}

inline static void lmn_ruleset_invalidate_atomic(LmnRuleSet rs) {
  rs->is_atomic_valid = FALSE;
}

inline static BOOL lmn_ruleset_is_valid_atomic(LmnRuleSet rs) {
  return rs->is_atomic_valid;
}

/* Returns the # of rules in ruleset */
inline static unsigned int lmn_ruleset_rule_num(LmnRuleSet ruleset) {
  return ruleset->num;
}

/* Returns the ith rule in ruleset */
inline static LmnRule lmn_ruleset_get_rule(LmnRuleSet ruleset, int i) {
  return ruleset->rules[i];
}

/* Returns id of ruleset */
inline static int lmn_ruleset_get_id(LmnRuleSet ruleset) {
  return ruleset->id;
}

inline static AtomicType lmn_ruleset_atomic_type(LmnRuleSet ruleset) {
  return ruleset->atomic;
}

inline static void lmn_ruleset_set_atomic(LmnRuleSet ruleset, AtomicType t) {
  ruleset->atomic = t;
}

/* Returns RuleSet associated with id. If nothing is, returns NULL */
inline static LmnRuleSet lmn_ruleset_from_id(int id) {
  if (ruleset_table->size <= (unsigned int)id) return NULL;
  else return ruleset_table->entry[id];
}

inline static BOOL lmn_ruleset_is_copy(LmnRuleSet ruleset) {
  return ruleset->is_copy;
}

inline static BOOL lmn_ruleset_has_uniqrule(LmnRuleSet ruleset) {
  return ruleset->has_uniqrule;
}

inline static LmnRule *lmn_ruleset_get_rules(LmnRuleSet ruleset) {
  return ruleset->rules;
}



/*----------------------------------------------------------------------
 * System Rule Set
 */

extern LmnRuleSet system_ruleset;
void lmn_add_system_rule(LmnRule rule);

/*----------------------------------------------------------------------
 * Initial Rule Set
 */

extern LmnRuleSet initial_ruleset;
extern LmnRuleSet initial_system_ruleset;
void lmn_add_initial_rule(LmnRule rule);
void lmn_add_initial_system_rule(LmnRule rule);

/*----------------------------------------------------------------------
 * Module
 */

LMN_EXTERN void lmn_set_module(lmn_interned_str module_name, LmnRuleSet ruleset);
LMN_EXTERN LmnRuleSet lmn_get_module_ruleset(lmn_interned_str module_name);

#endif /* LMN_RULE_H */
