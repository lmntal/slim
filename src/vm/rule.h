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

/* cldoc:begin-category(Lmntal::Rule) */

typedef struct LmnRule *LmnRuleRef;

typedef struct LmnRuleSet *LmnRuleSetRef;

#include "lmntal.h"
#include "element/vector.h"
#include "element/st.h"
#include "symbol.h"
#include "membrane.h"
#include "react_context.h"

typedef BOOL (*LmnTranslated)(LmnReactCxtRef, LmnMembraneRef, LmnRuleRef);


/*----------------------------------------------------------------------
 * Rule
 */
/* 関数によるルールの処理の表現。トランスレータにより、ルールを変換して
   生成された関数を想定している。戻り値は適用に成功した場合TRUE,失敗し
   た場合FALSEを返す */


LmnRuleRef lmn_rule_make(LmnRuleInstr instr, int instr_len, lmn_interned_str name);
LmnRuleRef dummy_rule(void);
LmnRuleRef lmn_rule_make_translated(LmnTranslated translated, lmn_interned_str name);
LmnRuleRef lmn_rule_copy(LmnRuleRef rule);
void lmn_rule_free(LmnRuleRef rule);

st_table_t lmn_rule_get_history_tbl(LmnRuleRef rule);

lmn_interned_str lmn_rule_get_pre_id(LmnRuleRef rule);
void lmn_rule_set_pre_id(LmnRuleRef rule, lmn_interned_str t);

/* ルールの処理を行う関数を返す。ルールが関数を持たなければNULLを返す */
LmnTranslated lmn_rule_get_translated(LmnRuleRef rule);

/* ルールの処理を行う中間語命令列を変換したバイト列を返す。ルールが列を
   持たなければNULLを返す。*/
BYTE *lmn_rule_get_inst_seq(LmnRuleRef rule);
/* ルールの名前を返す */
lmn_interned_str lmn_rule_get_name(LmnRuleRef rule);

/* ルール名のセット */
void lmn_rule_set_name(LmnRuleRef rule, lmn_interned_str rule_name);

LmnCost lmn_rule_get_cost(LmnRuleRef rule);

void lmn_rule_set_cost(LmnRuleRef rule, LmnCost rule_cost);

BOOL lmn_rule_is_invisible(LmnRuleRef rule);

void lmn_rule_init_uniq_rule(LmnRuleRef rule);


/*----------------------------------------------------------------------
 * Rule Set
 */

typedef enum AtomicType{
  ATOMIC_NONE = 0,
  ATOMIC_ALL_EXHAUSTIVE,
  ATOMIC_SIMULATION,
  ATOMIC_SYNC_STEP,
} AtomicType;



/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSetRef *entry;
};

extern struct LmnRuleSetTable *ruleset_table;

int lmn_gen_ruleset_id(void);
LmnRuleSetRef lmn_ruleset_make(LmnRulesetId id, int init_size);
void lmn_ruleset_free(LmnRuleSetRef ruleset);
void lmn_ruleset_put(LmnRuleSetRef ruleset, LmnRuleRef rule);
void lmn_set_ruleset(LmnRuleSetRef ruleset, int id);
long lmn_ruleset_history_num(LmnRuleSetRef ruleset);
LmnRuleSetRef lmn_ruleset_copy(LmnRuleSetRef ruleset);
void lmn_ruleset_copied_free(LmnRuleSetRef rs);
BOOL lmn_ruleset_equals(LmnRuleSetRef set1, LmnRuleSetRef set2);
BOOL lmn_rulesets_contains(Vector *rulesets, LmnRuleSetRef set1);
BOOL lmn_rulesets_equals(Vector *rulesets1, Vector *rulesets2);
unsigned long lmn_ruleset_space(LmnRuleSetRef rs);

void lmn_ruleset_validate_atomic(LmnRuleSetRef rs);

void lmn_ruleset_invalidate_atomic(LmnRuleSetRef rs);

BOOL lmn_ruleset_is_valid_atomic(LmnRuleSetRef rs);

/* Returns the # of rules in ruleset */
unsigned int lmn_ruleset_rule_num(LmnRuleSetRef ruleset);

/* Returns the ith rule in ruleset */
LmnRuleRef lmn_ruleset_get_rule(LmnRuleSetRef ruleset, int i);

/* Returns id of ruleset */
int lmn_ruleset_get_id(LmnRuleSetRef ruleset);

AtomicType lmn_ruleset_atomic_type(LmnRuleSetRef ruleset);

void lmn_ruleset_set_atomic(LmnRuleSetRef ruleset, AtomicType t);

/* Returns RuleSet associated with id. If nothing is, returns NULL */
LmnRuleSetRef lmn_ruleset_from_id(int id);

BOOL lmn_ruleset_is_copy(LmnRuleSetRef ruleset);

BOOL lmn_ruleset_has_uniqrule(LmnRuleSetRef ruleset);

LmnRuleRef *lmn_ruleset_get_rules(LmnRuleSetRef ruleset);



/*----------------------------------------------------------------------
 * System Rule Set
 */

extern LmnRuleSetRef system_ruleset;
void lmn_add_system_rule(LmnRuleRef rule);

/*----------------------------------------------------------------------
 * Initial Rule Set
 */

extern LmnRuleSetRef initial_ruleset;
extern LmnRuleSetRef initial_system_ruleset;
void lmn_add_initial_rule(LmnRuleRef rule);
void lmn_add_initial_system_rule(LmnRuleRef rule);

/*----------------------------------------------------------------------
 * Module
 */

LMN_EXTERN void lmn_set_module(lmn_interned_str module_name, LmnRuleSetRef ruleset);
LMN_EXTERN LmnRuleSetRef lmn_get_module_ruleset(lmn_interned_str module_name);

/* cldoc:end-category() */

#endif /* LMN_RULE_H */
