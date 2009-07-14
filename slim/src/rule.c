/*
 * rule.c
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
 * $Id: rule.c,v 1.7 2008/10/16 18:12:55 sasaki Exp $
 */

#include "lmntal.h"
#include "rule.h"
#include "system_ruleset.h"
#include "st.h"

/*----------------------------------------------------------------------
 * Rule
 */

/* 実行時のルールの表現。ルールの処理は中間語命令列を変換したバイナリ表
   現をinst_seqに持つか、関数をtranslatedに持つ。関数は,トランスレータ
   により、ルールを変換して生成された関数を想定している。*/
struct LmnRule {
  BYTE *inst_seq;
  int inst_seq_len;
  LmnTranslated translated;
  lmn_interned_str name;
  BOOL is_invisible;
  st_table *history_tbl;
};

/* prototypes */
LmnRule make_rule(LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated, lmn_interned_str name);
void init_rules(void);
void destroy_rules(void);

/* create new rule */
LmnRule make_rule(LmnRuleInstr inst_seq, int inst_seq_len, LmnTranslated translated, lmn_interned_str name)
{
  LmnRule rule = LMN_MALLOC(struct LmnRule);

  rule->inst_seq = inst_seq;
  rule->inst_seq_len = inst_seq_len;  /* inst_seqの長さ */
  rule->translated = translated;
  rule->name = name;                  /* ルール名 */
  rule->is_invisible = FALSE; /* ルールの可視性を決定するコンパイラ部分の実装が完成するまでは，すべてのルールをvisibleに固定しておく */
  rule->history_tbl = st_init_strtable();
  return rule;
}

/* create new rule with byte sequence of instructions.
   inst_seq_sizeはinst_seqの長さ(バイト単位)を表す */
LmnRule lmn_rule_make(BYTE *inst_seq, int inst_seq_len, lmn_interned_str name)
{
  return make_rule(inst_seq, inst_seq_len, NULL, name);
}

/* 中身のない、名前だけを持つルールを生成する */
LmnRule lmn_rule_make_dummy(lmn_interned_str name)
{
  return make_rule(NULL, -1, NULL, name);
}

/* create new rule with a translated function */
LmnRule lmn_rule_make_translated(LmnTranslated translated, lmn_interned_str name)
{
  return make_rule(NULL, 0, translated, name);
}

/* ruleをコピーして新しいルールを作成する */
LmnRule lmn_rule_copy(LmnRule rule)
{
  BYTE *inst_seq;

  inst_seq = LMN_NALLOC(BYTE, rule->inst_seq_len);
  inst_seq = memcpy(inst_seq, rule->inst_seq, rule->inst_seq_len);
  LmnRule new_rule = make_rule(inst_seq, rule->inst_seq_len, rule->translated, rule->name);
  new_rule->history_tbl = st_copy(rule->history_tbl);
  return new_rule;
}

/* ruleとruleの要素を解放する */
void lmn_rule_free(LmnRule rule)
{
  LMN_FREE(rule->inst_seq);
  LMN_FREE(rule);
}

/* ルールの処理を行う関数を返す。ルールが関数を持たなければNULLを返す */
LmnTranslated lmn_rule_get_translated(LmnRule rule)
{
  return rule->translated;
}

/* ルールの処理を行う中間語命令列を変換したバイト列を返す。ルールが列を
   持たなければNULLを返す。*/
BYTE *lmn_rule_get_inst_seq(LmnRule rule)
{
  return rule->inst_seq;
}

/* ルールの名前を返す */
lmn_interned_str lmn_rule_get_name(LmnRule rule)
{
  return rule->name;
}

/* ルール名のセット */
void lmn_rule_set_name(LmnRule rule, lmn_interned_str rule_name)
{
  rule->name = rule_name;
}

BOOL lmn_rule_is_invisible(LmnRule rule) {
  return rule->is_invisible == TRUE;
}

LmnRule dummy_rule(void)
{
  static struct LmnRule rule;
  static BOOL first = TRUE;

  if (first) {
    first = FALSE;
    rule.name = lmn_intern("");
  }

  return &rule;
}

/* uniq履歴照合(TURE:含まれる、FALSE:含まれない) */
BOOL lmn_rule_his_check(LmnRule rule, char *id){
  if(st_is_member(rule->history_tbl, (st_data_t)id))return TRUE;
  return FALSE;
}

struct st_table *lmn_rule_get_history(LmnRule rule){
  return rule->history_tbl;
}

/*----------------------------------------------------------------------
 * Rule Set
 */

/* structure of RuleSet */
struct LmnRuleSet {
  LmnRulesetId id;      /* RuleSet ID */
  int num, cap;         /* # of rules, and # of capacity */
  LmnRule *rules;       /* ルールのリスト */
  /* 非決定実行時にルールセットをatomicに実行するかのフラグ */
  int atomic;
  BOOL valid;
};

/* Generates and returns new RuleSet id */
int lmn_gen_ruleset_id()
{
  static int ruleset_next_id = 1;

  return ruleset_next_id++;
}

/* Generates a new RuleSet */
LmnRuleSet lmn_ruleset_make(LmnRulesetId id, int init_size)
{
  LmnRuleSet ruleset = LMN_MALLOC(struct LmnRuleSet);

  ruleset->id = id;
  ruleset->rules = LMN_CALLOC(LmnRule, init_size);
  ruleset->num = 0;
  ruleset->cap = init_size;
  ruleset->atomic = FALSE;
  ruleset->valid = TRUE;

  return ruleset;
}

/* Frees RuleSet and its elements */
void lmn_ruleset_free(LmnRuleSet ruleset)
{
  int i;
  for (i = 0; i < ruleset->num; i++) lmn_rule_free(ruleset->rules[i]);
  LMN_FREE(ruleset->rules);
  LMN_FREE(ruleset);
}

/* Adds rule into ruleset */
void lmn_ruleset_put(LmnRuleSet ruleset, LmnRule rule)
{
  if (ruleset->num == ruleset->cap) {
    ruleset->cap = (ruleset->cap * 2);
    ruleset->rules = LMN_REALLOC(LmnRule, ruleset->rules, ruleset->cap);
  }
  ruleset->rules[ruleset->num++] = rule;
}

/* Returns the # of rules in ruleset */
unsigned int lmn_ruleset_rule_num(LmnRuleSet ruleset)
{
  return ruleset->num;
}

/* Returns the ith rule in ruleset */
inline LmnRule lmn_ruleset_get_rule(LmnRuleSet ruleset, int i)
{
  return ruleset->rules[i];
}

/* Returns id of ruleset */
inline int lmn_ruleset_get_id(LmnRuleSet ruleset)
{
  return ruleset->id;
}

inline AtomicType lmn_ruleset_atomic_type(LmnRuleSet ruleset)
{
  return ruleset->atomic;
}

inline void lmn_ruleset_set_atomic(LmnRuleSet ruleset, AtomicType t)
{
  ruleset->atomic = t;
}

inline BOOL lmn_ruleset_is_valid(LmnRuleSet ruleset)
{
  return ruleset->valid;
}

inline void lmn_ruleset_set_valid(LmnRuleSet ruleset, BOOL b)
{
  ruleset->valid = b;
}

/* table, mapping RuleSet ID to RuleSet */
struct LmnRuleSetTable {
  unsigned int size;
  LmnRuleSet *entry;
} *ruleset_table;

/* ルールセットテーブルの初期化 */
static void init_ruleset_table()
{
  ruleset_table = LMN_MALLOC(struct LmnRuleSetTable);
  ruleset_table->size = 64;
  ruleset_table->entry = LMN_NALLOC(LmnRuleSet, ruleset_table->size);
  /* 安全なメモリ解放の為ゼロクリア */
  memset(ruleset_table->entry, 0, ruleset_table->size * sizeof(ruleset_table->entry[0]));
}

/* ルールセットテーブルの解放処理 */
static void destroy_ruleset_table()
{
  unsigned int i;

  for (i = 0; i< ruleset_table->size; i++) {
    if (ruleset_table->entry[i]) {
      lmn_ruleset_free(ruleset_table->entry[i]);
    }
  }
  LMN_FREE(ruleset_table->entry);
  LMN_FREE(ruleset_table);
}

/* Associates id with ruleset */
void lmn_set_ruleset(LmnRuleSet ruleset, int id)
{
  /* 必要ならば容量を拡張 */
  while (ruleset_table->size < (unsigned int)id) {
    int old_size = ruleset_table->size;
    ruleset_table->size *= 2;
    ruleset_table->entry = LMN_REALLOC(LmnRuleSet, ruleset_table->entry, ruleset_table->size);
    /* 安全なメモリ解放の為ゼロクリア */
    memset(ruleset_table->entry + old_size, 0, (ruleset_table->size - old_size) * sizeof(LmnRuleSet));
  }

  ruleset_table->entry[id] = ruleset;
}

/* Returns RuleSet associated with id. If nothing is, returns NULL */
LmnRuleSet lmn_ruleset_from_id(int id)
{
  if (ruleset_table->size <= (unsigned int)id) return NULL;
  else return ruleset_table->entry[id];
}

struct st_table *lmn_rule_get_histbl(LmnRule rule)
{
  return rule->history_tbl;
}

/* rulesetをコピーして新しいルールセットを作成する */
LmnRuleSet lmn_ruleset_copy(LmnRuleSet ruleset)
{
  LmnRuleSet new_ruleset = lmn_ruleset_make(ruleset->id, 16);
  unsigned int i = 0;

  for(; i<ruleset->num; i++) lmn_ruleset_put(new_ruleset, lmn_rule_copy(ruleset->rules[i]));
  return new_ruleset;
}

/*----------------------------------------------------------------------
 * System RuleSet
 */

LmnRuleSet system_ruleset;

static void init_system_rulset()
{
  system_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
}

static void destroy_system_ruleset()
{
  lmn_ruleset_free(system_ruleset);
}

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_system_rule(LmnRule rule)
{
  lmn_ruleset_put(system_ruleset, rule);
}

/*----------------------------------------------------------------------
 * Module
 */

st_table *module_table;

static void init_module_table()
{
  module_table = st_init_numtable();
}

static void destroy_module_table()
{
  st_free_table(module_table);
}

/* Associates module_name with ruleset */
void lmn_set_module(lmn_interned_str module_name, LmnRuleSet ruleset)
{
  st_insert(module_table, (st_data_t)module_name, (st_data_t)ruleset);
}

/* Returns RuleSet associated with module_name. If nothing is, returns NULL. */
LmnRuleSet lmn_get_module_ruleset(lmn_interned_str module_name)
{
  LmnRuleSet ruleset;

  if (st_lookup(module_table, (st_data_t)module_name, (void *)&ruleset)) return ruleset;
  return NULL;
}

/*----------------------------------------------------------------------*/

void init_rules()
{
  init_ruleset_table();
  init_module_table();
  init_system_rulset();
}

void destroy_rules()
{
  destroy_ruleset_table();
  destroy_module_table();
  destroy_system_ruleset();
 }



#undef GROWN_RATE
