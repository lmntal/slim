/*
 * rule.cpp
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
 * $Id: rule.c,v 1.7 2008/10/16 18:12:55 sasaki Exp $
 */

extern "C" {
#include "rule.h"
#include "system_ruleset.h"
}
#include "rule.hpp"

/*----------------------------------------------------------------------
 * Rule
 */

extern "C" {
/* prototypes */
void init_rules(void);
void destroy_rules(void);
}




/* 中身のない、名前だけを持つルールを生成する */
LmnRuleRef lmn_rule_make_dummy(lmn_interned_str name) {
  return new LmnRule(NULL, -1, NULL, name);
}

/* create new rule with a translated function */
LmnRuleRef lmn_rule_make_translated(LmnTranslated translated,
                                    lmn_interned_str name) {
  return new LmnRule(NULL, 0, translated, name);
}

/* ruleをコピーして新しいルールを作成する */
LmnRuleRef lmn_rule_copy(LmnRuleRef rule) {
  LmnRuleRef new_rule;
  BYTE *inst_seq;

  if (lmn_rule_get_inst_seq(rule)) {
    inst_seq = LMN_NALLOC(BYTE, rule->inst_seq_len);
    inst_seq = (BYTE *)memcpy(inst_seq, rule->inst_seq, rule->inst_seq_len);
  } else {
    inst_seq = NULL;
  }

  new_rule =
      new LmnRule(inst_seq, rule->inst_seq_len, rule->translated, rule->name);
  if (lmn_rule_get_history_tbl(rule)) {
    new_rule->history_tbl = st_copy(lmn_rule_get_history_tbl(rule));
    new_rule->pre_id = lmn_rule_get_pre_id(rule);
  }
  return new_rule;
}

st_table_t lmn_rule_get_history_tbl(LmnRuleRef rule) {
  return rule->history_tbl;
}

lmn_interned_str lmn_rule_get_pre_id(LmnRuleRef rule) { return rule->pre_id; }

void lmn_rule_set_pre_id(LmnRuleRef rule, lmn_interned_str t) {
  rule->pre_id = t;
}

/* ルールの処理を行う関数を返す。ルールが関数を持たなければNULLを返す */
LmnTranslated lmn_rule_get_translated(LmnRuleRef rule) {
  return rule->translated;
}

/* ルールの処理を行う中間語命令列を変換したバイト列を返す。ルールが列を
   持たなければNULLを返す。*/
BYTE *lmn_rule_get_inst_seq(LmnRuleRef rule) { return rule->inst_seq; }

/* ルールの名前を返す */
lmn_interned_str lmn_rule_get_name(LmnRuleRef rule) { return rule->name; }

/* ルール名のセット */
void lmn_rule_set_name(LmnRuleRef rule, lmn_interned_str rule_name) {
  rule->name = rule_name;
}

LmnCost lmn_rule_get_cost(LmnRuleRef rule) { return rule->cost; }

void lmn_rule_set_cost(LmnRuleRef rule, LmnCost rule_cost) {
  rule->cost = rule_cost;
}

BOOL lmn_rule_is_invisible(LmnRuleRef rule) {
  return rule->is_invisible == TRUE;
}

void lmn_rule_init_uniq_rule(LmnRuleRef rule) {
  rule->history_tbl = st_init_numtable();
}

LmnRuleRef dummy_rule(void) {
  static struct LmnRule rule;
  static BOOL first = TRUE;

  if (first) {
    first = FALSE;
    rule.name = lmn_intern("");
  }

  return &rule;
}

/*----------------------------------------------------------------------
 * Rule Set
 */

struct LmnRuleSetTable *ruleset_table;

/* Generates and returns new RuleSet id */
int lmn_gen_ruleset_id() {
  static int ruleset_next_id = 1;

  return ruleset_next_id++;
}


/* ルールセットテーブルの初期化 */
static void init_ruleset_table() {
  ruleset_table = new LmnRuleSetTable(64);
  /* 安全なメモリ解放の為ゼロクリア */
  memset(ruleset_table->entry, 0,
         ruleset_table->size * sizeof(ruleset_table->entry[0]));
}


LmnRuleSetRef lmn_ruleset_copy(LmnRuleSetRef src) {
  return src->duplicate();
}

unsigned long lmn_ruleset_space(LmnRuleSetRef rs) {
  unsigned long ret = 0;
  if (lmn_ruleset_has_uniqrule(rs) || lmn_ruleset_is_copy(rs)) {
    unsigned int i, n;

    n = rs->num;
    ret += sizeof(struct LmnRuleSet);
    ret += sizeof(struct LmnRule *) * n;
    for (i = 0; i < n; i++) {
      LmnRuleRef r = lmn_ruleset_get_rule(rs, i);
      if (lmn_rule_get_history_tbl(r)) { /* 履歴表を持っている場合  */
        st_table_space(lmn_rule_get_history_tbl(r));
      }
    }
  }
  return ret;
}

void lmn_ruleset_validate_atomic(LmnRuleSetRef rs) {
  rs->is_atomic_valid = TRUE;
}

void lmn_ruleset_invalidate_atomic(LmnRuleSetRef rs) {
  rs->is_atomic_valid = FALSE;
}

BOOL lmn_ruleset_is_valid_atomic(LmnRuleSetRef rs) {
  return rs->is_atomic_valid;
}

/* Returns the ith rule in ruleset */
LmnRuleRef lmn_ruleset_get_rule(LmnRuleSetRef ruleset, int i) {
  return ruleset->rules[i];
}

/* Returns id of ruleset */
int lmn_ruleset_get_id(LmnRuleSetRef ruleset) { return ruleset->id; }

AtomicType lmn_ruleset_atomic_type(LmnRuleSetRef ruleset) {
  return ruleset->atomic;
}

void lmn_ruleset_set_atomic(LmnRuleSetRef ruleset, AtomicType t) {
  ruleset->atomic = t;
}

/* Returns RuleSet associated with id. If nothing is, returns NULL */
LmnRuleSetRef lmn_ruleset_from_id(int id) {
  if (ruleset_table->size <= (unsigned int)id)
    return NULL;
  else
    return ruleset_table->entry[id];
}

BOOL lmn_ruleset_is_copy(LmnRuleSetRef ruleset) { return ruleset->is_copy; }

BOOL lmn_ruleset_has_uniqrule(LmnRuleSetRef ruleset) {
  return ruleset->has_uniqrule;
}

LmnRuleRef *lmn_ruleset_get_rules(LmnRuleSetRef ruleset) {
  return ruleset->rules;
}

void lmn_ruleset_validate_0step(LmnRuleSetRef ruleset) {
  ruleset->is_0step = TRUE;
}
BOOL lmn_ruleset_is_0step(LmnRuleSetRef ruleset) { return ruleset->is_0step; }


/* rulesetsにrulesetが含まれているか判定 */
BOOL lmn_rulesets_contains(Vector *rs_vec, LmnRuleSetRef set1) {
  unsigned int i, rs1_id;

  rs1_id = lmn_ruleset_get_id(set1);
  for (i = 0; i < vec_num(rs_vec); i++) {
    LmnRuleSetRef set2 = (LmnRuleSetRef)vec_get(rs_vec, i);

    /* 同じrulesetが見つかればTRUEを返す */
    if (rs1_id == lmn_ruleset_get_id(set2) && *set1 == *set2) {
      return TRUE;
    }
  }
  /* 見つからなければFALSE */
  return FALSE;
}

/* 2つの(Vector *)rulesetsが等価であるか判定, 等価の場合に真を返す.
 * Vectorはルールセットの整数IDで整列済みであることが前提 */
BOOL lmn_rulesets_equals(Vector *rs_v1, Vector *rs_v2) {
  unsigned int n;

  n = vec_num(rs_v1);
  if (n != vec_num(rs_v2)) {
    return FALSE;
  } else {
    unsigned int i, un1, un2;

    /* ルールセットの種類の比較 (IDで昇順に並べておくコードに依存) */
    un1 = 0;
    un2 = 0;
    for (i = 0; i < n; i++) {
      LmnRuleSetRef rs1, rs2;
      rs1 = (LmnRuleSetRef)vec_get(rs_v1, i);
      rs2 = (LmnRuleSetRef)vec_get(rs_v2, i);

      /* 異なるidであれば等価ではない */
      if (lmn_ruleset_get_id(rs1) != lmn_ruleset_get_id(rs2)) {
        return FALSE;
      }

      /* rulesetsがuniq ruleを含むrulesetを何個持っているか調べておく */
      if (lmn_ruleset_has_uniqrule(rs1))
        un1++;
      if (lmn_ruleset_has_uniqrule(rs2))
        un2++;
    }

    if (un1 != un2) {
      return FALSE;
    } else if (un1 == 0) {
      return TRUE;
    } else {
      /* ---uniq制約がある場合の処理--- */
      LMN_ASSERT(n > 0);
      BOOL *rs2v_matched, is_ok;

      rs2v_matched = LMN_NALLOC(BOOL, n);
      memset(rs2v_matched, 0U, sizeof(BOOL) * n);

      for (i = 0; i < n; i++) {
        LmnRuleSetRef rs1;
        unsigned int j;

        is_ok = FALSE;
        rs1 = (LmnRuleSetRef)vec_get(rs_v1, i);
        for (j = 0; j < n; j++) {
          LmnRuleSetRef rs2 = (LmnRuleSetRef)vec_get(rs_v2, i);
          if (lmn_ruleset_get_id(rs1) < lmn_ruleset_get_id(rs2)) {
            /* 比較打ち切り */
            break; /* INNER LOOP */
          } else if (lmn_ruleset_get_id(rs1) == lmn_ruleset_get_id(rs2) &&
                     !rs2v_matched[j] && *rs1 == *rs2) {
            is_ok = TRUE;
            rs2v_matched[j] = TRUE;
            break; /* INNER LOOP */
          }
        }

        if (!is_ok) {
          /* rs1にマッチするルールセットが存在しなかった */
          break; /* OUTER LOOP */
        }
      }

      LMN_FREE(rs2v_matched);
      return is_ok;
    }
  }
}

/*----------------------------------------------------------------------
 * System RuleSet
 */

LmnRuleSetRef system_ruleset;

static void init_system_rulset() {
  system_ruleset = new LmnRuleSet(lmn_gen_ruleset_id(), 10);
}

static void destroy_system_ruleset() { delete(system_ruleset); }

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_system_rule(LmnRuleRef rule) {
  system_ruleset->put(rule);
}

/*----------------------------------------------------------------------
 * Initial RuleSet
 */

LmnRuleSetRef initial_ruleset;
LmnRuleSetRef initial_system_ruleset;

static void init_initial_rulset() {
  initial_ruleset = new LmnRuleSet(lmn_gen_ruleset_id(), 10);
  initial_system_ruleset = new LmnRuleSet(lmn_gen_ruleset_id(), 10);
}

static void destroy_initial_ruleset() {
  delete(initial_ruleset);
  delete(initial_system_ruleset);
}

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_initial_rule(LmnRuleRef rule) {
  initial_ruleset->put(rule);
}

void lmn_add_initial_system_rule(LmnRuleRef rule) {
  initial_system_ruleset->put(rule);
}

/*----------------------------------------------------------------------
 * Module
 */

st_table_t module_table;

static void init_module_table() { module_table = st_init_numtable(); }

static void destroy_module_table() { st_free_table(module_table); }

/* Associates module_name with ruleset */
void lmn_set_module(lmn_interned_str module_name, LmnRuleSetRef ruleset) {
  st_insert(module_table, (st_data_t)module_name, (st_data_t)ruleset);
}

/* Returns RuleSet associated with module_name. If nothing is, returns NULL. */
LmnRuleSetRef lmn_get_module_ruleset(lmn_interned_str module_name) {
  LmnRuleSetRef ruleset;

  if (st_lookup(module_table, (st_data_t)module_name, (st_data_t *)&ruleset))
    return ruleset;
  return NULL;
}

/*----------------------------------------------------------------------*/

void init_rules() {
  init_ruleset_table();
  init_module_table();
  init_system_rulset();
  init_initial_rulset();
}

void destroy_rules() {
  delete(ruleset_table);
  destroy_module_table();
  destroy_system_ruleset();
  destroy_initial_ruleset();
}

#undef GROWN_RATE
