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

/* ruleとruleの要素を解放する */
void lmn_rule_free(LmnRuleRef rule) {
  LMN_FREE(rule->inst_seq);
  if (lmn_rule_get_history_tbl(rule)) {
    st_free_table(lmn_rule_get_history_tbl(rule));
  }
  delete(rule);
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
};
struct LmnRuleSetTable *ruleset_table;

/* Generates and returns new RuleSet id */
int lmn_gen_ruleset_id() {
  static int ruleset_next_id = 1;

  return ruleset_next_id++;
}

/* Generates a new RuleSet */
LmnRuleSetRef lmn_ruleset_make(LmnRulesetId id, int init_size) {
  LmnRuleSetRef ruleset = LMN_MALLOC(struct LmnRuleSet);

  ruleset->id = id;
  ruleset->rules = LMN_CALLOC(LmnRuleRef, init_size);
  ruleset->num = 0;
  ruleset->cap = init_size;
  ruleset->atomic = ATOMIC_NONE;
  ruleset->is_atomic_valid = FALSE;
  ruleset->is_copy = FALSE;
  ruleset->has_uniqrule = FALSE;
  ruleset->is_0step = FALSE;

  return ruleset;
}

/* Frees RuleSet and its elements */
void lmn_ruleset_free(LmnRuleSetRef ruleset) {
  int i;

  for (i = 0; i < ruleset->num; i++) {
    lmn_rule_free(ruleset->rules[i]);
  }
  LMN_FREE(ruleset->rules);
  LMN_FREE(ruleset);
}

/* Adds rule into ruleset */
void lmn_ruleset_put(LmnRuleSetRef ruleset, LmnRuleRef rule) {
  if (ruleset->num == ruleset->cap) {
    ruleset->cap = (ruleset->cap * 2);
    ruleset->rules = LMN_REALLOC(LmnRuleRef, ruleset->rules, ruleset->cap);
  }
  ruleset->rules[ruleset->num++] = rule;

  /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
  if (!lmn_ruleset_has_uniqrule(ruleset) && lmn_rule_get_history_tbl(rule)) {
    ruleset->has_uniqrule = TRUE;
  }
}

/* ルールセットテーブルの初期化 */
static void init_ruleset_table() {
  ruleset_table = LMN_MALLOC(struct LmnRuleSetTable);
  ruleset_table->size = 64;
  ruleset_table->entry = LMN_NALLOC(LmnRuleSetRef, ruleset_table->size);
  /* 安全なメモリ解放の為ゼロクリア */
  memset(ruleset_table->entry, 0,
         ruleset_table->size * sizeof(ruleset_table->entry[0]));
}

/* ルールセットテーブルの解放処理 */
static void destroy_ruleset_table() {
  unsigned int i;

  for (i = 0; i < ruleset_table->size; i++) {
    if (ruleset_table->entry[i]) {
      lmn_ruleset_free(ruleset_table->entry[i]);
    }
  }
  LMN_FREE(ruleset_table->entry);
  LMN_FREE(ruleset_table);
}

/* Associates id with ruleset */
void lmn_set_ruleset(LmnRuleSetRef ruleset, int id) {
  /* 必要ならば容量を拡張 */
  while (ruleset_table->size <= (unsigned int)id) {
    int old_size = ruleset_table->size;
    ruleset_table->size *= 2;
    ruleset_table->entry =
        LMN_REALLOC(LmnRuleSetRef, ruleset_table->entry, ruleset_table->size);
    /* 安全なメモリ解放の為ゼロクリア */
    memset(ruleset_table->entry + old_size, 0,
           (ruleset_table->size - old_size) * sizeof(LmnRuleSetRef));
  }

  ruleset_table->entry[id] = ruleset;
}

/* uniq rulesetに存在する各ruleの履歴の総数を返す.
 * uniq rulesetではない場合, -1を返す.
 */
long lmn_ruleset_history_num(LmnRuleSetRef rs) {
  if (lmn_ruleset_has_uniqrule(rs)) {
    return -1;
  } else {
    int i, n, his_num;

    n = 0;
    his_num = 0;
    for (i = 0; i < n; i++) {
      LmnRuleRef r = lmn_ruleset_get_rule(rs, i);
      if (lmn_rule_get_history_tbl(r)) {
        his_num += st_num(lmn_rule_get_history_tbl(r));
      }
    }
    return his_num;
  }
}

/* 実態をコピーしたルールセットを開放する */
void lmn_ruleset_copied_free(LmnRuleSetRef rs) {
  unsigned int i;
  for (i = 0; i < lmn_ruleset_rule_num(rs); i++) {
    LmnRuleRef r = lmn_ruleset_get_rule(rs, i);
    lmn_rule_free(r); /* free copied rule object */
  }

  /* free copied ruleset object */
  LMN_FREE(rs->rules);
  LMN_FREE(rs);
}

static inline LmnRuleSetRef lmn_ruleset_copy_object(LmnRuleSetRef src) {
  LmnRuleSetRef result;
  unsigned int i, r_n;

  r_n = lmn_ruleset_rule_num(src);
  result = lmn_ruleset_make(lmn_ruleset_get_id(src), r_n);
  result->is_copy = TRUE;
  result->atomic = src->atomic;
  result->is_atomic_valid = src->is_atomic_valid;

  /* ルール単位のオブジェクト複製 */
  for (i = 0; i < r_n; i++) {
    LmnRuleRef r = lmn_ruleset_get_rule(src, i);
    lmn_ruleset_put(result, lmn_rule_copy(r));
  }
  return result;
}

static inline BOOL ruleset_cp_is_need_object(LmnRuleSetRef src) {
  return lmn_ruleset_has_uniqrule(src) ||
         (lmn_ruleset_atomic_type(src) != ATOMIC_NONE);
}

/* ルールセットsrcを複製して返す.
 * 基本的にはオリジナルのobjectへの参照を返すのみ.
 * ただし, uniqルールセットである場合(現在はuniq使用時のみ),
 * ルールセットオブジェクトを独立に扱うため新たにmallocして複製したオブジェクトを返す.
 * uniqルールの場合, ルール単位でobjectを複製する */
LmnRuleSetRef lmn_ruleset_copy(LmnRuleSetRef src) {
  if (ruleset_cp_is_need_object(src)) {
    return lmn_ruleset_copy_object(src);
  } else {
    return src;
  }
}

unsigned long lmn_ruleset_space(LmnRuleSetRef rs) {
  unsigned long ret = 0;
  if (lmn_ruleset_has_uniqrule(rs) || lmn_ruleset_is_copy(rs)) {
    unsigned int i, n;

    n = lmn_ruleset_rule_num(rs);
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

/* Returns the # of rules in ruleset */
unsigned int lmn_ruleset_rule_num(LmnRuleSetRef ruleset) {
  return ruleset->num;
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

/* 2つのrulesetが同じruleを持つか判定する.
 * (ruleの順序はソースコード依存) */
BOOL lmn_ruleset_equals(LmnRuleSetRef set1, LmnRuleSetRef set2) {
  /* rulesetの種類をチェック */
  if (lmn_ruleset_get_id(set1) != lmn_ruleset_get_id(set2)) {
    return FALSE;
  } else {
    BOOL t1, t2;
    t1 = lmn_ruleset_has_uniqrule(set1);
    t2 = lmn_ruleset_has_uniqrule(set2);

    if (!t1 && !t2) {
      /* 互いにuniq rulsetでなければruleset idの比較でok */
      return TRUE;
    } else if ((!t1 && t2) || (t1 && !t2)) {
      /* uniq ruleset同士ではなければ当然FALSE */
      return FALSE;
    } else {
      /* uniq ruleset同士の場合:
       *   ruleの適用ヒストリまで比較 */
      unsigned int i, n;

      n = lmn_ruleset_rule_num(set1);
      if (n != lmn_ruleset_rule_num(set2)) {
        return FALSE;
      }
      for (i = 0; i < n; i++) {
        LmnRuleRef rule1, rule2;
        st_table_t hist1, hist2;

        rule1 = lmn_ruleset_get_rule(set1, i);
        rule2 = lmn_ruleset_get_rule(set2, i);
        hist1 = lmn_rule_get_history_tbl(rule1);
        hist2 = lmn_rule_get_history_tbl(rule2);

        if (!hist1 && !hist2) {
          continue;
        } else if ((!hist1 && hist2) || (hist1 && !hist2) ||
                   !st_equals(hist1, hist2)) {
          return FALSE;
        }
      }

      return TRUE;
    }
  }
}

/* rulesetsにrulesetが含まれているか判定 */
BOOL lmn_rulesets_contains(Vector *rs_vec, LmnRuleSetRef set1) {
  unsigned int i, rs1_id;

  rs1_id = lmn_ruleset_get_id(set1);
  for (i = 0; i < vec_num(rs_vec); i++) {
    LmnRuleSetRef set2 = (LmnRuleSetRef)vec_get(rs_vec, i);

    /* 同じrulesetが見つかればTRUEを返す */
    if (rs1_id == lmn_ruleset_get_id(set2) && lmn_ruleset_equals(set1, set2)) {
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
                     !rs2v_matched[j] && lmn_ruleset_equals(rs1, rs2)) {
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
  system_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
}

static void destroy_system_ruleset() { lmn_ruleset_free(system_ruleset); }

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_system_rule(LmnRuleRef rule) {
  lmn_ruleset_put(system_ruleset, rule);
}

/*----------------------------------------------------------------------
 * Initial RuleSet
 */

LmnRuleSetRef initial_ruleset;
LmnRuleSetRef initial_system_ruleset;

static void init_initial_rulset() {
  initial_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
  initial_system_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
}

static void destroy_initial_ruleset() {
  lmn_ruleset_free(initial_ruleset);
  lmn_ruleset_free(initial_system_ruleset);
}

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_initial_rule(LmnRuleRef rule) {
  lmn_ruleset_put(initial_ruleset, rule);
}

void lmn_add_initial_system_rule(LmnRuleRef rule) {
  lmn_ruleset_put(initial_system_ruleset, rule);
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
  destroy_ruleset_table();
  destroy_module_table();
  destroy_system_ruleset();
  destroy_initial_ruleset();
}

#undef GROWN_RATE
