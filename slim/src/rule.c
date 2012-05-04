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

#include "rule.h"
#include "system_ruleset.h"

/*----------------------------------------------------------------------
 * Rule
 */

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
  rule->pre_id = ANONYMOUS;
  rule->history_tbl = NULL;
  //rule->cost = 0U;

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
  LmnRule new_rule;
  BYTE *inst_seq;

  if (lmn_rule_get_inst_seq(rule)) {
    inst_seq = LMN_NALLOC(BYTE, rule->inst_seq_len);
    inst_seq = memcpy(inst_seq, rule->inst_seq, rule->inst_seq_len);
  } else {
    inst_seq = NULL;
  }

  new_rule = make_rule(inst_seq, rule->inst_seq_len, rule->translated, rule->name);
  if (lmn_rule_get_history_tbl(rule)) {
    new_rule->history_tbl = st_copy(lmn_rule_get_history_tbl(rule));
    new_rule->pre_id = lmn_rule_get_pre_id(rule);
  }
  return new_rule;
}

/* ruleとruleの要素を解放する */
void lmn_rule_free(LmnRule rule)
{
  LMN_FREE(rule->inst_seq);
  if (lmn_rule_get_history_tbl(rule)) {
    st_free_table(lmn_rule_get_history_tbl(rule));
  }
  LMN_FREE(rule);
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

/*----------------------------------------------------------------------
 * Rule Set
 */
struct LmnRuleSetTable *ruleset_table;

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
  ruleset->atomic = ATOMIC_NONE;
  ruleset->is_atomic_valid = FALSE;
  ruleset->is_copy = FALSE;
  ruleset->has_uniqrule = FALSE;

  return ruleset;
}

/* Frees RuleSet and its elements */
void lmn_ruleset_free(LmnRuleSet ruleset)
{
  int i;

  for (i = 0; i < ruleset->num; i++) {
    lmn_rule_free(ruleset->rules[i]);
  }
  LMN_FREE(ruleset->rules);
  LMN_FREE(ruleset);
}

/* Adds rule into ruleset */
inline void lmn_ruleset_put(LmnRuleSet ruleset, LmnRule rule)
{
  if (ruleset->num == ruleset->cap) {
    ruleset->cap = (ruleset->cap * 2);
    ruleset->rules = LMN_REALLOC(LmnRule, ruleset->rules, ruleset->cap);
  }
  ruleset->rules[ruleset->num++] = rule;

  /* 非uniqrulesetにuniq ruleが追加されたら, フラグを立てる. */
  if (!lmn_ruleset_has_uniqrule(ruleset) &&
      lmn_rule_get_history_tbl(rule)) {
    ruleset->has_uniqrule = TRUE;
  }
}


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
inline void lmn_set_ruleset(LmnRuleSet ruleset, int id)
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

/* uniq rulesetに存在する各ruleの履歴の総数を返す.
 * uniq rulesetではない場合, -1を返す.
 */
long lmn_ruleset_history_num(LmnRuleSet rs)
{
  if (lmn_ruleset_has_uniqrule(rs)) {
    return -1;
  } else {
    int i, n, his_num;

    n = 0;
    his_num = 0;
    for (i = 0; i < n; i++){
      LmnRule r = lmn_ruleset_get_rule(rs, i);
      if (lmn_rule_get_history_tbl(r)) {
        his_num += st_num(lmn_rule_get_history_tbl(r));
      }
    }
    return his_num;
  }
}

/* 実態をコピーしたルールセットを開放する */
void lmn_ruleset_copied_free(LmnRuleSet rs)
{
  unsigned int i;
  for (i = 0; i < lmn_ruleset_rule_num(rs); i++) {
    LmnRule r = lmn_ruleset_get_rule(rs, i);
    lmn_rule_free(r); /* free copied rule object */
  }

  /* free copied ruleset object */
  LMN_FREE(rs->rules);
  LMN_FREE(rs);
}

static inline LmnRuleSet lmn_ruleset_copy_object(LmnRuleSet src)
{
  LmnRuleSet new;
  unsigned int i, r_n;

  r_n = lmn_ruleset_rule_num(src);
  new = lmn_ruleset_make(lmn_ruleset_get_id(src), r_n);
  new->is_copy = TRUE;
  new->atomic = src->atomic;
  new->is_atomic_valid = src->is_atomic_valid;

  /* ルール単位のオブジェクト複製 */
  for (i = 0; i < r_n; i++) {
    LmnRule r = lmn_ruleset_get_rule(src, i);
    lmn_ruleset_put(new, lmn_rule_copy(r));
  }
  return new;
}

static inline BOOL ruleset_cp_is_need_object(LmnRuleSet src)
{
  return lmn_ruleset_has_uniqrule(src) ||
         (lmn_ruleset_atomic_type(src) != ATOMIC_NONE);
}

/* ルールセットsrcを複製して返す.
 * 基本的にはオリジナルのobjectへの参照を返すのみ.
 * ただし, uniqルールセットである場合(現在はuniq使用時のみ),
 * ルールセットオブジェクトを独立に扱うため新たにmallocして複製したオブジェクトを返す.
 * uniqルールの場合, ルール単位でobjectを複製する */
LmnRuleSet lmn_ruleset_copy(LmnRuleSet src)
{
  if (ruleset_cp_is_need_object(src)) {
    return lmn_ruleset_copy_object(src);
  } else {
    return src;
  }
}

unsigned long lmn_ruleset_space(LmnRuleSet rs)
{
  unsigned long ret = 0;
  if (lmn_ruleset_has_uniqrule(rs) || lmn_ruleset_is_copy(rs)) {
    unsigned int i, n;

    n = lmn_ruleset_rule_num(rs);
    ret += sizeof(struct LmnRuleSet);
    ret += sizeof(struct LmnRule*) * n;
    for (i = 0; i < n; i++) {
      LmnRule r = lmn_ruleset_get_rule(rs, i);
      if (lmn_rule_get_history_tbl(r)) { /* 履歴表を持っている場合  */
        st_table_space(lmn_rule_get_history_tbl(r));
      }
    }
  }
  return ret;
}

/* 2つのrulesetが同じruleを持つか判定する.
 * (ruleの順序はソースコード依存) */
BOOL lmn_ruleset_equals(LmnRuleSet set1, LmnRuleSet set2)
{
  /* rulesetの種類をチェック */
  if (lmn_ruleset_get_id(set1) != lmn_ruleset_get_id(set2))  {
    return FALSE;
  }
  else {
    BOOL t1, t2;
    t1 = lmn_ruleset_has_uniqrule(set1);
    t2 = lmn_ruleset_has_uniqrule(set2);

    if (!t1 && !t2) {
      /* 互いにuniq rulsetでなければruleset idの比較でok */
      return TRUE;
    }
    else if ((!t1 && t2) || (t1 && !t2)) {
      /* uniq ruleset同士ではなければ当然FALSE */
      return FALSE;
    }
    else {
      /* uniq ruleset同士の場合:
       *   ruleの適用ヒストリまで比較 */
      unsigned int i, n;

      n = lmn_ruleset_rule_num(set1);
      if (n != lmn_ruleset_rule_num(set2)) {
        return FALSE;
      }
      for (i = 0; i < n; i++) {
        LmnRule rule1, rule2;
        st_table_t hist1, hist2;

        rule1 = lmn_ruleset_get_rule(set1, i);
        rule2 = lmn_ruleset_get_rule(set2, i);
        hist1 = lmn_rule_get_history_tbl(rule1);
        hist2 = lmn_rule_get_history_tbl(rule2);

        if (!hist1 && !hist2) {
          continue;
        }
        else if ((!hist1 && hist2) || (hist1 && !hist2) || !st_equals(hist1, hist2)) {
          return FALSE;
        }
      }

      return TRUE;
    }
  }
}

/* rulesetsにrulesetが含まれているか判定 */
BOOL lmn_rulesets_contains(Vector *rs_vec, LmnRuleSet set1)
{
  unsigned int i, rs1_id;

  rs1_id = lmn_ruleset_get_id(set1);
  for (i = 0; i < vec_num(rs_vec); i++) {
    LmnRuleSet set2 = (LmnRuleSet)vec_get(rs_vec, i);

    /* 同じrulesetが見つかればTRUEを返す */
    if (rs1_id == lmn_ruleset_get_id(set2) &&
        lmn_ruleset_equals(set1, set2)) {
      return TRUE;
    }
  }
  /* 見つからなければFALSE */
  return FALSE;
}

/* 2つの(Vector *)rulesetsが等価であるか判定, 等価の場合に真を返す.
 * Vectorはルールセットの整数IDで整列済みであることが前提 */
BOOL lmn_rulesets_equals(Vector *rs_v1, Vector *rs_v2)
{
  unsigned int n;

  n = vec_num(rs_v1);
  if (n != vec_num(rs_v2)) {
    return FALSE;
  }
  else {
    unsigned int i, un1, un2;

    /* ルールセットの種類の比較 (IDで昇順に並べておくコードに依存) */
    un1 = 0;
    un2 = 0;
    for (i = 0; i < n; i++) {
      LmnRuleSet rs1, rs2;
      rs1 = (LmnRuleSet)vec_get(rs_v1, i);
      rs2 = (LmnRuleSet)vec_get(rs_v2, i);

      /* 異なるidであれば等価ではない */
      if (lmn_ruleset_get_id(rs1) != lmn_ruleset_get_id(rs2)) {
        return FALSE;
      }

      /* rulesetsがuniq ruleを含むrulesetを何個持っているか調べておく */
      if (lmn_ruleset_has_uniqrule(rs1)) un1++;
      if (lmn_ruleset_has_uniqrule(rs2)) un2++;
    }

    if (un1 != un2) {
      return FALSE;
    }
    else if (un1 == 0) {
      return TRUE;
    }
    else {
      /* ---uniq制約がある場合の処理--- */
      LMN_ASSERT(n > 0);
      BOOL *rs2v_matched, is_ok;

      rs2v_matched = LMN_NALLOC(BOOL, n);
      memset(rs2v_matched, 0U, sizeof(BOOL) * n);

      for (i = 0; i < n; i++) {
        LmnRuleSet rs1;
        unsigned int j;

        is_ok  = FALSE;
        rs1 = (LmnRuleSet)vec_get(rs_v1, i);
        for (j = 0; j < n; j++) {
          LmnRuleSet rs2 = (LmnRuleSet)vec_get(rs_v2, i);
          if (lmn_ruleset_get_id(rs1) < lmn_ruleset_get_id(rs2)) {
            /* 比較打ち切り */
            break; /* INNER LOOP */
          }
          else if (lmn_ruleset_get_id(rs1) == lmn_ruleset_get_id(rs2) &&
                   !rs2v_matched[j] &&
                   lmn_ruleset_equals(rs1, rs2)) {
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
 * Initial RuleSet
 */

LmnRuleSet initial_ruleset;
LmnRuleSet initial_system_ruleset;

static void init_initial_rulset()
{
  initial_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
  initial_system_ruleset = lmn_ruleset_make(lmn_gen_ruleset_id(), 10);
}

static void destroy_initial_ruleset()
{
  lmn_ruleset_free(initial_ruleset);
  lmn_ruleset_free(initial_system_ruleset);
}

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_initial_rule(LmnRule rule)
{
  lmn_ruleset_put(initial_ruleset, rule);
}

void lmn_add_initial_system_rule(LmnRule rule)
{
  lmn_ruleset_put(initial_system_ruleset, rule);
}

/*----------------------------------------------------------------------
 * Module
 */

st_table_t module_table;

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
  init_initial_rulset();
}

void destroy_rules()
{
  destroy_ruleset_table();
  destroy_module_table();
  destroy_system_ruleset();
  destroy_initial_ruleset();
 }



#undef GROWN_RATE
