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

#include "rule.h"
#include "ankerl/unordered_dense.hpp"
#include "lmntal.h"
#include "rule.hpp"
#include "system_ruleset.h"

/*----------------------------------------------------------------------
 * Rule
 */

/* prototypes */
void init_rules();
void destroy_rules();

/*----------------------------------------------------------------------
 * Rule Set
 */

/* 2つの(Vector *)rulesetsが等価であるか判定, 等価の場合に真を返す.
 * Vectorはルールセットの整数IDで整列済みであることが前提 */
bool lmn_rulesets_equals(std::vector<LmnRuleSetRef> const &rs_v1, std::vector<LmnRuleSetRef> const &rs_v2) {
  unsigned int n = rs_v1.size();
  if (rs_v1.size() != rs_v2.size())
    return false;

  /* ルールセットの種類の比較 (IDで昇順に並べておくコードに依存) */
  unsigned int un1 = 0;
  unsigned int un2 = 0;
  for (unsigned int i = 0; i < rs_v1.size(); i++) {
    LmnRuleSetRef rs1, rs2;
    rs1 = rs_v1[i];
    rs2 = rs_v2[i];

    /* 異なるidであれば等価ではない */
    if (rs1->id != rs2->id)
      return false;

    /* rulesetsがuniq ruleを含むrulesetを何個持っているか調べておく */
    if (rs1->has_unique())
      un1++;
    if (rs2->has_unique())
      un2++;
  }

  if (un1 != un2)
    return false;
  if (un1 == 0)
    return true;

  /* ---uniq制約がある場合の処理--- */
  LMN_ASSERT(rs_v1.size() > 0);
  BOOL *rs2v_matched;
  bool  is_ok;

  rs2v_matched = LMN_NALLOC<BOOL>(rs_v1.size());
  memset(rs2v_matched, 0U, sizeof(BOOL) * rs_v1.size());

  for (unsigned int i = 0; i < rs_v1.size(); i++) {
    is_ok     = false;
    auto *rs1 = (LmnRuleSetRef)rs_v1[i];
    for (unsigned int j = 0; j < rs_v1.size(); j++) {
      auto *rs2 = (LmnRuleSetRef)rs_v2[i];
      if (rs1->id < rs2->id) /* 比較打ち切り */
        break;               /* INNER LOOP */

      if (rs1->id == rs2->id && !rs2v_matched[j] && *rs1 == *rs2) {
        is_ok           = true;
        rs2v_matched[j] = true;
        break; /* INNER LOOP */
      }
    }

    if (!is_ok) /* rs1にマッチするルールセットが存在しなかった */
      break;    /* OUTER LOOP */
  }

  LMN_FREE(rs2v_matched);
  return is_ok;
}

/*----------------------------------------------------------------------
 * System RuleSet
 */

LmnRuleSetRef system_ruleset;

static void init_system_ruleset() { system_ruleset = new LmnRuleSet(LmnRuleSetTable::gen_id(), 10); }

static void destroy_system_ruleset() { delete (system_ruleset); }

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_system_rule(LmnRuleRef rule) { system_ruleset->put(rule); }

/*----------------------------------------------------------------------
 * Initial RuleSet
 */

LmnRuleSetRef initial_ruleset;
LmnRuleSetRef initial_system_ruleset;

static void init_initial_ruleset() {
  initial_ruleset        = new LmnRuleSet(LmnRuleSetTable::gen_id(), 10);
  initial_system_ruleset = new LmnRuleSet(LmnRuleSetTable::gen_id(), 10);
}

static void destroy_initial_ruleset() {
  delete (initial_ruleset);
  delete (initial_system_ruleset);
}

/* Adds rule to the system ruleset.
   ruleの解放は呼び出され側が行う */
void lmn_add_initial_rule(LmnRuleRef rule) { initial_ruleset->put(rule); }

void lmn_add_initial_system_rule(LmnRuleRef rule) { initial_system_ruleset->put(rule); }

/*----------------------------------------------------------------------
 * Module
 */

ankerl::unordered_dense::map<lmn_interned_str, LmnRuleSetRef> mod_table{};

/* Associates module_name with ruleset */
void lmn_set_module(lmn_interned_str module_name, LmnRuleSetRef ruleset) {
  mod_table[module_name] = ruleset;
}

/* Returns RuleSet associated with module_name. If nothing is, returns NULL. */
LmnRuleSetRef lmn_get_module_ruleset(lmn_interned_str module_name) {
  if (auto it = mod_table.find(module_name); it != mod_table.end())
    return it->second;
  return nullptr;
}

/*----------------------------------------------------------------------*/

void init_rules() {
  init_system_ruleset();
  init_initial_ruleset();
}

void destroy_rules() {
  destroy_system_ruleset();
  destroy_initial_ruleset();
}

#undef GROWN_RATE
