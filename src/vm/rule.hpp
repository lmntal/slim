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

#endif