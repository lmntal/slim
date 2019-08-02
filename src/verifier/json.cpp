/*
 * json.cpp
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 */

#include "json.hpp"

#include "statespace.h"

#include "element/element.h"

#include "element/json/conv.hpp"
#include "element/json/value.hpp"

#include <string>
#include <vector>
#include <unordered_map>

using namespace slim::element;

json_t to_json(State *s) {
  auto org_next_id = env_next_id();
  auto mem = s->restore_membrane_inner(FALSE);
  env_set_next_id(org_next_id);

  json_t res = json::string(slim::to_string_membrane(mem));

  if (s->is_binstr_user()) {
    mem->free_rec();
  }
  return res;
}

json_t to_json(StateSpace *ss) {
  std::unordered_map<std::string, json_t> res;
  auto states = ss->all_states();
  if (lmn_env.sp_dump_format != INCREMENTAL) {
    std::vector<std::unordered_map<std::string, json_t>> sts;
    for (auto &s : states) {
      /* Rehashが発生している場合,
       * dummyフラグが真かつエンコード済みフラグが偽のStateオブジェクトが存在する.
       * このようなStateオブジェクトのバイナリストリングは
       * Rehashされた側のテーブルに存在するStateオブジェクトに登録されているためcontinueする.
       */
      if (s->is_dummy() && !s->is_encoded())
        continue;

      /* この時点で状態は, ノーマル || (dummyフラグが立っている &&
       * エンコード済)である. dummyならば,
       * バイナリストリング以外のデータはオリジナル側(parent)に記録している. */
      State *target = !s->is_dummy() ? s : state_get_parent(s);
      auto print_id = ss ? state_format_id(target, ss->is_formatted()) : target->state_id;
      BOOL has_property = ss && ss->has_property();
      std::unordered_map<std::string, json_t> state;
      state["id"] = json::integer(print_id);
      state["process"] = to_json(s);
      if (has_property)
        state["property"] = json::string(ss->automata()->state_name(state_property_state(s)));

#ifdef KWBT_OPT
      state["cost"] = cost;
#endif
      sts.push_back(state);
    }
    res["states"] = slim::element::to_json(sts);
  }
  res["init"] = slim::element::to_json(
      state_format_id(ss->initial_state(), ss->is_formatted()));

  std::unordered_map<std::string,
                     std::vector<std::unordered_map<std::string, json_t>>>
      transitions;
  for (auto &s : states) {
    if (s->is_dummy() && s->is_encoded())
      continue;

    std::vector<std::unordered_map<std::string, json_t>> succ;
    if (s->successors) {
      for (int i = 0; i < s->successor_num; i++) {
        /* MEMO: rehashが発生していても, successorポインタを辿る先は, オリジナル
         */
        std::unordered_map<std::string, json_t> ts;
        ts["succ"] = slim::element::to_json(
            state_format_id(state_succ_state(s, i), ss->is_formatted()));

        if (s->has_trans_obj()) {
          auto t = transition(s, i);
          std::vector<std::string> rules;
          for (int j = 0; j < transition_rule_num(t); j++)
            rules.push_back(lmn_id_to_name(transition_rule(t, j)));
          ts["rules"] = slim::element::to_json(rules);
        }
        succ.push_back(ts);
      }
    }
    transitions[std::to_string(state_format_id(s, ss->is_formatted()))] = succ;
  }
  res["transitions"] = slim::element::to_json(std::move(transitions));
  return to_json(res);
}
