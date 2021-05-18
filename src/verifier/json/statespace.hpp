/*
 * verifier/json/statespace.hpp
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

#ifndef SLIM_VERIFIER_JSON_STATESPACE_HPP
#define SLIM_VERIFIER_JSON_STATESPACE_HPP

#include "../statespace.h"

#include "element/element.h"

#include <unordered_map>

namespace slim {
namespace verifier {
namespace json {

namespace c17 = slim::element;
using slim::element::json_t;

/**
 * 状態をjsonフォーマットで出力する.
 *
 * Example:
 *   { "string": (文字列表現<string>) }
 */
element::json::object_t to_json(State &s) {
  using slim::element::to_json;

  auto org_next_id = env_next_id();
  auto mem = s.restore_membrane_inner(FALSE);
  env_set_next_id(org_next_id);

  // 本当はJSONで出したいのだが文字列くらいしかダンプする方法がないので仕方なく
  // いつかやりたいところ
  lmn_env.output_format = DEFAULT;
  json_t string_rep = to_json(slim::to_string_membrane(mem));
  lmn_env.output_format = JSON;

  if (s.is_binstr_user()) {
    mem->free_rec();
  }

  std::unordered_map<std::string, json_t> res;
  res["string"] = string_rep;
  return c17::get<element::json::object_t>(to_json(res));
}

/**
 * 状態空間をjsonフォーマットで出力する.
 *
 * Example:
 *   {
 *     "size": (状態数<integer>),
 *     "init": "(状態ID<integer)",
 *     "states": { "(状態ID<integer>)": { "string": (文字列表現<string>) }, ...
 * } "transitions": { "(状態ID<integer>)": [ { "ID": "(状態ID<integer>)",
 * "label": { ... } }, ...] }
 *   }
 */
inline element::json_t to_json(StateSpace &ss) {
  using slim::element::to_json;
  std::unordered_map<std::string, json_t> res;
  auto states = ss.all_states();

  res["size"] = to_json(states.size());
  res["end_size"] = to_json(ss.num_of_ends());
  res["init"] = to_json(
      std::to_string(state_format_id(ss.initial_state(), ss.is_formatted())));

  if (lmn_env.sp_dump_format != INCREMENTAL) {
    std::unordered_map<std::string, json_t> states_json;

    for (auto &s : states) {
      /* Rehashが発生している場合,
       * dummyフラグが真かつエンコード済みフラグが偽のStateオブジェクトが存在する.
       * このようなStateオブジェクトのバイナリストリングは
       * Rehashされた側のテーブルに存在するStateオブジェクトに登録されているためcontinueする.
       */
      if (s->is_dummy() && !s->is_encoded())
        continue;

      // 状態をjsonに変換（ここは明示的に名前空間を指定する必要がある）
      auto state = verifier::json::to_json(*s);

      // 状態空間における付加情報
      if (ss.has_property())
        (*state)["property"] =
            std::string(ss.automata()->state_name(state_property_state(s)));

#ifdef KWBT_OPT
      (*state)["cost"] = cost;
#endif

      /* この時点で状態は, ノーマル || (dummyフラグが立っている &&
       * エンコード済)である. dummyならば,
       * バイナリストリング以外のデータはオリジナル側(parent)に記録している. */
      auto original = s->is_dummy() ? state_get_parent(s) : s;
      auto print_id = state_format_id(original, ss.is_formatted());
      states_json[std::to_string(print_id)] = state;
    }

    res["states"] = to_json(states_json);
  }

  using transition_t = std::vector<std::unordered_map<std::string, json_t>>;
  std::unordered_map<std::string, transition_t> transitions;
  for (auto &s : states) {
    if (s->is_dummy() && s->is_encoded())
      continue;

    transition_t succ;
    if (s->successors) {
      for (int i = 0; i < s->successor_num; i++) {
        /* MEMO: rehashが発生していてもsuccessorポインタを辿る先は変わらない */
        std::unordered_map<std::string, json_t> ts;
        ts["succ"] = std::to_string(
            state_format_id(state_succ_state(s, i), ss.is_formatted()));

        if (s->has_trans_obj()) {
          auto t = transition(s, i);
          std::vector<std::string> rules;
          for (int j = 0; j < transition_rule_num(t); j++)
            rules.push_back(lmn_id_to_name(transition_rule(t, j)));
          ts["rules"] = to_json(rules);
        }
        succ.push_back(ts);
      }
    }
    transitions[std::to_string(state_format_id(s, ss.is_formatted()))] = succ;
  }
  res["transitions"] = to_json(std::move(transitions));

  return to_json(res);
}
}
}
}

#endif /* SLIM_VERIFIER_JSON_STATESPACE_HPP */
