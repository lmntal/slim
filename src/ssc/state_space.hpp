/*
 * state_space.hpp
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
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
 */

#ifndef SSC_STATE_SPACE_HPP
#define SSC_STATE_SPACE_HPP

#include "element/element.h"

using array_ptr = slim::element::json::value_ptr<slim::element::json::array>;
using object_ptr = slim::element::json::value_ptr<slim::element::json::object>;

#include <cstddef>
#include <unordered_map>
#include <vector>
#include <exception>

namespace ssc {

namespace c17 = slim::element;
using namespace slim::element;

/// jsonデータを使いやすい形に変換するためのデータ型
struct state_space {
  /// 状態を0から始まる連番で管理するための添え字
  using state_id_t = size_t;

  state_space(const json_t &json) {
    auto &ss = c17::get<object_ptr>(json)->value;
    auto &sts = c17::get<object_ptr>(ss.at("states"))->value;

    // key -> state idの変換表
    std::unordered_map<std::string, state_id_t> id_map;

    // state idとkeyの変換表を用意
    id_to_key.resize(sts.size());
    size_t index = 0;
    for (auto &p : sts) {
      auto key = p.first;
      id_map[key] = index;
      id_to_key[index] = key;
      index++;
    }

    // 状態数で検証
    auto size = c17::get<json::integer>(ss.at("size")).value;
    if (size != sts.size())
      throw new std::runtime_error("number of states are incoherent (" + std::to_string(size) + " vs " + std::to_string(sts.size()) + ")");

    // 初期状態
    init = id_map[c17::get<json::string>(ss.at("init")).value];

    // 状態表を作成
    states.resize(sts.size());
    for (auto &p : sts)
      states[id_map[p.first]] = p.second;

    // 遷移表を作成
    auto &trs = c17::get<object_ptr>(ss.at("transitions"))->value;
    transitions.resize(trs.size());
    size_t end_count = 0;
    for (auto &p : trs) {
      if (id_map.find(p.first) == id_map.end())
        throw new std::runtime_error("unknown state '" + p.first + "' is found in transitions.");

      auto source = id_map[p.first];
      auto &succ = c17::get<array_ptr>(p.second)->value;

      if (succ.empty())
        end_count++;

      for (auto &tr : succ) {
        auto key = c17::get<json::string>(c17::get<object_ptr>(tr)->value.at("succ")).value;
        if (id_map.find(key) == id_map.end())
          throw new std::runtime_error("unknown destination '" + key + "' from state '" + p.first + "'is found.");

        auto dest = id_map[key];

        // 遷移ラベル（ルール名）は無視しているが良いのだろうか
        transitions[source].push_back(dest);
      }
    }

    // 終状態の数で検証
    auto end_states = c17::get<json::integer>(ss.at("end_size")).value;
    if (end_states != end_count)
      throw new std::runtime_error("number of end states are incoherent (" + std::to_string(end_states) + " vs " + std::to_string(end_count) + ")");
  }

  state_id_t init;
  std::vector<json_t> states;
  std::vector<std::vector<state_id_t>> transitions;
  // state idから元の状態番号への表（復元用）
  std::vector<std::string> id_to_key;
};

using state_space_homomorphism =
    std::unordered_map<state_space::state_id_t, state_space::state_id_t>;

} // namespace ssc

#endif /* SSC_STATE_SPACE_HPP */
