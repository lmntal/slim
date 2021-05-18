/*
 * isomorphism.hpp
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

#ifndef SSC_ISOMORPHISM_HPP
#define SSC_ISOMORPHISM_HPP

#include "check_result.hpp"
#include "state_space.hpp"

#include <algorithm>
#include <functional>
#include <unordered_map>
#include <vector>

namespace ssc {

// グラフ同型により等価性を判定する
// 正直なところ信頼性はアヤシイ
template <class Comparator>
check_result isomorphism_check(const json_t &json1, const json_t &json2,
                               Comparator cmp) {
  const state_space ss1(json1);
  const state_space ss2(json2);

  if (ss1.states.size() != ss2.states.size())
    return check_result::fail("number of states are different");

  std::unordered_map<state_space::state_id_t, state_space::state_id_t> isomorphism;
  isomorphism[ss1.init] = ss2.init;
  if (!cmp(ss1.states.at(ss1.init), ss2.states.at(ss2.init)))
    return check_result::fail("initial states are different");

  std::function<check_result(state_space::state_id_t)> match_successors =
      [&](state_space::state_id_t s1) -> check_result {
    auto s2 = isomorphism[s1];
    auto &succs1 = ss1.transitions.at(s1);
    auto &succs2 = ss2.transitions.at(s2);

    if (succs1.size() != succs2.size())
      return check_result::fail("number of successors are different");
    auto size = succs1.size();

    bool all_matched =
        std::all_of(succs1.begin(), succs1.end(), [&](state_space::state_id_t s) {
          return isomorphism.find(s) != isomorphism.end();
        });
    if (all_matched)
      return check_result();

    std::vector<size_t> permutation(succs1.size()); // successorの対応
    for (size_t i = 0; i < size; i++)
      permutation[i] = i;

    // バックトラックするときのためにまだマッチしていないものを記録
    std::vector<state_space::state_id_t> not_matched;
    for (size_t i = 0; i < size; i++)
      if (isomorphism.find(succs1[i]) == isomorphism.end())
        not_matched.push_back(i);

    do {
      bool matched = true;

      // すでにマッチしたものと矛盾していないか確認
      for (size_t i = 0; i < size; i++) {
        if (isomorphism.find(succs1[i]) != isomorphism.end() &&
            isomorphism[succs1[i]] != succs2[permutation[i]]) {
          matched = false;
          break;
        }
      }

      if (!matched)
        continue;

      // 状態同士の比較
      for (auto idx : not_matched) {
        if (!cmp(ss1.states.at(succs1[idx]),
                 ss2.states.at(succs2[permutation[idx]]))) {
          matched = false;
          break;
        }
      }

      if (!matched)
        continue;

      // 暫定的にマッチングさせる
      for (auto idx : not_matched)
        isomorphism[succs1[idx]] = succs2[permutation[idx]];

      // 以降のマッチングを試す
      for (auto idx : not_matched) {
        if (!match_successors(succs1[idx])) {
          matched = false;
          break;
        }
      }

      // マッチングに成功
      if (matched)
        return check_result();

      // どこかでマッチングに失敗したので巻き戻す
      for (auto idx : not_matched)
        isomorphism.erase(succs1[idx]);

    } while (std::next_permutation(permutation.begin(), permutation.end()));

    return check_result::fail("successors are not matched");
  };

  auto result = match_successors(ss1.init);
  return (result) ? check_result::success(isomorphism) : result;
}

}

#endif /* SSC_ISOMORPHISM_HPP */
