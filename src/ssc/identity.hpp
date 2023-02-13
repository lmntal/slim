/*
 * identity.hpp
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

#ifndef SSC_IDENTITY_HPP
#define SSC_IDENTITY_HPP

#include "check_result.hpp"
#include "state_space.hpp"

#include <unordered_map>

namespace ssc {

// idが全く同じかどうかで等価性を判定する
template <class Comparator>
check_result identity_check(const slim::element::json_t &json1,
                            const slim::element::json_t &json2,
                            Comparator cmp) {
  const state_space ss1(json1);
  const state_space ss2(json2);

  if (ss1.states.size() != ss2.states.size())
    return check_result::fail("number of states are different");

  if (!cmp(ss1.states.at(ss1.init), ss2.states.at(ss2.init)))
    return check_result::fail("initial states are different");

  for (state_space::state_id_t id = 0; id < ss1.states.size(); id++) {
    auto &s1 = ss1.states[id];
    auto &s2 = ss2.states[id];
    if (!cmp(s1, s2))
      return check_result::fail("state " + ss1.id_to_key[id] + " and state " +
                                ss2.id_to_key[id] + " are different");
  }

  if (ss1.transitions != ss2.transitions)
    return check_result::fail("transition sets are different");

  std::unordered_map<state_space::state_id_t, state_space::state_id_t>
      isomorphism;
  for (state_space::state_id_t id = 0; id < ss1.states.size(); id++) {
    isomorphism[id] = id;
  }

  return check_result::success(isomorphism);
}

} // namespace ssc

#endif /* SSC_IDENTITY_HPP */
