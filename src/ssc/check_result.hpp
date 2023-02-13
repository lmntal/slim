/*
 * check_result.hpp
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

#ifndef SSC_CHECK_RESULT_HPP
#define SSC_CHECK_RESULT_HPP

#include "state_space.hpp"

#include <string>
#include <unordered_map>

namespace ssc {

// 等価性判定結果を格納する
// 失敗の場合は失敗理由を示す文字列、成功の場合は同型射
struct check_result {
  bool succeeded;
  std::string reason;
  std::unordered_map<state_space::state_id_t, state_space::state_id_t> morphism;

  check_result() : succeeded(true) {}
  check_result(bool s, const std::string &r) : succeeded(s), reason(r) {}
  check_result(bool s, const state_space_homomorphism &r)
      : succeeded(s), morphism(r) {}
  static check_result success(const state_space_homomorphism &reason) {
    return check_result(true, reason);
  }
  static check_result fail(const std::string &reason) {
    return check_result(false, reason);
  }

  explicit operator bool() { return succeeded; }
};

} // namespace ssc

#endif /* SSC_CHECK_RESULT_HPP */
