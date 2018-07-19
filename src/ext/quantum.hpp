/*
 * ext/quantum.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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

#ifndef SLIM_EXT_QUANTUM_HPP
#define SLIM_EXT_QUANTUM_HPP

#include "element/element.h"
#include "vm/vm.h"

#include <memory>
#include <string>
#include <vector>

namespace slim {
namespace ext {

enum trilean { fls, tru, unk = 0xff };

namespace constraint {
struct integer_tag {};
struct gt_tag { long value; };
struct lt_tag { long value; };

static integer_tag integer;
constexpr gt_tag gt(long v) { return gt_tag{v}; }
constexpr lt_tag lt(long v) { return lt_tag{v}; }
}

struct constraint_store {
  trilean is_int_ = unk;
  std::pair<long, long> assumed_range = {std::numeric_limits<long>::min(), std::numeric_limits<long>::max()};

  trilean is_int() const { return is_int_; }

  void assume_int() {
    // assert(is_int_ != fls);
    is_int_ = tru;
  }

  trilean check(constraint::integer_tag _) const {
    return is_int_;
  }
  void assume(constraint::integer_tag _) {
    // assert(is_int_ != fls);
    is_int_ = tru;
  }

  trilean check(constraint::gt_tag t) const {
    if (is_int() == fls) return fls;
    if (t.value < assumed_range.first) return tru;
    if (assumed_range.first <= t.value && t.value < assumed_range.second) return unk;
    return fls;
  }
  void assume(constraint::gt_tag t) {
    assume(constraint::integer);
    assumed_range.first = std::max(t.value + 1, assumed_range.first);
  }

  trilean check(constraint::lt_tag t) const {
    if (is_int() == fls) return fls;
    if (assumed_range.second < t.value) return tru;
    if (assumed_range.first < t.value && t.value <= assumed_range.second) return unk;
    return fls;
  }
  void assume(constraint::lt_tag t) {
    assume(constraint::integer);
    assumed_range.second = std::min(t.value - 1, assumed_range.second);
  }
};

struct Quantum {
  LMN_SP_ATOM_HEADER;

  static int atom_type;

  std::unique_ptr<constraint_store> constraints;
  std::vector<std::unique_ptr<constraint_store>> saved_constraints;

  Quantum() : constraints(slim::element::make_unique<constraint_store>()) {};
  Quantum(const std::vector<uint8_t> &bytecode) : constraints(slim::element::make_unique<constraint_store>()) {
    auto res = bytecode;
    std::reverse(res.begin(), res.end());
    if (res[0] == tru) {
      constraints->is_int_ = tru;
      auto v = res.data() + 1;
      constraints->assumed_range = {((long *)v)[0], ((long *)v)[1]};
    }
  }
  Quantum(const Quantum &q) :
    constraints(slim::element::make_unique<constraint_store>(*q.constraints)) {
  }

  std::vector<uint8_t> encode() const {
    std::vector<uint8_t> result;
    result.push_back(constraints->is_int_);
    if (constraints->is_int_ == tru) {
      result.resize(1 + sizeof(long) * 2);
      ((long *)&result.data()[1])[0] = constraints->assumed_range.first;
      ((long *)&result.data()[1])[1] = constraints->assumed_range.second;
    }
    return result;
  }

  void save_constraints() {
    saved_constraints.push_back(slim::element::make_unique<constraint_store>(*constraints));
  }

  void restore_constraints() {
    constraints = std::move(saved_constraints.back());
    saved_constraints.pop_back();
  }

  std::string to_string() const {
    if (constraints->is_int() == tru) {
      std::string res;
      if (constraints->assumed_range.first == std::numeric_limits<long>::min()) res += "-inf";
      else res += std::to_string(constraints->assumed_range.first);
      res += ",";
      if (constraints->assumed_range.second == std::numeric_limits<long>::max()) res += "+inf";
      else res += std::to_string(constraints->assumed_range.second);
      return "<" + res + ">";
    }

    return "<any>";
  }
};

}  // namespace ext
}  // namespace slim

#endif /* SLIM_EXT_QUANTUM_HPP */
