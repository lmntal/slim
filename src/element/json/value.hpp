/*
 * value.hpp
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
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

#ifndef SLIM_ELEMENT_JSON_VALUE_HPP
#define SLIM_ELEMENT_JSON_VALUE_HPP

#include "../variant.hpp"

#include <cstdint>
#include <istream>
#include <map>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <iostream>

namespace slim {
namespace element {
namespace json {

namespace c17 = ::slim::element;

struct null;
struct integer;
struct real;
struct string;
struct boolean;
struct array;
struct object;

using value_type =
    c17::variant<null, integer, real, string, boolean, array, object>;

struct null {
  operator std::nullptr_t() { return nullptr; }
};

struct integer {
  int64_t value;

  // 数値型は暗黙の型変換を行わない
  explicit constexpr integer(int64_t v) : value(v) {}
  operator int64_t() { return value; }
};
struct real {
  double value;

  // 数値型は暗黙の型変換は行わない
  explicit constexpr real(double v) : value(v) {}
  operator double() { return value; }
};
struct boolean {
  bool value;

  // 数値型は暗黙の型変換は行わない
  constexpr explicit boolean(bool tf) : value(tf) {}
  operator bool() { return value; }
};
struct string {
  std::string value;

  string(const std::string &str) : value(str) {}
  operator std::string() { return value; }
};
struct array {
  std::vector<value_type> value;

  array() {}
  array(const std::vector<value_type> &v) : value(v) {}
  operator std::vector<value_type>() { return value; }
};
struct object {
  std::unordered_map<std::string, value_type> value;

  object() {}
  object(const std::unordered_map<std::string, value_type> &v) : value(v) {}
  operator std::unordered_map<std::string, value_type>() { return value; }
};

// throws json::syntax_error, json::overflow_error
std::istream &operator>>(std::istream &in, value_type &value);
std::ostream &operator<<(std::ostream &out, const value_type &value);

} // namespace json

using json_t = json::value_type;

} // namespace element
} // namespace slim

#endif /* SLIM_ELEMENT_JSON_VALUE_HPP */
