/*
 * value.cpp
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

#include "value.hpp"

#include "exception.hpp"

#include <cmath>
#include <cctype>
#include <limits>

// TODO:
//   一文字ずつ読むのはパフォーマンス的に問題あるかも？
//   エラー処理に入力位置を入れる（標準入力はtellgが効かない？）

namespace slim {
namespace element {
namespace json {

// for variant
namespace c17 = slim::element;

using namespace slim::element;

namespace {
constexpr bool is_sub_overflow_free(uint64_t a, uint64_t b) {
  return a >= b;
}
constexpr bool is_add_overflow_free(uint64_t a, uint64_t b) {
  return a <= std::numeric_limits<uint64_t>::max() - b;
}
constexpr bool is_div_overflow_free(uint64_t a, uint64_t b) {
  return b != 0u;
}
constexpr bool is_mul_overflow_free(uint64_t a, uint64_t b) {
  return is_div_overflow_free(std::numeric_limits<uint64_t>::max(), b) &&
         a <= std::numeric_limits<uint64_t>::max() / b;
}

// Read signed integral number with overflow check.
// throws json::overflow_error.
int64_t read_integral(std::istream &in, int &digits) {
  uint64_t integral_part = 0;
  digits = 0;

  // skip blanks
  while (in && std::isspace(in.peek()))
    in.ignore();

  int sign = (in.peek() == '-') ? -1 : 1;
  if (in.peek() == '+' || in.peek() == '-') {
    in.ignore();
  }

  // skip blanks
  while (in && std::isspace(in.peek()))
    in.ignore();

  while (in && std::isdigit(in.peek())) {
    if (!is_mul_overflow_free(integral_part, 10))
      throw json::overflow_error(in.tellg());
    integral_part *= 10;

    char c = in.get();
    if (!is_add_overflow_free(integral_part, c - '0'))
      throw json::overflow_error(in.tellg());
    integral_part += c - '0';
    digits++;
  }

  // 値がint64_tで表せる範囲を超える
  if (sign > 0 && integral_part > (uint64_t)std::numeric_limits<int64_t>::max())
    throw json::overflow_error(in.tellg());

  // std::absをint64_tの最小値に適用するとint64_tで表せる範囲を超えてしまう.
  // int64_tの負値表現が2の補数であることは規格により保証されているため、
  // ビット演算を用いて無理やり正しい絶対値を求める.
  uint64_t min_abs = (((uint64_t)std::numeric_limits<int64_t>::min() - 1) ^ (uint64_t)(~0));
  if (sign < 0 && integral_part > min_abs)
    throw json::overflow_error(in.tellg());

  return sign * (int64_t)integral_part;
}
int64_t read_integral(std::istream &in) {
  int digits;
  return read_integral(in, digits);
}
} // namespace

std::istream &operator>>(std::istream &in, value_type &value) {
  char c = '\t';
  while (in && std::isspace(c))
    in.get(c);

  if (std::isdigit(c) || c == '+' || c == '-') {
    in.unget();
    int64_t integral_part = read_integral(in);

    if (in.peek() != '.') {
      value = json::integer(integral_part);
    } else {
      in.ignore();

      // real number
      if (in.peek() == '-' || in.peek() == '+')
        throw json::syntax_error("fractional part must not be signed.", in.tellg());

      int digits;
      int64_t fractional_part = read_integral(in, digits);

      int exp = 0;
      if (in.peek() == 'e') {
        in.ignore();
        // read exponential part

        if (!std::isdigit(in.peek()) && in.peek() != '+' && in.peek() != '-')
          throw json::syntax_error("unexpected character.", in.tellg());

        exp = read_integral(in);
      }

      int sign = std::signbit(integral_part) ? -1 : 1;
      double ans = sign *
                   (std::abs((double)integral_part) +
                    (double)fractional_part * std::pow(10, -digits)) *
                   std::pow(10, exp);
      value = json::real(ans);
    }

  } else if (std::isalpha(c)) { // null, true, or false
    std::string word;
    while (in && isalpha(c)) {
      word += c;
      in.get(c);
    }

    in.unget();

    if (word == "null")
      value = json::null();
    else if (word == "true")
      value = json::boolean(true);
    else if (word == "false")
      value = json::boolean(false);
    else
      throw json::syntax_error("unexpected keyword '" + word + "'", in.tellg());

  } else if (c == '\"') {
    // TODO: escape sequence
    std::string s;
    in.get(c);
    while (c != '\"') {
      s += c;
      if (!in)
        throw json::syntax_error("unexpected end of file.", in.tellg());
      in.get(c);
    }
    value = json::string(s);
  } else if (c == '{') {
    std::unordered_map<std::string, json_t> m;
    while (true) {
      json_t key;
      in >> key;

      if (!c17::holds_alternative<json::string>(key))
        throw json::syntax_error("a key of an object must be a string.", in.tellg());

      do {
        in.get(c);
      } while (in && std::isspace(c));

      if (c != ':')
        throw json::syntax_error(
            "elements of an object must be key-value pair.", in.tellg());

      json_t value;
      in >> value;
      m[c17::get<json::string>(key)] = value;

      do {
        in.get(c);
      } while (in && std::isspace(c));

      if (c == '}')
        break;

      if (!in)
        throw json::syntax_error("unexpected end of file.", in.tellg());

      if (c != ',')
        throw json::syntax_error("expected ',' between key-value pairs.", in.tellg());
    }

    value = json::object(m);
  } else if (c == '[') {
    std::vector<json_t> v;
    while (true) {
      json_t value;
      in >> value;
      v.push_back(value);

      do {
        in.get(c);
      } while (in && std::isspace(c));

      if (c == ']')
        break;

      if (!in)
        throw json::syntax_error("unexpected end of file.", in.tellg());

      if (c != ',')
        throw json::syntax_error("expected ',' between elements.", in.tellg());
    }

    value = json::array(v);
  }

  return in;
}

struct pretty_printer {
  std::ostream &os;

  pretty_printer(std::ostream &os) : os(os) {}

  void operator()(const json::null &value) { os << "null"; }
  void operator()(const json::integer &value) { os << value.value; }
  void operator()(const json::real &value) { os << value.value; }
  void operator()(const json::string &value) {
    os << "\"" << value.value << "\"";
  } // TODO: escape sequences
  void operator()(const json::boolean &value) {
    os << (value.value ? "true" : "false");
  }
  void operator()(const json::array &value) {
    os << "[";
    for (size_t i = 0; i < value.value.size(); i++) {
      if (i > 0)
        os << ", ";
      c17::visit(*this, value.value[i]);
    }
    os << "]";
  }
  void operator()(const json::object &value) {
    os << "{";
    for (auto it = value.value.begin(); it != value.value.end(); ++it) {
      if (it != value.value.begin())
        os << ", ";
      os << "\"" << it->first << "\": ";
      c17::visit(*this, it->second);
    }
    os << "}";
  }
};

std::ostream &operator<<(std::ostream &out, const json::value_type &value) {
  c17::visit(pretty_printer(out), value);
  return out;
}

} // namespace json
} // namespace element
} // namespace slim
