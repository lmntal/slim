/*
 * conv.hpp
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

#ifndef SLIM_ELEMENT_JSON_CONV_HPP
#define SLIM_ELEMENT_JSON_CONV_HPP

#include "value.hpp"

namespace slim {
namespace element {

/**
 * Convert to JSON value.
 *
 * Allowed inputs are integers, floating points, booleans, std::string,
 * std::vector, std::map, and std::unordered_map.
 *
 * Usage:
 *   json::value(42)  -->  42
 *   std::vector<int> v = {1, 2, 3};
 *   json::value(v)   -->  [1, 2, 3]
 *
 * If you want to convert user-defined types, all you have to do is to define
 * 'to_json' function.
 *
 * Example:
 *   using namespace slim::element;
 *   struct Point { int x, y; };
 *   json_t to_json(const Point &p) {
 *     return json::to_json(std::vector<int>({p.x, p.y}));
 *   }
 */
template <class T, typename std::enable_if<std::is_integral<T>::value &&
                                               !std::is_same<T, bool>::value,
                                           std::nullptr_t>::type = nullptr>
inline json_t to_json(const T &v) {
  return json::integer(v);
}
template <class T, typename std::enable_if<std::is_floating_point<T>::value,
                                           std::nullptr_t>::type = nullptr>
inline json_t to_json(const T &v) {
  return json::real(v);
}
template <class T, typename std::enable_if<std::is_same<T, bool>::value,
                                           std::nullptr_t>::type = nullptr>
inline json_t to_json(T v) {
  return json::boolean(v);
}
inline json_t to_json(const std::string &v) { return json::string(v); }
template <class T> inline json_t to_json(const std::vector<T> &v) {
  json::value_ptr<json::array> ary(new json::array);
  ary->value.reserve(v.size());
  for (auto &u : v)
    ary->value.push_back(to_json(u));
  return ary;
}
template <class T>
inline json_t to_json(const std::unordered_map<std::string, T> &v) {
  json::value_ptr<json::object> res(new json::object);
  for (auto &p : v) {
    res->value[p.first] = to_json(p.second);
  }
  return res;
}
template <class T> inline json_t to_json(const std::map<std::string, T> &v) {
  json::value_ptr<json::object> res(new json::object);
  for (auto &p : v) {
    res->value[p.first] = to_json(p.second);
  }
  return res;
}
inline json_t to_json(const json_t &v) { return v; }
inline json_t to_json(json_t &&v) { return std::move(v); }

} // namespace element
} // namespace slim

#endif /* SLIM_ELEMENT_JSON_CONV_HPP */
