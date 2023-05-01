/*
 * optional.h
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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

#ifndef SLIM_ELEMENT_OPTIONAL_HPP
#define SLIM_ELEMENT_OPTIONAL_HPP

#include <utility>

namespace slim {
namespace element {

template <typename T> class optional {
  bool empty;
  T    value_;

public:
  optional() : empty(true) {}
  optional(T const &value_) : empty(false), value_(value_) {}
  optional(T &&value_) : empty(false), value_(std::move(value_)) {}
  optional(optional const &other) : empty(false), value_(*other) {}
  optional(optional &&other) : empty(false), value_(*std::move(other)) {}

  ~optional() {
    if (has_value()) {
      value_.~T();
    }
  }

  optional &operator=(optional const &rhs) {
    reset();
    empty = rhs.empty;
    if (empty)
      value_ = rhs.value_;
    return *this;
  }
  optional &operator=(optional &&rhs) noexcept {
    reset();
    empty = rhs.empty;
    if (empty)
      value_ = std::move(rhs.value_);
    return *this;
  }

  template <class U = T> optional &operator=(U &&rhs) {
    reset();
    value_ = std::move(rhs);
    empty  = false;
    return *this;
  }

  template <class U> optional &operator=(optional<U> const &rhs) {
    reset();
    empty = rhs.empty;
    if (empty)
      value_ = rhs.value_;
    return *this;
  }

  template <class U> optional &operator=(optional<U> &&rhs) {
    reset();
    empty = rhs.empty;
    if (empty)
      value_ = std::move(rhs.value_);
    return *this;
  }

  void reset() noexcept {
    value_.~T();
    empty = true;
  }

  operator bool() const { return !empty; }

  bool has_value() const noexcept { return !empty; }

  T const  &value() const  &{ return value_; }
  T        &value()        &{ return value_; }
  T       &&value()       &&{ return std::move(value_); }
  T const &&value() const && { return std::move(value_); }

  template <class U> constexpr T value_or(U &&default_value) const & {
    return bool(*this) ? **this : static_cast<T>(std::forward<U>(default_value));
  }
  template <class U> constexpr T value_or(U &&default_value) const && {
    return bool(*this) ? std::move(**this) : static_cast<T>(std::forward<U>(default_value));
  }

  T const  &operator*() const  &{ return value_; }
  T        &operator*()        &{ return value_; }
  T       &&operator*()       &&{ return std::move(value_); }
  T const &&operator*() const && { return std::move(value_); }

  T const *operator->() const { return &value_; }
  T       *operator->() { return &value_; }
};

} // namespace element
} // namespace slim

template <typename T, typename U> bool operator<(const slim::element::optional<T> opt, U const &value) {
  return (opt) ? (*opt < value) : true;
}

#endif /* SLIM_ELEMENT_OPTIONAL_HPP */
