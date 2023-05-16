/*
 * util.h - common utility functions and macros
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id$
 */

#ifndef LMN_UTIL_H
#define LMN_UTIL_H

#include "element/error.h"
#include "lmntal.h"

/**
 * @ingroup  Element
 * @defgroup Util
 * @{
 */

/** ----------------------
 *  byte operation
 */

/* See http://isthe.com/chongo/tech/comp/fnv/ */

namespace fnv {
template <size_t size = SIZEOF_LONG> constexpr unsigned long prime();

template <> constexpr unsigned long prime<4>() { return 16777619UL; };
template <> constexpr unsigned long prime<8>() { return 1099511628211UL; };

template <size_t size = SIZEOF_LONG> constexpr unsigned long basis();
template <> constexpr unsigned long                          basis<4>() { return 2166136261UL; }
template <> constexpr unsigned long                          basis<8>() { return 14695981039346656037UL; }

inline constexpr auto hash(unsigned char const *str, long i) {
  /*
   * FNV-1a hash each octet in the buffer
   */
  auto hval = fnv::basis();
  while (--i >= 0) {
    /* xor the bottom with the current octet */
    hval ^= (unsigned int)str[i];
    /* multiply by the FNV magic prime mod 2^32 or 2^64 */
    hval *= fnv::prime();
  }

  return hval;
}
} // namespace fnv

static inline unsigned long lmn_byte_hash(unsigned char const *str, long i) { return fnv::hash(str, i); }

/* 正: a ＞ b
 * ０: a = b
 * 負: a ＜ b */
static inline int lmn_byte_cmp(unsigned char const *a, long alen, unsigned char const *b, long blen) {
  if (alen != blen) {
    return static_cast<int>(alen - blen);
  }
  return memcmp(a, b, alen);
}

/** ----------------------
 *  else
 */

/* 配列の要素数 */
#define ARY_SIZEOF(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))

char *int_to_str(long n);

#include <iterator>
#include <memory>

namespace slim::element {
template <class T> class raw_pointer_iterator {
  T *p;

public:
  using iterator_category = std::input_iterator_tag;
  using value_type        = T;
  using difference_type   = std::ptrdiff_t;
  using pointer           = T *;
  using reference         = T &;

  raw_pointer_iterator(T *ptr) : p(ptr) {}
  raw_pointer_iterator(raw_pointer_iterator<T> const &it) : p(it.p) {}
  raw_pointer_iterator &operator=(raw_pointer_iterator<T> const &it) {
    if (this != &it) {
      p = it.p;
    }
  }
  ~raw_pointer_iterator() noexcept = default;

  reference                operator*() const { return *p; }
  pointer                  operator->() const { return p; }
  raw_pointer_iterator<T> &operator++() {
    p++;
    return *this;
  }
  raw_pointer_iterator<T> operator++(int i) {
    auto it = *this;
    ++(*this);
    return it;
  }

  bool operator!=(raw_pointer_iterator<T> const &a) { return !(*this == a); }
  bool operator==(raw_pointer_iterator<T> const &a) { return p == a.p; }
};
} // namespace slim::element

/* @} */

#endif /* !LMN_UTIL_H */
