/*
 * string.h - String API
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id$
 */

#ifndef LMN_STRING_H
#define LMN_STRING_H

#include "lmntal.h"
#include "util.h"

#include <string>

typedef struct LmnString *LmnStringRef;
struct LmnString : LmnSPAtomHeader {
  static int string_atom_type;
  std::string str;

  LmnString() : LmnSPAtomHeader(string_atom_type) {}
  LmnString(const char *s) : str(s), LmnSPAtomHeader(string_atom_type) {}
  LmnString(const std::string s) : str(s), LmnSPAtomHeader(string_atom_type) {}

  const char *c_str() const { return str.c_str(); }
  unsigned long hash() const {
    return lmn_byte_hash((const unsigned char *)str.c_str(), str.size() + 1);
  }
  template <typename T> void push_back(T &&s) {
    str.push_back(std::forward<T>(s));
  }
  template <typename T> void append(T &&s) { str.append(std::forward<T>(s)); }

  template <typename T>
  auto operator[](T &&idx) const -> decltype(str[std::forward<T>(idx)]) {
    return str[std::forward<T>(idx)];
  }

  size_t size() const { return str.size(); }

  friend bool operator==(const LmnString &s1, const LmnString &s2);
};

inline bool operator==(const LmnString &s1, const LmnString &s2) {
  return s1.str == s2.str;
}
inline bool operator!=(const LmnString &s1, const LmnString &s2) {
  return !(s1 == s2);
}

void string_init(void);
void string_finalize(void);

#endif
