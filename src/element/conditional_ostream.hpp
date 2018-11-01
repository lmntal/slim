/*
 * conditional_ostream.hpp
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

#ifndef SLIM_ELEMENT_CONDITIONAL_OSTREAM
#define SLIM_ELEMENT_CONDITIONAL_OSTREAM

#include <ostream>

namespace slim {
namespace element {

/**
 * @brief Outputs values to std::ostream conditionally.
 *
 *   Examples:
 *   @code
 *   slim::element::conditional_ostream debug_log(std::cout)
 *   debug_log << "it is printed." << std::endl;
 *   @endcode
 *
 *   @code
 *   slim::element::conditional_ostream debug_log(std::cout, false)
 *   debug_log << "it is not printed." << std::endl;
 *   @endcode
 *
 * @note When the validity of a conditional ostream can be determined at compile
 * time, the printing codes are usually omitted at runtime by compiler's
 * optimization.
 */
struct conditional_ostream {
  bool is_valid;
  std::ostream &os;

  constexpr conditional_ostream(std::ostream &stream, bool is_valid = true)
      : os(stream), is_valid(is_valid) {}

  slim::element::conditional_ostream &
  operator<<(std::ostream &(*pf)(std::ostream &)) {
    if (is_valid)
      pf(os);
    return *this;
  }
};
} // namespace element
} // namespace slim

template <typename T>
slim::element::conditional_ostream &
operator<<(slim::element::conditional_ostream &ds, const T &v) {
  if (ds.is_valid)
    ds.os << v;
  return ds;
}

#endif /* SLIM_ELEMENT_CONDITIONAL_OSTREAM */
