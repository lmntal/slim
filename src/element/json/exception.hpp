/*
 * exception.hpp
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

#ifndef SLIM_ELEMENT_JSON_EXCEPTION_HPP
#define SLIM_ELEMENT_JSON_EXCEPTION_HPP

#include <istream>
#include <stdexcept>
#include <string>

namespace slim {
namespace element {
namespace json {
struct parse_error {
  virtual const char *what() const noexcept = 0;
};

struct overflow_error : parse_error, std::overflow_error {
  overflow_error(std::istream::pos_type pos)
      : std::overflow_error("occurred at " + std::to_string(pos)) {}
  const char *what() const noexcept { return std::overflow_error::what(); }
};
struct syntax_error : parse_error, std::runtime_error {
  syntax_error(const std::string &what_arg, std::istream::pos_type pos)
      : std::runtime_error(what_arg + "(at " + std::to_string(pos) + ")") {}
  const char *what() const noexcept { return std::runtime_error::what(); }
};

} // namespace json
} // namespace element
} // namespace slim

#endif /* SLIM_ELEMENT_JSON_EXCEPTION_HPP */
