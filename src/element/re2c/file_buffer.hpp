/*
 * file_buffer.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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

#ifndef ELEMENT_RE2C_FILE_BUFFER_HPP
#define ELEMENT_RE2C_FILE_BUFFER_HPP

#include <cstddef>
#include <fstream>
#include <memory>

#include "../exception.hpp"
#include "buffer.hpp"

namespace slim {
namespace element {
namespace re2c {
class file_buffer : public buffer {
  std::unique_ptr<std::ifstream> ifs;

public:
  file_buffer(const std::string &file_path, int fill_size, int size = 256)
      : buffer(fill_size, size), ifs(std::unique_ptr<std::ifstream>(new std::ifstream(file_path))) {
    if (ifs->fail())
      throw exception("cannot open file '" + file_path + "'");
  }

  file_buffer(std::unique_ptr<std::ifstream> &&ifs, int fill_size, int size = 256)
      : buffer(fill_size, size), ifs(std::move(ifs)) {}

  bool is_finished() const { return ifs->eof(); }

  void update_limit(size_t free) {
    ifs->read(YYLIMIT, free);
    YYLIMIT += ifs->gcount();
  }
};
} // namespace re2c
} // namespace element
} // namespace slim

#endif /* ELEMENT_RE2C_FILE_BUFFER_HPP */
