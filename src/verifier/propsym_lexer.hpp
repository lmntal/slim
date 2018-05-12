/*
 * propsym_lexer.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 *
 */

#ifndef PROPSYM_LEXER_HPP
#define PROPSYM_LEXER_HPP

#include <cstdio>
#include <string>

namespace propsym {
class lexer;
}

#include "lmntal.h"
extern "C" {
#include "automata.h"
}
#include "propsym_parser.hpp"

namespace propsym {
class lexer {
  static const size_t SIZE = 256;
  FILE *input;
  char *buf;
  char *YYCURSOR;
  char *token;
  char *YYLIMIT;
  bool eof;
  int lineno_ = 0;
  int c;

  std::string get_token() const {
    return std::string(token, YYCURSOR - token);
  };

public:
  lexer(FILE *in);

  int lineno() const { return lineno_; }
  int lex(YYSTYPE *yylval, YYLTYPE *yyloc);
  bool fill(size_t need);
};
} // namespace propsym

int propsymlex(YYSTYPE *yylval, YYLTYPE *yyloc, propsym::lexer *lexer);

#endif /* NC_LEXER_HPP */
