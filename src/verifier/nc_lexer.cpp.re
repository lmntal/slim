/*
 * nc_lexer.cpp.re
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 */

#include "nc_lexer.hpp"

#include <iostream>

#include "lmntal.h"
#include "nc_parser.hpp"

namespace nc {
  using namespace std;

  /*!max:re2c*/

  lexer::lexer(FILE *in) :
      input(in), eof(false) {
    buf = new char[YYMAXFILL + SIZE];
    YYLIMIT = buf + SIZE;
    YYCURSOR = YYLIMIT;
    token = YYLIMIT;
  }

  int lexer::lex(YYSTYPE *yylval, YYLTYPE *yyloc) {
  start:
    char *YYMARKER;
    token = YYCURSOR;
    /*!re2c
      re2c:define:YYCTYPE = char;
      re2c:define:YYFILL@len = #;
      re2c:define:YYFILL = "if (!fill(#)) return _EOF;";
      re2c:define:YYFILL:naked = 1;

      digit = [0-9];
      blank = [ \t];

      "\x00" { return _EOF; }
      "/*"[^*/]*"*/" { goto start; }
      blank { goto start; }
      [\n\r] { lineno_++; goto start; }

      ":"                    { return COLON; }
      "::"                   { return COLON_COLON; }
      ";"                    { return SEMI_COLON; }
      "\{"                   { return LBRACE; }
      "\}"                   { return RBRACE; }
      "\("                   { return LPAREN; }
      "\)"                   { return RPAREN; }
      "!"                    { return NEGATION; }
      "->"                 { return IMPLICATION; }
      "<->"                { return EQUIVALENCE; }
      "&&"                   { return AND; }
      "||"                 { return OR; }
      "[]"                 { return ALWAYS; }
      "<>"                 { return EVENTUALLY; }
      "U"                  { return UNTIL; }
      "V"                  { return RELEASE; }
      "X"                  { return NEXT; }
      "skip"               { return KW_SKIP; }
      "goto"               { return KW_GOTO; }
      "if"                 { return KW_IF; }
      "fi"                 { return KW_FI; }
      "true"               { return KW_TRUE; }
      "false"              { return KW_FALSE; }

      [a-zA-Z_][a-zA-Z0-9_]* { yylval->str = strdup(get_token().c_str()); return SYMBOL; }
      [0-9]* {
        std::string s = get_token();
        yylval->_int = s.empty() ? 0 : stol(s);
        return NUMBER;
      }

    */
  }

  bool lexer::fill(size_t need) {
    if (eof) return false;
    const size_t free = token - buf;
    if (free < need) return false;
    memmove(buf, token, YYLIMIT - token);
    YYLIMIT -= free;
    YYCURSOR -= free;
    token -= free;
    YYLIMIT += fread(YYLIMIT, 1, free, input);
    if (YYLIMIT < buf + SIZE) {
        eof = true;
        memset(YYLIMIT, 0, YYMAXFILL);
        YYLIMIT += YYMAXFILL;
    }
    return true;
  }
}

int nclex(YYSTYPE *yylval, YYLTYPE *yyloc, nc::lexer *lexer) {
  return lexer->lex(yylval, yyloc);
}
