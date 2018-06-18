/*
 * il_lexer.cpp.re - Intermediate Language scanner
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

#include "il_lexer.hpp"

#include <fstream>

#include "lmntal.h"
#include "syntax.hpp"
#include "il_parser.hpp"
#include "exception.hpp"

/* エスケープキャラクタから文字への対応表 */
static char escape_char_map[] = {
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, '"', 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, '\\', 0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    '\n', 0, 0, 0, '\r', 0, '\t', 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,
    0,    0, 0, 0, 0,    0, 0,    0, 0, 0, 0, 0, 0,   0};

/* エスケープシーケンスを含むCの文字列を、エスケープキャラクタを実際の
   文字に変換した、新しい文字列を返す */
/* returns newly allocated string which is unescaped form of src */
std::string unescape_c_str(const std::string &src) {
  std::string s;

  for (int i = 0; i < src.length(); i++) {
    char c;
    if (i < src.length() - 1 && src[i] == '\\' && escape_char_map[(int)src[i + 1]]) {
      c = escape_char_map[(int)src[i + 1]];
      i++;
    } else {
      c = src[i];
    }
    s.push_back(c);
  }
  return s;
}

namespace il {
using namespace std;
using namespace slim::element::re2c;
using file_ptr = std::unique_ptr<FILE, decltype(&fclose)>;
/*!max:re2c*/

lexer::lexer(file_ptr in) : ruleset_id_tbl(st_init_numtable()) {
  buffer = std::unique_ptr<cfstream_buffer>(
      new cfstream_buffer(std::move(in), YYMAXFILL, SIZE));
}

lexer::lexer(const std::string &file_path)
    : ruleset_id_tbl(st_init_numtable()) {
  buffer =
      std::unique_ptr<file_buffer>(new file_buffer(file_path, YYMAXFILL, SIZE));
}

lexer::~lexer() { st_free_table(ruleset_id_tbl); }

std::string lexer::get_token() const {
  return std::string(buffer->parsed_pos, buffer->YYCURSOR - buffer->parsed_pos);
};

int lexer::lex(YYSTYPE *yylval, YYLTYPE *yyloc) {
start:
  char *YYMARKER;
  buffer->parsed_pos = buffer->YYCURSOR;
  /*!re2c
    re2c:define:YYCTYPE = char;
    re2c:define:YYLIMIT = 'buffer->YYLIMIT';
    re2c:define:YYCURSOR = 'buffer->YYCURSOR';
    re2c:define:YYFILL@len = #;
    re2c:define:YYFILL = "if (!buffer->fill(#)) return _EOF;";
    re2c:define:YYFILL:naked = 1;

    digit = [0-9];
    blank = [ \t\n\r];

    sstr = "'"  [^']* "'";
    dstr = "\"" [^"]* "\"";

    "\x00" { return _EOF; }
    blank+ { goto start; }

    '-'?digit+ {
      string s = get_token();
      yylval->_int = s.empty() ? 0 : stol(s);
      return INT;
    }
    '-'?digit+"."digit+ {
      yylval->_float = stod(get_token());
      return FLOAT;
    }
    '@' digit+ {
      // この時点でVM内で一意のルールセットIDに変換する
      // Convert input text into ruleset ID unique in the VM
      st_data_t id_local = stol(get_token().substr(1));
      st_data_t id_global;

      if (!st_lookup(ruleset_id_tbl, id_local, &id_global)) {
        id_global = lmn_gen_ruleset_id();
        st_insert(ruleset_id_tbl, id_local, id_global);
      }
      yylval->_int = (int)id_global;
      return RULESET_ID;
    }

    "null" { // name of anonymous membrane
       yylval->str = ANONYMOUS;
       return DQUOTED_STRING;
     }
    'L'digit+ {
      yylval->_int = stol(get_token().substr(1));
      return LABEL;
    }
    ','                    { return COMMA; }
    '\.'                   { return PERIOD; }
    ':'                    { return COLON; }
    "_"                  { return UNDERBAR; }
    '\{'                   { return LBRACE; }
    '\}'                   { return RBRACE; }
    '\['                   { return LBRACKET; }
    '\]'                   { return RBRACKET; }
    "$in_2"              { return INSIDE_PROXY; }
    "$out_2"             { return OUTSIDE_PROXY; }
    "Compiled SystemRuleset"   { return KW_COMPILED_SYSTEM_RULESET; }
    "Compiled Ruleset"   { return KW_COMPILED_RULESET; }
    "Compiled Uniq Rule" { return KW_COMPILED_UNIQ_RULE; }
    "Compiled Rule"      { return KW_COMPILED_RULE; }
    "--atommatch"        { return KW_ATOMMATCH; }
    "--memmatch"         { return KW_MEMMATCH; }
    "--guard"            { return KW_GUARD; }
    "--body"             { return KW_BODY; }
    "Inline"             { return KW_INLINE; }
    "Module"             { return KW_MODULE; }

    [a-z2]+ {
      yylval->_int = get_instr_id(get_token().c_str());
      // 変数のリストと命令のリストは構文解析では判別不可能なので
      // 命令のリストを持つ中間語命令を特別に扱う
      // Lists of variables and lists of instructions cannot be
      // parsed distinguishedly, so treat instructions accompanied by
      // a list of instructions as special cases.
      if (yylval->_int == INSTR_LOOP) { return INST_TK_LOOP; }
      if (yylval->_int == INSTR_RUN) { return INST_TK_RUN; }
      if (yylval->_int == INSTR_NOT) { return INST_TK_NOT; }
      if (yylval->_int == INSTR_GROUP) { return INST_TK_GROUP; }
      if (yylval->_int == INSTR_BRANCH) { return INST_TK_BRANCH; }

      if (yylval->_int < 0) {
        fprintf(stderr, "unknown instruction name %s\n", get_token().c_str());
        exit(EXIT_FAILURE);
      }
      return INST_NAME;
    }

    dstr {
      auto t = get_token();
      auto t2 = unescape_c_str(t.substr(1, t.size() - 2));
      yylval->str = lmn_intern(t2.c_str());
      return DQUOTED_STRING;
    }

    sstr {
      auto t = get_token();
      auto t2 = unescape_c_str(t.substr(1, t.size() - 2));
      yylval->str = lmn_intern(t2.c_str());
      return SQUOTED_STRING;
    }
  */
  }
}

int illex(YYSTYPE *yylval, YYLTYPE *yyloc, il::lexer *lexer) {
  return lexer->lex(yylval, yyloc);
}
