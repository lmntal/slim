/*
 * il_lexer.l - Intermediate Language scanner
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: il_lex.l,v 1.5 2008/09/19 05:18:17 taisuke Exp $
 */

%option reentrant noyywrap bison-bridge bison-locations yylineno


%{
/* need this for the call to atof() below */
#include <math.h>
#include <memory.h>
#include <string.h>
#include "syntax.h"
#include "symbol.h"
#include "il_parser.h"
#include "instruction.h"
#include "rule.h"

#define _POSIX_SOURCE 1
#define YY_NO_INPUT
#define YY_NO_UNISTD_H

struct lexer_context;
int get_instr_id(char *);

int ilget_column  (yyscan_t yyscanner);
void ilset_column (int  column_no , yyscan_t yyscanner);


/* エスケープキャラクタから文字への対応表 */
static char escape_char_map[] =
  {0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, '"',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,'\\',   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,'\n',   0,
   0,   0, '\r',  0,'\t',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0};

char *unescape_c_str(const char *s);

%}

DIGIT    [0-9]

%%

 /* ルールは行の先頭から書く。ルール以外（コメントやアクション）を先頭から
    書くことはできない */
 /* Rules must NOT be indented. Others (comments and actions) must be indented. */

 /* This can handle illmatic cases such as
    'slash star star star slash' or 'slash star slash star slash'.
    cf. コンパイラの構成と最適化 第2版, pp.65-66, 中田育男, 朝倉書店, 1999 */
"/*"([^*]|"*""*"*[^*/])*"*""*"*"/" /* eat comment */
"//".*                             /* eat one line comment */
[ \t\n\r]+                         /* eat blanks */
 /* float */
-?{DIGIT}+"."{DIGIT}+ { yylval->_float = atof(yytext); return FLOAT; }
 /* integer */
-?{DIGIT}+ { yylval->_int = atoi(yytext); return INT; }

 /* ruleset id */
@{DIGIT}+ {
  /* この時点でVM内で一意のルールセットIDに変換する */
  /* Convert input text into ruleset ID unique in the VM */
  st_data_t id_local = atoi(yytext+1);
  st_data_t id_global;
  struct lexer_context *c;

  c = yyget_extra(yyscanner);
  if (!st_lookup(c->ruleset_id_tbl, id_local, &id_global)) {
    /* テーブルになければ新しく追加 */
    /* Update ruleset ID table */
    id_global = lmn_gen_ruleset_id();
    st_insert(c->ruleset_id_tbl, id_local, id_global);
  }
  yylval->_int = (int)id_global;
  return RULESET_ID; }
"null" { /* name of anonymous membrane */
         yylval->str = ANONYMOUS;
         return DQUOTED_STRING; }

 /* label */
L{DIGIT}+ { yylval->_int = atoi(yytext+1); return LABEL; }
,                    { return COMMA; }
\.                   { return PERIOD; }
:                    { return COLON; }
"_"                  { return UNDERBAR; }
\{                   { return LBRACE; }
\}                   { return RBRACE; }
\[                   { return LBRACKET; }
\]                   { return RBRACKET; }
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

 /* instruction name */
[a-z2]+ {
  yylval->_int = get_instr_id(yytext);
  /* 変数のリストと命令のリストは構文解析では判別不可能なので
　 　命令のリストを持つ中間語命令を特別に扱う */
  /* Lists of variables and lists of instructions cannot be
     parsed distinguishedly, so treat instructions accompanied by
     a list of instructions as special cases. */
  if (yylval->_int == INSTR_LOOP) { return INST_TK_LOOP; }
  if (yylval->_int == INSTR_RUN) { return INST_TK_RUN; }
  if (yylval->_int == INSTR_NOT) { return INST_TK_NOT; }
  if (yylval->_int == INSTR_GROUP) { return INST_TK_GROUP; }
  if (yylval->_int == INSTR_BRANCH) { return INST_TK_BRANCH; }

  if (yylval->_int < 0) {
    fprintf(stderr, "unknown instruction name %s\n", yytext);
    exit(EXIT_FAILURE);
  }
  return INST_NAME; }

 /* double quoted string */
\"("\\\""|[^"])*\" {
    char *t, *t2;
    t = malloc(sizeof(char)*yyleng-1);
    strncpy(t, yytext+1, yyleng-2);
    t[yyleng-2] = '\0';
    t2 = unescape_c_str(t);
    yylval->str = lmn_intern(t2);
    free(t);
    free(t2);
    return DQUOTED_STRING; }

 /* single quoted string */
'[^']*' { char *t, *t2;
          t = malloc(sizeof(char)*yyleng-1);
          strncpy(t, yytext+1, yyleng-2);
          t[yyleng-2] = '\0';
          t2 = unescape_c_str(t);
          yylval->id = lmn_intern(t2);
          free(t);
          free(t2);
          return SQUOTED_STRING; }
<<EOF>>              { yyterminate(); return _EOF; }

%%

/* 命令の名前からIDを得る */
/* gets instruction ID from instruction name */
int get_instr_id(char *name)
{
  int i;
  for (i = 0; spec[i].op_str; i++) {
    if (!strcmp(name, spec[i].op_str)) return spec[i].op;
  }
  return -1;
}

/* エスケープシーケンスを含むCの文字列を、エスケープキャラクタを実際の
   文字に変換した、新しい文字列を返す */
/* returns newly allocated string which is unescaped form of src */
char *unescape_c_str(const char *src)
{
  int len = strlen(src);
  char *s = malloc(sizeof(char) * len + 1);
  int i, j;

  for (i = 0, j = 0; i < len; i++, j++) {
    char c;
    if (i < len - 1 && src[i] == '\\' && escape_char_map[(int)src[i+1]]) {
      c = escape_char_map[(int)src[i+1]];
      i++;
    } else {
      c = src[i];
    }
    s[j] = c;
  }
  s[j] = '\0';
  return s;
}
