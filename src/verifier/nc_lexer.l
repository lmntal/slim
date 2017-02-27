/*
 * nc_lexer.l - scanner of never claim in Promela
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

%option reentrant noyywrap bison-bridge bison-locations yylineno


%{
/* need this for the call to atof() below */
#include <math.h>
#include <memory.h>
#include <string.h>
#include "automata.h"
#include "nc_parser.h"

#define _POSIX_SOURCE 1
#define YY_NO_INPUT
#define YY_NO_UNISTD_H

struct lexer_context;

%}

%option nounput

DIGIT    [0-9]

%%
 /* ルールは行の先頭から書く。ルール以外（コメントやアクション）を先頭から
    書くことはできない */

"/*"[^*/]*"*/" /* eat coment */
[ \t\n\r]+     /* eat blanks */

:                    { return COLON; }
::                   { return COLON_COLON; }
;                    { return SEMI_COLON; }
\{                   { return LBRACE; }
\}                   { return RBRACE; }
\(                   { return LPAREN; }
\)                   { return RPAREN; }
!                    { return NEGATION; }
"->"                 { return IMPLICATION; }
"<->"                { return EQUIVALENCE; }
&&                   { return AND; }
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

[a-zA-Z_][a-zA-Z0-9_]* { yylval->str = strdup(yytext); return SYMBOL; }
[0-9]* { yylval->_int = atoi(yytext); return NUMBER; }

%%
