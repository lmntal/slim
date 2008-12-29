/*
 * propsym_lexer.l - propositional symbol definition lexer
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
#include "propsym_parser.h"

#define _POSIX_SOURCE 1
/* #define YY_NO_INPUT */
#define YY_NO_UNISTD_H

int get_instr_id(char *);

int symdefget_column  (yyscan_t yyscanner);
void symdefset_column (int  column_no , yyscan_t yyscanner);
int is_head = 0, is_guard=0;

%}

DIGIT    [0-9]

%%
 /* ルールは行の先頭から書く。ルール以外（コメントやアクション）を先頭から
    書くことはできない */

"/*"[^*/]*"*/" /* eat coment */
"//".*         /* eat one line comment */
[ \t]+     /* eat blanks */
[\n\r]+     { is_head = is_guard=0; return LINE_TERM; }

<<EOF>>              { yyterminate(); return _EOF; }
"|" { fflush(stdout); return VERT; }
":-" { fflush(stdout); is_guard = 1; return RULE_SEP; }
= { is_head = 1; return EQUAL; }
. {
     int c, depth;
     char *buf;
     int buf_size, i;

     if (!(is_head || is_guard)) REJECT;
     depth = 0;
     buf_size = 256;
     buf = malloc(sizeof(char) * buf_size);
     i = 0;

     for (c = yytext[0];; c = input(yyscanner)) {
        /* extend buffer */
       if (i >= buf_size) {
         buf_size *= 2;
         buf = realloc(buf, buf_size);
       }

       if (c == '\n' || c == '\r') {
         unput(c);
         break;
       }
       if (c == '(') depth++;
       if (c == ')') depth--;

       if (is_head && c == ':' && depth == 0) {
         unput(':');
         break;
       }

       if (is_guard && c == '|' && depth == 0) {
         unput('|');
         break;
       }

       buf[i++] = c;
     }
     buf[i] = '\0';

     yylval->str = buf;
     if (is_head) { is_head = 0; return HEAD; }
     if (is_guard) { is_guard = 0; return GUARD; }
  }
[a-z][a-zA-Z0-9_]*   {
    if ((is_head || is_guard)) REJECT; /* これがないとなぜかうまく動かない */
    yylval->str = strdup(yytext); return SYMBOL;
  }
. { fprintf(stderr, "unexpected character: '%s'", yytext); exit(1); }

%%
