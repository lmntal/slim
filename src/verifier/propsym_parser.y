/*
 * propsym_parser.y - propositional symbol definition parser
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id$
 */

%{
#include <math.h>
#include <stdio.h>
#include "st.h"
#include "vector.h"
#include "automata.h"
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
%}

%pure-parser
%locations
%parse-param {yyscan_t scanner}
%parse-param {Automata automata}
%parse-param {Vector **definitions}
%lex-param {yyscan_t scanner}

%union {
  int _int;
  char *str;
  struct Vector *vector;
  struct SymbolDefinition *definition;
}

%token <str> SYMBOL
%token EQUAL LINE_TERM VERT RULE_SEP;
%token <str> HEAD GUARD BODY; 
%token _EOF 0;

%start start;
%type <vector> definitions;
%type <definition> definition;

%{
#include "propsym_lexer.h"
#include "verifier/propositional_symbol.h"
static void propsymerror (YYLTYPE*, yyscan_t, Automata, Vector **, char *);
%}

%% /* Grammar rules and actions follow.  */

start: definitions _EOF { *definitions = $1; };

definitions:
/* empty */ { $$ = propsyms_make(); }
| definitions definition {
  if ($2) propsyms_set($1, propsym_symbol_id($2), $2);
  $$ = $1;
}
;

definition:
   LINE_TERM { $$ = NULL; }
|  SYMBOL EQUAL HEAD LINE_TERM {
    $$ = propsym_make(automata_propsym_to_id(automata, $1),
                      proposition_make($3, NULL, NULL));
    LMN_FREE($1);
    LMN_FREE($3);
  }
|  SYMBOL EQUAL HEAD RULE_SEP LINE_TERM {
    $$ = propsym_make(automata_propsym_to_id(automata, $1),
                      proposition_make($3, NULL, NULL));
    LMN_FREE($1);
    LMN_FREE($3);
  }
| SYMBOL EQUAL HEAD RULE_SEP GUARD LINE_TERM {
    $$ = propsym_make(automata_propsym_to_id(automata, $1),
                      proposition_make($3, $5, NULL));
    LMN_FREE($1);
    LMN_FREE($3);
    LMN_FREE($5);
  }
| SYMBOL EQUAL HEAD RULE_SEP VERT LINE_TERM {
    $$ = propsym_make(automata_propsym_to_id(automata, $1),
                      proposition_make($3, NULL, NULL));
    LMN_FREE($1);
    LMN_FREE($3);
  }
| SYMBOL EQUAL HEAD RULE_SEP GUARD VERT LINE_TERM {
    $$ = propsym_make(automata_propsym_to_id(automata, $1),
                      proposition_make($3, $5, NULL));
    LMN_FREE($1);
    LMN_FREE($3);
    LMN_FREE($5);
  }
;

%%

/* Called by yyparse on error.  */
void propsymerror (YYLTYPE *loc, yyscan_t scanner, Automata a, Vector **defs, char *s)
{
  fprintf(stderr, "propositional symbol: error %s line: %d\n", s, propsymget_lineno(scanner));
  exit(1);
}
