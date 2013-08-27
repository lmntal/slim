/*
 * il_parser.y - parse Intermediate Language
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
 * $Id: il_parser.y,v 1.9 2008/09/29 05:23:40 taisuke Exp $
 */

%{
#include <math.h>
#include <stdio.h>
#include "syntax.h"
#include "st.h"
#include "symbol.h"
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

%}

%pure-parser
%locations
%parse-param {yyscan_t scanner}
%parse-param {IL* ret_il}
%parse-param {Rule* ret_rule}
/* ローカルのルールセットIDからグローバルなIDへのテーブル */
%lex-param {yyscan_t scanner}

%union {              /* define stack type */
  double _float;
  int _int;
  lmn_interned_str id;
  lmn_interned_str str;
  RuleSet ruleset;
  IL il;
  RuleSets rulesets;
  RuleList rules;
  Rule rule;
  InstBlock inst_block;
  InstList instructions;
  Instruction instruction;
  InstrArg arg;
  VarList inst_vars;
  Functor functor;
  ModuleList module_list;
  ArgList args;
  InlineList inline_list;
}

%token <_int> INT
%token <_float> FLOAT
%token <id> ID
%token <_int> INST_NAME
%token <_int> LABEL
%token <str> DQUOTED_STRING
%token <str> SQUOTED_STRING
%token <_int> RULESET_ID
%token COMMA PERIOD COLON UNDERBAR LBRACE RBRACE LBRACKET RBRACKET
%token INSIDE_PROXY OUTSIDE_PROXY
%token KW_COMPILED_RULESET KW_COMPILED_SYSTEM_RULESET KW_COMPILED_UNIQ_RULE KW_COMPILED_RULE KW_ATOMMATCH KW_MEMMATCH
%token KW_GUARD KW_BODY KW_INLINE KW_MODULE

%token <_int> INST_TK_LOOP
%token <_int> INST_TK_RUN
%token <_int> INST_TK_NOT
%token <_int> INST_TK_GROUP
%token <_int> INST_TK_BRANCH

%token _EOF 0

%start start
%type <il> world
%type <module_list> module_def;
%type <module_list> modules;
%type <rulesets> ruleSetList
%type <ruleset> ruleSet
%type <rule> rule
%type <inst_block> atommatch
%type <inst_block> memmatch
%type <inst_block> guard
%type <inst_block> body
%type <instruction> instruction
%type <instructions> instructions
%type <rules> ruleList
%type <_int> label
%type <arg> arg
%type <functor> functor
%type <inst_vars> var_list;
%type <inst_vars> var_list_;
%type <arg> var_list_arg;
%type <args> args
%type <args> args_
%type <args> inst_list_arg
%type <inline_list> inlines;

%{
#include "il_lexer.h"
void ilerror (YYLTYPE*, yyscan_t, IL *, Rule *, char *);

%}

%% /* Grammar rules and actions follow.  */

start:
  world _EOF { *ret_il = $1; }   /* 中間言語全体のパース */
| rule { *ret_rule = $1; }  /* ルールのみの入力をパース */
;

world:
  ruleSetList { $$ = il_make($1, module_list_make(), inline_list_make()); }
| ruleSetList module_def { $$ = il_make($1, $2, inline_list_make()); }
| ruleSetList module_def KW_INLINE inlines { $$ = il_make($1, $2, $4); }
| ruleSetList KW_INLINE inlines { $$ = il_make($1, module_list_make(), $3); }
;

module_def:
KW_MODULE modules { $$ = $2; }
;

modules:
 /*empty*/ { $$ = module_list_make(); }
| modules SQUOTED_STRING LBRACE  RBRACE { $$ = $1; } /* この場合もある */
| modules SQUOTED_STRING LBRACE RULESET_ID RBRACE {
  module_list_push($1, module_make($2, $4));
  $$ = $1;
}
;

inlines:
  /*empty*/ { $$ = inline_list_make(); }
| inlines DQUOTED_STRING { inline_list_push($1, $2); $$ = $1; }
;

ruleSetList:
ruleSet { $$ = rulesets_make(); rulesets_push($$, $1); }
| ruleSetList ruleSet { rulesets_push($1, $2); $$ = $1; }
;

ruleSet:
  KW_COMPILED_SYSTEM_RULESET RULESET_ID ruleList { $$ = ruleset_make($2, $3, TRUE); }
| KW_COMPILED_RULESET RULESET_ID ruleList { $$ = ruleset_make($2, $3, FALSE); }
;

ruleList:
  rule { $$ = rulelist_make(); rulelist_push($$, $1); }
| ruleList rule { rulelist_push($1, $2); $$ = $1; }
;

rule:
  KW_COMPILED_UNIQ_RULE atommatch memmatch guard body {
    $$ = rule_make_anonymous(TRUE, $2, $3, $4, $5);
  }
| KW_COMPILED_RULE atommatch memmatch guard body {
    $$ = rule_make_anonymous(FALSE, $2, $3, $4, $5);
  }
;

atommatch:
KW_ATOMMATCH COLON instructions { $$ = inst_block_make_without_label($3); }
;

memmatch:
  KW_MEMMATCH COLON instructions { $$ = inst_block_make_without_label($3); }
;

guard:
/* empty */ { $$ = inst_block_make_without_label(inst_list_make()); }
|  KW_GUARD COLON label COLON instructions { $$ = inst_block_make($3, $5); }
;

body:
/* empty */ { $$ = inst_block_make_without_label(inst_list_make()); }
|  KW_BODY COLON label COLON instructions { $$ = inst_block_make($3, $5); }
;

label:
  LABEL { $$ = $1; }
;

instructions:
  /* empty */ { $$ = inst_list_make(); }
| instructions instruction { inst_list_push($1, $2); $$ = $1; }
;

 /* COMMITを特別にinst_makeで処理しているので注意 */
instruction:
 INST_NAME LBRACKET args RBRACKET {
  $$ = inst_make($1, $3);
}
  /* 変数のリストと命令のリストがreduce/reduce コンフリクトを起こすので別に扱う */
|  INST_TK_LOOP LBRACKET inst_list_arg RBRACKET { $$ = inst_make($1, $3); }
|  INST_TK_RUN LBRACKET inst_list_arg RBRACKET { $$ = inst_make($1, $3); }
|  INST_TK_NOT LBRACKET inst_list_arg RBRACKET { $$ = inst_make($1, $3); }
|  INST_TK_GROUP LBRACKET inst_list_arg RBRACKET { $$ = inst_make($1, $3); }
|  INST_TK_BRANCH LBRACKET inst_list_arg RBRACKET { $$ = inst_make($1, $3); }
;

arg:
  INT { $$ = instr_var_arg_make($1); }
| functor { $$ = functor_arg_make($1); }
| RULESET_ID { $$ = ruleset_arg_make($1); }
| DQUOTED_STRING { $$ = string_arg_make($1); }
| label { $$ = label_arg_make($1); }
| var_list_arg { $$ = $1; }
;

args:
/*empty*/ { $$ = arg_list_make(); }
// | args_ arg { arg_list_push($1, $2); $$ = $$ = $1; }
| args_ arg { arg_list_push($1, $2); $$ = $1; }
;

args_:
/*empty*/ { $$ = arg_list_make(); }
| args_ arg COMMA { arg_list_push($1, $2) ; $$ = $1; }
;

var_list_arg:
LBRACKET var_list RBRACKET { $$ = var_list_arg_make($2); }
;

var_list:
/*empty*/  { $$ = var_list_make(); }
| var_list_ INT { var_list_push($1, instr_var_arg_make($2)); $$ = $1; }
| var_list_ functor { var_list_push($1, functor_arg_make($2)); $$ = $1; }
;

var_list_:
/*empty*/ { $$ = var_list_make(); }
| var_list_ INT COMMA { var_list_push($1, instr_var_arg_make($2)); $$ = $1; }
| var_list_ functor COMMA { var_list_push($1, functor_arg_make($2)); $$ = $1; }
;

inst_list_arg:
LBRACKET instructions RBRACKET {
  ArgList l = arg_list_make();
  arg_list_push(l, inst_list_arg_make($2));
  $$ = l;
}
;

functor:
  INSIDE_PROXY { $$ = functor_make(STX_IN_PROXY); }
| OUTSIDE_PROXY { $$ = functor_make(STX_OUT_PROXY); }
| SQUOTED_STRING UNDERBAR INT {$$ = symbol_functor_make($1, $3); }
| DQUOTED_STRING UNDERBAR INT {$$ = string_functor_make($1); }
| SQUOTED_STRING PERIOD SQUOTED_STRING UNDERBAR INT {
    $$ = module_symbol_functor_make($1, $3, $5);
  }
| INT UNDERBAR INT {
    $$ = int_functor_make($1);
  }
| FLOAT UNDERBAR INT {
    $$ = float_functor_make($1);
  }
;


%%

#include "st.h"

/* Called by yyparse on error.  */
void ilerror (YYLTYPE *loc, yyscan_t scanner, IL *il, Rule *rule, char *s)
{
  fprintf (stderr, "il parser: error %s line: %d\n", s, ilget_lineno(scanner));
  exit(1);
}
