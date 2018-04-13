/*
 * syntax.h - syntax tree  of the Intermediate Language
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
 * $Id: syntax.h,v 1.4 2008/09/29 05:23:40 taisuke Exp $
 */

#ifndef LMN_SYNTAX_H
#define LMN_SYNTAX_H

#include "lmntal.h"
#include "vm/vm.h"
#include "element/element.h"

/**
 * @ingroup  Loader
 * @defgroup Syntax
 * @{
 */

/* 型名の解決の為に上に持ってきた */
typedef struct __InstList *InstList;

typedef struct InstrArg *InstrArgRef;


/* List of instrction variables */
typedef struct __VarList *VarList;

VarList var_list_make(void);
void var_list_push(VarList l, InstrArgRef n);

/* Functor */

enum FunctorType {STX_SYMBOL, INT_FUNC, FLOAT_FUNC, STRING_FUNC, STX_IN_PROXY, STX_OUT_PROXY, STX_UNIFY};

typedef struct Functor *FunctorRef;

FunctorRef functor_make(enum FunctorType type);
FunctorRef int_functor_make(long v);
FunctorRef float_functor_make(double v);
FunctorRef symbol_functor_make(lmn_interned_str name, int arity);
FunctorRef string_functor_make(lmn_interned_str name);
FunctorRef module_symbol_functor_make(lmn_interned_str module,
                                   lmn_interned_str name,
                                   int arity);

/* Instruction Argument */

InstrArgRef inst_arg_make(enum ArgType type);

InstrArgRef instr_var_arg_make(int var);
InstrArgRef ruleset_arg_make(int ruleset_id);
InstrArgRef var_list_arg_make(VarList var_list);
InstrArgRef functor_arg_make(FunctorRef functor);
InstrArgRef label_arg_make(int label);
InstrArgRef string_arg_make(int str_id);
InstrArgRef inst_list_arg_make(InstList inst_list);

/* List of arguments */

typedef struct __ArgList *ArgList;
ArgList arg_list_make(void);
void arg_list_push(ArgList l, InstrArgRef arg);

/* Instruction */

typedef struct Instruction *InstructionRef;
InstructionRef inst_make(enum LmnInstruction id, ArgList args);

/* List of instructions */

InstList inst_list_make(void);
void inst_list_push(InstList l, InstructionRef inst);

/* amatch, memmatchなど、命令をまとめたもの */

typedef struct InstBlock *InstBlockRef;

InstBlockRef inst_block_make(int label, InstList instrs );
InstBlockRef inst_block_make_without_label(InstList instrs);

/* Rule */

typedef struct Rule *RuleRef;

RuleRef rule_make_anonymous(BOOL hasuniq, InstBlockRef amatch, InstBlockRef mmatch, InstBlockRef guard, InstBlockRef body);
lmn_interned_str rule_get_name(RuleRef rule);
void stx_rule_free(RuleRef rule);

/* List of rules */

typedef struct __RuleList *RuleList;
RuleList rulelist_make(void);
void rulelist_push(RuleList l, RuleRef r);

/* Rule set */

typedef struct RuleSet *RuleSetRef;

RuleSetRef ruleset_make(int id, RuleList rules, BOOL is_system_ruleset);
int ruleset_get_id(RuleSetRef rs);

/* List of rule sets */

typedef struct __RuleSets *RuleSets;
RuleSets rulesets_make(void);
void rulesets_push(RuleSets rulesets, RuleSetRef rs);


/* Module, モジュール名とルールセットの対応 */

typedef struct Module *ModuleRef;
ModuleRef module_make(lmn_interned_str name_id, int ruleset_id);

/* List of modules */

typedef struct __ModuleList *ModuleList;
ModuleList module_list_make(void);
void module_list_push(ModuleList l, ModuleRef m);

/* Inline */
/* ファイル名が並んでいるだけ？ */

typedef struct __InlineList *InlineList;

InlineList inline_list_make(void);
void inline_list_push(InlineList l, lmn_interned_str file_name);

/* Root of the IL syntax tree */

typedef struct IL *ILRef;

ILRef il_make(RuleSets rulesets, ModuleList module_list, InlineList inlien_list);

/* 字句解析器で用いる情報。
 * TODO: ここに便宜上ここに置いておくが、適切な場所ではない */
struct lexer_context {
  /* 一つの中間言語ファイルにローカルなルールセットのIDとグローバルなIDの
     対応表 */
  st_table_t ruleset_id_tbl;
};

/* @} */

#endif
