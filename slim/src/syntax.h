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
#include "instruction.h"
#include "vector.h"
#include "functor.h"

/* 型名の解決の為に上に持ってきた */
typedef Vector *InstList;

/* List of instrction variables */

typedef Vector *VarList;
VarList var_list_make(void);
void var_list_push(VarList l, int n);
unsigned int var_list_num(VarList l);
int var_list_get(VarList l, int i);

/* Functor */

enum FunctorType {STX_SYMBOL, INT_FUNC, FLOAT_FUNC, STRING_FUNC, STX_IN_PROXY, STX_OUT_PROXY, STX_UNIFY};

typedef struct Functor *Functor;

Functor functor_make(enum FunctorType type);
Functor int_functor_make(int v);
Functor float_functor_make(double v);
Functor symbol_functor_make(lmn_interned_str name, int arity);
Functor string_functor_make(lmn_interned_str name);
Functor module_symbol_functor_make(lmn_interned_str module,
                                   lmn_interned_str name,
                                   int arity);
enum FunctorType functor_get_type(Functor f);
int functor_get_int_value(Functor f);
double functor_get_float_value(Functor f);
lmn_interned_str functor_get_string_value(Functor f);
int functor_get_id(Functor f);

/* Instruction Argument */

typedef struct InstrArg *InstrArg;

InstrArg inst_arg_make(enum ArgType type);
enum ArgType inst_arg_get_type(InstrArg arg);
int inst_arg_get_var(InstrArg arg);
int inst_arg_get_label(InstrArg arg);
lmn_interned_str inst_arg_get_str_id(InstrArg arg);
int inst_arg_get_linenum(InstrArg arg);
Functor inst_arg_get_functor(InstrArg arg);
int inst_arg_get_ruleset_id(InstrArg arg);
VarList inst_arg_get_var_list(InstrArg arg);
InstList inst_arg_get_inst_list(InstrArg arg);

InstrArg instr_var_arg_make(int var);
InstrArg ruleset_arg_make(int ruleset_id);
InstrArg var_list_arg_make(VarList var_list);
InstrArg functor_arg_make(Functor functor);
InstrArg label_arg_make(int label);
InstrArg string_arg_make(int str_id);
InstrArg inst_list_arg_make(InstList inst_list);

/* List of arguments */

typedef Vector *ArgList;
ArgList arg_list_make(void);
unsigned int arg_list_num(ArgList l);
void arg_list_push(ArgList l, InstrArg arg);
InstrArg arg_list_get(ArgList l, int index);

/* Instruction */

typedef struct Instruction *Instruction;

Instruction inst_make(enum LmnInstruction id, ArgList args);
int inst_get_id(Instruction inst);
ArgList inst_get_args(Instruction inst);

/* List of instructions */

InstList inst_list_make(void);
void inst_list_push(InstList l, Instruction inst);
unsigned int inst_list_num(InstList l);
Instruction inst_list_get(InstList l, int index);

/* amatch, memmatchなど、命令をまとめたもの */

typedef struct InstBlock *InstBlock;

InstBlock inst_block_make(int label, InstList instrs );
InstBlock inst_block_make_without_label(InstList instrs);
int inst_block_get_label(InstBlock ib);
InstList inst_block_get_instructions(InstBlock ib);
BOOL inst_block_has_label(InstBlock ib);

/* Rule */

typedef struct Rule *Rule;

Rule rule_make_anonymous(InstBlock amatch, InstBlock mmatch, InstBlock guard, InstBlock body);
lmn_interned_str rule_get_name(Rule rule);
void stx_rule_free(Rule rule);
InstBlock rule_get_amatch(Rule rule);
InstBlock rule_get_mmatch(Rule rule);
InstBlock rule_get_guard(Rule rule);
InstBlock rule_get_body(Rule rule);

/* List of rules */

typedef Vector *RuleList;
RuleList rulelist_make(void);
void rulelist_push(RuleList l, Rule r);
Rule rulelist_get(RuleList l, int index);
unsigned int rulelist_num(RuleList l);

/* Rule set */

typedef struct RuleSet *RuleSet;

RuleSet ruleset_make(int id, RuleList rules, BOOL is_system_ruleset);
BOOL ruleset_is_system_ruleset(RuleSet rs);
int ruleset_get_id(RuleSet rs);
RuleList ruleset_get_rulelist(RuleSet rs);

/* List of rule sets */

typedef Vector *RuleSets;
RuleSets rulesets_make(void);
void rulesets_push(RuleSets rulesets, RuleSet rs);
int rulesets_num(RuleSets rulesets);
RuleSet rulesets_get(RuleSets rulesets, int i);


/* Module, モジュール名とルールセットの対応 */

typedef struct Module *Module;

Module module_make(lmn_interned_str name_id, int ruleset_id);
lmn_interned_str module_get_name(Module m);
int module_get_ruleset(Module m);

/* List of modules */

typedef Vector *ModuleList;

ModuleList module_list_make(void);
void module_list_push(ModuleList l, Module m);
int module_list_num(ModuleList l);
Module module_list_get(ModuleList l, int i);

/* Inline */
/* ファイル名が並んでいるだけ？ */

typedef Vector *InlineList;
InlineList inline_list_make(void);
void inline_list_push(InlineList l, lmn_interned_str file_name);

/* Root of the IL syntax tree */

typedef struct IL *IL;

IL il_make(RuleSets rulesets, ModuleList module_list, InlineList inlien_list);
RuleSets il_get_rulesets(IL il);
ModuleList il_get_module_list(IL il);
InlineList il_get_inline_list(IL il);
void il_free(IL il);

/* 字句解析器で用いる情報。ここに便宜上ここに置いておくが、適切な場所ではないと思う */
#include "st.h"
struct lexer_context {
  /* 一つの中間言語ファイルにローカルなルールセットのIDとグローバルなIDの
     対応表 */
  st_table *ruleset_id_tbl;
};

#endif
