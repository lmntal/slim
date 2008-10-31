/*
 * syntax.c - syntax tree  of the Intermediate Language
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lang@ueda.info.waseda.ac.jp>
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
 * $Id: syntax.c,v 1.5 2008/09/29 05:23:40 taisuke Exp $
 */

#include "lmntal.h"
#include "syntax.h"
#include "symbol.h"


struct InstrArg {
  enum ArgType type; 
  union {
    int int_value;
    int instr_var;
    int label;
    int str_id;
    int line_num;
    Functor functor;
    int ruleset;
    VarList var_list;
    InstList inst_list;
  } v;
};

struct IL {
  RuleSets rulesets;
  ModuleList modules;
  InlineList inlines;
};

struct Instruction {
  enum LmnInstruction id;
  ArgList args;
};

struct Rule {
  lmn_interned_str name;
  InstBlock amatch;
  InstBlock mmatch;
  InstBlock guard;
  InstBlock body;
};

struct RuleSet {
  BOOL is_system_ruleset;
  int id;
  RuleList rules;
};

struct Module {
  lmn_interned_str name_id;
  int ruleset_id;
};

struct InstBlock {
  int label;
  InstList instrs;
};

struct Functor {
  enum FunctorType type;
  union {
    int int_value;
    double float_value;
    int functor_id;
  } v;
};


/* prototypes */

static void rulelist_free(RuleList l);
static void inst_block_free(InstBlock ib);
static void rulesets_free(RuleSets rulesets);
static void module_list_free(ModuleList l);
static void inst_list_free(InstList l);
static void arg_list_free(ArgList args);
static void inst_arg_free(InstrArg arg);
static void functor_free(Functor f);
static void inline_list_free(InlineList l);

/* List of instrction variables */

VarList var_list_make()
{
  return vec_make(10);
}

static void var_list_free(VarList l)
{
  vec_free(l);
}

void var_list_push(VarList l, int n)
{
  vec_push(l, n);
}

unsigned int var_list_num(VarList l)
{
  return vec_num(l);
}

int var_list_get(VarList l, int i)
{
  return vec_get(l, i);
}

/* Functor */

Functor functor_make(enum FunctorType type)
{
  Functor f = LMN_MALLOC(struct Functor);

  f->type = type;
  return f;
}

static void functor_free(Functor f)
{
  LMN_FREE(f);
}

Functor int_functor_make(int v)
{
  Functor f = functor_make(INT_FUNC);

  f->v.int_value = v;
  return f;
}

Functor float_functor_make(double v)
{
  Functor f = functor_make(FLOAT_FUNC);

  f->v.float_value = v;
  return f;
}

Functor symbol_functor_make(lmn_interned_str name, int arity)
{
  return module_symbol_functor_make(ANONYMOUS, name, arity);
}

Functor module_symbol_functor_make(lmn_interned_str module,
                                   lmn_interned_str name,
                                   int arity)
{
  Functor f = functor_make(SYMBOL);

  f->v.functor_id = lmn_functor_intern(module, name, arity);
  return f;
}

enum FunctorType functor_get_type(Functor f)
{
  return f->type;
}

int functor_get_int_value(Functor f)
{
  return f->v.int_value;
}

double functor_get_float_value(Functor f)
{
  return f->v.float_value;
}

/* シンボルアトムのファンクタのIDを取得 */
int functor_get_id(Functor f)
{
  return f->v.functor_id;
}


/* Instruction Argument */

InstrArg inst_arg_make(enum ArgType type)
{
  InstrArg arg = LMN_MALLOC(struct InstrArg);
  arg->type = type;
  return arg;
}

static void inst_arg_free(InstrArg arg) {
  switch (inst_arg_get_type(arg)) {
  case ArgFunctor:
    functor_free(inst_arg_get_functor(arg));
    break;
  case InstrList:
    inst_list_free(inst_arg_get_inst_list(arg));
    break;
  case InstrVarList:
    var_list_free(inst_arg_get_var_list(arg));
  default:
    break;
  }
  LMN_FREE(arg);
}

enum ArgType inst_arg_get_type(InstrArg arg)
{
  return arg->type;
}

int inst_arg_get_var(InstrArg arg)
{
  return arg->v.instr_var;
}

int inst_arg_get_label(InstrArg arg)
{
  return arg->v.label;
}

lmn_interned_str inst_arg_get_str_id(InstrArg arg)
{
  return arg->v.str_id;
}

int inst_arg_get_linenum(InstrArg arg)
{
  return arg->v.line_num;
}

Functor inst_arg_get_functor(InstrArg arg)
{
  return arg->v.functor;
}

int inst_arg_get_ruleset_id(InstrArg arg)
{
  return arg->v.ruleset;
}

VarList inst_arg_get_var_list(InstrArg arg)
{
  return arg->v.var_list;
}

InstList inst_arg_get_inst_list(InstrArg arg)
{
  return arg->v.inst_list;
}

InstrArg instr_var_arg_make(int var)
{
  InstrArg arg = inst_arg_make(InstrVar);

  arg->v.instr_var = var;
  return arg;
}

InstrArg ruleset_arg_make(int ruleset_id)
{
  InstrArg arg = inst_arg_make(ArgRuleset);

  arg->v.ruleset = ruleset_id;
  return arg;
}

InstrArg var_list_arg_make(VarList var_list)
{
  InstrArg arg = inst_arg_make(InstrVarList);

  arg->v.var_list = var_list;
  return arg;
}

InstrArg functor_arg_make(Functor functor)
{
  InstrArg arg = inst_arg_make(ArgFunctor);

  arg->v.functor = functor;
  return arg;
}


InstrArg label_arg_make(int label)
{
  InstrArg arg = inst_arg_make(Label);

  arg->v.label = label;
  return arg;
}

InstrArg string_arg_make(int str_id)
{
  InstrArg arg = inst_arg_make(String);

  arg->v.str_id = str_id;
  return arg;
}

InstrArg inst_list_arg_make(InstList inst_list)
{
  InstrArg arg = inst_arg_make(InstrList);

  arg->v.inst_list = inst_list;
  return arg;
}

/* List of arguments */

ArgList arg_list_make()
{
  return vec_make(10);
}

static void arg_list_free(ArgList args)
{
  unsigned int i;

  for (i = 0; i < arg_list_num(args); i++) {
    inst_arg_free(arg_list_get(args, i));
  }
  vec_free(args);
}

void arg_list_push(ArgList l, InstrArg arg)
{
  vec_push(l, (vec_data_t)arg);
}

unsigned int arg_list_num(ArgList l)
{
  return vec_num(l);
}

InstrArg arg_list_get(ArgList l, int index)
{
  return (InstrArg)vec_get(l, index);
}

/* Instruction */

Instruction inst_make(enum LmnInstruction id, ArgList args)
{
  Instruction i;

  i = LMN_MALLOC(struct Instruction);
  i->id = id;
  i->args = args;

  /* COMMITの第二引数を変数番号ではなく行番号とする */
  if (id == INSTR_COMMIT) {
    InstrArg line_num_arg = arg_list_get(args, 1);
    line_num_arg->type = LineNum;
    line_num_arg->v.line_num = line_num_arg->v.instr_var;
  }

  return i;
}

static void inst_free(Instruction inst)
{
  arg_list_free(inst_get_args(inst));
  LMN_FREE(inst);
}

int inst_get_id(Instruction inst)
{
  return inst->id;
}

ArgList inst_get_args(Instruction inst)
{
  return inst->args;
}

/* List of instructions */

InstList inst_list_make()
{
  return vec_make(64);
}

static void inst_list_free(InstList l)
{
  unsigned int i;

  for (i = 0; i < inst_list_num(l); i++) inst_free(inst_list_get(l, i));
  vec_free(l);
}

void inst_list_push(InstList l, Instruction inst)
{
  vec_push(l, (vec_data_t)inst);
}

unsigned int inst_list_num(InstList l)
{
  return vec_num(l);
}

Instruction inst_list_get(InstList l, int index)
{
  return (Instruction)vec_get(l, index);
}

/* amatch, memmatchなど、命令をまとめたもの */

InstBlock inst_block_make(int label, InstList instrs )
{
  InstBlock i = LMN_MALLOC(struct InstBlock);

  i->label = label;
  i->instrs = instrs;
  return i;
}

InstBlock inst_block_make_without_label(InstList instrs)
{
  return inst_block_make(0, instrs);
}

static void inst_block_free(InstBlock ib)
{
  inst_list_free(inst_block_get_instructions(ib));
  LMN_FREE(ib);
}

int inst_block_get_label(InstBlock ib)
{
  return ib->label;
}

InstList inst_block_get_instructions(InstBlock ib)
{
  return ib->instrs;
}

BOOL inst_block_has_label(InstBlock ib)
{
  return ib->label != 0;
}

/* Rule */

Rule rule_make_anonymous(InstBlock amatch, InstBlock mmatch, InstBlock guard, InstBlock body)
{
  Rule r = LMN_MALLOC(struct Rule);

  r->name = ANONYMOUS;
  r->amatch = amatch;
  r->mmatch = mmatch;
  r->guard = guard;
  r->body = body;
  return r;
}

static void rule_free(Rule rule)
{
  inst_block_free(rule->amatch);
  inst_block_free(rule->mmatch);
  inst_block_free(rule->guard);
  inst_block_free(rule->body);
  LMN_FREE(rule);
}

lmn_interned_str rule_get_name(Rule rule)
{
  return rule->name;
}

InstBlock rule_get_amatch(Rule rule)
{
  return rule->amatch;
}

InstBlock rule_get_mmatch(Rule rule)
{
  return rule->mmatch;
}

InstBlock rule_get_guard(Rule rule)
{
  return rule->guard;
}

InstBlock rule_get_body(Rule rule)
{
  return rule->body;
}

/* List of rules */

RuleList rulelist_make()
{
  return (RuleList)vec_make(16);
}

static void rulelist_free(RuleList l) {
  unsigned int i;

  for (i = 0; i< rulelist_num(l); i++) rule_free(rulelist_get(l, i));
  vec_free(l);
}

void rulelist_push(RuleList l, Rule r)
{
  vec_push(l, (vec_data_t)r);
}

Rule rulelist_get(RuleList l, int index)
{
  return (Rule)vec_get(l, index);
}

unsigned int rulelist_num(RuleList l)
{
  return vec_num(l);
}

/* Rule set */

RuleSet ruleset_make(int id, RuleList rules, BOOL is_system_ruleset)
{
  RuleSet r = LMN_MALLOC(struct RuleSet);

  r->id = id;
  r->rules = rules;
  r->is_system_ruleset = is_system_ruleset;
  return r;
}

static void ruleset_free(RuleSet rs)
{
  rulelist_free(ruleset_get_rulelist(rs));

  LMN_FREE(rs);
}

BOOL ruleset_is_system_ruleset(RuleSet rs)
{
  return rs->is_system_ruleset;
}

int ruleset_get_id(RuleSet rs)
{
  return rs->id;
}

RuleList ruleset_get_rulelist(RuleSet rs)
{
  return rs->rules;
}

/* List of rule sets */

RuleSets rulesets_make()
{
  return vec_make(8);
}

static void rulesets_free(RuleSets rulesets)
{
  int i;
  for (i = 0; i < rulesets_num(rulesets); i++) ruleset_free(rulesets_get(rulesets, i));
  vec_free(rulesets);
}

void rulesets_push(RuleSets rulesets, RuleSet rs)
{
  vec_push(rulesets, (vec_data_t)rs);
}

int rulesets_num(RuleSets rulesets)
{
  return vec_num(rulesets);
}

RuleSet rulesets_get(RuleSets rulesets, int i)
{
  return (RuleSet)vec_get(rulesets, i);
}

/* Module, モジュール名とルールセットの対応 */

Module module_make(lmn_interned_str name_id, int ruleset_id)
{
  Module m = LMN_MALLOC(struct Module);

  m->name_id = name_id;
  m->ruleset_id = ruleset_id;

  return m;
}

static void module_free(Module m)
{
  LMN_FREE(m);
}

lmn_interned_str module_get_name(Module m)
{
  return m->name_id;
}

int module_get_ruleset(Module m)
{
  return m->ruleset_id;
}

/* List of modules */

ModuleList module_list_make()
{
  return vec_make(4);
}

static void module_list_free(ModuleList l)
{
  int i;

  for (i = 0; i < module_list_num(l); i++) module_free(module_list_get(l, i));
  vec_free(l);
}

void module_list_push(ModuleList l, Module m)
{
  vec_push(l, (vec_data_t)m);
}

int module_list_num(ModuleList l)
{
  return vec_num(l);
}

Module module_list_get(ModuleList l, int i)
{
  return (Module)vec_get(l, i);
}

/* Inline */

InlineList inline_list_make()
{
  return vec_make(8);
}

static void inline_list_free(InlineList l)
{
  vec_free(l);
}

void inline_list_push(InlineList l, lmn_interned_str file_name)
{
  vec_push(l, file_name);
}

/* Root of the IL syntax tree */

IL il_make(RuleSets rulesets, ModuleList module_list, InlineList inline_list)
{
  IL il = LMN_MALLOC(struct IL);

  il->rulesets = rulesets;
  il->modules = module_list;
  il->inlines = inline_list;

  return il;
}

void il_free(IL il)
{
  rulesets_free(il_get_rulesets(il));
  module_list_free(il_get_module_list(il));
  inline_list_free(il_get_inline_list(il));
  LMN_FREE(il);
}

RuleSets il_get_rulesets(IL il)
{
  return il->rulesets;
}

ModuleList il_get_module_list(IL il)
{
  return il->modules;
}

ModuleList il_get_inline_list(IL il)
{
  return il->inlines;
}
