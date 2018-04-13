/*
 * syntax.c - syntax tree  of the Intermediate Language
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
 * $Id: syntax.c,v 1.5 2008/09/29 05:23:40 taisuke Exp $
 */

#include "syntax.hpp"


/* List of instrction variables */

VarList var_list_make()
{
  return new __VarList;
}

void var_list_push(VarList l, InstrArgRef n)
{
  l->push_back(n);
}


/* Functor */


FunctorRef functor_make(enum FunctorType type)
{
  return new Functor(type);
}

FunctorRef int_functor_make(long v)
{
  return new Functor(v);
}

FunctorRef float_functor_make(double v)
{
  return new Functor(v);
}

FunctorRef string_functor_make(lmn_interned_str name)
{
  return new Functor(name);
}

FunctorRef symbol_functor_make(lmn_interned_str name, int arity)
{
  return module_symbol_functor_make(ANONYMOUS, name, arity);
}

FunctorRef module_symbol_functor_make(lmn_interned_str module,
                                   lmn_interned_str name,
                                   int arity)
{
  FunctorRef f = functor_make(STX_SYMBOL);

  f->functor_id = lmn_functor_intern(module, name, arity);
  return f;
}


/* Instruction Argument */

InstrArgRef inst_arg_make(enum ArgType type)
{
  return new InstrArg(type);
}

InstrArgRef instr_var_arg_make(int var)
{
  InstrArgRef arg = inst_arg_make(InstrVar);

  arg->instr_var = var;
  return arg;
}

InstrArgRef ruleset_arg_make(int ruleset_id)
{
  InstrArgRef arg = inst_arg_make(ArgRuleset);

  arg->ruleset = ruleset_id;
  return arg;
}

InstrArgRef var_list_arg_make(VarList var_list)
{
  InstrArgRef arg = inst_arg_make(InstrVarList);

  arg->var_list = var_list;
  return arg;
}

InstrArgRef functor_arg_make(FunctorRef functor)
{
  InstrArgRef arg = inst_arg_make(ArgFunctor);

  arg->functor = functor;
  return arg;
}


InstrArgRef label_arg_make(int label)
{
  InstrArgRef arg = inst_arg_make(Label);

  arg->label = label;
  return arg;
}

InstrArgRef string_arg_make(int str_id)
{
  InstrArgRef arg = inst_arg_make(String);

  arg->str_id = str_id;
  return arg;
}

InstrArgRef inst_list_arg_make(InstList inst_list)
{
  InstrArgRef arg = inst_arg_make(InstrList);

  arg->inst_list = inst_list;
  return arg;
}

/* List of arguments */

ArgList arg_list_make()
{
  return new __ArgList;
}

InstrArg::~InstrArg() {
  switch (type) {
  case ArgFunctor:
    delete functor;
    break;
  case InstrList:
    delete inst_list;
    break;
  case InstrVarList:
    delete var_list;
  default:
    break;
  }
}

void arg_list_push(ArgList l, InstrArgRef arg)
{
  l->push_back(arg);
}

InstrArgRef arg_list_get(ArgList l, int index)
{
  return l->at(index);
}

/* Instruction */

InstructionRef inst_make(enum LmnInstruction id, ArgList args)
{
  return new Instruction(id, args);
}


/* List of instructions */

InstList inst_list_make()
{
  return new __InstList;
}

void inst_list_push(InstList l, InstructionRef inst)
{
  l->push_back(inst);
}

/* amatch, memmatchなど、命令をまとめたもの */

InstBlockRef inst_block_make(int label, InstList instrs )
{
  return new InstBlock(label, instrs);
}

InstBlockRef inst_block_make_without_label(InstList instrs)
{
  return new InstBlock(instrs);
}


/* Rule */

RuleRef rule_make_anonymous(BOOL hasuniq, InstBlockRef amatch, InstBlockRef mmatch, InstBlockRef guard, InstBlockRef body)
{
  return new Rule(hasuniq, amatch, mmatch, guard, body);
}

void stx_rule_free(RuleRef rule)
{
  delete rule;
}

BOOL rule_get_hasuniq(RuleRef rule)
{
  return rule->hasuniq;
}

/* List of rules */

RuleList rulelist_make()
{
  return new __RuleList;
}

void rulelist_push(RuleList l, RuleRef r)
{
  l->push_back(r);
}

/* Rule set */

RuleSetRef ruleset_make(int id, RuleList rules, BOOL is_system_ruleset)
{
  return new RuleSet(id, rules, is_system_ruleset);
}

int ruleset_get_id(RuleSetRef rs)
{
  return rs->id;
}

/* List of rule sets */

RuleSets rulesets_make()
{
  return new __RuleSets;
}

static void rulesets_free(RuleSets rulesets)
{
  delete rulesets;
}

void rulesets_push(RuleSets rulesets, RuleSetRef rs)
{
  rulesets->push_back(rs);
}

/* Module, モジュール名とルールセットの対応 */

ModuleRef module_make(lmn_interned_str name_id, int ruleset_id)
{
  return new Module(name_id, ruleset_id);
}

/* List of modules */

ModuleList module_list_make()
{
  return new __ModuleList();
}

void module_list_push(ModuleList l, ModuleRef m)
{
  l->push_back(m);
}

int module_list_num(ModuleList l)
{
  return l->size();
}

ModuleRef module_list_get(ModuleList l, int i)
{
  return l->at(i);
}

/* Inline */

InlineList inline_list_make()
{
  return new __InlineList();
}

void inline_list_push(InlineList l, lmn_interned_str file_name)
{
  l->push_back(file_name);
}

/* Root of the IL syntax tree */

ILRef il_make(RuleSets rulesets, ModuleList module_list, InlineList inline_list)
{
  return new IL(rulesets, module_list, inline_list);
}

