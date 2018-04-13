/*
 * syntax.hpp - syntax tree  of the Intermediate Language
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

#ifndef LMN_SYNTAX_HPP
#define LMN_SYNTAX_HPP

#include <vector>

extern "C" {
#include "lmntal.h"
#include "vm/vm.h"
#include "element/element.h"
#include "syntax.h"
}

struct __VarList : std::vector<InstrArgRef> {
  using std::vector<InstrArgRef>::vector;
};

struct Functor {
  enum FunctorType type;
  union {
    long int_value;
    double float_value;
    lmn_interned_str str;
    int functor_id;
  };

  Functor(FunctorType type) : type(type) {};
  Functor(long v) : type(INT_FUNC) {
    this->int_value = v;
  };
  Functor(double v) : type(FLOAT_FUNC) {
    this->float_value = v;
  };
  Functor(lmn_interned_str v) : type(STRING_FUNC) {
    this->str = v;
  };

  operator long() {
    return int_value;
  }
  operator double() {
    return float_value;
  }
  operator lmn_interned_str() {
    return str;
  }
};

struct InstrArg {
  enum ArgType type;
  union {
    int int_value;
    int instr_var;
    int label;
    int str_id;
    int line_num;
    FunctorRef functor;
    int ruleset;
    VarList var_list;
    InstList inst_list;
  };

  InstrArg(ArgType type) : type(type) {};

  ~InstrArg();
};

struct __ArgList : std::vector<InstrArgRef> {
  using std::vector<InstrArgRef>::vector;

  ~__ArgList() {
    for (auto &r : *this) delete r;
  }
};


struct Instruction {
  enum LmnInstruction id;
  ArgList args;

  Instruction(LmnInstruction id, ArgList args) : id(id), args(args) {
    /* COMMITの第二引数を変数番号ではなく行番号とする */
    if (id == INSTR_COMMIT) {
      InstrArgRef &line_num_arg = args->at(1);
      line_num_arg->type = LineNum;
      line_num_arg->line_num = line_num_arg->instr_var;
    }
  }

  ~Instruction() {
    delete args;
  }
};

struct __InstList : std::vector<InstructionRef> {
  using std::vector<InstructionRef>::vector;

  ~__InstList() {
    for (auto &i : *this) delete i;
  }
};

struct InstBlock {
  int label;
  InstList instrs;

  InstBlock(int label, InstList instrs) : label(label), instrs(instrs) {}
  InstBlock(InstList instrs) : InstBlock(0, instrs) {}

  ~InstBlock() {
    delete instrs;
  }

  BOOL has_label() const {
    return label != 0;
  }
};

struct Rule {
  BOOL hasuniq;
  lmn_interned_str name;
  InstBlockRef amatch;
  InstBlockRef mmatch;
  InstBlockRef guard;
  InstBlockRef body;

  Rule(BOOL hasuniq, InstBlockRef amatch, InstBlockRef mmatch, InstBlockRef guard, InstBlockRef body)
    : hasuniq(hasuniq), name(ANONYMOUS), amatch(amatch), mmatch(mmatch), guard(guard), body(body) {}

  ~Rule() {
    delete amatch;
    delete mmatch;
    delete guard;
    delete body;
  }
};

struct __RuleList : std::vector<RuleRef> {
  using std::vector<RuleRef>::vector;

  ~__RuleList() {
    for (auto &r : *this) delete r;
  }
};

struct RuleSet {
  BOOL is_system_ruleset;
  int id;
  RuleList rules;

  RuleSet(int id, RuleList rules, BOOL is_system_ruleset) :
    id(id), rules(rules), is_system_ruleset(is_system_ruleset) {}

  ~RuleSet() {
    delete rules;
  }
};

struct __RuleSets : std::vector<RuleSetRef> {
  using std::vector<RuleSetRef>::vector;

  ~__RuleSets() {
    for (auto &r : *this) delete r;
  }
};

struct Module {
  lmn_interned_str name_id;
  int ruleset_id;

  Module(lmn_interned_str name_id, int ruleset_id) :
    name_id(name_id), ruleset_id(ruleset_id) {}
};

struct __ModuleList : std::vector<ModuleRef> {
  using std::vector<ModuleRef>::vector;

  ~__ModuleList() {
    for (auto &m : *this) delete m;
  }
};

struct __InlineList : std::vector<lmn_interned_str> {
  using std::vector<lmn_interned_str>::vector;
};

struct IL {
  RuleSets rulesets;
  ModuleList modules;
  InlineList inlines;

  IL(RuleSets rulesets, ModuleList module_list, InlineList inline_list) :
    rulesets(rulesets), modules(module_list), inlines(inline_list) {}

  ~IL() {
    delete rulesets;
    delete modules;
    delete inlines;
  }
};

#endif
