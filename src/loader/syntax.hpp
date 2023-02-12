/*
 * syntax.hpp - syntax tree  of the Intermediate Language
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

#include <memory>
#include <type_traits>
#include <vector>

#include "element/element.h"
#include "lmntal.h"
#include "vm/vm.h"

class ByteEncoder;
struct Instruction;

namespace il {
namespace c17 = slim::element;

namespace functor {
struct in_proxy {};
struct out_proxy {};
struct unify {};
struct integer {
  long value;
  integer(long value) : value(value) {}
};
struct real {
  double value;
  real(double value) : value(value) {}
};
struct string {
  lmn_interned_str value;
  string(lmn_interned_str value) : value(value) {}
};
struct symbol {
  lmn_interned_str value;
  symbol(lmn_interned_str name, int arity)
      : value(lmn_functor_table->intern(ANONYMOUS, name, arity)) {}
  symbol(lmn_interned_str module, lmn_interned_str name, int arity)
      : value(lmn_functor_table->intern(module, name, arity)) {}
};
} // namespace functor

using Functor = c17::variant<functor::in_proxy, functor::out_proxy,
                             functor::unify, functor::integer, functor::real,
                             functor::string, functor::symbol>;

namespace instr_arg {
struct var;
struct label;
struct string;
struct lineno;
struct functor;
struct ruleset;
struct var_list;
struct inst_list;
} // namespace instr_arg

using InstrArg =
    c17::variant<c17::monostate, instr_arg::var, instr_arg::label,
                 instr_arg::string, instr_arg::lineno, instr_arg::functor,
                 instr_arg::ruleset, instr_arg::var_list, instr_arg::inst_list>;

namespace instr_arg {
struct var {
  int value;
  var(int value) : value(value) {}
};
struct label {
  int value;
  label(int value) : value(value) {}
};
struct string {
  int value;
  string(int value) : value(value) {}
};
struct lineno {
  int value;
  lineno(int value) : value(value) {}
};
struct functor {
  il::Functor value;
  functor(il::Functor &&value) : value(std::move(value)) {}
};
struct ruleset {
  int value;
  ruleset(int value) : value(value) {}
};
struct var_list {
  std::vector<InstrArg> value;
  var_list(std::vector<InstrArg> &&value) : value(std::move(value)) {}
};
struct inst_list {
  std::vector<Instruction> value;
  inst_list(std::vector<Instruction> &&value) : value(std::move(value)) {}
};
} // namespace instr_arg
} // namespace il

struct Instruction {
  LmnInstruction id;
  std::vector<il::InstrArg> args;

  Instruction() : id(INSTR_DUMMY), args() {}
  Instruction(LmnInstruction id, std::vector<il::InstrArg> &&args)
      : id(id), args(std::move(args)) {}
};

struct InstBlock {
  int label;
  std::vector<Instruction> instrs;

  InstBlock(int label, std::vector<Instruction> &&instrs) noexcept
      : label(label), instrs(std::move(instrs)) {}
  InstBlock(std::vector<Instruction> &&instrs) noexcept
      : label(0), instrs(std::move(instrs)) {}
  InstBlock() noexcept : label(0) {}

  BOOL has_label() const { return label != 0; }
};

struct Rule {
  BOOL hasuniq;
  lmn_interned_str name;
  InstBlock amatch;
  InstBlock mmatch;
  InstBlock guard;
  InstBlock body;

  Rule() {}
  Rule(BOOL hasuniq, InstBlock &&amatch, InstBlock &&mmatch, InstBlock &&guard,
       InstBlock &&body)
      : hasuniq(hasuniq), name(ANONYMOUS), amatch(std::move(amatch)),
        mmatch(std::move(mmatch)), guard(std::move(guard)),
        body(std::move(body)) {}
};

struct Subrule {
  lmn_interned_str name;
  InstBlock body;

  Subrule();
  Subrule(lmn_interned_str name, InstBlock &&body)
    : name(name), body(std::move(body)) {}
};

struct RuleSet {
  BOOL is_system_ruleset;
  int id;
  std::vector<il::c17::variant<Rule, Subrule>> rules;

  RuleSet() {}
  RuleSet(int id, std::vector<il::c17::variant<Rule, Subrule>> &&rules, BOOL is_system_ruleset)
      : id(id), rules(std::move(rules)), is_system_ruleset(is_system_ruleset) {}
};

struct Module {
  lmn_interned_str name_id;
  int ruleset_id;

  Module(lmn_interned_str name_id, int ruleset_id)
      : name_id(name_id), ruleset_id(ruleset_id) {}
};

struct IL {
  std::vector<RuleSet> rulesets;
  std::vector<Module> modules;
  std::vector<lmn_interned_str> inlines;

  IL(std::vector<RuleSet> &&rulesets, std::vector<Module> &&module_list,
     std::vector<lmn_interned_str> &&inline_list)
      : rulesets(std::move(rulesets)), modules(std::move(module_list)),
        inlines(std::move(inline_list)) {}
  IL(std::vector<RuleSet> &&rulesets, std::vector<Module> &&module_list)
      : rulesets(std::move(rulesets)), modules(std::move(module_list)) {}
  IL(std::vector<RuleSet> &&rulesets,
     std::vector<lmn_interned_str> &&inline_list)
      : rulesets(std::move(rulesets)), inlines(std::move(inline_list)) {}
  IL(std::vector<RuleSet> &&rulesets) : rulesets(std::move(rulesets)) {}
};

#endif
