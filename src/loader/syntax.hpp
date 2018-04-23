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
#include <memory>
#include <type_traits>

extern "C" {
#include "lmntal.h"
#include "vm/vm.h"
#include "element/element.h"
}

class ByteEncoder;
struct Instruction;

namespace il {
  namespace functor {
    struct interface {
      virtual ~interface() = default; 
      virtual void visit(ByteEncoder &) = 0;
    };

    struct in_proxy : interface {
      void visit(ByteEncoder &);
    };
    struct out_proxy : interface {
      void visit(ByteEncoder &);
    };
    struct unify : interface {
      void visit(ByteEncoder &);
    };
    struct integer : interface {
      long value;
      integer(long value) : value(value) {}
      void visit(ByteEncoder &);
    };
    struct real : interface {
      double value;
      real(double value) : value(value) {}
      void visit(ByteEncoder &);
    };
    struct string : interface {
      lmn_interned_str value;
      string(lmn_interned_str value) : value(value) {}
      void visit(ByteEncoder &);
    };
    struct symbol : interface {
      lmn_interned_str value;
      symbol(lmn_interned_str name, int arity) :
        value(lmn_functor_intern(ANONYMOUS, name, arity)) {}
      symbol(lmn_interned_str module, lmn_interned_str name, int arity) :
        value(lmn_functor_intern(module, name, arity)) {}
      void visit(ByteEncoder &);
    };
  }

  using Functor = functor::interface;

  namespace instr_arg {
    using namespace std;

    struct interface {
      virtual ~interface() = default;
      virtual void visit(ByteEncoder &) const = 0;
    };

    struct var : interface {
      int value;
      var(int value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct label : interface {
      int value;
      label(int value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct string : interface {
      int value;
      string(int value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct lineno : interface {
      int value;
      lineno(int value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct functor : interface {
      shared_ptr<il::Functor> value;
      functor(shared_ptr<il::Functor> value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct ruleset : interface {
      int value;
      ruleset(int value) : value(value) {}
      void visit(ByteEncoder &) const;
    };
    struct var_list : interface {
      vector<shared_ptr<interface>> value;
      var_list(vector<shared_ptr<interface>> &&value) :
        value(std::move(value)) {}
      void visit(ByteEncoder &) const;
    };
    struct inst_list : interface {
      vector<shared_ptr<Instruction>> value;
      inst_list(vector<shared_ptr<Instruction>> &&value) :
        value(std::move(value)) {}
      void visit(ByteEncoder &) const;
    };
  }

  using InstrArg = instr_arg::interface;
}

struct Instruction {
  LmnInstruction id;
  std::vector<std::shared_ptr<il::InstrArg>> args;

  Instruction(LmnInstruction id, std::vector<std::shared_ptr<il::InstrArg>> &&args) :
    id(id), args(std::move(args)) {}

  Instruction(Instruction &&) noexcept = default;
};
static_assert(std::is_nothrow_move_constructible<Instruction>::value == true, "");

struct InstBlock {
  int label;
  std::vector<std::shared_ptr<Instruction>> instrs;

  InstBlock(int label, std::vector<std::shared_ptr<Instruction>> &&instrs) noexcept :
    label(label), instrs(std::move(instrs)) {}
  InstBlock(std::vector<std::shared_ptr<Instruction>> &&instrs) noexcept :
    label(0), instrs(std::move(instrs)) {}
  InstBlock() noexcept :
    label(0) {}
  InstBlock(InstBlock &&) noexcept = default;
  InstBlock(const InstBlock &ib) noexcept = delete;

  ~InstBlock() noexcept = default;


  BOOL has_label() const {
    return label != 0;
  }
};

static_assert(std::is_nothrow_move_constructible<InstBlock>::value == true, "");

struct Rule {
  const BOOL hasuniq;
  const lmn_interned_str name;
  InstBlock amatch;
  InstBlock mmatch;
  InstBlock guard;
  InstBlock body;

  Rule(BOOL hasuniq, InstBlock &&amatch, InstBlock &&mmatch, InstBlock &&guard, InstBlock &&body) :
    hasuniq(hasuniq), name(ANONYMOUS),
    amatch(std::move(amatch)), mmatch(std::move(mmatch)),
    guard(std::move(guard)), body(std::move(body)) {}
  Rule(Rule &&) noexcept = default;

  ~Rule() noexcept = default;
};
static_assert(std::is_nothrow_move_constructible<Rule>::value == true, "");

struct RuleSet {
  BOOL is_system_ruleset;
  int id;
  std::vector<std::shared_ptr<Rule>> rules;

  RuleSet(int id, std::vector<std::shared_ptr<Rule>> &&rules, BOOL is_system_ruleset) :
    id(id), rules(std::move(rules)), is_system_ruleset(is_system_ruleset) {}
  RuleSet(RuleSet &&) noexcept = default;

  ~RuleSet() noexcept = default;

};
static_assert(std::is_nothrow_move_constructible<RuleSet>::value == true, "");


struct Module {
  lmn_interned_str name_id;
  int ruleset_id;

  Module(lmn_interned_str name_id, int ruleset_id) :
    name_id(name_id), ruleset_id(ruleset_id) {}
};

struct IL {
  std::vector<std::shared_ptr<RuleSet>> rulesets;
  std::vector<std::shared_ptr<Module>> modules;
  std::vector<lmn_interned_str> inlines;

  IL(std::vector<std::shared_ptr<RuleSet>> &&rulesets, std::vector<std::shared_ptr<Module>> &&module_list, std::vector<lmn_interned_str> &&inline_list) :
    rulesets(std::move(rulesets)), modules(std::move(module_list)), inlines(std::move(inline_list)) {}
  IL(std::vector<std::shared_ptr<RuleSet>> &&rulesets, std::vector<std::shared_ptr<Module>> &&module_list) :
    rulesets(std::move(rulesets)), modules(std::move(module_list)) {}
  IL(std::vector<std::shared_ptr<RuleSet>> &&rulesets, std::vector<lmn_interned_str> &&inline_list) :
    rulesets(std::move(rulesets)), inlines(std::move(inline_list)) {}
  IL(std::vector<std::shared_ptr<RuleSet>> &&rulesets) :
    rulesets(std::move(rulesets)) {}

  IL(IL&&) noexcept = default;
};
static_assert(std::is_nothrow_move_constructible<IL>::value == true, "");

#endif
