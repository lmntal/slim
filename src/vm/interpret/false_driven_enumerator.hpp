/*
 * false_driven_enumerator.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 */

#ifndef SLIM_VM_INTERPRET_FALSE_DRIVEN_ENUMERATOR_HPP
#define SLIM_VM_INTERPRET_FALSE_DRIVEN_ENUMERATOR_HPP

#include "../react_context.hpp"
#include "interpreter.hpp"
#include "verifier/runtime_status.h"

template <typename InputIterator, typename value_type>
struct false_driven_enumerator_impl;

template <typename InputIterator>
struct false_driven_enumerator_impl<InputIterator, LmnRegister> {
  size_t reg_idx;
  InputIterator begin;
  InputIterator end;
  LmnRuleInstr instr;
  slim::vm::interpreter &interpreter;

  false_driven_enumerator_impl(slim::vm::interpreter &interpreter,
                               LmnRuleInstr instr, size_t reg_idx,
                               InputIterator begin, InputIterator end)
      : interpreter(interpreter), instr(instr), reg_idx(reg_idx), begin(begin),
        end(end) {}

  bool operator()(bool result) {
    if (result)
      return true;
    if (begin == end)
      return false;
    interpreter.instr = instr;
    interpreter.rc->reg(reg_idx) = *(begin++);
    interpreter.push_stackframe(
        false_driven_enumerator_impl(interpreter, instr, reg_idx, begin, end));
    profile_backtrack();
    return interpreter.run();
  }
};

template <typename InputIterator>
struct false_driven_enumerator_impl<InputIterator,
                                    std::function<LmnRegister(void)>> {
  size_t reg_idx;
  InputIterator begin;
  InputIterator end;
  LmnRuleInstr instr;
  slim::vm::interpreter &interpreter;

  false_driven_enumerator_impl(slim::vm::interpreter &interpreter,
                               LmnRuleInstr instr, size_t reg_idx,
                               InputIterator begin, InputIterator end)
      : interpreter(interpreter), instr(instr), reg_idx(reg_idx), begin(begin),
        end(end) {}

  bool operator()(bool result) {
    if (result)
      return true;
    if (begin == end)
      return false;
    interpreter.instr = instr;
    interpreter.rc->reg(reg_idx) = (*(begin++))();
    interpreter.push_stackframe(
        false_driven_enumerator_impl(interpreter, instr, reg_idx, begin, end));
    profile_backtrack();
    return interpreter.run();
  }
};

template <typename Iterator>
using false_driven_enumerator =
    false_driven_enumerator_impl<Iterator, typename Iterator::value_type>;

template <typename InputIterator, typename... Args>
false_driven_enumerator<InputIterator>
make_false_driven_enumerator(slim::vm::interpreter &iter, LmnRuleInstr instr,
                             size_t reg_idx, InputIterator begin,
                             InputIterator end) {
  return false_driven_enumerator<InputIterator>(iter, instr, reg_idx, begin,
                                                end);
}

template <typename Container>
false_driven_enumerator<typename Container::iterator>
make_false_driven_enumerator(slim::vm::interpreter &iter, LmnRuleInstr instr,
                             size_t reg_idx, Container &&values) {
  auto p = new Container(std::move(values));
  iter.push_stackframe([=](bool result) {
    delete p;
    return result;
  });
  return false_driven_enumerator<typename Container::iterator>(iter, instr, reg_idx, std::begin(*p), std::end(*p));
}

#endif /* SLIM_VM_INTERPRET_FALSE_DRIVEN_ENUMERATOR_HPP */
