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


/**
 * 失敗駆動ループの候補を返すためのfunctional object
 */
template <typename InputIterator, typename value_type>
struct false_driven_enumerator_get_candidate;

template <typename InputIterator>
struct false_driven_enumerator_get_candidate<InputIterator, LmnRegister> {
  LmnRegister operator()(InputIterator &iter) { return (*iter); }
};

template <typename InputIterator>
struct false_driven_enumerator_get_candidate<InputIterator,
                                             std::function<LmnRegister(void)>> {
  LmnRegister operator()(InputIterator &iter) { return (*iter)(); }
};

/**
 * 失敗駆動ループの実装の
 * スタックフレームがポップされるときに呼ばれるコールバックで
 * 必要なら（失敗駆動ループするなら）次の候補をレジスタに設定し、スタックに積む
 * これ単体では役に立たない！
 */
template <typename InputIterator> struct false_driven_enumerator {
  using value_type = typename InputIterator::value_type;
  size_t reg_idx;
  InputIterator begin;
  InputIterator end;
  LmnRuleInstr instr;

  /**
   * コンストラクタ
   *
   * @param instr ループ時に再開する命令列のアドレス
   * @param reg_idx 次の候補を設定するレジスタのインデックス
   * @param begin 列挙する候補の最初の要素を指すイテレータ
   * @param end 最終候補の次の要素を指すイテレータ
   */
  false_driven_enumerator(LmnRuleInstr instr, size_t reg_idx,
                          InputIterator begin, InputIterator end)
      : instr(instr), reg_idx(reg_idx), begin(begin), end(end) {}

  slim::vm::interpreter::command_result operator()(slim::vm::interpreter &itr, bool result) {
    // 成功ならループしないで終了
    if (result)
      return slim::vm::interpreter::command_result::Success;

    // 候補がなくなったら終了
    if (this->begin == this->end)
      return slim::vm::interpreter::command_result::Failure;

    // 命令列の巻き戻し
    itr.instr = this->instr;

    // 候補を再設定
    itr.rc->reg(this->reg_idx) =
        false_driven_enumerator_get_candidate<InputIterator, value_type>()(
            this->begin);

    // 次の候補の準備
    ++this->begin;

    profile_backtrack();
    return slim::vm::interpreter::command_result::Trial;
  }
};

/**
 * 既存のイテレータから失敗駆動ループを作る
 */
template <typename InputIterator, typename... Args>
false_driven_enumerator<InputIterator>
make_false_driven_enumerator(slim::vm::interpreter &iter, LmnRuleInstr instr, size_t reg_idx,
                             InputIterator begin, InputIterator end) {
  return false_driven_enumerator<InputIterator>(instr, reg_idx, begin, end);
}

#endif /* SLIM_VM_INTERPRET_FALSE_DRIVEN_ENUMERATOR_HPP */
