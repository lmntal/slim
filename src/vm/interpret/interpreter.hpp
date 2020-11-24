/*
 * interpreter.hpp
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

#ifndef SLIM_VM_INTERPRETER_HPP
#define SLIM_VM_INTERPRETER_HPP

struct LmnReactCxt;
struct LmnRule;
struct LmnMembrane;
struct SameProcCxt;

#include "lmntal.h"

#include <functional>
#include <vector>
#include <mutex>

namespace slim {
namespace vm {
struct interpreter {
  LmnReactCxt *rc;
  LmnRule *rule;
  LmnRuleInstr instr;

  // stack frameのcallbackが返す値
  // Trial: callbackをstackからpopしないで続ける
  // Failure, Success: 命令列の結果を失敗/成功で上書きしてcallbackをstackからpopする
  enum class command_result {
    Trial, Failure, Success
  };

  struct stack_frame {
    // called when this frame is popped out
    std::function<command_result(interpreter &, bool)> callback;

    stack_frame(const std::function<command_result(interpreter &, bool)> &callback) : callback(callback) {}
  };

  interpreter(LmnReactCxt *rc, LmnRule *rule, LmnRuleInstr instr)
      : rc(rc), rule(rule), instr(instr) {}

  bool interpret(LmnReactCxt *rc, LmnRule *rule, LmnRuleInstr instr);
  bool exec_command(LmnReactCxt *rc, LmnRule *rule, bool &stop, bool* loading);
  void findatom(LmnReactCxt *rc, LmnRule *rule, LmnRuleInstr instr,
                LmnMembrane *mem, LmnFunctor f, size_t reg);
  void findatom_original_hyperlink(LmnReactCxt *rc, LmnRule *rule,
                                   LmnRuleInstr instr, SameProcCxt *spc,
                                   LmnMembrane *mem, LmnFunctor f, size_t reg);
  void findatom_clone_hyperlink(LmnReactCxt *rc, LmnRule *rule,
                                LmnRuleInstr instr, SameProcCxt *spc,
                                LmnMembrane *mem, LmnFunctor f, size_t reg);
  void findatom_through_hyperlink(LmnReactCxt *rc, LmnRule *rule,
                                  LmnRuleInstr instr, SameProcCxt *spc,
                                  LmnMembrane *mem, LmnFunctor f, size_t reg);
  bool run(int ti, bool* loading);

  template <typename... Args> void push_stackframe(Args... args) {
    callstack.emplace_back(std::forward<Args>(args)...);
  }

private:
  /**
   * 現在の命令アドレスを起点とする失敗駆動ループを開始する
   *
   * @param reg_idx 失敗駆動ループで列挙される要素が入るレジスタ番号
   * @param values 失敗駆動ループで列挙される要素群
   *
   * @brief
   *   この関数を呼び出した後に命令に失敗すると、reg_idx番のレジスタにvaluesの要素を設定した上で
   *   この関数を呼び出した時点での命令からやり直す。このとき全ての命令に成功すると成功扱いでルー
   *   プを終了する。あるいは、valuesの全ての要素を試した上で失敗すると失敗扱いで終了する。
   *
   *   この関数を呼び出した直後はreg_idxに値が設定されていないので必ず一度失敗する必要がある。（要検討）
   *
   *   この関数は失敗駆動ループのために必要なものをインタプリタのスタックに積む。
   *   valuesはループ中に解放されないようにするために、ヒープ上にコピーあるいはムーブされて、
   *   ループが終了した時点（成功or全て失敗）で解放される。
   *
   *   ヒント：失敗駆動ループはvaluesが返すイテレータの順番で要素を列挙する。
   */
  template <typename Container>
  void false_driven_enumerate(size_t reg_idx, Container &&values) {

    // ヒープ上にvaluesをコピーorムーブ
    auto p = new
        typename std::decay<Container>::type(std::forward<Container>(values));

    // ループ終了後に解放するためにコールバックをインタプリタのスタックに積む
    this->push_stackframe([=](slim::vm::interpreter &interpreter, bool result) {
      delete p;
      return result ? command_result::Success : command_result::Failure;
    });

    auto e = make_false_driven_enumerator(*this, this->instr, reg_idx,
                                          std::begin(*p), std::end(*p));
    this->push_stackframe(e);
  }

  std::vector<stack_frame> callstack;
};
} // namespace vm
} // namespace slim

#endif /* SLIM_VM_INTERPRETER_HPP */
