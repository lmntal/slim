/*
 * byte_encoder.hpp - Encode IL AST to bytecode.
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
 *
 */

#ifndef BYTE_ENCODER_HPP
#define BYTE_ENCODER_HPP

#include <map>

#include "lmntal.h"
#include "vm/vm.h"

#include "syntax.hpp"

/* 構文木の読み込み時に使うデータ。各ルールの解析じに作成し，解析後に破
   棄する。ラベルは各ルールにローカルなものとして処理している */
class ByteEncoder {
  using label = int;
  using location = size_t;
  std::map<label, location> label_loc; /* ラベルのからラベルのある位置の対応*/
  std::map<location, label>
      loc_label_ref; /* ラベルを参照している位置と参照しているラベルの対応 */
  location loc;
  location cap;   /* 書き込み位置とbyte_seqのキャパシティ */
  BYTE *byte_seq; /* ルールの命令列を書き込む領域 */

public:
  ByteEncoder() : loc(0), cap(256), byte_seq(LMN_NALLOC(BYTE, cap)) {}

  void load(const InstBlock &ib) {
    if (ib.has_label())
      label_loc[ib.label] = loc;

    for (auto &inst : ib.instrs)
      load(inst);
  }

  void resolve_labels() {
    for (auto &p : loc_label_ref) {
      auto loc = p.first;
      auto label = p.second;
      auto target_loc = label_loc.find(label);

      if (target_loc != label_loc.end()) {
        write_at<LmnJumpOffset>(
            target_loc->second - loc - sizeof(LmnJumpOffset), loc);
      } else {
        fprintf(stderr, "label not found L%d\n", label);
        lmn_fatal("implementation error");
      }
    }
  }

  // TODO: よくない設計。コピーにするか、したくなければ
  //       LmnRuleのコンストラクタに右辺値参照としてByteEncoderを渡して初期化とか。
  LmnRuleRef create_rule() {
    return new LmnRule(byte_seq, cap, NULL, ANONYMOUS);
  }

private:
  void expand_byte_sec() {
    cap *= 2;
    byte_seq = LMN_REALLOC(BYTE, byte_seq, cap);
  }

  void load(std::shared_ptr<Instruction> inst) {
    write_forward<LmnInstrOp>(inst->id);
    auto arg_num = inst->args.size();

    /* REMOVEATOMは引数の数が2と3の場合がある。第三引数の
       ファンクタは無視する */
    if (inst->id == INSTR_REMOVEATOM && arg_num == 3) {
      arg_num = 2;
    }

    for (int i = 0; i < arg_num; i++) {
      load(inst->args[i]);
    }
  }

  void load(std::shared_ptr<il::InstrArg> arg) { arg->visit(*this); }

  void load(std::shared_ptr<il::Functor> functor) { functor->visit(*this); }

  /* 現在の位置に書き込TYPE型のデータを書き込む */
  template <typename T> void write(T value) { write_at<T>(value, loc); }

  /* 現在の書き込み位置を移動する */
  template <typename T> void move_by() { loc += sizeof(T); }

  /* write & move_by */
  template <typename T> void write_forward(T value) {
    write<T>(value);
    move_by<T>();
  }

  /* LCOの位置に書き込む */
  template <typename T> void write_at(T value, size_t loc) {
    while (loc + sizeof(T) >= cap)
      expand_byte_sec();
    *(T *)(byte_seq + loc) = (value);
  }

public:
  void load(const il::functor::symbol &functor);
  void load(const il::functor::integer &functor);
  void load(const il::functor::real &functor);
  void load(const il::functor::string &functor);
  void load(const il::functor::in_proxy &functor);
  void load(const il::functor::out_proxy &functor);
  void load(const il::functor::unify &functor);

  void load(const il::instr_arg::var &arg);
  void load(const il::instr_arg::label &arg);
  void load(const il::instr_arg::string &arg);
  void load(const il::instr_arg::lineno &arg);
  void load(const il::instr_arg::functor &arg);
  void load(const il::instr_arg::ruleset &arg);
  void load(const il::instr_arg::var_list &arg);
  void load(const il::instr_arg::inst_list &arg);
};

#endif /* BYTE_ENCODER_HPP */
