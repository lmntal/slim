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

#include "syntax.hpp"

#include "lmntal.h"
#include "vm/vm.h"

#include <map>

namespace c17 = slim::element;

/* 構文木の読み込み時に使うデータ。各ルールの解析じに作成し，解析後に破
   棄する。ラベルは各ルールにローカルなものとして処理している */
class ByteEncoder {
  using label    = int;
  using location = size_t;
  std::map<label, location> label_loc;     /* ラベルのからラベルのある位置の対応*/
  std::map<location, label> loc_label_ref; /* ラベルを参照している位置と参照しているラベルの対応 */
  location         loc;
  location         cap;      /* 書き込み位置とbyte_seqのキャパシティ */
  BYTE            *byte_seq; /* ルールの命令列を書き込む領域 */
  bool             hasuniq;
  lmn_interned_str name;

public:
  static std::unique_ptr<LmnRule> encode_rule_ast(Rule const &rule) { return ByteEncoder(rule).create_rule(); }
  static std::unique_ptr<LmnRule> encode_rule_ast(Subrule const &rule) { return ByteEncoder(rule).create_subrule(); }

private:
  ByteEncoder(Rule const &rule) : loc(0), cap(256), byte_seq(LMN_NALLOC<BYTE>(cap)), hasuniq(rule.hasuniq) {
    /* load(rule.amatch); */
    load(rule.mmatch);
    load(rule.guard);
    load(rule.body);
    resolve_labels();
  }

  ByteEncoder(Subrule const &rule) : loc(0), cap(256), byte_seq(LMN_NALLOC<BYTE>(cap)), hasuniq(false) {
    /* load(rule.amatch); */
    load(rule.body);
    name = rule.name;
    resolve_labels();
  }

  std::unique_ptr<LmnRule> create_rule() {
    return std::unique_ptr<LmnRule>(new LmnRule(byte_seq, cap, ANONYMOUS, hasuniq));
  }

  std::unique_ptr<LmnRule> create_subrule() {
    return std::unique_ptr<LmnRule>(new LmnRule(byte_seq, cap, name, hasuniq, true));
  }

  void load(InstBlock const &ib) {
    if (ib.has_label())
      label_loc[ib.label] = loc;

    for (auto &inst : ib.instrs)
      load(inst);
  }

  void resolve_labels() {
    for (auto &p : loc_label_ref) {
      auto loc        = p.first;
      auto label      = p.second;
      auto target_loc = label_loc.find(label);

      if (target_loc != label_loc.end()) {
        write_at<LmnJumpOffset>(target_loc->second - loc - sizeof(LmnJumpOffset), loc);
      } else {
        fprintf(stderr, "label not found L%d\n", label);
        lmn_fatal("implementation error");
      }
    }
  }

  void expand_byte_sec() {
    cap      *= 2;
    byte_seq = LMN_REALLOC<BYTE>(byte_seq, cap);
  }

  struct loader {
    ByteEncoder &enc;
    loader(ByteEncoder &enc) : enc(enc) {}
    template <typename T> void operator()(T &&v) { enc.load(std::forward<T>(v)); }
  };

  void load(Instruction const &inst) {
    write_forward<LmnInstrOp>(inst.id);
    auto arg_num = inst.args.size();

    /* REMOVEATOMは引数の数が2と3の場合がある。第三引数の
       ファンクタは無視する */
    if (inst.id == INSTR_REMOVEATOM && arg_num == 3) {
      arg_num = 2;
    }

    for (int i = 0; i < arg_num; i++)
      std::visit(loader(*this), inst.args[i]);

    /* ISGROUNDとCOPYGROUNDは引数の数が3と4の場合がある。3の場合は
       第４引数として空リストを追加する */
    if (inst.id == INSTR_ISGROUND || inst.id == INSTR_COPYGROUND) {
      if (arg_num == 3) {
        write_forward<LmnInstrVar>(0);
      }
    }
  }

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

  void load(std::monostate const &){};
  void load(il::functor::symbol const &functor) {
    write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
    write_forward<LmnFunctor>(functor.value);
  }
  void load(il::functor::integer const &functor) {
    write_forward<LmnLinkAttr>(LMN_INT_ATTR);
    write_forward<long>(functor.value);
  }
  void load(il::functor::real const &functor) {
    write_forward<LmnLinkAttr>(LMN_DBL_ATTR);
    write_forward<double>(functor.value);
  }
  void load(il::functor::string const &functor) {
    write_forward<LmnLinkAttr>(LMN_STRING_ATTR);
    write_forward<lmn_interned_str>(functor.value);
  }
  void load(il::functor::in_proxy const &functor) {
    write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
    write_forward<LmnFunctor>(LMN_IN_PROXY_FUNCTOR);
  }
  void load(il::functor::out_proxy const &functor) {
    write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
    write_forward<LmnFunctor>(LMN_OUT_PROXY_FUNCTOR);
  }
  void load(il::functor::unify const &functor) {
    write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
    write_forward<LmnFunctor>(LMN_UNIFY_FUNCTOR);
  }

  void load(il::instr_arg::var const &arg) { write_forward<LmnInstrVar>(arg.value); }
  void load(il::instr_arg::label const &arg) {
    loc_label_ref[loc] = arg.value;
    move_by<LmnJumpOffset>();
  }
  void load(il::instr_arg::string const &arg) { write_forward<lmn_interned_str>(arg.value); }
  void load(il::instr_arg::lineno const &arg) { write_forward<LmnLineNum>(arg.value); }
  void load(il::instr_arg::functor const &arg) { std::visit(loader(*this), arg.value); }
  void load(il::instr_arg::ruleset const &arg) { write_forward<LmnRulesetId>(arg.value); }
  void load(il::instr_arg::var_list const &arg) {
    const auto &var_list = arg.value;

    write_forward<LmnInstrVar>(var_list.size());
    for (const auto &v : var_list)
      std::visit(loader(*this), v);
  }
  void load(il::instr_arg::inst_list const &arg) {
    /* 命令列の長さを求めるため、開始位置を記録する */
    /* INSTR_NOTでサブ命令列の長さを知る必要がある */
    auto start = loc;
    move_by<LmnSubInstrSize>();

    for (const auto &inst : arg.value)
      load(inst);

    /* startの位置に現在の位置との差を書き込む */
    auto t = loc;
    loc    = start;
    write<LmnSubInstrSize>(t - (start + sizeof(LmnSubInstrSize)));
    loc = t;
  }
};

#endif /* BYTE_ENCODER_HPP */
