/*
 * byte_encoder.cpp - Encode IL AST to bytecode.
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

#include "byte_encoder.hpp"

void ByteEncoder::load(const il::functor::symbol &functor) {
  write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
  write_forward<LmnFunctor>(functor.value);
}
void ByteEncoder::load(const il::functor::integer &functor) {
  write_forward<LmnLinkAttr>(LMN_INT_ATTR);
  write_forward<long>(functor.value);
}
void ByteEncoder::load(const il::functor::real &functor) {
  write_forward<LmnLinkAttr>(LMN_DBL_ATTR);
  write_forward<double>(functor.value);
}
void ByteEncoder::load(const il::functor::string &functor) {
  write_forward<LmnLinkAttr>(LMN_STRING_ATTR);
  write_forward<lmn_interned_str>(functor.value);
}
void ByteEncoder::load(const il::functor::in_proxy &functor) {
  write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
  write_forward<LmnFunctor>(LMN_IN_PROXY_FUNCTOR);
}
void ByteEncoder::load(const il::functor::out_proxy &functor) {
  write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
  write_forward<LmnFunctor>(LMN_OUT_PROXY_FUNCTOR);
}
void ByteEncoder::load(const il::functor::unify &functor) {
  write_forward<LmnLinkAttr>(LMN_ATTR_MAKE_LINK(0));
  write_forward<LmnFunctor>(LMN_UNIFY_FUNCTOR);
}

void ByteEncoder::load(const il::instr_arg::var &arg) {
  write_forward<LmnInstrVar>(arg.value);
}
void ByteEncoder::load(const il::instr_arg::label &arg) {
  loc_label_ref[loc] = arg.value;
  move_by<LmnJumpOffset>();
}
void ByteEncoder::load(const il::instr_arg::string &arg) {
  write_forward<lmn_interned_str>(arg.value);
}
void ByteEncoder::load(const il::instr_arg::lineno &arg) {
  write_forward<LmnLineNum>(arg.value);
}
void ByteEncoder::load(const il::instr_arg::functor &arg) { load(arg.value); }
void ByteEncoder::load(const il::instr_arg::ruleset &arg) {
  write_forward<LmnRulesetId>(arg.value);
}
void ByteEncoder::load(const il::instr_arg::var_list &arg) {
  auto &var_list = arg.value;

  write_forward<LmnInstrVar>(var_list.size());
  for (auto &v : var_list) {
    load(v);
  }
}
void ByteEncoder::load(const il::instr_arg::inst_list &arg) {
  /* 命令列の長さを求めるため、開始位置を記録する */
  /* INSTR_NOTでサブ命令列の長さを知る必要がある */
  auto start = loc;
  move_by<LmnSubInstrSize>();

  for (auto &inst : arg.value)
    load(inst);

  /* startの位置に現在の位置との差を書き込む */
  auto t = loc;
  loc = start;
  write<LmnSubInstrSize>(t - (start + sizeof(LmnSubInstrSize)));
  loc = t;
}
