/*
 * decoder.hpp
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

#ifndef SLIM_VERIFIER_MEM_ENCODE_DECODER_HPP
#define SLIM_VERIFIER_MEM_ENCODE_DECODER_HPP

#include "halfbyte_scanner.hpp"

#include "vm/vm.h"

enum log_type : uint8_t {
  BS_LOG_TYPE_NONE = (0x0U),
  BS_LOG_TYPE_ATOM = (0x1U),
  BS_LOG_TYPE_MEM = (0x2U),
  BS_LOG_TYPE_HLINK = (0x3U),
};

struct BsDecodeLog {
  LmnWord v;
  log_type type;
};

struct binstr_decoder {
  halfbyte_scanner scanner;
  std::vector<BsDecodeLog> log;
  int nvisit; /* カウンタ(== 1): 順序付けを記録しながらデコードする.
               * (0はグローバルルート膜なので1から) */

  binstr_decoder(BYTE *bs, size_t len, size_t idx = 0)
      : scanner(bs, len, idx), log(len * TAG_IN_BYTE), nvisit(VISITLOG_INIT_N) {
  }

  int decode_cell(LmnMembraneRef mem, LmnSymbolAtomRef from_atom, int from_arg);

private:
  int decode_mol(LmnMembraneRef mem, LmnSymbolAtomRef from_atom, int from_arg);
  int decode_atom(LmnMembraneRef mem, LmnSymbolAtomRef from_atom, int from_arg);

public:
  void decode_rulesets(int rs_num, Vector *rulesets);
};

#endif
