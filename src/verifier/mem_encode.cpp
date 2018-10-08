/*
 * mem_encode.cpp
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
 */

/* 膜を一意のバイト列に変換する */

/* encode specification
 *
 * アトムや膜などの各要素をバイト列に書き込む際には、まず要素の種類を表す4ビットのタグを書き込み、
 * その次の位置から、それぞれの要素毎の値を書き込んでいく。
 *
 * 実装では、4ビットを一つの単位としてバイト列に値を書き込んでいく。
 *
 * atom
 *   tag         :  0000
 *   functor     : 2Byte
 *   arguments
 *
 * membrane
 *   tag:        :  0001
 *   elements    : (any length)
 *   end tag     :  0010
 *
 * named membrane
 *   tag         :  0011
 *   name        : 4Byte
 *   elements    : (any length)
 *   end tag     :  0010
 *
 * atom/hlink ref
 *   tag         :  0100
 *   ref id      : 4byte
 *   arg num     : 1byte
 *
 * mem ref
 *   tag         :  0101
 *   ref id      : 4byte
 *
 * escape from membrane
 *   tag         :  0110
 *
 * escape from membrane with data atom
 *   tag         :  0111
 *   data tag
 *   data value
 *
 * from of traversal
 *   tag         :  1000
 *
 * rule sets(only one ruleset)
 *   tag         :  1001
 *   ruleset id  : 2byte
 *
 * rule sets
 *   tag         :  1010
 *   ruleset num : 4byte
 *   foreach
 *     ruleset id  : 2byte
 *
 * uniq applied histories
 *   tag         :  1011
 *   ruleset num : 4byte
 *   foreach
 *     ruleset id  : 2byte
 *     history num : 4byte
 *     foreach
 *       histroy ids  : 4byte
 *
 * int atom
 *   tag         : 1100
 *   value       : 1 word
 *
 * double atom
 *   tag         : 1101
 *   value       : sizeof(double)
 *
 * string atom
 *   tag         : 1110
 *   value       : sizeof(lmn_interned_str)
 *
 * hlink object
 *   tag         : 1111
 *   rank        : 4byte
 *
 */
#include "mem_encode.h"
#include "binstr_compress.h"
#include "delta_membrane.h"
#include "element/element.h"
#include "mem_encode/binstr.hpp"
#include "mem_encode/decoder.hpp"
#include "mem_encode/dumper.hpp"
#include "mem_encode/encoder.hpp"
#include "mem_encode/halfbyte_scanner.hpp"
#include "mem_encode/equalizer.hpp"
#include "vm/vm.h"

#include <algorithm>
#include <memory>

#ifdef PROFILE
#include "runtime_status.h"
#endif

using slim::element::make_unique;

/* ファンクタ優先度付けの実装 cf) mem_idの高速化 @hori */
uint16_t functor_priority[FUNCTOR_MAX + 1];

/*----------------------------------------------------------------------
 * Initialization
 */

void mem_isom_init() {
  memset(functor_priority, 0xff, sizeof(uint16_t) * FUNCTOR_MAX + 1);
}

void mem_isom_finalize() {}

void set_functor_priority(LmnFunctor f, int priority) {
  if (priority <= 0)
    lmn_fatal("implementation error");
  functor_priority[f] = priority;
}

/*----------------------------------------------------------------------
 * Binary String
 */
LmnBinStrRef lmn_binstr_make(unsigned int real_len) {
  LmnBinStrRef bs = LMN_MALLOC(struct LmnBinStr);
  bs->len = real_len * TAG_IN_BYTE;
  bs->type = 0x00U;
  bs->v = LMN_NALLOC(BYTE, real_len);
  memset(bs->v, 0x0U, sizeof(BYTE) * real_len);
  return bs;
}

LmnBinStrRef lmn_binstr_copy(struct LmnBinStr *src_bs) {
  unsigned long v_len_real;
  struct LmnBinStr *dst_bs;

  v_len_real = ((src_bs->len + 1) / TAG_IN_BYTE);
  dst_bs = lmn_binstr_make(v_len_real);
  dst_bs->len = src_bs->len;

  memcpy(dst_bs->v, src_bs->v, v_len_real);

  return dst_bs;
}

void lmn_binstr_free(struct LmnBinStr *bs) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(bs));
  }
#endif
  LMN_FREE(bs->v);
  LMN_FREE(bs);
}

unsigned long lmn_binstr_space(struct LmnBinStr *bs) {
  /* TODO: アラインメントで切り上げる必要があるはず */
  return sizeof(struct LmnBinStr) +
         sizeof(BYTE) * ((bs->len + 1) / TAG_IN_BYTE);
}

namespace std {
template <> struct default_delete<LmnBinStr> {
  void operator()(LmnBinStr *bs) const { lmn_binstr_free(bs); }
};
} // namespace std

int binstr_byte_size(LmnBinStrRef p) {
  return (p->len / TAG_IN_BYTE) + sizeof(struct LmnBinStr);
}

/* バイナリストリングのハッシュ値を返す */
unsigned long binstr_hash(const LmnBinStrRef a) {
  unsigned long hval;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_start_timer(PROFILE_TIME__STATE_HASH_MID);
#endif

  hval = lmn_byte_hash(a->v, (a->len + 1) / TAG_IN_BYTE);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_finish_timer(PROFILE_TIME__STATE_HASH_MID);
#endif

  return hval;
}

/* バイナリストリングaとbの比較を行いaがbより、小さい、同じ、大きい場合に、
 * それぞれ負の値、0、正の値を返す。*/
int binstr_compare(const LmnBinStrRef a, const LmnBinStrRef b) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif

  auto ret = lmn_byte_cmp(a->v, (a->len + 1) / TAG_IN_BYTE, b->v,
                          (b->len + 1) / TAG_IN_BYTE);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif
  return ret;
}

/*----------------------------------------------------------------------
 * Membrane Encode
 * 膜を一意なバイナリストリングにエンコードする
 */

/* prototypes */

static LmnBinStrRef lmn_mem_encode_sub(LmnMembraneRef mem,
                                       unsigned long tbl_size);

/* memを一意なバイナリストリングに変換する */
LmnBinStrRef lmn_mem_encode(LmnMembraneRef mem) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_CANONICAL);
  }
#endif

  // ret = lmn_mem_encode_sub(mem, 1024);
  auto ret = lmn_mem_encode_sub(mem, round2up(env_next_id() + 1));

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__MENC_CANONICAL);
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
  }
#endif

  return ret;
}

LmnBinStrRef lmn_mem_encode_delta(struct MemDeltaRoot *d) {
  dmem_root_commit(d);
  auto ret_bs =
      lmn_mem_encode_sub(dmem_root_get_root_mem(d), dmem_root_get_next_id(d));
  dmem_root_revert(d);

  return ret_bs;
}

using slim::verifier::mem_encode::encoder;

/* memを一意なバイナリストリングに変換する */
static LmnBinStrRef lmn_mem_encode_sub(LmnMembraneRef mem,
                                       unsigned long tbl_size) {
  return encoder::encode(mem, tbl_size);
}

/*----------------------------------------------------------------------
 * Decode Binary String
 */

static LmnBinStrRef lmn_mem_to_binstr_sub(LmnMembraneRef mem,
                                          unsigned long tbl_size);

/* エンコードされた膜をデコードし、構造を再構築する */
LmnMembraneRef lmn_binstr_decode(const LmnBinStrRef bs) {
  /* MEMO:
   *   8bit列を, binary stringの長さ * TAG_IN_BYTE(== 2)だけ確保(少し多めになる)
   *   logは, 復元したプロセスへのポインタを持ち,
   * 出現(nvisited)順に先頭から積んでいく */
  auto target = is_comp_z(bs) ? lmn_bscomp_z_decode(bs) : bs;


#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_RESTORE);
  }
#endif

  auto groot = lmn_mem_make();

  lmn_mem_set_active(groot, TRUE); /* globalだから恒真 */
  binstr_decoder dec(target->v, target->len, target->pos_to_id);
  dec.decode_cell(groot, NULL, 0);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__MENC_RESTORE);
  }
#endif

  if (is_comp_z(bs)) {
    lmn_binstr_free(target);
  }
  return groot;
}

/*----------------------------------------------------------------------
 * Dump Membrane to Binary String
 */

LmnBinStrRef lmn_mem_to_binstr(LmnMembraneRef mem) {


#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  // ret = lmn_mem_to_binstr_sub(mem, 128);
  auto ret = lmn_mem_to_binstr_sub(mem, round2up(env_next_id() + 1));

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
    profile_finish_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  return ret;
}

/* 膜のdumpを計算する. dump_root_memとかから名称変更したみたい */
static LmnBinStrRef lmn_mem_to_binstr_sub(LmnMembraneRef mem,
                                          unsigned long tbl_size) {
  return encoder::dump(mem, tbl_size);
}

/*----------------------------------------------------------------------
 * Membrane Isomorphism
 */

static BOOL mem_equals_enc_sub(LmnBinStrRef bs, LmnMembraneRef mem,
                               unsigned long tbl_size) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  auto i_bs = 0;
  auto i_ref = VISITLOG_INIT_N;
// BS_MEMEQ_OLD is never defined
#ifndef BS_MEMEQ_OLD
  equalizer<TraceLog> e(bs, mem);
  auto t = e.check();

#else

  /* **とりあえず**これなら参照の数以上のサイズになる */
  BsDecodeLog *ref_log =
      LMN_NALLOC(BsDecodeLog, round2up(binstr_byte_size(bs) * TAG_IN_BYTE));
  auto log = visitlog_make_with_size(log, tbl_size);
  equalizer<VisitLog> e;
  auto t = e.mem_eq_enc_mols(bs, &i_bs, mem, ref_log, &i_ref, log)
           /* memに未訪問のプロセスが存在する場合, FALSE */
           && visitlog_element_num(log) == process_num(mem);
  visitlog_free(log);
  LMN_FREE(ref_log);
#endif

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  return t;
}

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc(LmnBinStrRef bs, LmnMembraneRef mem) {
  if (is_comp_z(bs)) {
    auto target = std::unique_ptr<LmnBinStr>(lmn_bscomp_z_decode(bs));
    return mem_equals_enc_sub(target.get(), mem, round2up(env_next_id() + 1));
  }

  return mem_equals_enc_sub(bs, mem, round2up(env_next_id() + 1));
}

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc_delta(LmnBinStrRef bs, struct MemDeltaRoot *d) {
  dmem_root_commit(d);
  auto t = lmn_mem_equals_enc(bs, dmem_root_get_root_mem(d));
  dmem_root_revert(d);

  return t;
}
