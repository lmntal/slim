/*
 * halfbyte_scanner.hpp
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

#ifndef SLIM_VERIFIER_MEM_ENCODE_HALFBYTE_SCANNER_HPP
#define SLIM_VERIFIER_MEM_ENCODE_HALFBYTE_SCANNER_HPP

#include "binstr.hpp"

#include "lmntal.h"

#include <cstddef>
#include <cstdint>

/* bsの位置posから1バイト読み込み，返す */
static BYTE binstr_get_byte(BYTE *bs, int pos) {
  return (BS_GET(bs, pos + 1)) | (BS_GET(bs, pos) << 4);
}

static uint16_t binstr_get_uint16(BYTE *bs, int pos) {
  return (uint16_t)((binstr_get_byte(bs, pos + 2)) |
                    ((binstr_get_byte(bs, pos)) << 8));
}

static uint32_t binstr_get_uint32(BYTE *bs, int pos) {
  return (uint32_t)(binstr_get_uint16(bs, pos + 4) |
                    (binstr_get_uint16(bs, pos) << 16));
}

static uint64_t binstr_get_uint64(BYTE *bs, int pos) {
  return (uint64_t)(binstr_get_uint32(bs, pos + 8) |
                    (uint64_t)binstr_get_uint32(bs, pos) << 32);
}


/* LMNtal言語はinteger valueの範囲を1wordとしている(型がない)ため, long型で良い
 */
static long binstr_get_int(BYTE *bs, int pos) {
#if SIZEOF_LONG == 4
  return (long)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (long)binstr_get_uint64(bs, pos);
#else
#error "not supported"
#endif
}

static double binstr_get_dbl(BYTE *bs, int pos) {
  int i;
  union {
    double d;
    BYTE t[8];
  } v;

  for (i = 7; i >= 0; i--) {
    v.t[i] = binstr_get_byte(bs, pos + (7 - i) * TAG_IN_BYTE);
  }

  return v.d;
}

static LmnWord binstr_get_word(BYTE *bs, int pos) LMN_UNUSED;
static LmnWord binstr_get_word(BYTE *bs, int pos) {
#if SIZEOF_LONG == 4
  return (LmnWord)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (LmnWord)binstr_get_uint64(bs, pos);
#else
#error "not supported"
#endif
}

static LmnFunctor binstr_get_functor(BYTE *bs, int pos) {
  LMN_ASSERT(sizeof(LmnFunctor) == 2);
  long f = binstr_get_uint16(bs, pos);
  return (LmnFunctor)(FUNCTOR_MAX - f);
}

static unsigned int binstr_get_ref_num(BYTE *bs, int pos) {
  LMN_ASSERT(BS_PROC_REF_SIZE == (sizeof(uint32_t) * TAG_IN_BYTE));
  return (unsigned int)binstr_get_uint32(bs, pos);
}

static unsigned int binstr_get_arg_ref(BYTE *bs, int pos) {
  LMN_ASSERT(BS_ATOM_REF_ARG_SIZE == 2);
  return binstr_get_byte(bs, pos);
}

static lmn_interned_str binstr_get_mem_name(BYTE *bs, int pos) {
  return binstr_get_uint32(bs, pos);
}

static long binstr_get_ruleset_num(BYTE *bs, int pos) {
  return binstr_get_uint32(bs, pos);
}

/* ruleset id(2byte)の取得 */
static long binstr_get_ruleset(BYTE *bs, int pos) {
  return binstr_get_uint16(bs, pos);
}

static lmn_interned_str binstr_get_strid(BYTE *bs, int pos) {
  LMN_ASSERT(BS_HISTORY_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

static long binstr_get_history_num(BYTE *bs, int pos) {
  LMN_ASSERT(BS_HISTORY_NUM_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

static lmn_interned_str binstr_get_history(BYTE *bs, int pos) {
  return binstr_get_strid(bs, pos);
}

struct halfbyte_scanner {
  uint8_t *bs;
  size_t size;
  size_t index;

public:
  halfbyte_scanner(uint8_t *bs, size_t size, size_t index = 0)
      : bs(bs), size(size), index(index) {}

  size_t location() const { return index; }

  uint8_t scan_tag() {
    uint8_t result = BS_GET(bs, index);
    index++;
    return result;
  }

  long scan_ruleset_num() {
    auto res = binstr_get_uint32(bs, index);
    index += BS_RULESET_NUM_SIZE;
    return res;
  }

  LmnRulesetId scan_ruleset() {
    auto result = binstr_get_uint16(bs, index);
    index += BS_RULESET_SIZE;
    return result;
  }

  long scan_history_num() {
    LMN_ASSERT(BS_HISTORY_NUM_SIZE == 8);
    auto res = binstr_get_uint32(bs, index);
    index += BS_HISTORY_NUM_SIZE;
    return res;
  }

  lmn_interned_str scan_history() {
    LMN_ASSERT(BS_HISTORY_SIZE == 8);
    auto res = binstr_get_uint32(bs, index);
    index += BS_HISTORY_SIZE;
    return res;
  }

  lmn_interned_str scan_mem_name() {
    auto res = binstr_get_uint32(bs, index);
    index += BS_MEM_NAME_SIZE;
    return res;
  }

  long scan_integer() {
#if SIZEOF_LONG == 4
    auto res = (long)binstr_get_uint32(bs, index);
#elif SIZEOF_LONG == 8
    auto res = (long)binstr_get_uint64(bs, index);
#else
#error "not supported"
#endif
    index += BS_INT_SIZE;
    return res;
  }

  double scan_double() {
    int i;
    union {
      double d;
      uint8_t t[8];
    } v;

    for (i = 7; i >= 0; i--) {
      v.t[i] = binstr_get_byte(bs, index + (7 - i) * TAG_IN_BYTE);
    }

    auto res = v.d;
    index += BS_DBL_SIZE;
    return res;
  }

  lmn_interned_str scan_strid() {
    LMN_ASSERT(BS_HISTORY_SIZE == 8);
    auto res = binstr_get_uint32(bs, index);
    index += BS_STR_ID_SIZE;
    return res;
  }

  LmnFunctor scan_functor() {
    LMN_ASSERT(sizeof(LmnFunctor) == 2);
    long f = binstr_get_uint16(bs, index);
    auto result = (LmnFunctor)(FUNCTOR_MAX - f);
    index += BS_FUNCTOR_SIZE;
    return result;
  }

  unsigned int scan_hlink_num() {
    LMN_ASSERT(BS_PROC_REF_SIZE == (sizeof(uint32_t) * TAG_IN_BYTE));
    auto res = (unsigned int)binstr_get_uint32(bs, index);
    index += BS_HLINK_NUM_SIZE;
    return res;
  }

  unsigned int scan_ref_num() {
    LMN_ASSERT(BS_PROC_REF_SIZE == (sizeof(uint32_t) * TAG_IN_BYTE));
    auto res = (unsigned int)binstr_get_uint32(bs, index);
    index += BS_PROC_REF_SIZE;
    return res;
  }

  unsigned int scan_arg_ref() {
    LMN_ASSERT(BS_ATOM_REF_ARG_SIZE == 2);
    auto res = binstr_get_byte(bs, index);
    index += BS_ATOM_REF_ARG_SIZE;
    return res;
  }

  void unput_tag() { index--; }
};

#endif /* SLIM_VERIFIER_MEM_ENCODE_HALFBYTE_SCANNER_HPP */
