/*
 * mem_encode.h
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
 */

#ifndef LMN_MEM_ENCODE_H
#define LMN_MEM_ENCODE_H

#include "lmntal.h"
#include "membrane.h"
#include "delta_membrane.h"

typedef struct LmnBinStr *LmnBinStr;

/* 最終的なエンコード結果を表すバイナリストリング */
struct LmnBinStr {
  BOOL comp_type;     /* バイト列の圧縮情報を記録しておくためのbit field
                       * (64bit環境ではアラインメントの隙間に配置されるのでメモリ使用量は増えないはず) */
  unsigned int len;   /* 確保したbyte型の数(列の長さ) */
  BYTE *v;            /* 1byte(8bit)の可変列へのポインタ */
};

#define TAG_BIT_SIZE      4
#define TAG_DATA_TYPE_BIT 2
#define TAG_IN_BYTE       2

void mem_isom_init(void);
void mem_isom_finalize(void);
void set_functor_priority(LmnFunctor f, int priority);

LmnBinStr lmn_mem_encode(LmnMembrane *mem);
LmnBinStr lmn_mem_encode_delta(struct MemDeltaRoot *d);
int binstr_compare(const LmnBinStr a, const LmnBinStr b);
unsigned long binstr_hash(const LmnBinStr a);
int binstr_byte_size(LmnBinStr p);
LmnBinStr lmn_binstr_make(unsigned int size);
LmnBinStr lmn_binstr_copy(LmnBinStr src_bs);
LmnMembrane *lmn_binstr_decode(const LmnBinStr bs);

BOOL lmn_mem_equals_enc(LmnBinStr bs, LmnMembrane *mem);

void lmn_binstr_free(LmnBinStr p);
void lmn_binstr_dump(const LmnBinStr bs);
unsigned long lmn_binstr_space(struct LmnBinStr *bs);
LmnBinStr lmn_mem_to_binstr(LmnMembrane *mem);
LmnBinStr lmn_mem_to_binstr_delta(struct MemDeltaRoot *d);

#endif /* LMN_MEM_ENCODE_H */
