/*
 * lmn_binstr.hpp
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

#ifndef SLIM_VERIFIER_MEM_ENCODE_LMN_BINSTR_HPP
#define SLIM_VERIFIER_MEM_ENCODE_LMN_BINSTR_HPP

#include <cstdint>
#include<iostream>
typedef struct LmnBinStr *LmnBinStrRef;

/* 最終的なエンコード結果を表すバイナリストリング */
struct LmnBinStr {
  bool type; /* バイト列への記録方式を記録しておくためのbit field.
              * 圧縮方式のメモ用に用いる.
              * (64bit環境ではアラインメントの隙間に配置されるのでメモリ使用量は増えないはず)
              */
  unsigned int len; /* 確保したbyte型の数(列の長さ) */
  uint8_t *v;          /* 1byte(8bit)の可変列へのポインタ */
  void dump_binstr() {
    for(int i=0; i<len; i++) {
      printf("<%02x>", v[i]);
    }
    std::cout << std::endl;
  }/*バイト列出力用*/
};

#endif /* SLIM_VERIFIER_MEM_ENCODE_BINSTR_HPP */
