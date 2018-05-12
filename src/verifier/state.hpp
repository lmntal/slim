/*
 * state.hpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

#ifndef LMN_STATE_HPP
#define LMN_STATE_HPP

#include "lmntal.h"
#include "vm/vm.h"

#include "state_defs.h"

/* Descriptor */
struct State {                /* Total:72(36)byte */
  unsigned int successor_num; /*  4(4)byte: サクセッサの数 */
  BYTE state_name; /*  1(1)byte: 同期積オートマトンの性質ラベル */
  BYTE flags;      /*  1(1)byte: フラグ管理用ビットフィールド */
  BYTE flags2;     /*  1(1)byte: フラグ管理用ビットフィールド2 */
  BYTE flags3; /*  1(1)byte: アラインメントの隙間(一時的にdpor_naiveで使用中) */
  unsigned long hash; /*  8(4)byte: 通常時: 膜memのハッシュ値, --mem-enc時:
                         膜の一意なバイト列のハッシュ値  */
  state_data_t data; /*  8(4)byte: 膜, バイナリストリングのどちらか */
  TreeCompressData
      tcd; /*  8(8)byte: Tree Compression 用のデータ無理やり8 Byteにしている */
  succ_data_t *successors; /*  8(4)byte: サクセッサポインタの配列 */
  State *next; /*  8(4)byte: 状態管理表に登録する際に必要なポインタ */
  State *parent; /*  8(4)byte: 自身を生成した状態へのポインタを持たせておく */
  unsigned long state_id; /*  8(4)byte: 生成順に割り当てる状態の整数ID */
  State *map; /*  8(4)byte: MAP値 or 最適化実行時の前状態 */
#ifndef MINIMAL_STATE
  BYTE *
      local_flags; /*  8(4)byte:
                      並列実行時、スレッド事に保持しておきたいフラグ(mcndfsのcyanフラグ等)
                    */
  pthread_mutex_t expand_lock;
  unsigned long expander_id;
  void state_set_expander_id(unsigned long id) { expander_id = id; }
  unsigned long state_expander_id() { return expander_id; }
#else
  void state_set_expander_id(unsigned long id) {}
  unsigned long state_expander_id() { return 0; }
#endif
#ifdef KWBT_OPT
  LmnCost cost; /*  8(4)byte: cost */
#endif
};

#endif
