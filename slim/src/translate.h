/*
 * translate.h
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
 * $Id: translate.h,v 1.4 2008/09/29 04:47:03 taisuke Exp $
 */

#ifndef LMN_TRANSLATE_H
#define LMN_TRANSLATE_H

#include "lmntal.h"

/* 自動生成される 1つ命令を変換して出力し,次変換する位置を返す */
const BYTE *translate_instruction_generated(const BYTE *instr, /* 変換する場所 */
                                            Vector *jump_points, /* 変換対象の場所 */
                                            const char *header, /* その場所が属する */
                                            const char *successcode, /* 成功時実行するコード */
                                            const char *failcode, /* */
                                            int indent, /* */
                                            int *finishflag); /* 変換の結果(正なら成功+継続,0なら成功+終了,負なら失敗 */

/* 手動生成 1つ命令を変換して出力し,次変換する位置を返す */
const BYTE *translate_instruction(const BYTE *instr,
                                            Vector *jump_points,
                                            const char *header,
                                            const char *successcode,
                                            const char *failcode,
                                            int indent,
                                            int *finishflag);

/* vにwが含まれる場合そのindexを返す. 含まれない場合wをvの最後に追加してそのindexを返す */
int vec_inserted_index(Vector *v, LmnWord w);

/* 現在ロードしている情報をfilepath.so の名前で使えるように出力する */
void translate(char *filepath);

#endif

