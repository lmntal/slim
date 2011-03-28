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
#include "atom.h"
#include "membrane.h"
#include "rule.h"
#include "functor.h"
#include <stdarg.h>

/* この辺の読み込みマクロはインタプリタ出力時も使えるはず */
/* translate.c内でも使えるはず */


#define READ_VAL_FUNC(I,X)                      \
  do{                                           \
    READ_VAL(LmnLinkAttr, I, X##_attr);         \
    switch(X##_attr){                                           \
    case LMN_INT_ATTR: READ_VAL(long, I, X.long_data); break;           \
    case LMN_DBL_ATTR: READ_VAL(double, I, X.double_data); break;       \
    case LMN_STRING_ATTR: READ_VAL(lmn_interned_str, I, X.string_data); break; \
    default: READ_VAL(LmnFunctor, I, X.functor_data); break;            \
    }                                                                   \
  }while(0)

#define READ_VAL_LIST(I,X)                      \
  do{                                                               \
    READ_VAL(LmnInstrVar, I, X##_num);                                  \
    X = malloc(sizeof(LmnWord)*X##_num);                                \
    { int i; for(i=0; i<X##_num; ++i){ READ_VAL(LmnInstrVar, I, X[i]); } } \
  }while(0)

struct trans_rule
{
  lmn_interned_str name;
  LmnTranslated function;
};

struct trans_ruleset
{
  int size;
  struct trans_rule *rules;
};

struct trans_module
{
  lmn_interned_str name;
  int ruleset; /* ruleset id */
};

struct trans_maindata
{
  int count_of_symbol;
  const char **symbol_table;
  int count_of_functor;
  LmnFunctorEntry *functor_table;
  int count_of_ruleset;
  struct trans_ruleset *ruleset_table;
  int count_of_module;
  struct trans_module *module_table;
  int *symbol_exchange;
  int *functor_exchange;
  int *ruleset_exchange;
};

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

/* 1ブロック相当を変換して出力し,読み込み終わった次の位置を返す */
/* findatomのような再帰的な変換が必要な場合に呼び出す */
const BYTE *translate_instructions(const BYTE *p,
                                   Vector *jump_points,
                                   const char *header,
                                   const char *successcode,
                                   const char *failcode,
                                   int indent);

/* 出力ファイル内で一時的に使うconst vectorをconst LmnWord[]から作る vectorの中を触るので注意 */
/* この戻り値の要素を書き換えるのも配列を拡張するのもvec_freeするのもダメ */
Vector vec_const_temporary_from_array(int size, const LmnWord *w);

/* vにwが含まれる場合そのindexを返す. 含まれない場合wをvの最後に追加してそのindexを返す */
int vec_inserted_index(Vector *v, LmnWord w);

/* トランスレート時に使う targX (X=argi)の名前でlistとlist_numを出力 */
void tr_print_list(int indent, int argi, int list_num, const LmnWord *list);

/* 単にn*2個空白を出力 */
void print_indent(int n);

/* 必要なサイズ分のバッファを勝手にmallocしてsprintf free必須 */
char *automalloc_sprintf(const char *format, ...);

/* 変換途中で見つけたcommitからルール名を拾う */
void set_translating_rule_name(lmn_interned_str rule_name);

/* 0番はなし,1番はsystemruleset,2番はinitialruleset,3番はinitialsystemruleset,普通のルールセットは4番から */
#define FIRST_ID_OF_NORMAL_RULESET 4

/* ファンクタを中間バイト列から読み込む時に使用する構造体 */
/* READ_DATA_ATOM等を吸収するために使いたい */
union LmnFunctorLiteral{
  long long_data;
  double double_data;
  lmn_interned_str string_data;
  LmnFunctor functor_data;
};

/* 現在ロードしている情報をfilepath.so の名前で使えるように出力する */
void translate(char *filepath);

#endif
