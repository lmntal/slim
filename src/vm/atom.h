/*
 * atom.h
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
 * $Id: atom.h,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_ATOM_H
#define LMN_ATOM_H

/* cldoc:begin-category(Lmntal::Atom) */

#include "lmntal.h"
#include "special_atom.h"
#include "functor.h"
#include "element/element.h"
#include "symbol.h"
#include "hyperlink.h"

/** Atom Structure
 *
 *  * Atom
 *      1st Word      : アトムリストにおけるprevポインタ
 *      2nd Word      : アトムリストにおけるnextポインタ
 *      3rd Word      : アトムと膜に割り当てる一意な整数ID
 *      aligned Word  : 以下のByte要素の合計をWordサイズへアラインメント
 *        next 2 Bytes: アトムの種類(アトム名とリンク数の組)を表すfunctorに対応した整数ID
 *        N Bytes     : リンク属性   (1 Byte * N本)
 *      N Words       : リンクデータ (1 Word * N本)
 *    (Nはリンクの数)
 *
 *  * Link Attribute
 *     リンク属性は, 先頭1ビットが立っていない場合は, 下位7bitが接続先リンクの番号を記録しており,
 *                先頭1ビットが立っている場合は, Primitiveデータの種類を記録する。
 *     [Link Number]  0-------
 *     [int]          1000 0000
 *     [double]       1000 0001
 *     [special]      1000 0011
 *     [string]       1000 0011
 *     [const string] 1000 0100
 *     [const double] 1000 0101
 *     [hyper link]   1000 1010
 *
 *     We are going to support some primitive data types.
 *     (signed/unsigned) int, short int, long int, byte, long long int,
 *     float, double, long double,
 *     bool, string, character,
 *     ground array, ground with membrane array, primitive arrays
 *
 *     But, incompletely-specified.
 *
 */

typedef void *LmnAtomRef;
typedef LmnWord LmnDataAtomRef;
typedef struct LmnAtomData *LmnSymbolAtomRef;

#include "lmntal.h"


/* プロキシの3番目の引数番号の領域を remove_proxy, insert_proxyで利用中。
 * 所属する膜へのポインタを持っている */

#define LMN_ATOM_ATTR(X)                ((LmnLinkAttr)(X))
#define LMN_ATTR_MASK                   (0x7fU)
#define LMN_ATTR_FLAG                   (0x80U)

#define LMN_ATOM(X)                     ((LmnAtom)(X))
#define LMN_SATOM(X)                    ((LmnSAtom)(X))

/* アトムリストからATOMのprev/nextアトムを取得/設定する.
 * アトムリストから履歴アトムを読み飛ばさないので, 呼び出し側で適宜なんとかする */
LmnSymbolAtomRef LMN_SATOM_GET_PREV(LmnSymbolAtomRef atom);
void LMN_SATOM_SET_PREV(LmnSymbolAtomRef atom, LmnSymbolAtomRef prev);
LmnSymbolAtomRef LMN_SATOM_GET_NEXT_RAW(LmnSymbolAtomRef atom);
void LMN_SATOM_SET_NEXT(LmnSymbolAtomRef atom, LmnSymbolAtomRef next);


/* ファンクタIDの取得/設定, ファンクタIDからリンク数の取得のユーティリティ（プロキシはリンク1本分余分にデータ領域があるので分岐する） */

/* アトムATOMのプロセスIDを取得/設定 */
LmnWord LMN_SATOM_ID(LmnSymbolAtomRef atom);
void LMN_SATOM_SET_ID(LmnSymbolAtomRef atom, LmnWord id);

LmnFunctor LMN_SATOM_GET_FUNCTOR(LmnSymbolAtomRef atom);
void LMN_SATOM_SET_FUNCTOR(LmnSymbolAtomRef atom, LmnFunctor func);
int LMN_SATOM_GET_ARITY(LmnSymbolAtomRef atom);
int LMN_FUNCTOR_GET_LINK_NUM(LmnFunctor atom);
int LMN_SATOM_GET_LINK_NUM(LmnSymbolAtomRef atom);


/* アトムATOMのN番目のリンク属性/リンクデータを取得 */
LmnLinkAttr LMN_SATOM_GET_ATTR(LmnSymbolAtomRef atom, int n);
void LMN_SATOM_SET_ATTR(LmnSymbolAtomRef atom, int n, LmnLinkAttr attr);
LmnAtomRef LMN_SATOM_GET_LINK(LmnSymbolAtomRef atom, int n);
void LMN_SATOM_SET_LINK(LmnSymbolAtomRef atom, int n, LmnAtomRef v);
void LMN_HLATOM_SET_LINK(LmnSymbolAtomRef atom, LmnAtomRef v);

const LmnAtomRef *LMN_SATOM_PLINK(LmnSymbolAtomRef atom, int n);


/* size of atom の加算は prev, next, id, functorのワード */
size_t LMN_SATOM_SIZE(int arity);

/* リンク属性ATTRであるアトムATOMのファンクタがFUNCならばTRUEを返す */
BOOL LMN_HAS_FUNCTOR(LmnSymbolAtomRef ATOM, LmnLinkAttr ATTR, LmnFunctor FUNC);

/* operations for link attribute */
BOOL LMN_ATTR_IS_DATA(LmnLinkAttr attr);
/* make data/link link attribute from value */
LmnLinkAttr LMN_ATTR_MAKE_DATA(int X);
LmnLinkAttr LMN_ATTR_MAKE_LINK(int X);
/* get link attribute value (remove tag) */
int LMN_ATTR_GET_VALUE(int X);
/* set link attribute value. Tag is not changed. */
void LMN_ATTR_SET_VALUE(LmnLinkAttr *PATTR, int X);

/* get/set membrane of proxy */
BOOL LMN_SATOM_IS_PROXY(LmnSymbolAtomRef ATOM);
LmnMembraneRef LMN_PROXY_GET_MEM(LmnSymbolAtomRef PROXY_ATM);
void LMN_PROXY_SET_MEM(LmnSymbolAtomRef PROXY_ATM, LmnMembraneRef X);
BOOL LMN_IS_PROXY_FUNCTOR(LmnFunctor FUNC);
BOOL LMN_IS_SYMBOL_FUNCTOR(LmnFunctor FUNC);

const char *LMN_SATOM_STR(LmnSymbolAtomRef ATOM);
const char *LMN_FUNCTOR_STR(LmnFunctor F);

/* operations for extended atom */
BOOL LMN_ATTR_IS_DATA_WITHOUT_EX(LmnLinkAttr ATTR);
BOOL LMN_ATTR_IS_EX(LmnLinkAttr ATTR);
BOOL LMN_IS_EX_FUNCTOR(LmnFunctor FUNC);

/* link attribute of primitive data type */
/* low 7 bits of link attribute */
enum {
  LMN_INT_ATTR       = LMN_ATTR_FLAG | 0x00U,
  LMN_DBL_ATTR       = LMN_ATTR_FLAG | 0x01U,
  LMN_SP_ATOM_ATTR   = LMN_ATTR_FLAG | 0x03U,
  LMN_STRING_ATTR    = LMN_SP_ATOM_ATTR,
  /* 定数アトム */
  LMN_CONST_STR_ATTR = LMN_ATTR_FLAG | 0x04U,
  LMN_CONST_DBL_ATTR = LMN_ATTR_FLAG | 0x05U,
  /* ハイパーリンクアトム (⊂ extended atom ⊂ data atom ⊂ unary) 
  ハイパーリンクアトムはプロキシと同様シンボルアトムとしても扱われることに注意 */
  LMN_HL_ATTR        = LMN_ATTR_FLAG | 0x0aU
};

/*----------------------------------------------------------------------
 * allocation
 */

LmnSymbolAtomRef lmn_new_atom(LmnFunctor f);
void lmn_delete_atom(LmnSymbolAtomRef ap);
void free_atom_memory_pools(void);



/*----------------------------------------------------------------------
 * functions
 */
void mpool_init(void);
LmnAtomRef lmn_copy_atom(LmnAtomRef atom, LmnLinkAttr attr);
LmnSymbolAtomRef lmn_copy_satom(LmnSymbolAtomRef atom);
LmnDataAtomRef lmn_copy_data_atom(LmnDataAtomRef atom, LmnLinkAttr attr);
LmnSymbolAtomRef lmn_copy_satom_with_data(LmnSymbolAtomRef atom, BOOL is_new_hl);
void lmn_free_atom(LmnAtomRef atom, LmnLinkAttr attr);
void free_symbol_atom_with_buddy_data(LmnSymbolAtomRef atom);
BOOL lmn_eq_func(LmnAtomRef atom0, LmnLinkAttr attr0,
                 LmnAtomRef atom1,LmnLinkAttr attr1);
BOOL lmn_data_atom_is_ground(LmnDataAtomRef atom, LmnLinkAttr attr,
                             ProcessTableRef *hlinks);
BOOL lmn_data_atom_eq(LmnDataAtomRef atom1, LmnLinkAttr attr1,
                      LmnDataAtomRef atom2, LmnLinkAttr attr2);
double lmn_get_double(LmnDataAtomRef atom);
LmnDataAtomRef lmn_create_double_atom(double d);
void lmn_destroy_double_atom(LmnDataAtomRef atom);

#ifdef LMN_DOUBLE_IS_IMMEDIATE
# define LMN_GETREF_DOUBLE(Atom) ((double *)&Atom)
#else
# define LMN_GETREF_DOUBLE(Atom) ((double *)Atom)
#endif

#define LMN_COPY_DBL_ATOM(Dst, Src)                                            \
  do {                                                                         \
    (Dst) = (LmnWord)lmn_create_double_atom(lmn_get_double(Src));              \
  } while (0)


/* cldoc:end-category() */

#endif /* LMN_ATOM_H */

