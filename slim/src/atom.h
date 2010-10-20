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

#include "lmntal.h"
#include "special_atom.h"
#include "functor.h"
#include "error.h"
#include "symbol.h"

/** Atom Structure
 *
 *  * Atom
 *      1st Word      : アトムリストにおけるprevポインタ
 *      2nd Word      : アトムリストにおけるnextポインタ
 *      3rd Word      : アトムと膜に割り当てる一意な整数ID
 *      aligned Word  : 以下のByte要素の合計をWordサイズへアラインメント
 *        next 2 Bytes: アトムの種類(アトム名とリンク数の組)を表すfunctorに対応した整数ID
 *        N Bytes     : リンク属性
 *      N Words       : リンクデータ
 *    (Nはリンクの数)
 *
 *  * Link Attribute
 *     リンク属性は, 先頭1ビットが立っている場合は, リンク番号を表現しており,
 *                  先頭1ビットが立っていない場合は, ProxyアトムやPrimitiveデータを表現する。
 *     [Link Number]  0-------
 *     [int]          10000000
 *     [double]       10000001
 *     We are going to support some primitive data types.
 *     (signed/unsigned) int, short int, long int, byte, long long int,
 *     float, double, long double,
 *     bool, string, character,
 *     ground array, ground with membrane array, primitive arrays
 *
 *     But, incompletely-specified.
 *
 */

/* プロキシの3番目の引数番号の領域を remove_proxy, insert_proxyで利用中。
 * 所属する膜へのポインタを持っている */

#define LMN_ATOM_ATTR(X)                ((LmnLinkAttr)(X))
#define LMN_ATTR_BYTES                  (sizeof(LmnLinkAttr))
#define LMN_ATTR_MASK                   (0x7fU)
#define LMN_ATTR_FLAG                   (0x80U)

#define LMN_ATOM(X)                     ((LmnAtom)(X))
#define LMN_SATOM(X)                    ((LmnSAtom)(X))

/* アトムリストからATOMのprev/nextアトムを取得/設定する. RAWはアトムリストから履歴アトムを読み飛ばさない */
#define LMN_SATOM_PPREV(ATOM)           (((LmnWord *)(ATOM)))
#define LMN_SATOM_PNEXT(ATOM)           (((LmnWord *)(ATOM)) + 1)
#define LMN_SATOM_GET_PREV(ATOM)        (LMN_SATOM(*LMN_SATOM_PPREV(LMN_SATOM(ATOM))))
#define LMN_SATOM_SET_PREV(ATOM, X)     (*LMN_SATOM_PPREV(LMN_SATOM(ATOM)) = LMN_ATOM((X)))
#define LMN_SATOM_GET_NEXT_RAW(ATOM)    (LMN_SATOM(*LMN_SATOM_PNEXT(LMN_SATOM(ATOM))))
#define LMN_SATOM_SET_NEXT(ATOM, X)     (*LMN_SATOM_PNEXT(LMN_SATOM(ATOM)) = LMN_ATOM((X)))

/* リンク番号のタグのワード数。ファンクタと同じワードにある分は数えない */
#define LMN_ATTR_WORDS(ARITY)           (((ARITY + (LMN_WORD_BYTES - 1))) >> LMN_WORD_SHIFT)

/* ファンクタIDの取得/設定, ファンクタIDからリンク数の取得のユーティリティ（プロキシはリンク1本分余分にデータ領域があるので分岐する） */

/* アトムATOMのプロセスIDを取得/設定 */
#ifdef TIME_OPT
#  define LMN_SATOM_ID(ATOM)              (*(((LmnWord *)(ATOM)) + 2))
#  define LMN_SATOM_SET_ID(ATOM, ID)      (LMN_SATOM_ID(ATOM) = (ID))
#  define LMN_FUNCTOR_SHIFT               (3)
#  define LMN_LINK_SHIFT                  (4)
#else
#  define LMN_SATOM_ID(ATOM)              ((LmnWord)(ATOM))
#  define LMN_SATOM_SET_ID(ATOM, ID)
#  define LMN_FUNCTOR_SHIFT               (2)
#  define LMN_LINK_SHIFT                  (3)
#endif

#define LMN_SATOM_GET_FUNCTOR(ATOM)     LMN_FUNCTOR(*(((LmnWord *)LMN_SATOM(ATOM)) + LMN_FUNCTOR_SHIFT))
#define LMN_SATOM_SET_FUNCTOR(ATOM, X)  (*(LmnFunctor*)((LmnWord*)(ATOM) + LMN_FUNCTOR_SHIFT) = (X))
#define LMN_SATOM_GET_ARITY(ATOM)       (LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(LMN_SATOM(ATOM))))
#define LMN_FUNCTOR_GET_LINK_NUM(F)     ((LMN_FUNCTOR_ARITY(F)) - (LMN_IS_PROXY_FUNCTOR(F) ? 1U : 0U))
#define LMN_SATOM_GET_LINK_NUM(ATOM)    (LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(LMN_SATOM(ATOM))))

/* アトムATOMのN番目のリンク属性/リンクデータを取得 */
#define LMN_SATOM_PATTR(ATOM, N)        ((LmnLinkAttr *)(((BYTE *)(((LmnWord *)(ATOM)) + LMN_LINK_SHIFT)) + (N) * LMN_ATTR_BYTES))
#define LMN_SATOM_PLINK(ATOM,N)         (((LmnWord *)(ATOM)) + LMN_LINK_SHIFT + LMN_ATTR_WORDS(LMN_SATOM_GET_ARITY(ATOM)) + (N))
#define LMN_SATOM_GET_ATTR(ATOM, N)     (*LMN_SATOM_PATTR(LMN_SATOM(ATOM), N))
#define LMN_SATOM_SET_ATTR(ATOM, N, X)  ((*LMN_SATOM_PATTR(LMN_SATOM(ATOM), N)) = (X))
#define LMN_SATOM_GET_LINK(ATOM, N)     (LMN_ATOM(*LMN_SATOM_PLINK(LMN_SATOM(ATOM), N)))
#define LMN_SATOM_SET_LINK(ATOM, N, X)  (*LMN_SATOM_PLINK(LMN_SATOM(ATOM), N) = (LmnWord)(X))

/* word size of atom の加算は prev, next, id, functorのワード */
#define LMN_SATOM_WORDS(ARITY)          (LMN_LINK_SHIFT + LMN_ATTR_WORDS(ARITY) + (ARITY))

/* リンク属性ATTRであるアトムATOMのファンクタがFUNCならばTRUEを返す */
#define LMN_HAS_FUNCTOR(ATOM, ATTR, FUNC) \
          (LMN_ATTR_IS_DATA(ATTR) ? FALSE \
                                  : LMN_SATOM_GET_FUNCTOR(LMN_SATOM(ATOM)) == (FUNC))

/* operations for link attribute */
#define LMN_ATTR_IS_DATA(X)             ((X) & ~LMN_ATTR_MASK)
/* make data/link link attribute from value */
#define LMN_ATTR_MAKE_DATA(X)           (0x80U | (X))
#define LMN_ATTR_MAKE_LINK(X)           (X)
/* get link attribute value (remove tag) */
#define LMN_ATTR_GET_VALUE(X)           ((X) & LMN_ATTR_MASK)
/* set link attribute value. Tag is not changed. */
#define LMN_ATTR_SET_VALUE(PATTR, X)    (*(PATTR) = ((((X) & ~LMN_ATTR_MASK)) | X))

/* get/set membrane of proxy */
#define LMN_SATOM_IS_PROXY(ATOM)        (LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR((ATOM))))
#define LMN_PROXY_GET_MEM(PROXY_ATM)    ((LmnMembrane *)LMN_SATOM_GET_LINK((PROXY_ATM), 2))
#define LMN_PROXY_SET_MEM(PROXY_ATM, X) LMN_SATOM_SET_LINK((PROXY_ATM), 2, (LmnWord)(X))
#define LMN_PROXY_FUNCTOR_NUM           (3)
#define LMN_IS_PROXY_FUNCTOR(FUNC)      ((FUNC) < LMN_PROXY_FUNCTOR_NUM)
#define LMN_IS_SYMBOL_FUNCTOR(FUNC)     ((FUNC) >= LMN_PROXY_FUNCTOR_NUM)

#define LMN_SATOM_STR(ATOM)             LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(LMN_SATOM(ATOM))))
#define LMN_FUNCTOR_STR(F)              LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(F))

/* link attribute of primitive data type */
/* low 7 bits of link attribute */

#define LMN_INT_ATTR                    (LMN_ATTR_FLAG | 0)
#define LMN_DBL_ATTR                    (LMN_ATTR_FLAG | 1)
#define LMN_SP_ATOM_ATTR                (LMN_ATTR_FLAG | 2)
#define LMN_STRING_ATTR                 LMN_SP_ATOM_ATTR
/* 定数アトム */
#define LMN_CONST_STR_ATTR              (LMN_ATTR_FLAG | 3)
#define LMN_CONST_DBL_ATTR              (LMN_ATTR_FLAG | 4)


/*----------------------------------------------------------------------
 * allocation
 */

LmnSAtom lmn_new_atom(LmnFunctor f);
void lmn_delete_atom(LmnSAtom ap);
void free_atom_memory_pools(void);



/*----------------------------------------------------------------------
 * functions
 */
void mpool_init(void);
inline static LmnAtom lmn_copy_atom(LmnAtom atom, LmnLinkAttr attr);
inline static LmnSAtom lmn_copy_satom(LmnSAtom atom);
inline static LmnAtom lmn_copy_data_atom(LmnAtom atom, LmnLinkAttr attr);
inline static LmnSAtom lmn_copy_satom_with_data(LmnSAtom atom);
inline static void lmn_free_atom(LmnAtom atom, LmnLinkAttr attr);
inline static void free_symbol_atom_with_buddy_data(LmnSAtom atom);
inline static BOOL lmn_eq_func(LmnAtom atom0, LmnLinkAttr attr0,
                               LmnAtom atom1,LmnLinkAttr attr1);
inline static BOOL lmn_data_atom_is_ground(LmnAtom atom, LmnLinkAttr attr);
inline static BOOL lmn_data_atom_eq(LmnAtom atom1, LmnLinkAttr attr1,
                                    LmnAtom atom2, LmnLinkAttr attr2);

#define LMN_COPY_DBL_ATOM(dest, src) \
  do { \
    (dest) = (double *)LMN_ATOM(LMN_MALLOC(double));    \
    *((double *)dest) = *(double*)(src);       \
  } while (0)


/* アトムをコピーして返す。
 * atomがシンボルアトムの場合、リンク先のデータアトムもコピーする */
inline static LmnAtom lmn_copy_atom(LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return lmn_copy_data_atom(atom, attr);
  } else { /* symbol atom */
    return LMN_ATOM(lmn_copy_satom(LMN_SATOM(atom)));
  }
}

inline static LmnSAtom lmn_copy_satom(LmnSAtom atom) {
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  LmnSAtom newatom = lmn_new_atom(f);

  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES*LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));

  LMN_SATOM_SET_ID(newatom, 0);
  return newatom;
}

inline static LmnAtom lmn_copy_data_atom(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    return atom;
  case LMN_DBL_ATTR:
    {
      double *d;
      LMN_COPY_DBL_ATOM(d, atom);
      *d = *(double*)atom;
      return LMN_ATOM(d);
    }
  case LMN_SP_ATOM_ATTR:
    return LMN_ATOM(SP_ATOM_COPY(atom));
  default:
    LMN_ASSERT(FALSE);
    return -1;
  }
}

inline static LmnSAtom lmn_copy_satom_with_data(LmnSAtom atom)
{
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  LmnSAtom newatom = lmn_new_atom(f);
  unsigned int i, arity = LMN_SATOM_GET_LINK_NUM(atom);

  LMN_ASSERT(newatom != atom); /* たまに起こる未解決のバグ */

  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES * LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));
  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      LMN_SATOM_SET_LINK(newatom, i,
                         lmn_copy_data_atom(LMN_SATOM_GET_LINK(atom, i),
                                            LMN_SATOM_GET_ATTR(atom, i)));
    }
  }

  LMN_SATOM_SET_ID(newatom, 0);

  return newatom;
}

inline static void free_data_atom(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    break;
  case LMN_DBL_ATTR:
    LMN_FREE((double*)atom);
    break;
  case LMN_CONST_STR_ATTR:
  case LMN_CONST_DBL_ATTR:
    break;
  case LMN_SP_ATOM_ATTR:
    SP_ATOM_FREE(atom);
    break;
  default:
    LMN_ASSERT(FALSE);
    break;
  }
  return;
}

/* O(ARITY) */
inline static void lmn_free_atom(LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    free_data_atom(atom, attr);
  } else { /* symbol atom */
    lmn_delete_atom(LMN_SATOM(atom));
  }
}

/* シンボルアトムとリンクで接続しているデータアトムを解放する */
inline static void free_symbol_atom_with_buddy_data(LmnSAtom atom)
{
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      free_data_atom(LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    }
  }
  lmn_delete_atom(LMN_SATOM(atom));
}

inline static BOOL lmn_eq_func(LmnAtom     atom0, LmnLinkAttr attr0,
                               LmnAtom     atom1, LmnLinkAttr attr1) {
  /* TODO: TOFIX シンボルアトムのattrがすべて等しい値であることを確認する */
  if (attr0 != attr1) return FALSE;
  switch (attr0) {
  case LMN_INT_ATTR:
    return atom0 == atom1;
  case LMN_DBL_ATTR:
    return *(double *)atom0 == *(double *)atom1;
  default: /* symbol atom */
    return LMN_SATOM_GET_FUNCTOR(atom0) == LMN_SATOM_GET_FUNCTOR(atom1);
  }
}

inline static BOOL lmn_data_atom_is_ground(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
  case LMN_DBL_ATTR:
    return TRUE;
  case LMN_SP_ATOM_ATTR:
    return SP_ATOM_IS_GROUND(atom);
  default:
    lmn_fatal("Implementation error");
  }
}

inline static BOOL lmn_data_atom_eq(LmnAtom atom1, LmnLinkAttr attr1,
                                    LmnAtom atom2, LmnLinkAttr attr2) {
  if (attr1 != attr2) return FALSE;
  switch (attr1) {
  case LMN_INT_ATTR:
    return atom1 == atom2;
  case LMN_DBL_ATTR:
    return *(double*)atom1 == *(double*)atom2;
  case LMN_SP_ATOM_ATTR:
    return SP_ATOM_EQ(atom1, atom2);
  default:
    lmn_fatal("Implementation error");
  }
}

#endif /* LMN_ATOM_H */

