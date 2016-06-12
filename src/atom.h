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
#include "error.h"
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


/* プロキシの3番目の引数番号の領域を remove_proxy, insert_proxyで利用中。
 * 所属する膜へのポインタを持っている */

#define LMN_ATOM_ATTR(X)                ((LmnLinkAttr)(X))
#define LMN_ATTR_BYTES                  (sizeof(LmnLinkAttr))
#define LMN_ATTR_MASK                   (0x7fU)
#define LMN_ATTR_FLAG                   (0x80U)

#define LMN_ATOM(X)                     ((LmnAtom)(X))
#define LMN_SATOM(X)                    ((LmnSAtom)(X))

/* アトムリストからATOMのprev/nextアトムを取得/設定する.
 * アトムリストから履歴アトムを読み飛ばさないので, 呼び出し側で適宜なんとかする */
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
#define LMN_HLATOM_SET_LINK(ATOM, X)    (LMN_SATOM_SET_LINK(ATOM, 0, X))

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

/* operations for extended atom */
#define LMN_ATTR_IS_DATA_WITHOUT_EX(ATTR)  (LMN_ATTR_IS_DATA(ATTR) \
                                              && !LMN_ATTR_IS_HL(ATTR) )
#define LMN_ATTR_IS_EX(ATTR)               (LMN_ATTR_IS_DATA(ATTR) \
                                              && LMN_ATTR_IS_HL(ATTR) )
#define LMN_IS_EX_FUNCTOR(FUNC)            ((FUNC) == LMN_HL_FUNC)

/* link attribute of primitive data type */
/* low 7 bits of link attribute */

#define LMN_INT_ATTR                    (LMN_ATTR_FLAG | 0x00U)
#define LMN_DBL_ATTR                    (LMN_ATTR_FLAG | 0x01U)
#define LMN_SP_ATOM_ATTR                (LMN_ATTR_FLAG | 0x03U)
#define LMN_STRING_ATTR                 LMN_SP_ATOM_ATTR
/* 定数アトム */
#define LMN_CONST_STR_ATTR              (LMN_ATTR_FLAG | 0x04U)
#define LMN_CONST_DBL_ATTR              (LMN_ATTR_FLAG | 0x05U)
/* ハイパーリンクアトム (⊂ extended atom ⊂ data atom ⊂ unary) 
ハイパーリンクアトムはプロキシと同様シンボルアトムとしても扱われることに注意 */
#define LMN_HL_ATTR                     (LMN_ATTR_FLAG | 0x0aU)

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
static inline LmnAtom lmn_copy_atom(LmnAtom atom, LmnLinkAttr attr);
static inline LmnSAtom lmn_copy_satom(LmnSAtom atom);
static inline LmnAtom lmn_copy_data_atom(LmnAtom atom, LmnLinkAttr attr);
static inline LmnSAtom lmn_copy_satom_with_data(LmnSAtom atom, BOOL is_new_hl);
static inline void lmn_free_atom(LmnAtom atom, LmnLinkAttr attr);
static inline void free_symbol_atom_with_buddy_data(LmnSAtom atom);
static inline BOOL lmn_eq_func(LmnAtom atom0, LmnLinkAttr attr0,
                               LmnAtom atom1,LmnLinkAttr attr1);
static inline BOOL lmn_data_atom_is_ground(LmnAtom atom, LmnLinkAttr attr,
                                           ProcessTableRef *hlinks);
static inline BOOL lmn_data_atom_eq(LmnAtom atom1, LmnLinkAttr attr1,
                                    LmnAtom atom2, LmnLinkAttr attr2);
static inline double lmn_get_double(LmnAtom atom);
static inline LmnAtom lmn_create_double_atom(double d);
static inline void lmn_destroy_double_atom(LmnAtom atom);

#ifdef LMN_DOUBLE_IS_IMMEDIATE
# define LMN_GETREF_DOUBLE(Atom) ((double *)&Atom)
#else
# define LMN_GETREF_DOUBLE(Atom) ((double *)Atom)
#endif

#define LMN_COPY_DBL_ATOM(Dst, Src)                                            \
  do {                                                                         \
    (Dst) = (LmnWord)lmn_create_double_atom(lmn_get_double(Src));              \
  } while (0)


/* アトムをコピーして返す。
 * atomがシンボルアトムの場合、リンク先のデータアトムもコピーする */
static inline LmnAtom lmn_copy_atom(LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return lmn_copy_data_atom(atom, attr);
  } else { /* symbol atom */
    return LMN_ATOM(lmn_copy_satom(LMN_SATOM(atom)));
  }
}

static inline LmnSAtom lmn_copy_satom(LmnSAtom atom) {
  LmnSAtom newatom;
  LmnFunctor f;

  f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  newatom = lmn_new_atom(f);

  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES * LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));

  LMN_SATOM_SET_ID(newatom, 0);
  return newatom;
}

static inline LmnAtom lmn_copy_data_atom(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
    case LMN_INT_ATTR:
      return atom;
    case LMN_DBL_ATTR:
    {
      LmnAtom d;
      LMN_COPY_DBL_ATOM(d, atom);
      return d;
    }
    case LMN_SP_ATOM_ATTR:
      return LMN_ATOM(SP_ATOM_COPY(atom));
    case LMN_HL_ATTR:
    {
      LmnSAtom copyatom;
      copyatom = lmn_copy_satom(LMN_SATOM(atom));
      LMN_SATOM_SET_ID(copyatom, 0);

      lmn_hyperlink_copy(copyatom, LMN_SATOM(atom));
      return LMN_ATOM(copyatom);
    }
    default:
      LMN_ASSERT(FALSE);
      return -1;
  }
}
//is_new_hl = TRUEで新しく生成したハイパーリンクは元のハイパーリンクと接続しない
static inline LmnSAtom lmn_copy_satom_with_data(LmnSAtom atom, BOOL is_new_hl)
{
  LmnFunctor f;
  LmnSAtom newatom;
  unsigned int i, arity = LMN_SATOM_GET_LINK_NUM(atom);

  f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  newatom = lmn_new_atom(f);

  LMN_ASSERT(newatom != atom);

  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES * LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));
  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      if (is_new_hl && LMN_SATOM_GET_ATTR(atom, i) == LMN_HL_ATTR) { 
        LmnSAtom hlAtom = LMN_SATOM(LMN_SATOM_GET_LINK(atom, i));
        HyperLink *hl = lmn_hyperlink_at_to_hl(hlAtom);
        LmnSAtom new_hlAtom = lmn_hyperlink_new_with_attr(LMN_HL_ATTRATOM(hl), LMN_HL_ATTRATOM_ATTR(hl));
        LMN_SATOM_SET_LINK(newatom, i, new_hlAtom);
        LMN_SATOM_SET_ATTR(newatom, i, LMN_HL_ATTR);
        LMN_SATOM_SET_LINK(new_hlAtom, 0, newatom); 
        LMN_SATOM_SET_ATTR(new_hlAtom, 0, LMN_ATTR_MAKE_LINK(i));
      } else {
        LmnAtom dt = lmn_copy_data_atom(LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
        LMN_SATOM_SET_LINK(newatom, i, dt);
        if (LMN_SATOM_GET_ATTR(atom, i) == LMN_HL_ATTR) {
          LMN_HLATOM_SET_LINK(LMN_SATOM(dt), newatom); 
        }
      }
    }
  }

  LMN_SATOM_SET_ID(newatom, 0);

  return newatom;
}

static inline void free_data_atom(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
    case LMN_INT_ATTR:
      break;
    case LMN_DBL_ATTR:
      lmn_destroy_double_atom(atom);
      break;
    case LMN_CONST_STR_ATTR: /* FALLTHROUGH */
    case LMN_CONST_DBL_ATTR:
      break;
    case LMN_SP_ATOM_ATTR:
      SP_ATOM_FREE(atom);
      break;
    case LMN_HL_ATTR:
      lmn_hyperlink_delete(LMN_SATOM(atom));
      lmn_delete_atom(LMN_SATOM(atom));
      break;
    default:
      LMN_ASSERT(FALSE);
      break;
  }
}

/* O(ARITY) */
static inline void lmn_free_atom(LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    free_data_atom(atom, attr);
  } else { /* symbol atom */
    lmn_delete_atom(LMN_SATOM(atom));
  }
}

/* シンボルアトムとリンクで接続しているデータアトムを解放する */
static inline void free_symbol_atom_with_buddy_data(LmnSAtom atom)
{
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      free_data_atom(LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    }
  }

  if (LMN_FUNC_IS_HL(LMN_SATOM_GET_FUNCTOR(atom))) {
    lmn_hyperlink_delete(atom);
  }

  lmn_delete_atom(LMN_SATOM(atom));
}

static inline BOOL lmn_eq_func(LmnAtom     atom0, LmnLinkAttr attr0,
                               LmnAtom     atom1, LmnLinkAttr attr1) {
  /* TODO: TOFIX シンボルアトムのattrがすべて等しい値であることを確認する */
  if (attr0 != attr1) return FALSE;
  switch (attr0) {
  case LMN_INT_ATTR:
    return atom0 == atom1;
  case LMN_DBL_ATTR:
    return lmn_get_double(atom0) == lmn_get_double(atom1);
  case LMN_SP_ATOM_ATTR:
    return SP_ATOM_EQ(atom0, atom1);
  case LMN_HL_ATTR:
    return lmn_hyperlink_eq(LMN_SATOM(atom0), attr0, LMN_SATOM(atom1), attr1);
  default: /* symbol atom */
    return LMN_SATOM_GET_FUNCTOR(atom0) == LMN_SATOM_GET_FUNCTOR(atom1);
  }
}

static inline BOOL lmn_data_atom_is_ground(LmnAtom atom, LmnLinkAttr attr, ProcessTableRef *hlinks) {
  switch (attr) {
  case LMN_INT_ATTR:
  case LMN_DBL_ATTR:
  case LMN_HL_ATTR:
    return TRUE;
  case LMN_SP_ATOM_ATTR:
    return SP_ATOM_IS_GROUND(atom);
  default:
    lmn_fatal("Implementation error");
  }
}

static inline BOOL lmn_data_atom_eq(LmnAtom atom1, LmnLinkAttr attr1,
                                    LmnAtom atom2, LmnLinkAttr attr2) {
  if (attr1 != attr2) {
    return FALSE;
  }
  else {
    switch (attr1) {
    case LMN_INT_ATTR:
      return atom1 == atom2;
    case LMN_DBL_ATTR:
      return lmn_get_double(atom1) == lmn_get_double(atom2);
    case LMN_SP_ATOM_ATTR:
      return SP_ATOM_EQ(atom1, atom2);
    case LMN_HL_ATTR:
      return lmn_hyperlink_eq(LMN_SATOM(atom1), attr1, LMN_SATOM(atom2), attr2);
    default:
      lmn_fatal("Implementation error");
      return FALSE;
    }
  }
}

/* caller must ensure |atom| has LMN_DBL_ATTR */
static inline double lmn_get_double(LmnAtom atom) {
#ifdef LMN_DOUBLE_IS_IMMEDIATE
  return *(double *)&atom; // forward bit pattern
#else
  return *(double *)atom;
#endif
}

/* Create atom represents double data. Return value must be freed by |lmn_destroy_double_atom| */
static inline LmnAtom lmn_create_double_atom(double d) {
#ifdef LMN_DOUBLE_IS_IMMEDIATE
  return *(LmnAtom *)&d; // forward bit pattern
#else
  double *result = (double *)LMN_MALLOC(double);
  *result = d;
  return LMN_ATOM(result);
#endif
}

/* User don't call this function directly. Use |lmn_free_atom| instead. */
static inline void lmn_destroy_double_atom(LmnAtom atom) {
#ifndef LMN_DOUBLE_IS_IMMEDIATE
  LMN_FREE((double *)atom);
#endif
}

/* cldoc:end-category() */

#endif /* LMN_ATOM_H */

