/*
 * atom.cpp
 *
 *   Copyright (c) 2016, Ueda Laboratory LMNtal Group
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
 * $Id $
 */

#include "atom.h"
#include <stddef.h>

struct LmnAtomData {
  LmnSymbolAtomRef prev;
  LmnSymbolAtomRef next;
  LmnWord procId;
  union {
    struct {
      LmnFunctor functor;
      LmnLinkAttr attr[0];
    };
    LmnAtomRef links[0];
  };
};

LmnSymbolAtomRef LMN_SATOM_GET_PREV(LmnSymbolAtomRef atom) {
  return atom->prev;
}
void LMN_SATOM_SET_PREV(LmnSymbolAtomRef atom, LmnSymbolAtomRef prev) {
  atom->prev = prev;
}
LmnSymbolAtomRef LMN_SATOM_GET_NEXT_RAW(LmnSymbolAtomRef atom) {
  return atom->next;
}
void LMN_SATOM_SET_NEXT(LmnSymbolAtomRef atom, LmnSymbolAtomRef next) {
  atom->next = next;
}

LmnWord LMN_SATOM_ID(LmnSymbolAtomRef atom) { return atom->procId; }
void LMN_SATOM_SET_ID(LmnSymbolAtomRef atom, LmnWord id) { atom->procId = id; }

LmnFunctor LMN_SATOM_GET_FUNCTOR(LmnSymbolAtomRef atom) {
  return atom->functor;
}
void LMN_SATOM_SET_FUNCTOR(LmnSymbolAtomRef atom, LmnFunctor func) {
  atom->functor = func;
}
int LMN_SATOM_GET_ARITY(LmnSymbolAtomRef atom) {
  return LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom));
}
int LMN_FUNCTOR_GET_LINK_NUM(LmnFunctor func) {
  return LMN_FUNCTOR_ARITY(func) - (LMN_IS_PROXY_FUNCTOR(func) ? 1U : 0U);
}
int LMN_SATOM_GET_LINK_NUM(LmnSymbolAtomRef atom) {
  return LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
}

/* リンク番号のタグのワード数。ファンクタと同じワードにある分も数える */
int LMN_ATTR_WORDS(int arity) {
  return 1 + ((arity + sizeof(LmnFunctor) - 1) >> LMN_WORD_SHIFT);
}

const LmnAtomRef *LMN_SATOM_PLINK(LmnSymbolAtomRef atom, int n) {
  return &atom->links[LMN_ATTR_WORDS(LMN_SATOM_GET_ARITY(atom)) + n];
}

LmnLinkAttr LMN_SATOM_GET_ATTR(LmnSymbolAtomRef atom, int n) {
  return atom->attr[n];
}
/* set link attribute value. Tag is not changed. */
void LMN_SATOM_SET_ATTR(LmnSymbolAtomRef atom, int n, LmnLinkAttr attr) {
  atom->attr[n] = attr;
}
LmnAtomRef LMN_SATOM_GET_LINK(LmnSymbolAtomRef atom, int n) {
  return atom->links[LMN_ATTR_WORDS(LMN_SATOM_GET_ARITY(atom)) + n];
}
void LMN_SATOM_SET_LINK(LmnSymbolAtomRef atom, int n, LmnAtomRef v) {
  atom->links[LMN_ATTR_WORDS(LMN_SATOM_GET_ARITY(atom)) + n] = v;
}
void LMN_HLATOM_SET_LINK(LmnSymbolAtomRef atom, LmnAtomRef v) {
  LMN_SATOM_SET_LINK(atom, 0, v);
}

size_t LMN_SATOM_SIZE(int arity) {
  return offsetof(struct LmnAtomData, links) +
         (LMN_ATTR_WORDS(arity) + arity) * LMN_WORD_BYTES;
}

BOOL LMN_HAS_FUNCTOR(LmnSymbolAtomRef ATOM, LmnLinkAttr ATTR, LmnFunctor FUNC) {
  return LMN_ATTR_IS_DATA(ATTR) ? FALSE : LMN_SATOM_GET_FUNCTOR(ATOM) == FUNC;
}

BOOL LMN_ATTR_IS_DATA(LmnLinkAttr attr) { return attr & ~LMN_ATTR_MASK; }

LmnLinkAttr LMN_ATTR_MAKE_DATA(int X) { return 0x80U | X; }
LmnLinkAttr LMN_ATTR_MAKE_LINK(int X) { return X; }

int LMN_ATTR_GET_VALUE(int X) { return X & LMN_ATTR_MASK; }

void LMN_ATTR_SET_VALUE(LmnLinkAttr *PATTR, int X) {
  *PATTR = (X & ~LMN_ATTR_MASK) | X;
}

/////

BOOL LMN_SATOM_IS_PROXY(LmnSymbolAtomRef ATOM) {
  return LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(ATOM));
}
LmnMembraneRef LMN_PROXY_GET_MEM(LmnSymbolAtomRef PROXY_ATM) {
  return (LmnMembraneRef)LMN_SATOM_GET_LINK(PROXY_ATM, 2);
}
void LMN_PROXY_SET_MEM(LmnSymbolAtomRef PROXY_ATM, LmnMembraneRef X) {
  LMN_SATOM_SET_LINK(PROXY_ATM, 2, X);
}
#define LMN_PROXY_FUNCTOR_NUM (3)
BOOL LMN_IS_PROXY_FUNCTOR(LmnFunctor FUNC) {
  return FUNC < LMN_PROXY_FUNCTOR_NUM;
}
BOOL LMN_IS_SYMBOL_FUNCTOR(LmnFunctor FUNC) {
  return FUNC >= LMN_PROXY_FUNCTOR_NUM;
}

/////

const char *LMN_SATOM_STR(LmnSymbolAtomRef ATOM) {
  return LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(ATOM)));
}
const char *LMN_FUNCTOR_STR(LmnFunctor F) {
  return LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(F));
}

/////

BOOL LMN_ATTR_IS_DATA_WITHOUT_EX(LmnLinkAttr ATTR) {
  return LMN_ATTR_IS_DATA(ATTR) && !LMN_ATTR_IS_HL(ATTR);
}
BOOL LMN_ATTR_IS_EX(LmnLinkAttr ATTR) {
  return LMN_ATTR_IS_DATA(ATTR) && LMN_ATTR_IS_HL(ATTR);
}
BOOL LMN_IS_EX_FUNCTOR(LmnFunctor FUNC) { return FUNC == LMN_HL_FUNC; }

/////

/* アトムをコピーして返す。
 * atomがシンボルアトムの場合、リンク先のデータアトムもコピーする */
LmnAtomRef lmn_copy_atom(LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)atom, attr);
  } else { /* symbol atom */
    return lmn_copy_satom((LmnSymbolAtomRef)atom);
  }
}

LmnSymbolAtomRef lmn_copy_satom(LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef newatom;
  LmnFunctor f;

  f = LMN_SATOM_GET_FUNCTOR(atom);
  newatom = lmn_new_atom(f);

  memcpy((void *)newatom, (void *)atom, LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(f)));

  LMN_SATOM_SET_ID(newatom, 0);
  return newatom;
}

LmnDataAtomRef lmn_copy_data_atom(LmnDataAtomRef atom, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    return atom;
  case LMN_DBL_ATTR:
    return lmn_create_double_atom(lmn_get_double(atom));
  case LMN_SP_ATOM_ATTR:
    return (LmnDataAtomRef)SP_ATOM_COPY(atom);
  case LMN_HL_ATTR: {
    LmnSymbolAtomRef copyatom = lmn_copy_satom((LmnSymbolAtomRef)atom);
    LMN_SATOM_SET_ID(copyatom, 0);

    lmn_hyperlink_copy(copyatom, (LmnSymbolAtomRef)atom);
    return (LmnDataAtomRef)copyatom;
  }
  default:
    LMN_ASSERT(FALSE);
    return -1;
  }
}
// is_new_hl =
// TRUEで新しく生成したハイパーリンクは元のハイパーリンクと接続しない
LmnSymbolAtomRef lmn_copy_satom_with_data(LmnSymbolAtomRef atom,
                                          BOOL is_new_hl) {
  LmnFunctor f;
  LmnSymbolAtomRef newatom;
  unsigned int i, arity = LMN_SATOM_GET_LINK_NUM(atom);

  f = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom);
  newatom = lmn_new_atom(f);

  LMN_ASSERT(newatom != atom);

  memcpy((void *)newatom, (void *)atom, LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(f)));
  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      if (is_new_hl && LMN_SATOM_GET_ATTR(atom, i) == LMN_HL_ATTR) {
        LmnAtomRef hlAtom = LMN_SATOM_GET_LINK(atom, i);
        HyperLink *hl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)hlAtom);
        LmnAtomRef new_hlAtom = lmn_hyperlink_new_with_attr(
            LMN_HL_ATTRATOM(hl), LMN_HL_ATTRATOM_ATTR(hl));
        LMN_SATOM_SET_LINK(newatom, i, new_hlAtom);
        LMN_SATOM_SET_ATTR(newatom, i, LMN_HL_ATTR);
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)new_hlAtom, 0, newatom);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)new_hlAtom, 0,
                           LMN_ATTR_MAKE_LINK(i));
      } else {
        LmnDataAtomRef dt =
            lmn_copy_data_atom((LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                               LMN_SATOM_GET_ATTR(atom, i));
        LMN_SATOM_SET_LINK(newatom, i, (LmnAtomRef)dt);
        if (LMN_SATOM_GET_ATTR(atom, i) == LMN_HL_ATTR) {
          LMN_HLATOM_SET_LINK((LmnSymbolAtomRef)dt, newatom);
        }
      }
    }
  }

  LMN_SATOM_SET_ID(newatom, 0);

  return newatom;
}

void free_data_atom(LmnDataAtomRef atom, LmnLinkAttr attr) {
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
    lmn_hyperlink_delete((LmnSymbolAtomRef)atom);
    lmn_delete_atom((LmnSymbolAtomRef)atom);
    break;
  default:
    LMN_ASSERT(FALSE);
    break;
  }
}

/* O(ARITY) */
void lmn_free_atom(LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    free_data_atom((LmnDataAtomRef)atom, attr);
  } else { /* symbol atom */
    lmn_delete_atom((LmnSymbolAtomRef)atom);
  }
}

/* シンボルアトムとリンクで接続しているデータアトムを解放する */
void free_symbol_atom_with_buddy_data(LmnSymbolAtomRef atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      free_data_atom((LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                     LMN_SATOM_GET_ATTR(atom, i));
    }
  }

  if (LMN_FUNC_IS_HL(LMN_SATOM_GET_FUNCTOR(atom))) {
    lmn_hyperlink_delete(atom);
  }

  lmn_delete_atom(atom);
}

BOOL lmn_eq_func(LmnAtomRef atom0, LmnLinkAttr attr0, LmnAtomRef atom1,
                 LmnLinkAttr attr1) {
  /* TODO: TOFIX シンボルアトムのattrがすべて等しい値であることを確認する */
  if (attr0 != attr1)
    return FALSE;
  switch (attr0) {
  case LMN_INT_ATTR:
    return atom0 == atom1;
  case LMN_DBL_ATTR:
    return lmn_get_double((LmnDataAtomRef)atom0) ==
           lmn_get_double((LmnDataAtomRef)atom1);
  case LMN_SP_ATOM_ATTR:
    return SP_ATOM_EQ(atom0, atom1);
  case LMN_HL_ATTR:
    return lmn_hyperlink_eq((LmnSymbolAtomRef)atom0, attr0,
                            (LmnSymbolAtomRef)atom1, attr1);
  default: /* symbol atom */
    return LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom0) ==
           LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom1);
  }
}

BOOL lmn_data_atom_is_ground(LmnDataAtomRef atom, LmnLinkAttr attr,
                             ProcessTableRef *hlinks) {
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

BOOL lmn_data_atom_eq(LmnDataAtomRef atom1, LmnLinkAttr attr1,
                      LmnDataAtomRef atom2, LmnLinkAttr attr2) {
  if (attr1 != attr2) {
    return FALSE;
  } else {
    switch (attr1) {
    case LMN_INT_ATTR:
      return atom1 == atom2;
    case LMN_DBL_ATTR:
      return lmn_get_double(atom1) == lmn_get_double(atom2);
    case LMN_SP_ATOM_ATTR:
      return SP_ATOM_EQ(atom1, atom2);
    case LMN_HL_ATTR:
      return lmn_hyperlink_eq((LmnSymbolAtomRef)atom1, attr1,
                              (LmnSymbolAtomRef)atom2, attr2);
    default:
      lmn_fatal("Implementation error");
      return FALSE;
    }
  }
}

/* caller must ensure |atom| has LMN_DBL_ATTR */
double lmn_get_double(LmnDataAtomRef atom) {
#ifdef LMN_DOUBLE_IS_IMMEDIATE
  return *(double *)&atom; // forward bit pattern
#else
  return *(double *)atom;
#endif
}

/* Create atom represents double data. Return value must be freed by
 * |lmn_destroy_double_atom| */
LmnDataAtomRef lmn_create_double_atom(double d) {
#ifdef LMN_DOUBLE_IS_IMMEDIATE
  return *(LmnDataAtomRef *)&d; // forward bit pattern
#else
  double *result = (double *)LMN_MALLOC(double);
  *result = d;
  return (LmnDataAtomRef)result;
#endif
}

/* User don't call this function directly. Use |lmn_free_atom| instead. */
void lmn_destroy_double_atom(LmnDataAtomRef atom) {
#ifndef LMN_DOUBLE_IS_IMMEDIATE
  LMN_FREE((double *)atom);
#endif
}