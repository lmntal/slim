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

#include "hyperlink.h"


/* 以下, 履歴管理用アトムの追加コード(nakata) */
void LmnSymbolAtom::atom_swap_forward() {
  LmnSymbolAtomRef atom_prev = this->prev;
  LmnSymbolAtomRef atom_next = this->next;
  LmnSymbolAtomRef atom_next_next = atom_next->next;

  atom_prev->set_next(atom_next);
  this->next = atom_next_next;
  atom_next->set_next(this);

  atom_next_next->set_prev(this);
  atom_next->set_prev(atom_prev);
  this->prev = atom_next;
}

void LmnSymbolAtom::swap_to_head(LmnSymbolAtomRef head) {
  this->prev->set_next(this->next);
  this->next->set_prev(this->prev);

  this->set_prev(head);
  this->set_next(head->next);

  head->next->set_prev(this);
  head->set_next(this);
}

void LmnSymbolAtom::remove_atom() {
  LmnSymbolAtomRef atom_prev = this->prev;
  LmnSymbolAtomRef atom_next = this->next;
  this->prev->next = this->next;
  this->next->prev = this->prev;
}
/* ここまで */

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

  f = atom->get_functor();
  newatom = lmn_new_atom(f);

  memcpy((void *)newatom, (void *)atom, LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(lmn_functor_table, f)));

  newatom->set_id(0);
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
    copyatom->set_id(0);

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
  unsigned int i, arity = atom->get_link_num();

  f = ((LmnSymbolAtomRef)atom)->get_functor();
  newatom = lmn_new_atom(f);

  LMN_ASSERT(newatom != atom);

  memcpy((void *)newatom, (void *)atom, LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(lmn_functor_table, f)));
  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      if (is_new_hl && atom->get_attr(i) == LMN_HL_ATTR) {
	// fprintf(stderr,"new hyperlink being created, %d\n", i);  // extended
        LmnAtomRef hlAtom = atom->get_link(i);
        HyperLink *hl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)hlAtom);
        LmnAtomRef new_hlAtom = lmn_hyperlink_new_with_attr(
            LMN_HL_ATTRATOM(hl), LMN_HL_ATTRATOM_ATTR(hl));
        newatom->set_link(i, new_hlAtom);
        newatom->set_attr(i, LMN_HL_ATTR);
        ((LmnSymbolAtomRef)new_hlAtom)->set_link(0, newatom);
        ((LmnSymbolAtomRef)new_hlAtom)->set_attr(0,LMN_ATTR_MAKE_LINK(i));
	// fprintf(stderr,"new hyperlink: %4lu\n",
	// 	LMN_HL_ID(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)new_hlAtom)));
      } else {
        LmnDataAtomRef dt =
            lmn_copy_data_atom((LmnDataAtomRef)atom->get_link(i),
                               atom->get_attr(i));
        newatom->set_link(i, (LmnAtomRef)dt);
        if (atom->get_attr(i) == LMN_HL_ATTR) {
          ((LmnSymbolAtomRef)dt)->set_link(0, newatom);
        }
      }
    }
  }

  newatom->set_id(0);

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
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      free_data_atom((LmnDataAtomRef)atom->get_link(i),
                     atom->get_attr(i));
    }
  }

  if (LMN_FUNC_IS_HL(atom->get_functor())) {
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
    return ((LmnSymbolAtomRef)atom0)->get_functor() ==
           ((LmnSymbolAtomRef)atom1)->get_functor();
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
