/*
 * atom.c
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
 * $Id: atom.c,v 1.3 2008/09/19 05:18:17 taisuke Exp $
 */

#include "atom.h"
#include "special_atom.h"
#include "functor.h"
#include "membrane.h"
#include "error.h"
LmnSAtom LMN_SATOM_GET_NEXT(const LmnSAtom ATOM)
{
  LmnSAtom NEXT;
  while (NEXT = LMN_SATOM_GET_NEXT_RAW(ATOM), NEXT !=lmn_atomlist_end(NEXT) && LMN_SATOM_GET_FUNCTOR(NEXT) == LMN_RESUME_FUNCTOR) {printf("hoge\n");}
  return NEXT;
}

/* アトムをコピーして返す。atomがシンボルアトムの場合、リンク先のデータ
   アトムもコピーされる */
inline LmnAtom lmn_copy_atom(LmnAtom atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return lmn_copy_data_atom(atom, attr);
  } else { /* symbol atom */
    return LMN_ATOM(lmn_copy_satom(LMN_SATOM(atom)));
  }
}

inline LmnSAtom lmn_copy_satom(LmnSAtom atom)
{
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  LmnSAtom newatom = lmn_new_atom(f);
  
  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES*LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));

  return newatom;
}

inline LmnAtom lmn_copy_data_atom(LmnAtom atom, LmnLinkAttr attr)
{
  switch (attr) {
  case LMN_INT_ATTR:
    return atom;
  case LMN_DBL_ATTR:
    {
      double *d = LMN_MALLOC(double);
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

LmnSAtom lmn_copy_satom_with_datom(LmnSAtom atom)
{
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(LMN_SATOM(atom));
  LmnSAtom newatom = lmn_new_atom(f);
  unsigned int i, arity = LMN_SATOM_GET_LINK_NUM(atom);
  
  memcpy((void *)newatom,
         (void *)atom,
         LMN_WORD_BYTES*LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(f)));

  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      LMN_SATOM_SET_LINK(newatom, i,
                         lmn_copy_data_atom(LMN_SATOM_GET_LINK(atom, i),
                                            LMN_SATOM_GET_ATTR(atom, i)));
    }
  }
  return newatom;
}

static inline void free_data_atom(LmnAtom atom, LmnLinkAttr attr)
{
  switch (attr) {
  case LMN_INT_ATTR:
    break;
  case LMN_DBL_ATTR:
    LMN_FREE((double*)atom);
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
void lmn_free_atom(LmnAtom atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    free_data_atom(atom, attr);
  }
  else { /* symbol atom */
    lmn_delete_atom(LMN_SATOM(atom));
  }
}

/* シンボルアトムとリンクで接続しているデータアトムを解放する */
void free_symbol_atom_with_buddy_data(LmnSAtom atom)
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

BOOL lmn_eq_func(LmnAtom atom0, LmnLinkAttr attr0, LmnAtom atom1, LmnLinkAttr attr1)
{
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

inline BOOL lmn_data_atom_is_ground(LmnAtom atom, LmnLinkAttr attr)
{
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

inline BOOL lmn_data_atom_eq(LmnAtom atom1, LmnLinkAttr attr1,
                             LmnAtom atom2, LmnLinkAttr attr2)
{
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
