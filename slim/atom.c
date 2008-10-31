/*
 * atom.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lang@ueda.info.waseda.ac.jp>
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
#include "functor.h"

LmnWord lmn_copy_atom(LmnWord atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return lmn_copy_data_atom(atom, attr);
  } else { /* symbol atom */
    LmnFunctor f = LMN_ATOM_GET_FUNCTOR(LMN_ATOM(atom));
    LmnAtomPtr newatom = lmn_new_atom(f);
    memcpy((void *)newatom, (void *)atom, LMN_WORD_BYTES*LMN_ATOM_WORDS(LMN_FUNCTOR_ARITY(f)));
    return (LmnWord)newatom;
  }
}

LmnWord lmn_copy_data_atom(LmnWord atom, LmnLinkAttr attr)
{
  switch (attr) {
  case LMN_INT_ATTR:
    return atom;
  case LMN_DBL_ATTR:
    {
      double *d = LMN_MALLOC(double);
      *d = *(double*)atom;
      return (LmnWord)d;
    }
  default:
    LMN_ASSERT(FALSE);
    return -1;
  }
}

static inline void free_data_atom(LmnWord atom, LmnLinkAttr attr)
{
  switch (attr) {
  case LMN_INT_ATTR:
    break;
  case LMN_DBL_ATTR:
    LMN_FREE((double*)atom);
    break;
  default:
    LMN_ASSERT(FALSE);
    break;
  }
  return;
}

/* O(ARITY) */
void lmn_free_atom(LmnWord atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    free_data_atom(atom, attr);
  }
  else { /* symbol atom */
    lmn_delete_atom((LmnAtomPtr)atom);
  }
}

/* シンボルアトムとリンクで接続しているデータアトムを解放する */
void free_symbol_atom_with_buddy_data(LmnAtomPtr atom)
{
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_ATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_ATOM_GET_ATTR(atom, i))) {
      free_data_atom(LMN_ATOM_GET_LINK(atom, i), LMN_ATOM_GET_ATTR(atom, i));
    }
  }
  lmn_delete_atom((LmnAtomPtr)atom);
}

BOOL lmn_eq_func(LmnWord atom0, LmnLinkAttr attr0, LmnWord atom1, LmnLinkAttr attr1)
{
  /* TODO: TOFIX シンボルアトムのattrがすべて等しい値であることを確認する */
  if (attr0 != attr1) return FALSE;
  switch (attr0) {
  case LMN_INT_ATTR:
    return atom0 == atom1;
  case LMN_DBL_ATTR:
    return *(double *)atom0 == *(double *)atom1;
  default: /* symbol atom */
    return LMN_ATOM_GET_FUNCTOR(atom0) == LMN_ATOM_GET_FUNCTOR(atom1);
  }
}
