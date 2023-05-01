/*
 * atom.c - atom utility
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

#include "element/element.h"
#include "vm/vm.h"
#include <cstdio>

/*
 * Internal Constructor
 */
static LmnSymbolAtomRef lmn_make_atom(LmnMembraneRef mem, LmnAtomRef s, LmnWord size) {
  LmnSymbolAtomRef a = lmn_mem_newatom(
      mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern(reinterpret_cast<LmnString *>(s)->c_str()), size));
  for (int k = 0; k < (int)size - 1; k++) {
    lmn_mem_newlink(mem, a, LMN_ATTR_MAKE_LINK(0), k, 0, LMN_INT_ATTR, 0);
    lmn_mem_push_atom(mem, (LmnAtomRef)k, LMN_INT_ATTR);
  }
  return a;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 * ファンクタ名や要素数にかかわらず必ず記号アトムが生成される
 *
 * +a0: ファンクタ名（文字列）
 * +a1: 要素数
 * -a2: アトム
 */
void cb_atom_new(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1_, LmnLinkAttr t1,
                 LmnAtomRef a2, LmnLinkAttr t2) {
  LmnSymbolAtomRef atom, res;

  LmnWord a1 = (LmnWord)a1_; /**< a1 is assumed to be an integer data atom */
  if (a1 > 0 && a1 <= 127) {
    atom = lmn_make_atom(mem, a0, a1);
    res  = lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern("some"), 2));
    lmn_mem_newlink(mem, atom, LMN_ATTR_MAKE_LINK(0), a1 - 1, res, LMN_ATTR_MAKE_LINK(0), 0);
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), res, LMN_ATTR_MAKE_LINK(0), 1);
    lmn_mem_delete_atom(mem, a1_, t1);
    lmn_mem_delete_atom(mem, a0, t0);
  } else if (a1 == 0) {
    atom = lmn_make_atom(mem, a0, a1);
    res  = lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern("none"), 1));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), res, LMN_ATTR_MAKE_LINK(0), 0);
    lmn_mem_delete_atom(mem, a1_, t1);
    lmn_mem_delete_atom(mem, a0, t0);
  } else {
    lmn_fatal("Atom's arity must be between 0 and 127.");
  }
}

/*
 * ファンクタと要素数取得
 *
 * +a0: アトム（現在は記号アトムに限定）
 * -a1: ファンクタ文字列
 * -a2: 要素数
 * -a3: 新アトム
 */
void cb_atom_functor(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                     LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3) {
  if (LMN_ATTR_IS_DATA(t0))
    lmn_fatal("atom.functor cannot be applied to non-symbol atoms\
 (numbers, strings, ...).");
  LmnStringRef s = new LmnString(((LmnSymbolAtomRef)a0)->str());
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), s, LMN_SP_ATOM_ATTR, 0);

  lmn_mem_push_atom(mem, s, LMN_SP_ATOM_ATTR);
  long n = ((LmnSymbolAtomRef)a0)->get_arity();
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), (LmnAtomRef)n, LMN_INT_ATTR, 0);
  lmn_mem_push_atom(mem, (LmnAtomRef)n, LMN_INT_ATTR);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3, LMN_ATTR_GET_VALUE(t3));
}

/*
 * 要素交換
 *
 * +a0: アトム（現在は記号アトムに限定）
 * +a1: 引数番号（非負整数）
 * +a2: 新要素値（データアトム可）
 * -a3: 旧要素値（現在は接続先は記号アトムに限定）
 * -a4: 新アトム
 */

void cb_atom_swap(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                  LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3, LmnAtomRef a4, LmnLinkAttr t4) {
  LmnAtomRef ap1, self;
  LmnByte    attr1;

  if (LMN_ATTR_IS_DATA(t0))
    lmn_fatal("Arg 0 of atom.swap cannot be non-symbol atoms (numbers, strings, ...).");

  if (LMN_ATTR_IS_DATA(t3))
    lmn_fatal("Arg 3 of atom.swap cannot be non-symbol atoms (numbers, strings, ...).");

  printf("cb_atom_swap arity: %d\n", LMN_FUNCTOR_ARITY(lmn_functor_table, ((LmnSymbolAtomRef)a0)->get_functor()));

  if ((unsigned long)a1 >= LMN_FUNCTOR_ARITY(lmn_functor_table, ((LmnSymbolAtomRef)a0)->get_functor()) - 1)
    lmn_fatal("atom.swap index out of range.");

  self  = ((LmnSymbolAtomRef)a0)->get_link(t0);
  ap1   = ((LmnSymbolAtomRef)a0)->get_link((LmnWord)a1);
  attr1 = ((LmnSymbolAtomRef)a0)->get_attr((LmnWord)a1);

  //  lmn_relink_symbols(a2, t2, ap1, attr1);  //works fine
  //  lmn_relink_symbols(a0, a1, self, 3);  // works fine
  newlink_symbol_and_something((LmnSymbolAtomRef)a0, (LmnWord)a1, a2, t2); // works fine

  //  lmn_relink_symbols(ap1, attr1, self, 4);  //works fine
  //  lmn_relink_symbols(a3, t3, a0, a1); //doesn't work due to ordering
  newlink_symbol_and_something((LmnSymbolAtomRef)a3, t3, ap1, attr1);

  lmn_mem_unify_atom_args(mem, (LmnSymbolAtomRef)self, 1, (LmnSymbolAtomRef)self, 5);
}

/*----------------------------------------------------------------------
 * Initialization
 */

void init_atom() {
  CCallback::lmn_register_c_fun("cb_atom_new", (void *)cb_atom_new, 3);
  CCallback::lmn_register_c_fun("cb_atom_functor", (void *)cb_atom_functor, 4);
  CCallback::lmn_register_c_fun("cb_atom_swap", (void *)cb_atom_swap, 5);
}
