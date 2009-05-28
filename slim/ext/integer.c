/* 整数関連のコールバック */

#include <stdio.h>
#include "../src/lmntal_ext.h"

void init_integer(void);

void integer_set(LmnMembrane *mem,
                 LmnWord a0, LmnLinkAttr t0,
                 LmnWord a1, LmnLinkAttr t1,
                 LmnWord a2, LmnLinkAttr t2)
{
  int i, n;
  int start = (int)a0;
  int end = (int)a1;
  LmnFunctor f = LMN_ATOM_GET_FUNCTOR(a2);

  for (i = 0, n = start; n <= end; i++, n++) {
    LmnAtomPtr a = lmn_new_atom(f);
    LMN_ATOM_SET_LINK(a, 0, n);
    LMN_ATOM_SET_ATTR(a, 0, LMN_INT_ATTR);
    mem_push_symbol_atom(mem, a);
  }

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_mem_remove_atom(mem, a2, t2);
  lmn_free_atom(a0, t0);
  lmn_free_atom(a1, t1);
  lmn_free_atom(a2, t2);
}

void init_integer(void)
{
  lmn_register_c_fun("integer_set", integer_set, 3);
}
