/* 整数関連のコールバック */

#include <stdio.h>
#include "../src/lmntal_ext.h"

void init_integer(void);

/**
 * ($start, $end, $g)
 * where
 *  start, end = integer
 *  g = ground
 *
 * Creates a (multi)set $g[$a], $g[$a+1], ..., $g[$b].
 */
void integer_set(LmnMembrane *mem,
                 LmnWord a0, LmnLinkAttr t0,
                 LmnWord a1, LmnLinkAttr t1,
                 LmnWord a2, LmnLinkAttr t2)
{
  int i, j, n;
  int start = (int)a0;
  int end = (int)a1;
  Vector *srcvec = vec_make(16);

  vec_push(srcvec, (LmnWord)LinkObj_make(a2, t2));

  for (i = 0, n = start; n <= end; i++, n++) {
    Vector *dstlovec;
    SimpleHashtbl *atommap;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);
    
    LinkObj l = (LinkObj)vec_get(dstlovec, 0);
    lmn_mem_newlink(mem, n, LMN_INT_ATTR, 0,
                    l->ap, t2, LMN_ATTR_GET_VALUE(l->pos));
                    
    for (j = 0; j < vec_num(dstlovec); j++) LMN_FREE(vec_get(dstlovec, j));
    vec_free(dstlovec);
    hashtbl_free(atommap);
  }

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_free_atom(a0, t0);
  lmn_free_atom(a1, t1);

  lmn_mem_remove_ground(mem, srcvec);
  lmn_mem_free_ground(srcvec);

  for (i = 0; i < vec_num(srcvec); i++) LMN_FREE(vec_get(srcvec, i));
  vec_free(srcvec);
}

void init_integer(void)
{
  lmn_register_c_fun("integer_set", integer_set, 3);
}
