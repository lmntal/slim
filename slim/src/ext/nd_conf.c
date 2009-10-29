/*
 * nd_conf.c
 */

#include <stdio.h>
#include "../rule.h"
#include "../functor.h"
#include "../ccallback.h"
#include "../membrane.h"
#include "../mem_encode.h"
#include "../react_context.h"

/* ポートa0から一行読み込む
 * +a0     : 優先度を表す整数
 * +a1     : アトム名のunaryアトム
 * +a2     : アリティの整数
 */
void cb_set_functor_priority(ReactCxt rc,
                             LmnMembrane *mem,
                             LmnAtom a0, LmnLinkAttr t0,
                             LmnAtom a1, LmnLinkAttr t1,
                             LmnAtom a2, LmnLinkAttr t2)
{
  set_functor_priority(lmn_functor_intern(ANONYMOUS,
                                          LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(a1)),
                                          a2),
                       a0);
  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_delete_atom(mem, a2, t2);
}

void init_nd_conf(void)
{
  lmn_register_c_fun("set_functor_priority", cb_set_functor_priority, 3);
}

