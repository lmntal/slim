/* atomic */

#include <stdio.h>
#include "../lmntal_ext.h"

LMN_EXTERN void init_nlmem(void);

void atomic_ruleset(LmnMembrane *mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  if (LMN_INT_ATTR == t0) {
    int i, n = lmn_mem_ruleset_num(mem);
    int do_atomic = a0;
    
    for (i = 0; i < n; i++) {
      lmn_ruleset_set_atomic(lmn_mem_get_ruleset(mem, i), do_atomic);
      lmn_mem_add_ruleset(lmn_mem_parent(mem), lmn_mem_get_ruleset(mem, i));
    }
    lmn_mem_delete_atom(mem, a0, t0);
  }

  lmn_memstack_delete(mem);
  lmn_mem_delete_mem(mem->parent, mem);
}


void init_atomic(void)
{
  lmn_register_c_fun("atomic_ruleset", atomic_ruleset, 1);
}
