/* atomic */

#include <stdio.h>
#include "../lmntal_ext.h"
#include "../react_context.h"
#include "../slim_header/memstack.h"

LMN_EXTERN void init_nlmem(void);

void atomic_ruleset(ReactCxt rc,
                    LmnMembrane *mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  if (LMN_INT_ATTR == t0) {
    int i, n = lmn_mem_ruleset_num(mem);
    AtomicType atomic_type = a0;
    
    for (i = 0; i < n; i++) {
      lmn_ruleset_set_atomic(lmn_mem_get_ruleset(mem, i), atomic_type);
      lmn_mem_add_ruleset(lmn_mem_parent(mem), lmn_ruleset_copy(lmn_mem_get_ruleset(mem, i)));
    }
    lmn_mem_delete_atom(mem, a0, t0);
  }

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    lmn_memstack_delete(RC_MEMSTACK(rc), mem);
  }
  lmn_mem_delete_mem(mem->parent, mem);
}


void init_atomic(void)
{
  lmn_register_c_fun("atomic_ruleset", atomic_ruleset, 1);
}
