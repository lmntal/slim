/*
 * initial_ruleset.c - 
 */

#include <stdio.h>
#include "../rule.h"
#include "../membrane.h"
#include "../react_context.h"
#include "../slim_header/memstack.h"

void init_initial_ruleset(void);

#define INITIAL_RULESET_MEM_NAME "initial_ruleset"
#define INITIAL_SYSTEM_RULESET_MEM_NAME "initial_system_ruleset"

const char *initial_modules[] = {"nd_conf"};

BOOL register_initial_rulesets(ReactCxt rc, LmnMembrane *mem, LmnRule rule)
{
  LmnMembrane *m, *next;
  BOOL ok = FALSE;
  
  for (m = mem->child_head; m; m = next) {
    next = m->next;
    if ((LMN_MEM_NAME_ID(m) == lmn_intern(INITIAL_RULESET_MEM_NAME) ||
         LMN_MEM_NAME_ID(m) == lmn_intern(INITIAL_SYSTEM_RULESET_MEM_NAME)) &&
        lmn_mem_nfreelinks(m, 0) &&
        lmn_mem_atom_num(m) == 0 &&
        lmn_mem_child_mem_num(m) == 0) {
      int i, j;

      for (i = 0; i < lmn_mem_ruleset_num(m); i++) {
        LmnRuleSet rs = lmn_mem_get_ruleset(m, i);

        for (j = 0; j < lmn_ruleset_rule_num(rs); j++) {
          if (LMN_MEM_NAME_ID(m) == lmn_intern(INITIAL_RULESET_MEM_NAME)) {
            lmn_add_initial_rule(lmn_rule_copy(lmn_ruleset_get_rule(rs, j)));
          } else if (LMN_MEM_NAME_ID(m) == lmn_intern(INITIAL_SYSTEM_RULESET_MEM_NAME)) {
            lmn_add_initial_system_rule(lmn_rule_copy(lmn_ruleset_get_rule(rs, j)));
          }
        }
      }
      
      if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
        lmn_memstack_delete(RC_MEMSTACK(rc), m);
      }
      lmn_mem_delete_mem(mem, m);

      ok = TRUE;
    }
  }

  return ok;
}

BOOL register_initial_module(ReactCxt rc, LmnMembrane *mem, LmnRule rule)
{
  static int done = 0;
  int i;

  if (done == 1) return FALSE;
  done = 1;

  for (i = 0; i < sizeof(initial_modules)/sizeof(initial_modules[0]); i++) {
    LmnRuleSet rs;
    int j;

    rs = lmn_get_module_ruleset(lmn_intern(initial_modules[i]));
    if (!rs) continue;
    
    for (j = 0; j < lmn_ruleset_rule_num(rs); j++) {
      lmn_add_initial_system_rule(lmn_rule_copy(lmn_ruleset_get_rule(rs, j)));
    }
  }
  return TRUE;
}


void init_initial_ruleset(void)
{
  lmn_add_initial_rule(lmn_rule_make_translated(register_initial_rulesets, lmn_intern("register_initial_ruleset")));
  lmn_add_initial_rule(lmn_rule_make_translated(register_initial_module, lmn_intern("register_initial_module")));
}
