/*
 * initial_ruleset.c
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
#include <stdio.h>

#define INITIAL_RULESET_MEM_NAME "initial_ruleset"
#define INITIAL_SYSTEM_RULESET_MEM_NAME "initial_system_ruleset"

const char *initial_modules[] = {"nd_conf"};

BOOL register_initial_rulesets(LmnReactCxtRef rc, LmnMembraneRef mem,
                               LmnRuleRef rule) {
  LmnMembraneRef m, next;
  BOOL ok = FALSE;

  for (m = mem->mem_child_head(); m; m = next) {
    next = m->mem_next();
    if ((m->NAME_ID() == lmn_intern(INITIAL_RULESET_MEM_NAME) ||
         m->NAME_ID() == lmn_intern(INITIAL_SYSTEM_RULESET_MEM_NAME)) &&
        m->nfreelinks(0) && m->atom_num() == 0 &&
	m->child_mem_num() == 0) {
      int i, j;

      for (i = 0; i < m->ruleset_num(); i++) {
        LmnRuleSetRef rs = lmn_mem_get_ruleset(m, i);

        for (auto r : *rs) {
          if (m->NAME_ID() == lmn_intern(INITIAL_RULESET_MEM_NAME)) {
            lmn_add_initial_rule(new LmnRule(*r));
          } else if (m->NAME_ID() ==
                     lmn_intern(INITIAL_SYSTEM_RULESET_MEM_NAME)) {
            lmn_add_initial_system_rule(new LmnRule(*r));
          }
        }
      }

      if (rc->has_mode(REACT_MEM_ORIENTED)) {
        ((MemReactContext *)rc)->memstack_remove(m);
      }
      mem->delete_mem(m);

      ok = TRUE;
    }
  }

  return ok;
}

BOOL register_initial_module(LmnReactCxtRef rc, LmnMembraneRef mem,
                             LmnRuleRef rule) {
  static int done = 0;
  int i, j;

  if (done == 1)
    return FALSE;
  done = 1;

  for (i = 0; i < ARY_SIZEOF(initial_modules); i++) {
    LmnRuleSetRef rs = lmn_get_module_ruleset(lmn_intern(initial_modules[i]));
    if (rs) {
      for (auto r : *rs) {
        lmn_add_initial_system_rule(new LmnRule(*r));
      }
    }
  }
  return TRUE;
}

void init_initial_ruleset(void) {
  lmn_add_initial_rule(new LmnRule(register_initial_rulesets,
                                   lmn_intern("register_initial_ruleset")));
  lmn_add_initial_rule(new LmnRule(register_initial_module,
                                   lmn_intern("register_initial_module")));
}
