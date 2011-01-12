/*
 * atomic.c
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

#include <stdio.h>
#include "lmntal_ext.h"
#include "react_context.h"
#include "slim_header/memstack.h"

LMN_EXTERN void init_nlmem(void);

void atomic_ruleset(ReactCxt rc,
                    LmnMembrane *mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  if (LMN_INT_ATTR == t0) {
    int i, n = lmn_mem_ruleset_num(mem);
    AtomicType atomic_type;

    switch ((int)a0) {
    case  1 :
      atomic_type = ATOMIC_ALL_EXHAUSTIVE;
      break;
    case  2 :
      atomic_type = ATOMIC_EACH_SYNC;
      break;
    default :
      atomic_type = ATOMIC_NONE;
      break;
    }

    for (i = 0; i < n; i++) {
      lmn_ruleset_set_atomic(lmn_mem_get_ruleset(mem, i), atomic_type);
      lmn_mem_add_ruleset(lmn_mem_parent(mem),
                          lmn_ruleset_copy(lmn_mem_get_ruleset(mem, i)));
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
