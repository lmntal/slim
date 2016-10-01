/*
 * nd_conf.c
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
#include "vm/vm.h"
#include "mem_encode.h"

/* ポートa0から一行読み込む
 * +a0     : 優先度を表す整数
 * +a1     : アトム名のunaryアトム
 * +a2     : アリティの整数
 */
void cb_set_functor_priority(LmnReactCxtRef rc,
                             LmnMembraneRef mem,
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
  lmn_register_c_fun("set_functor_priority", (void *)cb_set_functor_priority, 3);
}

