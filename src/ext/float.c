/*
 * float.c
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

/* 浮動小数点数関連のコールバック */

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"

void init_float(void);

/*
 * (S, N):
 *
 * N is bound to a floating-point number with the string representation S.
 */
void float_of_string(LmnReactCxtRef rc,
                     LmnMembraneRef mem,
                     LmnAtomRef a0, LmnLinkAttr t0,
                     LmnAtomRef a1, LmnLinkAttr t1)
{
  char *t;
  LmnAtomRef d;
  const char *s = (const char *)lmn_string_c_str(LMN_STRING(a0));
  t = NULL;
  d = (LmnAtomRef)lmn_create_double_atom(strtod(s, &t));
  if (t == NULL || s == t) {
    LmnSAtom a = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS,
                                                         lmn_intern("fail"),
                                                         1));
    lmn_mem_newlink(mem,
                    a1, t1, LMN_ATTR_GET_VALUE(t1),
                    a, LMN_ATTR_MAKE_LINK(0), 0);
  } else { /* 変換できた */
    lmn_mem_newlink(mem,
                    a1, t1, LMN_ATTR_GET_VALUE(t1),
                    d, LMN_DBL_ATTR, 0);
    lmn_mem_push_atom(mem, d, LMN_DBL_ATTR);
  }

  lmn_mem_delete_atom(mem, a0, t0);
}

void init_float(void)
{
  lmn_register_c_fun("float_of_string", (void *)float_of_string, 2);
}
