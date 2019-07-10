/*
 * integer.c
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

/* 整数関連のコールバック */

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"
#include "verifier/verifier.h"

void init_integer(void);
void integer_set(LmnReactCxtRef rc,
                 LmnMembraneRef mem,
                 LmnAtomRef a0, LmnLinkAttr t0,
                 LmnAtomRef a1, LmnLinkAttr t1,
                 LmnAtomRef a2, LmnLinkAttr t2);
void integer_srand(LmnReactCxtRef rc,
                   LmnMembraneRef mem,
                   LmnAtomRef a0, LmnLinkAttr t0);
void integer_rand(LmnReactCxtRef rc,
                  LmnMembraneRef mem,
                  LmnAtomRef a0, LmnLinkAttr t0,
                  LmnAtomRef a1, LmnLinkAttr t1);
void integer_of_string(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtomRef a0, LmnLinkAttr t0,
                       LmnAtomRef a1, LmnLinkAttr t1);
void init_integer(void);

/**
 * ($start, $end, $g)
 * where
 *  start, end = integer
 *  g = ground
 *
 * Creates a (multi)set $g[$a], $g[$a+1], ..., $g[$b].
 */
void integer_set(LmnReactCxtRef rc,
                 LmnMembraneRef mem,
                 LmnAtomRef a0, LmnLinkAttr t0,
                 LmnAtomRef a1, LmnLinkAttr t1,
                 LmnAtomRef a2, LmnLinkAttr t2)
{
  Vector *srcvec;
  LmnWord i;
  long long n, start, end;

  start  = (long long)a0;
  end    = (long long)a1;
  srcvec = new Vector(16);
  srcvec->push((LmnWord)LinkObj_make(a2, t2));

  for (i = 0, n = start; n <= end; i++, n++) {
    Vector *dstlovec;
    ProcessTableRef atommap;
    LinkObjRef l;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);

    l = (LinkObjRef)dstlovec->get(0);
    lmn_mem_newlink(mem, (LmnAtomRef)n, LMN_INT_ATTR, 0,
                    LinkObjGetAtom(l), t2, LMN_ATTR_GET_VALUE(LinkObjGetPos(l)));
    lmn_mem_push_atom(mem, (LmnAtomRef)n, LMN_INT_ATTR);

    for (int j = 0; j < dstlovec->get_num(); j++) LMN_FREE(dstlovec->get(j));
    delete dstlovec;
    proc_tbl_free(atommap);
  }

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_mem_delete_ground(mem, srcvec);

  for (i = 0; i < srcvec->get_num(); i++) LMN_FREE(srcvec->get(i));
  delete srcvec;
}


/*
 * (N):
 *
 * sets N as the seed for random numbers
 */
void integer_srand(LmnReactCxtRef rc,
                   LmnMembraneRef mem,
                   LmnAtomRef a0, LmnLinkAttr t0)
{
  srand((LmnWord)a0);
  lmn_mem_delete_atom(mem, a0, t0);
}


/*
 * (N, H):
 *
 * H is bound to a random number between 0 and N-1.
 */
void integer_rand(LmnReactCxtRef rc,
                  LmnMembraneRef mem,
                  LmnAtomRef a0, LmnLinkAttr t0,
                  LmnAtomRef a1, LmnLinkAttr t1)
{
  LmnWord n = rand() % (LmnWord)a0;

  lmn_mem_newlink(mem,
                  (LmnSymbolAtomRef)a1, LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(t1),
                  (LmnAtomRef)n, LMN_INT_ATTR, 0);
  lmn_mem_push_atom(mem, (LmnAtomRef)n, LMN_INT_ATTR);

  lmn_mem_delete_atom(mem, (LmnSymbolAtomRef)a0, t0);
}

/*
 * (S, N):
 *
 * N is bound to a number with the string representation S.
 */
void integer_of_string(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtomRef a0, LmnLinkAttr t0,
                       LmnAtomRef a1, LmnLinkAttr t1)
{
  long n;
  char *t;
  const char *s = reinterpret_cast<LmnString *>(a0)->c_str();
  t = NULL;
  n = strtol(s, &t, 10);
  if (t == NULL || s == t) {
    LmnSymbolAtomRef a = lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS,
                                                         lmn_intern("fail"),
                                                         1));
    lmn_mem_newlink(mem,
                    (LmnSymbolAtomRef)a1, t1, LMN_ATTR_GET_VALUE(t1),
                    a, LMN_ATTR_MAKE_LINK(0), 0);
  } else { /* 変換できた */
    lmn_mem_newlink(mem,
                    (LmnSymbolAtomRef)a1, t1, LMN_ATTR_GET_VALUE(t1),
                    (LmnAtomRef)n, LMN_INT_ATTR, 0);
    lmn_mem_push_atom(mem, (LmnSymbolAtomRef)n, LMN_INT_ATTR);
  }

  lmn_mem_delete_atom(mem, (LmnSymbolAtomRef)a0, t0);
}

void init_integer(void)
{
  lmn_register_c_fun("integer_set", (void *)integer_set, 3);
  lmn_register_c_fun("integer_srand", (void *)integer_srand, 1);
  lmn_register_c_fun("integer_rand", (void *)integer_rand, 2);
  lmn_register_c_fun("integer_of_string", (void *)integer_of_string, 2);

  srand((unsigned)time(NULL));
}
