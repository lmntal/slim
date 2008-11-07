/*
 * alloc.c -- memory management
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: alloc.c,v 1.3 2008/09/19 05:18:17 taisuke Exp $
 */

#include "memory_pool.h"
#include "lmntal.h"
#include "atom.h"
#include "functor.h"
#include <malloc.h>

#define ARYSIZE(ary)	(sizeof(ary)/sizeof((ary)[0]))

/*----------------------------------------------------------------------
 * memory allocation for atom
 */

static memory_pool *atom_memory_pools[128];

LmnAtomPtr lmn_new_atom(LmnFunctor f)
{
  LmnAtomPtr ap;
  int arity = LMN_FUNCTOR_ARITY(f);
  
  if(atom_memory_pools[arity] == 0){
    atom_memory_pools[arity] = memory_pool_new(sizeof(LmnWord)*LMN_ATOM_WORDS(arity));
  }
  ap = (LmnAtomPtr)memory_pool_malloc(atom_memory_pools[arity]);
  LMN_ATOM_SET_FUNCTOR(ap, f);
  return ap;
}

void lmn_delete_atom(LmnAtomPtr ap)
{
  memory_pool_free(atom_memory_pools[LMN_FUNCTOR_ARITY(LMN_ATOM_GET_FUNCTOR(ap))], ap);
}

void free_atom_memory_pools(void)
{
  unsigned int i;
  
  for (i = 0; i < ARYSIZE(atom_memory_pools); i++) {
    if (atom_memory_pools[i]) {
      memory_pool_delete(atom_memory_pools[i]);
    }
  }
}

/*----------------------------------------------------------------------
 * memory allocation for membrane
 */

/* in membrane.c */
/* lmn_mem_make / lmn_mem_delete */

/*----------------------------------------------------------------------
 * low level allocation
 */

void *lmn_calloc(size_t num, size_t size)
{
#if HAVE_CALLOC
  void *new = calloc (num, size);
  if (!new) lmn_fatal("Memory exhausted");
#else
  void *new = lmn_malloc(num * size);
#endif

  return new;
}

void *lmn_malloc(size_t num)
{
  void *new = malloc(num);
  if (!new) lmn_fatal("Memory exhausted");

  return new;
}

void *lmn_realloc(void *p, size_t num)
{
  void *new;

  if (!p) return lmn_malloc (num);
  new = realloc (p, num);
  if (!new) lmn_fatal("Memory exhausted");

  return new;
}

void lmn_free(void *p)
{
  free(p);
}
