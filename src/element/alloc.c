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
#include "vector.h"
#include "lmntal.h"
#include "vm/vm.h"
#include "error.h"
#include "lmntal_thread.h"
#include "util.h"


/*----------------------------------------------------------------------
 * memory allocation for atom
 */

static memory_pool **atom_memory_pools[128];

void mpool_init()
{
  int i, core_num, arity_num;
  arity_num = ARY_SIZEOF(atom_memory_pools);
  core_num  = lmn_env.core_num;
  for (i = 0; i < arity_num; i++) {
    atom_memory_pools[i] = (memory_pool **)malloc(sizeof(memory_pool *) * core_num);
    memset(atom_memory_pools[i], 0, sizeof(memory_pool *) * core_num);
  }
}

LmnSymbolAtomRef lmn_new_atom(LmnFunctor f)
{
  LmnSAtom ap;
  int arity, cid;
  arity = LMN_FUNCTOR_ARITY(f);
  cid   = env_my_thread_id();

  if (atom_memory_pools[arity][cid] == 0) {
    atom_memory_pools[arity][cid] =
      memory_pool_new(sizeof(LmnWord) * LMN_SATOM_WORDS(arity));
  }
  ap = LMN_SATOM(memory_pool_malloc(atom_memory_pools[arity][cid]));
  LMN_SATOM_SET_FUNCTOR(ap, f);
  LMN_SATOM_SET_ID(ap, 0);

  return ap;
}

void lmn_delete_atom(LmnSymbolAtomRef ap)
{
  int arity, cid;

  env_return_id(LMN_SATOM_ID(ap));

  arity = LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(ap));
  cid = env_my_thread_id();
  memory_pool_free(atom_memory_pools[arity][cid], ap);
}

void free_atom_memory_pools(void)
{
  unsigned int i, j, arity_num, core_num;

  arity_num = ARY_SIZEOF(atom_memory_pools);
  core_num  = lmn_env.core_num;
  for (i = 0; i < arity_num; i++) {
    for (j = 0; j < core_num; j++) {
      if (atom_memory_pools[i][j]) {
        memory_pool_delete(atom_memory_pools[i][j]);
      }
    }
    free(atom_memory_pools[i]);
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

/* TODO:
 *   headerに持っていってstatic inlineにした方が良い?
 *   memory exhausted時にもprofile情報をdumpさせたい */

void *lmn_calloc(size_t num, size_t size)
{
  void *result;
#if HAVE_DECL_CALLOC
  result = calloc (num, size);
  if (!result) {
    lmn_fatal("Memory exhausted");
  }
#else
  result = lmn_malloc(num * size);
  memset(result, 0x00, num * size);
#endif

  return result;
}

void *lmn_malloc(size_t num)
{
  LMN_ASSERT(num > 0);
//printf("%lu\n", num);
  void *result = malloc(num);
  if (!result) lmn_fatal("Memory exhausted");

  return result;
}

void *lmn_realloc(void *p, size_t num)
{
  void *result;

  if (!p) return lmn_malloc (num);
  result = realloc (p, num);
  if (!result) lmn_fatal("Memory exhausted");

  return result;
}

void lmn_free(void *p)
{
  free(p);
}
