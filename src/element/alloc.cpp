/*
 * alloc.c -- memory management
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

#include "error.h"
#include "lmntal.h"
#include "lmntal_thread.h"
#include "memory_pool.h"
#include "util.h"
#include "vector.h"
#include "vm/vm.h"

/*----------------------------------------------------------------------
 * memory allocation for atom
 */

static std::array<memory_pool **, 128> atom_memory_pools;

void mpool_init() {
  auto arity_num = atom_memory_pools.size();
  auto core_num  = lmn_env.core_num;
  for (auto i = 0; i < arity_num; i++) {
    atom_memory_pools[i] = (memory_pool **)malloc(sizeof(memory_pool *) * core_num);
    memset(atom_memory_pools[i], 0, sizeof(memory_pool *) * core_num);
  }
}

LmnSymbolAtomRef lmn_new_atom(LmnFunctor f) {
  auto arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);
  auto cid   = env_my_thread_id();

  if (atom_memory_pools[arity][cid] == nullptr) {
    atom_memory_pools[arity][cid] = new memory_pool(LMN_SATOM_SIZE(arity));
  }
  auto *ap = (LmnSymbolAtomRef)atom_memory_pools[arity][cid]->allocate(0);
  ap->set_functor(f);
  ap->set_id(0);

  ap->record_flag = false;
  return ap;
}

void lmn_delete_atom(LmnSymbolAtomRef ap) {
  env_return_id(ap->get_id());
  auto arity = LMN_FUNCTOR_ARITY(lmn_functor_table, ap->get_functor());
  auto cid   = env_my_thread_id();
  atom_memory_pools[arity][cid]->deallocate(ap, 0);
}

void free_atom_memory_pools() {
  auto arity_num = atom_memory_pools.size();
  auto core_num  = lmn_env.core_num;
  for (auto i = 0; i < arity_num; i++) {
    for (auto j = 0; j < core_num; j++) {
      delete atom_memory_pools[i][j];
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

void *lmn_calloc(size_t num, size_t size) {
  void *result;
#if HAVE_DECL_CALLOC
  result = calloc(num, size);
  if (!result) {
    lmn_fatal("Memory exhausted");
  }
#else
  result = lmn_malloc(num * size);
  memset(result, 0x00, num * size);
#endif

  return result;
}

void *lmn_malloc(size_t num) {
  LMN_ASSERT(num > 0);
  // printf("%lu\n", num);
  void *result = malloc(num);
  if (!result)
    lmn_fatal("Memory exhausted");

  return result;
}

void *lmn_realloc(void *p, size_t num) {
  void *result;

  if (!p)
    return lmn_malloc(num);
  result = realloc(p, num);
  if (!result)
    lmn_fatal("Memory exhausted");

  return result;
}

void lmn_free(void *p) { free(p); }

void *operator new(std::size_t num) { return lmn_malloc(num); }

void *operator new[](std::size_t num) { return lmn_malloc(num); }

void operator delete(void *p) noexcept { lmn_free(p); }

void operator delete[](void *p) noexcept { lmn_free(p); }

void operator delete(void *p, std::size_t num) noexcept { lmn_free(p); }

void operator delete[](void *p, std::size_t num) noexcept { lmn_free(p); }
