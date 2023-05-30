/*
 * memory_pool.c
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
 * $Id: memory_pool.c,v 1.2 2008/09/19 05:18:17 taisuke Exp $
 */

#include "memory_pool.h"
#include "lmntal.h"

#define REF_CAST(T, X) (*(T *)&(X))

constexpr int blocksize = 8;

void *memory_pool::do_allocate(size_t bytes, size_t alignment) {
  void *res;

  if (free_head == nullptr) {
    char *rawblock;
    int   i;

    /* fprintf(stderr, "no more free space, so allocate new block\n"); */

    /* top of block is used as pointer to head of next block */
    rawblock           = (char *)lmn_malloc(aligned_size(sizeof(void *)) + sizeof_element * blocksize);
    *(void **)rawblock = block_head;
    block_head         = rawblock;

    /* rest is used as space for elements */
    /* skip top of block */
    rawblock  = rawblock + aligned_size(sizeof(void *));
    free_head = rawblock;

    for (i = 0; i < (blocksize - 1); i++) {
      /* top of each empty elements is used as pointer to next empty element */
      REF_CAST(void *, rawblock[sizeof_element * i]) = &rawblock[sizeof_element * (i + 1)];
    }
    REF_CAST(void *, rawblock[sizeof_element * (blocksize - 1)]) = nullptr;
  }

  res       = free_head;
  free_head = *(void **)free_head;

  return res;
}

void memory_pool::do_deallocate(void *p, size_t bytes = 0, size_t alignment = 0) {
  *(void **)p = free_head;
  free_head   = p;
}

bool memory_pool::do_is_equal(std::pmr::memory_resource const &other) const noexcept { return this == &other; }