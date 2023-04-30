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

/* each element must be bigger than void*, so align everything in sizeof(void*)
 * !! */
/* after alignment, X byte object needs ALIGNED_SIZE(X) byte. */
#define ALIGNED_SIZE(X)                                                        \
  (((X + sizeof(void *) - 1) / sizeof(void *)) * sizeof(void *))

memory_pool *memory_pool_new(int s) {
  memory_pool *res = LMN_MALLOC<memory_pool>();

  res->sizeof_element = ALIGNED_SIZE(s);
  res->block_head = 0;
  res->free_head = 0;

  /* fprintf(stderr, "this memory_pool allocate %d, aligned as %d\n", s,
   * res->sizeof_element); */

  return res;
}

static const int blocksize = 8;
void *memory_pool_malloc(memory_pool *p) {
  void *res;

  if (p->free_head == 0) {
    char *rawblock;
    int i;

    /* fprintf(stderr, "no more free space, so allocate new block\n"); */

    /* top of block is used as pointer to head of next block */
    rawblock = (char *)lmn_malloc(ALIGNED_SIZE(sizeof(void *)) +
                                  p->sizeof_element * blocksize);
    *(void **)rawblock = p->block_head;
    p->block_head = rawblock;

    /* rest is used as space for elements */
    /* skip top of block */
    rawblock = rawblock + ALIGNED_SIZE(sizeof(void *));
    p->free_head = rawblock;

    for (i = 0; i < (blocksize - 1); i++) {
      /* top of each empty elements is used as pointer to next empty element */
      REF_CAST(void *, rawblock[p->sizeof_element * i]) =
          &rawblock[p->sizeof_element * (i + 1)];
    }
    REF_CAST(void *, rawblock[p->sizeof_element * (blocksize - 1)]) = 0;
  }

  res = p->free_head;
  p->free_head = *(void **)p->free_head;

  return res;
}

void memory_pool_free(memory_pool *p, void *e) {
  if (p) {
    *(void **)e = p->free_head;
    p->free_head = e;
  }
}

void memory_pool_delete(memory_pool *p) {
  void *blockhead = p->block_head;

  while (blockhead) {
    void *next_blockhead = *(void **)blockhead;
    free(blockhead);
    blockhead = next_blockhead;
  }

  free(p);
}

/*
int main()
{
  memory_pool *p = memory_pool_new(5);

  int i;
  void *x[20];
  for(i=0; i<10; ++i){
    x[i] = memory_pool_malloc(p);
    printf("ok. allocate %dth element on %p\n", i, x[i]);
  }

  for(i=0; i<10; ++i){
    memory_pool_free(p, x[i]);
    printf("ok. free %dth element on %p\n", i, x[i]);
  }

  for(i=0; i<20; ++i){
    x[i] = memory_pool_malloc(p);
    printf("ok. allocate %dth element on %p\n", i, x[i]);
  }

  memory_pool_delete(p);
  return 0;
}
*/
