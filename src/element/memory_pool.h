/*
 * memory_pool.h
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
 * $Id: memory_pool.h,v 1.2 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_MEMORY_POOL_H
#define LMN_MEMORY_POOL_H

/**
 * @ingroup Element
 * @defgroup MemoryPool
 * @{
 */

#include <cstddef>
#include <cstdlib>
#include <memory_resource>

/* each element must be bigger than void*, so align everything in sizeof(void*)
 * !! */
/* after alignment, X byte object needs ALIGNED_SIZE(X) byte. */
constexpr auto aligned_size(int x) { return ((x + sizeof(void *) - 1) / sizeof(void *)) * sizeof(void *); }

struct memory_pool : std::pmr::memory_resource {
  size_t sizeof_element;
  void  *block_head{};
  void  *free_head{};

  memory_pool(int s) : sizeof_element(aligned_size(s)) {}
  ~memory_pool() override {
    auto *blockhead = block_head;

    while (blockhead) {
      auto *next_blockhead = *(void **)blockhead;
      std::free(blockhead);
      blockhead = next_blockhead;
    }
  }

  void *do_allocate(size_t bytes, size_t alignment) override;
  void  do_deallocate(void *p, size_t bytes, size_t alignment) override;
  bool  do_is_equal(std::pmr::memory_resource const &other) const noexcept override;
};

/* @} */

#endif /* LMN_MEMORY_POOL_H */
