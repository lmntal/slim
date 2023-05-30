/*
 * lmntal_thread.h
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

/** @author Masato Gocho
 *  common thread library
 */

#ifndef LMN_THREAD_H
#define LMN_THREAD_H

/**
 * @ingroup Element
 * @defgroup Thread
 * @{
 */

/* check for atomic operation */
#include <cstdint>
#include <mutex>
#include <vector>

void lmn_thread_set_CPU_affinity(unsigned long id);
void lmn_thread_yield_CPU();

using mtx_data_t = unsigned long;

/* 2のべき乗を使うと, &演算で剰余を求めることができる.
 * というわけで, wlockの数は2のべき乗でないと, 困る */
constexpr auto DEFAULT_WLOCK_NUM = 16384U;
#define READABLE (TRUE)
#define DISREADABLE (FALSE)

#include "lmntal.h"

struct EWLock {
  unsigned int            elock_num;
  std::vector<std::mutex> elock{};
  unsigned long           wlock_num;
  std::vector<std::mutex> wlock{};
  EWLock(unsigned int e_num, unsigned int w_num);
  ~EWLock() = default;
  void acquire_write(mtx_data_t id);
  void release_write(mtx_data_t id);
  void acquire_enter(mtx_data_t id);
  void release_enter(mtx_data_t id);
  void reject_enter(mtx_data_t my_id);
  void permit_enter(mtx_data_t my_id);

  unsigned long space() const;
};

namespace slim::element {
enum class EWType { Write, Enter, ExclusiveEnter };
/** std::lock_guard/std::unique_lockのためのEWLockのラッパー */
template <EWType T> struct ewmutex {
private:
  EWLock       *lck;
  unsigned long id;

public:
  ewmutex(EWLock *lck, unsigned long id) : lck(lck), id(id) {}

  void lock() {
    if (lck) {
      if constexpr (T == EWType::Write)
        lck->acquire_write(id);
      else if constexpr (T == EWType::Enter)
        lck->acquire_enter(id);
      else if constexpr (T == EWType::ExclusiveEnter)
        lck->reject_enter(id);
    }
  }

  void unlock() {
    if (lck) {
      if constexpr (T == EWType::Write)
        lck->release_write(id);
      else if constexpr (T == EWType::Enter)
        lck->release_enter(id);
      else if constexpr (T == EWType::ExclusiveEnter)
        lck->permit_enter(id);
    }
  }
};
} // namespace slim::element

using WriteMutex   = slim::element::ewmutex<slim::element::EWType::Write>;
using EnterMutex   = slim::element::ewmutex<slim::element::EWType::Enter>;
using ExEnterMutex = slim::element::ewmutex<slim::element::EWType::ExclusiveEnter>;

/* @} */

#endif
