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
#ifdef ENABLE_PARALLEL
#
#ifdef HAVE_ATOMIC_CAS /* AとBが等しければAの実態をCに置き換え,                                        \
                          成功したら真を返す */
#define CAS(A, B, C) __sync_bool_compare_and_swap(&(A), B, C)
#else
#define CAS(A, B, C) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#endif /* HAVE_ATOMIC_CAS */
#
#ifdef HAVE_ATOMIC_ADD /* AにBを加算し, 加算後のAの値を返す */
#define ADD_AND_FETCH(A, B) __sync_add_and_fetch(&(A), B)
#else
#define ADD_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#endif /* HAVE_ATOMIC_ADD */
#
#ifdef HAVE_ATOMIC_SUB
#define SUB_AND_FETCH(A, B) __sync_sub_and_fetch(&(A), B)
#else
#define SUB_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#endif /* HAVE_ATOMIC_SUB */
#
#ifdef HAVE_ATOMIC_LOGICAL_AND
#define AND_AND_FETCH(A, B) __sync_and_and_fetch(&(A), B)
#else
#define AND_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#endif /* HAVE_ATOMIC_LOGICAL_AND */
#
#ifdef HAVE_ATOMIC_LOGICAL_OR
#define OR_AND_FETCH(A, B) __sync_or_and_fetch(&(A), B)
#else
#define OR_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexepcted.")
#endif /* HAVE_ATOMIC_LOGICAL_OR */
#
#ifdef HAVE_BUILTIN_MBARRIER
#define MEM_BARRIER() __sync_synchronize()
#endif /* HAVE_BUILTIN_MBARRIER */
#
#else /* !defined(ENABLE_PARALLEL) */
#
#define CAS(A, B, C) (A = C)
#define ADD_AND_FETCH(A, B) (A += B)
#define SUB_AND_FETCH(A, B) (A -= B)
#define AND_AND_FETCH(A, B) (A &= B)
#define OR_AND_FETCH(A, B) (A |= B)
#define MEM_BARRIER() lmn_fatal("__sync_synchronize is unsupported")
#endif /* ENABLE_PARALLEL */

void lmn_thread_set_CPU_affinity(unsigned long id);
#ifdef HAVE_SCHED_H
#
void thread_yield_CPU();
#
#define lmn_thread_yield_CPU() thread_yield_CPU()
#
#else
#define lmn_thread_yield_CPU()
#endif

using mtx_data_t = unsigned long;

/* 2のべき乗を使うと, &演算で剰余を求めることができる.
 * というわけで, wlockの数は2のべき乗でないと, 困る */
constexpr auto DEFAULT_WLOCK_NUM = 16384U;
#define READABLE (TRUE)
#define DISREADABLE (FALSE)

#include "../lmntal.h"

struct EWLock {
  BOOL         *elock_used;
  unsigned int  elock_num;
  lmn_mutex_t  *elock;
  unsigned long wlock_num;
  lmn_mutex_t  *wlock;
  EWLock(unsigned int e_num, unsigned int w_num);
  ~EWLock();
  void acquire_write(mtx_data_t id);
  void release_write(mtx_data_t id);
  void acquire_enter(mtx_data_t id);
  void release_enter(mtx_data_t id);
  void reject_enter(mtx_data_t my_id);
  void permit_enter(mtx_data_t my_id);
};

#define lmn_ewlock_space(L)                                                                                            \
  (!(L) ? 0 : ((sizeof(L) + ((L)->elock_num * sizeof(lmn_mutex_t)) + ((L)->wlock_num * sizeof(lmn_mutex_t)))))

#define DEFAULT_LOCK_ID 0

void ewlock_free(EWLock *lock);
void ewlock_acquire_write(EWLock *lock, unsigned long use_id);
void ewlock_release_write(EWLock *lock, unsigned long use_id);
void ewlock_acquire_enter(EWLock *lock, unsigned long something);
void ewlock_release_enter(EWLock *lock, unsigned long something);
void ewlock_reject_enter(EWLock *lock, unsigned long something);
void ewlock_permit_enter(EWLock *lock, unsigned long something);

namespace slim::element {
/** std::lock_guard/std::unique_lockのためのEWLockのラッパー */
struct ewmutex {
private:
  EWLock       *lck;
  unsigned long id;

  struct ewmutex_tag {
    void (*lock)(EWLock *, unsigned long);
    void (*unlock)(EWLock *, unsigned long);
  };
  const ewmutex_tag tag;

public:
  static constexpr ewmutex_tag write{ewlock_acquire_write, ewlock_release_write};
  static constexpr ewmutex_tag enter{ewlock_acquire_enter, ewlock_release_enter};
  static constexpr ewmutex_tag exclusive_enter{ewlock_reject_enter, ewlock_permit_enter};

  ewmutex(ewmutex_tag tag, EWLock *lck, unsigned long id) : tag(tag), lck(lck), id(id) {}

  void lock() {
    if (lck)
      tag.lock(lck, id);
  }

  void unlock() {
    if (lck)
      tag.unlock(lck, id);
  }
};
} // namespace slim::element

/* @} */

#endif
