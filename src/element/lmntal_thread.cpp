/*
 * lmntal_thread.c
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
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "lmntal_thread.h"
#include "error.h"
#include "util.h"
#if defined(HAVE_SCHED_H) && defined(HAVE_SYSCALL_H)
#include <sched.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>
#define ENABLE_CPU_AFFINITY
#endif

/* 呼び出したスレッドとn番のCPUを貼り付ける */
void lmn_thread_set_CPU_affinity(unsigned long n) {
#ifdef ENABLE_CPU_AFFINITY
  if (lmn_env.core_num <= HAVE_PROCESSOR_ELEMENTS) {
    pid_t     my_pid;
    cpu_set_t my_mask;
    my_pid = syscall(SYS_gettid);
    CPU_ZERO(&my_mask);
    sched_setaffinity(my_pid, sizeof(my_mask), &my_mask);
  }
#endif
}

#ifdef HAVE_SCHED_H
void thread_yield_CPU() { sched_yield(); }
#endif

/** ----------------------------------
 *  Double Lock
 */

/* TODO: stripeの粒度を呼出側で指定できた方が汎用的だと思う */
EWLock::EWLock(unsigned int e_num, unsigned int w_num):elock(e_num),wlock(w_num) {
  unsigned int i;
  w_num            = std::bit_ceil(w_num);
  this->elock_used = nullptr;
  this->elock_num  = e_num;
  this->wlock_num  = w_num;
}

EWLock::~EWLock() {
  LMN_FREE(this);
}

void ewlock_free(EWLock *lock) {
  LMN_FREE(lock);
}
void EWLock::acquire_write(mtx_data_t id) {
  unsigned long idx = id & (this->wlock_num - 1);
  this->wlock[idx].lock();
}
void ewlock_acquire_write(EWLock *lock, mtx_data_t id) {
  unsigned long idx = id & (lock->wlock_num - 1);
  lock->wlock[idx].lock();
}
void EWLock::release_write(mtx_data_t id) {
  unsigned long idx = id & (this->wlock_num - 1);
  this->wlock[idx].unlock();
}
void ewlock_release_write(EWLock *lock, mtx_data_t id) {
  unsigned long idx = id & (lock->wlock_num - 1);
  lock->wlock[idx].unlock();
}
void EWLock::acquire_enter(mtx_data_t id) {
  id = id >= this->elock_num ? id % this->elock_num : id;
  this->elock[id].lock();
}
void ewlock_acquire_enter(EWLock *lock, mtx_data_t id) {
  id = id >= lock->elock_num ? id % lock->elock_num : id;
  lock->elock[id].lock();
}
void EWLock::release_enter(mtx_data_t id) {
  id = id >= this->elock_num ? id % this->elock_num : id;
  this->elock[id].unlock();
}
void ewlock_release_enter(EWLock *lock, mtx_data_t id) {
  id = id >= lock->elock_num ? id % lock->elock_num : id;
  lock->elock[id].unlock();
}

/* lockが持つelockを昇順に確保していく.
 * 呼びたしスレッドがelockを既に確保していた場合の処理は未定義
 * (単にskipするだけでもよいような) */
void EWLock::reject_enter(mtx_data_t my_id) {
  unsigned long i, n = this->elock_num;
  for (i = 0; i < n; i++) {
    this->elock[i].lock();
  }
}
void ewlock_reject_enter(EWLock *lock, mtx_data_t my_id) {
  unsigned long i, n = lock->elock_num;
  for (i = 0; i < n; i++) {
    lock->elock[i].lock();
  }
}
void EWLock::permit_enter(mtx_data_t my_id) {
  unsigned long i, n = this->elock_num;
  for (i = 0; i < n; i++) {
    this->elock[i].unlock();
  }
}
void ewlock_permit_enter(EWLock *lock, mtx_data_t my_id) {
  unsigned long i, n = lock->elock_num;
  for (i = 0; i < n; i++) {
    // lmn_mutex_unlock(&(lock->elock[i]));
    lock->elock[i].unlock();
  }
}
