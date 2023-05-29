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

#include "lmntal_thread.h"
#include "error.h"
#include "util.h"
#include <thread>
#ifdef __linux__
#include <sched.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>
#elif defined __APPLE__ && defined __MACH__
#include <mach/mach.h>
#include <mach/thread_policy.h>
#include <pthread.h>
#elif defined _WIN32
#include <windows.h>
#endif

/* 呼び出したスレッドとn番のCPUを貼り付ける */
void lmn_thread_set_CPU_affinity(unsigned long id) {
#ifdef __linux__
  if (lmn_env.core_num <= std::thread::hardware_concurrency()) {
    cpu_set_t mask;
    CPU_ZERO(&mask);
    CPU_SET(id, &mask);
    sched_setaffinity(syscall(SYS_gettid), sizeof(cpu_set_t), &mask);
  }
#elif defined __APPLE__ && defined __MACH__
  if (lmn_env.core_num <= std::thread::hardware_concurrency()) {
    thread_affinity_policy_data_t policy;
    policy.affinity_tag = id;
    thread_policy_set(pthread_mach_thread_np(pthread_self()), THREAD_AFFINITY_POLICY, (thread_policy_t)&policy,
                      THREAD_AFFINITY_POLICY_COUNT);
  }
#elif defined                      _WIN32
  if (lmn_env.core_num <= std::thread::hardware_concurrency()) {
    HANDLE hThread              = GetCurrentThread();
    DWORD  dwThreadAffinityMask = 1 << id;
    SetThreadAffinityMask(hThread, dwThreadAffinityMask);
  }
#endif
}

void lmn_thread_yield_CPU() {
#ifdef __linux__
  sched_yield();
#elif defined __APPLE__ && defined __MACH__
  sched_yield();
#elif defined                      _WIN32
  SwitchToThread();
#endif
}

/** ----------------------------------
 *  Double Lock
 */

/* TODO: stripeの粒度を呼出側で指定できた方が汎用的だと思う */
EWLock::EWLock(unsigned int e_num, unsigned int w_num)
    : elock(e_num), wlock(w_num), elock_num(e_num), wlock_num(std::bit_ceil(w_num)) {}

void EWLock::acquire_write(mtx_data_t id) {
  unsigned long idx = id & (this->wlock_num - 1);
  this->wlock[idx].lock();
}
void EWLock::release_write(mtx_data_t id) {
  unsigned long idx = id & (this->wlock_num - 1);
  this->wlock[idx].unlock();
}
void EWLock::acquire_enter(mtx_data_t id) {
  id = id >= this->elock_num ? id % this->elock_num : id;
  this->elock[id].lock();
}
void EWLock::release_enter(mtx_data_t id) {
  id = id >= this->elock_num ? id % this->elock_num : id;
  this->elock[id].unlock();
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
void EWLock::permit_enter(mtx_data_t my_id) {
  unsigned long i, n = this->elock_num;
  for (i = 0; i < n; i++) {
    this->elock[i].unlock();
  }
}

unsigned long EWLock::space() const {
  return ((sizeof(EWLock) + (elock_num * elock.capacity()) + (wlock_num * wlock.capacity())));
}