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

#define _GNU_SOURCE
#include "lmntal_thread.h"
#include "util.h"
#include "element/error.h"
#if defined (HAVE_SCHED_H) && defined (HAVE_SYSCALL_H)
# include <sched.h>
# include <syscall.h>
# include <unistd.h>
# include <sys/types.h>
# define ENABLE_CPU_AFFINITY
#endif

/* 呼び出したスレッドとn番のCPUを貼り付ける */
void lmn_thread_set_CPU_affinity(unsigned long n)
{
  /* TODO: マニュアルによればpthread_npライブラリを使った方がよい  */
#ifdef ENABLE_CPU_AFFINITY
  if (lmn_env.core_num <= HAVE_PROCESSOR_ELEMENTS) {
    pid_t      my_pid;
    cpu_set_t  my_mask;
    my_pid = syscall(SYS_gettid);
    CPU_ZERO(&my_mask);
    sched_setaffinity(my_pid, sizeof(my_mask), &my_mask);
  }
#endif
}

#ifdef HAVE_SCHED_H
void thread_yield_CPU()
{
  sched_yield();
}
#endif


/** ----------------------------------
 *  Double Lock
 */

/* TODO: stripeの粒度を呼出側で指定できた方が汎用的だと思う */
EWLock *ewlock_make(unsigned int e_num, unsigned int w_num)
{
  EWLock *lock;
  unsigned int i;
  w_num = round2up(w_num);

  lock = LMN_MALLOC(EWLock);
  lock->elock_used   = NULL;
  lock->elock_num    = e_num;
  lock->elock        = NULL;
  lock->wlock_num    = w_num;
  lock->wlock        = NULL;

  lock->elock = LMN_NALLOC(lmn_mutex_t, e_num);
  for (i = 0; i < e_num; i++) {
    lmn_mutex_init(&(lock->elock[i]));
  }

  lock->wlock = LMN_NALLOC(pthread_mutex_t, w_num);
  for (i = 0; i < w_num; i++) {
    lmn_mutex_init_onthefly(lock->wlock[i]);
  }

  return lock;
}

void ewlock_free(EWLock *lock)
{
  unsigned long i, e_num, w_num;

  e_num = lock->elock_num;
  w_num = lock->wlock_num;

  for (i = 0; i < e_num; i++) {
    lmn_mutex_destroy(&(lock->elock[i]));
  }
  LMN_FREE(lock->elock);

  for (i = 0; i < w_num; i++) {
    lmn_mutex_destroy(&(lock->wlock[i]));
  }
  LMN_FREE(lock->wlock);
  LMN_FREE(lock);
}


void ewlock_acquire_write(EWLock *lock, mtx_data_t id)
{
  unsigned long idx = id & (lock->wlock_num - 1);
  lmn_mutex_lock(&(lock->wlock[idx]));
}

void ewlock_release_write(EWLock *lock, mtx_data_t id)
{
  unsigned long idx = id & (lock->wlock_num - 1);
  lmn_mutex_unlock(&(lock->wlock[idx]));
}

void ewlock_acquire_enter(EWLock *lock, mtx_data_t id)
{
  id = id >= lock->elock_num ? id % lock->elock_num
                            : id;
  lmn_mutex_lock(&(lock->elock[id]));
}

void ewlock_release_enter(EWLock *lock, mtx_data_t id)
{
  id = id >= lock->elock_num ? id % lock->elock_num
                             : id;
  lmn_mutex_unlock(&(lock->elock[id]));
}

/* lockが持つelockを昇順に確保していく.
 * 呼びたしスレッドがelockを既に確保していた場合の処理は未定義 (単にskipするだけでもよいような) */
void ewlock_reject_enter(EWLock *lock, mtx_data_t my_id)
{
  unsigned long i, n = lock->elock_num;
  for (i = 0; i < n; i++) {
    lmn_mutex_lock(&(lock->elock[i]));
  }
}

void ewlock_permit_enter(EWLock *lock, mtx_data_t my_id)
{
  unsigned long i, n = lock->elock_num;
  for (i = 0; i < n; i++) {
    lmn_mutex_unlock(&(lock->elock[i]));
  }
}


