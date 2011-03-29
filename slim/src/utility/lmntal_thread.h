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

#include "lmntal.h"
#include "atom.h"

//////////////  configureで書くかlmntal.hに移動した方がよい記述ここから ///////////////
/* check for thread library */
#if defined (HAVE_LIBPTHREAD) || defined (HAVE_WINAPI)
#  define HAVE_MT_LIBRARY  (1)
#  ifdef HAVE_LIBPTHREAD
#    include <pthread.h>
#  else /* winapi */
#    include <windows.h>
#  endif
#else
#  undef  HAVE_MT_LIBRARY
#endif

/* check for thread local storage  */
#if defined (HAVE___THREAD)
#  define HAVE_TLS_KEYWORD  (1)
#  define LMN_TLS           __thread
#else
#  define LMN_TLS
#endif


#if defined (HAVE_MT_LIBRARY) && defined (HAVE_TLS_KEYWORD)
#  define ENABLE_PARALLEL
#endif

/* check for atomic operation */
#ifdef ENABLE_PARALLEL

   extern LMN_TLS unsigned long lmn_thread_id;
   extern LMN_TLS unsigned long lmn_state_id;
   extern LMN_TLS ProcessID lmn_next_id;

#  ifdef HAVE_ATOMIC_CAS /* AとBが等しければAの実態をCに置き換え, 成功したら真を返す */
#    define CAS(A, B, C)        __sync_bool_compare_and_swap(&(A), B, C)
#  else
#    define CAS(A, B, C)        lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#  endif
#  ifdef HAVE_ATOMIC_ADD /* AにBを加算し, 加算後のAの値を返す */
#    define ADD_AND_FETCH(A, B) __sync_add_and_fetch(&(A), B)
#  else
#    define ADD_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#  endif
#  ifdef HAVE_ATOMIC_SUB
#    define SUB_AND_FETCH(A, B) __sync_sub_and_fetch(&(A), B)
#  else
#    define SUB_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#  endif
#  ifdef HAVE_ATOMIC_LOGICAL_AND
#    define AND_AND_FETCH(A, B) __sync_and_and_fetch(&(A), B)
#  else
#    define AND_AND_FETCH(A, B) lmn_fatal("disable ATOMIC OPERATION, unexpected.");
#  endif
#  ifdef HAVE_ATOMIC_LOGICAL_OR
#    define OR_AND_FETCH(A, B)  __sync_or_and_fetch(&(A), B)
#  else
#    define OR_AND_FETCH(A, B)  lmn_fatal("disable ATOMIC OPERATION, unexepcted.")
#  endif
#else /* disable parallel */
   static unsigned long lmn_thread_id = 0;
   extern unsigned long lmn_state_id;
   extern ProcessID lmn_next_id;
#  define CAS(A,B,C)           (A = C)
#  define ADD_AND_FETCH(A, B)  (A += B)
#  define SUB_AND_FETCH(A, B)  (A -= B)
#  define AND_AND_FETCH(A, B)  (A &= B)
#  define OR_AND_FETCH(A, B)   (A |= B)
#endif /* ENABLE_PARALLEL */


//////////////  configureで書くかlmntal.hに移動した方がよい記述この辺まで ///////////////

/* setting thread library */
#ifdef HAVE_LIBPTHREAD
   /* ATTRとか全然書いてないから, 使いたいときに適宜追加する方向で */
   typedef pthread_t         lmn_thread_t;
   typedef pthread_mutex_t   lmn_mutex_t;
   typedef pthread_once_t    lmn_once_t;
#  if (!defined(__CYGWIN__) && !defined(__APPLE__))
     typedef pthread_barrier_t lmn_barrier_t;
#  else
     typedef struct LmnBarrier {
       unsigned int    thread_num;
       unsigned int    reach_num;
       pthread_mutex_t mutex;
       pthread_cond_t  cond;
     } lmn_barrier_t;
#  endif /* __CYGWIN__ */
#  define lmn_thread_create_with_attr(Pth, Pattr, Pfunc, Parg) \
                                               pthread_create(Pth, Pattr, (void *)Pfunc, (void *)Parg)
#  define lmn_thread_create(Pth, Pfunc, Parg)  lmn_thread_create_with_attr(Pth, NULL, Pfunc, Parg)
#  define lmn_thread_join_with_attr(Th, Pattr) pthread_join(Th, Pattr)
#  define lmn_thread_join(Th)                  lmn_thread_join_with_attr(Th, NULL)
#  define lmn_thread_self()                    pthread_self()
#  define lmn_thread_detach(Pth)               pthread_detach(Pth)
#  define lmn_thread_detach_me()               lmn_thread_detach(lmn_thread_self())
#  define lmn_mutex_init_with_attr(Pm, Pmattr) pthread_mutex_init(Pm, Pmattr)
#  define lmn_mutex_init(Pm)                   lmn_mutex_init_with_attr(Pm, NULL)
#  define lmn_mutex_init_onthefly(Pm)          (Pm) = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER
#  define lmn_mutex_destroy(Pm)                pthread_mutex_destroy(Pm)
#  define lmn_mutex_lock(Pm)                   pthread_mutex_lock(Pm)
#  define lmn_mutex_unlock(Pm)                 pthread_mutex_unlock(Pm)
#  define lmn_thread_once_init(Po)             (Po) = (pthread_once_t) PTHREAD_ONCE_INIT
#  define lmn_thread_once(Po, Func)            pthread_once(Po, (void *)Func)
#  if (!defined(__CYGWIN__) && !defined(__APPLE__))
#    define lmn_barrier_init_with_attr(Pm, At, Num) \
                                                 pthread_barrier_init(Pm, At, Num)
#    define lmn_barrier_init(Pm, Num)            lmn_barrier_init_with_attr(Pm, NULL, Num)
#    define lmn_barrier_destroy(Pm)              pthread_barrier_destroy(Pm)
#    define lmn_barrier_wait(Pm)                 pthread_barrier_wait(Pm)
#  else
     inline static void lmn_barrier_init(lmn_barrier_t *b, unsigned int num) {
       b->thread_num = num;
       b->reach_num = 0;
       b->mutex = (pthread_mutex_t) PTHREAD_MUTEX_INITIALIZER;
       b->cond  = (pthread_cond_t) PTHREAD_COND_INITIALIZER;
     }
     inline static void lmn_barrier_destroy(lmn_barrier_t *b) {
       pthread_mutex_destroy(&b->mutex);
       pthread_cond_destroy(&b->cond);
     }
     inline static void lmn_barrier_wait(lmn_barrier_t *b) {
       pthread_mutex_lock(&b->mutex);
       b->reach_num++;
       if (b->reach_num != b->thread_num) {
         pthread_cond_wait(&b->cond, &b->mutex);
       } else { /* ok! */
         b->reach_num = 0;
         pthread_cond_broadcast(&b->cond);
       }
       pthread_mutex_unlock(&b->mutex);
     }
#  endif /* __CYGWIN__ */
#else
#  error "need pthread.h"
#endif /* HAVE_LIBPTHREAD */

void thread_set_cpu_affinity(unsigned long id);
#ifdef HAVE_SCHED_H
void thread_yield_cpu(void);
#  define lmn_thread_yield_cpu() thread_yield_cpu()
#else
#  define lmn_thread_yield_cpu()
#endif


typedef struct EWLock EWLock;
typedef unsigned long mtx_data_t;

/* 16384 = 2^9 (9は適当)2のべき乗を使うと, &演算で剰余を求めることができる
 * というわけで, STRIPE_GRANULARITYは2のべき乗でないと, 困る */
#define STRIPE_GRANULARITY    (16384UL)
#define READABLE              (TRUE)
#define DISREADABLE           (FALSE)

struct EWLock {
  BOOL *elock_used;
  unsigned int elock_num;
  lmn_mutex_t *elock;
  unsigned long wlock_num;
  lmn_mutex_t *wlock;
};


#define lmn_ewlock_space(L) ((L) ? (                                          \
                                     (sizeof(L)                               \
                                   + ((L)->elock_num * sizeof(lmn_mutex_t))   \
                                   + ((L)->wlock_num * sizeof(lmn_mutex_t)))) \
                                :  0)

/* TODO: ##は文字列の連結. 移植性はある？ */
/** ENTER__CRITICAL_SECTIONとEXIT___CRITICAL_SECTIONは必ずペアで使用する.
 * CsName              :   1つのクリティカルセクションに対してプログラマがつけるユニークな名前.
 * LockPtr             :   排他制御を行うためのロックオブジェクトのアドレス
 * LockFunc/UnLockFunc :   LockPtrおよびFuncArgを引数にした排他制御関数Lock/UnLockを呼び出す.
 * Fetch_v, Fetch_ptr  :   Fecth_ptrのアドレスが指す値とFecth_vの値が異なる場合,
 *                         クリティカルセクション内部の処理をスキップし, 組にしたEXIT___CRITICAL_SECTIONへjumpする.
 */
#define ENTER__CRITICAL_SECTION(CsName, LockPtr, LockFunc, FuncArg, Fetch_v, Fetch_ptr) do { \
  if (LockPtr) { /* if MT */                                                                 \
    if ((Fetch_v) != (Fetch_ptr)) {                                                          \
      goto CS_EXIT_NOTHING_TO_DO__ ## CsName;                                                \
    }                                                                                        \
    else {                                                                                   \
      LockFunc((LockPtr), (FuncArg));                                                        \
      if ((Fetch_v) != (Fetch_ptr)) {                                                        \
        goto CS_EXIT_WITH_UNLOCK__ ## CsName;                                                \
      }                                                                                      \
    }                                                                                        \
  }                                                                                          \
} while (0)

#define EXIT___CRITICAL_SECTION(CsName, LockPtr, UnLockFunc, FuncArg)  \
CS_EXIT_WITH_UNLOCK__ ## CsName:                                       \
  if (LockPtr) {                                                       \
    UnLockFunc((LockPtr), (FuncArg));                                  \
  }                                                                    \
CS_EXIT_NOTHING_TO_DO__ ## CsName:

/* simply */
#define START__CRITICAL_SECTION(LockPtr, LockFunc, FuncArg) \
  if (LockPtr) {                                            \
    LockFunc((LockPtr), (FuncArg));                         \
  }

#define FINISH_CRITICAL_SECTION(LockPtr, UnLockFunc, FuncArg) \
  if (LockPtr) {                                              \
    UnLockFunc((LockPtr), (FuncArg));                         \
  }


#define DEFAULT_LOCK_ID  0

EWLock  *ewlock_make(void);
void     ewlock_free(EWLock *lock);
void     ewlock_acquire_write(EWLock *lock, unsigned long use_id);
void     ewlock_release_write(EWLock *lock, unsigned long use_id);
void     ewlock_acquire_enter(EWLock *lock, unsigned long something);
void     ewlock_release_enter(EWLock *lock, unsigned long something);
void     ewlock_reject_enter(EWLock *lock, unsigned long something);
void     ewlock_permit_enter(EWLock *lock, unsigned long something);


#endif
