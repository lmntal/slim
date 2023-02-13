/*
 * clock.c
 *
 *   Copyright (c) 2016, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
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

#include "clock.h"

#include "lmntal.h" // config.h
#include <stdio.h>

#if defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#include <time.h>
#define ENABLE_TIME_PROFILE
#endif

/* ------------------------------------------------------------------------
 * CPU時間・実経過時間を返す.
 * 取得可能な時間がナノ秒ではあるが精度もその通りであるとは限らないため注意
 */

/* スレッド単位で計測したCPU時間は,
 * プロセッサ間でスレッドがスイッチした場合に誤差がでるので結果は鵜呑みせずあくまで目安とする
 */
double get_cpu_time() {
#ifdef ENABLE_TIME_PROFILE
#if defined(HAVE_LIBRT) && defined(CLOCK_THREAD_CPUTIME_ID)
  /* 環境によってはCLOCK_THREAD_CPUTIME_IDが定義されていない. */
  struct timespec ts;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
  return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
#else
  /* この場合, CPU Usageはプロセス単位(全スレッド共有)になってしまう */
  return clock() / (double)CLOCKS_PER_SEC;
#endif
#
#else
  fprintf(stderr, "not support the time profiler on this environment.");
#endif
}

double get_wall_time() {
#ifdef ENABLE_TIME_PROFILE
#ifdef HAVE_LIBRT
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9; /* ナノ秒 */
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec + (double)tv.tv_usec * 1e-6; /* マイクロ秒 */
#endif
#
#else
  fprintf(stderr, "not support the time profiler on this environment.");
#endif
}
