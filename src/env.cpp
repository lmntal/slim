/*
 * env.cpp
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

#include "lmntal.h"
#include "verifier/runtime_status.h"

#include <climits>

std::vector<LmnWord> *lmn_id_pool;
struct LmnEnv         lmn_env;
struct LmnProfiler    lmn_prof;
thread_local LmnTLS   lmn_tls;

// static void env_init(void);

/* TODO: slim自体をスレッドセーフ化してライブラリ化したい。
 * そうすると, 共有ライブラリとしてslimをビルドし, Java Native
 * Interfaceを利用したアプリ (例えばUNYOやLaViTとのコードレベルの連携,
 * 差分をよこせとか)がしやすくなる. そのためには,
 * グローバル変数を全てなんとかする必要がある.
 */

static inline void lmn_TLS_init(LmnTLS *p, unsigned int thread_id) {
  p->thread_num   = lmn_env.core_num;
  p->thread_id    = thread_id;
  p->state_id     = 0UL;
  p->proc_next_id = 1UL;
}

static inline void lmn_TLS_destroy(LmnTLS *p) { /* nothing now */
}

static inline LmnTLS *lmn_TLS_make(unsigned int thread_id) LMN_UNUSED;
static inline LmnTLS *lmn_TLS_make(unsigned int thread_id) {
  auto *p = LMN_MALLOC<struct LmnTLS>();
  lmn_TLS_init(p, thread_id);
  return p;
}

static inline void lmn_TLS_free(LmnTLS *p) LMN_UNUSED;
static inline void lmn_TLS_free(LmnTLS *p) {
  lmn_TLS_destroy(p);
  LMN_FREE(p);
}

void env_my_TLS_init(unsigned int th_id) {
  if (th_id == LMN_PRIMARY_ID) {
    env_set_my_thread_id(th_id);
  } else {
    lmn_TLS_init(&lmn_tls, th_id);
  }
  env_reset_proc_ids();
}

void env_my_TLS_finalize() { env_set_my_thread_id(LMN_PRIMARY_ID); /* resetする */ }

void lmn_stream_init() {
  lmn_id_pool = nullptr;
  lmn_TLS_init(&lmn_tls, LMN_PRIMARY_ID);
}

void lmn_stream_destroy() { lmn_TLS_destroy(&lmn_tls); }
