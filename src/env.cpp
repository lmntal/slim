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

struct Vector *lmn_id_pool;
struct LmnEnv lmn_env;
struct LmnProfiler lmn_prof;
LMN_TLS_TYPE(LmnTLS) lmn_tls;

//static void env_init(void);

/* TODO: slim自体をスレッドセーフ化してライブラリ化したい。
 * そうすると, 共有ライブラリとしてslimをビルドし, Java Native
 * Interfaceを利用したアプリ (例えばUNYOやLaViTとのコードレベルの連携,
 * 差分をよこせとか)がしやすくなる. そのためには,
 * グローバル変数を全てなんとかする必要がある.
 */

/* LMN_TLS部にpthread_key_tを埋め込む場合は, free関数をdestructorに渡しておく.
 */

static inline void lmn_TLS_init(LmnTLS *p, unsigned int thread_id) {
  p->thread_num = lmn_env.core_num;
  p->thread_id = thread_id;
  p->state_id = 0UL;
  p->proc_next_id = 1UL;
}

static inline void lmn_TLS_destroy(LmnTLS *p) { /* nothing now */ }

static inline LmnTLS *lmn_TLS_make(unsigned int thread_id) LMN_UNUSED;
static inline LmnTLS *lmn_TLS_make(unsigned int thread_id) {
  struct LmnTLS *p = LMN_MALLOC<struct LmnTLS>();
  lmn_TLS_init(p, thread_id);
  return p;
}

static inline void lmn_TLS_free(LmnTLS *p) LMN_UNUSED;
static inline void lmn_TLS_free(LmnTLS *p) {
  lmn_TLS_destroy(p);
  LMN_FREE(p);
}

void env_my_TLS_init(unsigned int th_id) {
#if !defined(ENABLE_PARALLEL) || defined(USE_TLS_KEYWORD)
  if (th_id == LMN_PRIMARY_ID) {
    env_set_my_thread_id(th_id);
  } else {
    lmn_TLS_init(&lmn_tls, th_id);
  }
#elif defined(USE_TLS_PTHREAD_KEY)
  if (th_id != LMN_PRIMARY_ID) {
    lmn_TLS_set_value(lmn_tls, lmn_TLS_make(th_id));
  }
#endif
  env_reset_proc_ids();
}

void env_my_TLS_finalize() {
#if !defined(ENABLE_PARALLEL) || defined(USE_TLS_KEYWORD)
  env_set_my_thread_id(LMN_PRIMARY_ID); /* resetする */
#elif defined(USE_TLS_PTHREAD_KEY)
  if (env_my_thread_id() != LMN_PRIMARY_ID) {
    lmn_TLS_free(lmn_TLS_get_value(lmn_tls));
  }
#endif
}

void lmn_stream_init() {
//  lmn_env.init();

  lmn_id_pool = NULL;
#if !defined(ENABLE_PARALLEL) || defined(USE_TLS_KEYWORD)
  /* 並列処理無効の場合か, 並列処理有効かつthread local
   * storageキーワードが利用可能な場合 */
  lmn_TLS_init(&lmn_tls, LMN_PRIMARY_ID);
#elif defined(USE_TLS_PTHREAD_KEY)
  lmn_TLS_key_init(&lmn_tls);
  lmn_TLS_set_value(lmn_tls, lmn_TLS_make(LMN_PRIMARY_ID));
  /* pthread_join後も, primaryだけはprofile出力などに使用するため,
   * 子供たちとはTLS objectのmalloc/freeタイミングをずらす */
#else
#error "oops.. "
#endif
}

void lmn_stream_destroy() {
#if !defined(ENABLE_PARALLEL) || defined(USE_TLS_KEYWORD)
  lmn_TLS_destroy(&lmn_tls);
#elif defined(USE_TLS_PTHREAD_KEY)
  lmn_TLS_free(lmn_TLS_get_value(lmn_tls));
  lmn_TLS_key_destroy(lmn_tls);
#endif
}

/* lmn_env構造体の初期化 */
LmnEnv::LmnEnv() {
  trace = FALSE;
  this->show_proxy = FALSE;
  this->show_chr = FALSE;
  this->show_ruleset = TRUE;
  this->output_format = DEFAULT;
  this->mc_dump_format = CUI;
  this->sp_dump_format = SP_NONE;
  this->show_laststep_only = FALSE;
  this->nd = FALSE;
  this->ltl = FALSE;
  this->ltl_all = FALSE;
  this->enable_por_old = FALSE;
  this->enable_por = FALSE;
  this->show_transition = FALSE;
  this->translate = FALSE;
  this->optimization_level = 3;
  this->profile_level = 0;
  this->load_path_num = 0;
  this->automata_file = NULL;
  this->propositional_symbol = NULL;
  this->ltl_exp = NULL;
  this->bfs = FALSE;
  this->prop_scc_driven = FALSE;
  this->depth_limits = UINT_MAX;
  this->nd_search_end = FALSE;
  this->mem_enc = FALSE;
  this->delta_mem = FALSE;
  this->dump = TRUE;
  this->end_dump = FALSE;
  this->benchmark = FALSE;
  this->property_dump = FALSE;
  this->enable_compress_mem = TRUE;
  this->z_compress = FALSE;
  this->d_compress = FALSE;
  this->r_compress = FALSE;
  this->enable_parallel = FALSE;
  this->core_num = 1;
  this->cutoff_depth = 7;
  this->optimize_lock = FALSE;
  this->optimize_hash = TRUE;
  this->optimize_loadbalancing = TRUE;

  this->opt_mode = OPT_NONE;
  
  /* only jni-interactive mode */
  this->interactive = FALSE;
  this->normal_remain = FALSE;
  this->normal_remaining = FALSE;
  this->normal_cleaning = FALSE;
  this->nd_remain = FALSE;
  this->nd_remaining = FALSE;
  this->nd_cleaning = FALSE;

  this->enable_owcty = FALSE;
  this->enable_map = FALSE;
  this->enable_bledge = FALSE;
  this->bfs_layer_sync = FALSE;

  this->enable_map_heuristic = TRUE;

  this->show_reduced_graph = FALSE;

  this->hash_compaction = FALSE;
  this->tree_compress = FALSE;
  this->hash_depth = 2;
  this->tree_compress_table_size = 20;
#ifdef PROFILE
  this->optimize_hash_old = FALSE;
  this->prof_no_memeq = FALSE;
#endif

  // 履歴管理用アトム(nakata)
  this->history_management = FALSE;
  
#ifdef DEBUG
  this->debug_por_dep = FALSE;
  this->debug_id = FALSE;
  this->debug_delta = FALSE;
  this->debug_hash = FALSE;
  this->debug_isomor = FALSE;
  this->debug_mc = FALSE;
  this->debug_por = FALSE;
#endif

  this->shuffle_rule = FALSE;
  this->shuffle_atom = FALSE;
  
  this->findatom_parallel_mode = FALSE;
  this->find_atom_parallel = FALSE;
  this->findatom_parallel_inde = FALSE;
}
