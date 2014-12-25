/*
 * env.c
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
#include "runtime_status.h"

struct Vector         *lmn_id_pool;
struct LmnEnv         lmn_env;
struct LmnProfiler    lmn_prof;
LMN_TLS_TYPE(LmnTLS)  lmn_tls;


static void env_init(void);

/* TODO: slim自体をスレッドセーフ化してライブラリ化したい。
 * そうすると, 共有ライブラリとしてslimをビルドし, Java Native Interfaceを利用したアプリ
 * (例えばUNYOやLaViTとのコードレベルの連携, 差分をよこせとか)がしやすくなる.
 * そのためには, グローバル変数を全てなんとかする必要がある.
 */

/* LMN_TLS部にpthread_key_tを埋め込む場合は, free関数をdestructorに渡しておく. */


static inline void lmn_TLS_init(LmnTLS *p, unsigned int thread_id) {
  p->thread_num   = lmn_env.core_num;
  p->thread_id    = thread_id;
  p->state_id     = 0UL;
  p->proc_next_id = 1UL;
}

static inline void lmn_TLS_destroy(LmnTLS *p) {
  /* nothing now */
}

static inline LmnTLS *lmn_TLS_make(unsigned int thread_id) {
  struct LmnTLS *p = LMN_MALLOC(struct LmnTLS);
  lmn_TLS_init(p, thread_id);
  return p;
}

static inline void lmn_TLS_free(LmnTLS *p) {
  lmn_TLS_destroy(p);
  LMN_FREE(p);
}

void env_my_TLS_init(unsigned int th_id)
{
#if !defined (ENABLE_PARALLEL) || defined (USE_TLS_KEYWORD)
  if (th_id == LMN_PRIMARY_ID){
    env_set_my_thread_id(th_id);
  } else {
    lmn_TLS_init(&lmn_tls, th_id);
  }
#elif defined (USE_TLS_PTHREAD_KEY)
  if (th_id != LMN_PRIMARY_ID) {
    lmn_TLS_set_value(lmn_tls, lmn_TLS_make(th_id));
  }
#endif
  env_reset_proc_ids();
}


void env_my_TLS_finalize()
{
#if !defined (ENABLE_PARALLEL) || defined (USE_TLS_KEYWORD)
  env_set_my_thread_id(LMN_PRIMARY_ID); /* resetする */
#elif defined (USE_TLS_PTHREAD_KEY)
  if (env_my_thread_id() != LMN_PRIMARY_ID) {
    lmn_TLS_free(lmn_TLS_get_value(lmn_tls));
  }
#endif
}


void lmn_stream_init()
{
  env_init();

  lmn_id_pool = NULL;
#if !defined (ENABLE_PARALLEL) || defined (USE_TLS_KEYWORD)
  /* 並列処理無効の場合か, 並列処理有効かつthread local storageキーワードが利用可能な場合 */
  lmn_TLS_init(&lmn_tls, LMN_PRIMARY_ID);
#elif defined (USE_TLS_PTHREAD_KEY)
  lmn_TLS_key_init(&lmn_tls);
  lmn_TLS_set_value(lmn_tls, lmn_TLS_make(LMN_PRIMARY_ID));
  /* pthread_join後も, primaryだけはprofile出力などに使用するため,
   * 子供たちとはTLS objectのmalloc/freeタイミングをずらす */
#else
# error "oops.. "
#endif
}

void lmn_stream_destroy()
{
#if !defined (ENABLE_PARALLEL) || defined (USE_TLS_KEYWORD)
  lmn_TLS_destroy(&lmn_tls);
#elif defined (USE_TLS_PTHREAD_KEY)
  lmn_TLS_free(lmn_TLS_get_value(lmn_tls));
  lmn_TLS_key_destroy(lmn_tls);
#endif
}


/* lmn_env構造体の初期化 */
void env_init()
{
  lmn_env.trace                  = FALSE;
  lmn_env.show_proxy             = FALSE;
  lmn_env.show_chr               = FALSE;
  lmn_env.show_ruleset           = TRUE;
  lmn_env.output_format          = DEFAULT;
  lmn_env.mc_dump_format         = CUI;
  lmn_env.sp_dump_format         = SP_NONE;
  lmn_env.nd                     = FALSE;
  lmn_env.ltl                    = FALSE;
  lmn_env.ltl_all                = FALSE;
  lmn_env.enable_por_old         = FALSE;
  lmn_env.enable_por             = FALSE;
  lmn_env.show_transition        = FALSE;
  lmn_env.translate              = FALSE;
  lmn_env.optimization_level     = 3;
  lmn_env.profile_level          = 0;
  lmn_env.load_path_num          = 0;
  lmn_env.automata_file          = NULL;
  lmn_env.propositional_symbol   = NULL;
  lmn_env.ltl_exp                = NULL;
  lmn_env.bfs                    = FALSE;
  lmn_env.prop_scc_driven        = FALSE;
  lmn_env.depth_limits           = UINT_MAX;
  lmn_env.nd_search_end          = FALSE;
  lmn_env.mem_enc                = FALSE;
  lmn_env.delta_mem              = FALSE;
  lmn_env.dump                   = TRUE;
  lmn_env.end_dump               = FALSE;
  lmn_env.benchmark              = FALSE;
  lmn_env.property_dump          = FALSE;
  lmn_env.enable_compress_mem    = TRUE;
  lmn_env.z_compress             = FALSE;
  lmn_env.d_compress             = FALSE;
  lmn_env.r_compress             = FALSE;
  lmn_env.enable_parallel        = FALSE;
  lmn_env.core_num               = 1;
  lmn_env.cutoff_depth           = 7;
  lmn_env.optimize_lock          = FALSE;
  lmn_env.optimize_hash          = TRUE;
  lmn_env.optimize_loadbalancing = TRUE;

  lmn_env.opt_mode               = OPT_NONE;

  /* only jni-interactive mode */
  lmn_env.interactive            = FALSE;
  lmn_env.normal_remain          = FALSE;
  lmn_env.normal_remaining       = FALSE;
  lmn_env.normal_cleaning        = FALSE;
  lmn_env.nd_remain              = FALSE;
  lmn_env.nd_remaining           = FALSE;
  lmn_env.nd_cleaning            = FALSE;

  lmn_env.enable_owcty           = FALSE;
  lmn_env.enable_map             = FALSE;
  lmn_env.enable_bledge          = FALSE;
  lmn_env.bfs_layer_sync         = FALSE;

  lmn_env.enable_map_heuristic   = TRUE;

  lmn_env.show_reduced_graph     = FALSE;

  lmn_env.hash_compaction        = FALSE;
  lmn_env.hash_depth             = 2;
#ifdef PROFILE
  lmn_env.optimize_hash_old      = FALSE;
  lmn_env.prof_no_memeq          = FALSE;
#endif

#ifdef DEBUG
  lmn_env.debug_por_dep          = FALSE;
  lmn_env.debug_id               = FALSE;
  lmn_env.debug_delta            = FALSE;
  lmn_env.debug_hash             = FALSE;
  lmn_env.debug_isomor           = FALSE;
  lmn_env.debug_mc               = FALSE;
  lmn_env.debug_por              = FALSE;
#endif

  lmn_env.findatom_parallel_mode = FALSE;
  lmn_env.find_atom_parallel     = FALSE;
  lmn_env.findatom_parallel_inde = FALSE;
}

