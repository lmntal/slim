/*
 * dpor.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id:
 */

/**
 * Dynamic Partial Order Reduction using concurrency of declarative language
 */

#include "dpor.h"
#include "dpor_naive.h"
#include "task.h"

typedef struct McDporData {
  Vector *cur_ground_atoms;   /* 現在判定中の遷移で, ground命令によるProcessTblを集める */
  Vector *LHS_procs;
} McDporData;


static McDporData *dpor_data;

#define DPOR_DATA()       (dpor_data[lmn_thread_id])
#define DPOR_GR_ATOMS()   (DPOR_DATA().cur_ground_atoms)
#define DPOR_LHS_PROCS()  (DPOR_DATA().LHS_procs)

void dpor_data_init(McDporData *d)
{
  d->cur_ground_atoms = vec_make(4);
  d->LHS_procs = vec_make(16);
}


void dpor_data_destroy(McDporData *d)
{
  vec_free(d->cur_ground_atoms);
  vec_free(d->LHS_procs);
}


void dpor_env_init(void)
{
#ifdef DPOR_DEVEL
  unsigned int i, n;

  if (lmn_env.enable_por_old) {
    init_por_vars();
    return;
  }
  n = lmn_env.core_num;
  dpor_data = LMN_NALLOC(McDporData, n);
  for (i = 0; i < n; i++) {
    dpor_data_init(&dpor_data[i]);
  }
#else
  init_por_vars();
#endif
}


void dpor_env_destroy(void)
{
#ifdef DPOR_DEVEL
  unsigned int i, n;
  if (lmn_env.enable_por_old) {
    free_por_vars();
    return;
  }

  for (i = 0; i < n; i++) {
    dpor_data_destroy(&dpor_data[i]);
  }
  LMN_FREE(dpor_data);
#else
  free_por_vars();
#endif
}


void dpor_start(StateSpace      ss,
                State           *s,
                struct ReactCxt *rc,
                Vector          *new_s,
                BOOL            flag)
{
#ifdef DPOR_DEVEL
  if (RC_MC_USE_DPOR_NAIVE(rc)) {
    por_calc_ampleset(ss, s, rc, new_s, flag);
  } else {
    lmn_fatal("under construction..");
  }
#else
  por_calc_ampleset(ss, s, rc, new_s, flag);
#endif
}


/* 左辺でground命令が出現した場合, マッチングしたアトムの集合を保存 */
void dpor_add_ground_status(ProcessTbl atoms)
{
  vec_push(DPOR_GR_ATOMS(), (vec_data_t)atoms);
}


/* 左辺で出現したgroundにマッチングしたアトムの集合を捨てる */
void dpor_remove_ground_status(ProcessTbl atoms)
{
  Vector *g = DPOR_GR_ATOMS();
  if (vec_peek(g) == (vec_data_t)atoms) {
    vec_pop(g);
  } else {
    /* pushした順にpopされるので, ここに来ることはまずないが念のため書いておく */
    unsigned int i;
    for (i = 0; i < vec_num(g); i++) {
      vec_data_t t = vec_get(g, i);
      if (t == (vec_data_t)atoms) {
        vec_pop_n(g, i);
      }
    }
  }
}


/* 右辺で膜mからルールセットが消される場合  */
void dpor_add_clearrule_status(LmnMembrane *m)
{
  /* 膜mに依存, groot memのルールは消せない */
}


void dpor_LHS_procs_update(struct ReactCxt *rc,
                           LmnWord         *wt,
                           LmnByte         *at,
                           LmnByte         *tt)
{
  ProcessTbl procs;
  unsigned int i, n, trans_n;

  /* 現在のサクセッサ数を遷移番号として扱う
   * ((RC_EXPANDED--サクセサのVector-- にアクセスする際の添字に相当する) */

  trans_n  =  mc_react_cxt_expanded_num(rc);
  n        =  RC_WORK_VEC_SIZE(rc);
  procs    =  proc_tbl_make_with_size(round2up(n)); /* てきとう */

  for (i = 0; i < n; i++) {
    if (tt[i] == TT_ATOM) {
      if(LMN_INT_ATTR == at[i] || LMN_DBL_ATTR == at[i]) {
        /* データアトムは接続元のsymbol atomに埋め込まれている
         * delta-memにおいてリンクの繋ぎ替えが発生する場合は,
         * 接続元から差分を取る(元の構造を消して, 作る)ためskip */
         continue;
      } else if (LMN_HL_ATTR == at[i]) {
        lmn_fatal("under constructions: verification for hyper graph model");
      }
      else { /* symbol atom */
        LMN_ASSERT(proc_tbl_get(procs, LMN_SATOM_ID(wt[i]), NULL));
        proc_tbl_put_atom(procs, (LmnSAtom)wt[i], trans_n);
      }
    }
    else if (tt[i] == TT_MEM) {
      LMN_ASSERT(proc_tbl_get(procs, lmn_mem_id((LmnMembrane *)wt[i]), NULL));
      proc_tbl_put_mem(procs, (LmnMembrane *)wt[i], trans_n);
    }
  }

  vec_push(DPOR_LHS_PROCS(), (vec_data_t)procs);
}
