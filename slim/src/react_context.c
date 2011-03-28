/*
 * react_context.c
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

#include "react_context.h"
#include "task.h"
#include "slim_header/memstack.h"

inline void react_context_init(struct ReactCxt *rc, BYTE mode)
{
  rc->mode          = mode;
  rc->flag          = 0x00U;
  rc->global_root   = NULL;
  rc->v             = NULL;
  rc->work_vec_size = 0;
  rc->atomic_id     = -1;
  task_allocate_workspace(rc);
}

inline void react_context_destroy(struct ReactCxt *rc)
{
}

/*----------------------------------------------------------------------
 * Stand Alone React Context
 */

inline void stand_alone_react_cxt_init(struct ReactCxt *cxt)
{
  react_context_init(cxt, REACT_STAND_ALONE);
}

inline void stand_alone_react_cxt_destroy(struct ReactCxt *cxt)
{
}

/*----------------------------------------------------------------------
 * Property React Context
 */

inline void property_react_cxt_init(struct ReactCxt *cxt)
{
  react_context_init(cxt, REACT_PROPERTY);
}

inline void property_react_cxt_destroy(struct ReactCxt *cxt)
{
}

/*----------------------------------------------------------------------
 * Mem React Context
 */

inline void mem_react_cxt_init(struct ReactCxt *cxt)
{
  struct MemReactCxtData *v = LMN_MALLOC(struct MemReactCxtData);
  react_context_init(cxt, REACT_MEM_ORIENTED);
  cxt->v = v;
  RC_MEMSTACK(cxt) = lmn_memstack_make();
}

inline void mem_react_cxt_destroy(struct ReactCxt *cxt)
{
  lmn_memstack_free(RC_MEMSTACK(cxt));
  LMN_FREE(cxt->v);
}


/*----------------------------------------------------------------------
 * ND React Context
 */

inline static struct McReactCxtData *mc_react_data_make()
{
  struct McReactCxtData *v = LMN_MALLOC(struct McReactCxtData);
  v->succ_tbl       = st_init_ptrtable();
  v->roots          = vec_make(32);
  v->rules          = vec_make(32);
  v->props          = vec_make(8);
  v->mem_deltas     = NULL;
  v->mem_delta_tmp  = NULL;
  v->opt_mode       = 0x00U;
  v->org_succ_num   = 0;

  if (lmn_env.delta_mem) {
    v->mem_deltas = vec_make(32);
  }

  if (lmn_env.enable_por && !lmn_env.enable_por_old) {
    v->por = DPOR_DATA();
  }

  return v;
}


inline static void mc_react_data_free(struct McReactCxtData *v)
{
  st_free_table(v->succ_tbl);
  vec_free(v->roots);
  vec_free(v->rules);
  vec_free(v->props);
  if (v->mem_deltas) {
    vec_free(v->mem_deltas);
  }
  LMN_FREE(v);
}

inline void mc_react_cxt_init(struct ReactCxt *rc)
{
  struct McReactCxtData *v = mc_react_data_make();
  react_context_init(rc, REACT_ND);
  rc->v              = v;
  rc->work_vec_size  = 0;

  if (v->mem_deltas) {
    RC_MC_SET_DMEM(rc);
  }

  if (lmn_env.enable_por_old) {
    RC_MC_SET_DPOR_NAIVE(rc);
  } else if (lmn_env.enable_por) {
    RC_MC_SET_DPOR(rc);
  }
}


inline void mc_react_cxt_destroy(struct ReactCxt *cxt)
{
  mc_react_data_free(RC_ND_DATA(cxt));
}


inline void mc_react_cxt_add_expanded(struct ReactCxt *cxt,
                                      LmnMembrane *mem,
                                      LmnRule rule)
{
  vec_push(RC_EXPANDED(cxt), (vec_data_t)mem);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


void mc_react_cxt_add_mem_delta(struct ReactCxt *cxt,
                                struct MemDeltaRoot *d,
                                LmnRule rule)
{
  vec_push(RC_MEM_DELTAS(cxt), (vec_data_t)d);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


