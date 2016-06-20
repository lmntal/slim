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
#include "util.h"
#include "hyperlink.h"
#include "dpor.h"
#include "slim_header/memstack.h"


LmnRegister *lmn_register_make(unsigned int size)
{
  LmnRegister *v = LMN_NALLOC(struct LmnRegister, round2up(size));
  memset(v, 0, sizeof(struct LmnRegister) * size);
  return v;
}

void lmn_register_copy(LmnRegister *to, LmnRegister *from, unsigned int size)
{
  memcpy(to, from, sizeof(struct LmnRegister) * size);
}

void lmn_register_free(LmnRegister *v)
{
  LMN_FREE(v);
}

void lmn_register_extend(LmnReactCxt *rc, unsigned int new_size)
{
  new_size = round2up(new_size);
  rc->work_arry = LMN_REALLOC(struct LmnRegister, rc->work_arry, new_size);
  memset(rc->work_arry + warry_size(rc),
         0,
         sizeof(struct LmnRegister) * (new_size - warry_size(rc)));
  warry_size_set(rc, new_size);
}

void react_context_init(LmnReactCxt *rc, BYTE mode)
{
  rc->mode          = mode;
  rc->flag          = 0x00U;
  rc->global_root   = NULL;
  rc->v             = NULL;
  rc->work_arry     = lmn_register_make(WARRY_DEF_SIZE);
  rc->warry_cur     = 0;
  rc->warry_num     = 0;
  rc->warry_cap     = WARRY_DEF_SIZE;
  rc->atomic_id     = -1;
  rc->hl_sameproccxt = NULL;
}

void react_context_copy(LmnReactCxt *to, LmnReactCxt *from)
{
  to->mode          = from->mode;
  to->flag          = from->flag;
  to->global_root   = from->global_root;
  to->v             = from->v;
  to->warry_cur     = from->warry_cur;
  to->warry_num     = from->warry_num;
  to->warry_cap     = from->warry_cap;
  to->atomic_id     = from->atomic_id;
}

void react_context_destroy(LmnReactCxt *rc)
{
  if (RC_HLINK_SPC(rc)) {
    lmn_sameproccxt_clear(rc);
  }
  if (rc->work_arry) {
    lmn_register_free(rc->work_arry);
  }
}

/*----------------------------------------------------------------------
 * Stand Alone React Context
 */

void stand_alone_react_cxt_init(LmnReactCxt *cxt)
{
  react_context_init(cxt, REACT_STAND_ALONE);
}

void stand_alone_react_cxt_destroy(LmnReactCxt *cxt)
{
  react_context_destroy(cxt);
}

/*----------------------------------------------------------------------
 * Property React Context
 */

void property_react_cxt_init(LmnReactCxt *cxt)
{
  react_context_init(cxt, REACT_PROPERTY);
}

void property_react_cxt_destroy(LmnReactCxt *cxt)
{
  react_context_destroy(cxt);
}


/*----------------------------------------------------------------------
 * Mem React Context
 */

void mem_react_cxt_init(LmnReactCxt *cxt)
{
  struct MemReactCxtData *v = LMN_MALLOC(struct MemReactCxtData);
  react_context_init(cxt, REACT_MEM_ORIENTED);
  cxt->v = v;
  RC_MEMSTACK(cxt) = lmn_memstack_make();
}

void mem_react_cxt_destroy(LmnReactCxt *cxt)
{
  lmn_memstack_free(RC_MEMSTACK(cxt));
  LMN_FREE(cxt->v);
  react_context_destroy(cxt);
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
  v->d_cur          = 0;

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

void mc_react_cxt_init(LmnReactCxt *rc)
{
  struct McReactCxtData *v = mc_react_data_make();
  react_context_init(rc, REACT_ND);
  rc->v = v;

  if (v->mem_deltas) {
    RC_MC_SET_DMEM(rc);
  }

  if (lmn_env.enable_por_old) {
    RC_MC_SET_DPOR_NAIVE(rc);
  } else if (lmn_env.enable_por) {
    RC_MC_SET_DPOR(rc);
  }

  if (lmn_env.d_compress) {
    RC_MC_SET_D(rc);
  }
}


void mc_react_cxt_destroy(LmnReactCxt *cxt)
{
  mc_react_data_free(RC_ND_DATA(cxt));
  react_context_destroy(cxt);
}


void mc_react_cxt_add_expanded(LmnReactCxt *cxt,
                                      LmnMembrane *mem,
                                      LmnRuleRef rule)
{
  vec_push(RC_EXPANDED(cxt), (vec_data_t)mem);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


void mc_react_cxt_add_mem_delta(LmnReactCxt *cxt,
                                struct MemDeltaRoot *d,
                                LmnRuleRef rule)
{
  vec_push(RC_MEM_DELTAS(cxt), (vec_data_t)d);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


