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
#include "hyperlink.h"
#include "verifier/verifier.h"
#include "memstack.h"

#ifdef USE_FIRSTCLASS_RULE
#include "firstclass_rule.h"
#endif

struct LmnRegister {
  LmnWord wt;
  LmnByte at;
  LmnByte tt;
};

struct LmnReactCxt {
  LmnMembraneRef global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
  LmnRegisterArray work_arry;   /* ルール適用レジスタ */
  unsigned int warry_cur;   /* work_arryの現在の使用サイズ */
  unsigned int warry_num;   /* work_arryの最大使用サイズ(SPEC命令指定) */
  unsigned int warry_cap;   /* work_arryのキャパシティ */
  unsigned int trace_num;   /* ルール適用回数 (通常実行用トレース実行で使用)  */
  LmnRulesetId atomic_id;   /* atomic step中: atomic set id(signed int), default:-1 */
  ProcessID proc_org_id;    /* atomic step終了時に Process ID をこの値に復帰 */
  ProcessID proc_next_id;   /* atomic step継続時に Process ID をこの値に設定 */
  LmnMembraneRef cur_mem;     /* atomic step継続時に現在膜をこの値に設定 */
  BYTE mode;
  BOOL flag;                /* mode以外に指定するフラグ */
  void *v;                  /* 各mode毎に固有の持ち物 */
  SimpleHashtbl *hl_sameproccxt; /* findatom 時のアトム番号と、同名型付きプロセス文脈を持つアトム引数との対応関係を保持 */
#ifdef USE_FIRSTCLASS_RULE
  Vector *insersion_events;
#endif
};

LmnWord lmn_register_wt(LmnRegisterRef r) {
  return r->wt;
}
LmnByte lmn_register_at(LmnRegisterRef r) {
  return r->at;
}
LmnByte lmn_register_tt(LmnRegisterRef r) {
  return r->tt;
}
void lmn_register_set_wt(LmnRegisterRef r, LmnWord wt) {
  r->wt = wt;
}
void lmn_register_set_at(LmnRegisterRef r, LmnByte at) {
  r->at = at;
}
void lmn_register_set_tt(LmnRegisterRef r, LmnByte tt) {
  r->tt = tt;
}

LmnRegisterRef lmn_register_array_get(LmnRegisterArray array, int idx) {
  return (LmnRegisterRef)array + idx;
}

LmnRegisterArray lmn_register_make(unsigned int size)
{
  LmnRegisterArray v = (LmnRegisterArray)LMN_NALLOC(struct LmnRegister, round2up(size));
  memset(v, 0, sizeof(struct LmnRegister) * size);
  return v;
}

void lmn_register_copy(LmnRegisterArray to, LmnRegisterArray from, unsigned int size)
{
  memcpy(to, from, sizeof(struct LmnRegister) * size);
}

void lmn_register_free(LmnRegisterArray v)
{
  LMN_FREE(v);
}

void lmn_register_extend(LmnReactCxtRef rc, unsigned int new_size)
{
  new_size = round2up(new_size);
  rc->work_arry = (LmnRegisterArray)LMN_REALLOC(struct LmnRegister, rc->work_arry, new_size);
  memset((LmnRegisterRef)rc->work_arry + warry_size(rc),
         0,
         sizeof(struct LmnRegister) * (new_size - warry_size(rc)));
  warry_size_set(rc, new_size);
}


BYTE RC_MODE(LmnReactCxtRef cxt) {
  return cxt->mode;
}

void RC_SET_MODE(LmnReactCxtRef cxt, BYTE mode) {
  cxt->mode = mode;
}

void RC_ADD_MODE(LmnReactCxtRef cxt, BYTE mode) {
  cxt->mode |= mode;
}

BOOL RC_GET_MODE(LmnReactCxtRef cxt, BYTE mode) {
  return (cxt->mode & mode) == mode;
}

unsigned int warry_size(LmnReactCxtRef cxt) {
  return cxt->warry_cap;
}

void warry_size_set(LmnReactCxtRef cxt, unsigned int n) {
  cxt->warry_cap = n;
}

unsigned int warry_use_size(LmnReactCxtRef cxt) {
  return cxt->warry_num;
}
void warry_use_size_set(LmnReactCxtRef cxt, unsigned int n) {
  cxt->warry_num = n;
}

unsigned int warry_cur_size(LmnReactCxtRef cxt) {
  return cxt->warry_cur;
}
void warry_cur_size_set(LmnReactCxtRef cxt, unsigned int n) {
  cxt->warry_cur = n;
}

void warry_cur_update(LmnReactCxtRef cxt, unsigned int i) {
  if (warry_cur_size(cxt) <= i) {
    warry_cur_size_set(cxt, i + 1);
  }
}

LmnRegisterArray rc_warry(LmnReactCxtRef cxt) {
  return cxt->work_arry;
}

void rc_warry_set(LmnReactCxtRef cxt, LmnRegisterArray arry) {
  cxt->work_arry = arry;
}

LmnWord wt(LmnReactCxtRef cxt, unsigned int i) {
  return lmn_register_wt(lmn_register_array_get(cxt->work_arry, i));
}

void wt_set(LmnReactCxtRef cxt, unsigned int i, LmnWord o) {
  LmnRegisterRef r__ = lmn_register_array_get(cxt->work_arry, i);
  lmn_register_set_wt(r__, o);
  warry_cur_update(cxt, i);
}

LmnByte at(LmnReactCxtRef cxt, unsigned int i) {
  return lmn_register_at(lmn_register_array_get(cxt->work_arry, i));
}

void at_set(LmnReactCxtRef cxt, unsigned int i, LmnByte o) {
  LmnRegisterRef r__ = lmn_register_array_get(cxt->work_arry, i);
  lmn_register_set_at(r__, o);
  warry_cur_update(cxt, i);
}

LmnByte tt(LmnReactCxtRef cxt, unsigned int i) {
  return lmn_register_tt(lmn_register_array_get(cxt->work_arry, i));
}

void tt_set(LmnReactCxtRef cxt, unsigned int i, LmnByte o) {
  LmnRegisterRef r__ = lmn_register_array_get(cxt->work_arry, i);
  lmn_register_set_tt(r__, o);
  warry_cur_update(cxt, i);
}

void warry_set(LmnReactCxtRef cxt, unsigned int i, LmnWord w, LmnByte a, LmnByte t) {
    LmnRegisterRef r__ = lmn_register_array_get(cxt->work_arry, i);
    lmn_register_set_wt(r__, w);
    lmn_register_set_at(r__, a);
    lmn_register_set_tt(r__, t);
    warry_cur_update(cxt, i);                                                   
}

unsigned int RC_TRACE_NUM(LmnReactCxtRef cxt) {
  return cxt->trace_num;
}
unsigned int RC_TRACE_NUM_INC(LmnReactCxtRef cxt) {
  return cxt->trace_num++;
}

LmnMembraneRef RC_GROOT_MEM(LmnReactCxtRef cxt) {
  return cxt->global_root;
}

void RC_SET_GROOT_MEM(LmnReactCxtRef cxt, LmnMembraneRef mem) {
  cxt->global_root = mem;
}

void RC_START_ATOMIC_STEP(LmnReactCxtRef cxt, LmnRulesetId id) {
  cxt->atomic_id = id;
}

BOOL RC_IS_ATOMIC_STEP(LmnReactCxtRef cxt) {
  return cxt->atomic_id >= 0;
}

void RC_FINISH_ATOMIC_STEP(LmnReactCxtRef cxt) {
  cxt->atomic_id = -1;
}

ProcessID RC_PROC_ORG_ID(LmnReactCxtRef cxt) {
  return cxt->proc_org_id;
}

void RC_SET_PROC_ORG_ID(LmnReactCxtRef cxt, ProcessID id) {
  cxt->proc_org_id = id;
}

ProcessID RC_PROC_NEXT_ID(LmnReactCxtRef cxt) {
  return cxt->proc_next_id;
}

void RC_SET_PROC_NEXT_ID(LmnReactCxtRef cxt, ProcessID id) {
  cxt->proc_next_id = id;
}

LmnMembraneRef RC_CUR_MEM(LmnReactCxtRef cxt) {
  return cxt->cur_mem;
}

void RC_SET_CUR_MEM(LmnReactCxtRef cxt, LmnMembraneRef mem) {
  cxt->cur_mem = mem;
}

SimpleHashtbl *RC_HLINK_SPC(LmnReactCxtRef cxt) {
  return cxt->hl_sameproccxt;
}

void RC_SET_HLINK_SPC(LmnReactCxtRef cxt, SimpleHashtbl *spc) {
  cxt->hl_sameproccxt = spc;
}

BOOL rc_hlink_opt(LmnInstrVar atomi, LmnReactCxtRef rc) {
  /*  return hl_sameproccxtが初期化済み && atomiは同名プロセス文脈を持つアトム */
  return RC_HLINK_SPC(rc) &&
         hashtbl_contains(RC_HLINK_SPC(rc), (HashKeyType)atomi);
}

struct McReactCxtData *RC_ND_DATA(LmnReactCxtRef cxt) {
  return cxt->v;
}

LmnReactCxtRef react_context_alloc() {
  return (LmnReactCxtRef)LMN_MALLOC(struct LmnReactCxt);
}
void react_context_dealloc(LmnReactCxtRef cxt) {
  LMN_FREE(cxt);
}

void react_context_init(LmnReactCxtRef rc, BYTE mode)
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
#ifdef USE_FIRSTCLASS_RULE
  rc->insersion_events = vec_make(4);
#endif
}

void react_context_copy(LmnReactCxtRef to, LmnReactCxtRef from)
{
  to->mode          = from->mode;
  to->flag          = from->flag;
  to->global_root   = from->global_root;
  to->v             = from->v;
  to->warry_cur     = from->warry_cur;
  to->warry_num     = from->warry_num;
  to->warry_cap     = from->warry_cap;
  to->atomic_id     = from->atomic_id;
#ifdef USE_FIRSTCLASS_RULE
  vec_free(to->insersion_events);
  to->insersion_events = vec_copy(from->insersion_events);
#endif
}

void react_context_destroy(LmnReactCxtRef rc)
{
  if (RC_HLINK_SPC(rc)) {
    lmn_sameproccxt_clear(rc);
  }
  if (rc->work_arry) {
    lmn_register_free(rc->work_arry);
  }
#ifdef USE_FIRSTCLASS_RULE
  if (rc->insersion_events) {
    vec_free(rc->insersion_events);
  }
#endif
}

/*----------------------------------------------------------------------
 * Stand Alone React Context
 */

void stand_alone_react_cxt_init(LmnReactCxtRef cxt)
{
  react_context_init(cxt, REACT_STAND_ALONE);
}

void stand_alone_react_cxt_destroy(LmnReactCxtRef cxt)
{
  react_context_destroy(cxt);
}

/*----------------------------------------------------------------------
 * Property React Context
 */

void property_react_cxt_init(LmnReactCxtRef cxt)
{
  react_context_init(cxt, REACT_PROPERTY);
}

void property_react_cxt_destroy(LmnReactCxtRef cxt)
{
  react_context_destroy(cxt);
}


/*----------------------------------------------------------------------
 * Mem React Context
 */

LmnMemStack RC_MEMSTACK(LmnReactCxtRef cxt) {
  return ((struct MemReactCxtData *)cxt->v)->memstack;
}

void RC_MEMSTACK_SET(LmnReactCxtRef cxt, LmnMemStack s) {
  ((struct MemReactCxtData *)cxt->v)->memstack = s;
}

void mem_react_cxt_init(LmnReactCxtRef cxt)
{
  struct MemReactCxtData *v = LMN_MALLOC(struct MemReactCxtData);
  react_context_init(cxt, REACT_MEM_ORIENTED);
  cxt->v = v;
  RC_MEMSTACK_SET(cxt, lmn_memstack_make());
}

void mem_react_cxt_destroy(LmnReactCxtRef cxt)
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

void mc_react_cxt_init(LmnReactCxtRef rc)
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


void mc_react_cxt_destroy(LmnReactCxtRef cxt)
{
  mc_react_data_free(RC_ND_DATA(cxt));
  react_context_destroy(cxt);
}


void mc_react_cxt_add_expanded(LmnReactCxtRef cxt,
                                      LmnMembraneRef mem,
                                      LmnRuleRef rule)
{
  vec_push(RC_EXPANDED(cxt), (vec_data_t)mem);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


void mc_react_cxt_add_mem_delta(LmnReactCxtRef cxt,
                                struct MemDeltaRoot *d,
                                LmnRuleRef rule)
{
  vec_push(RC_MEM_DELTAS(cxt), (vec_data_t)d);
  vec_push(RC_EXPANDED_RULES(cxt), (vec_data_t)rule);
}


LmnWord mc_react_cxt_expanded_pop(LmnReactCxtRef cxt) {
  vec_pop(RC_EXPANDED_RULES(cxt));
  if (RC_MC_USE_DMEM(cxt)) {
    return vec_pop(RC_MEM_DELTAS(cxt));
  } else {
    return vec_pop(RC_EXPANDED(cxt));
  }
}

LmnWord mc_react_cxt_expanded_get(LmnReactCxtRef cxt, unsigned int i) {
  if (RC_MC_USE_DMEM(cxt)) {
    return vec_get(RC_MEM_DELTAS(cxt), i);
  } else {
    return vec_get(RC_EXPANDED(cxt), i);
  }
}

unsigned int mc_react_cxt_succ_num_org(LmnReactCxtRef cxt) {
  return RC_ND_ORG_SUCC_NUM(cxt);
}

unsigned int mc_react_cxt_expanded_num(LmnReactCxtRef cxt) {
  return RC_MC_USE_DMEM(cxt) ? vec_num(RC_MEM_DELTAS(cxt))
                             : vec_num(RC_EXPANDED(cxt));
}

///// first-class rulesets

BOOL lmn_rc_has_insersion(LmnReactCxtRef rc) {
  printf("%s(%d): stub\n", __func__, __LINE__);
  return FALSE;
}

void lmn_rc_push_insersion(LmnReactCxtRef rc, LmnSymbolAtomRef satom, LmnMembraneRef mem) {
  printf("%s(%d): stub\n", __func__, __LINE__);
}
void lmn_rc_pop_insersion(LmnMembraneRef rc, LmnSymbolAtomRef *satom, LmnMembraneRef *mem) {
  printf("%s(%d): stub\n", __func__, __LINE__);
  *satom = NULL;
  *mem = NULL;
}

void lmn_rc_execute_insersion_events(LmnReactCxtRef rc) {
  printf("%s(%d): stub\n", __func__, __LINE__);
}
