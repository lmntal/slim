/*
 * atomic.c
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

#include "../lmntal.h"
#include "../lmntal_ext.h"
#include "../react_context.h"
#include "../slim_header/memstack.h"
#include "../rule.h"
#include "../task.h"
#include "mc_worker.h"
#include "mc_generator.h"
#ifdef PROFILE
# include "runtime_status.h"
#endif

void atomic_ruleset(LmnReactCxt *rc, LmnMembraneRef mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  if (LMN_INT_ATTR == t0) {
    int i, n = lmn_mem_ruleset_num(mem);
    AtomicType atomic_type;

    switch ((int)a0) {
    case  1:
      atomic_type = ATOMIC_ALL_EXHAUSTIVE;
      break;
    case  2:
      atomic_type = ATOMIC_SIMULATION;
      break;
    case  3:
      atomic_type = ATOMIC_SYNC_STEP;
      break;
    default :
      atomic_type = ATOMIC_NONE;
      break;
    }

    for (i = 0; i < n; i++) {
      lmn_ruleset_set_atomic(lmn_mem_get_ruleset(mem, i), atomic_type);
      lmn_mem_add_ruleset(lmn_mem_parent(mem),
                          lmn_ruleset_copy(lmn_mem_get_ruleset(mem, i)));
    }
    lmn_mem_delete_atom(mem, a0, t0);
  }

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    lmn_memstack_delete(RC_MEMSTACK(rc), mem);
  }
  lmn_mem_delete_mem(lmn_mem_parent(mem), mem);
}


void init_atomic(void)
{
  lmn_register_c_fun("atomic_ruleset", (void *)atomic_ruleset, 1);
}



/* this function applies rules in $rs to $mem  as much as possible  */
static inline BOOL react_ruleset_AMAP(LmnReactCxt *rc,
                                      LmnMembraneRef mem,
                                      LmnRuleSetRef rs)
{
  unsigned int i;
  BOOL ret = FALSE;
  for (i = 0; i < lmn_ruleset_rule_num(rs); i++) {
    LmnRuleRef r = lmn_ruleset_get_rule(rs, i);
#ifdef PROFILE
    if (!lmn_env.nd && lmn_env.profile_level >= 2) {
      profile_rule_obj_set(rs, r);
    }
#endif
    if (react_rule(rc, mem, r)) {
      i = 0;
      ret = TRUE;
    }
  }
  return ret;
}


static inline LmnWorker *build_atomic_worker()
{
  LmnWorker *w;
  StateSpaceRef sub_states;
  BYTE sub_flags = 0x00U;

  mc_set_compress(sub_flags);
  sub_states = statespace_make(NULL, NULL);
  w = lmn_worker_make(sub_states, env_my_thread_id(), sub_flags);
  dfs_env_set(w);
  worker_init(w);
  mc_react_cxt_init(&worker_rc(w));

  /* TODO: ここで作ったLmnWorkerは所属groupがNULLのため,
   *       atomic stepで更なる並列実行はできない */

  return w;
}

static inline void abort_atomic_worker(LmnWorker *w)
{
  statespace_free(worker_states(w));
  mc_react_cxt_destroy(&worker_rc(w));
  lmn_worker_free(w);
}

/* ルールセットat_setの各ルールを可能な限り適用する.
 * 非決定実行時には状態遷移のサブグラフを構築し, 停止状態をmemの遷移先として生成し, rcに登録する. */
static BOOL react_ruleset_atomic_all(LmnReactCxt *rc,
                                     LmnMembraneRef mem,
                                     LmnRuleSetRef  at_set)
{
  unsigned int i, n, succ_i_from;
  lmn_ruleset_validate_atomic(at_set);

  succ_i_from  = mc_react_cxt_expanded_num(rc);
  for (i = 0; i < lmn_ruleset_rule_num(at_set); i++) {
    react_rule(rc, mem, lmn_ruleset_get_rule(at_set, i));
  }

  n = mc_react_cxt_expanded_num(rc) - succ_i_from;
  if (n > 0) {
    const Vector *ends;
    LmnWorker *w = build_atomic_worker();
    RC_START_ATOMIC_STEP(&worker_rc(w), lmn_ruleset_get_id(at_set));
    for (i = 0; i < n; i++) {
      State *sub_s = state_make((LmnMembraneRef)mc_react_cxt_expanded_pop(rc),
                                DEFAULT_STATE_ID,
                                statespace_use_memenc(worker_states(w)));
      if (sub_s != statespace_insert(worker_states(w), sub_s)) {
        state_free(sub_s);
      } else {
        worker_states(w)->init_state = sub_s;
        dfs_start(w);
      }
    }

    /* サブルーチン側であるatomic workerの並列実行は想定していないので, 以下の記述でもok */
    ends = statespace_end_states(worker_states(w));
    for (i = 0; i < vec_num(ends); i++) {
      LmnMembraneRef end_m = lmn_binstr_decode(state_binstr((State *)vec_get(ends, i)));
      mc_react_cxt_add_expanded(rc, end_m, dummy_rule());
    }
//    RC_FINISH_ATOMIC_STEP(&WORKER_RC(w)); /* どうせ破棄しちゃうから要らない */
    abort_atomic_worker(w);
  }

  lmn_ruleset_invalidate_atomic(at_set);
  return succ_i_from != mc_react_cxt_expanded_num(rc);
}


/* ルールセットat_setの各ルールを昇順にちょうど1stepずつ展開する.
 * 適用のなかったルールはskip.
 * ex)
 * ---------
 *  a(0).
 *  { '$callback'(atomic_ruleset, 3).
 *    ruleA @@ a(X) :- X>0, Y=X+1 | a(Y).
 *    ruleB @@ a(X) :- a(X), b.
 *    ruleC @@ a(5) :- ok.
 *  }
 * ---------
 * result: ok, b, b, b, b, b.
 */
static BOOL react_ruleset_atomic_sync(LmnReactCxt *rc,
                                      LmnMembraneRef mem,
                                      LmnRuleSetRef  at_set)
{
  BOOL ret;

  if (!RC_GET_MODE(rc, REACT_ND)) {
    unsigned int i;
    ret = FALSE;
    for (i = 0; i < lmn_ruleset_rule_num(at_set); i++) {
      if (react_rule(rc, mem, lmn_ruleset_get_rule(at_set, i))) {
        ret = TRUE;
      }
    }
  }
  else {
    unsigned int j, r_i, succ_i_from;
    BYTE mode_org;
    LmnMembraneRef groot_m;

    //    lmn_ruleset_validate_atomic(at_set);
    succ_i_from = mc_react_cxt_expanded_num(rc);

    /* 1step目: generate successor membrane */
    for (r_i = 0; r_i < lmn_ruleset_rule_num(at_set); r_i++) {
      react_rule(rc, mem, lmn_ruleset_get_rule(at_set, r_i));
      if (mc_react_cxt_expanded_num(rc) > succ_i_from) {
        r_i++;
        break;
      }
    }

    /* ND モードでの Process ID のリセットをオーバーライド */
    env_set_next_id(RC_PROC_NEXT_ID(rc));

    if (r_i < lmn_ruleset_rule_num(at_set)) {
      lmn_ruleset_validate_atomic(at_set);
      mode_org = RC_MODE(rc);
      groot_m = RC_GROOT_MEM(rc);
      RC_SET_MODE(rc, REACT_STAND_ALONE);

      /* ignore all but the last reaction */
      LmnMembraneRef succ_m;

      LmnRuleRef succ_r = (LmnRuleRef)vec_pop(RC_EXPANDED_RULES(rc));
      if (RC_MC_USE_DMEM(rc)) {
        succ_m = (LmnMembraneRef)vec_pop(RC_MEM_DELTAS(rc));
      } else {
        succ_m = (LmnMembraneRef)vec_pop(RC_EXPANDED(rc));
      }
      while (succ_i_from < mc_react_cxt_expanded_num(rc)) {
        mc_react_cxt_expanded_pop(rc);
      }
      mc_react_cxt_add_expanded(rc, succ_m, succ_r);

      /* 2〜r_n step目 */
      //      for (i = succ_i_from; i < mc_react_cxt_expanded_num(rc); i++) {

        // restore the current membrane of the last reaction
        LmnMembraneRef cur_mem = RC_CUR_MEM(rc);  

        RC_SET_GROOT_MEM(rc, succ_m);
        react_ruleset_AMAP(rc, cur_mem, system_ruleset);
        for (j = r_i; j < lmn_ruleset_rule_num(at_set); j++) {
          if (react_rule(rc, cur_mem, lmn_ruleset_get_rule(at_set, j))) {
            react_ruleset_AMAP(rc, cur_mem, system_ruleset);
          }
        }
	//      }

      RC_SET_GROOT_MEM(rc, groot_m);
      RC_SET_MODE(rc, mode_org);
      lmn_ruleset_invalidate_atomic(at_set);
    }

    env_set_next_id(RC_PROC_ORG_ID(rc));
    ret = FALSE;
  }

  return ret;
}


/* ルールセットat_setのルールを書換えが停止するまで繰返し適用する．
 * ルールセットの停止性と合流性を仮定している．非決定モードでも
 * 複数の書換え経路の探索は行わない．
 */
static inline BOOL react_ruleset_atomic_simulation(LmnReactCxt *rc,
                                                   LmnMembraneRef mem,
                                                   LmnRuleSetRef  at_set)
{
  BOOL ret;

  if (!RC_GET_MODE(rc, REACT_ND)) {
    ret = FALSE;
    while (react_ruleset_AMAP(rc, mem, at_set) ||
           react_ruleset_AMAP(rc, mem, system_ruleset)) {
       ret = TRUE;
    }
  }
  else {
    unsigned int i, j, succ_i_from;
    BYTE mode_org;
    LmnMembraneRef groot_m;

    succ_i_from = mc_react_cxt_expanded_num(rc);
    /* 1step目: generate successor membrane */
    for (i = 0; i < lmn_ruleset_rule_num(at_set); i++) {
      react_rule(rc, mem, lmn_ruleset_get_rule(at_set, i));
      if (mc_react_cxt_expanded_num(rc) > succ_i_from) {
        break;
      }
    }

    /* ND モードでの Process ID のリセットをオーバーライド */
    env_set_next_id(RC_PROC_NEXT_ID(rc));

    if (i < lmn_ruleset_rule_num(at_set)) {
      lmn_ruleset_validate_atomic(at_set);
      mode_org = RC_MODE(rc);
      groot_m = RC_GROOT_MEM(rc);
      RC_SET_MODE(rc, REACT_STAND_ALONE);

      /* ignore all but the last reaction */
      LmnMembraneRef succ_m;

      LmnRuleRef succ_r = (LmnRuleRef)vec_pop(RC_EXPANDED_RULES(rc));
      if (RC_MC_USE_DMEM(rc)) {
        succ_m = (LmnMembraneRef)vec_pop(RC_MEM_DELTAS(rc));
      } else {
        succ_m = (LmnMembraneRef)vec_pop(RC_EXPANDED(rc));
      }
      while (succ_i_from < mc_react_cxt_expanded_num(rc)) {
        mc_react_cxt_expanded_pop(rc);
      }
      mc_react_cxt_add_expanded(rc, succ_m, succ_r);

      /* 2〜r_n step目 */
      //    for (i = succ_i_from; i < mc_react_cxt_expanded_num(rc); i++) {

        // restore the current membrane of the last reaction
        LmnMembraneRef cur_mem = RC_CUR_MEM(rc);
        BOOL reacted;

        RC_SET_GROOT_MEM(rc, succ_m);
        react_ruleset_AMAP(rc, cur_mem, system_ruleset);
        reacted = TRUE;
        while (reacted) {
          reacted = FALSE;
          for (j = 0; j < lmn_ruleset_rule_num(at_set); j++) {
            reacted = reacted ||
                      react_rule(rc, cur_mem, lmn_ruleset_get_rule(at_set, j));
          }
          if (reacted) {
            react_ruleset_AMAP(rc, cur_mem, system_ruleset);
          }
        }
        //    }
      RC_SET_GROOT_MEM(rc, groot_m);
      RC_SET_MODE(rc, mode_org);
      lmn_ruleset_invalidate_atomic(at_set);
    }

    env_set_next_id(RC_PROC_ORG_ID(rc));
    ret = FALSE;
  }

  return ret;
}


BOOL react_ruleset_atomic(LmnReactCxt *rc,
                          LmnMembraneRef mem,
                          LmnRuleSetRef  rs)
{
  BOOL result = FALSE;

  if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DMEM(rc)) {
    lmn_fatal("unsupported delta-membrane, atomic step");
  }

  RC_START_ATOMIC_STEP(rc, lmn_ruleset_get_id(rs));
  switch (lmn_ruleset_atomic_type(rs)) {
  case ATOMIC_SYNC_STEP:
    result = react_ruleset_atomic_sync(rc, mem, rs);
    break;
  case ATOMIC_ALL_EXHAUSTIVE:
    if (RC_GET_MODE(rc, REACT_ND)) {
      result = react_ruleset_atomic_all(rc, mem, rs);
    } /*
    else  FALLTHROUTH */
  case ATOMIC_SIMULATION:
    result = react_ruleset_atomic_simulation(rc, mem, rs);
    break;
  default:
    lmn_fatal("unexpected react testing");
    break;
  }
  RC_FINISH_ATOMIC_STEP(rc);
  return result;
}


