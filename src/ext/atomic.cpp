/*
 * atomic.cpp
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
#include "verifier/verifier.h"
#include "vm/vm.h"
#ifdef PROFILE
#include "verifier/runtime_status.h"
#endif
#include "vm/react_context.hpp"

namespace c14 = slim::element;

void atomic_ruleset(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                    LmnLinkAttr t0) {

  if (LMN_INT_ATTR == t0) {
    int i, n = mem->ruleset_num();
    AtomicType atomic_type;

    switch ((LmnWord)a0) {
    case 1:
      atomic_type = ATOMIC_ALL_EXHAUSTIVE;
      break;
    case 2:
      atomic_type = ATOMIC_SIMULATION;
      break;
    case 3:
      atomic_type = ATOMIC_SYNC_STEP;
      break;
    default:
      atomic_type = ATOMIC_NONE;
      break;
    }

    for (i = 0; i < n; i++) {
      lmn_mem_get_ruleset(mem, i)->atomic = atomic_type;
      lmn_mem_add_ruleset(mem->mem_parent(),
                          new LmnRuleSet(*lmn_mem_get_ruleset(mem, i)));
    }
    lmn_mem_delete_atom(mem, a0, t0);
  }

  if (rc->has_mode(REACT_MEM_ORIENTED)) {
    ((MemReactContext*)rc)->memstack_remove(mem);
  }
  mem->mem_parent()->remove_mem(mem);
}

void init_atomic(void) {
  CCallback::lmn_register_c_fun("atomic_ruleset", (void *)atomic_ruleset, 1);
}

/* this function applies rules in $rs to $mem  as much as possible  */
static inline BOOL react_ruleset_AMAP(LmnReactCxtRef rc, LmnMembraneRef mem,
                                      LmnRuleSetRef rs) {
  BOOL ret = FALSE;
  for (int i = 0; i < rs->size(); i++) {
    LmnRuleRef r = rs->get_rule(i);
#ifdef PROFILE
    if (!lmn_env.nd && lmn_env.profile_level >= 2) {
      profile_rule_obj_set(rs, r);
    }
#endif
    if (Task::react_rule(rc, mem, r)) {
      i = 0;
      ret = TRUE;
    }
  }
  return ret;
}

static inline LmnWorker *build_atomic_worker() {
  LmnWorker *w;
  StateSpaceRef sub_states;
  BYTE sub_flags = 0x00U;

  mc_set_compress(sub_flags);
  sub_states = new StateSpace(NULL, NULL);
  w = lmn_worker_make(sub_states, env_my_thread_id(), sub_flags);
  dfs_env_set(w);
  worker_init(w);
  worker_rc(w) = c14::make_unique<MCReactContext>(nullptr);


  /* TODO: ここで作ったLmnWorkerは所属groupがNULLのため,
   *       atomic stepで更なる並列実行はできない */

  return w;
}

static inline void abort_atomic_worker(LmnWorker *w) {
  // delete (worker_states(w));
  // delete (worker_rc(w));
  lmn_worker_free(w);
}

/* ルールセットat_setの各ルールを可能な限り適用する.
 * 非決定実行時には状態遷移のサブグラフを構築し,
 * 停止状態をmemの遷移先として生成し, rcに登録する. */
static BOOL react_ruleset_atomic_all(MCReactContext* rc, LmnMembraneRef mem,
                                     LmnRuleSetRef at_set) {
  unsigned int i, n, succ_i_from;
  at_set->validate_atomic();

  succ_i_from = mc_react_cxt_expanded_num(rc);
  for (auto r : *at_set)
    Task::react_rule(rc, mem, r);

  n = mc_react_cxt_expanded_num(rc) - succ_i_from;
  if (n > 0) {
    LmnWorker *w = build_atomic_worker();
    worker_rc(w)->start_atomic_step(at_set->id);
    for (i = 0; i < n; i++) {
      State *sub_s =
          new State((LmnMembraneRef)rc->expanded_pop(),
                    DEFAULT_STATE_ID, (worker_states(w))->use_memenc());
      if (sub_s != (worker_states(w))->insert(sub_s)) {
        delete (sub_s);
      } else {
        worker_states(w)->set_init_state(sub_s);
        dfs_start(w);
      }
    }

    /* サブルーチン側であるatomic workerの並列実行は想定していないので,
     * 以下の記述でもok */
    auto ends = (worker_states(w))->end_states;
    for (i = 0; i < ends.size(); i++) {
      for (int j = 0; j < ends[i].size(); j++) {
        LmnMembraneRef end_m =
            lmn_binstr_decode(((State *)ends[i][j])->state_binstr());
        mc_react_cxt_add_expanded((MCReactContext*)rc, end_m, new LmnRule());
      }
    }
    //    RC_FINISH_ATOMIC_STEP(&WORKER_RC(w)); /*
    //    どうせ破棄しちゃうから要らない */
    abort_atomic_worker(w);
  }

  at_set->invalidate_atomic();
  return succ_i_from != mc_react_cxt_expanded_num((MCReactContext*)rc);
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
static BOOL react_ruleset_atomic_sync(LmnReactCxtRef rc, LmnMembraneRef mem,
                                      LmnRuleSetRef at_set) {
  BOOL ret;

  if (!rc->has_mode(REACT_ND)) {
    for (auto r : *at_set) {
      if (Task::react_rule(rc, mem, r)) {
        return true;
      }
    }
    return false;
  }

  unsigned int r_i, succ_i_from;
  BYTE mode_org;
  LmnMembraneRef groot_m;
  MCReactContext* mcrc = ((MCReactContext*)rc);

  succ_i_from = mc_react_cxt_expanded_num(mcrc);

  /* 1step目: generate successor membrane */
  for (r_i = 0; r_i < at_set->size(); r_i++) {
    Task::react_rule(rc, mem, at_set->get_rule(r_i));
    if (mc_react_cxt_expanded_num(mcrc) > succ_i_from) {
      r_i++;
      break;
    }
  }

  /* ND モードでの Process ID のリセットをオーバーライド */
  env_set_next_id(rc->get_proc_next_id());

  if (r_i < at_set->size()) {
    at_set->validate_atomic();
    mode_org = rc->mode;
    groot_m = rc->get_global_root();
    rc->set_mode(REACT_STAND_ALONE);

    /* ignore all but the last reaction */
    LmnMembraneRef succ_m;

    LmnRuleRef succ_r = (LmnRuleRef) mcrc->pop_expanded_rule();
    if (mcrc->has_optmode(DeltaMembrane)) {
      succ_m = (LmnMembraneRef) mcrc->pop_mem_delta_root();
    } else {
      succ_m = (LmnMembraneRef) mcrc->pop_expanded_state();
    }
    while (succ_i_from < mc_react_cxt_expanded_num(mcrc)) {
      mcrc->expanded_pop();
    }
    mc_react_cxt_add_expanded(mcrc, succ_m, succ_r);

    /* 2〜r_n step目 */
    //      for (i = succ_i_from; i < mc_react_cxt_expanded_num(rc); i++) {

    // restore the current membrane of the last reaction
    LmnMembraneRef cur_mem = rc->get_cur_mem();

    mcrc->set_global_root(succ_m);
    react_ruleset_AMAP(rc, cur_mem, system_ruleset);
    for (int j = r_i; j < at_set->size(); j++) {
      if (Task::react_rule(rc, cur_mem, at_set->get_rule(j))) {
        react_ruleset_AMAP(rc, cur_mem, system_ruleset);
      }
    }
    //      }

    mcrc->set_global_root(groot_m);
    rc->set_mode(mode_org);
    at_set->invalidate_atomic();
  }

  env_set_next_id(rc->get_proc_org_id());
  ret = FALSE;

  return ret;
}

/* ルールセットat_setのルールを書換えが停止するまで繰返し適用する．
 * ルールセットの停止性と合流性を仮定している．非決定モードでも
 * 複数の書換え経路の探索は行わない．
 */
static inline BOOL react_ruleset_atomic_simulation(LmnReactCxtRef rc,
                                                   LmnMembraneRef mem,
                                                   LmnRuleSetRef at_set) {
  BOOL ret;

  if (!rc->has_mode(REACT_ND)) {
    ret = FALSE;
    while (react_ruleset_AMAP(rc, mem, at_set) ||
           react_ruleset_AMAP(rc, mem, system_ruleset)) {
      ret = TRUE;
    }
  } else {
    unsigned int i, j, succ_i_from;
    BYTE mode_org;
    LmnMembraneRef groot_m;
    MCReactContext* mcrc = (MCReactContext*) rc;

    succ_i_from = mc_react_cxt_expanded_num(mcrc);
    /* 1step目: generate successor membrane */
    for (auto r : *at_set) {
      Task::react_rule(rc, mem, r);
      if (mc_react_cxt_expanded_num(mcrc) > succ_i_from) {
        break;
      }
    }

    /* ND モードでの Process ID のリセットをオーバーライド */
    env_set_next_id(rc->get_proc_next_id());

    if (i < at_set->size()) {
      at_set->validate_atomic();
      mode_org = rc->mode;
      groot_m = rc->get_global_root();
      rc->set_mode(REACT_STAND_ALONE);

      /* ignore all but the last reaction */
      LmnMembraneRef succ_m;

      LmnRuleRef succ_r = (LmnRuleRef) mcrc->pop_expanded_rule();
      if (mcrc->has_optmode(DeltaMembrane)) {
        succ_m = (LmnMembraneRef) mcrc->pop_mem_delta_root();
      } else {
        succ_m = (LmnMembraneRef) mcrc->pop_expanded_state();
      }
      while (succ_i_from < mc_react_cxt_expanded_num(mcrc)) {
        mcrc->expanded_pop();
      }
      mc_react_cxt_add_expanded(mcrc, succ_m, succ_r);

      /* 2〜r_n step目 */
      //    for (i = succ_i_from; i < mc_react_cxt_expanded_num(rc); i++) {

      // restore the current membrane of the last reaction
      LmnMembraneRef cur_mem = rc->get_cur_mem();
      BOOL reacted;

      mcrc->set_global_root(succ_m);
      react_ruleset_AMAP(rc, cur_mem, system_ruleset);
      reacted = TRUE;
      while (reacted) {
        reacted = FALSE;
        for (j = 0; j < at_set->size(); j++) {
          reacted = reacted || Task::react_rule(rc, cur_mem, at_set->get_rule(j));
        }
        if (reacted) {
          react_ruleset_AMAP(rc, cur_mem, system_ruleset);
        }
      }
      //    }
      mcrc->set_global_root(groot_m);
      rc->set_mode(mode_org);
      at_set->invalidate_atomic();
    }

    env_set_next_id(rc->get_proc_org_id());
    ret = FALSE;
  }

  return ret;
}

BOOL react_ruleset_atomic(LmnReactCxtRef rc, LmnMembraneRef mem,
                          LmnRuleSetRef rs) {
  BOOL result = FALSE;

  if (rc->has_mode(REACT_ND) && ((MCReactContext*)rc)->has_optmode(DeltaMembrane)) {
    lmn_fatal("unsupported delta-membrane, atomic step");
  }

  rc->start_atomic_step(rs->id);
  switch (rs->atomic) {
  case ATOMIC_SYNC_STEP:
    result = react_ruleset_atomic_sync(rc, mem, rs);
    break;
  case ATOMIC_ALL_EXHAUSTIVE:
    if (rc->has_mode(REACT_ND)) {
      result = react_ruleset_atomic_all((MCReactContext*)rc, mem, rs);
    } /*
    else  FALLTHROUTH */
  case ATOMIC_SIMULATION:
    result = react_ruleset_atomic_simulation(rc, mem, rs);
    break;
  default:
    lmn_fatal("unexpected react testing");
    break;
  }
  rc->finish_atomic_step();
  return result;
}
