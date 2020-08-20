/*
 * mc.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without4
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
#include "mc.h"
#include "binstr_compress.h"
#include "delta_membrane.h"
#include "dpor.h"
#include "ltl2ba_adapter.h"
#include "mc_worker.h"
#include "mhash.h"
#include "propositional_symbol.h"
#include "runtime_status.h"
#ifdef DEBUG
#include "vm/dumper.h"
#endif
#include "state.h"
#include "state.hpp"
#include "state_dumper.h"

/** =======================================
 *  ==== Entrance for model checking ======
 *  =======================================
 */

static inline void do_mc(LmnMembraneRef world_mem, AutomataRef a, Vector *psyms,
                         int thread_num);
static void mc_dump(LmnWorkerGroup *wp);

/* 非決定実行を行う. run_mcもMT-unsafeなので子ルーチンとしては使えない */
void run_mc(Vector *start_rulesets, AutomataRef a, Vector *psyms) {
  static LmnMembraneRef mem;

  if (lmn_env.nd_cleaning) {
    /* nd_cleaning状態の場合は、グローバルルート膜の解放を行う */
    mem->free_rec();
    lmn_env.nd_cleaning = FALSE;
  }

  if (!lmn_env.nd_remain && !lmn_env.nd_remaining) {
    mem = new LmnMembrane();
  }

  react_start_rulesets(mem, start_rulesets);
  mem->activate_ancestors();

  do_mc(mem, a, psyms, lmn_env.core_num);

  if (lmn_env.nd_remain) {
    lmn_env.nd_remaining = TRUE;
  } else {
    lmn_env.nd_remaining = FALSE;
    mem->drop();
    delete mem;
  }
}

static inline void do_mc(LmnMembraneRef world_mem_org, AutomataRef a,
                         Vector *psyms, int thread_num) {
  LmnWorkerGroup *wp;
  StateSpaceRef states;
  LmnMembraneRef mem;
  State *init_s;
  BYTE p_label;

  /** INITIALIZE
   */
  mhash_set_depth(lmn_env.hash_depth);
  if (lmn_env.tree_compress) {
    lmn_bscomp_tree_init();
  }
  wp = new LmnWorkerGroup(a, psyms, thread_num);
  states = worker_states(wp->get_worker(LMN_PRIMARY_ID));
  p_label = a ? a->get_init_state() : DEFAULT_STATE_ID;
  mem = world_mem_org->copy();
  init_s = new State(mem, p_label, states->use_memenc());
  state_id_issue(init_s); /* 状態に整数IDを発行 */
#ifdef KWBT_OPT
  if (lmn_env.opt_mode != OPT_NONE)
    state_set_cost(init_s, 0U, NULL); /* 初期状態のコストは0 */
#endif
  states->set_init_state(init_s);
  if (lmn_env.enable_compress_mem)
    init_s->free_mem();

  /** START
   */
  wp->launch_lmn_workers();

#ifdef DEBUG
  if (lmn_env.show_reduced_graph && lmn_env.enable_por &&
      !lmn_env.enable_por_old) {
    dpor_explore_redundunt_graph(states);
  }
#endif

  if (lmn_env.mem_enc == FALSE)
    mem->free_rec();
  /** FINALIZE
   */
  profile_statespace(wp);
  mc_dump(wp);
  delete wp;
  if (lmn_env.tree_compress) {
    lmn_bscomp_tree_clean();
  }
}

/* 後始末と出力周りを担当 */
static void mc_dump(LmnWorkerGroup *wp) {
  StateSpaceRef ss = worker_states(wp->get_worker(LMN_PRIMARY_ID));
  if (lmn_env.dump) {
    ss->format_states();

    /* 1. 状態遷移グラフの標準出力 */
    if (lmn_env.trace) {
      ss->dump();
    }

    /* 2. 反例パス or 最終状態集合の出力 */
    auto out = ss->output();
    if (wp->workers_are_do_search()) {
      mc_dump_all_errors(wp, out);
    } else if (lmn_env.end_dump && lmn_env.mc_dump_format == CUI) {
      /* とりあえず最終状態集合の出力はCUI限定。(LaViTに受付フォーマットがない)
       */
      ss->dump_ends();
    }

    /* CUIモードの場合状態数などのデータも標準出力 */
    if (lmn_env.mc_dump_format == CUI) {
      fprintf(out, "\'# of States\'(stored)   = %lu.\n",
              ss->num());
      fprintf(out, "\'# of States\'(end)      = %lu.\n",
              ss->num_of_ends());
      if (wp->workers_are_do_search()) {
        fprintf(out, "\'# of States\'(invalid)  = %lu.\n",
                mc_invalids_get_num(wp));
      }
#ifdef KWBT_OPT
      if (lmn_env.opt_mode != OPT_NONE) {
        if (!wp->workers_get_opt_end_state()) {
          fprintf(out, "\'# can't solve the problem\'.\n");
        } else {
          fprintf(out, "\'# optimized cost\'      = %lu.\n",
                  wp->opt_cost());
        }
      }
#endif
    }
  }

  if (ss->has_property())
    lmn_prof.has_property = TRUE;
  if (wp->workers_have_error())
    lmn_prof.found_err = TRUE;
}

/** =====================================================
 *  === Fundamental System for StateSpace Generation ====
 *  =====================================================
 */

static inline void mc_gen_successors_inner(MCReactContext *rc,
                                           LmnMembraneRef cur_mem);
static inline void stutter_extension(State *s, LmnMembraneRef mem,
                                     BYTE next_label, MCReactContext *rc,
                                     BOOL flags);

/* 状態sから1stepで遷移する状態を計算し, 遷移元状態と状態空間に登録を行う
 * 遷移先状態のうち新規状態がnew_statesに積まれる */
void mc_expand(const StateSpaceRef ss, State *s, AutomataStateRef p_s,
               MCReactContext *rc, Vector *new_ss, Vector *psyms, BOOL f) {
  LmnMembraneRef mem;

  /** restore : 膜の復元 */
  mem = state_restore_mem(s);

  /** expand  : 状態の展開 */
  if (p_s) {
    mc_gen_successors_with_property(s, mem, p_s, rc, psyms, f);
  } else {
    mc_gen_successors(s, mem, DEFAULT_STATE_ID, rc, f);
  }

  if (rc->get_expanded_num() == 0) {
    /* sを最終状態集合として記録 */
    ss->mark_as_end(s);
  } else if (mc_enable_por(f) && !s->s_is_reduced()) {
    /* POR: 遷移先状態集合:en(s)からample(s)を計算する.
     * 呼び出し先で, mc_store_successorsに相当する処理を済ませる */
    dpor_start(ss, s, rc, new_ss, f);
  } else {
    /* sのサクセッサを状態空間ssに記録 */
    mc_store_successors(ss, s, rc, new_ss, f);
  }

  if (!s->state_mem()) {
    /** free   : 遷移先を求めた状態sからLMNtalプロセスを開放 */
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__REDUCED_MEMSET, mem->space());
    }
#endif
    mem->free_rec();
    if (s->is_binstr_user() &&
        (lmn_env.hash_compaction || lmn_env.tree_compress)) {
      s->free_binstr();
    }
  }

  /* この時点で, sのサクセッサ登録が全て完了->フラグセット
   * (フラグがセットされた状態が, 受理サイクル探索の対象となるので,
   *  フラグセットのタイミングは重要.) */

  s->set_expanded();
  RC_CLEAR_DATA(rc);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_total_space_update(ss);
  }
#endif
}

/* 状態sの全ての遷移先状態のコストを可能ならば更新し、updateフラグを立てる.
 * TODO: 排他制御が適当です by kawabata */
void mc_update_cost(State *s, Vector *new_ss, EWLock *ewlock) {
  unsigned int i, n;
  BOOL f;
  State *succ;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__COST_UPDATE);
  }
#endif

  s->s_unset_update();
  n = s->successor_num;
  f = (lmn_env.opt_mode == OPT_MINIMIZE);
  for (i = 0; i < n; i++) {
    succ = state_succ_state(s, i);
    state_update_cost(succ, transition(s, i), s, new_ss, f, ewlock);
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__COST_UPDATE);
  }
#endif
}

/** 生成した各Successor Stateが既出か否かを検査し,
 * 遷移元の状態sのサクセッサに設定する.
 *   + 多重辺を除去する.
 *   + "新規"状態をnew_ssへ積む.　 */
void mc_store_successors(const StateSpaceRef ss, State *s, MCReactContext *rc,
                         Vector *new_ss, BOOL f) {
  unsigned int i, succ_i;

  /** 状態登録 */
  succ_i = 0;
  for (i = 0; i < rc->get_expanded_num(); i++) {
    TransitionRef src_t;
    void *tmp;
    State *src_succ, *succ;
    LmnMembraneRef src_succ_m;

    /* 状態sのi番目の遷移src_tと遷移先状態src_succを取得 */
    if (!s->has_trans_obj()) {
      /* Transitionオブジェクトを利用しない場合 */
      src_t = NULL;
      src_succ = (State *)rc->expanded_states(i);
    } else {
      src_t = (TransitionRef)rc->expanded_states(i);
      src_succ = transition_next_state(src_t);
    }

    if (rc->has_optmode(BinaryStringDeltaCompress) && RC_D_COND(rc)) {
      /* delta-stringフラグをこの時点で初めて立てる */
      src_succ->s_set_d();
    }

    /* 状態空間に状態src_succを記録 */
    if (rc->has_optmode(DeltaMembrane)) { /* --delta-mem */
      MemDeltaRoot *d = rc->get_mem_delta_roots().at(i);
      succ = ss->insert_delta(src_succ, d);
      src_succ_m = NULL;
    } else if (src_succ->is_encoded()) { /* !--delta-mem && --mem-enc */
      if (src_succ->s_is_d())
        src_succ->calc_binstr_delta();
      succ = ss->insert(src_succ);
      src_succ_m = NULL;
    } else {                            /* default */
      src_succ_m = src_succ->state_mem(); /* for free mem pointed by src_succ */
      succ = ss->insert(src_succ);
    }

    if (succ == src_succ) {
      /* new state */
      state_id_issue(succ);
      if (mc_use_compress(f) && src_succ_m) {
	src_succ_m->free_rec();
      }
      if (new_ss)
        new_ss->push((vec_data_t)succ);
      if (mc_is_dump(f))
        StateDumper::from_env(stdout)->dump(succ);
    } else {
      /* contains */
      delete(src_succ);
      if (s->has_trans_obj()) {
        /* Transitionオブジェクトが指すサクセッサを検出した等価な状態の方へ設定し直す
         */
        transition_set_state(src_t, succ);
      }
    }

    /* 多重辺(1stepで合流する遷移関係)を除去 */
    tmp = rc->get_transition_to(succ);
    if (tmp == nullptr) {
      /* succへの遷移が多重辺ではない場合 */
      void *ins;
      if (s->has_trans_obj()) {
        ins = src_t;
      } else {
        ins = succ;
      }
      /* 状態succをサクセッサテーブルへ記録(succをkeyにして,
       * succに対応する遷移insを登録) */
      rc->set_transition_to(succ, ins);

      /* 遷移先情報を記録する一時領域(in ReactCxt)を更新 */
      rc->set_expanded_state(succ_i++, ins);
    } else if (s->has_trans_obj()) {
      /* succへの遷移が多重辺かつTransitionオブジェクトを利用する場合 */
      /* src_tは状態生成時に張り付けたルール名なので, 0番にしか要素はないはず */
      transition_add_rule((TransitionRef)tmp, transition_rule(src_t, 0),
                          transition_cost(src_t));
      transition_free(src_t);
    } /*
    else succへの遷移が多重辺かつTransitionオブジェクトを利用しない場合
         then "辺"という構造を持たない(直接pointerで刺している)ので何もしない
    */
  }

  rc->clear_successor_table();

  rc->resize_expanded_states(succ_i); /* 危険なコード. いつか直すかも. */
  rc->resize_expanded_rules(succ_i);
  /*  上記につられて以下のコードを記述すると実行時エラーになる. (r436でdebug)
   *  RC_MEM_DELTASはmc_store_successors終了後に, struct
   * MemDeltaRootの開放処理を行うため要素数に手を加えてはならない. */
  //  if (rc->has_optmode(DeltaMembrane)) {
  //    rc->resize_mem_delta_roots(succ_i);
  //  }

  state_D_progress(s, rc);
  s->succ_set(rc->expanded_states()); /* successorを登録 */
}

/*
 * 状態を展開する
 * 引数として与えられた膜と、その膜に所属する全てのアクティブ膜に対して
 * ルール適用検査を行う
 * 子膜からルール適用を行っていく
 */
BOOL mc_expand_inner(MCReactContext *rc, LmnMembraneRef cur_mem) {
  BOOL ret_flag = FALSE;

  for (; cur_mem; cur_mem = cur_mem->mem_next()) {
    unsigned long org_num = rc->get_expanded_num();

    /* 代表子膜に対して再帰する */
    if (mc_expand_inner(rc, cur_mem->mem_child_head())) {
      ret_flag = TRUE;
    }
    if (cur_mem->is_active()) {
      react_all_rulesets(rc, cur_mem);
    }
    /* 子膜からルール適用を試みることで, 本膜の子膜がstableか否かを判定できる */
    if (org_num == rc->get_expanded_num()) {
      cur_mem->set_active(FALSE);
    }
  }

  return ret_flag;
}

/** 膜memから1stepで遷移可能な膜を, state_nameと合わせて状態として生成し,
 *  TransitionをRC_EXPANDEDへセットする.
 *  生成された状態は重複（多重辺）を含む */
void mc_gen_successors(State *src, LmnMembraneRef mem, BYTE state_name,
                       MCReactContext *rc, BOOL f) {
  unsigned int i, n, old;

  rc->set_global_root(mem);

  /* 性質遷移数だけ本関数を呼び出している.
   * 一つ前までの展開遷移数をメモしておくことで, 今回の展開遷移数を計算する */
  old = rc->get_expanded_num();

  /** Generate Successor Membrane (or DeltaMemrane) */
  mc_gen_successors_inner(rc, mem);

  /** Generate Successor States */
  n = rc->get_expanded_num();

  for (i = old; i < n; i++) {
    State *news;
    void *data;

    /* DeltaMembrane時はこの時点でSuccessor Membraneがない */
    if (mc_use_delta(f)) {
      news = new State();
    } else {
      news = new State((LmnMembraneRef)rc->expanded_states(i), state_name,
                        mc_use_canonical(f));
    }

    state_set_property_state(news, state_name);
    state_set_parent(news, src);
    if (mc_has_trans(f)) {
      lmn_interned_str nid;
      nid = rc->get_expanded_rule(i)->name;
      data = transition_make(news, nid);
     src->set_trans_obj();
    } else {
      data = news;
    }

    /* DeltaMembrane時は, expanded_rootsが空であるため, 生成した空の状態を積む
     * 通常時は, expanded_rootsのi番目のデータを
     *           Successor MembraneからSuccessor Stateへ設定し直す */
    if (mc_use_delta(f)) {
      rc->push_expanded_state(data);
    } else {
      rc->set_expanded_state(i, data);
    }
  }

  RC_ND_SET_ORG_SUCC_NUM(rc, (n - old));
}

/* 膜memから,
 * 性質ルールとシステムルールの同期積によって遷移可能な全状態(or遷移)をベクタに積む
 */
void mc_gen_successors_with_property(State *s, LmnMembraneRef mem,
                                     AutomataStateRef p_s, MCReactContext *rc,
                                     Vector *propsyms, BOOL f) {
  unsigned int i, j;

  /** 状態展開:
   *   性質ルールが適用される場合,
   * global_rootに対してシステムルール適用検査を行う.
   *   システムルール適用はglobal_rootのコピーに対して行われる.
   *   状態展開にexpandが使用された場合は,
   * 現状態から遷移可能な全ての状態が生成されるのに対し，
   *   ampleが使用された場合はPartial Order
   * Reductionに基づき，次状態の代表のみ生成される．
   *   なお，適用可能なシステムルールが存在しない場合は，
   *   何も処理を行わない特殊なシステムルール(stutter extension
   * rule)によるε遷移を行う. これにより,
   * 受理頂点に次状態が存在しない場合でも受理サイクルを形成できるため,
   *   満たすべき仕様に対する反例を漏れなく検出できるようになる．
   *   (c.f. "The Spin Model Checker" pp.130-131)
   */

  for (i = 0; i < p_s->get_transition_num(); i++) {
    AutomataTransitionRef p_t = p_s->get_transition(i);
    if (eval_formula(mem, propsyms, p_t->get_formula())) {
      BYTE p_nxt_l = p_t->get_next();
      rc->push_expanded_property(p_nxt_l);
    }
  }

  if (rc->get_expanded_properties().empty()) {
    return;
  } else {
    BYTE first_prop = rc->get_expanded_properties().at(0);
    mc_gen_successors(s, mem, first_prop, rc, f);
    if (rc->get_expanded_num() == 0) {
      stutter_extension(s, mem, first_prop, rc, f);
    }
  }

  /* 階層グラフ構造は等価だが性質ラベルの異なる状態を生成する.　*/
  RC_ND_SET_ORG_SUCC_NUM(rc, rc->get_expanded_num());
  for (i = 1; i < rc->get_expanded_properties().size(); i++) {
    BYTE p_nxt_l = rc->get_expanded_properties().at(i);
    for (j = 0; j < RC_ND_ORG_SUCC_NUM(rc); j++) {
      TransitionRef src_succ_t;
      State *src_succ_s, *new_s;
      void *data;

      if (mc_has_trans(f)) {
        src_succ_t = (TransitionRef)rc->expanded_states(j);
        src_succ_s = transition_next_state(src_succ_t);
      } else {
        src_succ_t = NULL;
        src_succ_s = (State *)rc->expanded_states(j);
      }

      new_s = src_succ_s->duplicate(NULL);
      state_set_parent(new_s, s);
      state_set_property_state(new_s, p_nxt_l);

      if (mc_has_trans(f)) {
        data =
            transition_make(new_s, transition_rule(src_succ_t, 0));
#ifdef KWBT_OPT
        transition_set_cost((Transition)data, transition_cost(src_succ_t));
#endif
       s->set_trans_obj();
      } else {
        data = new_s;
      }
      rc->push_expanded_state(data);

      /* 差分オブジェクトは状態展開時のみの一時データなので,
       * 効率化のためにポインタcopyのみにしている(deep copyしない)
       * !! 開放処理は要注意 (r435でdebug) !! */
      if (rc->has_optmode(DeltaMembrane)) {
        rc->push_mem_delta_root(rc->get_mem_delta_roots().at(j));
      }
    }
  }
}

/* 状態sからStutter Extension Ruleを適用した遷移を展開する.
 * 膜memとmemに対応する状態sをコピーし, 性質ラベルnext_labelを持った状態として,
 * サクセッサ展開の一時領域(in ReactCxt)に追加する */
static inline void stutter_extension(State *s, LmnMembraneRef mem,
                                     BYTE next_label, MCReactContext *rc,
                                     BOOL f) {
  void *data;
  State *new_s;

  /* Stutter Extension Rule:
   *
   * 性質ルールで可能な遷移が存在するにも拘らず,
   * システムルールで可能な遷移が存在しない場合,
   * 同期積オートマトングラフ上で遷移が出現せず,
   * 受理サイクルが形成されなくなってしまうことがある.
   * これを防ぐために同期積オートマトングラフ上でε遷移を形成させる.
   * これをStutter Extensionと呼ぶ.
   * Stutter Extensionは性質オートマトン上のみの遷移となるため,
   * 同期積オートマトングラフ上では
   *   1. self-loopとなる(遷移の前後で性質ラベルが変化しない)か
   *   2. 性質ラベルの変化のみ
   *      (状態データには変化がないため,
   * どもっている(Stuttering)と表現するらしい) となる.
   */

  if (mc_use_delta(f)) {
    /* 差分構造が存在しないstruct MemDeltaRootを登録する. */
    rc->add_mem_delta(new MemDeltaRoot(mem, NULL, 0), NULL);
    new_s = new State();
  } else {
    /* 遷移元状態sをdeep copyする.
     * ただし, sに対応した階層グラフ構造はこの時点では既に破棄されているため,
     * mc_expandの時点で再構築した階層グラフ構造memを渡す.  */
    new_s = s->duplicate(mem);
  }
  state_set_property_state(new_s, next_label);
  state_set_parent(new_s, s);

  if (mc_has_trans(f)) {
    data = transition_make(new_s, lmn_intern("ε"));
   s->set_trans_obj();
  } else {
    data = new_s;
  }
  rc->push_expanded_state(data);
}

static inline void mc_gen_successors_inner(MCReactContext *rc,
                                           LmnMembraneRef cur_mem) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif

  mc_expand_inner(rc, cur_mem);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif
}

/** ===========================================
 *  === Counter Example generator / dumper ====
 *  ===========================================
 */

/* エラーを示す頂点を登録する.
 * MT-Unsafe, but ok */
void mc_found_invalid_state(LmnWorkerGroup *wp, State *s) {
  wp->workers_found_error();
  if (s) {
    LmnWorker *w = wp->workers_get_my_worker();
    worker_invalid_seeds(w)->push((vec_data_t)s);
  }

  if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}

/* エラーを示すサイクルパスを載せたVectorを登録する.
 * MT-Unsafe, but ok  */
void mc_found_invalid_path(LmnWorkerGroup *wp, Vector *v) {
  wp->workers_found_error();

  if (v) {
    LmnWorker *w = wp->workers_get_my_worker();
    worker_cycles(w)->push((vec_data_t)v);
  }

  if (!wp->workers_are_do_exhaustive()) {
    wp->workers_set_exit();
  }
}

unsigned long mc_invalids_get_num(LmnWorkerGroup *wp) {
  unsigned long ret;
  unsigned int i;

  ret = 0;
  for (i = 0; i < wp->workers_get_entried_num(); i++) {
    ret += worker_invalid_seeds(wp->get_worker(i))->get_num();
  }

  return ret;
}

/* 初期頂点から状態seedに至るパスを載せたVectorをmallocして返す */
static Vector *mc_gen_invalids_path(State *seed) {
  Vector *path;
  State *pred;

  path = new Vector(32);
  pred = seed;

  while (pred) { /* 初期頂点のparentはNULL */
    path->push((vec_data_t)pred);
    pred = state_get_parent(pred);
  }

  path->reverse();

  return path;
}

#define MC_INSERT_INVALIDS(G, S_KEY, SUCC)                                     \
  do {                                                                         \
    Vector *succs;                                                             \
    st_data_t t;                                                               \
                                                                               \
    t = 0;                                                                     \
    if (st_lookup(G, (st_data_t)(S_KEY), &t)) {                                \
      succs = (Vector *)t;                                                     \
      if (!succs->contains((vec_data_t)SUCC)) {                            \
        succs->push((vec_data_t)SUCC);                                     \
      }                                                                        \
    } else {                                                                   \
      succs = new Vector(2);                                                     \
      succs->push((vec_data_t)SUCC);                                       \
      st_insert(G, (st_data_t)(S_KEY), (st_data_t)succs);                      \
    }                                                                          \
  } while (0)

/* Vectorに積まれた状態を, 遷移元の状態idをkeyに, 遷移先の集合をvalueとした
 * ハッシュ表に登録する */
static void mc_store_invalids_graph(AutomataRef a, st_table_t g, Vector *v) {
  unsigned int i, j;

  for (i = 0, j = 1; i < v->get_num() && j < v->get_num(); i++, j++) {

    State *s1, *s2;

    s1 = (State *)v->get(i);
    s2 = (State *)v->get(j);

    MC_INSERT_INVALIDS(g, s1, s2);
  }

  if (state_is_end(a, (State *)v->peek())) {
    State *s = (State *)v->peek();
    MC_INSERT_INVALIDS(g, s, NULL);
  }
}

BOOL mc_vec_states_valid(Vector *v) {
  unsigned int i, j;
  for (i = 0, j = 1; i < v->get_num() && j < v->get_num(); i++, j++) {
    State *s, *t;
    s = (State *)v->get(i);
    t = (State *)v->get(j);
    if (!state_succ_contains(s, t))
      return FALSE;
  }

  return TRUE;
}

static int mc_dump_invalids_f(st_data_t _key, st_data_t _v, st_data_t _arg) {
  StateSpaceRef ss;
  State *s;
  Vector *succs;
  unsigned int i;

  ss = (StateSpaceRef)_arg;
  s = (State *)_key;
  succs = (Vector *)_v;

  /* TODO: Rehashされていた場合には状態データが出力できない
   *       オリジナルテーブル側のdummy状態に対応するmemid状態を探索して引っ張ってくる必要がある
   */

  auto out = ss->output();
  fprintf(out, "%lu::", state_format_id(s, ss->is_formatted()));
  for (i = 0; i < succs->get_num(); i++) {
    State *succ = (State *)succs->get(i);
    if (succ) {
      fprintf(out, "%s%lu", (i > 0) ? ", " : "",
              state_format_id(succ, ss->is_formatted()));
    }
  }
  fprintf(out, "\n");

  return ST_CONTINUE;
}

int mc_free_succ_vec_f(st_data_t _key, st_data_t _v, st_data_t _arg) {
  Vector *v = (Vector *)_v;
  delete v;
  return ST_CONTINUE;
}

void mc_print_vec_states(StateSpaceRef ss, Vector *v, State *seed) {
  unsigned int i;

  for (i = 0; i < v->get_num(); i++) {
    State *s;
    auto out = ss->output();

    if (lmn_env.sp_dump_format != LMN_SYNTAX) {
      const char *m;
      s = (State *)v->get(i);
      m = (s == seed) ? "*" : " ";
      fprintf(out, "%s%2lu::%s", m, state_format_id(s, ss->is_formatted()),
              ss->automata()->state_name(state_property_state(s)));
      StateDumper::from_env(out)->state_print_mem(s);
    } else {
      s = (State *)v->get(i);
      fprintf(out, "path%lu_%s", state_format_id(s, ss->is_formatted()),
              ss->automata()->state_name(state_property_state(s)));
      StateDumper::from_env(out)->state_print_mem(s);
      fprintf(out, ".\n");

      fprintf(out, "path%lu_%s", state_format_id(s, ss->is_formatted()),
              ss->automata()->state_name(state_property_state(s)));
      StateDumper::from_env(out)->state_print_mem(s);
      fprintf(out, ":- ");
    }
  }
}

/* 各LmnWorkerに登録した反例を出力する.
 * 登録された反例は頂点のみであり,
 * 登録された頂点が, AutomataState::is_endの場合, safety
 * その他の場合は, その頂点を起点にしたサイクルパスにon_cycle_flagが立っている
 */
void mc_dump_all_errors(LmnWorkerGroup *wp, FILE *f) {
  if (!wp->workers_have_error()) {
    fprintf(f, "%s\n",
            lmn_env.mc_dump_format == CUI
                ? "No Accepting Cycle (or Invalid State) exists."
                : "");
  } else {
    switch (lmn_env.mc_dump_format) {
    case LaViT:
    case CUI: {
      st_table_t invalids_graph;
      unsigned int i, j;
      BOOL cui_dump;

      fprintf(f, "%s\n",
              lmn_env.sp_dump_format == LMN_SYNTAX ? "counter_exapmle."
                                                   : "CounterExamplePaths");

      cui_dump = (lmn_env.mc_dump_format == CUI);
      invalids_graph = cui_dump ? NULL : st_init_ptrtable();

      /* state property */
      for (i = 0; i < wp->workers_get_entried_num(); i++) {
        LmnWorker *w;
        Vector *v;
        StateSpaceRef ss;

        w = wp->get_worker(i);
        v = worker_invalid_seeds(w);
        ss = worker_states(w);

        for (j = 0; j < v->get_num(); j++) {
          Vector *path = mc_gen_invalids_path((State *)v->get(j));

          if (cui_dump) { /* 出力 */
            mc_print_vec_states(ss, path, NULL);
            fprintf(f, "\n");
          } else { /* ハッシュ表に追加 */
            mc_store_invalids_graph(ss->automata(), invalids_graph,
                                    path);
          }
          delete path;
        }
      }

      /* path property */
      for (i = 0; i < wp->workers_get_entried_num(); i++) {
        LmnWorker *w;
        Vector *v;
        StateSpaceRef ss;

        w = wp->get_worker(i);
        v = worker_cycles(w);
        ss = worker_states(w);

        for (j = 0; j < v->get_num(); j++) {
          Vector *cycle, *path;
          State *seed;

          cycle = (Vector *)v->get(j);
          seed = (State *)cycle->get(0);
          path = mc_gen_invalids_path(seed);

          cycle->push(
                   (vec_data_t)seed); /* seed to seedのパスを取得するため */

          if (cui_dump) {
            path->pop(); /* cycle Vectorとpath
                              Vectorでseedが重複して積まれているため */
            mc_print_vec_states(ss, path, NULL);
            mc_print_vec_states(ss, cycle, seed);
            fprintf(f, "\n");
          } else {
            LMN_ASSERT(invalids_graph);
            mc_store_invalids_graph(ss->automata(), invalids_graph,
                                    path);
            mc_store_invalids_graph(ss->automata(), invalids_graph,
                                    cycle);
          }

          delete path;
        }
      }

      if (!cui_dump) {
        StateSpaceRef represent =
            worker_states(wp->get_worker(LMN_PRIMARY_ID));
        st_foreach(invalids_graph, (st_iter_func)mc_dump_invalids_f,
                   (st_data_t)represent);
        st_foreach(invalids_graph, (st_iter_func)mc_free_succ_vec_f,
                   (st_data_t)0);
        st_free_table(invalids_graph);
      }

      break;
    }

    case Dir_DOT: /* TODO:
                     反例パスをサブグラフとして指定させたら分かりやすくなりそう
                   */;
    default:
      lmn_fatal("unexpected.");
      break;
    }
  }
}

/** =================================
 *  === Property Automata loader ====
 *  =================================
 */

enum MC_ERRORNO {
  MC_ERR_NC_ENV,
  MC_ERR_PROP_ENV,
  MC_NC_OPEN_ERROR,
  MC_NC_LOAD_ERROR,
  MC_PROP_OPEN_ERROR,
  MC_PROP_LOAD_ERROR,
};

/* 性質を表すBuchiオートマトン, 命題記号定義(ルール左辺)の集合を,
 * それぞれa, prop_defsに読み込む.
 * 成功ならば0, そうでないならば0以外を返す */
int mc_load_property(AutomataRef *a, PVector *prop_defs) {
  FILE *nc_fp, *prop_fp;
  int r;

  *a = NULL;
  *prop_defs = NULL;
  nc_fp = prop_fp = NULL;

  if (lmn_env.ltl_exp) {
    nc_fp = ltl2ba_str(lmn_env.ltl_exp);
  } else {
    if (!lmn_env.automata_file)
      goto NC_ENV;
    if (!(nc_fp = fopen(lmn_env.automata_file, "r")))
      goto NC_OPEN_ERROR;
  }
  if (never_claim_load(nc_fp, a))
    goto NC_LOAD_ERROR;

  if (!lmn_env.propositional_symbol)
    goto PROP_ENV;
  if (!(prop_fp = fopen(lmn_env.propositional_symbol, "r")))
    goto PROP_OPEN_ERROR;
  if (propsym_load_file(prop_fp, *a, prop_defs))
    goto PROP_LOAD_ERROR;

  r = 0;
  goto RET;

NC_ENV:
  r = MC_ERR_NC_ENV;
  goto FINALLY;
PROP_ENV:
  r = MC_ERR_PROP_ENV;
  goto FINALLY;
NC_OPEN_ERROR:
  r = MC_NC_OPEN_ERROR;
  goto FINALLY;
NC_LOAD_ERROR : {
  char c;
  rewind(nc_fp);
  while ((c = fgetc(nc_fp)) != EOF) {
    fputc(c, stderr);
  }
  r = MC_NC_LOAD_ERROR;
  goto FINALLY;
}
PROP_OPEN_ERROR:
  r = MC_PROP_OPEN_ERROR;
  goto FINALLY;
PROP_LOAD_ERROR:
  r = MC_PROP_LOAD_ERROR;
  goto FINALLY;

FINALLY:
  LMN_FREE(*a);
  LMN_FREE(*prop_defs);

RET:
  if (prop_fp)
    fclose(prop_fp);
  if (nc_fp)
    fclose(nc_fp);
  return r;
}

void mc_explain_error(int error_id) { lmn_report(mc_error_msg(error_id)); }

const char *mc_error_msg(int error_id) {
  switch (error_id) {
  case MC_ERR_NC_ENV:
    return "specify never claim file";
  case MC_ERR_PROP_ENV:
    return "specify propositional symbol definition file";
  case MC_NC_OPEN_ERROR:
    return "cannot open never claim file";
  case MC_NC_LOAD_ERROR:
    return "error while parsing never claim file";
  case MC_PROP_OPEN_ERROR:
    return "cannot open propositional symbol definition file";
  case MC_PROP_LOAD_ERROR:
    return "error while parsing propositional symbol definition file";
  default:
    lmn_fatal("implementation error\n");
    return NULL;
  }
}
