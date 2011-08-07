/*
 * mc.c
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

#include "mc.h"
#include "mc_worker.h"
#include "task.h"
#include "dpor.h"
#include "error.h"
#include "delta_membrane.h"
#include "propositional_symbol.h"
#include "ltl2ba_adapter.h"
#include "runtime_status.h"
#ifdef DEBUG
#  include "dumper.h"
#endif


/** =======================================
 *  ==== Entrance for model checking ======
 *  =======================================
 */

static BOOL mc_status_init(void);
static void mc_status_finalize(StateSpace states);


/* 非決定実行を行う. run_mcもMT-unsafeなので子ルーチンとしては使えない */
void run_mc(Vector *start_rulesets)
{
  static LmnMembrane *mem; /* jniではここがstaticでないと困るらしい */
  StateSpace states;
  BOOL flags;

  flags = mc_status_init();

  if (lmn_env.nd_cleaning) {
    /* nd_cleaning状態の場合は、グローバルルート膜の解放を行う */
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
    lmn_env.nd_cleaning = FALSE;
  }

  if(!lmn_env.nd_remain && !lmn_env.nd_remaining) {
    mem = lmn_mem_make();
  }

  react_start_rulesets(mem, start_rulesets);
  lmn_mem_activate_ancestors(mem);

  states = do_mc(mem, flags);

  if (lmn_env.nd_remain) {
    lmn_env.nd_remaining = TRUE;
  } else {
    lmn_env.nd_remaining = FALSE;
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }

  profile_statespace(states);
  mc_status_finalize(states);
  state_space_free(states);
}


/* mc_workers_env_initがMT-unsafeなので, do_mcもMT-unsafe */
StateSpace do_mc(LmnMembrane *world_mem_org, BOOL f)
{
  StateSpace states;
  State *init_s;
  BYTE p_label;

  /** INITIALIZE */
  states  = mc_data.do_parallel ? state_space_make_for_parallel()
                                : state_space_make();
  p_label = !mc_has_property(f) ? DEFAULT_STATE_ID
                                : automata_get_init_state(mc_data.property_automata);
  init_s  = state_make(lmn_mem_copy(world_mem_org),
                       p_label,
                       state_space_use_memenc(states));
  state_space_set_init_state(states, init_s, mc_use_compact(f));

  state_id_issue(init_s); /* 状態番号を記録 */
  if (mc_is_dump(f))  dump_state_data(init_s, (LmnWord)stdout);

  lmn_workers_env_init(states, f); /* フラグfはここで各Workerに登録される */

  /** START */
  launch_lmn_workers();

#ifdef DEBUG
  if (lmn_env.show_reduced_graph && lmn_env.enable_por && !lmn_env.enable_por_old) {
    dpor_explore_redundunt_graph(states);
  }
#endif

  /** FINALIZE */
  lmn_workers_env_finalize();

  return states;
}



/* 実行時オプションが増えたため,
 * 組み合わせをサポートしていないオプションの整合性を取るなどを行う. */
static BOOL mc_status_init()
{
  BOOL flags = 0x00U;

  if (lmn_env.output_format == DOT)   lmn_env.output_format = DEFAULT;
  if (lmn_env.sp_dump_format == INCREMENTAL) {
    mc_set_dump(flags);
    if (lmn_env.mc_dump_format != CUI && lmn_env.mc_dump_format != LaViT) {
      lmn_fatal("unsupported incremental output format");
    }
    fprintf(stdout, "States\n");
  }

  if (lmn_env.show_transition) {
    mc_set_trans(flags);
  }

  if (lmn_env.enable_compress_mem) {
    mc_set_compress(flags);
    if (lmn_env.delta_mem)     mc_set_delta(flags);
    if (lmn_env.compact_stack) mc_set_compact(flags);
  } else {
    lmn_env.optimize_hash = FALSE;
#ifdef PROFILE
    lmn_env.optimize_hash_old = FALSE;
#endif
  }

  if (mc_data.has_property) {
    mc_set_property(flags);
  }

  if (lmn_env.ltl) {
    if (!mc_data.has_property) {
      lmn_env.ltl = FALSE;
      lmn_env.ltl_all = FALSE;
      mc_data.do_search = FALSE;
      mc_data.do_exhaustive = FALSE;
    } else {
      unsigned int i;
      mc_data.invalid_seeds = LMN_NALLOC(struct Vector *, lmn_env.core_num);
      mc_data.cycles = LMN_NALLOC(struct Vector *, lmn_env.core_num);
      for (i = 0; i < lmn_env.core_num; i++) {
        mc_data.invalid_seeds[i] = vec_make(4);
        mc_data.cycles[i] = vec_make(4);
      }
    }
  }

  if (lmn_env.enable_por || lmn_env.enable_por_old) {
    mc_set_por(flags);
  }

  if (lmn_env.core_num >= 2) {
    if (lmn_env.sp_dump_format == INCREMENTAL) {
   //   lmn_fatal("unsupported combination incremental state dumper & parallelization.");
    }
#ifdef PROFILE
    if (lmn_env.optimize_hash_old) {
      lmn_fatal("unsupported combination optimized hash (old) & parallelization.");
    }
#endif
  }

  return flags;
}


/* 後始末と出力周りを担当 */
static void mc_status_finalize(StateSpace states)
{
  if (lmn_env.dump) {
    FILE *out = stdout;
    state_space_format_states(states);

    /* 1. 状態遷移グラフの標準出力 */
    if (lmn_env.trace) {
      state_space_dumper(states, out);
    }

    /* 2. 反例パス or 最終状態集合の出力 */
    if (mc_data.do_search) {
      mc_dump_all_errors(states, out);
    }
    else if (lmn_env.end_dump && lmn_env.mc_dump_format == CUI) {
      /* とりあえず最終状態集合の出力はCUI限定。(LaViTに受付フォーマットがない) */
      state_space_ends_dumper(states, out);
    }

    /* CUIモードの場合状態数などのデータも標準出力 */
    if (lmn_env.mc_dump_format == CUI) {
      fprintf(out, "\'# of States\'(stored)   = %lu.\n", state_space_num(states));
      fprintf(out, "\'# of States\'(end)      = %lu.\n", state_space_end_num(states));
      if (mc_data.do_search) {
        fprintf(out, "\'# of States\'(invalid)  = %lu.\n", mc_invalids_get_num());
      }
    }
  }

  if (lmn_env.ltl) {
    unsigned int i, j;
    for (i = 0; i < lmn_env.core_num; i++) {
      vec_free(mc_data.invalid_seeds[i]);

      for (j = 0; j < vec_num(mc_data.cycles[i]); j++) {
        vec_free((Vector *)vec_get(mc_data.cycles[i], j));
      }
      vec_free(mc_data.cycles[i]);
    }
    LMN_FREE(mc_data.invalid_seeds);
    LMN_FREE(mc_data.cycles);
  }
}



/** =====================================================
 *  === Fundamental System for StateSpace Generation ====
 *  =====================================================
 */

inline static void mc_gen_successors_inner(struct ReactCxt *rc, LmnMembrane *cur_mem);
inline static void stutter_extension(State           *s,
                                     LmnMembrane     *mem,
                                     BYTE            next_label,
                                     struct ReactCxt *rc,
                                     BOOL            flags);


/* 状態sから1stepで遷移する状態を計算し, 遷移元状態と状態空間に登録を行う
 * 遷移先状態のうち新規状態がnew_statesに積まれる */
void mc_expand(const StateSpace ss,
               State            *s,
               AutomataState    p_s,
               struct ReactCxt  *rc,
               Vector           *new_ss,
               BOOL             f)
{
  LmnMembrane *mem;

  /** restore : 膜の復元 */
  mem = state_restore_mem(s);

  /** expand  : 状態の展開 */
  if (mc_has_property(f)) {
    mc_gen_successors_with_property(s, mem, p_s, rc, f);
  } else {
    mc_gen_successors(s, mem, DEFAULT_STATE_ID, rc, f);
  }

  if (mc_react_cxt_expanded_num(rc) == 0) {
    state_space_add_end_state(ss, s);
  } else if (!mc_enable_por(f) || s_is_reduced(s)) {
    mc_store_successors(ss, s, rc, new_ss, f);
  } else {
    /* POR使用時は, 遷移先状態集合:en(s)からample(s)を計算する
     * サブルーチン側で, sに対するサクセッサの登録まで済ませる */
    dpor_start(ss, s, rc, new_ss, f);
  }

  /** free   : 遷移先を求めた状態sからLMNtalプロセスを開放 */
  if (mc_use_compress(f)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__REDUCED_MEMSET, lmn_mem_space(mem));
    }
#endif
    if (!state_mem(s)) { /* compact-stack */
      lmn_mem_drop(mem);
      lmn_mem_free(mem);
    } else {
      state_free_mem(s);
    }
  }

  /* この時点で, sのサクセッサ登録が全て完了->フラグセット
   * (フラグがセットされた状態が, 受理サイクル探索の対象となるので,
   *  フラグセットのタイミングは重要.) */

  set_expanded(s);
  RC_CLEAR_DATA(rc);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_total_space_update(ss);
  }
#endif
}


/** 生成した各Successor Stateが既出か否かを検査し, 遷移元の状態sのサクセッサに設定する.
 *   + 多重辺を除去する.
 *   + "新規"状態をnew_ssへ積む.　 */
void mc_store_successors(const StateSpace ss,
                         State            *s,
                         struct ReactCxt  *rc,
                         Vector           *new_ss,
                         BOOL             f)
{
  st_table_t succ_tbl;         /* サクセッサのポインタの重複検査に使用 */
  unsigned int i, succ_i;

  succ_i       = 0;
  succ_tbl     = RC_SUCC_TBL(rc);

  /** 状態登録 */
  for (i = 0; i < mc_react_cxt_expanded_num(rc); i++) {
    Transition src_t;
    st_data_t tmp;
    State *src_succ, *succ;
    MemDeltaRoot *d;

    src_t     = !has_trans_obj(s) ? NULL
                                  : (Transition)vec_get(RC_EXPANDED(rc), i);
    src_succ  = !has_trans_obj(s) ? (State *)vec_get(RC_EXPANDED(rc), i)
                                  : transition_next_state(src_t);
    d = RC_MC_USE_DMEM(rc) ? (struct MemDeltaRoot *)vec_get(RC_MEM_DELTAS(rc), i)
                           :  NULL;

    /** ハッシュ表へ状態の追加を試みる */
    succ = d ? state_space_insert_delta(ss, src_succ, d)
             : state_space_insert(ss, src_succ);

    if (succ == src_succ) { /** src_succが状態空間に追加された */
      state_id_issue(succ); /* 状態番号を記録 */
      if (mc_is_dump(f))  dump_state_data(succ, (LmnWord)stdout);
      if (mc_use_compact(f)) state_free_mem(succ);
      if (new_ss) {
        vec_push(new_ss, (vec_data_t)succ);
      }
    }
    else {                  /** src_succは追加されなかった（すでに同じ状態がある) */
      state_free(src_succ);
      if (has_trans_obj(s)) { /* ポインタを既存状態へセット */
        transition_set_state(src_t, succ);
      } else {
        vec_set(RC_EXPANDED(rc), i, (vec_data_t)succ);
      }
    }

    /** 多重辺を除去しながら, 状態にサクセッサを設定 */
    tmp = 0;
    if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) { /* 多重辺ではない */
      st_data_t ins = has_trans_obj(s) ? (st_data_t)src_t : (st_data_t)succ;
      st_add_direct(succ_tbl, (st_data_t)succ, ins);
      vec_set(RC_EXPANDED(rc), succ_i++, ins);
    }
    else { /* 多重辺は消す */
      if (has_trans_obj(s)) {
        /* src_tは状態生成時に張り付けたルール名なので, 0番にしか要素はないはず */
        transition_add_rule((Transition)tmp, transition_rule(src_t, 0));
        transition_free(src_t);
      } /* else  "辺"という構造を持たない(直接pointerで表現している)ので何もしない */
    }
  }

  RC_EXPANDED(rc)->num = succ_i;      /* 危険なコード. いつか直すかも. */
  RC_EXPANDED_RULES(rc)->num = succ_i;
/*  上記につられて以下のコードを記述すると実行時エラーになる. (r436でdebug)
 *  RC_MEM_DELTASはmc_store_successors終了後に, struct MemDeltaRootの開放処理を行うため要素数に手を加えてはならない. */
//  if (RC_MC_USE_DMEM(rc)) {
//    RC_MEM_DELTAS(rc)->num = succ_i;
//  }

  state_succ_set(s, RC_EXPANDED(rc)); /* successorを登録 */
  st_clear(RC_SUCC_TBL(rc));
}


/*
 * 状態を展開する
 * 引数として与えられた膜と、その膜に所属する全てのアクティブ膜に対して
 * ルール適用検査を行う
 * 子膜からルール適用を行っていく
 */
BOOL mc_expand_inner(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
  BOOL ret_flag = FALSE;

  for (; cur_mem; cur_mem = cur_mem->next) {
    unsigned long org_num = mc_react_cxt_expanded_num(rc);

    /* 代表子膜に対して再帰する */
    if (mc_expand_inner(rc, cur_mem->child_head)) {
      ret_flag = TRUE;
    }
    if (lmn_mem_is_active(cur_mem)) {
      react_all_rulesets(rc, cur_mem);
    }
    /* 子膜からルール適用を試みることで, 本膜の子膜がstableか否かを判定できる */
    if (org_num == mc_react_cxt_expanded_num(rc)) {
      lmn_mem_set_active(cur_mem, FALSE);
    }
  }

  return ret_flag;
}


/** 膜memから1stepで遷移可能な膜を, state_nameと合わせて状態として生成し,
 *  TransitionをRC_EXPANDEDへセットする.
 *  生成された状態は重複（多重辺）を含む */
void mc_gen_successors(State           *src,
                       LmnMembrane     *mem,
                       BYTE            state_name,
                       struct ReactCxt *rc,
                       BOOL            f)
{
  Vector *expanded_roots, *expanded_rules;
  unsigned int i, n, old;

  RC_SET_GROOT_MEM(rc, mem);

  old = mc_react_cxt_expanded_num(rc);

  /** Generate Successor Membrane (or DeltaMemrane) */
  mc_gen_successors_inner(rc, mem);

  /** Generate Successor States */
  expanded_roots   = RC_EXPANDED(rc);       /* DeltaMembrane時は空 */
  expanded_rules   = RC_EXPANDED_RULES(rc);
  n = mc_react_cxt_expanded_num(rc);
  for (i = old; i < n; i++) {
    State *new;
    vec_data_t data;

    /* DeltaMembrane時はこの時点でSuccessor Membraneがない */
    if (mc_use_delta(f)) {
      new = state_make_minimal();
      state_set_property_state(new, state_name);
    }
    else {
      new = state_make((LmnMembrane *)vec_get(expanded_roots, i),
                       state_name,
                       lmn_env.mem_enc);
    }

    state_set_parent(new, src);
    if (mc_has_trans(f)) {
      data = (vec_data_t)transition_make(new, lmn_rule_get_name((LmnRule)vec_get(expanded_rules, i)));
      set_trans_obj(src);
    } else {
      data = (vec_data_t)new;
    }

    /* DeltaMembrane時は, expanded_rootsが空であるため, 生成した空の状態を積む
     * 通常時は, expanded_rootsのi番目のデータを
     *           Successor MembraneからSuccessor Stateへ設定し直す */
    mc_use_delta(f) ? vec_push(expanded_roots, data)
                    : vec_set(expanded_roots, i, data);
  }
  RC_ND_SET_ORG_SUCC_NUM(rc, mc_react_cxt_expanded_num(rc));
}



/* 膜memから, 性質ルールとシステムルールの同期積によって遷移可能な全状態(or遷移)をベクタに載せて返す */
void mc_gen_successors_with_property(State           *s,
                                     LmnMembrane     *mem,
                                     AutomataState   p_s,
                                     struct ReactCxt *rc,
                                     BOOL            f)
{
  unsigned int i, j;

  /** 状態展開:
   *   性質ルールが適用される場合, global_rootに対してシステムルール適用検査を行う.
   *   システムルール適用はglobal_rootのコピーに対して行われる.
   *   状態展開にexpandが使用された場合は, 現状態から遷移可能な全ての状態が生成されるのに対し，
   *   ampleが使用された場合はPartial Order Reductionに基づき，本当に必要な次状態のみが生成される．
   *   なお，適用可能なシステムルールが存在しない場合は，何も処理を行わない特殊なシステムルール(stutter extension rule)
   *   が存在するものと考えてε遷移をさせ，受理頂点に次状態が存在しない場合でも受理サイクルを形成できるようにする．
   *   (c.f. "The Spin Model Checker" pp.130-131)
   */

  for (i = 0; i < atmstate_transition_num(p_s); i++) {
    AutomataTransition p_t = atmstate_get_transition(p_s, i);
    if (eval_formula(mem, mc_data.propsyms, atm_transition_get_formula(p_t))) {
      BYTE p_nxt_l = atm_transition_next(p_t);
      vec_push(RC_EXPANDED_PROPS(rc), (vec_data_t)p_nxt_l);
    }
  }

  if (vec_num(RC_EXPANDED_PROPS(rc)) == 0) {
    return;
  } else {
    BYTE first_prop = (BYTE)vec_get(RC_EXPANDED_PROPS(rc), 0);
    mc_gen_successors(s, mem, first_prop, rc, f);
    if (mc_react_cxt_expanded_num(rc) == 0) {
      stutter_extension(s, mem, first_prop, rc, f);
    }
  }

  /* 階層グラフ構造は等価だが性質ラベルの異なる状態を生成する.　*/
  RC_ND_SET_ORG_SUCC_NUM(rc, mc_react_cxt_expanded_num(rc));
  for (i = 1; i < vec_num(RC_EXPANDED_PROPS(rc)); i++) {
    BYTE p_nxt_l = (BYTE)vec_get(RC_EXPANDED_PROPS(rc), i);
    for (j = 0; j < RC_ND_ORG_SUCC_NUM(rc); j++) {
      Transition src_succ_t;
      State *src_succ_s, *new_s;
      vec_data_t data;

      if (mc_has_trans(f)) {
        src_succ_t = (Transition)vec_get(RC_EXPANDED(rc), j);
        src_succ_s = transition_next_state(src_succ_t);
      } else {
        src_succ_t = NULL;
        src_succ_s = (State *)vec_get(RC_EXPANDED(rc), j);
      }

      new_s = state_copy(src_succ_s);
      state_set_parent(new_s, s);
      state_set_property_state(new_s, p_nxt_l);

      if (mc_has_trans(f)) {
        data = (vec_data_t)transition_make(new_s, transition_rule(src_succ_t, 0));
        set_trans_obj(s);
      } else {
        data = (vec_data_t)new_s;
      }
      vec_push(RC_EXPANDED(rc), data);

      /* 差分オブジェクトは状態展開時のみの一時データなので,
       * 効率化のためにポインタcopyのみにしている(deep copyしない)
       * !! 開放処理は要注意 (r435でdebug) !! */
      if (RC_MC_USE_DMEM(rc)) {
        vec_push(RC_MEM_DELTAS(rc), vec_get(RC_MEM_DELTAS(rc), j));
      }
    }
  }
}


/* 膜memとmemに対応する状態sを受け取り, コピーしてラベルnext_labelを持った状態をRC_EXPANDED(rc)へ積む */
inline static void stutter_extension(State           *s,
                                     LmnMembrane     *mem,
                                     BYTE            next_label,
                                     struct ReactCxt *rc,
                                     BOOL            f)
{
  vec_data_t data;
  State *new_s;

  if (mc_use_delta(f)) {
	/* stutter extensionによる遷移はself-loopになるか性質ラベルの変化のみであるため,
	 * struct LmnMembraneへの差分構造が存在しないstruct MemDeltaRootを登録する. */
    mc_react_cxt_add_mem_delta(rc, dmem_root_make(mem, NULL, 0), NULL);
    new_s = state_make_minimal();
  } else {
	/* 遷移先のstruct LmnMembraneへの差分構造を計算しない場合は, 遷移元状態sを階層グラフ構造ごとdeep copyする.
	 * ただし, struct Stateのメンバであるstruct Membraneはこの時点でNULLであるため, (NULLでない場合も任意のスレッドがNULLに書き換える可能性もある)
	 * mc_expandの手続きを始める際にバイト列から復元したmem (struct LmnMembrane)を渡す. */
    new_s = state_copy_with_mem(s, mem);
  }
  state_set_property_state(new_s, next_label);
  state_set_parent(new_s, s);

  if (mc_has_trans(f)) {
    data = (vec_data_t)transition_make(new_s, lmn_intern("ε"));
    set_trans_obj(s);
  } else {
    data = (vec_data_t)new_s;
  }
  vec_push(RC_EXPANDED(rc), data);
}


inline static void mc_gen_successors_inner(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
#ifdef PROFILE
  if(lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif

  mc_expand_inner(rc, cur_mem);

#ifdef PROFILE
  if(lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif
}




/** ===========================================
 *  === Counter Example generator / dumper ====
 *  ===========================================
 */
/*
static void mc_print_vec_states(FILE        *f,
                                Vector       *v,
                                State        *seed);
*/

/* エラーを示す頂点を登録する */
void mc_found_invalid_state(State *s)
{
  mc_data.error_exist = TRUE;

  if (s) {
    vec_push(mc_data.invalid_seeds[lmn_thread_id], (vec_data_t)s);
  }

  if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }
}


/* エラーを示すサイクルパスを載せたVectorを登録する. */
void mc_found_invalid_path(Vector *v)
{
  mc_data.error_exist = TRUE;

  if (v) {
    vec_push(mc_data.cycles[lmn_thread_id], (vec_data_t)v);
    MC_DEBUG(
      if (!mc_vec_states_valid(v)) {
        state_space_dumper(workerpool_get_my_worker()->states, stdout);
        mc_print_vec_states(stdout, v, NULL);
        lmn_fatal("unexpected.");
      }
    );
  }

  if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }
}


unsigned long mc_invalids_get_num()
{
  unsigned long ret;
  unsigned int i, n;

  ret = 0;
  n = lmn_env.core_num;
  for (i = 0; i < n; i++) {
    ret += vec_num(mc_data.invalid_seeds[i]);
  }

  return ret;
}


/* 初期頂点から状態seedに至るパスを載せたVectorをmallocして返す */
static Vector *mc_gen_invalids_path(State *seed)
{
  Vector *path;
  State  *pred;

  path = vec_make(32);
  pred = seed;

  while (pred) { /* 初期頂点のparentはNULL */
    vec_push(path, (vec_data_t)pred);
    pred = state_get_parent(pred);
  }

  vec_reverse(path);

  return path;
}

#define MC_INSERT_INVALIDS(G, S_KEY, SUCC)                               \
  do {                                                                   \
    Vector *succs;                                                       \
    st_data_t t;                                                         \
                                                                         \
    t = 0;                                                               \
    if (st_lookup(G, (st_data_t)(S_KEY), &t)) {                          \
      succs = (Vector *)t;                                               \
      if (!vec_contains(succs, (vec_data_t)SUCC)) {                      \
        vec_push(succs, (vec_data_t)SUCC);                               \
      }                                                                  \
    } else {                                                             \
      succs = vec_make(2);                                               \
      vec_push(succs, (vec_data_t)SUCC);                                 \
      st_insert(G, (st_data_t)(S_KEY), (st_data_t)succs);                \
    }                                                                    \
  } while (0)


/* Vectorに積まれた状態を, 遷移元の状態idをkeyに, 遷移先の集合をvalueとした
 * ハッシュ表に登録する */
static void mc_store_invalids_graph(st_table_t g, Vector *v)
{
  unsigned int i, j;

  for (i = 0, j = 1;
       i < vec_num(v) && j < vec_num(v);
       i++, j++) {

    State *s1, *s2;

    s1 = (State *)vec_get(v, i);
    s2 = (State *)vec_get(v, j);

    MC_INSERT_INVALIDS(g, s1, s2);
  }

  if (state_is_end((State *)vec_peek(v))) {
    State *s = (State *)vec_peek(v);
    MC_INSERT_INVALIDS(g, s, NULL);
  }
}


static int mc_dump_invalids_f(st_data_t _key, st_data_t _v, st_data_t _arg)
{
  State *s;
  Vector *succs;
  FILE *out;
  unsigned int i;

  s     = (State *)_key;
  succs = (Vector *)_v;
  out   = (FILE *)_arg;

  fprintf(out, "%lu::", state_format_id(s));
  for (i = 0; i < vec_num(succs); i++) {
    State *succ = (State *)vec_get(succs, i);
    if (succ) {
      MC_DEBUG(
        if (!state_succ_contains(s, succ)) {
          mc_print_vec_states(stderr, succs, NULL);
        }
      );
      fprintf(out, "%s%lu", (i > 0) ? ", " : ""
                          , state_format_id(succ));
    }
  }
  fprintf(out, "\n");

  return ST_CONTINUE;
}


int mc_free_succ_vec_f(st_data_t _key, st_data_t _v, st_data_t _arg)
{
  Vector *v = (Vector *)_v;
  vec_free(v);
  return ST_CONTINUE;
}


void mc_print_vec_states(FILE *f, Vector *v, State *seed)
{
  unsigned int i;
  for (i = 0; i < vec_num(v); i++) {
    State *s;

    if (lmn_env.sp_dump_format != LMN_SYNTAX) {
      char *m;
      s = (State *)vec_get(v, i);
      m = (s == seed) ? "*" : " ";
      fprintf(f, "%s%2lu::%s", m
                             , state_format_id(s)
                             , automata_state_name(mc_data.property_automata,
                                                   state_property_state(s)));
      state_print_mem(s, (LmnWord)f);
    }
    else {
      s = (State *)vec_get(v, i);
      fprintf(f, "path%lu_%s", state_format_id(s)
                             , automata_state_name(mc_data.property_automata,
                                                   state_property_state(s)));
      state_print_mem(s, (LmnWord)f);
      fprintf(f, ".\n");

      fprintf(f, "path%lu_%s", state_format_id(s)
                             , automata_state_name(mc_data.property_automata,
                                                   state_property_state(s)));
      state_print_mem(s, (LmnWord)f);
      fprintf(f, ":- ");
    }
  }
}



/* mc_dataに登録された反例を出力する.
 * mc_dataに登録された反例は頂点のみであり,
 * 登録された頂点が, atmstate_is_endの場合, safety
 * その他の場合は, その頂点を起点にしたサイクルパスにon_cycle_flagが立っている */
void mc_dump_all_errors(StateSpace ss, FILE *f)
{
  if (!mc_data.error_exist) {
    fprintf(f, "%s\n", lmn_env.mc_dump_format == CUI
                      ? "No Accepting Cycle (or Invalid State) exists."
                      : "");
  }
  else {
    switch (lmn_env.mc_dump_format) {
    case LaViT:
    case CUI:
    {
      st_table_t invalids_graph;
      unsigned int i, j, n;
      BOOL cui_dump;

      fprintf(f, "%s\n", lmn_env.sp_dump_format == LMN_SYNTAX
                         ? "counter_exapmle."
                         : "CounterExamplePaths");

      cui_dump = (lmn_env.mc_dump_format == CUI);
      invalids_graph = cui_dump ? NULL : st_init_ptrtable();
      n = lmn_env.core_num;

      /* state property */
      for (i = 0; i < n; i++) {
        Vector *v = mc_data.invalid_seeds[i];
        for (j = 0; j < vec_num(v); j++) {
          Vector *path = mc_gen_invalids_path((State *)vec_get(v, j));

          if (cui_dump) { /* 出力 */
            mc_print_vec_states(f, path, NULL);
            fprintf(f, "\n");
          } else {    /* ハッシュ表に追加 */
            mc_store_invalids_graph(invalids_graph, path);
          }
          vec_free(path);
        }
      }

      /* path property */
      for (i = 0; i < n; i++) {
        Vector *v = mc_data.cycles[i];
        for (j = 0; j < vec_num(v); j++) {
          Vector *cycle, *path;
          State *seed;

          cycle = (Vector *)vec_get(v, j);
          seed  = (State *)vec_get(cycle, 0);
          path  = mc_gen_invalids_path(seed);

          vec_push(cycle, (vec_data_t)seed); /* seed to seedのパスを取得するため */

          if (cui_dump){
            vec_pop(path); /* cycle Vectorとpath Vectorでseedが重複して積まれているため */
            mc_print_vec_states(f, path, NULL);
            mc_print_vec_states(f, cycle, seed);
            fprintf(f, "\n");
          } else {
            LMN_ASSERT(invalids_graph);
            mc_store_invalids_graph(invalids_graph, path);
            mc_store_invalids_graph(invalids_graph, cycle);
          }

          vec_free(path);
        }
      }

      if (!cui_dump) {
        st_foreach(invalids_graph, mc_dump_invalids_f, (st_data_t)f);
        st_foreach(invalids_graph, mc_free_succ_vec_f, (st_data_t)0);
        st_free_table(invalids_graph);
      }

      break;
    }

    case Dir_DOT: /* 反例パスをサブグラフとして指定させたい */
    case FSM: /* 未定. ltsviewはdotも読み出せるので廃止しても良いかも */
      fprintf(f, "under constructions..\n");
      break;
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


/* 性質を表すBuchiオートマトン, 命題記号定義(ルール左辺)の集合を, それぞれa, prop_defsに読み込む.
 * 成功ならば0, そうでないならば0以外を返す */
int mc_load_property(Automata *a, PVector *prop_defs)
{
  FILE *nc_fp, *prop_fp;
  int r;

  *a = NULL;
  *prop_defs = NULL;
  nc_fp = prop_fp = NULL;

  if (lmn_env.ltl_exp) {
    nc_fp = ltl2ba_str(lmn_env.ltl_exp);
  } else {
    if (!lmn_env.automata_file) goto NC_ENV;
    if (!(nc_fp = fopen(lmn_env.automata_file, "r"))) goto NC_OPEN_ERROR;
  }
  if (never_claim_load(nc_fp, a)) goto NC_LOAD_ERROR;

  if (!lmn_env.propositional_symbol) goto PROP_ENV;
  if (!(prop_fp = fopen(lmn_env.propositional_symbol, "r"))) goto PROP_OPEN_ERROR;
  if (propsym_load_file(prop_fp, *a, prop_defs)) goto PROP_LOAD_ERROR;

  r = 0;
  goto RET;

NC_ENV:        r = MC_ERR_NC_ENV;    goto FINALLY;
PROP_ENV:      r = MC_ERR_PROP_ENV;  goto FINALLY;
NC_OPEN_ERROR: r = MC_NC_OPEN_ERROR; goto FINALLY;
NC_LOAD_ERROR:
  {
    char c;
    rewind(nc_fp);
    while ((c = fgetc(nc_fp)) != EOF) {
      fputc(c, stderr);
    }
    r = MC_NC_LOAD_ERROR;
    goto FINALLY;
  }
PROP_OPEN_ERROR: r = MC_PROP_OPEN_ERROR; goto FINALLY;
PROP_LOAD_ERROR: r = MC_PROP_LOAD_ERROR; goto FINALLY;

FINALLY:
  LMN_FREE(*a);
  LMN_FREE(*prop_defs);

RET:
  if (prop_fp) fclose(prop_fp);
  if (nc_fp) fclose(nc_fp);
  return r;
}

void mc_explain_error(int error_id)
{
  lmn_report(mc_error_msg(error_id));
}

char *mc_error_msg(int error_id)
{
  switch (error_id) {
  case MC_ERR_NC_ENV:
    return "specify never claim file";
  case MC_ERR_PROP_ENV:
    return "specify propositional symbol definition file";
  case MC_NC_OPEN_ERROR:
    return "cannot open never claim file";
  case  MC_NC_LOAD_ERROR:
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
