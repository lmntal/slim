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
#include "nd.h"
#include "por.h"
#include "task.h"
#include "mhash.h"
#include "propositional_symbol.h"
#include "ltl2ba_adapter.h"
#include "error.h"
#include "dumper.h"
#include "mem_encode.h"
#include "state.h"
#include "runtime_status.h"

#include <string.h>

enum MC_ERRORNO {
  MC_ERR_NC_ENV,
  MC_ERR_PROP_ENV,
  MC_NC_OPEN_ERROR,
  MC_NC_LOAD_ERROR,
  MC_PROP_OPEN_ERROR,
  MC_PROP_LOAD_ERROR,
};

static void violate(Vector *stack);
static void exit_ltl_model_checking(void);
static void stutter_extension(StateSpace states, State *s, BYTE next_label);
static Vector *mc_expand(const StateSpace states,
                         State *state,
                         BYTE prop_state);
static void do_mc(StateSpace states, LmnMembrane *world_mem);
static void ltl_search2(StateSpace states, Vector *stack, State *seed);
static int print_state_name(st_data_t _k, st_data_t state_ptr, st_data_t _a);
static void dump_state_name(StateSpace states, FILE *file);

/**
 * コンストラクタ
 */
StateTransition *strans_make(State *s, unsigned long id, LmnRule rule) {
  StateTransition *strans = LMN_MALLOC(StateTransition);
  strans->succ_state = s;
  strans->id = id;
  strans->rule = rule;
  return strans;
}

void strans_free(StateTransition *strans) {
  LMN_FREE(strans);
}
/**
 * 膜スタックの代替品
 * 自身を含めた全ての先祖膜を起こす
 */
inline void activate_ancestors(LmnMembrane *mem) {
  LmnMembrane *cur;
  for (cur=mem; cur; cur=cur->parent) {
    lmn_mem_set_active(mem, TRUE);
  }
}

/* 成功ならば0, そうでないならば0以外を返す */
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

NC_ENV: r = MC_ERR_NC_ENV; goto FINALLY;
PROP_ENV: r = MC_ERR_PROP_ENV; goto FINALLY;
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

/*----------------------------------------------------------------------
 * LTL Model Checking
 */

/* 状態sからstutter extensionを行う */
static void stutter_extension(StateSpace states, State *s, BYTE next_label)
{
  /* state_space_insertはコストが高いため, 先にラベル比較でself-loopを検出する */
  if (state_property_state(s) == next_label) {
    /** self-loop
     */
    state_succ_add(s, transition_make(s, ANONYMOUS));
  }
  else {
    /** extension
     */
    State *new_s, *t;
    new_s = state_make(lmn_mem_copy(state_mem(s)), next_label, dummy_rule());
    t = state_space_insert(states, new_s);

    /* TODO: stutter rule name */
    if (t == new_s) {
      state_succ_add(s, transition_make(new_s, ANONYMOUS));
    }
    else {
      state_free(new_s);
      state_succ_add(s, transition_make(t, ANONYMOUS));
    }
  }
}


/* nested(またはdouble)DFSにおける1段階目の実行 */
void ltl_search1(StateSpace states, Vector *stack)
{
  unsigned int i, i_trans, n, n_trans;
  State *s;
  AutomataState prop_atm_s;

  s = (State *)vec_peek(stack);
  prop_atm_s = automata_get_state(mc_data.property_automata, state_property_state(s));

  if (atmstate_is_end(prop_atm_s)) {
    violate(stack);
    unset_open((State *)vec_peek(stack));
    unset_fst((State *)vec_pop(stack));
    return;
  }

  if (!is_expanded(s)) {
    set_expanded(s);

    /** 状態展開:
     *   性質ルールが適用される場合, global_rootに対してシステムルール適用検査を行う.
     *   システムルール適用はglobal_rootのコピーに対して行われる.
     *   状態展開にexpandが使用された場合は, 現状態から遷移可能な全ての状態が生成されるのに対し，
     *   ampleが使用された場合はPartial Order Reductionに基づき，本当に必要な次状態のみが生成される．
     *   なお，適用可能なシステムルールが存在しない場合は，何も処理を行わない特殊なシステムルール(stutter extension rule)
     *   が存在するものと考えてε遷移をさせ，受理頂点に次状態が存在しない場合でも受理サイクルを形成できるようにする．
     *   (c.f. "The Spin Model Checker" pp.130-131)
     */
    n_trans = atmstate_transition_num(prop_atm_s);
    for (i_trans = 0; i_trans < n_trans; i_trans++) {
      unsigned int tmp_succ_num = 0;
      AutomataTransition prop_t = atmstate_get_transition(prop_atm_s, i_trans);

      /* EFFICIENCY: 状態のコピーを（mem_idやmem_dumpを使い）効率的に行える */
      if (eval_formula(state_mem(s), mc_data.propsyms, atm_transition_get_formula(prop_t))) {
        BYTE prop_next_label;
        Vector *new_states;

        /* TODO: mc_expandの仕様とstutter extensionに必要な仕様がミスマッチ */
        prop_next_label = atm_transition_next(prop_t);
        new_states = mc_expand(states, s, prop_next_label);

        n = state_succ_num(s);
        if ((n - tmp_succ_num) == 0) {
          stutter_extension(states, s, prop_next_label);
        }
        tmp_succ_num += n;
        vec_free(new_states);
      }
    }

    /** recursive: ltl_search1 */
    /* TODO: recursiveからloopへ */
    n = state_succ_num(s);
    for (i = 0; i < n; i++) {
      State *succ = state_succ_get(s, i);
      if (!is_expanded(succ)) {
        vec_push(stack, (LmnWord)succ);
        set_open(succ); /* 状態ssがスタック上に存在する旨のフラグを立てる(POR用) */
        set_fst(succ);
        ltl_search1(states, stack);
      }
    }
  }

  /** entering second DFS
   * (この時点で, sおよびsから到達可能な状態は全て展開済み) */
  if (atmstate_is_accept(prop_atm_s) && !is_snd(s)) {
    set_snd(s);
    if (lmn_env.profile_level >= 2) status_start_dfs2();
    ltl_search2(states, stack, s);
    if (lmn_env.profile_level >= 2) status_finish_dfs2();
  }

  unset_open((State *)vec_peek(stack)); /* スタックから取り除かれる旨のフラグをセット(POR用) */
  unset_fst((State *)vec_peek(stack));
  vec_pop(stack);
}


/**
 * 2段階目のDFS:
 *   1段階目のDFSで展開済みとなった受理頂点から, 自身に戻る閉路(受理サイクル)を探索する.
 *   本DFS中に未展開の状態を訪問することは想定されていない.
 */
static void ltl_search2(StateSpace states, Vector *stack, State *seed)
{
  unsigned int i, n;
  State *s = (State *)vec_peek(stack);

  n = state_succ_num(s);
  for(i = 0; i < n; i++) { /* for each (s,l,s') */
    State *ss = state_succ_get(s, i);

    /** 受理サイクルの条件:
     * 1. successorがseedと同一の状態
     * 2. successorが探索スタック上の状態
     */
    if((ss == seed) || (is_fst(ss) && !is_snd(ss))) {
      set_snd(ss);
      violate(stack);
      return;
    }

    /* second DFS で未訪問→再帰 */
    /* TODO: recursiveからloopへ */
    if(!is_snd(ss)) {
      set_snd(ss);
      vec_push(stack, (LmnWord)ss);
      ltl_search2(states, stack, seed);
      vec_pop(stack);
    }
  }
}

static void violate(Vector *stack)
{
  unsigned int i;
  if (lmn_env.dump) {
    fprintf(stdout, "cycle(or error) found:\n");
    /* 結果出力 */
    for (i = 0; i < vec_num(stack); i++) {
      State *tmp_s = (State *)vec_get(stack, i);
      if (is_snd(tmp_s)) printf("*");
      else printf(" ");
      printf("%d (%s): %s: ",
             i,
             lmn_id_to_name(lmn_rule_get_name(state_rule(tmp_s))),
             automata_state_name(mc_data.property_automata,
                                 state_property_state(tmp_s)));
      if (lmn_env.ltl_nd){
    	  fprintf(stdout, "%lu\n", ((State *)vec_get(stack, i))->id);
      }else{
    	  lmn_dump_mem_stdout(((State *)vec_get(stack, i))->mem);
      }
    }
    fprintf(stdout, "\n");
  }
  if (!lmn_env.ltl_all) { /* 一つエラーが見つかったら終了する */
    exit_ltl_model_checking();
  }
#ifdef PROFILE
  status_count_counterexample();
#endif
}

static void exit_ltl_model_checking()
{
  exit(0);
}

static Vector *mc_expand(const StateSpace states,
                         State *state,
                         BYTE prop_state)
{
  Vector *r;

  if (lmn_env.por) r = ample(states, state);
  else r = nd_expand(states, state, prop_state);

  return r;
}

/* モデル検査を行う。automataは性質のオートマトン、propsymsは性質のシンボル */
//void run_mc(LmnRuleSet start_ruleset, Automata automata, Vector *propsyms)
void run_mc(Vector *start_rulesets, Automata automata, Vector *propsyms)
{
  LmnMembrane *mem;
  StateSpace states;

  states = state_space_make();
  init_por_vars();

  mc_data.property_automata = automata;
  mc_data.propsyms = propsyms;

  /* -- ここから -- TODO シミュレーション実行と重複した準備処理 */

  /* make global root membrane */
  mem = lmn_mem_make();
  /* 初期構造の生成 */
  react_start_rulesets(mem, start_rulesets);
  activate_ancestors(mem);

  do_mc(states, mem);

  /* finalize */
  state_space_free(states);
  free_por_vars();
}

static void do_mc(StateSpace states, LmnMembrane *world_mem)
{
  State *initial_state;
  Vector *stack;

  /* 初期プロセスから得られる初期状態を生成 */
  initial_state = state_make(world_mem,
                             automata_get_init_state(mc_data.property_automata),
                             dummy_rule());
  state_space_set_init_state(states, initial_state);

  stack = vec_make(1024);
  vec_push(stack, (LmnWord)initial_state);

  if (lmn_env.profile_level > 0) {
    status_nd_start_running();
  }

  /* LTLモデル検査 */
  set_fst(initial_state);
  ltl_search1(states, stack);

  if (lmn_env.profile_level > 0) {
    status_nd_finish_running();
    status_state_space(states);
  }

  if (lmn_env.dump) {
    fprintf(stdout, "no cycles found\n");
    fprintf(stdout, "\n");

    if (lmn_env.ltl_nd){
      dump_all_state_mem(states, stdout);
      dump_state_transition_graph(states, stdout);
      dump_state_name(states, stdout);
    }
    fprintf(stdout, "# of States = %lu\n", state_space_num(states));
  }

  vec_free(stack);
}

static int print_state_name(st_data_t _k, st_data_t state_ptr, st_data_t _a)
{
  State *tmp = (State *)state_ptr;
  fprintf(stdout, "%lu::%s\n", tmp->id, automata_state_name(mc_data.property_automata, tmp->state_name) );
  return ST_CONTINUE;
}

/**
 * --ltl_nd時に使用．状態の名前（accept_s0など）を表示．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
static void dump_state_name(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "Labels\n");
  st_foreach(state_space_tbl(states), print_state_name, 0);
  fprintf(file, "\n");
}
