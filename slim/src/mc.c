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
#ifdef PROFILE
#include "runtime_status.h"
#endif

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


/* nested(またはdouble)DFSにおける1段階目の実行 */
void ltl_search1(StateSpace states, Vector *stack)
{
  unsigned int i, i_trans;
  State *s = (State *)vec_peek(stack);
  AutomataState property_automata_state = automata_get_state(mc_data.property_automata, s->state_name);

/*   printf("ltl search1, state: %p\n", s); */
  if (atmstate_is_end(property_automata_state)) {
    violate(stack);
    unset_open((State *)vec_peek(stack));
    unset_fst((State *)vec_pop(stack));
    return;
  }

    if (is_expanded(s)) {
      /* 状態が展開済みである場合，スタック上から除去してフラグを解除する */
      vec_pop(stack);
      unset_open(s);
      return;
    }
    set_expanded(s); /* sに展開済みフラグを立てる */

  /*
   * 状態展開
   */
  for (i_trans = 0;
       i_trans < atmstate_transition_num(property_automata_state);
       i_trans++) {
    AutomataTransition transition = atmstate_get_transition(property_automata_state,
                                                            i_trans);

    /**
     * 性質ルールの適用
     */
    if (eval_formula(state_mem(s),
                     mc_data.propsyms,
                     transition_get_formula(transition))) {
      /**
       * global_rootに対してシステムルール適用検査を行う．
       * システムルール適用はglobal_rootのコピーに対して行い，
       * 展開された"次の状態"はすべてexpandedに放り込まれる．
       * 状態展開にexpandが使用された場合は現状態から遷移可能なすべての状態が生成されるのに対し，
       * ampleが使用された場合はPartial Order Reductionに基づき，本当に必要な次状態のみが生成される．
       * なお，適用可能なシステムルールが存在しない場合は，何も処理を行わない特殊なシステムルール(stutter extension rule)
       * が存在するものと考えてε遷移をさせ，受理頂点に次状態が存在しない場合でも受理サイクルを形成できるようにする．
       * (c.f. "The Spin Model Checker" pp.130-131)
       */
      Vector *new_states = mc_expand(states, s, transition_next(transition));

      if (state_succ_num(s) == 0) { /* stutter extension */
        State *new_s, *t;

        /* 性質ルールのみが適用されている。ルール名(遷移のラベル)はどうする？ */
        /* EFFICIENCY: 状態のコピーを（mem_idやmem_dumpを使い）効率的に行える */
        new_s = state_make(lmn_mem_copy(state_mem(s)),
                           transition_next(transition),
                           dummy_rule());
        t = state_space_insert(states, new_s);
        if (t == new_s) {
          /* 状態空間に追加された */
          state_succ_add(s, new_s);
          vec_push(new_states, (vec_data_t)new_s);
        } else {
          /* 状態空間に追加されなかった（等価な状態が既にあった） */
          state_free(new_s);
          state_succ_add(s, t);
        }
      }

      for (i = 0; i < state_succ_num(s); i++) {
        State *succ = state_succ_get(s, i);

        /* push とset を１つの関数にする */
        vec_push(stack, (vec_data_t)succ);
        set_open(succ); /* 状態ssがスタック上に存在する旨のフラグを立てる(POR用) */
        set_fst(succ);
        ltl_search1(states, stack);
      }

      vec_free(new_states);
    }
  }


  /* entering second DFS */
  if (atmstate_is_accept(property_automata_state)) {
    set_snd(s);
    ltl_search2(states, stack, s);
  }

#ifdef DEBUG
  fprintf(stdout, "+++++ return function: ltl_search1() +++++\n\n");
#endif

  unset_open((State *)vec_peek(stack)); /* スタックから取り除かれる旨のフラグをセット(POR用) */
  unset_fst((State *)vec_pop(stack));
}

/*
 *  2段階目のDFS
 *  ltl_search1()で発見した受理頂点や、本メソッド実行中に新たに発見した受理頂点の集合へ
 *  ある適当な受理頂点から到達可能であるかどうかを判定する。
 *  すなわち受理頂点から受理頂点へ至る閉路(受理サイクル)の存在を確認することで、
 *  与えられたLTL式を満足する実行経路が存在するか否かを判定する。
 */
static void ltl_search2(StateSpace states, Vector *stack, State *seed)
{
  unsigned int i;
  State *s = (State *)vec_peek(stack);

#ifdef DEBUG
  fprintf(stdout, "\n----- enter function: ltl_search2() -----\n");
  fprintf(stdout, "seed=");
  lmn_dump_mem_stdout(seed->mem);
  fprintf(stdout, "\n");

  fprintf(stdout, "stack:\n");
  for(i = vec_num(stack) - 1; i >= 0; i--) {
    State *tmp_s = (State *)vec_get(stack, i);
    fprintf(stdout, "%d\n", is_snd(tmp_s));
    fprintf(stdout, "%d: (fst=%d,snd=%d):\t", i, is_fst(tmp_s) ? 1 : 0, is_snd(tmp_s) ? 1 : 0);
    lmn_dump_mem_stdout(tmp_s->mem);
  }
  fprintf(stdout, "\n");
#endif


  for(i = 0; i < state_succ_num(s); i++) { /* for each (s,l,s') */
    State *ss = state_succ_get(s, i);

    /* successorがseedと同一の状態 or successorが探索スタック上の状態ならば反例を出力 */
    if(ss == seed || (is_fst(ss) && !is_snd(ss))) {
      set_snd(ss);
      violate(stack);
#ifdef DEBUG
      fprintf(stdout, "+++++ return function: ltl_search2() +++++\n\n");
#endif
      return;
    }

    /* second DFS で未訪問→再帰 */
    if(!is_snd(ss)) {
      set_snd(ss);
/*      set_open(ss); */
      vec_push(stack, (LmnWord)ss);
      ltl_search2(states, stack, seed);

/*      unset_open((State *)vec_pop(stack)); */
      vec_pop(stack);
    }
  }
}

static void violate(Vector *stack)
{
  unsigned int i;
  fprintf(stdout, "cycle(or error) found:\n");
  if (lmn_env.dump) {
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
    	  fprintf(stdout, "%lu\n", vec_get(stack, i));
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
/*   if (lmn_env.ltl_nd) { */
/*     dump_state_transition_graph(init_state, stdout); */
/*   } */
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
  { /* 初期構造の生成 */
    {
      int temp_env_p = lmn_env.profile_level;
      lmn_env.profile_level = 0;
      react_start_rulesets(mem, start_rulesets);
      lmn_env.profile_level = temp_env_p;
    }
  }

  activate_ancestors(mem);

  do_mc(states, mem);

#ifdef PROFILE
  calc_hash_conflict(states);
#endif

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


  /* LTLモデル検査 */
  set_fst(initial_state);
  ltl_search1(states, stack);
  fprintf(stdout, "no cycles found\n");
  fprintf(stdout, "\n");

  if (lmn_env.ltl_nd){
    dump_all_state_mem(states, stdout);
    dump_state_transition_graph(states, stdout);
    dump_state_name(states, stdout);
  }
  fprintf(stdout, "# of States = %lu\n", state_space_num(states));

#ifdef PROFILE
  status_set_state_num(state_space_num(states));
#endif

  vec_free(stack);
}

static int print_state_name(st_data_t _k, st_data_t state_ptr, st_data_t _a)
{
  State *tmp = (State *)state_ptr;
  fprintf(stdout, "%lu::%s\n", (long unsigned int)tmp, automata_state_name(mc_data.property_automata, tmp->state_name) );
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
