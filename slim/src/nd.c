/*
 * nd.c
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

#include "lmntal.h"
#include "nd.h"
#include "por.h"
#include "task.h"
#include "dumper.h"
#include "error.h"
#ifdef PROFILE
#include "runtime_status.h"
#endif

struct StateSpace {
  State *init_state;
  Vector *end_states;
  st_table_t tbl;
};

static Vector *expand_sub(struct ReactCxt *rc, LmnMembrane *cur_mem);
static BOOL react_all_rulesets(struct ReactCxt *rc,
                               LmnMembrane *cur_mem);
static void nd_loop(StateSpace states, State *init_state);
static int kill_States_chains(st_data_t _k,
                              st_data_t state_ptr,
                              st_data_t rm_tbl_ptr);
static BOOL expand_inner(struct ReactCxt *rc,
                         LmnMembrane *cur_mem);
static void dump_state_data(State *state);

static int print_state_mem(st_data_t _k, st_data_t state_ptr, st_data_t _a);
static int print_state_transition_graph(st_data_t _k, st_data_t state_ptr, st_data_t _a);
static int print_state_name(st_data_t _k, st_data_t state_ptr, st_data_t _a);

/* 状態stateから１ステップで遷移する状態のベクタを返す。
   返される状態のベクタには、重複はない */
Vector *nd_expand(const StateSpace states, State *state)
{
  Vector *r;
#ifdef PROFILE
  if (lmn_env.profile_level>0)  status_start_expand();
#endif

  if (lmn_env.por) { r = ample(states, state); }
  else  {
    struct ReactCxt rc;
    LmnMembrane *mem;

    mem = state_mem(state);
    if (!mem) {
      mem = lmn_binstr_decode(state_mem_id(state));
    }
    nd_react_cxt_init(&rc, DEFAULT_STATE_ID);
    RC_SET_GROOT_MEM(&rc, mem);
    r = expand_sub(&rc, mem);
    nd_react_cxt_destroy(&rc);

    if (!state_mem(state)) {
      lmn_mem_drop(mem);
      lmn_mem_free(mem);
    }
  }

#ifdef PROFILE
  if (lmn_env.profile_level>0) status_finish_expand();
#endif

  return r;
}

static Vector *expand_sub(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
  int i;
  Vector *expanded_roots;
  Vector *expanded;
  st_table *s;

  expand_inner(rc, cur_mem);
  expanded_roots = RC_EXPANDED(rc);
  s = st_init_table(&type_statehash);
  expanded = vec_make(32);
  for (i = 0; i < vec_num(expanded_roots); i++) {
    State *state;
    st_data_t t;

    state = state_make_for_nd((LmnMembrane *)vec_get(expanded_roots, i),
                              (LmnRule)vec_get(RC_EXPANDED_RULES(rc), i));

    if (st_lookup(s, (st_data_t)state, &t)) {
      state_free(state);
    } else {
      st_insert(s, (st_data_t)state, (st_data_t)state);
      vec_push(expanded, (LmnWord)state);
    }
  }

  st_free_table(s);
  return expanded;
}

/*
 * 状態を展開する
 * 引数として与えられた膜と、その膜に所属する全てのアクティブ膜に対して
 * ルール適用検査を行う
 * 子膜からルール適用を行っていく
 */
/* must_be_activatedはこの膜がstableである可能性がなくなったときにTRUEを代入する
 * 子膜と自身のルール適応テストが終了したときFALSEならこの膜はstableである
 */
static BOOL expand_inner(struct ReactCxt *rc,
                         LmnMembrane *cur_mem)
{
  BOOL ret_flag = FALSE;

  for (; cur_mem; cur_mem = cur_mem->next) {
    unsigned long org_num = vec_num(RC_EXPANDED(rc));

    /* 代表子膜に対して再帰する */
    if (expand_inner(rc, cur_mem->child_head)) {
      ret_flag = TRUE;
    }

    if (lmn_mem_is_active(cur_mem) && react_all_rulesets(rc, cur_mem)) {
      ret_flag = TRUE;
    }

    if (org_num == vec_num(RC_EXPANDED(rc))) {
      /* 状態が一つも生成されなかった */
      lmn_mem_set_active(cur_mem, FALSE);
    }
  }
  return ret_flag;
}

/*
 * nondeterministic execution
 * 深さ優先で全実行経路を取得する
 */
static void nd_loop(StateSpace states, State *init_state) {
  Vector *stack;
  unsigned long i;

  stack = vec_make(2048);
  vec_push(stack, (LmnWord)init_state);

  while (vec_num(stack) != 0) {
    /* 展開元の状態。popはしないでsuccessorの展開が終わるまで
       スタックに積んでおく */
    State *s = (State *)vec_peek(stack);
    Vector *expanded;
    unsigned long expanded_num;

    if (is_expanded(s)) {
      /* 状態が展開済みである場合，スタック上から除去してフラグを解除する */
      vec_pop(stack);
      unset_open(s);
      continue;
    }

    expanded = nd_expand(states, s); /* 展開先をexpandedに格納する */
    expanded_num = vec_num(expanded);

    if (expanded_num == 0) {
      state_space_add_end_state(states, s);
    }

    state_succ_init(s, vec_num(expanded));
    for (i = 0; i < expanded_num; i++) {
      State *src_succ = (State *)vec_get(expanded, i);
      State *succ = insert_state(states, src_succ);

      if (succ == src_succ) { /* succが状態空間に追加された */
        vec_push(stack, (LmnWord)src_succ);
        /* set ss to open, i.e., on the search stack */
        set_open(src_succ);
        dump_state_data(succ);

        if (lmn_env.mem_enc_optmem) {
          /* メモリ最適化のために、サクセッサの作成後に膜を解放する */
          state_free_mem(succ);
        }
      } else { /* src_succは追加されなかった（すでに同じ状態がある) */
        state_free(src_succ);
      }
      vec_push(&s->successor, (LmnWord)succ);
    }
    vec_free(expanded);
    set_expanded(s); /* sに展開済みフラグを立てる */

    /* 膜のエンコードを行っている場合は、展開済みになった状態の膜を解放する */
    if (lmn_env.mem_enc) {
      state_free_mem(s);
    }
  }

  vec_free(stack);
}

/* 非決定実行を行う */
void run_nd(Vector *start_rulesets)
{
  LmnMembrane *mem;
  struct ReactCxt init_rc;
  StateSpace states;

  /**
   * initialize containers
   */
  init_por_vars();

  stand_alone_react_cxt_init(&init_rc);

  /* make global root membrane */
  mem = lmn_mem_make();
  RC_SET_GROOT_MEM(&init_rc, mem);

  {
    int temp_env_p = lmn_env.profile_level;
    lmn_env.profile_level = 0;
    react_start_rulesets(&init_rc, mem, start_rulesets);
    lmn_react_systemruleset(&init_rc, mem);
    lmn_env.profile_level = temp_env_p;
  }
  stand_alone_react_cxt_destroy(&init_rc);
  activate_ancestors(mem);

  if (lmn_env.dump) {
    fprintf(stdout, "States\n");
  }

  states = do_nd(mem);
#ifdef PROFILE
  calc_hash_conflict(states);
  calc_encode_info(states);
  status_set_state_num(state_space_num(states));
#endif
  fprintf(stdout, "\n");

  dump_state_transition_graph(states, stdout);
  fprintf(stdout, "# of States = %lu\n", state_space_num(states));

  lmn_mem_drop(mem);
  lmn_mem_free(mem);
  state_space_free(states);

  /* finalize */
  free_por_vars();
}

StateSpace do_nd(LmnMembrane *world_mem_org)
{
  State *initial_state;
  StateSpace states = state_space_make();
  LmnMembrane *world_mem = lmn_mem_copy(world_mem_org);

  /* 初期プロセスから得られる初期状態を生成 */
  initial_state = state_make_for_nd(world_mem, ANONYMOUS);
/*   mc_flags.initial_state = initial_state; */
  state_space_set_init_state(states, initial_state);
  dump_state_data(initial_state);

/*   /\* --nd_dumpの実行 *\/ */
/*   else if(lmn_env.nd_dump){ */
/*     nd_dump_exec(); */
/*   } */
  /* --ndの実行（非決定実行後に状態遷移グラフを出力する） */
/*   else{ */
  nd_loop(states, initial_state);
/*   } */

  return states;
}

static void dump_state_data(State *state)
{
  if (!lmn_env.dump) return;
  fprintf(stdout, "%lu::", (long unsigned int)state); /* dump src state's ID */
  if (!state->mem) lmn_fatal("unexpected");
  lmn_dump_cell_stdout(state->mem); /* dump src state's global root membrane */
}

/**
 * --ltl_nd実行終了時に状態の内容を出力する．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
static int print_state_mem(st_data_t _k, st_data_t state_ptr, st_data_t _a) {
  State *tmp = (State *)state_ptr;
  dump_state_data(tmp);
  return ST_CONTINUE;
}

/**
 * 非決定(--nd または --nd_result)実行終了時に状態遷移グラフを出力する．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
static int print_state_transition_graph(st_data_t _k, st_data_t state_ptr, st_data_t _a) {
  unsigned int j = 0;
  State *tmp = (State *)state_ptr;

  fprintf(stdout, "%lu::", (long unsigned int)tmp); /* dump src state's ID */
  while (j < vec_num(&tmp->successor)) { /* dump dst state's IDs */
    fprintf(stdout, "%lu", vec_get(&tmp->successor, j++));
    if (j < vec_num(&tmp->successor)) {
      fprintf(stdout,",");
    }
  }
  fprintf(stdout, "\n");

  return ST_CONTINUE;
}

static int print_state_name(st_data_t _k, st_data_t state_ptr, st_data_t _a) {
  State *tmp = (State *)state_ptr;
  fprintf(stdout, "%lu::%s\n", (long unsigned int)tmp, automata_state_name(mc_data.property_automata, tmp->state_name) );
  return ST_CONTINUE;
}

/* cur_memと、その子孫膜に存在するルールすべてに対して適用を試みる */
static BOOL react_all_rulesets(struct ReactCxt *rc,
                               LmnMembrane *cur_mem)
{
  unsigned int i;
  struct Vector rulesets = cur_mem->rulesets; /* 本膜のルールセットの集合 */
  BOOL ok = FALSE;

  for (i = 0; i < vec_num(&rulesets); i++) {
    ok = ok || lmn_react_ruleset(rc, cur_mem,
                                 (LmnRuleSet)vec_get(&rulesets, i));
  }
#ifdef OLD
  if (!ok) { /* 通常のルールセットが適用できなかった場合 */
    /* システムルールセットの適用 */
    if (lmn_react_ruleset(rc, cur_mem, system_ruleset)) {
      ok = TRUE;
    }
  }
#endif
  return ok;
}

StateSpace state_space_make()
{
  struct StateSpace *ss = LMN_MALLOC(struct StateSpace);
  ss->init_state = NULL;
  ss->tbl = st_init_table(&type_statehash);
  ss->end_states = vec_make(64);
  return ss;
}

void state_space_free(StateSpace states)
{
  HashSet rm_tbl; /* LTLモデル検査モード時に二重解放を防止するため */

  hashset_init(&rm_tbl, 16);
  st_foreach(states->tbl, kill_States_chains, (st_data_t)&rm_tbl);
  hashset_destroy(&rm_tbl);
  st_free_table(states->tbl);
  vec_free(states->end_states);
  LMN_FREE(states);
}

void state_space_set_init_state(StateSpace states, State* init_state)
{
  states->init_state = init_state;
  st_insert(states->tbl, (st_data_t)init_state, (st_data_t)init_state);
}

State *state_space_init_state(StateSpace states)
{
  return states->init_state;
}

unsigned long state_space_num(StateSpace states)
{
  return st_num(states->tbl);
}

/* 状態空間にすでに含まれている状態sを最終状態として登録する */
void state_space_add_end_state(StateSpace states, State *s)
{
  vec_push(states->end_states, (LmnWord)s);
}

const Vector *state_space_end_states(StateSpace states)
{
  return states->end_states;
}

/**
 * 非決定実行 or LTLモデル検査終了後にStates内に存在するチェインをすべてfreeする
 * 高階関数st_foreach(c.f. st.c)に投げて使用
 */
static int kill_States_chains(st_data_t _k, st_data_t state_ptr, st_data_t rm_tbl_ptr)
{
  State *tmp = (State *)state_ptr;
  HashSet *rm_tbl = (HashSet *)rm_tbl_ptr;

  if(tmp->mem && hashset_contains(rm_tbl, (HashKeyType)tmp->mem)) {
    vec_destroy(&tmp->successor);
    LMN_FREE(tmp);
  }
  else {
    hashset_add(rm_tbl, (HashKeyType)tmp->mem);
    state_free(tmp);
  }
  return ST_CONTINUE;
}

State *insert_state(StateSpace states, State *s)
{
  State *t;
  if (st_lookup(states->tbl, (st_data_t)s, (st_data_t *)&t)) {
    return t;
  } else {
    st_add_direct(states->tbl, (st_data_t)s, (st_data_t)s); /* 状態空間に追加 */
    return s;
  }
}

/* 状態空間内のsと等価な状態を返す。存在しない場合は、NULLを返す */
State *state_space_get(const StateSpace states, State *s)
{
  st_data_t t;
  if (st_lookup(states->tbl, (st_data_t)s, &t)) {
    return (State *)t;
  } else {
    return NULL;
  }
}

/* 状態空間から状態sを削除する。ただし、状態空間のサクセッサは更新され
   ないので注意 */
void state_space_remove(const StateSpace states, State *s)
{
  st_delete(states->tbl, (st_data_t)s, 0);
}

st_table_t state_space_tbl(StateSpace states)
{
  return states->tbl;
}


void dump_all_state_mem(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "States\n");
  st_foreach(states->tbl, print_state_mem, 0);
  fprintf(file, "\n");
}

void dump_state_transition_graph(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "Transitions\n");
  fprintf(file, "init:%lu\n", (long unsigned int)state_space_init_state(states));
  st_foreach(states->tbl, print_state_transition_graph, 0);
  fprintf(file, "\n");
}


/**
 * --ltl_nd時に使用．状態の名前（accept_s0など）を表示．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
void dump_state_name(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "Labels\n");
  st_foreach(states->tbl, print_state_name, 0);
  fprintf(file, "\n");
}

