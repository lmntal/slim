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
#include "runtime_status.h"

static Vector *expand_sub(struct ReactCxt *rc, LmnMembrane *cur_mem, BYTE state_name);
static void nd_loop(StateSpace states, State *init_state, BOOL dump);
static StateSpace do_nd_sub(LmnMembrane *world_mem, BOOL dump);
static BOOL expand_inner(struct ReactCxt *rc,
                         LmnMembrane *cur_mem);

/* 状態stateから１ステップで遷移する状態を展開、stateのサクセッサを設定、状態空間に追加する。
   新たに生成された状態（サクセッサすべてではない）のベクタを返す */
Vector *nd_expand(StateSpace states, State *state, BYTE state_name)
{
  Vector *expanded;
  Vector successors, *new_states;
  st_table_t succ_tbl;         /* サクセッサのポインタの重複検査に使用 */
  unsigned long i, expanded_num;

  if (lmn_env.por) expanded = ample(states, state);
  else expanded = nd_gen_successors(state, state_name);

  /* 状態空間へのサクセッサの追加 */

  expanded_num = vec_num(expanded);

  if (expanded_num == 0) {
    state_space_add_end_state(states, state);
  }

  succ_tbl= st_init_ptrtable();
  vec_init(&successors, 16);
  new_states = vec_make(16);

  for (i = 0; i < expanded_num; i++) {
    Transition t;
    st_data_t tmp;
    State *src_succ;
    State *succ;

    t = (Transition)vec_get(expanded, i);
    src_succ = transition_next_state(t);
    succ = state_space_insert(states, src_succ);

    if (succ == src_succ) { /* succが状態空間に追加された */
      vec_push(new_states, (vec_data_t)succ);
    } else { /* src_succは追加されなかった（すでに同じ状態がある) */
      state_free(src_succ);
      transition_set_state(t, succ);
    }

    /* expandedに同じ状態がなければ、サクセッサとして追加 */
    if (st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp) == 0) {
      st_add_direct(succ_tbl, (st_data_t)succ, (st_data_t)t);
      vec_push(&successors, (vec_data_t)t);
    } else {
      transition_add_rule((Transition)tmp, transition_rule(t, 0));
      transition_free(t);
    }
  }

  /* 状態にサクセッサを設定 */
  for (i = 0; i < vec_num(&successors); i++) {
    state_succ_add(state, (Transition)vec_get(&successors, i));
  }

  st_free_table(succ_tbl);
  vec_free(expanded);
  vec_destroy(&successors);
#ifdef PROFILE
  runtime_status_update(states);
#endif
  return new_states;
}

/* 状態stateから１ステップで遷移する状態を生成する。生成された状態は重複（多重辺）を含む */
Vector *nd_gen_successors(State *state, BYTE state_name)
{
  Vector *expanded;
  struct ReactCxt rc;
  LmnMembrane *mem;


  mem = state_mem(state);
  if (!mem) {
    mem = lmn_binstr_decode(state_mem_binstr(state));
  }
  nd_react_cxt_init(&rc, DEFAULT_STATE_ID);
  RC_SET_GROOT_MEM(&rc, mem);
  expanded = expand_sub(&rc, mem, state_name);
  nd_react_cxt_destroy(&rc);

  if (!state_mem(state)) {
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }

  return expanded;
}

static Vector *expand_sub(struct ReactCxt *rc, LmnMembrane *cur_mem, BYTE state_name)
{
  int i;
  Vector *expanded_roots;
  Vector *expanded;

  expand_inner(rc, cur_mem);
  expanded_roots = RC_EXPANDED(rc);
  expanded = vec_make(32);
  for (i = 0; i < vec_num(expanded_roots); i++) {
    State *state;
    lmn_interned_str rule_name;

    state = state_make((LmnMembrane *)vec_get(expanded_roots, i),
                       state_name,
                       (LmnRule)vec_get(RC_EXPANDED_RULES(rc), i));
    rule_name = lmn_rule_get_name((LmnRule)vec_get(RC_EXPANDED_RULES(rc), i));
    vec_push(expanded, (vec_data_t)transition_make(state, rule_name));
  }

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
static void nd_loop(StateSpace states, State *init_state, BOOL dump) {
  Vector *stack;
  unsigned long i, n;

  stack = vec_make(2048);
  vec_push(stack, (LmnWord)init_state);

  while (vec_num(stack) != 0) {
    /* 展開元の状態をpop */
    State *s = (State *)vec_pop(stack);
    Vector *new_states;

#ifdef PROFILE
      status_nd_pop_stack();
#endif

    if (is_expanded(s)) {
      /* 状態が展開済みである場合，スタック上から除去してフラグを解除する */
      unset_open(s);

      continue;
    }
    set_expanded(s); /* sに展開済みフラグを立てる */

    /* サクセッサを展開 */
    new_states = nd_expand(states, s, DEFAULT_STATE_ID);

    if (dump) { /* 状態の出力 */
      n = vec_num(new_states);
      for (i = 0; i < n; i++) {
        dump_state_data((State *)vec_get(new_states, i));
      }
    }

    n = state_succ_num(s);
    for (i = 0; i < n; i++) {
      State *succ = state_succ_get(s, i);

      vec_push(stack, (vec_data_t)succ);
      set_open(succ); /* set ss to open, i.e., on the search stack */

#ifdef PROFILE
      status_nd_push_stack();
#endif

      if (lmn_env.compact_stack && ((i + 1) < n)) {
        /* メモリ最適化のために、サクセッサの作成後に膜を解放する */
        /* ただし, 最後にスタックに積まれた状態は直後にデコードされるため解放しない */
        state_free_mem(succ);
      }
    }

    vec_free(new_states);

    /* 展開済みになった状態の膜を解放する */
    state_free_mem(s);
  }

  vec_free(stack);
}

/*
 * nondeterministic execution
 * 幅優先で全実行経路を取得する
 * [yueno] base r315
 */
static void nd_loop_bfs(StateSpace states, State *init_state, BOOL dump) {
  Vector *primary, *secondary, *swap;
  unsigned long i, n, depth;

  primary = vec_make(2048);
  secondary = vec_make(2048);
  vec_push(primary, (LmnWord)init_state);
  depth = 0;

  while(TRUE) {
    ++depth;
    while (vec_num(primary) != 0) {
      /* 展開元の状態をpop */
      State *s = (State *)vec_pop(primary);
      Vector *new_states;

  #ifdef PROFILE
        status_nd_pop_stack();
  #endif

      if (is_expanded(s)) {
        /* 状態が展開済みである場合，展開しない */
        continue;
      }
      set_expanded(s); /* sに展開済みフラグを立てる */

      /* サクセッサを展開 */
      new_states = nd_expand(states, s, DEFAULT_STATE_ID);

      if (dump) { /* 状態の出力 */
        n = vec_num(new_states);
        for (i = 0; i < n; i++) {
          dump_state_data((State *)vec_get(new_states, i));
        }
      }

      n = state_succ_num(s);
      for (i = 0; i < n; i++) {
        State *succ = state_succ_get(s, i);

        vec_push(secondary, (vec_data_t)succ);

  #ifdef PROFILE
        status_nd_push_stack();
  #endif

        if (lmn_env.compact_stack && ((i + 1) < n)) {
          /* メモリ最適化のために、サクセッサの作成後に膜を解放する */
          /* ただし, 最後にスタックに積まれた状態は直後にデコードされるため解放しない */
          state_free_mem(succ);
        }
      }

      vec_free(new_states);

      /* 展開済みになった状態の膜を解放する */
      state_free_mem(s);
    }

    if (lmn_env.bfs_depth == depth) break; /* BFS終了条件 */
    if (vec_num(secondary) == 0) break;

    /* swap states Vectors */
    swap = primary;
    primary = secondary;
    secondary = swap;
  }

  vec_free(primary);
  vec_free(secondary);
}

/* 非決定実行を行う */
void run_nd(Vector *start_rulesets)
{
  LmnMembrane *mem;
  StateSpace states;

  /**
   * initialize containers
   */
  init_por_vars();

  /* make global root membrane */
  mem = lmn_mem_make();
  react_start_rulesets(mem, start_rulesets);
  activate_ancestors(mem);

  if (lmn_env.dump) {
    fprintf(stdout, "States\n");
  }

  states = do_nd_dump(mem);

  if (lmn_env.profile_level > 0) {
    status_state_space(states);
  }

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
  return do_nd_sub(world_mem_org, FALSE);
}

StateSpace do_nd_dump(LmnMembrane *world_mem_org)
{
  return do_nd_sub(world_mem_org, TRUE);
}

static StateSpace do_nd_sub(LmnMembrane *world_mem_org, BOOL dump)
{
  State *initial_state;
  StateSpace states = state_space_make();
  LmnMembrane *world_mem = lmn_mem_copy(world_mem_org);

  /* 初期プロセスから得られる初期状態を生成 */
  initial_state = state_make_for_nd(world_mem, ANONYMOUS);
/*   mc_flags.initial_state = initial_state; */

  state_space_set_init_state(states, initial_state);

  if (lmn_env.profile_level > 0) {
    status_nd_start_running();
  }

/*   /\* --nd_dumpの実行 *\/ */
/*   else if(lmn_env.nd_dump){ */
/*     nd_dump_exec(); */
/*   } */
  /* --ndの実行（非決定実行後に状態遷移グラフを出力する） */
/*   else{ */
  dump_state_data(initial_state);
  if (lmn_env.bfs) {
    nd_loop_bfs(states, initial_state, dump);
  } else {
    nd_loop(states, initial_state, dump);
  }
/*   } */

  if (lmn_env.profile_level > 0) {
    status_nd_finish_running();
  }
  return states;
}

/*----------------------------------------------------------------------
 * ND React Context
 */

void nd_react_cxt_init(struct ReactCxt *cxt, BYTE prop_state)
{
  struct NDReactCxtData *v = LMN_MALLOC(struct NDReactCxtData);

  RC_SET_MODE(cxt, REACT_ND);
  cxt->v = v;
  v->roots = vec_make(64);
  v->rules = vec_make(64);
  v->property_state = prop_state;
}

void nd_react_cxt_destroy(struct ReactCxt *cxt)
{
  vec_free(((struct NDReactCxtData *)(cxt->v))->roots);
  vec_free(((struct NDReactCxtData *)(cxt->v))->rules);
  LMN_FREE(cxt->v);
}

void nd_react_cxt_add_expanded(struct ReactCxt *cxt,
                               LmnMembrane *mem,
                               LmnRule rule)
{
  vec_push(((struct NDReactCxtData *)cxt->v)->roots, (LmnWord)mem);
  vec_push(((struct NDReactCxtData *)cxt->v)->rules, (LmnWord)rule);
}

