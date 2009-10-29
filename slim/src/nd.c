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

#ifdef PROFILE
  if (lmn_env.profile_level>0)  status_start_expand();
#endif

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
    State *src_succ;
    State *succ;

    src_succ = (State *)vec_get(expanded, i);
    succ = state_space_insert(states, src_succ);

    if (succ == src_succ) { /* succが状態空間に追加された */
      vec_push(new_states, (vec_data_t)succ);
    } else { /* src_succは追加されなかった（すでに同じ状態がある) */
      state_free(src_succ);
    }
    /* expandedに同じ状態がなければ、サクセッサとして追加 */
    if (st_insert(succ_tbl, (st_data_t)succ, 0) == 0) {
      vec_push(&successors, (vec_data_t)succ);
    }
  }

  /* 状態にサクセッサを設定 */
  for (i = 0; i < vec_num(&successors); i++) {
    state_succ_add(state, (State *)vec_get(&successors, i));
  }

  st_free_table(succ_tbl);
  vec_free(expanded);
  vec_destroy(&successors);

#ifdef PROFILE
  if (lmn_env.profile_level>0) status_finish_expand();
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

    state = state_make((LmnMembrane *)vec_get(expanded_roots, i),
                       state_name,
                       (LmnRule)vec_get(RC_EXPANDED_RULES(rc), i));

    vec_push(expanded, (LmnWord)state);
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
  unsigned long i;

  stack = vec_make(2048);
  vec_push(stack, (LmnWord)init_state);

  while (vec_num(stack) != 0) {
    /* 展開元の状態。popはしないでsuccessorの展開が終わるまで
       スタックに積んでおく */
    State *s = (State *)vec_peek(stack);
    Vector *new_states;

    if (is_expanded(s)) {
      /* 状態が展開済みである場合，スタック上から除去してフラグを解除する */
      vec_pop(stack);
      unset_open(s);
      continue;
    }
    set_expanded(s); /* sに展開済みフラグを立てる */

    state_restore_mem(s);
    if (dump) dump_state_data(s);

    new_states = nd_expand(states, s, DEFAULT_STATE_ID);
    state_free_mem(s);

    for (i = 0; i < state_succ_num(s); i++) {
      State *succ = state_succ_get(s, i);

      vec_push(stack, (vec_data_t)succ);
      /* set ss to open, i.e., on the search stack */
      set_open(succ);

      if (lmn_env.compact_stack) {
        /* メモリ最適化のために、サクセッサの作成後に膜を解放する */
        state_free_mem(succ);
      }
    }

    vec_free(new_states);

    set_expanded(s); /* sに展開済みフラグを立てる */

    /* 展開済みになった状態の膜を解放する */
    state_free_mem(s);
  }

  vec_free(stack);
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

  {
    int temp_env_p = lmn_env.profile_level;
    lmn_env.profile_level = 0;
    react_start_rulesets(mem, start_rulesets);
    lmn_env.profile_level = temp_env_p;
  }
  activate_ancestors(mem);

  if (lmn_env.dump) {
    fprintf(stdout, "States\n");
  }

  states = do_nd_dump(mem);
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

/*   /\* --nd_dumpの実行 *\/ */
/*   else if(lmn_env.nd_dump){ */
/*     nd_dump_exec(); */
/*   } */
  /* --ndの実行（非決定実行後に状態遷移グラフを出力する） */
/*   else{ */
  nd_loop(states, initial_state, dump);
/*   } */

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

