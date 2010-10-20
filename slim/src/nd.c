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

#include "nd.h"
#include "task.h"
#include "parallel.h"
#include "por.h"
#include "error.h"
#include "delta_membrane.h"
#include "runtime_status.h"

static inline BOOL nd_status_init(void);
static inline void nd_status_finalize(StateSpace states);
inline static void nd_gen_successors_inner(struct ReactCxt *rc, LmnMembrane *cur_mem);
static BOOL expand_inner(struct ReactCxt *rc, LmnMembrane *cur_mem);
static void nd_store_successors(const StateSpace states,
                                State            *state,
                                struct ReactCxt  *rc,
                                Vector           *new_s,
                                BOOL             flag);

/* 状態sから1stepで遷移する状態のうち, 全ての未展開の状態をunexpand_sへ積む */
void expand(const StateSpace states,
            State            *s,
            AutomataState    property_automata_state,
            struct ReactCxt  *rc,
            Vector           *new_states,
            BOOL             flags)
{
  LmnMembrane *mem;

  /** restore : 膜の復元 */
  mem = state_restore_mem(s);

  if (is_dump(flags)) {
    dump_state_data(s, (LmnWord)stdout);
  }

  /** expand  : 状態の展開 */
  if (enable_por(flags)) {
    por_ample(states, s, property_automata_state, rc, new_states, flags);
  }
  else {
    if (has_property(flags)) {
      mc_gen_successors(s, mem, property_automata_state, rc, flags);
    } else {
      nd_gen_successors(s, mem, DEFAULT_STATE_ID, rc, flags);
    }

    if (nd_react_cxt_expanded_num(rc) > 0) {
      nd_store_successors(states, s, rc, new_states, flags);
    } else {
      state_space_add_end_state(states, s);
    }
  }

  /** free   : 遷移先を求めたLMNtalプロセスは開放 */
  if (use_compress(flags)) {
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

  set_expanded(s);
  RC_CLEAR_DATA(rc);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_total_space_update(states);
  }
#endif
}

/** 膜memから1stepで遷移可能な膜を, state_nameと合わせて状態として生成し, TransitionをRC_EXPANDEDへセットする.
 *  生成された状態は重複（多重辺）を含む */
void nd_gen_successors(State           *src,
                          LmnMembrane      *mem,
                          BYTE             state_name,
                          struct ReactCxt *rc,
                          BOOL            flags)
{
  Vector *expanded_roots, *expanded_rules;
  unsigned int i, n, old;

  RC_SET_GROOT_MEM(rc, mem);
  RC_SET_PROPERTY(rc, state_name);

  old = nd_react_cxt_expanded_num(rc);

  /** Generate Successor Membrane (or DeltaMemrane) */
  nd_gen_successors_inner(rc, mem);

  /** Generate Successor States */
  expanded_roots   = RC_EXPANDED(rc);       /* DeltaMembrane時は空 */
  expanded_rules   = RC_EXPANDED_RULES(rc);
  n = nd_react_cxt_expanded_num(rc);
  for (i = old; i < n; i++) {
    State *new;
    vec_data_t data;

    /* DeltaMembrane時はこの時点でSuccessor Membraneがない */
    if (use_delta(flags)) {
      new = state_make_minimal();
      state_set_property_state(new, state_name);
    }
    else {
      new = state_make((LmnMembrane *)vec_get(expanded_roots, i),
                       state_name,
                       lmn_env.mem_enc && lmn_env.enable_compress_mem);
    }

    state_set_predecessor(new, src);
    data = has_trans(flags) ? (vec_data_t)transition_make(new, lmn_rule_get_name((LmnRule)vec_get(expanded_rules, i)))
                            : (vec_data_t)new;

    /* DeltaMembrane時は, expanded_rootsが空であるため, 生成した空の状態を積む
     * 通常時は, expanded_rootsのi番目のデータをSuccessor MembraneからSuccessor Stateへ設定し直す */
    use_delta(flags) ? vec_push(expanded_roots, data)
                     : vec_set(expanded_roots, i, data);
  }
}

/** 生成した各Successor Stateが既出か否かを検査し, 遷移元の状態stateのサクセッサに設定する.
 *   + 多重辺を除去する.
 *   + "新規"状態をnew_statesへ積む.　 */
static void nd_store_successors(const StateSpace states,
                                State            *state,
                                struct ReactCxt  *rc,
                                Vector           *new_states,
                                BOOL             flags)
{
  st_table_t succ_tbl;         /* サクセッサのポインタの重複検査に使用 */
  Vector *expand_res;
  unsigned long i, expanded_num;
  unsigned int succ_i;

  succ_i       = 0;
  expand_res   = RC_EXPANDED(rc);
  expanded_num = nd_react_cxt_expanded_num(rc);
  succ_tbl     = st_init_ptrtable();

  LMN_ASSERT(vec_num(expand_res) == expanded_num); /* こまる */

  /** 状態登録 */
  for (i = 0; i < expanded_num; i++) {
    Transition src_t;
    st_data_t tmp;
    State *src_succ, *succ;

    src_t     = !has_trans(flags) ? NULL
                                  : (Transition)vec_get(expand_res, i);
    src_succ  = !has_trans(flags) ? (State *)vec_get(expand_res, i)
                                  : transition_next_state(src_t);

    /** ハッシュ表へ状態の追加を試みる */
    succ = use_delta(flags)
           ? state_space_insert_delta(states, src_succ,
                                     (struct MemDeltaRoot *)vec_get(RC_MEM_DELTAS(rc), i))
           : state_space_insert(states, src_succ);

    if (succ == src_succ) { /** src_succが状態空間に追加された */
      state_id_issue(succ);
      if (use_compact(flags))  state_free_mem(succ);
      if (new_states) {
        vec_push(new_states, (vec_data_t)succ);
      }
    }
    else {                  /** src_succは追加されなかった（すでに同じ状態がある) */
      state_free(src_succ);
      if (has_trans(flags)) { /* ポインタを既存状態へセット */
        transition_set_state(src_t, succ);
      } else {
        vec_set(expand_res, i, (vec_data_t)succ);
      }
    }

    /** 多重辺を除去しながら, 状態にサクセッサを設定 */
    if (st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp) == 0) { /* 多重辺ではない */
      st_data_t ins = has_trans(flags) ? (st_data_t)src_t : (st_data_t)succ;
      st_add_direct(succ_tbl, (st_data_t)succ, ins);
      vec_set(expand_res, succ_i++, ins);
    }
    else { /* 多重辺は消す */
      if (has_trans(flags)) {
        transition_add_rule((Transition)tmp, transition_rule(src_t, 0));
        transition_free(src_t);
      } /* else  "辺"という構造を持たない(直接pointerで表現している)ので何もしない */
    }
  }

  expand_res->num = succ_i;
  state_succ_set(state, expand_res); /* successorを登録 */

  st_free_table(succ_tbl);
}


/*
 * 状態を展開する
 * 引数として与えられた膜と、その膜に所属する全てのアクティブ膜に対して
 * ルール適用検査を行う
 * 子膜からルール適用を行っていく
 */
static BOOL expand_inner(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
  BOOL ret_flag = FALSE;

  for (; cur_mem; cur_mem = cur_mem->next) {
    unsigned long org_num = nd_react_cxt_expanded_num(rc);

    /* 代表子膜に対して再帰する */
    if (expand_inner(rc, cur_mem->child_head)) {
      ret_flag = TRUE;
    }
    if (lmn_mem_is_active(cur_mem)) {
      react_all_rulesets(rc, cur_mem);
    }
    /* 子膜からルール適用を試みることで, 本膜の子膜がstableか否かを判定できる */
    if (org_num == nd_react_cxt_expanded_num(rc)) {
      lmn_mem_set_active(cur_mem, FALSE);
    }
  }

  return ret_flag;
}

inline static void nd_gen_successors_inner(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
#ifdef PROFILE
  if(lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif

  expand_inner(rc, cur_mem);

#ifdef PROFILE
  if(lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__TRANS_RULE);
  }
#endif
}

#ifdef PROFILE
#  define pop_stack(List)                                                  \
      do {                                                                 \
        if (lmn_env.profile_level >= 3) {                                  \
          profile_remove_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord)); \
        }                                                                  \
        unset_on_stack((State *)vec_pop(List));                            \
      } while (0)
#  define put_stack(List, St)                                              \
      do {                                                                 \
        if (lmn_env.profile_level >= 3) {                                  \
          profile_add_space(PROFILE_SPACE__OPEN_LIST, sizeof(LmnWord));    \
        }                                                                  \
        vec_push((List), (vec_data_t)(St));                                \
        set_on_stack((St));                                                \
      } while (0)
#else
#  define pop_stack(List)                                                  \
      do {                                                                 \
        unset_on_stack((State *)vec_pop(List));                            \
      } while (0)
#  define put_stack(List, St)                                              \
      do {                                                                 \
        vec_push((List), (vec_data_t)(St));                                \
        set_on_stack((St));                                                \
      } while (0)
#endif

/*
 * nondeterministic execution
 * 深さ優先で全実行経路を取得する
 */
void nd_loop_dfs(StateSpace      states,
                 State           *init_state,
                 struct ReactCxt *rc,
                 BOOL            flags)
{
  Vector *stack, *new_states, *secondary, *cycle;
  unsigned long i, n;

  stack      = vec_make(8192);
  secondary  = mc_data.do_search ? vec_make(1024) : NULL;
  cycle      = mc_data.do_search ? vec_make(512) : NULL;
  new_states = vec_make(16);
  put_stack(stack, init_state);

  while (vec_num(stack) != 0) {
    State *s;
    AutomataState prop_atm_s;

    /** 展開元の状態の取得 */
    s = (State *)vec_peek(stack);
    if (mc_data.mc_exit) break;
    prop_atm_s = !has_property(flags) ? DEFAULT_PROP_AUTOMATA
                                      : automata_get_state(mc_data.property_automata,
                                                           state_property_state(s));

    if (is_expanded(s)) { /* (この時点で, sから到達可能な状態は全て展開済み) */
      if (mc_data.do_search && atmstate_is_accept(prop_atm_s)
          && !is_snd(s) && !is_on_cycle(s)) {
        /** entering second DFS */
        mc_nested_dfs(states, secondary, cycle, s, flags);
      }
      pop_stack(stack);
      continue;
    }
    else if (mc_data.do_search && atmstate_is_end(prop_atm_s)) { /* safety property analysis */
      mc_violate(states, s, NULL, flags);
      pop_stack(stack);
      continue;
    }

    /** サクセッサを展開 */
    expand(states, s, prop_atm_s, rc, new_states, flags);
    n = state_succ_num(s);
    for (i = 0; i < n; i++) {
      State *succ = state_succ_get(s, i);
      if (!is_expanded(succ)) {
        put_stack(stack, succ);
      }
    }
    vec_clear(new_states);
  }

  if (mc_data.do_search) {
    vec_free(secondary);
    vec_free(cycle);
  }
  vec_free(stack);
  vec_free(new_states);
}

/**
 * nondeterministic execution
 * 幅優先で全実行経路を取得する
 * @author Yusuke Ueno
 */
void nd_loop_bfs(StateSpace      states,
                 State           *init_state,
                 struct ReactCxt *rc,
                 BOOL            flags)
{
  Vector *primary, *secondary, *swap, *new_states;
  unsigned long i, n, depth;

  depth      = 0;
  primary    = vec_make(4096);
  secondary  = vec_make(4096);
  new_states = vec_make(16);
  vec_push(primary, (vec_data_t)init_state);

  while (vec_num(primary) > 0) { /* # of states@current layer > 0 */
    BOOL end_s = FALSE;

    depth++;
    swap      = primary;
    primary   = secondary;
    secondary = swap;

    while (vec_num(secondary) > 0) {
      State *s;
      AutomataState prop_atm_s;

      if (mc_data.mc_exit) goto NDBFS_END;
      s = (State *)vec_pop(secondary);
      prop_atm_s = !has_property(flags) ? DEFAULT_PROP_AUTOMATA
                                        : automata_get_state(mc_data.property_automata,
                                                             state_property_state(s));

      if (is_expanded(s)) {
        continue;
      } else if (mc_data.do_search && atmstate_is_end(prop_atm_s)) { /* safety property analysis */
        mc_violate(states, s, NULL, flags);
        continue;
      }

      expand(states, s, prop_atm_s, rc, new_states, flags);
      n = state_succ_num(s);
      if (n > 0) {
        for (i = 0; i < n; i++) {
          State *succ = state_succ_get(s, i);
          if (!is_expanded(succ)) {
            vec_push(primary, (vec_data_t)succ);
          }
        }
      } else {
        end_s = TRUE;
      }
      vec_clear(new_states);
    }

    /* 指定した深さか, 最終状態を発見したら抜ける */
    if (depth == lmn_env.depth_limits || (end_s && lmn_env.nd_search_end )) {
      break;
    }
  }
NDBFS_END:

  vec_free(primary);
  vec_free(secondary);
  vec_free(new_states);
}

static inline BOOL nd_status_init()
{
  BOOL flags = 0x00U;

  if (lmn_env.output_format == DOT) {
    lmn_env.output_format = DEFAULT;
  }

  if (lmn_env.sp_dump_format == INCREMENTAL) {
    set_dump(flags);
    if (lmn_env.mc_dump_format != CUI && lmn_env.mc_dump_format != LaViT) {
      lmn_fatal("unsupported incremental output format");
    }
    fprintf(stdout, "States\n");
  }

  if (lmn_env.show_transition) {
    set_trans(flags);
  }

  if (lmn_env.enable_compress_mem) {
    set_compress(flags);
    if (lmn_env.delta_mem) {
      set_delta(flags);
    }
    if (lmn_env.compact_stack) {
      set_compact(flags);
    }
  } else {
    lmn_env.optimize_hash = FALSE;
#ifdef PROFILE
    lmn_env.optimize_hash_old = FALSE;
#endif
  }

  if (mc_data.has_property) {
    set_property(flags);
  }

  if (lmn_env.ltl) {
    if (!mc_data.has_property) {
      mc_data.do_search = FALSE;
      mc_data.do_exhaustive = FALSE;
    }

    if (lmn_env.core_num == 1) {
      mc_data.errors = vec_make(8);
      mc_data.cycles = vec_make(32);
    } else {
      unsigned int i;
      mc_data.errors = LMN_NALLOC(struct Vector, lmn_env.core_num);
      mc_data.cycles = LMN_NALLOC(struct Vector, lmn_env.core_num);
      for (i = 0; i < lmn_env.core_num; i++) {
        vec_init(&mc_data.errors[i], 8);
        vec_init(&mc_data.cycles[i], 32);
      }
    }
  }

  if (lmn_env.por) {
    set_por(flags);
  }

  /* 組み合わせをサポートしていないオプションの整合性を取る */
  if (lmn_env.core_num >= 2) {
    if (lmn_env.sp_dump_format == INCREMENTAL) {
      lmn_fatal("unsupported combination incremental state dumper & parallelization.");
    }
#ifdef PROFILE
    if (lmn_env.optimize_hash_old) {
      lmn_fatal("unsupported combination optimized hash (old) & parallelization.");
    }
#endif
  }

  return flags;
}

static inline void nd_status_finalize(StateSpace states)
{
  if (lmn_env.dump) {
    FILE *out = stdout;
    state_space_format_states(states);

    if (lmn_env.trace) {
      /* 1. 状態遷移グラフの標準出力 */
      state_space_dumper(states, out);
    }

    if (mc_data.do_search) {
      /* 2. 反例パスの出力 */
      if (mc_get_error_num() > 0) {
        mc_dump_all_errors(states, out);
      }
    }
    else if (lmn_env.end_dump && lmn_env.mc_dump_format == CUI) {
      /* 3. 最終状態集合の出力(現状CUIモードのみ && ltlmc時にはskip) */
      state_space_ends_dumper(states, out);
    }

    /* CUIモードの場合状態数などのデータも標準出力 */
    if (lmn_env.mc_dump_format == CUI) {
      fprintf(out, "\'# of States\'(end)      = %lu.\n", state_space_end_num(states));
      fprintf(out, "\'# of States\'(stored)   = %lu.\n", state_space_num(states));
      if (mc_data.do_search && mc_get_error_num() > 0) {
        fprintf(out, "\'# of Errors or Cycles\' = %lu.\n", mc_get_error_num());
      }
    }
  }

  if (mc_data.errors) {
    if (lmn_env.core_num == 1) {
      vec_free(mc_data.errors);
      vec_free(mc_data.cycles);
    } else {
      unsigned int i;
      for (i = 0; i < lmn_env.core_num; i++) {
        vec_destroy(&mc_data.errors[i]);
        vec_destroy(&mc_data.cycles[i]);
      }
      LMN_FREE(mc_data.errors);
      LMN_FREE(mc_data.cycles);
    }
  }
}

/* 非決定実行を行う */
void run_nd(Vector *start_rulesets)
{
  static LmnMembrane *mem; /* jniではここがstaticでないと困るらしい */
  StateSpace states;
  BOOL flags = nd_status_init();

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

  if (lmn_env.enable_parallel) {
    states = do_nd_parallel(mem, flags);
  } else {
    states = do_nd(mem, flags);
  }

  if (lmn_env.nd_remain) {
    lmn_env.nd_remaining = TRUE;
  } else {
    lmn_env.nd_remaining = FALSE;
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }

  if (lmn_env.profile_level >= 2) {
    profile_statespace(states);
  }
  nd_status_finalize(states);
  state_space_free(states);
}

StateSpace do_nd(LmnMembrane *world_mem_org, BOOL flags)
{
  struct ReactCxt rc;
  StateSpace states;
  State *init_s;
  BYTE p_label;

  /* 初期プロセスから得られる初期状態を生成して登録 */
  states  = state_space_make();
  p_label = !mc_data.has_property ? DEFAULT_STATE_ID
                                  : automata_get_init_state(mc_data.property_automata);
  init_s  = state_make(lmn_mem_copy(world_mem_org),
                       p_label,
                       lmn_env.mem_enc && use_compact(flags));
  state_space_set_init_state(states, init_s, use_compact(flags));

  if (lmn_env.profile_level >= 1) {
    profile_start_exec();
    profile_start_exec_thread();
  }

  /* --ndの実行 */
  nd_react_cxt_init(&rc, DEFAULT_STATE_ID);
  if (lmn_env.bfs) {
    nd_loop_bfs(states, init_s, &rc, flags);
  } else {
    nd_loop_dfs(states, init_s, &rc, flags);
  }
  nd_react_cxt_destroy(&rc);

  if (lmn_env.profile_level >= 1) {
    profile_finish_exec_thread();
    profile_finish_exec();
  }
  return states;
}
