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
#include "parallel.h"
#include "propositional_symbol.h"
#include "ltl2ba_adapter.h"
#include "error.h"
#include "runtime_status.h"

enum MC_ERRORNO {
  MC_ERR_NC_ENV,
  MC_ERR_PROP_ENV,
  MC_NC_OPEN_ERROR,
  MC_NC_LOAD_ERROR,
  MC_PROP_OPEN_ERROR,
  MC_PROP_LOAD_ERROR,
};

static BOOL mc_nested_dfs_inner(State  *seed,
                                Vector *search_stack,
                                Vector *postordered,
                                BOOL   flags);
inline static void stutter_extension(State           *s,
                                     LmnMembrane     *mem,
                                     BYTE            next_label,
                                     struct ReactCxt *rc,
                                     BOOL            flags);
inline static void mc_store_vertex(Vector *v, State *seed);
static void mc_store_cycle(const Vector *cycle, State *seed);
static unsigned int mc_print_vec_state(FILE         *fp,
                                       Vector       *v,
                                       State        *seed,
                                       unsigned int from_i,
                                       unsigned int end_i);
static Vector *gen_from_init_path(State *s);

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


/*----------------------------------------------------------------------
 * Nested Depth First Search
 */

/**
 * 2段階目のDFS:
 *   1段階目のDFSで展開済みとなった受理頂点から, 自身に戻る閉路(受理サイクル)を探索する.
 *   本DFS中に未展開の状態を訪問することは想定されていない.
 */
void mc_nested_dfs(StateSpace states, Vector *list, Vector *cycle, State *seed, BOOL flags) {
  BOOL has_error;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__CYCLE_EXPLORE);
  }
#endif

  LMN_ASSERT(list && cycle && seed);

  has_error = FALSE;
  vec_push(list, (vec_data_t)seed);
  has_error = mc_nested_dfs_inner(seed, list, cycle, flags);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__CYCLE_EXPLORE);
  }
#endif

  if (has_error) {
    mc_violate(states, seed, cycle, flags);
  }
  vec_clear(cycle);
  vec_clear(list);
}

static BOOL mc_nested_dfs_inner(State  *seed,
                                Vector *search,
                                Vector *postordered,
                                BOOL   flags)
{
  while(vec_num(search) > 0) {
    State *s = (State *)vec_peek(search);
    vec_push(postordered, (vec_data_t)s);
    if (is_snd(s)) {
      /** DFS2 BackTracking */
      vec_pop(search);
      vec_pop(postordered);
    }
    else {
      unsigned int i, n;

      set_snd(s);
      n = state_succ_num(s);
      for (i = 0; i < n; i++) {
        State *succ = state_succ_get(s, i);

        if (is_on_cycle(succ)) {
          return FALSE;
        }
        else if (succ == seed /* || is_on_stack(succ) */) {
          /* コードの都合で出力がうまくいかなかったため, 一旦コメントアウト */
          return TRUE; /* 同一のseedから探索する閉路が1つ見つかったならば探索を打ち切る */
        } else if (!is_snd(succ)) {
          vec_push(search, (vec_data_t)succ);
        }
      }
    }
  }

  return FALSE;
}


/* 膜memから, 性質ルールとシステムルールの同期積によって遷移可能な全状態(or遷移)をベクタに載せて返す */
void mc_gen_successors(State           *s,
                       LmnMembrane     *mem,
                       AutomataState   prop_atm_s,
                       struct ReactCxt *rc,
                       BOOL            flags)
{
  Vector *expanded;
  unsigned int i, j, n;
  int tmp_expand_num;

  /** 状態展開:
   *   性質ルールが適用される場合, global_rootに対してシステムルール適用検査を行う.
   *   システムルール適用はglobal_rootのコピーに対して行われる.
   *   状態展開にexpandが使用された場合は, 現状態から遷移可能な全ての状態が生成されるのに対し，
   *   ampleが使用された場合はPartial Order Reductionに基づき，本当に必要な次状態のみが生成される．
   *   なお，適用可能なシステムルールが存在しない場合は，何も処理を行わない特殊なシステムルール(stutter extension rule)
   *   が存在するものと考えてε遷移をさせ，受理頂点に次状態が存在しない場合でも受理サイクルを形成できるようにする．
   *   (c.f. "The Spin Model Checker" pp.130-131)
   */

  tmp_expand_num = -1; /* 初めて性質ルールにマッチングした際に展開したsuccessorの数を入れておくとこ */
  expanded = RC_EXPANDED(rc); /* set pointer */
  n = atmstate_transition_num(prop_atm_s);
  for (i = 0; i < n; i++) {
    AutomataTransition prop_t = atmstate_get_transition(prop_atm_s, i);

    /* 性質ルールにマッチングした数だけシステムルールによる状態展開を行う */
    if (eval_formula(mem, mc_data.propsyms, atm_transition_get_formula(prop_t))) {
      BYTE prop_next_label = atm_transition_next(prop_t);

      /* TODO: MemDeltaRootの複製処理 */
      if (use_delta(flags)) {
        /** Delta-Membrane */
        tmp_expand_num = vec_num(expanded);
        nd_gen_successors(s, mem, prop_next_label, rc, flags);
        if (vec_num(expanded) == tmp_expand_num) {
          stutter_extension(s, mem, prop_next_label, rc, flags);
        }
      }
      else {
        /** Default */
        if (tmp_expand_num < 0) {
          /* 初めて性質ルールにマッチングした場合:
           *  システムルールを適用し, サクセッサの階層グラフの集合を作る */
          nd_gen_successors(s, mem, prop_next_label, rc, flags); /* システムルールを適用し, 状態を展開する */
          tmp_expand_num = vec_num(expanded);
          if (tmp_expand_num == 0) { /* stutter extensionルールによる状態展開 */
            stutter_extension(s, mem, prop_next_label, rc, flags);
          }
        } else {
          /* 2つ目以降の性質ルールがマッチングした場合:
           *  システムルールの適用結果と生成される階層グラフの集合は1つ目と同様であるため,
           *  集合を複製し, 性質ラベルの異なる新たな状態として生成する */
          if (tmp_expand_num == 0) {
            stutter_extension(s, mem, prop_next_label, rc, flags);
          } else {
            for (j = 0; j < tmp_expand_num; j++) {
              Transition src_succ_t;
              State *src_succ_s, *new_s;
              vec_data_t data;
              src_succ_t = has_trans(flags) ? (Transition)vec_get(expanded, j)  : NULL;
              src_succ_s = has_trans(flags) ? transition_next_state(src_succ_t) : (State *)vec_get(expanded, j);

              new_s = state_copy(src_succ_s);
              state_set_predecessor(new_s, s);
              state_set_property_state(new_s, prop_next_label);
              data  = has_trans(flags) ? (vec_data_t)transition_make(new_s, transition_rule(src_succ_t, 0))
                                       : (vec_data_t)new_s;
              vec_push(expanded, data);
            }
          }
        }
      }
    }
  }
}

/* 膜memとmemに対応する状態sを受け取り, コピーしてラベルnext_labelを持った状態をRC_EXPANDED(rc)へ積む */
inline static void stutter_extension(State           *s,
                                     LmnMembrane     *mem,
                                     BYTE            next_label,
                                     struct ReactCxt *rc,
                                     BOOL            flags)
{
  vec_data_t data;
  State *new_s;

  if (use_delta(flags)) {
    nd_react_cxt_add_mem_delta(rc, dmem_root_make(mem, NULL, 0), NULL);
    new_s = state_make_minimal();
  } else {
    new_s = state_copy_with_mem(s, mem);
  }
  state_set_property_state(new_s, next_label);
  state_set_predecessor(new_s, s);
  data  = has_trans(flags) ? (vec_data_t)transition_make(new_s, lmn_intern("ε"))
                           : (vec_data_t)new_s;
  vec_push(RC_EXPANDED(rc), data);
}

void mc_violate(StateSpace   states,
                State        *seed,
                const Vector *cycle,
                BOOL         flags)
{
  mc_store_vertex(mc_data.errors, seed);
  mc_store_cycle(cycle, seed);

  if (!mc_data.do_exhaustive) {
    mc_data.mc_exit = TRUE;
  }
}

/* ベクタvのfrom_iからend_i未満までの状態を出力する.
 * seedと等価な状態を発見した場合はアスタリスクを付加して出力し, 以降の出力を打ち切る.
 * 処理終了時のindexを返す */
static unsigned int mc_print_vec_state(FILE *fp,
                                       Vector *v,
                                       State *seed,
                                       unsigned int from_i,
                                       unsigned int end_i)
{
  State *s;
  unsigned int ret;

  if (!v || (end_i > vec_num(v))) {
    return from_i;
  }

  s = NULL;
  for (ret = from_i; (ret < end_i) && (s != seed); ret++) {
    if (lmn_env.sp_dump_format != LMN_SYNTAX) {
      char *m;
      s = (State *)vec_get(v, ret);
      m = (s == seed) ? "*" : " ";
      fprintf(fp, "%s%2lu::%s", m, state_format_id(s), automata_state_name(mc_data.property_automata, state_property_state(s)));
      state_print_mem(s, (LmnWord)fp);
    }
    else {
      s = (State *)vec_get(v, ret);
      fprintf(fp, "path%lu_%s", state_format_id(s), automata_state_name(mc_data.property_automata, state_property_state(s)));
      state_print_mem(s, (LmnWord)fp);
      fprintf(fp, ".\n");

      fprintf(fp, "path%lu_%s", state_format_id(s), automata_state_name(mc_data.property_automata, state_property_state(s)));
      state_print_mem(s, (LmnWord)fp);
      fprintf(fp, ":- ");
    }
  }
  return ret;
}


void mc_dump_all_errors(StateSpace ss, FILE *f)
{
  switch (lmn_env.mc_dump_format) {
  case LaViT:
    /* 反例パスグラフを出力する */
  {
    Vector *v;
    unsigned int i, j, v_num;

    fprintf(f, "CounterExamplePaths\n");

    v_num = mc_data.do_parallel ? lmn_env.core_num : 1;
    for (i = 0; i < v_num; i++) {
      v = mc_data.do_parallel ? &mc_data.errors[i] : mc_data.errors;
      for (j = 0; j < vec_num(v); j++) {
        State *s;
        AutomataState atm;
        Vector *path;

        s   = (State *)vec_get(v, j);
        atm = automata_get_state(mc_data.property_automata,
                                 state_property_state(s));
        path = gen_from_init_path(s); /* パス上の状態にフラグを立てる */
        vec_free(path);
      }
    }
    state_space_dump_all_error_paths(ss, f);
    break;
  }
  case Dir_DOT:
    /* 反例パスをサブグラフにしたい */
    break;
  case FSM:
    break;
  case CUI:
  {
    Vector *v, *c;
    unsigned int i, j, cycle_i, v_num;

    fprintf(f, "%s\n", lmn_env.sp_dump_format == LMN_SYNTAX ? "counter_exapmle."
                                                            : "CounterExamplePaths");

    v_num = mc_data.do_parallel ? lmn_env.core_num : 1;
    for (i = 0; i < v_num; i++) {
      cycle_i = 0;
      if (mc_data.do_parallel) {
        v = &mc_data.errors[i];
        c = &mc_data.cycles[i];
      } else {
        v = mc_data.errors;
        c = mc_data.cycles;
      }

      for (j = 0; j < vec_num(v); j++) {
        State *s;
        AutomataState atm;
        Vector *path;
        int align = lmn_env.sp_dump_format == LMN_SYNTAX ? 1 : 0;

        s   = (State *)vec_get(v, j);
        atm = automata_get_state(mc_data.property_automata,
                                 state_property_state(s));
        path = gen_from_init_path(s);

        if (atmstate_is_end(atm)) { /* safety: error vertex */
          mc_print_vec_state(f, path, s, 0, vec_num(path) - align);
        } else { /* liveness: cycle seed */
          mc_print_vec_state(f, path, s, 0, vec_num(path));
          cycle_i = mc_print_vec_state(f, c, s, cycle_i + 1, vec_num(c) - align);
        }
        fprintf(f, "\n");
        vec_free(path);

        if (lmn_env.sp_dump_format == LMN_SYNTAX) { /* とりあえず一つだけ反例をdumpする */
          fprintf(f, "path%lu_%s", state_format_id(s), automata_state_name(mc_data.property_automata, state_property_state(s)));
          state_print_mem(s, (LmnWord)f);
          fprintf(f, ".\n");
          return;
        }
      }
    }
    break;
  }
  default:
    lmn_fatal("unexpected");
    break;
  }
}


/* 初期頂点から状態sに至る経路を載せたVectorをmallocして返す.
 * Vectorに載せた状態にはon_cycleフラグが立つ */
static Vector *gen_from_init_path(State *seed)
{
  Vector *path;
  State  *pred;

  path = vec_make(256);
  pred = seed;
  while (pred) {
    set_on_cycle(pred);
    vec_push(path, (vec_data_t)pred);
    pred = state_get_predecessor(pred);
  }

  vec_reverse(path);

  return path;
}

inline static void mc_store_vertex(Vector *v, State *vertex)
{
  if (mc_data.do_parallel) {
    v = &v[lmn_thread_id];
  }
  vec_push(v, (vec_data_t)vertex);
}

static void mc_store_cycle(const Vector *cycle, State *seed)
{
  if (cycle) {
    unsigned int i, n;
    n = vec_num(cycle);
    for (i = 0; i < n; i++) {
      State *c = (State *)vec_get(cycle, i);
      set_on_cycle(c);
      mc_store_vertex(mc_data.cycles, c);
    }
    mc_store_vertex(mc_data.cycles, seed);
    set_on_cycle(seed);
  }
}

unsigned long mc_get_error_num()
{
  unsigned long ret;

  ret = 0;
  if (mc_data.do_parallel) {
    unsigned int i;
    for (i = 0; i < lmn_env.core_num; i++) {
      Vector *v = &mc_data.errors[i];
      ret += vec_num(v);
    }
  } else {
    ret = vec_num(mc_data.errors);
  }
  return ret;
}
