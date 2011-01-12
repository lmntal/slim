/*
 * por.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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

#include "por.h"
#include "lmntal.h"
#include "membrane.h"
#include "task.h"
#include "mc.h"
#include "st.h"
#include "vector.h"
#include "rule.h"
#include "error.h"

/**
 * PORの変数やデータ構造の初期化を行う
 */
void init_por_vars() {
#ifdef COMMENT
  por_data.States_POR = state_table_make_with_size(POR_TABLE_SIZE);
  por_data.strans_independency = st_init_numtable();
  por_data.succ_strans = vec_make(5);
  por_data.ample_candidate = vec_make(1);
  por_data.Stack_POR = vec_make(256);
  por_data.next_strans_id = 1U; /* 0は使用しない */
#endif
}

void free_por_vars() {
#ifdef COMMENT
  state_table_free(por_data.States_POR);
  st_free_table(por_data.strans_independency);
  vec_free(por_data.succ_strans);
  vec_free(por_data.ample_candidate);
  vec_free(por_data.Stack_POR);
#endif
}

/* PORコードは一旦諦める(再開予定あり) */
void por_ample(const StateSpace states,
    State *state,
    AutomataState property_automata_state,
    struct ReactCxt *rc,
    Vector *new_s,
    BOOL flag)
{

  return;
}


#ifdef COMMENT
static void finalize_ample(void);
static void ample_inner(StateSpace states, State *s, struct ReactCxt *rc, Vector *unexpand_s);
static void ample_store_successors(StateSpace states, State *s, Vector *new_states, Vector *unexpand_t, BOOL flags);
static void ample_redundunt_states(StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states, Vector *unexpand_s);
static void independncy_table_insert_id(Transition t);
static void ample_redundunt_states(StateSpace states, State *s, struct ReactCxt *rc, Vector *unexpand_s);

static BOOL independency_check(const StateSpace states, State *s);
static BOOL is_independent_of_ample(Transition strans);
static int independency_vec_free(st_data_t _k, st_data_t vec, st_data_t _a);
static int destroy_tmp_state_graph(st_data_t _k, st_data_t state, st_data_t _a);
static Vector *push_succstates_to_expanded(State *s);
static Vector *push_ample_to_expanded(State *s);
static int build_ample_satisfying_lemma(st_data_t key, st_data_t val, st_data_t current_state);
static BOOL check0_independency_inner(const StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states);
static BOOL check0_independency(StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states);
static void gen_successors(const StateSpace states, State *s);
static void expand_States_POR(State *s);
static BOOL push_independent_strans_to_table(unsigned long i1, unsigned long i2);
static BOOL is_independent_of_ample(Transition strans);
static BOOL check_C3(const StateSpace states, State *s);
static BOOL check_C2(State *s);
static BOOL check_C1(const StateSpace states, State *s);

/* LMN_EXTERN int dump__strans_independency(st_data_t key, st_data_t vec, st_data_t _a); */
/* LMN_EXTERN void dump__ample_candidate(void); */


void ample(const StateSpace states,
           State *s,
           AutomataState property_automata_state,
           struct ReactCxt *rc,
           Vector *unexpand_s,
           BOOL flag)
{
  Vector *new_states;
  Vector *expand_result;
  LmnMembrane *mem;
  unsigned long n;

  /** init */
  new_states = vec_make(32);
  state_table_add_direct(por_data.States_POR, s);
  set_ample(s); /* s(展開元)が簡約グラフ内に含まれる旨のフラグを立てる */
  vec_clear(por_data.ample_candidate);

  /** 状態の展開 */
  if (!is_atomic(flags) && has_property(flags)) {
    mc_gen_successors(s, mem, property_automata_state, rc);
  }
  else {
    nd_gen_successors(mem, DEFAULT_STATE_ID, rc);
  }
  n = vec_num(RC_EXPANDED(rc));

  /** RC_EXPANDEDから新規状態へのTransitonをnew_statesに, 新規状態ではない未展開の状態へのTransitonををunexpand_sに積む.
   * 同時に多重辺は除去しておく. */
  ample_redundunt_states(states, s, rc, new_states, unexpand_s);
  ample_inner(states, s, rc, new_states);  /** 新規状態についてreductionを試みる */

  if (n > 0) {
    /** 残った新規状態をハッシュ表へ追加し, 未展開の状態と合わせてunexpand_sとsのサクセサへ積む */
    ample_store_successors(states, s, new_states, unexpand_s, flags);
  } else { /* 遷移数0の状態として登録 */
    state_space_add_end_state(states, s);
  }

  vec_free(new_states);
  finalize_ample();
}

static void finalize_ample()
{
  por_data.next_strans_id = POR_ID_INITIALIZER;                       /* StateTransitionのIDを初期値に戻す */
  st_foreach(por_data.strans_independency, independency_vec_free, 0); /* 独立性情報テーブルの解放 */

  /* por_data.States_POR上の状態の内，ample(s)内の要素でないものをすべて解放する．
   * また，StateTransitionは実際に構築するstate graph内では不要なため，ここですべて解放する */
  st_foreach(por_data.States_POR, destroy_tmp_state_graph, 0);
}

static void independncy_table_insert_id(Transition t) {
  id = transition_id(t);
  is_new_id = !st_lookup(por_data.strans_independency, (st_data_t)id, (st_data_t *)&t);
  if (is_new_id) {
    Vector *v = vec_make(1);
    st_add_direct(por_data.strans_independency, (st_data_t)id, (st_data_t)v);
  }
  else {
    /* ここでキーがidなエントリーが既に独立性情報テーブル内に存在していることはないはず */
    lmn_fatal("implementation error");
  }
}

/** RC_EXPANDEDから新規状態へのTransitionをnew_statesに, 新規状態ではない未展開の状態へのTransitionをunexpand_sに積む.
 * 各遷移に対し, 仮IDを割り振る */
static void ample_redundunt_states(StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states, Vector *unexpand_s)
{
  Vector *expanded;
  st_table_t succ_tbl;
  unsigned long i, expanded_num;

  succ_tbl = st_init_ptrtable();

  /** 状態登録 */
  expanded = RC_EXPANDED(rc);
  expanded_num = vec_num(expanded);
  for (i = 0; i < expanded_num; i++) {
    Transition src_t;
    st_data_t tmp;
    State *src_succ, *succ;

    src_t    = (Transition)vec_get(expand_res, i);
    src_succ = transition_next_state(src_t);

    /** ハッシュ表から状態が出現済みか否かを探索する */
    if (state_space_lookup(states, src_succ, succ) == 0) { /** src_succが状態空間に存在しない */
      if (st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp) == 0) { /* かつ多重辺ではない */
        vec_push(new_states, (vec_data_t)src_t);
        st_add_direct(succ_tbl, (st_data_t)succ, (st_data_t)src_t);
        transition_set_id(src_t, por_data.next_strans_id++);
        independency_table_insert_id(src_t);
      } else { /* 多重辺は消す */
        transition_add_rule((Transition)tmp, transition_rule(src_t, 0));
        transition_free(src_t);
      }
    } else {                  /** すでに同じ状態がある */
      state_free(src_succ);
      if (st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp) == 0) { /* 多重辺ではない */
        st_add_direct(succ_tbl, (st_data_t)succ, (st_data_t)src_t);
        transition_set_state(src_t, succ);
        transition_set_id(src_t, por_data.next_strans_id++);
        if (!is_expanded(succ)) {
          vec_push(unexpand_s, (vec_data_t)succ_t);
        }
      }
      else { /* 多重辺は消す */
        transition_add_rule((Transition)tmp, transition_rule(src_t, 0));
        transition_free(src_t);
      }
    }
    state_table_add_direct(por_data.States_POR, succ);
  }

  /** finalize */
  st_free_table(succ_tbl);
}

/* 状態空間statesへ新規状態new_statesを追加し, 未展開状態unexpand_sと合わせてsのサクセサとして登録する */
static void ample_store_successors(StateSpace states, State *s, Vector *new_states, Vector *unexpand_t, BOOL flags)
{
  unsigned int i, n;

  n = vec_num(unexpand_t);
  for (i = 0; i < n; i++) {
    set_open(s);
    state_succ_add(s, (Transition)vec_get(unexpand_t, i));
  }

  n = vec_num(new_states);
  for (i = 0; i < n; i++) {
    Transition succ_t;
    State *succ, *tmp;
    succ_t = (Transition)vec_get(new_states, i);
    succ = transition_next_state(succ_t);
    tmp  = state_space_insert(states, s);
    if (tmp == succ) {
      if (is_dump(flags)) dump_state_data(succ);
      if (lmn_env.compact_stack && lmn_env.use_compress_mem) { /* メモリ最適化のために、サクセッサの作成後に膜を解放する */
        state_free_mem(succ);
      }
    } else { /* basically, unexpected */
      state_free(succ);
      transition_set_state(succ_t, tmp);
    }
    state_succ_add(s, succ_t);
  }
}

/** 生成された新規状態new_statesから, reduction可能な状態をreductionしたベクタに置き換える. */
static void ample_inner(StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states)
{
  RC_CLEAR_DATA(rc);

  if (check0_independency(states, s, rc, new_states)) {
    check1_calculate_with_lemma();
  }

  /* C1から導かれるLemmaを満たすような，sで可能な遷移の集合を求める．
   * この処理により，sにおいてC1を満足するためには絶対にample(s)内に含めておかなくてはならない
   * 遷移の集合がpor_data.ample_candidate内にPUSHされる */
  st_foreach(por_data.strans_independency, build_ample_satisfying_lemma, (st_data_t)s);

  /* ここでpor_data.ample_candidateが空の場合は，sで可能なすべての遷移が互いに独立であることになるので，
   * その中でC2，C3を共に満足する1本をpor_data.ample_candidateの要素とする */
  if (vec_is_empty(por_data.ample_candidate)) {
    unsigned int i;
    BOOL found_proper_candidate;

    found_proper_candidate = FALSE;
    for (i = 0; i < state_succ_num(s); ++i) {
      vec_push(por_data.ample_candidate, (LmnWord)transition_id(transition(s, i)));
      if (check_C2(s) && check_C3(states, s)) {
        found_proper_candidate = TRUE;
        break;
      } else {
        /* 選択した遷移がC2もしくはC3のいずれかに反したため，これを候補から除外する */
        vec_clear(por_data.ample_candidate);
      }
    }
    /* sにおいてC1を満たす遷移は存在したものの，さらにC2もC3も満たすものが1本も存在しない場合はen(s)を返して終了する */
    if (!found_proper_candidate) {
      expanded = push_succstates_to_expanded(s);
      if (expanded == NULL) { lmn_fatal("unexpected"); }
      finalize_ample();
      return expanded;
    }
  } else {
    /* por_data.ample_candidateが空でない場合，まずpor_data.ample_candidate = en(s)であるかどうかチェックする．
     * por_data.ample_candidate = en(s)である場合(i.e. en(s)内のすべての遷移が互いに依存している場合)はen(s)を返して終了する．
     *
     * por_data.ample_candidate != en(s)の場合は，por_data.ample_candidate内の各遷移がC2およびC3を共に満足することを確認する．
     * このチェックに通らない場合，C0〜C3をすべて満足するようなen(s)の真部分集合は存在しないことになるため，
     * C0に従い，en(s)を返して終了する．
     */
    if (vec_num(por_data.ample_candidate) == state_succ_num(s) ||
        !check_C2(s) || !check_C3(states, s)) {
      expanded = push_succstates_to_expanded(s);
      if (expanded == NULL) { lmn_fatal("unexpected"); }
      finalize_ample();
      return expanded;
    }
  }

  /******************************************************************
   * この段階で少なくとも状態sにおいてはC0〜C3のすべてを満たすen(s)の
   * 真部分集合por_data.ample_candidateが求まっていることになる．
   * ここからは，sから始まるfull-stateグラフを対象にこれがC1を
   * 満足しているか否かチェックしていく．
   ******************************************************************/
  if (!check_C1(states, s)) {
    /* C1〜C3をすべて満足するample(s)が決定不能のため，C0に従いen(s)を返して終了する */
    expanded = push_succstates_to_expanded(s);
    if (expanded == NULL) { lmn_fatal("unexpected"); }
    finalize_ample();
    return expanded;
  }

  /* C0〜C3をすべて満たすen(s)の真部分集合が求まったので，s->successor[i]->succ_stateの内，
   * s->successor[i]->idがpor_data.ample_candidate内に含まれているものをテーブルexpanded内に重複なく放り込んでいく．
   * その際，テーブルに放り込まれた各Stateにample(s)のメンバーに選ばれた旨のフラグを立てておく． */
  expanded = push_ample_to_expanded(s);

  /* finalize */
  finalize_ample();

  return expanded;
}



/**
 * 遷移stransがample(s)内のすべての遷移と互いに独立であれば真を返す
 */
static BOOL is_independent_of_ample(Transition strans) {
  unsigned int i;
  unsigned long id;
  st_data_t vec_independency;

  for (i = 0; i < vec_num(por_data.ample_candidate); ++i) {
    id = (unsigned long)vec_get(por_data.ample_candidate, i);
    if(st_lookup(por_data.strans_independency, (st_data_t)id, (st_data_t *)&vec_independency)) {
      if (!vec_contains((Vector *)vec_independency, (LmnWord)transition_id(strans))) {
        return FALSE;
      }
    } else {
      LMN_ASSERT(FALSE);
    }
  }
  return TRUE;
}

/* 互いに独立な遷移(それぞれのIDはi1, i2)のエントリーが独立性情報テーブル内に存在するか否かをチェックする．
 * エントリーが存在しない場合は偽を返して終了する．エントリーが存在する場合は，要素が重複しないようにIDをpushし，真を返す．
 *
 * 例えば，キーがi1なるエントリー(値はVector)内にまだi2が存在しないならば，i2をVector内にpushし，
 * 同時にキーがi2なるエントリー内にはi1をpushする．
 * (逆にキーがi1なるエントリー内に既にi2が存在するような場合は，重複を避けるためi2をVector内にpushしないようにする) */
static BOOL push_independent_strans_to_table(unsigned long i1, unsigned long i2) {
  st_data_t v1, v2;
  unsigned int k;
  BOOL is_new_id;

  if (!st_lookup(por_data.strans_independency, (st_data_t)i1, (st_data_t *)&v1)) {
    LMN_ASSERT(!st_lookup(por_data.strans_independency, (st_data_t)i2, (st_data_t *)&v2));
    return FALSE; /* i1のエントリーが存在しない場合 */
  } else {
    for (k = 0, is_new_id = TRUE; k < vec_num((Vector *)v1); ++k) {
      if ((unsigned long)vec_get((Vector *)v1, k) == i2) {
        /* [i1]--> ... i2 ... のようになっているため，(i1,i2)∈Iなる情報は独立性情報テーブル内に
         * 既に反映されている旨のフラグを立てる */
        is_new_id = FALSE;
      }
    }
    if (is_new_id) { /* i1，i2のエントリーにそれぞれi1，i2をPUSHする */
      if (st_lookup(por_data.strans_independency, (st_data_t)i2, (st_data_t *)&v2)) {
        #ifdef DEBUG
        for (k = 0; k < vec_num((Vector *)v2); ++k) {
          if ((unsigned long)vec_get((Vector *)v2, k) == i1) {
            /* is_new_idが真であることと矛盾する */
            LMN_ASSERT(FALSE);
          }
        }
        #endif
        vec_push((Vector *)v1, (LmnWord)i2);
        vec_push((Vector *)v2, (LmnWord)i1);
      } else {
        /* 万が一ここに入った場合は，i1のエントリーが存在するにも関わらず，これと独立なi2のエントリーが
         * 存在しないことになり，独立性情報テーブルの対称性が崩れてしまっていることになる．
         * 基本的にここに入ることは考えられない． */
        LMN_ASSERT(FALSE);
      }
    }
    return TRUE;
  }
}

/**
 * テーブルexpanded内の各要素をpor_data.States_POR内に放り込む．
 * ここで衝突が発生した場合は重複して生成された状態を解放し，同時に，
 * por_data.succ_strans内の遷移の中でその解放対象となった状態へのポインタを持つものがあれば，
 * その指し示す先をテーブルpor_data.States_POR内の対応する状態に書換える処理を行う．
 *
 * 本関数はst_foreach内から呼び出す形で使用し，実行後にテーブルexpandedは空になる．
 */
static void expand_States_POR(State *s) {
  st_data_t s_on_table;
  unsigned int i;

  if (!st_lookup(por_data.States_POR, (st_data_t)s, (st_data_t *)&s_on_table)) {
    st_add_direct(por_data.States_POR, (st_data_t)s, (st_data_t)s);
  } else {
    for (i = 0; i < vec_num(por_data.succ_strans); ++i) {
      Transition t = (Transition)vec_get(por_data.succ_strans, i);
      if (transition_next_state(t) == s) {
        transition_set_state(t, (State *)s_on_table);
      }
    }
    state_free(s);
  }
}

static void gen_successors(const StateSpace states, State *s)
{
  Vector *expanded;
  Vector *expanded_rules;
  Vector *succ_strans;
  unsigned long expanded_num, i;
  struct ReactCxt rc;

  nd_react_cxt_init(&rc, DEFAULT_STATE_ID);
  succ_strans = vec_make(32);

  nd_gen_successors(state_mem(s), DEFAULT_STATE_ID, &rc);

  expanded = RC_EXPANDED(&rc);
  expanded_rules = RC_EXPANDED_RULES(&rc);
  expanded_num = vec_num(expanded);
  for (i = 0; i < expanded_num; i++) {
    Transition t = (Transition)vec_get(expanded, i);
    vec_push(succ_strans,
             (LmnWord)transition_make_with_id(transition_next_state(t),
                                              por_data.next_strans_id++,
                                              lmn_rule_get_name((LmnRule)vec_get(expanded_rules, i))));
  }

  for (i = 0; i < expanded_num; i++) {
    expand_States_POR(transition_next_state((Transition)vec_get(expanded, i)));
  }

  while (!vec_is_empty(succ_strans)) {
    state_succ_add(s, (Transition)vec_pop(succ_strans));
  }

  set_expanded(s); /* 展開済フラグを立てる */

  nd_react_cxt_destroy(&rc);
  vec_free(succ_strans);
  vec_free(expanded);
}

/**
 * 状態sにおいて可能な遷移(ルール適用)間の独立性を調べ，独立性情報テーブルを拡張する．
 * 独立性のチェックが完了したならば，sに独立性チェック済フラグを立てる(set_independency_checked(s) @mc.h)．
 *
 * (Step 1)
 *   expand/1(c.f. nd.c)を用いてsに対してルール適用を行い，sから直接可能なすべての遷移(構造体StateTransition)の集合en(s)を求める．
 *   en(s)の実体はpor_data.succ_strans(Successor StateTransitionsの略)なるVectorであり，これにPUSHされた各遷移(StateTransition *)は
 *   一意のグローバルなID，遷移先状態(expand/1中にテーブルexpandedに放り込まれたもの)，適用されたシステムルールを情報として持つ．
 *
 *   ここで|en(s)|>1 (i.e. sにおいて可能なルール適用が複数存在する)ならば，これらの間に独立性が存在するか否かチェックする必要が
 *   生じるため，次の<Step 2>へ進む．逆に，|en(s)|<=1 ならばsを起点とする遷移間で独立性を定義することはできないため，FALSEを
 *   返して終了する．
 *
 * (Step 2)
 *   ※ 以降，|en(s)|>1 が満たされる
 *   (Step 1)で求めたsから直接遷移可能な各状態に対してさらにexpand/1を適用する．
 *   ここで同型性判定の結果コンフリクトが検出されたならば，sからの2回の状態遷移において適用されてきたシステムルールの履歴を
 *   チェックする．チェックによって履歴の一致が確認されれば，2本の遷移それぞれに付与されたIDは互いに独立であると判定され，
 *   独立性情報テーブル（independency_table）内に情報がPUSHされる．
 *   正常に独立性情報テーブルの拡張できたならばTRUEを返す．
 */
static BOOL check0_independency(StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states)
{
  /** C0: |en(s)|<=1 ならば，C0によりただちに ample(s)=en(s) と決定される.
   *      よってFALSEを返しampleを終了する */
  unsigned int i, n;
  BOOL ret = FALSE;

  /* >>>>>>>>>>>>>>>>>>>> Step 1. <<<<<<<<<<<<<<<<<<<< */
  n = vec_num(new_states);
  if (vec_num(new_states) > 1) {
    ret = check0_independency_inner(states, s, rc, new_states);
  }
  set_independency_checked(s);
  return ret;
}

static BOOL check0_independency_inner(const StateSpace states, State *s, struct ReactCxt *rc, Vector *new_states) {

  unsigned int i, j, n;

  /* >>>>>>>>>>>>>>>>>>>> Step 2. <<<<<<<<<<<<<<<<<<<< */

  /* new_statesにある状態を更に1段階展開し, 仮置きする */
  n = vec_num(new_states);
  for (i = 0; i < n; i++) {
    State *succ;       /* sから直接遷移可能な状態 */
    Transition succ_t; /* s-->succ間の遷移 */

    succ_t = (Transition)vec_get(new_states, i);
    succ   = transition_next_state(s2ss);

    RC_CLEAR_DATA(rc);

    /* succから可能な遷移および遷移先状態をすべて求める．
     * ただしこの段階では，succからの各遷移に付与されたIDは仮のものである． */
    if (mc_data.has_property) {
      AutomataState prop_atm_s;
      prop_atm_s = automata_get_state(mc_data.property_automata, state_property_state(succ));
      mc_gen_successors(succ, succ->mem, prop_atm_s, rc);
    } else {
      nd_gen_successors(succ->mem, DEFAULT_STATE_ID, rc);
    }

    ample_redundunt_states(states, succ, rc, unexpand_s);
  }

  /* sを起点とする遷移同士で独立な関係にあるものを調べ，独立性情報テーブルを更新する．
   * "gen_successors(ss);"した際に付けられた仮のIDは独立性有りと判定された際，
   * 対応するsを起点とする遷移に付与されているIDで書換えられる． */
  for (i = 0; i < state_succ_num(s) - 1; ++i) {
    /*
     *    s ----(s_i)---- ss1
     *    |                |
     *  (s_j)          (ss1_i2)
     *    |                |
     *   ss2 --(ss2_j2)--- t (= t1, t2)
     *
     * なる構造を検出したら，(ss2_j2)->id := (s_i)->id, (ss1_i2)->id := (s_j)->id
     * なるIDの書換えを行う．この書換えのための必要条件は，
     * (ss2_j2)->rule = (s_i)->rule /\ (ss1_i2)->rule = (s_j)->rule /\ t1 = t2 = t である
     */
    State *ss1, *ss2, *t1, *t2;
    Transition s_i, s_j, ss1_i2, ss2_j2;
    unsigned int i2, j2;

    s_i = transition(s, i);
    ss1 = transition_next_state(s_i);

    for (j = i+1; j < state_succ_num(s); ++j) {
      s_j = transition(s, j);
      ss2 = transition_next_state(s_j);

      for (i2 = 0; i2 < state_succ_num(ss1); ++i2) {
        ss1_i2 = transition(ss1, i2);
        t1 = transition_next_state(ss1_i2);

        for (j2 = 0; j2 < state_succ_num(ss2); ++j2) {
          ss2_j2 = transition(ss2, j2);
          t2 = transition_next_state(ss2_j2);

#ifdef UNDER_CONSTRUCTION
          if (t1 == t2
              && ss1_i2 != ss2_j2 /* ss1=ss2となった際に，同一の遷移同士で独立性を定義しないようにする */
              && ss2_j2->rule_name == s_i->rule
              && ss1_i2->rule_name == s_j->rule) {
#endif
          if (t1 == t2 && ss1_i2 != ss2_j2) {
            /* 遷移s_iと遷移s_jが独立であるため，IDを書換えた上で独立性情報テーブルを更新する */
            unsigned long alpha, beta;

            alpha = transition_id(ss2_j2) = transition_id(s_i);
            beta  = transition_id(ss1_i2) = transition_id(s_j);
            push_independent_strans_to_table(alpha, beta);
          }
        }
      }
    }
  }

  /* s--(s2ss)-->ss なる状態ssで可能な遷移の内，独立性情報テーブル上に
   * まだエントリーが作成されていないものについてエントリーの新規作成を行う．
   * ここで対象となる遷移は，sから可能な遷移と独立でないものに限られる．
   * (∵独立なものはIDを書換えられた上でテーブル内に整理済のため) */
  for (i = 0; i < state_succ_num(s); ++i) {
    State *ss;
    Transition s2ss;

    s2ss = transition(s, i);
    ss = transition_next_state(s2ss);
    for (j = 0; j < state_succ_num(ss); ++j) {
      st_data_t t;
      unsigned long id;
      BOOL is_new_id;

      id = transition_id(transition(ss, j));
      is_new_id = !st_lookup(por_data.strans_independency, (st_data_t)id, (st_data_t *)&t);
      if (is_new_id) {
        Vector *v = vec_make(1);
        st_add_direct(por_data.strans_independency, (st_data_t)id, (st_data_t)v);
      }
    }
  }

  set_independency_checked(s);
  return TRUE;
}

/**
 * Lemma: en(s)\ample(s)内のすべての遷移は，ample(s)内のすべての遷移と互いに独立である必要がある
 *        (i.e. en(s)内の遷移同士が互いに依存している(= 独立でない)場合は，これらを両方共ample(s)内に含めておく必要がある)
 *
 * C1から導かれる上記のLemmaを満足するような遷移(ただし，en(s)内に含まれるもの)の集合を求める．
 * 本関数は高階関数st_foreach内で呼び出される形で使用されるため，en(s)内のすべての遷移が互いに独立であるような場合は
 * ample(s)の候補por_data.ample_candidateが空のままになる．この場合は後で必要な処理をしてやるものとする．
 *
 * 次に処理の内容を具体例で示す．
 * -----------------------------------------
 * 独立性情報テーブルの例:
 *   [0]--> 1
 *   [1]--> 0 2
 *   [2]--> 1
 *   [3]
 *   [6]
 *
 * 例えば en(s) = {0,1,2} のもとでLemmaを満たす遷移の集合を考える．
 * ここで{3,6} はen(s)の部分集合ではないので，予めチェック対象からは除外しておく．
 *
 * 独立性情報テーブルを参照すると，ID=0なる遷移と独立なのは1をIDとして持つ遷移のみであり，
 * ID=2なる遷移とは依存関係にあることが分かる．ゆえにLemmaにより，ample(s)には少なくとも
 * 0と2を両方含めておく必要があることが分かる．
 *
 * 同様にID=1なる遷移に注目すると，これは0とも2とも独立であるため，1については必ずしも
 * ample(s)の要素に含める必要はないことが分かる．
 *
 * 最後にID=2についても同様のチェックを行うことで，Lemmaを満たす
 * ample(s)の候補(Vector *por_data.ample_candidate)は{0,2}と求まる．
 *
 * このような処理をここでは行う．
 */
static int build_ample_satisfying_lemma(st_data_t key, st_data_t val, st_data_t current_state) {
  unsigned long id_key = (unsigned long)key;      /* 現在チェック中のエントリーのキー */
  Vector *ids_independent_of_id_key = (Vector *)val; /* キーのIDを持つ遷移と独立な遷移が持つIDを管理するVector */
  State *s = (State *)current_state;

  unsigned int i, j, k;
  unsigned long checked_id; /* キーのIDを持つ遷移と依存関係がある可能性のある遷移のIDを一時的に管理する */
  BOOL is_dependent,        /* checked_id なるIDを持つ遷移がキーのIDと依存関係にあるならば真 */
       need_to_push_id_key; /* is_dependentが真の場合，キーのIDもample(s)内に放り込む必要が生じる．このような時に真． */


  for (i = 0, need_to_push_id_key = FALSE; i < state_succ_num(s); ++i) {
    if (id_key == transition_id(transition(s, i))) {
      /* 現在チェックしているテーブル上のエントリーが，sから可能な遷移の中のi番目のもの(id_key)に対応している */
      for (j = 0; j < state_succ_num(s); ++j) {
        /* sから可能な遷移の内，ids_independent_of_id_key(= エントリーの値)に含まれていないものが
         * あるかどうかチェックする．含まれていないものがid_keyなるIDを持つ遷移と依存関係にあるものであるので，
         * このような遷移とid_keyをample(s)の候補に放り込む */
        if (j != i) { /* j=iの場合はチェック不要 */
          checked_id = transition_id(transition(s, j));

          for (k = 0, is_dependent = TRUE; k < vec_num(ids_independent_of_id_key); ++k) {
            if (checked_id == vec_get(ids_independent_of_id_key, k)) {
              is_dependent = FALSE;
              break;
            }
          }
          if (is_dependent) {
            need_to_push_id_key = TRUE;
            if (!vec_contains(por_data.ample_candidate, (LmnWord)checked_id)) {
              vec_push(por_data.ample_candidate, (LmnWord)checked_id);
            }
          }
        }
      }
      if (need_to_push_id_key && !vec_contains(por_data.ample_candidate, (LmnWord)id_key)) {
        vec_push(por_data.ample_candidate, (LmnWord)id_key);
      }
      break;
    }
  }
  return ST_CONTINUE;
}


/**
 * ample(s)が満たすべき必要条件の1つであるC1の検査を行う．
 *
 * sを起点とし，かつpor_data.ample_candidateの要素に選ばれなかった遷移から始まる
 * 任意の経路P上において次の性質Fが満たされていることを確認する．
 *
 * F: P上においてpor_data.ample_candidate内のいずれかの要素が現れるまでの間に出現する
 *    すべての遷移はpor_data.ample_candidate内のすべての遷移と互いに独立である
 *
 * 上記の性質Fを満たさないsを起点とする経路が少なくとも1つ存在するならば，
 * por_data.ample_candidateはC1を満たしていないことになるので偽を返して終了する．
 * C1を満足することが確認されたならば真が返される．
 *
 * このC1の検査は，sを起点とするstate graph(の中で必要な部分)をBFSで構築しながら進めていく．
 */
static BOOL check_C1(const StateSpace states, State *s) {

  unsigned int i;
  State *ss;
  Transition strans, strans2;

  /* init */
  vec_clear(por_data.Stack_POR);
  for (i = 0; i < state_succ_num(s); ++i) {
    strans = transition(s, i);
    if (!vec_contains(por_data.ample_candidate, (LmnWord)transition_id(strans))) {
      /* sで可能かつample(s)の候補に含まれない遷移をスタック上に乗せる */
      vec_push(por_data.Stack_POR, (LmnWord)strans);
    }
  }

  while (!vec_is_empty(por_data.Stack_POR)) {
    strans = (Transition)vec_pop_n(por_data.Stack_POR, 0U); /* スタックはFIFO */
    if (!is_independent_of_ample(strans)) {
      /* Fに反する経路Pが検出されたので偽を返して終了する */
      return FALSE;
    }

    ss = transition_next_state(strans);
    if (!is_independency_checked(ss)) {
      independency_check(states, ss);
      for (i = 0; i < state_succ_num(ss); ++i) {
        strans2 = transition(ss, i);
        if (!vec_contains(por_data.ample_candidate, (LmnWord)transition_id(strans2))) {
          /* ample(s)内に含まれない遷移はさらにチェックする必要があるのでスタック上に乗せる */
          vec_push(por_data.Stack_POR, (LmnWord)strans2);
        }
      }
    }
  }
  return TRUE;
}

/**
 * ample(s)が満たすべき必要条件の1つであるC2の検査を行う．
 *
 * sで可能な遷移の内，por_data.ample_candidate内に含まれるIDを持つものが
 * すべてinvisibleであるならば真を返す．
 * (少なくとも1つがvisibleであるならば偽を返す)
 *
 * 注) 本検査はLTLモデル検査実行時のみ有意義であるため，
 * 　　そうでない場合(非決定実行の場合)は無条件に真を返すようにする．
 */
static BOOL check_C2(State *s) {
  if (lmn_env.ltl) {
    unsigned int i;
    Transition strans;
    for (i = 0; i < state_succ_num(s); ++i) {
      strans = transition(s, i);
      if (vec_contains(por_data.ample_candidate, (LmnWord)transition_id(strans))) {
#ifdef UNDER_CONSTRUCTION
        if (!lmn_rule_is_invisible(strans->rule)) {
          return FALSE;
        }
#endif
      }
    }
  }
  return TRUE;
}

/**
 * ample(s)が満たすべき必要条件の1つであるC3の検査を行う．
 *
 * sで可能な遷移の内，por_data.ample_candidate内に含まれるIDを持つものによって
 * 行き着くStateがStack上に乗っているならば偽を返す(不完全な閉路形成の禁止)
 */
static BOOL check_C3(const StateSpace states, State *s) {
  unsigned int i;
  Transition succ_t;
  State *succ;
  for (i = 0; i < state_succ_num(s); ++i) {
    succ_t = transition(s, i);
    succ = transition_next_state(succ_t);
    if (vec_contains(por_data.ample_candidate, (LmnWord)transition_id(succ_t))) {
      /* por_data.States_POR上に存在するss自体はStackに積まれていない．
       * (∵Stackに積まれているのはStates上に存在するStateのため)
       * ゆえに，ssに相当するStates上のStateがStack上に存在するか否かチェックする必要がある */
      State *ss_on_States;

      if ((ss_on_States = state_space_get(states, succ))) {
        if (ss_on_States != NULL &&
            is_open((State *)ss_on_States)) {
          return FALSE;
        }
      }
    }
  }
  return TRUE;
}

#endif /* COMMENT */
