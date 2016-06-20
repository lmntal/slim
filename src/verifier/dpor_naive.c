/*
 * dpor_naive.c
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

#include "dpor.h"
#include "dpor_naive.h"
#include "delta_membrane.h"
#include "mc.h"
#include "mc_worker.h"
#include "../rule.h"
#ifdef PROFILE
# include "runtime_status.h"
#endif


/* 概要:
 *   Sasaki P.O.RコードをRev.108から復刻した. (gocho Rev.422)
 *   探索空間(ss)とは別に, 代表経路を選択するための一時的な状態空間(tmp)を構築することで,
 *   ssのグラフを動的に半順序簡約を行う.
 *   ssのグラフがTransitionオブジェクトを使用しなくても構わないことに対し,
 *   tmpのグラフはTransitionオブジェクトを使用する.
 *
 * TODO:
 *   Transitionオブジェクトを開放すべきどこかで開放忘れがある. (メモリリークがある.)
 *   C2(不可視な遷移の選択によるStutter Equivalent保証)が未実装.
 *   Rev.108よりReduction率がなぜか弱い.
 */


struct McPorData {
  State      *root;
  st_table_t strans_independency; /* 独立性情報テーブル:
                                   *   構造体StateTransitionのidをキーとし
                                   *   bins[id]は高々1個のエントリー(Vector)を持つ．
                                   *   Vectorには, キーであるidの遷移と独立関係にある遷移idが積まれる. */
  st_table_t states;              /* ample(s)計算中のみ使用．展開されたすべてのStateを管理． */
  Queue      *queue;              /* C1のチェックにあたってstate graphを展開するする際に使用 */
  Vector     *ample_candidate;    /* ample(s)の候補を管理するVector．本Vector内のすべての遷移が，C0〜C3のチェック対象となる */
  LmnReactCxt *rc;
  unsigned long next_strans_id;
  BOOL       flags;
} mc_por;


/** Macros
 */
#define POR_TABLE_SIZE      (512U)
#define POR_VEC_SIZE         (32U)
#define POR_STRANS_SIZE       (4U)
#define POR_ID_INITIALIZER    (1U)

#define INDEPENDENCY_CHECKED_MASK  (0x01U)
#define REPRESENTATIVE_MASK        (0x01U << 1)
#define POR_EXPANDED_MASK          (0x01U << 2)
#define POR_INSERTED_MASK          (0x01U << 3)
#define POR_OUTSIDE_MASK           (0x01U << 4)

#define set_independency_checked(S)    (state_flags3(S) |=   INDEPENDENCY_CHECKED_MASK)
#define unset_independency_checked(S)  (state_flags3(S) &= (~INDEPENDENCY_CHECKED_MASK))
#define is_independency_checked(S)     (state_flags3(S) &    INDEPENDENCY_CHECKED_MASK)
#define set_ample(S)                   (state_flags3(S) |=   REPRESENTATIVE_MASK)
#define unset_ample(S)                 (state_flags3(S) &= (~REPRESENTATIVE_MASK))
#define is_ample(S)                    (state_flags3(S) &    REPRESENTATIVE_MASK)
#define set_por_expanded(S)            (state_flags3(S) |=   POR_EXPANDED_MASK)
#define unset_por_expanded(S)          (state_flags3(S) &= (~POR_EXPANDED_MASK))
#define is_por_expanded(S)             (state_flags3(S) &    POR_EXPANDED_MASK)
#define set_inserted(S)                (state_flags3(S) |=   POR_INSERTED_MASK)
#define unset_inserted(S)              (state_flags3(S) &= (~POR_INSERTED_MASK))
#define is_inserted(S)                 (state_flags3(S) &    POR_INSERTED_MASK)
#define set_outside_exist(S)           (state_flags3(S) |=   POR_OUTSIDE_MASK)
#define unset_outside_exist(S)         (state_flags3(S) &= (~POR_OUTSIDE_MASK))
#define is_outside_exist(S)            (state_flags3(S) &    POR_OUTSIDE_MASK)


/** ProtoTypes
 */
static int destroy_tmp_state_graph(State *s, LmnWord _d);
static void finalize_ample(BOOL org_f);
static BOOL ample(StateSpaceRef  ss,
                  State       *s,
                  LmnReactCxt *rc,
                  Vector      *new_s,
                  BOOL        org_f);
static void por_gen_successors(State *s, LmnReactCxt *rc, AutomataRef a, Vector *psyms);
static void por_store_successors(State *s, LmnReactCxt *rc, BOOL is_store);
static int  independency_vec_free(st_data_t _k, st_data_t vec, st_data_t _a);
static BOOL independency_check(State *s, AutomataRef a, Vector *psyms);
static BOOL check_C1(State *s, AutomataRef a, Vector *psyms);
static BOOL check_C2(State *s);
static BOOL check_C3(StateSpaceRef  ss,
                     State       *s,
                     LmnReactCxt *rc,
                     Vector      *new_ss,
                     BOOL        f);
static BOOL is_independent_of_ample(TransitionRef strans);
static BOOL push_independent_strans_to_table(unsigned long i1, unsigned long i2);
static int build_ample_satisfying_lemma(st_data_t key,
                                        st_data_t val,
                                        st_data_t current_state);
static void push_ample_to_expanded(StateSpaceRef  ss,
                                   State       *s,
                                   LmnReactCxt *rc,
                                   Vector      *new_ss,
                                   BOOL        f);
static BOOL push_succstates_to_expanded(StateSpaceRef  ss,
                                        State       *s,
                                        LmnReactCxt *rc,
                                        Vector      *new_ss,
                                        BOOL        f);
/* FOR DEBUG ONLY */
int dump__strans_independency(st_data_t key, st_data_t vec, st_data_t _a);
void dump__ample_candidate(void);
int dump__tmp_graph(st_data_t _k, st_data_t _v, st_data_t _a);

/**
 * PORの変数やデータ構造の初期化を行う
 */
void init_por_vars() {
  mc_por.root                = NULL;
  mc_por.strans_independency = st_init_numtable();
  mc_por.states              = st_init_statetable();
  mc_por.queue               = new_queue();
  mc_por.ample_candidate     = vec_make(POR_VEC_SIZE);
  mc_por.next_strans_id      = POR_ID_INITIALIZER; /* 0は使用しない */
  mc_por.rc                  = NULL;
  mc_por.flags               = 0x00U;
}


void free_por_vars() {
  st_free_table(mc_por.states);
  st_free_table(mc_por.strans_independency);
  q_free(mc_por.queue);
  vec_free(mc_por.ample_candidate);
  if (mc_por.rc) {
    mc_react_cxt_destroy(mc_por.rc);
  }
}


void por_calc_ampleset(StateSpaceRef  ss,
                       State       *s,
                       LmnReactCxt *rc,
                       Vector      *new_s,
                       BOOL        f)
{
  if (!mc_por.rc) {
    mc_por.rc = LMN_MALLOC(struct LmnReactCxt);
    mc_react_cxt_init(mc_por.rc);
    mc_por.flags = f;
    mc_unset_por(mc_por.flags);
    mc_set_trans(mc_por.flags);
    mc_unset_dump(mc_por.flags);
    POR_DEBUG(mc_set_dump(mc_por.flags));
  }

  if (mc_react_cxt_expanded_num(rc) <= 1) {
    /* C0: |en(s)|<=1 --> C0によりample(s)=en(s) と決定 */
    mc_store_successors(ss, s, rc, new_s, f);
    return;
  }

  mc_por.root = s;
  if (ample(ss, s, rc, new_s, f)) {
    /* C0〜C3をすべて満たすen(s)の真部分集合ample(s)が決定 */
    push_ample_to_expanded(ss, s, rc, new_s, f);
  } else {
    /* λ.. ample(s)=en(s) */
    push_succstates_to_expanded(ss, s, rc, new_s, f);
  }

  finalize_ample(f);
}


static int independency_vec_free(st_data_t _k, st_data_t vec, st_data_t _a)
{
  vec_free((Vector *)vec);
  return ST_DELETE;
}


/* PORのために一時的に構築した状態空間上の頂点を削除する. */
static int destroy_tmp_state_graph(State *s, LmnWord _a)
{
  if (s != mc_por.root) {
    if (is_outside_exist(s)) {
      /* 展開元の状態から1stepで遷移可能な頂点(rootのサクセサ)が状態空間に追加されている場合 */
      state_succ_clear(s);
      s->flags3 = 0x00U;
    } else {
      /* それ以外は, rootでなければ開放 */
      state_free(s);
    }
  }
  else if (!mc_has_trans(((BOOL)_a))) {
    /* rootなら, サクセッサの遷移オブジェクトを調整 */
    unsigned int i;
    for (i = 0; i < state_succ_num(s); i++) {
      TransitionRef succ_t;
      State *succ_s;
      succ_t = transition(s, i);
      succ_s = transition_next_state(succ_t);
      transition_free(succ_t);
      s->successors[i] = (succ_data_t)succ_s;
    }
    unset_trans_obj(s);
  }

  return ST_DELETE;
}


static void finalize_ample(BOOL org_f)
{
  mc_por.next_strans_id = POR_ID_INITIALIZER;
  st_foreach(mc_por.strans_independency, (st_iter_func)independency_vec_free, (st_data_t)0);
  st_foreach(mc_por.states, (st_iter_func)destroy_tmp_state_graph, (LmnWord)org_f);
  queue_clear(mc_por.queue);
  vec_clear(mc_por.ample_candidate);
  RC_CLEAR_DATA(mc_por.rc);
  mc_por.root = NULL;
}



/* 状態sとsから遷移可能な状態集合をrcとして入力し,
 * reduced state graphに必要なサクセッサを状態空間ssとsに登録する.
 * ample(s)=en(s)の場合にFALSEを返す. */
static BOOL ample(StateSpaceRef  ss,
                  State       *s,
                  LmnReactCxt *rc,
                  Vector      *new_s,
                  BOOL        org_f)
{
  set_ample(s);
  set_por_expanded(s);
  st_add_direct(mc_por.states, (st_data_t)s, (st_data_t)s);

  por_store_successors(s, rc, TRUE);

  /* sから2stepの状態空間を立ち上げ, 遷移間の独立性情報テーブルを展開 */
  if (!independency_check(s, statespace_automata(ss), statespace_propsyms(ss))) {
    return FALSE;
  }

  /* - sから可能な遷移の内, 依存関係にある遷移の集合をample_candidateに積む.
   * -- C1から導かれるLemmaを満たすような，sで可能な遷移の集合を求める．
   * -- この処理では，sにおいてC1を満足するためには絶対にample(s)内に
   *    含めておかなくてはならない遷移の集合をample_candidate内にPUSHする. */
  st_foreach(mc_por.strans_independency, (st_iter_func)build_ample_satisfying_lemma, (st_data_t)s);

  /* ここでample_candidateが空の場合は，sで可能なすべての遷移が互いに独立であることになるので，
   * その中でC2，C3を共に満足する1本をample_candidateの要素とする */
  if (vec_is_empty(mc_por.ample_candidate)) {
    unsigned int i;
    for (i = 0; i < state_succ_num(s); i++) {
      TransitionRef t = transition(s, i);
      set_ample(transition_next_state(t));
      vec_push(mc_por.ample_candidate, (vec_data_t)transition_id(t));
//      if (check_C2(s) && check_C3(ss, s, rc, new_s, org_f)) {
      if (check_C1(s, statespace_automata(ss), statespace_propsyms(ss)) && check_C2(s)) {
        break;
      } else {
        /* 選択した遷移はC2もしくはC3のいずれかに反したため，次の候補を検査する */
        unset_ample(transition_next_state(t));
        vec_clear(mc_por.ample_candidate);
      }
    }

    /* sにおいてC1を満たす遷移は存在したものの
     * さらにC2もC3も満たすものが1本も存在しない場合はen(s)を返して終了する */
    if (vec_is_empty(mc_por.ample_candidate)) {
      return FALSE;
    }
  }
  else {
    /* - ample_candidateが空でない場合，まずample_candidate = en(s)であるかどうかチェックする．
     * - ample_candidate = en(s)である場合(i.e. en(s)内のすべての遷移が互いに依存している場合)は
     *   en(s)を返して終了する.
     * - ample_candidate != en(s)の場合は
     *   ample_candidate内の各遷移がC2およびC3を共に満足することを確認する．
     * - このチェックに通らない場合，
     *   C0〜C3をすべて満足するようなen(s)の真部分集合は存在しないことになるため，
     *   C0に従い，en(s)を返して終了する．
     */
//    if (vec_num(mc_por.ample_candidate) == state_succ_num(s) ||
//        !check_C2(s) || !check_C3(ss, s, rc, new_s, org_f)) {
    if (vec_num(mc_por.ample_candidate) == state_succ_num(s) ||
        !check_C1(s, statespace_automata(ss), statespace_propsyms(ss)) || !check_C2(s)) {
      return FALSE;
    }
  }

  /******************************************************************
   * この段階で少なくとも状態sにおいてはC0〜C3のすべてを満たすen(s)の
   * 真部分集合ample_candidateが求まっていることになる．
   * ここからは，sから始まるfull-stateグラフを対象にこれがC1を
   * 満足しているか否かチェックしていく．
   ******************************************************************/
 // if (!check_C1(s, statespace_automata(ss), statespace_propsyms(ss))) {
  if (!check_C3(ss, s, rc, new_s, org_f)) {
    /* C1〜C3をすべて満足するample(s)が決定不能のため，C0に従いen(s)を返して終了する */
    return FALSE;
  }

  POR_DEBUG({
    printf("*** C1--3 ok! ample set calculated\n");
    st_foreach(mc_por.strans_independency, dump__strans_independency, (st_data_t)0);
    dump__ample_candidate();
  });

  return TRUE;
}


static void por_gen_successors(State *s, LmnReactCxt *rc, AutomataRef a, Vector *psyms)
{
  LmnMembrane *mem;
  mem = state_restore_mem(s);
  if (a) {
    AutomataStateRef p_s = MC_GET_PROPERTY(s, a);
    mc_gen_successors_with_property(s, mem, p_s, rc, psyms, mc_por.flags);
  } else {
    mc_gen_successors(s, mem, DEFAULT_STATE_ID, rc, mc_por.flags);
  }

  if (mc_use_compress(mc_por.flags)) {
    if (!state_mem(s)) { /* compact-stack */
      lmn_mem_drop(mem);
      lmn_mem_free(mem);
    } else {
      state_free_mem(s);
    }
  }
}


static inline State *por_state_insert(State *succ, struct MemDeltaRoot *d)
{
  State *ret;
  st_data_t t;
  LmnMembrane *tmp_m;

  if (d) {
    dmem_root_commit(d);
    state_set_mem(succ, DMEM_ROOT_MEM(d));
    state_calc_hash(succ, state_mem(succ), lmn_env.mem_enc);
    tmp_m = NULL;
  } else {
    tmp_m = state_mem(succ);
  }

  t = 0;
  if (st_lookup(mc_por.states, (st_data_t)succ, (st_data_t *)&t)) {
    ret = (State *)t;
  } else {
    LmnBinStrRef bs;
    st_add_direct(mc_por.states, (st_data_t)succ, (st_data_t)succ);
    if (!is_encoded(succ)) {
      bs = state_calc_mem_dump(succ);
      state_set_binstr(succ, bs);
    }
    ret = succ;
    POR_DEBUG({state_id_issue(succ);});
  }

  if (d) {
    dmem_root_revert(d);
  } else if (ret == succ && tmp_m) {
    lmn_mem_free_rec(tmp_m);
  }

  return ret;
}


static inline State *por_state_insert_statespace(StateSpaceRef ss,
                                                 TransitionRef succ_t,
                                                 State      *succ_s,
                                                 Vector     *new_ss,
                                                 BOOL       org_f)
{
  State *t;
  LmnMembrane *succ_m;

  succ_m = state_mem(succ_s);
  t = statespace_insert(ss, succ_s);

  set_inserted(t);
  if (t == succ_s) {
    set_outside_exist(t);
    state_id_issue(succ_s);
    if (mc_is_dump(org_f)) dump_state_data(succ_s, (LmnWord)stdout, (LmnWord)NULL);
    if (succ_m) lmn_mem_free_rec(succ_m);
    if (new_ss) vec_push(new_ss, (vec_data_t)succ_s);
  }
  else {
    transition_set_state(succ_t, t);
  }

  return t;
}


static inline void por_store_successors_inner(State *s, LmnReactCxt *rc)
{
  st_table_t succ_tbl;
  unsigned int i, succ_i;

  succ_i   = 0;
  succ_tbl = RC_SUCC_TBL(rc);

  for (i = 0; i < mc_react_cxt_expanded_num(rc); i++) {
    TransitionRef src_t;
    st_data_t tmp;
    State *src_succ, *succ;
    MemDeltaRoot *d;


    if (has_trans_obj(s)) {
      src_t = (TransitionRef)vec_get(RC_EXPANDED(rc), i);
      src_succ = transition_next_state(src_t);
    } else {
      src_succ = (State *)vec_get(RC_EXPANDED(rc), i);
      src_t = transition_make(src_succ, (lmn_interned_str)vec_get(RC_EXPANDED_RULES(rc), i));
    }

    d = RC_MC_USE_DMEM(rc) ? (MemDeltaRoot *)vec_get(RC_MEM_DELTAS(rc), i)
                           : NULL;
    succ = por_state_insert(src_succ, d);
    if (succ != src_succ) {
      state_free(src_succ);
      transition_set_state(src_t, succ);
    }

    tmp = 0;
    if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) {
      st_add_direct(succ_tbl, (st_data_t)succ, (st_data_t)src_t);
      vec_set(RC_EXPANDED(rc), succ_i, (vec_data_t)src_t);
      if (RC_MC_USE_DMEM(rc)) {
        vec_set(RC_MEM_DELTAS(rc), succ_i, vec_get(RC_MEM_DELTAS(rc), i));
      }
      succ_i++;
    } else {
      transition_add_rule((TransitionRef)tmp,
                          transition_rule(src_t, 0),
                          transition_cost(src_t));
      transition_free(src_t);
    }
  }

  RC_EXPANDED(rc)->num = succ_i;
  RC_EXPANDED_RULES(rc)->num = succ_i;
  if (RC_MC_USE_DMEM(rc)) {
    RC_MEM_DELTAS(rc)->num = succ_i;
  }


  if (!has_trans_obj(s)) {
    set_trans_obj(s);
  }

  if (s->successors) {
    printf("unexpected.\n");
    dump_state_data((State *)(s->successors[0]), (LmnWord)stdout, (LmnWord)NULL);
  }

  state_succ_set(s, RC_EXPANDED(rc));
  st_clear(RC_SUCC_TBL(rc));
}


/* ReactCxtに生成してあるサクセッサをPOR状態空間と状態sに登録する.
 * 同時に, 遷移オブジェクトに対して, 仮IDを発行する.
 * なお, サクセッサに対する遷移オブジェクトが存在しない場合はこの時点でmallocを行う.
 * 入力したis_storeが真である場合は, IDの発行と同時に独立性情報テーブルの拡張を行う */
static void por_store_successors(State *s, LmnReactCxt  *rc, BOOL is_store)
{
  unsigned int i;

  por_store_successors_inner(s, rc);
  for (i = 0; i < state_succ_num(s); i++) {
    TransitionRef succ_t;
    unsigned long assign_id = 0;

    /* 1. transitionに仮idを設定 */
    succ_t = transition(s, i);
    if (transition_id(succ_t) == 0) {
      assign_id = mc_por.next_strans_id++;
      transition_set_id(succ_t, assign_id);
    }

    if (assign_id == 0) continue;

    /* 2. 独立性情報テーブルに仮idを登録 */
    if (is_store) {
      st_data_t t = 0;
      if (!st_lookup(mc_por.strans_independency, (st_data_t)assign_id, (st_data_t *)&t)) {
        Vector *v = vec_make(1);
        st_add_direct(mc_por.strans_independency, (st_data_t)assign_id, (st_data_t)v);
      } else {
        lmn_fatal("transition id: illegal assignment");
      }
    }
  }
}


/**
 * 状態sにおいて可能な遷移(ルール適用)間の独立性を調べ，独立性情報テーブルを拡張する．
 * 独立性のチェックが完了したならば，sに独立性チェック済フラグを立てる
 * (set_independency_checked(s))．
 *
 * (Step 1)
 *   sに対してルール適用を行い，
 *   sから直接可能なすべての遷移(構造体StateTransition)の集合en(s)を求める．
 *   en(s)の実体はsucc_strans(Successor StateTransitionsの略)なるVectorであり,
 *   これにPUSHされた各遷移(StateTransition *)は
 *       一意のグローバルなID
 *       遷移先状態(expand/1中にテーブルexpandedに放り込まれたもの)
 *       適用されたシステムルール
 *   を情報として持つ．
 *
 *   ここで|en(s)|>1 (i.e. sにおいて可能なルール適用が複数存在する)ならば，
 *   これらの間に独立性が存在するか否かチェックする必要が生じる. (<Step 2>へ進む)
 *   逆に，|en(s)|<=1 ならばsを起点とする遷移間で独立性を定義することはできないため，
 *   FALSEを返して終了する．
 *
 * (Step 2)
 *   ※ 以降，|en(s)|>1 が満たされる
 *   (Step 1)で求めたsから直接遷移可能な各状態に対してさらにexpand/1を適用する．
 *   ここで同型性判定の結果コンフリクトが検出されたならば，
 *   sからの2回の状態遷移において適用されてきたシステムルールの履歴をチェックする．
 *   チェックによって履歴の一致が確認されれば，2本の遷移それぞれに付与されたIDは互いに独立であると判定.
 *   独立性情報テーブル（independency_table）内に情報がPUSHし,
 *   正常に独立性情報テーブルの拡張できたならばTRUEを返す．
 */
static BOOL independency_check(State *s, AutomataRef a, Vector *psyms)
{

  unsigned int i, j;

  /* >>>>>>>>>>>>>>>>>>>> Step 1. <<<<<<<<<<<<<<<<<<<< */
  if (!is_por_expanded(s)) {
    por_gen_successors(s, mc_por.rc, a, psyms);
    por_store_successors(s, mc_por.rc, TRUE);

    RC_CLEAR_DATA(mc_por.rc);
    set_por_expanded(s);
  }

  /* |en(s)|<=1の場合はsで可能な遷移間で
   * 独立性を定義することができないのでFALSEを返して終了する */
  if (state_succ_num(s) <= 1) {
    set_independency_checked(s);
    return FALSE;
  }

  /* >>>>>>>>>>>>>>>>>>>> Step 2. <<<<<<<<<<<<<<<<<<<< */

  /* sから2step目の遷移を経て到達可能なすべての状態を求める */
  for (i = 0; i < state_succ_num(s); i++) {
    TransitionRef succ_t;
    State    *succ_s;

    succ_t = transition(s, i);
    succ_s = transition_next_state(succ_t);

    if (!is_por_expanded(succ_s)) {
      /* ssから可能な遷移および遷移先状態をすべて求める．
       * ただしこの段階では，ssからの各遷移に付与されたIDは仮のものである． */
      por_gen_successors(succ_s, mc_por.rc, a, psyms);
      por_store_successors(succ_s, mc_por.rc, FALSE); /* 2step目の遷移のIDはテーブルに登録しない */
      RC_CLEAR_DATA(mc_por.rc);
      set_por_expanded(succ_s);
    }
  }


  POR_DEBUG({
    printf("\nbefore\n");
    st_foreach(mc_por.states, dump__tmp_graph, (st_data_t)FALSE);
    printf("\n");
  });

  /* sを起点とする遷移同士で独立な関係にあるものを調べ，独立性情報テーブルを更新する．
   * "por_gen_successors"した際に付けられた仮のIDは独立性有りと判定された際，
   * 対応するsを起点とする遷移に付与されているIDで書換えられる． */
 // por_successor_comb_foreach(s, por_update_independency_tbl);
  for (i = 0; i < state_succ_num(s) - 1; i++) {
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
    TransitionRef s_i, s_j, ss1_i2, ss2_j2;
    unsigned int i2, j2;

    s_i = transition(s, i);
    ss1 = transition_next_state(s_i);

    for (j = i+1; j < state_succ_num(s); j++) {
      s_j = transition(s, j);
      ss2 = transition_next_state(s_j);

      for (i2 = 0; i2 < state_succ_num(ss1); i2++) {
        ss1_i2 = transition(ss1, i2);
        t1 = transition_next_state(ss1_i2);

        for (j2 = 0; j2 < state_succ_num(ss2); j2++) {
          ss2_j2 = transition(ss2, j2);
          t2 = transition_next_state(ss2_j2);

          if (t1 == t2 &&
              ss1_i2 != ss2_j2 /* ss1=ss2となった際に，同一の遷移同士で独立性を定義しないようにする */
              ) {
            /* 遷移s_iと遷移s_jが独立であるため，IDを書換えた上で独立性情報テーブルを更新する */
            unsigned long alpha, beta;

            alpha = transition_id(s_i);
            beta  = transition_id(s_j);
            transition_set_id(ss2_j2, alpha);
            transition_set_id(ss1_i2, beta);
            push_independent_strans_to_table(alpha, beta);
          }
        }
      }
    }
  }



  POR_DEBUG({
    printf("after\n");
    st_foreach(mc_por.states, dump__tmp_graph, (st_data_t)FALSE);
    printf("\n");
  });

  /* s--(s2ss)-->ss なる状態ssで可能な遷移の内，独立性情報テーブル上に
   * まだエントリーが作成されていないものについてエントリーの新規作成を行う．
   * ここで対象となる遷移は，sから可能な遷移と独立でないものに限られる．
   * (∵独立なものはIDを書換えられた上でテーブル内に整理済のため) */
  for (i = 0; i < state_succ_num(s); i++) {
    State *ss;
    TransitionRef s2ss;

    s2ss = transition(s, i);
    ss   = transition_next_state(s2ss);
    for (j = 0; j < state_succ_num(ss); j++) {
      st_data_t t;
      unsigned long id;

      id = transition_id(transition(ss, j));
      t = 0;
      if (!st_lookup(mc_por.strans_independency, (st_data_t)id, (st_data_t *)&t)) {
        Vector *v = vec_make(1);
        st_add_direct(mc_por.strans_independency, (st_data_t)id, (st_data_t)v);
      } /* else 独立な遷移 */
    }
  }

  set_independency_checked(s);
  return TRUE;
}



/**
 * ample(s)が満たすべき必要条件の1つであるC1の検査を行う．
 *
 * sを起点とし，かつample_candidateの要素に選ばれなかった遷移から始まる
 * 任意の経路P上において次の性質Fが満たされていることを確認する．
 *
 * F: P上においてample_candidate内のいずれかの要素が現れるまでの間に出現する
 *    すべての遷移はample_candidate内のすべての遷移と互いに独立である
 *
 * 上記の性質Fを満たさないsを起点とする経路が少なくとも1つ存在するならば，
 * ample_candidateはC1を満たしていないことになるので偽を返して終了する．
 * C1を満足することが確認されたならば真が返される．
 *
 * このC1の検査は，sを起点とするstate graph(の中で必要な部分)をBFSで構築しながら進めていく．
 */
static BOOL check_C1(State *s, AutomataRef a, Vector *psyms)
{
  unsigned int i;

  for (i = 0; i < state_succ_num(s); i++) {
    TransitionRef t = transition(s, i);
    if (!vec_contains(mc_por.ample_candidate, (vec_data_t)transition_id(t))) {
      /* sで可能かつample(s)の候補に含まれない遷移をスタック上に乗せる */
      enqueue(mc_por.queue, (vec_data_t)t);
    }
  }

  while (!is_empty_queue(mc_por.queue)) {
    TransitionRef succ_t = (TransitionRef)dequeue(mc_por.queue);
    if (!is_independent_of_ample(succ_t)) {
      /* Fに反する経路Pが検出されたので偽を返して終了する */
      POR_DEBUG({
         printf("   λ.. C1 violate_id::%lu\n", transition_id(succ_t));
         st_foreach(mc_por.strans_independency, dump__strans_independency, (st_data_t)0);
         dump__ample_candidate();
         st_foreach(mc_por.states, dump__tmp_graph, (st_data_t)FALSE);
         printf("\n");
       });
      return FALSE;
    } else {
      State *succ_s = transition_next_state(succ_t);
//      if (!is_independency_checked(succ_s) && !is_expanded(succ_s)) {
      if (!is_independency_checked(succ_s)) {
        independency_check(succ_s, a, psyms);
        for (i = 0; i < state_succ_num(succ_s); i++) {
          TransitionRef succ_succ_t = transition(succ_s, i);
          if (!vec_contains(mc_por.ample_candidate, (vec_data_t)transition_id(succ_succ_t))) {
            /* ample(s)内に含まれない遷移はさらにチェックする必要がある */
            enqueue(mc_por.queue, (LmnWord)succ_succ_t);
          }
        }
      }
    }
  }
  return TRUE;
}


/**
 * ample(s)が満たすべき必要条件の1つであるC2の検査を行う．
 *
 * sで可能な遷移の内，ample_candidate内に含まれるIDを持つものが
 * すべてinvisibleであるならば真を返す．
 * (少なくとも1つがvisibleであるならば偽を返す)
 *
 * 注) 本検査はLTLモデル検査実行時のみ有意義であるため，
 * 　　そうでない場合(非決定実行の場合)は無条件に真を返すようにする．
 */
static BOOL check_C2(State *s)
{
  if (lmn_env.ltl) {
    unsigned int i;
    for (i = 0; i < state_succ_num(s); i++) {
      TransitionRef t = transition(s, i);
      if (vec_contains(mc_por.ample_candidate, (vec_data_t)transition_id(t))) {
        /* TODO: 設計と実装 */
//        if (!lmn_rule_is_invisible(strans->rule)) {
          return FALSE;
//        }
      }
    }
  }
  return TRUE;
}


/* 状態空間に対して登録をしかけた頂点succと,
 * 状態空間が返した頂点t(登録に成功していた場合はsucc, 等価状態がいた場合はその状態)を
 * 入力として受け, Cycle Ignoring Problemのための検査結果を返す. */
static inline BOOL C3_cycle_proviso_satisfied(State *succ, State *t)
{
  if (succ == t) {
    /* General Visited Proviso:
     *  既存状態への再訪問でない(新規状態への遷移)なら閉路形成を行う遷移ではない.
     *  Hash-Based分割と併用するとサクセッサの情報を取得するための通信で遅くなる. */
    return TRUE;
  }
  else if (t == mc_por.root) {
    /* self-loop detection */
    return FALSE;
  }
  else if (is_on_stack(t)) {
    /* Stack Proviso:
     *  Stack上の状態に戻るということは閉路であるということ
     *  DFS Stackによる空間構築(逐次)が前提 */
    return FALSE;
  }
  else if (lmn_env.bfs && is_expanded(t) && lmn_env.core_num == 1) {
    /* Open Set Proviso:
     *  閉路形成を行なう遷移は,
     *  展開済み状態へ再訪問する遷移のサブセットである.(逐次限定) */
    return FALSE;
  }

  return TRUE;
}


/**
 * ample(s)が満たすべき必要条件の1つであるC3の検査を行う．
 *
 * sで可能な遷移の内，ample_candidate内に含まれるIDを持つものによって
 * 行き着くStateがStack上に乗っているならば偽を返す(不完全な閉路形成の禁止)
 */
static BOOL check_C3(StateSpaceRef  ss,
                     State       *s,
                     LmnReactCxt *rc,
                     Vector      *new_ss,
                     BOOL        f)
{
  unsigned int i;

  if (!mc_has_property(f)) return TRUE;

  for (i = 0; i < state_succ_num(s); i++) {
    TransitionRef succ_t;
    State     *succ_s, *t;

    succ_t = transition(s, i);
    succ_s = transition_next_state(succ_t);

    if (!vec_contains(mc_por.ample_candidate, (vec_data_t)transition_id(succ_t))) {
      continue;
    }

    /* POR用の状態空間ではなく,
     * 本来の状態空間に対して等価性検査をしかけ, 新規であれば追加してしまう */
    t = por_state_insert_statespace(ss, succ_t, succ_s, new_ss, f);

    if (!C3_cycle_proviso_satisfied(succ_s, t)) {
      return FALSE;
    }
  }

  return TRUE;
}


/**
 * 遷移stransがample(s)内のすべての遷移と互いに独立であれば真を返す
 */
static BOOL is_independent_of_ample(TransitionRef strans)
{
  unsigned int i;

  for (i = 0; i < vec_num(mc_por.ample_candidate); i++) {
    unsigned long id;
    st_data_t vec_independency;

    id = (unsigned long)vec_get(mc_por.ample_candidate, i);
    vec_independency = 0;
    if(st_lookup(mc_por.strans_independency, (st_data_t)id, (st_data_t *)&vec_independency)) {
      if (!vec_contains((Vector *)vec_independency, (vec_data_t)transition_id(strans))) {
        return FALSE;
      }
    } else {
      lmn_fatal("unexpected.");
    }
  }

  return TRUE;
}


/* 互いに独立な遷移(それぞれのIDはi1, i2)のエントリーが
 * 独立性情報テーブル内に存在するか否かをチェックする．
 * エントリーが存在しない場合は偽を返して終了する．
 * エントリーが存在する場合は，要素が重複しないようにIDをpushし，真を返す．
 * 例えば，キーがi1なるエントリー(値はVector)内にまだi2が存在しないならば，i2をVector内にpushし，
 * 同時にキーがi2なるエントリー内にはi1をpushする．
 * (逆にキーがi1なるエントリー内に既にi2が存在するような場合は
 *  重複を避けるためi2をVector内にpushしないようにする) */
static BOOL push_independent_strans_to_table(unsigned long i1, unsigned long i2)
{
  st_data_t v1, v2;
  v1 = 0;
  v2 = 0;
  if (!st_lookup(mc_por.strans_independency, (st_data_t)i1, (st_data_t *)&v1)) {
    LMN_ASSERT(!st_lookup(mc_por.strans_independency, (st_data_t)i2, (st_data_t *)&v2));
    return FALSE; /* i1のエントリーが存在しない場合 */
  }
  else {
    unsigned int k;
    BOOL is_new_id;

    is_new_id = TRUE;
    for (k = 0; k < vec_num((Vector *)v1); k++) {
      if ((unsigned long)vec_get((Vector *)v1, k) == i2) {
        /* [i1]--> ... i2 ... のようになっているため，(i1,i2)∈Iなる情報は独立性情報テーブル内に
         * 既に反映されている旨のフラグを立てる */
        is_new_id = FALSE;
        break;
      }
    }

    if (is_new_id) { /* i1，i2のエントリーにそれぞれi1，i2をPUSHする */
      if (st_lookup(mc_por.strans_independency, (st_data_t)i2, (st_data_t *)&v2)) {
        POR_DEBUG({
          unsigned int _k;
          for (_k = 0; _k < vec_num((Vector *)v2); _k++) {
            if ((unsigned long)vec_get((Vector *)v2, _k) == i1) {
              /* is_new_idが真であることと矛盾する */
              LMN_ASSERT(FALSE);
            }
          }
        });
        vec_push((Vector *)v1, (vec_data_t)i2);
        vec_push((Vector *)v2, (vec_data_t)i1);
      } else {
        /* 万が一ここに入った場合は，i1のエントリーが存在するにも関わらず，これと独立なi2のエントリーが
         * 存在しないことになり，独立性情報テーブルの対称性が崩れてしまっていることになる．
         * 基本的にここに入ることは考えられない． */
        lmn_fatal("unexpected.");
      }
    }

    return TRUE;
  }
}



/**
 * Lemma: en(s)\ample(s)内のすべての遷移は，ample(s)内のすべての遷移と互いに独立である必要がある
 *        (i.e. en(s)内の遷移同士が互いに依存している(= 独立でない)場合は，これらを両方共ample(s)内に含めておく必要がある)
 *
 * C1から導かれる上記のLemmaを満足するような遷移(ただし，en(s)内に含まれるもの)の集合を求める．
 * 本関数は高階関数st_foreach内で呼び出される形で使用されるため，en(s)内のすべての遷移が互いに独立であるような場合は
 * ample(s)の候補ample_candidateが空のままになる．この場合は後で必要な処理をしてやるものとする．
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
 * ample(s)の候補(Vector *ample_candidate)は{0,2}と求まる．
 *
 * このような処理をここでは行う．
 */

static int build_ample_satisfying_lemma(st_data_t key,
                                        st_data_t val,
                                        st_data_t current_state)
{
  unsigned long id_key;              /* 現在チェック中のエントリーのキー */
  Vector *ids_independent_of_id_key; /* キーのIDを持つ遷移と独立な遷移が持つIDを管理するVector */
  State *s;
  unsigned int i, j, k;
  BOOL need_to_push_id_key; /* ID=is_keyの遷移と依存関係にある遷移が存在する場合,
                             * キーのIDもample(s)内に放り込む必要が生じる.
                             * このような時に真． */

  id_key = (unsigned long)key;
  ids_independent_of_id_key = (Vector *)val;
  s = (State *)current_state;
  need_to_push_id_key = FALSE;

  /* sから可能な遷移の内，ids_independent_of_id_key(= エントリーの値)に
   * >>含まれていない<<IDの遷移があるかどうかチェックする．
   * 含まれていないIDの遷移が, ID=id_keyの遷移と依存関係にある遷移Tである.
   * このような遷移Tとid_keyをample(s)の候補に放り込む */

  for (i = 0; i < state_succ_num(s); i++) {
    /* 目的のID以外はskip.. */
    TransitionRef trans_key = transition(s, i);
    if (transition_id(trans_key) != id_key) continue;

    /* 現在チェックしているテーブル上のエントリーが
     * sから可能な遷移の中のi番目(id_key)に対応 */
    for (j = 0; j < state_succ_num(s); j++) {
      TransitionRef check;
      unsigned long checked_id; /* キーのIDを持つ遷移と依存関係がある可能性のある遷移IDを一時的に管理 */
      BOOL is_dependent;        /* checked_id なるIDを持つ遷移がキーのIDと依存関係にあるならば真 */
      need_to_push_id_key = FALSE;

      if (j == i) continue; /* j=iの場合はチェック不要 */

      check = transition(s, j);
      checked_id   = transition_id(check);
      is_dependent = TRUE;

      for (k = 0; k < vec_num(ids_independent_of_id_key); k++) {
        if (checked_id == (unsigned long)vec_get(ids_independent_of_id_key, k)) {
          is_dependent = FALSE;
          break;
        }
      }

      if (is_dependent) {
        need_to_push_id_key = TRUE;
        if (!vec_contains(mc_por.ample_candidate, (vec_data_t)checked_id)) {
          vec_push(mc_por.ample_candidate, (vec_data_t)checked_id);
          set_ample(transition_next_state(check));
        }
      }
    }

    if (need_to_push_id_key && !vec_contains(mc_por.ample_candidate, (vec_data_t)id_key)) {
      vec_push(mc_por.ample_candidate, (vec_data_t)id_key);
      set_ample(transition_next_state(trans_key));
    }

    break;
  }
  return ST_CONTINUE;
}


static void push_ample_to_expanded(StateSpaceRef  ss,
                                   State       *s,
                                   LmnReactCxt *rc,
                                   Vector      *new_ss,
                                   BOOL        f)
{
  if (state_succ_num(s) > 0) {
    struct Vector tmp;
    unsigned int i;
    vec_init(&tmp, 32);

    for (i = 0; i < state_succ_num(s); i++) {
      TransitionRef  succ_t;
      State      *succ_s;

      succ_t = transition(s, i);
      succ_s = transition_next_state(succ_t);

      if (is_inserted(succ_s) && is_outside_exist(succ_s)) {
        /* C3 check時, 探索空間への追加に成功してしまっていた場合 */
        vec_push(&tmp, (vec_data_t)succ_t);
      } else if (!vec_contains(mc_por.ample_candidate, (vec_data_t)transition_id(succ_t))) {
        /* amplesetに含まれない遷移は除去 */
        transition_free(succ_t);
      }
      else {
        /* 探索空間へ追加していない(しているがcontainsの場合) /\ amplesetに含める遷移 */
        if (!is_inserted(succ_s)) {
          por_state_insert_statespace(ss, succ_t, succ_s, new_ss, f);
        }
        vec_push(&tmp, (vec_data_t)succ_t);
      }
    }

    LMN_FREE(s->successors);
    s->successors = NULL;
    s->successor_num = 0;
    state_succ_set(s, &tmp);
    vec_destroy(&tmp);
  }
}


/**
 * ample(s)=en(s)であるときのみ使用．
 * 状態sの次の状態をすべてテーブルexpanded内に重複なくpushし，真を返す．
 * expanded内にpushする際，ample(s)内に含まれることを表すフラグを立てる．
 * ただし，sが未展開の場合やsから直接遷移可能な状態が存在しない場合は偽を返す．
 */
static BOOL push_succstates_to_expanded(StateSpaceRef  ss,
                                        State       *s,
                                        LmnReactCxt *rc,
                                        Vector      *new_ss,
                                        BOOL        f)
{
  BOOL ret = FALSE;
  if (is_por_expanded(s) && state_succ_num(s) > 0) {
    unsigned int i;
    ret = TRUE;
    for (i = 0; i < state_succ_num(s); i++) {
      TransitionRef succ_t;
      State     *succ_s;

      succ_t = transition(s, i);
      succ_s = transition_next_state(succ_t);

      if (!is_inserted(succ_s)) {
        por_state_insert_statespace(ss, succ_t, succ_s, new_ss, f);
      }
    }
  }

  return ret;
}


/* FOR DEBUG ONLY */
int dump__strans_independency(st_data_t key, st_data_t vec, st_data_t _a)
{
  Vector *v;
  unsigned long id;
  unsigned int i;

  v = (Vector *)vec;
  id = (unsigned long)key;

  fprintf(stdout, "[%lu]-->", id);
  for (i = 0; i < vec_num(v); i++) {
    fprintf(stdout, " %lu", (unsigned long)vec_get(v, i));
  }
  fprintf(stdout, "\n");

  return ST_CONTINUE;
}


/* FOR DEBUG ONLY */
void dump__ample_candidate()
{
  unsigned int i;
  fprintf(stdout, "ample:");
  for (i = 0; i < vec_num(mc_por.ample_candidate); ++i) {
    fprintf(stdout, " %lu", (unsigned long)vec_get(mc_por.ample_candidate, i));
  }
  fprintf(stdout, "\n");
}


int dump__tmp_graph(st_data_t _k, st_data_t _v, st_data_t _a)
{
  FILE *f;
  State *s;
  unsigned int i;
  BOOL is_formated;

  s = (State *)_v;
  f = stdout;
  is_formated = (BOOL)_a;

  fprintf(f, "%lu::", state_format_id(s, is_formated));
  if (s->successors) {
    for (i = 0; i < state_succ_num(s); i++) {

      if (i > 0) fprintf(f, ",");

      fprintf(f, "%lu", state_format_id(state_succ_state(s, i), is_formated));

      if (has_trans_obj(s)) {
        TransitionRef t;
        unsigned int j;

        t = transition(s, i);
        fprintf(f, "(%lu:", transition_id(t));

        for (j = 0; j < transition_rule_num(t); j++) {
          if (j > 0) fprintf(f, " ");
          fprintf(f, "%s", lmn_id_to_name(transition_rule(t, j)));
        }
        fprintf(f, ")");
      }
    }
  }
  fprintf(f, "\n");
  return ST_CONTINUE;
}


