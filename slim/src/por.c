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


unsigned long next_strans_id;

static BOOL independency_check(const StateSpace states, State *s);
static BOOL is_independent_of_ample(StateTransition *strans);
/* LMN_EXTERN int dump__strans_independency(st_data_t key, st_data_t vec, st_data_t _a); */
/* LMN_EXTERN void dump__ample_candidate(void); */

/**
 * PORの変数やデータ構造の初期化を行う
 */
void init_por_vars() {
  States_POR = st_init_table(&type_statehash);
  strans_independency = st_init_numtable();
  succ_strans = vec_make(5);
  ample_candidate = vec_make(1);
  Stack_POR = vec_make(256);
  next_strans_id = 0U;
}

void free_por_vars() {
  st_free_table(States_POR);
  st_free_table(strans_independency);
  vec_free(succ_strans);
  vec_free(ample_candidate);
  vec_free(Stack_POR);
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
static BOOL check_C1(const StateSpace states, State *s) {

  unsigned int i;
  State *ss;
  StateTransition *strans, *strans2;

  /* init */
  vec_clear(Stack_POR);
  for (i = 0; i < vec_num(&s->successor); ++i) {
    strans = (StateTransition *)vec_get(&s->successor, i);
    if (!vec_contains(ample_candidate, (LmnWord)strans->id)) {
      /* sで可能かつample(s)の候補に含まれない遷移をスタック上に乗せる */
      vec_push(Stack_POR, (LmnWord)strans);
    }
  }

  while (!vec_is_empty(Stack_POR)) {
    strans = (StateTransition *)vec_pop_n(Stack_POR, 0U); /* スタックはFIFO */
    if (!is_independent_of_ample(strans)) {
      /* Fに反する経路Pが検出されたので偽を返して終了する */
      return FALSE;
    }

    ss = strans->succ_state;
    if (!is_independency_checked(ss)) {
      independency_check(states, ss);
      for (i = 0; i < vec_num(&ss->successor); ++i) {
        strans2 = (StateTransition *)vec_get(&ss->successor, i);
        if (!vec_contains(ample_candidate, (LmnWord)strans2->id)) {
          /* ample(s)内に含まれない遷移はさらにチェックする必要があるのでスタック上に乗せる */
          vec_push(Stack_POR, (LmnWord)strans2);
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
static BOOL check_C2(State *s) {
  if (lmn_env.ltl) {
    unsigned int i;
    StateTransition *strans;
    for (i = 0; i < vec_num(&s->successor); ++i) {
      strans = (StateTransition *)vec_get(&s->successor, i);
      if (vec_contains(ample_candidate, (LmnWord)strans->id)) {
        if (!lmn_rule_is_invisible(strans->rule)) {
          return FALSE;
        }
      }
    }
  }
  return TRUE;
}

/**
 * ample(s)が満たすべき必要条件の1つであるC3の検査を行う．
 *
 * sで可能な遷移の内，ample_candidate内に含まれるIDを持つものによって
 * 行き着くStateがStack上に乗っているならば偽を返す(不完全な閉路形成の禁止)
 */
static BOOL check_C3(const StateSpace states, State *s) {
  unsigned int i;
  StateTransition *s2ss;
  State *ss;
  for (i = 0; i < vec_num(&s->successor); ++i) {
    s2ss = (StateTransition *)vec_get(&s->successor, i);
    ss = (State *)s2ss->succ_state;
    if (vec_contains(ample_candidate, (LmnWord)s2ss->id)) {
      /* States_POR上に存在するss自体はStackに積まれていない．
       * (∵Stackに積まれているのはStates上に存在するStateのため)
       * ゆえに，ssに相当するStates上のStateがStack上に存在するか否かチェックする必要がある */
      State *ss_on_States;

      if ((ss_on_States = state_space_get(states, ss))) {
        if (ss_on_States != NULL &&
            is_open((State *)ss_on_States)) {
          return FALSE;
        }
      }
    }
  }
  return TRUE;
}

/**
 * 遷移stransがample(s)内のすべての遷移と互いに独立であれば真を返す
 */
static BOOL is_independent_of_ample(StateTransition *strans) {
  unsigned int i;
  unsigned long id;
  st_data_t vec_independency;

  for (i = 0; i < vec_num(ample_candidate); ++i) {
    id = (unsigned long)vec_get(ample_candidate, i);
    if(st_lookup(strans_independency, (st_data_t)id, (st_data_t *)&vec_independency)) {
      if (!vec_contains((Vector *)vec_independency, (LmnWord)strans->id)) {
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

  if (!st_lookup(strans_independency, (st_data_t)i1, (st_data_t *)&v1)) {
    LMN_ASSERT(!st_lookup(strans_independency, (st_data_t)i2, (st_data_t *)&v2));
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
      if (st_lookup(strans_independency, (st_data_t)i2, (st_data_t *)&v2)) {
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
 * テーブルexpanded内の各要素をStates_POR内に放り込む．
 * ここで衝突が発生した場合は重複して生成された状態を解放し，同時に，
 * succ_strans内の遷移の中でその解放対象となった状態へのポインタを持つものがあれば，
 * その指し示す先をテーブルStates_POR内の対応する状態に書換える処理を行う．
 *
 * 本関数はst_foreach内から呼び出す形で使用し，実行後にテーブルexpandedは空になる．
 */
static void expand_States_POR(State *s) {
  st_data_t s_on_table;
  unsigned int i;

  if (!st_lookup(States_POR, (st_data_t)s, (st_data_t *)&s_on_table)) {
    st_add_direct(States_POR, (st_data_t)s, (st_data_t)s);
  } else {
    for (i = 0; i < vec_num(succ_strans); ++i) {
      if (((StateTransition *)vec_get(succ_strans, i))->succ_state == s) {
        ((StateTransition *)vec_get(succ_strans, i))->succ_state = (State *)s_on_table;
      }
    }
    state_free(s);
  }
}

static void gen_successors(const StateSpace states, State *s)
{
  Vector *expanded;
  Vector *succ_strans;
  unsigned long expanded_num, i;
  
  succ_strans = vec_make(32);

  expanded = nd_expand(states, s);
  expanded_num = vec_num(expanded);
  for (i = 0; i < expanded_num; i++) {
    vec_push(succ_strans,
             (LmnWord)strans_make((State *)vec_get(expanded, i),
                                  next_strans_id++,
                                  state_rule((State *)vec_get(expanded, i))));
  }

  for (i = 0; i < expanded_num; i++) {
    expand_States_POR((State *)vec_get(expanded, i));
  }
  
  state_succ_init(s, vec_num(succ_strans));
  while (!vec_is_empty(succ_strans)) {
    vec_push(&s->successor, vec_pop(succ_strans));
  }

  set_expanded(s); /* 展開済フラグを立てる */
}

/**
 * 状態sにおいて可能な遷移(ルール適用)間の独立性を調べ，独立性情報テーブルを拡張する．
 * 独立性のチェックが完了したならば，sに独立性チェック済フラグを立てる(set_independency_checked(s) @mc.h)．
 *
 * (Step 1)
 *   expand/1(c.f. task.c)を用いてsに対してルール適用を行い，sから直接可能なすべての遷移(構造体StateTransition)の集合en(s)を求める．
 *   en(s)の実体はsucc_strans(Successor StateTransitionsの略)なるVectorであり，これにPUSHされた各遷移(StateTransition *)は
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
static BOOL independency_check(const StateSpace states, State *s) {

  unsigned int i, j;
  
  /* >>>>>>>>>>>>>>>>>>>> Step 1. <<<<<<<<<<<<<<<<<<<< */

  /* 状態sの次の状態がまだ完全に展開されていない場合はen(s)を求める */
  if (!is_expanded(s)) {

    /* sから直接遷移可能なすべての状態を生成すると共に，各状態への遷移StateTransitionを生成してVector succ_strans内に放り込む */
    gen_successors(states, s);

    /* s->successor[i]->idを独立性情報テーブル内に放り込んでいく
     * ここで放り込まれるidはまだ独立性情報テーブル内には存在しないはずのものである */
    for (i = 0; i < vec_num(&s->successor); ++i) {
      st_data_t t;
      unsigned long id;
      BOOL is_new_id;

      id = ((StateTransition *)vec_get(&s->successor, i))->id;
      is_new_id = !st_lookup(strans_independency, (st_data_t)id, (st_data_t *)&t);
      if (is_new_id) {
        Vector *v = vec_make(1);
        st_add_direct(strans_independency, (st_data_t)id, (st_data_t)v);
      } else {
        /* ここでキーがidなエントリーが既に独立性情報テーブル内に存在していることはないはず */
        LMN_ASSERT(FALSE);
      }
    }
  }

  /* |en(s)|<=1の場合はsで可能な遷移間で独立性を定義することができないのでFALSEを返して終了する */
  if (vec_num(&s->successor) <= 1) {
    set_independency_checked(s);
    return FALSE;
  }


  /* >>>>>>>>>>>>>>>>>>>> Step 2. <<<<<<<<<<<<<<<<<<<< */

  /* sから2回の遷移を経て到達可能なすべての状態を求める */
  for (i = 0; i < vec_num(&s->successor); ++i) {
    State *ss;             /* sから直接遷移可能な状態 */
    StateTransition *s2ss; /* s-->ss間の遷移 */

    s2ss = (StateTransition *)vec_get(&s->successor, i);
    ss = s2ss->succ_state;

    /* s<-->ss間に複数の遷移が存在する場合に重複して展開しないようにする */
    if (is_expanded(ss)) {
      continue;
    }

    /* ssから可能な遷移および遷移先状態をすべて求める．
     * ただしこの段階では，ssからの各遷移に付与されたIDは仮のものである． */
    gen_successors(states, ss);
  }

  /* sを起点とする遷移同士で独立な関係にあるものを調べ，独立性情報テーブルを更新する．
   * "gen_successors(ss);"した際に付けられた仮のIDは独立性有りと判定された際，
   * 対応するsを起点とする遷移に付与されているIDで書換えられる． */
  for (i = 0; i < vec_num(&s->successor) - 1; ++i) {
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
    StateTransition *s_i, *s_j, *ss1_i2, *ss2_j2;
    unsigned int i2, j2;

    s_i = (StateTransition *)vec_get(&s->successor, i);
    ss1 = s_i->succ_state;

    for (j = i+1; j < vec_num(&s->successor); ++j) {
      s_j = (StateTransition *)vec_get(&s->successor, j);
      ss2 = s_j->succ_state;

      for (i2 = 0; i2 < vec_num(&ss1->successor); ++i2) {
        ss1_i2 = (StateTransition *)vec_get(&ss1->successor, i2);
        t1 = ss1_i2->succ_state;

        for (j2 = 0; j2 < vec_num(&ss2->successor); ++j2) {
          ss2_j2 = (StateTransition *)vec_get(&ss2->successor, j2);
          t2 = ss2_j2->succ_state;

          if (t1 == t2
              && ss1_i2 != ss2_j2 /* ss1=ss2となった際に，同一の遷移同士で独立性を定義しないようにする */
              && ss2_j2->rule == s_i->rule
              && ss1_i2->rule == s_j->rule) {
            /* 遷移s_iと遷移s_jが独立であるため，IDを書換えた上で独立性情報テーブルを更新する */
            unsigned long alpha, beta;

            alpha = ss2_j2->id = s_i->id;
            beta  = ss1_i2->id = s_j->id;
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
  for (i = 0; i < vec_num(&s->successor); ++i) {
    State *ss;
    StateTransition *s2ss;

    s2ss = (StateTransition *)vec_get(&s->successor, i);
    ss = s2ss->succ_state;
    for (j = 0; j < vec_num(&ss->successor); ++j) {
      st_data_t t;
      unsigned long id;
      BOOL is_new_id;

      id = ((StateTransition *)vec_get(&ss->successor, j))->id;
      is_new_id = !st_lookup(strans_independency, (st_data_t)id, (st_data_t *)&t);
      if (is_new_id) {
        Vector *v = vec_make(1);
        st_add_direct(strans_independency, (st_data_t)id, (st_data_t)v);
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
static int build_ample_satisfying_lemma(st_data_t key, st_data_t val, st_data_t current_state) {
  unsigned long id_key = (unsigned long)key;      /* 現在チェック中のエントリーのキー */
  Vector *ids_independent_of_id_key = (Vector *)val; /* キーのIDを持つ遷移と独立な遷移が持つIDを管理するVector */
  State *s = (State *)current_state;

  unsigned int i, j, k;
  unsigned long checked_id; /* キーのIDを持つ遷移と依存関係がある可能性のある遷移のIDを一時的に管理する */
  BOOL is_dependent,        /* checked_id なるIDを持つ遷移がキーのIDと依存関係にあるならば真 */
       need_to_push_id_key; /* is_dependentが真の場合，キーのIDもample(s)内に放り込む必要が生じる．このような時に真． */


  for (i = 0, need_to_push_id_key = FALSE; i < vec_num(&s->successor); ++i) {
    if (id_key == ((StateTransition *)vec_get(&s->successor, i))->id) {
      /* 現在チェックしているテーブル上のエントリーが，sから可能な遷移の中のi番目のもの(id_key)に対応している */
      for (j = 0; j < vec_num(&s->successor); ++j) {
        /* sから可能な遷移の内，ids_independent_of_id_key(= エントリーの値)に含まれていないものが
         * あるかどうかチェックする．含まれていないものがid_keyなるIDを持つ遷移と依存関係にあるものであるので，
         * このような遷移とid_keyをample(s)の候補に放り込む */
        if (j != i) { /* j=iの場合はチェック不要 */
          checked_id = ((StateTransition *)vec_get(&s->successor, j))->id;

          for (k = 0, is_dependent = TRUE; k < vec_num(ids_independent_of_id_key); ++k) {
            if (checked_id == vec_get(ids_independent_of_id_key, k)) {
              is_dependent = FALSE;
              break;
            }
          }
          if (is_dependent) {
            need_to_push_id_key = TRUE;
            if (!vec_contains(ample_candidate, (LmnWord)checked_id)) {
              vec_push(ample_candidate, (LmnWord)checked_id);
            }
          }
        }
      }
      if (need_to_push_id_key && !vec_contains(ample_candidate, (LmnWord)id_key)) {
        vec_push(ample_candidate, (LmnWord)id_key);
      }
      break;
    }
  }
  return ST_CONTINUE;
}

static Vector *push_ample_to_expanded(State *s) {
  unsigned int i;
  State *ss;
  StateTransition *strans;
  Vector *expanded;
  st_table_t tbl;

  tbl = st_init_table(&type_statehash);

  expanded = vec_make(32);

  LMN_ASSERT(st_num(expanded) == 0);
  for (i = 0; i < vec_num(&s->successor); ++i) {
    strans = (StateTransition *)vec_get(&s->successor, i);
    ss = strans->succ_state;
    if (vec_contains(ample_candidate, (LmnWord)strans->id)) {
      /* stransがample(s)の要素として選ばれたので，ssをテーブルexpanded内に放り込む．
       * その際，ssがテーブル内で重複しないよう注意し，放り込むことができた場合は
       * ssがample(s)の要素である旨のフラグを立てておく． */
      st_table_t t;
      if (!st_lookup(tbl, (st_data_t)ss, (st_data_t *)&t)) {
        st_add_direct(tbl, (st_data_t)ss, (st_data_t)ss);
        vec_push(expanded, (LmnWord)ss);
        set_ample(ss);
      }
    }
  }
  st_free_table(tbl);
  return expanded;
}

/**
 * ample(s)=en(s)であるときのみ使用．
 * 状態sの次の状態をすべてテーブルexpanded内に重複なくpushし，真を返す．
 * expanded内にpushする際，ample(s)内に含まれることを表すフラグを立てる．
 * ただし，sが未展開の場合やsから直接遷移可能な状態が存在しない場合は偽を返す．
 */
static Vector *push_succstates_to_expanded(State *s)
{

  if (is_expanded(s) && !vec_is_empty(&s->successor)) {
    Vector *expanded;
    st_table_t tbl = st_init_table(&type_statehash);
    
    expanded = vec_make(32);
    unsigned int i;
    for (i = 0; i < vec_num(&s->successor); ++i) {
      StateTransition *s2ss = (StateTransition *)vec_get(&s->successor, i);
      State *ss = s2ss->succ_state;

      st_data_t t;
      if (!st_lookup(tbl, (st_data_t)ss, (st_data_t *)&t)) {
        st_add_direct(tbl, (st_data_t)ss, (st_data_t)ss);
        vec_push(expanded, (LmnWord)ss);
        set_ample(ss);
      }
    }
    st_free_table(tbl);
    return expanded;
  } else {
    return NULL;
  }
}

/* FOR DEBUG ONLY                                                               */
/* int dump__strans_independency(st_data_t key, st_data_t vec, st_data_t _a) {  */
/*   Vector *v = (Vector *)vec;                                                 */
/*   unsigned long id = (unsigned long)key;                                     */
/*   unsigned int i;                                                            */
/*   fprintf(stdout, "[%lu]-->", id);                                           */
/*   for (i = 0; i < vec_num(v); ++i) {                                         */
/*     fprintf(stdout, " %lu", (unsigned long)vec_get(v, i));                   */
/*   }                                                                          */
/*   fprintf(stdout, "\n");                                                     */
/*   return ST_CONTINUE;                                                        */
/* }                                                                            */
/*                                                                              */
/* void dump__ample_candidate() {                                               */
/*   unsigned int i;                                                            */
/*   fprintf(stdout, "ample:");                                                 */
/*   for (i = 0; i < vec_num(ample_candidate); ++i) {                           */
/*     fprintf(stdout, " %lu", (unsigned long)vec_get(ample_candidate, i));     */
/*   }                                                                          */
/*   fprintf(stdout, "\n");                                                     */
/* }                                                                            */

static int independency_vec_free(st_data_t _k, st_data_t vec, st_data_t _a) {
  vec_free((Vector *)vec);
  return ST_DELETE;
}

static int destroy_tmp_state_graph(st_data_t _k, st_data_t state, st_data_t _a) {
  unsigned int i;
  State *s = (State *)state;

  /* StateTransitionはすべて解放 */
  for (i = 0; i < vec_num(&s->successor); ++i) {
    strans_free((StateTransition *)vec_get(&s->successor, i));
  }
  if (!vec_is_empty(&s->successor)) { /* メモリーリークを防ぐための処理 */
    vec_destroy(&s->successor); /* 中身(StateTransition)のなくなった((Vector *)&s->successor)->tblを解放 */
    memset(&s->successor, 0x00U, sizeof(Vector)); /* successorの記憶域をゼロクリアする */
  }

  /* 不要なフラグをすべて解除する */
  unset_expanded(s);
  unset_independency_checked(s);

  /* ample(s)内に含まれないStateは解放 */
  if (!is_ample(s)) {
    state_free(s);
  }
  return ST_DELETE;
}

static void finalize_ample() {
  /* StateTransitionのIDを0に戻す */
  next_strans_id = 0U;

  /* 独立性情報テーブルの解放 */
  st_foreach(strans_independency, independency_vec_free, 0);

  /* States_POR上の状態の内，ample(s)内の要素でないものをすべて解放する．
   * また，StateTransitionは実際に構築するstate graph内では不要なため，ここですべて解放する */
  st_foreach(States_POR, destroy_tmp_state_graph, 0);
}

Vector *ample(const StateSpace states, State *s) {
  /* POR無効の場合は状態展開にexpand/1を用い，full-stateグラフを構築する */
  Vector *expanded;
    
  /* init */
  st_add_direct(States_POR, (st_data_t)s, (st_data_t)s);
  set_ample(s); /* s(展開元)が簡約グラフ内に含まれる旨のフラグを立てる */
  vec_clear(ample_candidate);

  /* check C0: |en(s)|<=1 ならば，C0によりただちに ample(s)=en(s) と決定される */
  if (!independency_check(states, s)) {
    expanded = push_succstates_to_expanded(s);
    if (expanded == NULL) { lmn_fatal("unexpected"); }
    finalize_ample();
    if (expanded != NULL) {
      return expanded;
    } else {
      /* |en(s)|=0 i.e. 遷移先が存在しない場合は偽を返す */
      expanded = vec_make(1);
      return expanded;
    }
  }

  /* C1から導かれるLemmaを満たすような，sで可能な遷移の集合を求める．
   * この処理により，sにおいてC1を満足するためには絶対にample(s)内に含めておかなくてはならない
   * 遷移の集合がample_candidate内にPUSHされる */
  st_foreach(strans_independency, build_ample_satisfying_lemma, (st_data_t)s);

  /* ここでample_candidateが空の場合は，sで可能なすべての遷移が互いに独立であることになるので，
   * その中でC2，C3を共に満足する1本をample_candidateの要素とする */
  if (vec_is_empty(ample_candidate)) {
    unsigned int i;
    BOOL found_proper_candidate;

    found_proper_candidate = FALSE;
    for (i = 0; i < vec_num(&s->successor); ++i) {
      vec_push(ample_candidate, (LmnWord)((StateTransition *)vec_get(&s->successor, i))->id);
      if (check_C2(s) && check_C3(states, s)) {
        found_proper_candidate = TRUE;
        break;
      } else {
        /* 選択した遷移がC2もしくはC3のいずれかに反したため，これを候補から除外する */
        vec_clear(ample_candidate);
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
    /* ample_candidateが空でない場合，まずample_candidate = en(s)であるかどうかチェックする．
     * ample_candidate = en(s)である場合(i.e. en(s)内のすべての遷移が互いに依存している場合)はen(s)を返して終了する．
     *
     * ample_candidate != en(s)の場合は，ample_candidate内の各遷移がC2およびC3を共に満足することを確認する．
     * このチェックに通らない場合，C0〜C3をすべて満足するようなen(s)の真部分集合は存在しないことになるため，
     * C0に従い，en(s)を返して終了する．
     */
    if (vec_num(ample_candidate) == vec_num(&s->successor) ||
        !check_C2(s) || !check_C3(states, s)) {
      expanded = push_succstates_to_expanded(s);
      if (expanded == NULL) { lmn_fatal("unexpected"); }
      finalize_ample();
      return expanded;
    }
  }

  /******************************************************************
   * この段階で少なくとも状態sにおいてはC0〜C3のすべてを満たすen(s)の
   * 真部分集合ample_candidateが求まっていることになる．
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
   * s->successor[i]->idがample_candidate内に含まれているものをテーブルexpanded内に重複なく放り込んでいく．
   * その際，テーブルに放り込まれた各Stateにample(s)のメンバーに選ばれた旨のフラグを立てておく． */
  expanded = push_ample_to_expanded(s);

  /* finalize */
  finalize_ample();

  return expanded;
}
