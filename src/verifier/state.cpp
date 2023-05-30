/*
 * state.cpp
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

#include "state.h"
#include "automata.h"
#include "binstr_compress.h"
#include "element/element.h"
#include "mc.h"
#include "mem_encode.h"
#include "mhash.h"
#include "runtime_status.h"
#include "vm/vm.h"

#ifdef KWBT_OPT
#include <climits>
#endif
#include "state.hpp"

#include <cstring>
#include <string>

namespace c14 = slim::element;

LmnCost state_cost(State *S) {
#ifdef KWBT_OPT
  return ((S)->cost);
#else
  return 0U;
#endif
}

void state_cost_lock(EWLock *EWLOCK, mtx_data_t ID) { (EWLOCK->acquire_write(ID)); }
void state_cost_unlock(EWLock *EWLOCK, mtx_data_t ID) { (EWLOCK->release_write(ID)); }

static inline LmnBinStrRef state_binstr_D_compress(LmnBinStrRef org, State *ref_s);

void tcd_set_root_ref(TreeCompressData *tcd, uint64_t ref) { memcpy(&tcd->root_ref, &ref, sizeof(TreeRootRef)); }

void tcd_get_root_ref(TreeCompressData *tcd, uint64_t *ref) { memcpy(ref, &tcd->root_ref, sizeof(TreeRootRef)); }

unsigned short tcd_get_byte_length(TreeCompressData *data) { return data->byte_length; }

void tcd_set_byte_length(TreeCompressData *data, unsigned short byte_length) { data->byte_length = byte_length; }

/*----------------------------------------------------------------------
 * State
 */

/* 膜memを用いて状態sのハッシュ値を計算する.
 * canonicalをTRUEで入力した場合, バイナリストリングの設定まで行う */

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals_with_compress(State *check, State *stored) {
  LmnBinStrRef bs1, bs2;
  int          t;

#ifdef PROFILE
  if (lmn_env.prof_no_memeq) {
    /* 本フラグが真の場合はグラフ同形成判定を行わず,
     * ハッシュ値の一致不一致で状態の等価性を判定する.
     * ハッシュコンフリクトで完全性が損なわれるが, .
     * ハッシュ値が完全に散らばることが既知の問題に対し,
     * メモリ使用量等のプロファイルを取る際など,
     * プロファイル収集を円滑に行うべく使用する.
     * グラフ同型成判定の時間分だけ高速化できる */
    return (state_hash(check) == state_hash(stored));
  }
#endif

  if (check->s_is_d()) {
    bs1 = state_D_fetch(check);
  } else {
    bs1 = check->state_binstr();
  }

  if (stored->s_is_d()) {
    bs2 = stored->reconstruct_binstr();
  } else {
    bs2 = stored->state_binstr();
  }

  if (check->is_encoded() && stored->is_encoded()) {
    /* 膜のIDで比較 */
    t = check->state_name == stored->state_name && binstr_compare(bs1, bs2) == 0;
  } else if (check->state_mem() && bs2) {
    /* 同型性判定 */
    t = check->state_name == stored->state_name && lmn_mem_equals_enc(bs2, check->state_mem());
  } else if (bs1 && bs2) {
    /* このブロックは基本的には例外処理なので注意.
     * PORなどでコピー状態を挿入する際に呼ばれることがある. */
    LmnMembraneRef mem = lmn_binstr_decode(bs1);
    t                  = check->state_name == stored->state_name && lmn_mem_equals_enc(bs2, mem);
    mem->free_rec();
  } else {
    lmn_fatal("implementation error");
  }

  if (stored->s_is_d()) {
    lmn_binstr_free(bs2);
  }

  return t;
}

static int state_equals(State *s1, State *s2) {
#ifdef DEBUG
  if (s1->is_binstr_user() || s2->is_binstr_user()) {
    lmn_fatal("unexpected");
  }
#endif
  return s1->state_name == s2->state_name && s1->hash == s2->hash && (s1->state_mem())->equals(s2->state_mem());
}

/**
 * 与えられた2つの状態が互いに異なっていれば真を、逆に等しい場合は偽を返す
 */
#ifndef DEBUG
int state_cmp_with_compress(State *s1, State *s2) { return !state_equals_with_compress(s1, s2); }
#else

#define CMP_STR(Str) ((Str) ? "equal" : "NOT equal")

int state_cmp_with_compress(State *s1, State *s2) {
  if (lmn_env.debug_isomor && !(s1->is_encoded() && s2->is_encoded())) {
    LmnMembraneRef s2_mem;
    LmnBinStrRef s1_mid, s2_mid;
    BOOL org_check, mid_check, meq_check;

    /* TODO: --disable-compress時にもチェックできるよう修正して構造化する. */

    /* s1がcheckなのでmem, s2がstored(ハッシュ表に記録済み)なのでbinstrを保持 */

    /* データ構造の構築 */
    s2_mem = lmn_binstr_decode(s2->state_binstr());
    s1_mid = lmn_mem_encode(s1->state_mem());
    s2_mid = lmn_mem_encode(s2_mem);

    org_check = (state_equals_with_compress(s1, s2) != 0); /* A. slim本来のグラフ同型成判定手続き */
    mid_check = (binstr_compare(s1_mid, s2_mid) == 0); /* B. 互いに一意エンコードしたグラフの比較手続き */
    meq_check = ((s1->state_mem())->equals(s2_mem)); /* C. 膜同士のグラフ同型性判定 */

    /* A, B, Cが同じ判定結果を返す場合はokだが.. */
    if (org_check != mid_check || org_check != meq_check || mid_check != meq_check) {
      FILE *f;
      LmnBinStrRef s1_bs;
      BOOL sp1_check, sp2_check, sp3_check, sp4_check;

      f = stdout;
      s1_bs = lmn_mem_to_binstr(s1->state_mem());

      sp1_check = ((s1->state_mem())->equals(s2_mem) != 0);
      sp2_check = (lmn_mem_equals_enc(s1_bs, s2_mem) != 0);
      sp3_check = (lmn_mem_equals_enc(s1_mid, s2_mem) != 0);
      sp4_check = (lmn_mem_equals_enc(s2_mid, s1->state_mem()) != 0);
      fprintf(f, "fatal error: checking graphs isomorphism was invalid\n");
      fprintf(f, "============================================================="
                 "=======================\n");
      fprintf(f, "%18s | %-18s | %-18s | %-18s\n", " - ", "s1.Mem (ORG)", "s1.BS (calc)", "s1.MID (calc)");
      fprintf(f, "-------------------------------------------------------------"
                 "-----------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n", "s2.Mem (calc)", CMP_STR(sp1_check), CMP_STR(sp2_check),
              CMP_STR(sp3_check));
      fprintf(f, "-------------------------------------------------------------"
                 "-----------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n", "s2.BS (ORG)", CMP_STR(org_check), "-", "-");
      fprintf(f, "-------------------------------------------------------------"
                 "-----------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n", "s2.MID (calc)", CMP_STR(sp4_check), "-", CMP_STR(mid_check));
      fprintf(f, "============================================================="
                 "=======================\n");

      fprintf(f, "%s\n", slim::to_string_membrane(s1->state_mem()).c_str());

      lmn_binstr_free(s1_bs);
      lmn_fatal("graph isomorphism procedure has invalid implementations");
    }

    s2_mem->free_rec();

    lmn_binstr_free(s1_mid);
    lmn_binstr_free(s2_mid);

    return !org_check;
  } else {
    return !state_equals_with_compress(s1, s2);
  }
}
#endif

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals_with_tree(State *check, State *stored) {
  LmnBinStrRef bs1, bs2;
  TreeNodeID   ref;
  int          t;

  bs1 = check->state_binstr();

  tcd_get_root_ref(&stored->tcd, &ref);
  LMN_ASSERT(ref != 0);
  LMN_ASSERT(tcd_get_byte_length(&stored->tcd) != 0);
  bs2 = lmn_bscomp_tree_decode((TreeNodeID)ref, tcd_get_byte_length(&stored->tcd));

  if (check->is_encoded() && stored->is_encoded()) {
    /* 膜のIDで比較 */
    t = check->state_name == stored->state_name && binstr_compare(bs1, bs2) == 0;
  } else if (check->state_mem() && bs2) {
    /* 同型性判定 */
    t = check->state_name == stored->state_name && lmn_mem_equals_enc(bs2, check->state_mem());
  } else if (bs1 && bs2) {
    /* このブロックは基本的には例外処理なので注意.
     * PORなどでコピー状態を挿入する際に呼ばれることがある. */
    LmnMembraneRef mem = lmn_binstr_decode(bs1);
    t                  = check->state_name == stored->state_name && lmn_mem_equals_enc(bs2, mem);
    mem->free_rec();
  } else {
    lmn_fatal("implementation error");
  }

  lmn_binstr_free(bs2);
  return t;
}

int state_cmp_with_tree(State *s1, State *s2) { return !state_equals_with_tree(s1, s2); }

int state_cmp(State *s1, State *s2) { return !state_equals(s1, s2); }

/* バイナリストリングorgと状態ref_sのバイナリストリングとの差分バイナリストリングを返す.
 * orgのメモリ管理は呼出し側で行う. */
static inline LmnBinStrRef state_binstr_D_compress(LmnBinStrRef org, State *ref_s) {
  LmnBinStrRef ref, dif;

  ref = state_D_fetch(ref_s);
  if (ref) {
    dif = lmn_bscomp_d_encode(org, ref);
  } else {
    ref = ref_s->reconstruct_binstr();
    dif = lmn_bscomp_d_encode(org, ref);
    if (ref_s->s_is_d()) {
      lmn_binstr_free(ref);
    }
  }

  return dif;
}

LmnBinStrRef state_calc_mem_dump(State *s) { return s->mem_dump(); }

/* 状態sに対応した階層グラフ構造のバイナリストリングをzlibで圧縮して返す.
 * 状態sはread only */
LmnBinStrRef state_calc_mem_dump_with_z(State *s) { return s->mem_dump_with_z(); }

/* 状態sに対応する階層グラフ構造をバイナリストリングにエンコードして返す.
 * sのフラグを操作する. */
LmnBinStrRef state_calc_mem_dump_with_tree(State *s) { return s->mem_dump_with_tree(); }

LmnBinStrRef state_calc_mem_dummy(State *s) {
  /* DUMMY: nothing to do */
  return nullptr;
}

unsigned long transition_space(TransitionRef t) {
  unsigned long ret;
  ret = sizeof(struct Transition);
  ret += t->rule_names.space_inner();
  return ret;
}

TransitionRef transition_make(State *s, lmn_interned_str rule_name) {
  struct Transition *t = LMN_MALLOC<struct Transition>();

  t->s  = s;
  t->id = 0;
  transition_set_cost(t, 0);
  t->rule_names.init(4);
  t->rule_names.push(rule_name);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
  }
#endif
  return t;
}

void transition_free(TransitionRef t) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
  }
#endif
  t->rule_names.destroy();
  LMN_FREE(t);
}

void transition_add_rule(TransitionRef t, lmn_interned_str rule_name, LmnCost cost) {
  if (rule_name != ANONYMOUS || !t->rule_names.contains(rule_name)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
    }
#endif

    t->rule_names.push(rule_name);

#ifdef KWBT_OPT
    if (((lmn_env.opt_mode == OPT_MINIMIZE) && t->cost > cost) ||
        ((lmn_env.opt_mode == OPT_MAXIMIZE) && t->cost < cost)) {
      t->cost = cost;
    }
#endif

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
    }
#endif
  }
}

/* 状態sに対応する階層グラフ構造memへのアドレスを返す.
 * memがエンコードされている場合は, デコードしたオブジェクトのアドレスを返す.
 * デコードが発生した場合のメモリ管理は呼び出し側で行う. */
LmnMembraneRef state_restore_mem(State *s) { return s->restore_membrane(); }

/* 状態sに割り当てた整数IDを返す. */
unsigned long state_id(State *s) { return s->state_id; }

/* 状態sに対して, 状態固有となる整数IDを割り当てる.
 * 即ち, 実行スレッド間で, 同じ整数IDは使用されない.
 * 既にIDが割り当てられている場合はなにもしない. */
void state_id_issue(State *s) {
  if (s->state_id == 0) {
    /* 状態ID用に設置したTLS機構から整数IDを生成して割り当てる. */
    s->state_id = env_gen_state_id();
  }
}

/* 特殊な整数ID(format id) vを状態sに割り当てる.
 * 状態遷移グラフをCUIにプリントする際, 可視性を向上させるために呼び出す.
 * 状態遷移グラフの構築および探索中に使用してはならない */
void state_set_format_id(State *s, unsigned long v) {
  /* ハッシュ値が不要となる段階で呼び出すため, hash値の領域を再利用する */
  s->hash = v;
}

/* 状態sに割り当てた特殊な整数ID(format id)を返す.
 * format_idを割り当てていない場合(is_formated==FALSE)や
 * 状態遷移グラフを構築しながら状態データを出力する場合は,
 * 状態生成時に割り当てた整数IDをそのまま返す. */
unsigned long state_format_id(State *s, BOOL is_formated) {
  if (is_formated && lmn_env.sp_dump_format != INCREMENTAL) {
    return s->hash;
  } else {
    return state_id(s);
  }
}

/* 状態sに割り当てた性質オートマトン上の状態名を返す.
 * 性質オートマトンを使用していない場合は, 戻り値は0 */
BYTE state_property_state(State *s) { return s->state_name; }

/* 状態sに性質オートマトン上の状態名labelを割り当てる. */
void state_set_property_state(State *s, BYTE label) { s->state_name = label; }

/* 状態sに対応するハッシュ値を返す. */
unsigned long state_hash(State *s) {
  /* state_property_stateは0の場合があるので+1する */
  return s->hash * (state_property_state(s) + 1);
}

/* 状態sに階層グラフ構造memを割り当てる.
 * sに対してバイナリストリングBを割り当てている場合は,
 * Bのメモリ管理は呼出し側で行う*/

/* 状態sが参照する階層グラフ構造用の領域をクリアする.
 * 階層グラフ構造の参照を持たない場合は, なにもしない. */
void state_unset_mem(State *s) {
  if (!s->is_binstr_user()) {
    s->state_set_mem(nullptr);
  }
}

/* 状態sに対応する階層グラフ構造からエンコードしたバイナリストリングbsを,
 * sに割り当てる　*/

/* 状態sが参照するバイナリストリング用の領域をクリアする.
 * バイナリストリングに対する参照を持たない場合は, なにもしない. */
void state_unset_binstr(State *s) {
  if (s->is_binstr_user()) {
    s->data = nullptr;
    s->unset_binstr_user();
  }
}

/* 状態sを生成した状態(親ノード)へのアドレスを返す. */
State *state_get_parent(State *s) { return s->parent; }

/* 状態sに, sを生成した状態(親ノード)へのアドレスを割り当てる. */
void state_set_parent(State *s, State *parent) { s->parent = parent; }

/* 状態sから遷移可能な状態数を返す. */

/* 状態sから遷移可能な状態の集合から, idx番目の状態を返す. */
State *state_succ_state(State *s, int idx) {
  /* successorデータはTransitionがある場合とそうでない場合とで処理が異なる */
  if (s->has_trans_obj()) {
    return transition_next_state((TransitionRef)s->successors[idx]);
  } else {
    return (State *)s->successors[idx];
  }
}

/* 状態sから遷移可能な状態集合に, 状態tが含まれている場合に真を返す.
 * O(successor_num)と効率的ではないため, 可能な限り利用しない. */
BOOL state_succ_contains(State *s, State *t) {
  unsigned int i;
  for (i = 0; i < s->successor_num; i++) {
    State *succ = state_succ_state(s, i);
    if (succ == t)
      return TRUE;
  }
  return FALSE;
}

/* 状態sが,
 * 性質オートマトンa上のaccept状態に対応している(受理状態)ならば真を返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す. */
BOOL state_is_accept(AutomataRef a, State *s) {
  if (a) {
    return a->get_state(state_property_state(s))->get_is_accept();
  } else {
    return FALSE;
  }
}

/* 状態sが, 性質オートマトンa上のend状態に対応している(invalid end
 * state)ならば真を返す. 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * TOFIX: rename (end --> invalid end) */
BOOL state_is_end(AutomataRef a, State *s) {
  if (a) {
    return a->get_state(state_property_state(s))->get_is_end();
  } else {
    return FALSE;
  }
}

/* 状態sに対応する性質オートマトンa上の状態が,
 * 属している強連結成分(scc)のグループIDを返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * 性質オートマトンaに対して強連結成分分解を行っていない場合の戻り値は, 未定義.
 */
BYTE state_scc_id(AutomataRef a, State *s) {
  if (a) {
    return a->get_state(state_property_state(s))->scc_type();
  } else {
    return FALSE;
  }
}

/* 状態sとの差分計算の対象とする状態に対する参照を返す. */
State *state_D_ref(State *s) {
  /* とりあえず親ノードにした */
  return state_get_parent(s);
}

/* 状態sに対応する非圧縮バイナリストリングdをキャッシングする. */
void state_D_cache(State *s, LmnBinStrRef d) {
  LMN_ASSERT(!state_D_fetch(s));
  /* メモリ節約の結果, 保守性ないコード. 注意 */
  s->successors = (succ_data_t *)d;
}

/* キャッシングしておいた状態sに対応する非圧縮バイナリストリングに対する参照を返す.
 */
LmnBinStrRef state_D_fetch(State *s) {
  if (s->s_is_d()) {
    return (LmnBinStrRef)s->successors;
  }
  return nullptr;
}

/* 状態sに対応する非圧縮バイナリストリングのキャッシュをクリアする. */
void state_D_flush(State *s) {
  LmnBinStrRef cached = state_D_fetch(s);
  if (cached) {
    lmn_binstr_free(cached);
  }
  s->successors = nullptr;
}

/* 差分圧縮バイト列に基づく状態生成処理のfinalizeを行う. */
void state_D_progress(State *s, MCReactContext *rc) {
  RC_D_PROGRESS(rc);
  state_D_flush(s);
}

/* MT-unsafe */
void state_set_cost(State *s, LmnCost cost, State *pre) {
#ifdef KWBT_OPT
  s->cost = cost;
#endif
  s->map = pre;
}

/* 状態sのcostが最適ならば更新し、状態sを遷移先更新状態にする
 * f==true: minimize
 * f==false: maximize */
void state_update_cost(State *s, TransitionRef t, State *pre, Vector *new_ss, BOOL f, EWLock *ewlock) {
  LmnCost cost;
  cost = transition_cost(t) + state_cost(pre);
  if (env_threads_num() >= 2)
    state_cost_lock(ewlock, state_hash(s));
  if ((f && state_cost(s) > cost) || (!f && state_cost(s) < cost)) {
    state_set_cost(s, cost, pre);
    if (s->is_expanded() && new_ss)
      new_ss->push((vec_data_t)s);
    s->s_set_update();
  }
  if (env_threads_num() >= 2)
    state_cost_unlock(ewlock, state_hash(s));
}

/* 遷移tに割り当てたidを返す. */
unsigned long transition_id(TransitionRef t) { return t->id; }

/* 遷移tに整数ID idを割り当てる. */
void transition_set_id(TransitionRef t, unsigned long id) { t->id = id; }

int transition_rule_num(TransitionRef t) { return t->rule_names.get_num(); }

lmn_interned_str transition_rule(TransitionRef t, int idx) { return t->rule_names.get(idx); }

State *transition_next_state(TransitionRef t) { return t->s; }

void transition_set_state(TransitionRef t, State *s) { t->s = s; }

TransitionRef transition(State *s, unsigned int i) { return (TransitionRef)(s->successors[i]); }

LmnCost transition_cost(TransitionRef t) {
#ifdef KWBT_OPT
  return t->cost;
#else
  return 0;
#endif
}

void transition_set_cost(TransitionRef t, LmnCost cost) {
#ifdef KWBT_OPT
  t->cost = cost;
#endif
}
