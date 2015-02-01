/*
 * state.c
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
#include "mc.h"
#include "membrane.h"
#include "mem_encode.h"
#include "rule.h"
#include "mhash.h"
#include "error.h"
#include "dumper.h"
#include "util.h"
#include "task.h"
#include "binstr_compress.h"
#include "runtime_status.h"

#ifdef KWBT_OPT
# include <limits.h>
#endif

static inline LmnBinStr state_binstr_D_compress(LmnBinStr org, State *ref_s);

/*----------------------------------------------------------------------
 * State
 */

/* delta-membrane使用時には呼ばない.
 * memに対して一意なIDとなるバイナリストリングを計算した場合は, memを破棄する. */
State *state_make(LmnMembrane *mem, BYTE property_label, BOOL do_encode)
{
  State *new_s = state_make_minimal();

  state_set_mem(new_s, mem);
  new_s->state_name = property_label;
  state_calc_hash(new_s, mem, do_encode);

  if (is_encoded(new_s)) {
    lmn_mem_free_rec(mem);
  }
#ifdef PROFILE
  else if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
  }
#endif

  return new_s;
}


/* まっさらなState構造体をmallocして返してもらう */
State *state_make_minimal()
{
  State *new_s = LMN_MALLOC(State);
  new_s->data             = NULL;
  new_s->state_name       = 0x00U;
  new_s->flags            = 0x00U;
  new_s->flags2           = 0x00U;
  new_s->flags3           = 0x00U;
  new_s->hash             = 0;
  new_s->next             = NULL;
  new_s->successors       = NULL;
  new_s->successor_num    = 0;
  new_s->parent           = NULL;
  new_s->state_id         = 0;
  new_s->map              = NULL;
  memset(&new_s->tcd, 0x00, sizeof(TreeCompressData));

#ifndef MINIMAL_STATE
  state_set_expander_id(new_s, LONG_MAX);
  new_s->local_flags      = 0x00U;
  state_expand_lock_init(new_s);
#endif
  s_set_fresh(new_s);

#ifdef KWBT_OPT
  if (lmn_env.opt_mode != OPT_NONE) {
    new_s->cost = lmn_env.opt_mode == OPT_MINIMIZE ? ULONG_MAX : 0;
  }
#endif

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
  }
#endif
  return new_s;
}


/* 膜memを用いて状態sのハッシュ値を計算する.
 * canonicalをTRUEで入力した場合, バイナリストリングの設定まで行う */
void state_calc_hash(State *s, LmnMembrane *mem, BOOL canonical)
{
  if (canonical) {
    state_set_binstr(s, lmn_mem_encode(mem));
    s->hash = binstr_hash(state_binstr(s));
    set_encoded(s);
  } else {
    s->hash = mhash(mem);
  }
}


/* 状態srcと等価な状態を新たに構築して返す.
 * srcに階層グラフ構造が割り当てられている場合, その階層グラフ構造までを,
 * else: srcにバイナリストリングが割り当てられている場合, そのバイナリストリングを,
 * 複製(即ち, deep copy)する.
 * また, これに伴うencode関連の状態フラグもコピーするが, 状態空間構築のためのフラグはコピーしない.
 * なお, 引数memがNULLではない場合は, これをsrcに対応する階層グラフ構造として扱う.　*/
State *state_copy(State *src, LmnMembrane *mem)
{
#ifdef PROFILE
  if (lmn_env.profile_level >= 3 && mem) {
    profile_start_timer(PROFILE_TIME__STATE_COPY);
  }
#endif

  State *dst = state_make_minimal();

  if (!is_binstr_user(src) && !mem) {
    mem = state_mem(src);
  }

  if (mem) {
    state_set_mem(dst, lmn_mem_copy_ex(mem));
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
    }
#endif
  }
  else if (state_binstr(src)) {
    state_set_binstr(dst, lmn_binstr_copy(state_binstr(src)));
    if (is_encoded(src)) {
      set_encoded(dst);
    }
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
    }
#endif
  }

  dst->hash = src->hash;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3 && mem) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
    profile_finish_timer(PROFILE_TIME__STATE_COPY);
  }
#endif

  return dst;
}


/**
 * デストラクタ
 */
void state_free(State *s)
{
  if (s->successors) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, sizeof(succ_data_t) * state_succ_num(s));
#endif

    if (has_trans_obj(s)) {
      unsigned int i;
      for (i = 0; i < state_succ_num(s); i++) {
        transition_free(transition(s, i));
      }
    }
    LMN_FREE(s->successors);
  }

#ifndef MINIMAL_STATE
  if (s->local_flags) {
      LMN_FREE(s->local_flags);
  }
#endif

  state_expand_lock_destroy(s);

  state_free_mem(s);
  state_free_binstr(s);
  LMN_FREE(s);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
  }
#endif
}

void state_free_mem(State *s)
{
  if (state_mem(s)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(state_mem(s)));
    }
#endif
    lmn_mem_free_rec(state_mem(s));
    state_set_mem(s, NULL);
  }
}


void state_succ_set(State *s, Vector *v)
{
  if (!vec_is_empty(v) && !s->successors) {
    unsigned int i;
    s->successor_num = vec_num(v);
    s->successors    = LMN_NALLOC(succ_data_t, state_succ_num(s));
    for (i = 0; i < state_succ_num(s); i++) {
      s->successors[i] = (succ_data_t)vec_get(v, i);
    }
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__TRANS_OBJECT,
                        sizeof(succ_data_t) * vec_num(v));
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, 0);
    }
#endif
  }
}


void state_succ_add(State *s, succ_data_t succ)
{
  if (!s->successors) {
    s->successors = LMN_NALLOC(succ_data_t, 1);
  } else {
    s->successors = LMN_REALLOC(succ_data_t, s->successors, s->successor_num + 1);
  }
  s->successors[s->successor_num] = succ;
  s->successor_num++;
}


void state_succ_clear(State *s) {
  if (has_trans_obj(s)) {
    unsigned int i;
    for (i = 0; i < state_succ_num(s); i++) {
      Transition t = transition(s, i);
      transition_free(t);
    }
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, sizeof(succ_data_t) * state_succ_num(s));
    profile_add_space(PROFILE_SPACE__TRANS_OBJECT, 0);
  }
#endif


  LMN_FREE(s->successors);
  s->successors = NULL;
  s->successor_num = 0;
  unset_trans_obj(s);
}


/* 状態sに対応する階層グラフ構造と等価な階層グラフ構造を新たに構築して返す.
 * 構築できなかった場合は偽を返す. */
LmnMembrane *state_mem_copy(State *s)
{
  LmnMembrane *ret = NULL;
  if (!is_binstr_user(s) && state_mem(s)) {
    ret = lmn_mem_copy(state_mem(s));
  }
  else if (is_binstr_user(s) && state_binstr(s)) {
    ret = lmn_binstr_decode(state_binstr(s));
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3 && ret) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(ret));
  }
#endif

  return ret;
}


/* 状態sに対応するバイナリストリングを, sがrefする状態を基に再構築して返す. */
LmnBinStr state_binstr_reconstructor(State *s)
{
  LmnBinStr ret;
  if (!s_is_d(s)) {
    ret = state_binstr(s);
  }
  else {
    LmnBinStr ref;
    LMN_ASSERT(state_D_ref(s));
    ref = state_binstr_reconstructor(state_D_ref(s));
    ret = lmn_bscomp_d_decode(ref, state_binstr(s));
    if (s_is_d(state_D_ref(s))) {
      lmn_binstr_free(ref);
    }
  }

  return ret;
}

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals_with_compress(State *check, State *stored)
{
  LmnBinStr bs1, bs2;
  int t;

#ifdef PROFILE
  if (lmn_env.prof_no_memeq) {
    /* 本フラグが真の場合はグラフ同形成判定を行わず, ハッシュ値の一致不一致で状態の等価性を判定する.
     * ハッシュコンフリクトで完全性が損なわれるが, .
     * ハッシュ値が完全に散らばることが既知の問題に対し, メモリ使用量等のプロファイルを取る際など,
     * プロファイル収集を円滑に行うべく使用する. グラフ同型成判定の時間分だけ高速化できる */
    return (state_hash(check) == state_hash(stored));
  }
#endif

  if (s_is_d(check)) {
    bs1 = state_D_fetch(check);
  } else {
    bs1 = state_binstr(check);
  }

  if (s_is_d(stored)) {
    bs2 = state_binstr_reconstructor(stored);
  } else {
    bs2 = state_binstr(stored);
  }

  if (is_encoded(check) && is_encoded(stored)) {
    /* 膜のIDで比較 */
    t =
      check->state_name == stored->state_name &&
      binstr_compare(bs1, bs2) == 0;
  }
  else if (state_mem(check) && bs2) {
    /* 同型性判定 */
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(bs2, state_mem(check));
  }
  else if (bs1 && bs2) {
    /* このブロックは基本的には例外処理なので注意.
     * PORなどでコピー状態を挿入する際に呼ばれることがある. */
    LmnMembrane *mem = lmn_binstr_decode(bs1);
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(bs2, mem);
    lmn_mem_free_rec(mem);
  }
  else {
    lmn_fatal("implementation error");
  }

  if (s_is_d(stored)) {
    lmn_binstr_free(bs2);
  }

  return t;
}

static int state_equals(State *s1, State *s2)
{
#ifdef DEBUG
  if (is_binstr_user(s1) || is_binstr_user(s2)) {
    lmn_fatal("unexpected");
  }
#endif
  return
    s1->state_name == s2->state_name &&
    s1->hash       == s2->hash       &&
    lmn_mem_equals(state_mem(s1), state_mem(s2));
}


/**
 * 与えられた2つの状態が互いに異なっていれば真を、逆に等しい場合は偽を返す
 */
#ifndef DEBUG
int state_cmp_with_compress(State *s1, State *s2)
{
  return !state_equals_with_compress(s1, s2);
}
#else

#define CMP_STR(Str) ((Str) ? "equal" : "NOT equal")

int state_cmp_with_compress(State *s1, State *s2)
{
  if (lmn_env.debug_isomor && !(is_encoded(s1) && is_encoded(s2))) {
    LmnMembrane *s2_mem;
    LmnBinStr s1_mid, s2_mid;
    BOOL org_check, mid_check, meq_check;

    /* TODO: --disable-compress時にもチェックできるよう修正して構造化する. */

    /* s1がcheckなのでmem, s2がstored(ハッシュ表に記録済み)なのでbinstrを保持 */

    /* データ構造の構築 */
    s2_mem = lmn_binstr_decode(state_binstr(s2));
    s1_mid = lmn_mem_encode(state_mem(s1));
    s2_mid = lmn_mem_encode(s2_mem);

    org_check = (state_equals_with_compress(s1, s2) != 0);  /* A. slim本来のグラフ同型成判定手続き */
    mid_check = (binstr_compare(s1_mid, s2_mid) == 0);      /* B. 互いに一意エンコードしたグラフの比較手続き */
    meq_check = (lmn_mem_equals(state_mem(s1), s2_mem));    /* C. 膜同士のグラフ同型性判定 */

    /* A, B, Cが同じ判定結果を返す場合はokだが.. */
    if (org_check != mid_check ||
        org_check != meq_check ||
        mid_check != meq_check) {
      FILE *f;
      LmnBinStr s1_bs;
      BOOL sp1_check, sp2_check, sp3_check, sp4_check;

      f = stdout;
      s1_bs = lmn_mem_to_binstr(state_mem(s1));

      sp1_check = (lmn_mem_equals(state_mem(s1), s2_mem) != 0);
      sp2_check = (lmn_mem_equals_enc(s1_bs, s2_mem) != 0);
      sp3_check = (lmn_mem_equals_enc(s1_mid, s2_mem) != 0);
      sp4_check = (lmn_mem_equals_enc(s2_mid, state_mem(s1)) != 0);
      fprintf(f, "fatal error: checking graphs isomorphism was invalid\n");
      fprintf(f, "====================================================================================\n");
      fprintf(f,  "%18s | %-18s | %-18s | %-18s\n",        " - ",      "s1.Mem (ORG)",     "s1.BS (calc)",    "s1.MID (calc)");
      fprintf(f, "------------------------------------------------------------------------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n", "s2.Mem (calc)",  CMP_STR(sp1_check), CMP_STR(sp2_check), CMP_STR(sp3_check));
      fprintf(f, "------------------------------------------------------------------------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n",   "s2.BS (ORG)",  CMP_STR(org_check),                "-",                "-");
      fprintf(f, "------------------------------------------------------------------------------------\n");
      fprintf(f, "%-18s | %18s | %18s | %18s\n", "s2.MID (calc)",  CMP_STR(sp4_check),                "-", CMP_STR(mid_check));
      fprintf(f, "====================================================================================\n");

      lmn_binstr_dump(state_binstr(s2));
      lmn_dump_mem_stdout(state_mem(s1));

      lmn_binstr_free(s1_bs);
      lmn_fatal("graph isomorphism procedure has invalid implementations");
    }

    lmn_mem_free_rec(s2_mem);

    lmn_binstr_free(s1_mid);
    lmn_binstr_free(s2_mid);

    return !org_check;
  }
  else {
    return !state_equals_with_compress(s1, s2);
  }
}
#endif

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals_with_tree(State *check, State *stored)
{
  LmnBinStr bs1, bs2;
  TreeNodeRef ref;
  int t;

  bs1 = state_binstr(check);

  tcd_get_root_ref(&stored->tcd, &ref);
  LMN_ASSERT(ref != 0);
  LMN_ASSERT(tcd_get_byte_length(&stored->tcd) != 0);
  bs2 = lmn_bscomp_tree_decode((TreeNodeRef)ref, tcd_get_byte_length(&stored->tcd));

  if (is_encoded(check) && is_encoded(stored)) {
    /* 膜のIDで比較 */
    t =
      check->state_name == stored->state_name &&
      binstr_compare(bs1, bs2) == 0;
  }
  else if (state_mem(check) && bs2) {
    /* 同型性判定 */
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(bs2, state_mem(check));
  }
  else if (bs1 && bs2) {
    /* このブロックは基本的には例外処理なので注意.
     * PORなどでコピー状態を挿入する際に呼ばれることがある. */
    LmnMembrane *mem = lmn_binstr_decode(bs1);
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(bs2, mem);
    lmn_mem_free_rec(mem);
  }
  else {
    lmn_fatal("implementation error");
  }

  lmn_binstr_free(bs2);
  return t;
}

int state_cmp_with_tree(State *s1, State *s2)
{
  return !state_equals_with_tree(s1, s2);
}

int state_cmp(State *s1, State *s2)
{
  return !state_equals(s1, s2);
}


void state_free_binstr(State *s)
{
  if (state_binstr(s)) {
    lmn_binstr_free(state_binstr(s));
    s->data = NULL;
  }
  unset_binstr_user(s);
}


/* 状態sに対応する階層グラフ構造Xを, Xに対して一意なIDとなるバイナリストリングへエンコードする.
 * エンコードしたバイナリストリングをsに割り当てる.
 * sに対応する階層グラフ構造Xへのメモリ参照が消えるため, 呼び出し側でメモリ管理を行う.
 * sが既にバイナリストリングを保持している場合は, そのバイナリストリングは破棄する.
 * (ただし, sが既に一意なIDとなるバイナリストリングへエンコード済みの場合は何もしない.)
 * 既に割当済みのバイナリストリングを破棄するため,
 * sをハッシュ表に登録した後の操作はMT-unsafeとなる. 要注意. */
void state_calc_mem_encode(State *s)
{
  if (!is_encoded(s)) {
    LmnBinStr mid;

    if (state_mem(s)) {
      mid = lmn_mem_encode(state_mem(s));
    }
    else if (state_binstr(s)) {
      LmnMembrane *m;

      m   = lmn_binstr_decode(state_binstr(s));
      mid = lmn_mem_encode(m);
      state_free_binstr(s);
      lmn_mem_free_rec(m);
    }
    else {
      lmn_fatal("unexpected.");
    }

    state_set_binstr(s, mid);
    s->hash = binstr_hash(state_binstr(s));
    set_encoded(s);
  }
}

/**/
void state_calc_binstr_delta(State *s)
{
  LmnBinStr org = state_binstr(s);
  if (org && state_D_ref(s)) {
    LmnBinStr dif;
    dif = state_binstr_D_compress(org, state_D_ref(s));
    state_D_cache(s, org);
    state_set_binstr(s, dif);
  }
  else {
    s_unset_d(s);
  }
}


/* バイナリストリングorgと状態ref_sのバイナリストリングとの差分バイナリストリングを返す.
 * orgのメモリ管理は呼出し側で行う. */
static inline LmnBinStr state_binstr_D_compress(LmnBinStr org, State *ref_s)
{
  LmnBinStr ref, dif;

  ref = state_D_fetch(ref_s);
  if (ref) {
    dif = lmn_bscomp_d_encode(org, ref);
  }
  else {
    ref = state_binstr_reconstructor(ref_s);
    dif = lmn_bscomp_d_encode(org, ref);
    if (s_is_d(ref_s)) {
      lmn_binstr_free(ref);
    }
  }

  return dif;
}


/* 状態sに対応した階層グラフ構造のバイナリストリングをzlibで圧縮して返す.
 * 状態sはread only */
LmnBinStr state_calc_mem_dump_with_z(State *s)
{
  LmnBinStr ret;
  if (is_binstr_user(s)) {
    /* 既にバイナリストリングを保持している場合は, なにもしない. */
    ret = state_binstr(s);
  }
  else if (state_mem(s)) {
    LmnBinStr bs = lmn_mem_to_binstr(state_mem(s));
    /* TODO: --d-compressとの組合わせ */
    ret = lmn_bscomp_z_encode(bs);
    if (ret != bs) { /* 圧縮成功 */
      lmn_binstr_free(bs);
    }
  } else {
    lmn_fatal("unexpected");
  }

  return ret;
}


/* 状態sに対応する階層グラフ構造をバイナリストリングにエンコードして返す.
 * sのフラグを操作する. */
LmnBinStr state_calc_mem_dump(State *s)
{
  LmnBinStr ret;

  if (state_binstr(s)) {
    /* 既にエンコード済みの場合は何もしない. */
    ret = state_binstr(s);
  }
  else if (state_mem(s)) {
    ret = lmn_mem_to_binstr(state_mem(s));
    if (s_is_d(s) && state_D_ref(s)) {
      LmnBinStr dif;
      dif = state_binstr_D_compress(ret, state_D_ref(s));
      /* 元のバイト列は直ちに破棄せず, 一時的にキャッシュしておく. */
      state_D_cache(s, ret);
      ret = dif;
    } else {
      s_unset_d(s);
    }
  }
  else {
    lmn_fatal("unexpected.");
    ret = NULL;
  }

  return ret;
}


/* 状態sに対応する階層グラフ構造をバイナリストリングにエンコードして返す.
 * sのフラグを操作する. */
LmnBinStr state_calc_mem_dump_with_tree(State *s)
{
  LmnBinStr ret;

  if (state_binstr(s)) {
    /* 既にエンコード済みの場合は何もしない. */
    ret = state_binstr(s);
  }
  else if (state_mem(s)) {
    ret = lmn_mem_to_binstr(state_mem(s));
  }
  else {
    lmn_fatal("unexpected.");
    ret = NULL;
  }

  return ret;
}


LmnBinStr state_calc_mem_dummy(State *s)
{
  /* DUMMY: nothing to do */
  return NULL;
}

unsigned long transition_space(Transition t)
{
  unsigned long ret;
  ret  = sizeof(struct Transition);
  ret += vec_space_inner(&t->rule_names);
  return ret;
}


Transition transition_make(State *s, lmn_interned_str rule_name)
{
  struct Transition *t = LMN_MALLOC(struct Transition);

  t->s    = s;
  t->id   = 0;
  transition_set_cost(t, 0);
  vec_init(&t->rule_names, 4);
  vec_push(&t->rule_names, rule_name);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
  }
#endif
  return t;
}


void transition_free(Transition t)
{
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
  }
#endif
  vec_destroy(&t->rule_names);
  LMN_FREE(t);
}


void transition_add_rule(Transition t,
                         lmn_interned_str rule_name,
                         LmnCost cost)
{
  if (rule_name != ANONYMOUS || !vec_contains(&t->rule_names, rule_name)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
    }
#endif

    vec_push(&t->rule_names, rule_name);

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


/** Printer
 * ownerはNULLでもok */
void dump_state_data(State *s, LmnWord _fp, LmnWord _owner)
{
  FILE *f;
  StateSpace owner;
  unsigned long print_id;
#ifdef KWBT_OPT
  LmnCost cost = lmn_env.opt_mode != OPT_NONE ? state_cost(s) : 0UL;
#endif

  /* Rehashが発生している場合,
   * dummyフラグが真かつエンコード済みフラグが偽のStateオブジェクトが存在する.
   * このようなStateオブジェクトのバイナリストリングは
   * Rehashされた側のテーブルに存在するStateオブジェクトに登録されているためcontinueする. */
  if (is_dummy(s) && !is_encoded(s)) return;

  f     = (FILE *)_fp;
  owner = (StateSpace)_owner;
  {
    /* この時点で状態は, ノーマル || (dummyフラグが立っている && エンコード済)である.
     * dummyならば, バイナリストリング以外のデータはオリジナル側(parent)に記録している. */
    State *target = !is_dummy(s) ? s : state_get_parent(s);
    if (owner) {
      print_id = state_format_id(target, owner->is_formated);
    } else {
      print_id = state_format_id(target, FALSE);
    }
  }

  switch (lmn_env.mc_dump_format) {
  case LaViT:
    fprintf(f, "%lu::", print_id);
    state_print_mem(s, _fp);
    break;
  case FSM:
    /* under constructions.. */
    fprintf(f, "1\n");
    break;
  case Dir_DOT:
    if (state_succ_num(s) == 0) {
      fprintf(f, "  %lu [style=filled, fillcolor = \"#C71585\", shape = Msquare];\n",
                  print_id);
    }
    break;
  case CUI:
  {
    BOOL has_property = owner && statespace_has_property(owner);
#ifdef KWBT_OPT
    fprintf(f, "%lu::%lu::%s"
            , print_id, cost
             , has_property ? automata_state_name(statespace_automata(owner),
                                                  state_property_state(s))
                            : "");
#else
    fprintf(f, "%lu::%s"
             , print_id
             , has_property ? automata_state_name(statespace_automata(owner),
                                                  state_property_state(s))
                            : "");
#endif
    state_print_mem(s, _fp);
    break;
  }
  default:
    lmn_fatal("unexpected");
    break;
  }
}


void state_print_mem(State *s, LmnWord _fp)
{
  LmnMembrane *mem;
  ProcessID org_next_id;

  org_next_id = env_next_id();
  mem = state_restore_mem_inner(s, FALSE);
  env_set_next_id(org_next_id);

//  fprintf((FILE *)_fp, "natoms=%lu :: hash=%16lu ::", lmn_mem_atom_num(mem), s->hash);
  if (lmn_env.mc_dump_format == LaViT) {
    lmn_dump_cell_stdout(mem);
  } else {
    lmn_dump_mem_stdout(mem); /* TODO: グラフ構造の出力先をファイルポインタに */
    if (lmn_env.show_hyperlink) lmn_hyperlink_print(mem);
  }

  if (is_binstr_user(s)) {
    lmn_mem_free_rec(mem);
  }
}


/* TODO: 美しさ */
void state_print_transition(State *s, LmnWord _fp, LmnWord _owner)
{
  FILE *f;
  StateSpace owner;
  unsigned int i;

  BOOL need_id_foreach_trans;
  char *state_separator,
       *trans_separator,
       *label_begin,
       *label_end;
  BOOL formated;

  /* Rehashが発生している場合,
   * サクセッサへの情報は, RehashしたオリジナルのStateオブジェクトが保持しているため,
   * dummyフラグが真かつエンコード済みの状態には遷移情報は載っていない.
   * (エンコード済のバイナリストリングしか載っていない) */
  if ((is_dummy(s) && is_encoded(s))) return;

  f     = (FILE *)_fp;
  owner = (StateSpace)_owner;

  need_id_foreach_trans = TRUE;
  switch (lmn_env.mc_dump_format) {
  case DOT:
    state_separator = " -> ";
    trans_separator = NULL;
    label_begin     = " [ label = \"";
    label_end       = "\" ];";
    break;
  case FSM:
    state_separator = " ";
    trans_separator = NULL;
    label_begin     = " \"";
    label_end       = "\"";
    break;
  case LaViT: /* FALLTHROUGH: CUIモードと共通 */
  case CUI:
    state_separator = "::"; /* なぜかspaceが混ざるとlavitは読み込むことができない */
    trans_separator = ",";
    label_begin     = "(";
    label_end       = ")";
    need_id_foreach_trans = FALSE;
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }


  formated = owner ? owner->is_formated : FALSE;
  if (!need_id_foreach_trans) {
    fprintf(f, "%lu%s", state_format_id(s, formated), state_separator);
  }

  if (s->successors) {
    for (i = 0; i < state_succ_num(s); i++) { /* dump dst state's IDs */
      if (need_id_foreach_trans) {
        fprintf(f, "%lu%s", state_format_id(s, formated), state_separator);
      } else if (i > 0) {
        LMN_ASSERT(trans_separator);
        fprintf(f, "%s", trans_separator);
      }

      /* MEMO: rehashが発生していても, successorポインタを辿る先は, オリジナル */
      fprintf(f, "%lu", state_format_id(state_succ_state(s, i), formated));

      if (has_trans_obj(s)) {
        Transition t;
        unsigned int j;

        fprintf(f, "%s", label_begin);
        t = transition(s, i);

        for (j = 0; j < transition_rule_num(t); j++) {
          if (j > 0) fprintf(f, " "); /* ルール名の区切りは半角スペース1文字 */
          fprintf(f, "%s", lmn_id_to_name(transition_rule(t, j)));
        }
        fprintf(f, "%s", label_end);
      }

      if (i + 1 < state_succ_num(s) && need_id_foreach_trans) {
        fprintf(f,"\n");
      }
    }
    fprintf(f, "\n");
  } else if (!need_id_foreach_trans){
    fprintf(f, "\n");
  }
}


void state_print_label(State *s, LmnWord _fp, LmnWord _owner)
{
  Automata a;
  FILE *f;
  StateSpace owner;

  owner = (StateSpace)_owner;
  if (!statespace_has_property(owner) || (is_dummy(s) && is_encoded(s))) {
    return;
  }

  a     = statespace_automata(owner);
  f     = (FILE *)_fp;

  switch (lmn_env.mc_dump_format) {
  case Dir_DOT:
  {
    if (state_is_accept(a, s) || state_is_end(a, s)) {
      fprintf(f, "  %lu [peripheries = 2]\n", state_format_id(s, owner->is_formated));
    }
    break;
  }
  case LaViT:
    fprintf(f, "%lu::", state_format_id(s, owner->is_formated));
    fprintf(f, "%s\n", automata_state_name(a, state_property_state(s)));
  case FSM:
  case CUI:  /* 状態のグローバルルート膜の膜名としてdump済 */
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }
}
