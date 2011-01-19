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


/*----------------------------------------------------------------------
 * State
 */

State *state_make(LmnMembrane *mem, BYTE property_label, BOOL encode)
{
  /* delta-mem時には呼ばない */
  State *new = state_make_minimal();

  new->mem = mem;
  new->state_name = property_label;
  state_calc_hash(new, mem, encode);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
  }
#endif
  return new;
}


/* まっさらなState構造体をmallocして返してもらう */
inline State *state_make_minimal()
{
  State *new = LMN_MALLOC(State);
  new->mem              = NULL;
  new->state_name       = 0x00U;
  new->flags            = 0x00U;
  new->flags2           = 0x00U;
  new->flagsN           = 0x00U;
  new->compress_mem     = NULL;
  new->hash             = 0;
  new->next             = NULL;
  new->successors       = NULL;
  new->successor_num    = 0;
  new->parent           = NULL;
  new->state_id         = 0;
  new->map              = NULL;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
  }
#endif
  return new;
}


/* 状態のハッシュ値を計算する */
inline void state_calc_hash(State *s, LmnMembrane *mem, BOOL encode)
{
  if (encode) {
    s->compress_mem = lmn_mem_encode(mem);
    s->hash         = binstr_hash(s->compress_mem);
    set_encoded(s);
  } else {
    s->hash = mhash(mem);
  }
}


/* 状態をコピーして返す.
 * コピー内容:
 *   膜, バイト列の実態
 *   encode関連の状態フラグ */
State *state_copy(State *src)
{
  return state_copy_with_mem(src, state_mem(src));
}

inline
State *state_copy_with_mem(State *src, LmnMembrane *mem)
{
  State *dst;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
    profile_start_timer(PROFILE_TIME__STATE_COPY);
  }
#endif

  dst = state_make_minimal();

  dst->mem = mem ? lmn_mem_copy(mem) : NULL;
  dst->hash = src->hash;

  if (state_mem_binstr(src)) {
    dst->compress_mem = lmn_binstr_copy(state_mem_binstr(src));
    if (is_encoded(src)) {
      set_encoded(dst);
    }
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
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
  state_free_mem(s);

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
  state_free_compress_mem(s);
  LMN_FREE(s);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
  }
#endif
}

inline void state_free_mem(State *s)
{
  if (state_mem(s)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(state_mem(s)));
    }
#endif
    lmn_mem_drop(state_mem(s));
    lmn_mem_free(state_mem(s));
    s->mem = NULL;
  }
}


void state_succ_set(State *s, Vector *v) {
  if (vec_num(v) > 0) {
    unsigned int i;
    state_succ_num(s) = vec_num(v);
    s->successors = LMN_NALLOC(succ_data_t, state_succ_num(s));
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
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT,
                        sizeof(succ_data_t) * state_succ_num(s));
      profile_add_space(PROFILE_SPACE__TRANS_OBJECT, 0);
    }
#endif


  LMN_FREE(s->successors);
  s->successors = NULL;
  state_succ_num(s) = 0;
  unset_trans_obj(s);
}


/* 状態の膜と等価な、新たに生成した膜を返す */
inline LmnMembrane *state_copied_mem(State *s)
{
  LmnMembrane *ret = NULL;
  if (state_mem(s)) {
    ret = lmn_mem_copy(state_mem(s));
  }
  else if (state_mem_binstr(s)) {
    ret = lmn_binstr_decode(state_mem_binstr(s));
  }
  else {
    lmn_fatal("unexpected");
  }
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(ret));
  }
#endif
  return ret;
}


/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals_with_compress(State *check, State *stored)
{
  int t;

#ifdef PROFILE
  if (lmn_env.prof_no_memeq) {
    /* ハッシュコンフリクトによって完全性を損なうが
     * ハッシュ値と状態ラベルで等価性を判定することで高速化できる */
    return (state_hash(check) == state_hash(stored));
  }
#endif

  if (is_encoded(check) && is_encoded(stored)) {
    /* 膜のIDで比較 */
    t =
      check->state_name == stored->state_name &&
      binstr_compare(state_mem_binstr(check),
                     state_mem_binstr(stored)) == 0;
  }
  else if (state_mem(check) && state_mem_binstr(stored)) {
    /* 同型性判定 */
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(state_mem_binstr(stored), state_mem(check));
  }
  else if (state_mem(check) && state_mem_binstr(stored)) {
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(state_mem_binstr(check), state_mem(stored));
  }
  else if (state_mem_binstr(check) && state_mem_binstr(stored)) {
    LmnMembrane *mem = lmn_binstr_decode(state_mem_binstr(check));
    t =
      check->state_name == stored->state_name &&
      lmn_mem_equals_enc(state_mem_binstr(stored), mem);
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }
  else {
    lmn_fatal("implementation error");
  }

  return t;
}

static int state_equals(State *s1, State *s2)
{
  return
    s1->state_name == s2->state_name &&
    s1->hash       == s2->hash       &&
    lmn_mem_equals(s1->mem, s2->mem);
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
int state_cmp_with_compress(State *s1, State *s2)
{
  /* ignore checking for model checking using canonical membrane */
  if (lmn_env.debug_isomor && !(is_encoded(s1) && is_encoded(s2))) {
    LmnMembrane *s2_m;
    LmnBinStr s1_e, s2_e;
    BOOL ret_check, ret_org;

    s2_m  = lmn_binstr_decode(state_mem_binstr(s2));
    s1_e = lmn_mem_encode(state_mem(s1));
    s2_e = lmn_mem_encode(s2_m);
    ret_check = binstr_compare(s1_e, s2_e) == 0;

    ret_org   = state_equals_with_compress(s1, s2);

    /* if both checking has been returned same result, then it's safe. */
    if (!((ret_check && ret_org) || (!ret_check && !ret_org))) {
      FILE *f = stdout;
      fprintf(f, "found, isomorphism error.\n");
      fprintf(f, "  *** state_eq succ_mem vs. stored binstr *** >> result=%s\n", ret_org ? "SAME" : "DIFFERENT");
      fprintf(f, "  [checking_state, org_mem]\n");
      fprintf(f, "    ");
      lmn_dump_mem_stdout(state_mem(s1));

      fprintf(f, "  [stored_state, binstr_decode_mem]\n");
      fprintf(f, "    ");
      lmn_dump_mem_stdout(s2_m);
      state_print_mem(s2, (LmnWord)stdout);

      fprintf(f, "\n");
      fprintf(f, "  *** state_eq canonical binstr compare *** >> result=%s\n", ret_check ? "SAME" : "DIFFERENT");
      fprintf(f, "  [checking state, org_mem--->**mem_id**--->restore_mem]\n");
      fprintf(f, "    ");
      lmn_dump_mem_stdout(lmn_binstr_decode(s1_e)); /* memory-leak, but ok */
      fprintf(f, "  [stored state, binstr--->restore_mem--->**mem_id**-->restore_mem]\n");
      fprintf(f, "    ");
      lmn_dump_mem_stdout(lmn_binstr_decode(s2_e)); /* memory-leak, but ok */

      fprintf(f, "  [dump_binstr]\n");
      lmn_binstr_dump(s1_e);
      lmn_binstr_dump(s2_e);

      /* TODO: 等価な状態をどの時点で異形と判定しているかの検査を
       *        ここに書く or 書いたものをここで呼び出す */

      lmn_fatal("invalid ends.");
    }

   
    lmn_mem_drop(s2_m);
    lmn_mem_free(s2_m);
    
    lmn_binstr_free(s1_e);
    lmn_binstr_free(s2_e);

    return !ret_org;
  }
  else {
    return !state_equals_with_compress(s1, s2);
  }
}
#endif


int state_cmp(State *s1, State *s2)
{
  return !state_equals(s1, s2);
}


inline void state_free_compress_mem(State *s)
{
  if (s->compress_mem) {
    lmn_binstr_free(s->compress_mem);
    s->compress_mem = NULL;
  }
}


/* 膜のIDと膜のIDのハッシュ値へ計算し直す.
 * mem_dumpを持っている場合は, mem_dumpを解放する.
 * MT-UnSafe */
inline void state_calc_mem_encode(State *s)
{
  if (!is_encoded(s)) {
    if (state_mem(s)) {
      state_free_compress_mem(s);
      state_set_compress_mem(s, lmn_mem_encode(state_mem(s)));
    }
    else if (state_mem_binstr(s)) {
      LmnMembrane *m;

      m = lmn_binstr_decode(state_mem_binstr(s));
      state_free_compress_mem(s);
      state_set_compress_mem(s, lmn_mem_encode(m));
      lmn_mem_drop(m);
      lmn_mem_free(m);
    } else {
      lmn_fatal("implementation error");
    }

    s->hash = binstr_hash(state_mem_binstr(s));
    set_encoded(s);
  }
}


inline LmnBinStr state_calc_mem_dump_with_z(State *s)
{
  if (!state_mem_binstr(s)) {
    LmnBinStr bs, c;
    LMN_ASSERT(state_mem(s));
    bs  = lmn_mem_to_binstr(state_mem(s));
    c = lmn_bscomp_z_encode(bs);
    if (bs == c) { /* 圧縮失敗 */
      lmn_binstr_free(c);
      c = bs;
    } else {       /* 圧縮成功 */
      lmn_binstr_free(bs);
    }
    return c;
  }
  else {
    /* TODO: 圧縮されてないときは圧縮する, というコードを書いてない */
    return s->compress_mem;
  }
}


/* 状態の膜のダンプを計算する */
inline LmnBinStr state_calc_mem_dump(State *s)
{
  LMN_ASSERT(!is_encoded(s));
  LMN_ASSERT(state_mem(s));
  if (!s->compress_mem) {
    return lmn_mem_to_binstr(state_mem(s));
  } else {
    return s->compress_mem;
  }
}


inline LmnBinStr state_calc_mem_dummy(State *s)
{
  /* DUMMY: nothing to do */
  return NULL;
}


inline unsigned long transition_space(Transition t)
{
  unsigned long ret;
  ret  = sizeof(struct Transition);
  ret += vec_space_inner(&t->rule_names);
  return ret;
}


Transition transition_make(State *s, lmn_interned_str rule_name)
{
  struct Transition *t = LMN_MALLOC(struct Transition);

  t->s  = s;
  t->id = 0;
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


void transition_add_rule(Transition t, lmn_interned_str rule_name)
{
  if (rule_name != ANONYMOUS || !vec_contains(&t->rule_names, rule_name)) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
#endif

    vec_push(&t->rule_names, rule_name);

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) profile_add_space(PROFILE_SPACE__TRANS_OBJECT, transition_space(t));
#endif
  }
}


/** Printer */
void dump_state_data(State *s, LmnWord _fp)
{
  FILE *f;

  if (is_dummy(s) && !is_encoded(s)) return; /* 状態データはdummy側に存在 */

  f = (FILE *)_fp;

  switch (lmn_env.mc_dump_format) {
  case LaViT:
    fprintf(f, "%lu::", state_format_id(s));
    state_print_mem(s, _fp);
    break;
  case FSM:
    /* under constructions.. */
    fprintf(f, "1\n");
    break;
  case Dir_DOT:
    if (state_succ_num(s) == 0) {
      fprintf(f, "  %lu [style=filled, fillcolor = \"#C71585\", shape = Msquare];\n",
                  state_format_id(s));
    }
    break;
  case CUI:
    fprintf(f, "%lu::%s"
             , state_format_id(s)
             , !mc_data.has_property ? ""
                                     : automata_state_name(mc_data.property_automata,
                                                           state_property_state(s)));
    state_print_mem(s, _fp);
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }
}


void state_print_mem(State *s, LmnWord _fp)
{
  LmnMembrane *mem;

  mem = state_mem(s);

  if (!mem) {
    unsigned int org_next_id = env_next_id();
    mem = lmn_binstr_decode(state_mem_binstr(s));
    env_set_next_id(org_next_id);
  }

//  fprintf(stdout, "natoms=%lu :: hash=%16lu ::", lmn_mem_atom_num(mem), s->hash);
  if (lmn_env.mc_dump_format == LaViT) {
    lmn_dump_cell_stdout(mem);
  } else {
    lmn_dump_mem_stdout(mem); /* TODO: グラフ構造の出力先をファイルポインタに */
  }
  if (!state_mem(s)) {
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }
}


/* TODO: 美しさ */
void state_print_transition(State *s, LmnWord _fp)
{
  FILE *f;
  unsigned int i;

  BOOL need_id_foreach_trans;
  char *state_separator,
       *trans_separator,
       *label_begin,
       *label_end;

  if ((is_dummy(s) && is_encoded(s))) return;

  f = (FILE *)_fp;

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
  case LaViT: /* FALLTHROUGH CUIモードと共通 */
  case CUI:
    state_separator = "::"; /* spaceが混ざるとlavitは読み込むことができない */
    trans_separator = ",";
    label_begin     = "(";
    label_end       = ")";
    need_id_foreach_trans = FALSE;
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }

  if (!need_id_foreach_trans) {
    fprintf(f, "%lu%s", state_format_id(s), state_separator);
  }

  if (s->successors) {
    for (i = 0; i < state_succ_num(s); i++) { /* dump dst state's IDs */
      if (need_id_foreach_trans) {
        fprintf(f, "%lu%s", state_format_id(s), state_separator);
      } else if (i > 0) {
        LMN_ASSERT(trans_separator);
        fprintf(f, "%s", trans_separator);
      }


      fprintf(f, "%lu", state_format_id(state_succ_state(s, i)));

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


void state_print_label(State *s, LmnWord _fp)
{
  FILE *f;
  if (!mc_data.has_property || (is_dummy(s) && is_encoded(s))) {
    return;
  }

  f = (FILE *)_fp;
  switch (lmn_env.mc_dump_format) {
  case Dir_DOT:
  {
    if (state_is_accept(s) || state_is_end(s)) {
      fprintf(f, "  %lu [peripheries = 2]\n", state_format_id(s));
    }
    break;
  }
  case LaViT:
    fprintf(f, "%lu::", state_format_id(s));
    fprintf(f, "%s\n", automata_state_name(mc_data.property_automata,
                                           state_property_state(s)));
  case FSM:
  case CUI:  /* 状態のグローバルルート膜の膜名としてdump済 */
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }
}


