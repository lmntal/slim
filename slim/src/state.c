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
#include "parallel.h"
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
  new->compress_mem     = NULL;
  new->hash             = 0;
  new->next             = NULL;
  new->successors       = NULL;
  new->successor_num    = 0;
  new->predecessor_num  = 0;
  new->predecessor      = NULL;
  state_id_issue(new);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
  }
#endif
  return new;
}

/* 状態のハッシュ値を計算する */
inline void state_calc_hash(State *s, LmnMembrane *mem, BOOL encode) {
  if (encode) {
    s->compress_mem = lmn_mem_encode(mem);
    s->hash         = binstr_hash(s->compress_mem);
    set_encoded(s);
  } else {
    s->hash = mhash(mem);
  }
}

/* 状態をコピーして返す. 膜やバイト列も, 実態をコピーする
 * ただし, 状態フラグのコピーはencode関連のみ */
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

    if (lmn_env.show_transition) {
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

  if (is_encoded(check) && is_encoded(stored)) {
    /* 膜のIDで比較 */
    t =
      check->state_name == stored->state_name &&
      binstr_compare(state_mem_binstr(check),
                     state_mem_binstr(stored)) == 0;
  }
  else if (state_mem_binstr(check) || state_mem_binstr(stored)) {
    /* 同型性判定 */
    LMN_ASSERT(state_mem(check) && state_mem_binstr(stored));
    t =
      check->state_name == stored->state_name &&
      state_mem_binstr(stored) ? lmn_mem_equals_enc(state_mem_binstr(stored), state_mem(check))
                                : lmn_mem_equals_enc(state_mem_binstr(check),  state_mem(stored));
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
int state_cmp_with_compress(State *s1, State *s2)
{
  return !state_equals_with_compress(s1, s2);
}

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

Transition transition(State *s, unsigned int i)
{
  return (Transition)(s->successors[i]);
}

Transition transition_make(State *s, lmn_interned_str rule_name)
{
  return transition_make_with_id(s, DEFAULT_TRANSITION_ID, rule_name);
}

Transition transition_make_with_id(State *s, unsigned long id, lmn_interned_str rule_name)
{
  struct Transition *t = LMN_MALLOC(struct Transition);

  t->s  = s;
  t->id = id;
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
  if (rule_name != ANONYMOUS) {
    int i;
    /* ルール名の重複検査 */
    for (i = 0; i < vec_num(&t->rule_names); i++) {
      if (vec_get(&t->rule_names, i) == rule_name) return;
    }

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
void state_set_id_for_dump(State *s, LmnWord _d)
{
  static unsigned long id = 1;
  state_set_format_id(s, id++);
}

void dump_state_data(State *s, LmnWord _fp)
{
  FILE *f;
  unsigned long dump_id;

  if (is_dummy(s) && !is_encoded(s)) return; /* 状態データはdummy側に存在 */

  f = (FILE *)_fp;
  dump_id = state_format_id(s);

  switch (lmn_env.mc_dump_format) {
  case LaViT:
    fprintf(f, "%lu::", dump_id);
    state_print_mem(s, _fp);
    break;
  case FSM:
    /* under constructions.. */
    fprintf(f, "1\n");
    break;
  case Dir_DOT:
    if (state_succ_num(s) == 0) {
      fprintf(f, "  %lu [style=filled, fillcolor = \"#C71585\", shape = Msquare];\n",
                  dump_id);
    }
    break;
  case CUI:
    fprintf(f, "%lu::%s"
             , dump_id
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

  lmn_dump_mem_stdout(mem); /* TODO: グラフ構造の出力先をファイルポインタに */
  if (!state_mem(s)) {
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
  }
}

/* あとで書き直す */
void state_print_transition(State *s, LmnWord _fp)
{
  FILE *f;
  unsigned int i;
  BOOL need_id_foreach_trans;
  char *state_separator,
       *trans_separator,
       *label_begin,
       *label_end;

  if (is_dummy(s) && is_encoded(s)) return;

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

      fprintf(f, "%lu", state_format_id(state_succ_get(s, i)));

      if (lmn_env.show_transition) {
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
    AutomataState atm;
    atm = automata_get_state(mc_data.property_automata,
                             state_property_state(s));
    if (atmstate_is_accept(atm) || atmstate_is_end(atm)) {
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

void state_print_error_path(State *s, LmnWord _fp)
{
  FILE *f;
  if ((is_dummy(s) && is_encoded(s)) || !is_on_cycle(s)) {
    return;
  }
  f = (FILE *)_fp;

  switch (lmn_env.mc_dump_format) {
  case LaViT:
    fprintf(f, "%lu::", state_format_id(s));
    if (s->successors) {
      unsigned int i;
      BOOL dump_once = FALSE;
      for (i = 0; i < state_succ_num(s); i++) {
        State *succ = state_succ_get(s, i);
        if (is_on_cycle(succ)) {
          fprintf(f, "%s%lu", (dump_once ? ", " : ""), state_format_id(succ));
          if (!dump_once) {
            dump_once = TRUE;
          }
        }
      }
    }
    fprintf(f, "\n");
    break;
  case Dir_DOT:  /* 実装予定 */
  case FSM:      /* 未定 */
  case CUI:      /* 別で出力 */
    break;
  default:
    lmn_fatal("unexpected");
    break;
  }
}

