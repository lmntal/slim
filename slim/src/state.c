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
#include "membrane.h"
#include "mem_encode.h"
#include "rule.h"
#include "mhash.h"
#include "error.h"
#include "dumper.h"
#ifdef PROFILE
#include "runtime_status.h"
#endif


static int kill_States_chains(st_data_t _k,
                              st_data_t state_ptr,
                              st_data_t rm_tbl_ptr);

static int print_state_mem(st_data_t _k, st_data_t state_ptr, st_data_t _a);
static int print_state_transition_graph(st_data_t _k, st_data_t state_ptr, st_data_t _a);

/*----------------------------------------------------------------------
 * State Space
 */

struct StateSpace {
  State *init_state;
  Vector *end_states;
  st_table_t tbl;

  /* ハッシュ値（mhash）が一定値（MEM_EQ_FAIL_THRESHOLD）以上衝突すると、
     そのハッシュ値を持つ状態は、膜のIDを計算し、以降はIDで比較を行う */
  st_table_t memid_tbl;        /* 膜のIDを計算した状態を格納（tblにも同じ状態がある） */
  HashSet memid_hashes;   /* 膜のIDで同型性の判定を行うハッシュ値(mhash)のSet */
};

/**
 * 非決定(--nd または --nd_result)実行終了時に状態遷移グラフを出力する．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
static int print_state_transition_graph(st_data_t _k, st_data_t state_ptr, st_data_t _a) {
  unsigned int j = 0;
  State *tmp = (State *)state_ptr;

  fprintf(stdout, "%lu::", (long unsigned int)tmp); /* dump src state's ID */
  while (j < vec_num(&tmp->successor)) { /* dump dst state's IDs */
    fprintf(stdout, "%lu", vec_get(&tmp->successor, j++));
    if (j < vec_num(&tmp->successor)) {
      fprintf(stdout,",");
    }
  }
  fprintf(stdout, "\n");

  return ST_CONTINUE;
}

StateSpace state_space_make()
{
  struct StateSpace *ss = LMN_MALLOC(struct StateSpace);
  ss->init_state = NULL;
  if (lmn_env.mem_enc) ss->tbl = st_init_table(&type_memid_statehash);
  else ss->tbl = st_init_table(&type_statehash);
  ss->memid_tbl = st_init_table(&type_memid_statehash);
  ss->end_states = vec_make(64);
  hashset_init(&ss->memid_hashes, 128);
  return ss;
}

void state_space_free(StateSpace states)
{
  HashSet rm_tbl; /* LTLモデル検査モード時に二重解放を防止するため */

  hashset_init(&rm_tbl, 16);
  st_foreach(states->tbl, kill_States_chains, (st_data_t)&rm_tbl);
  hashset_destroy(&rm_tbl);
  st_free_table(states->tbl);
  st_free_table(states->memid_tbl);
  vec_free(states->end_states);
  hashset_destroy(&states->memid_hashes);
  LMN_FREE(states);
}

/* 初期状態を追加する */
void state_space_set_init_state(StateSpace states, State* init_state)
{
  states->init_state = init_state;
  state_space_insert(states, init_state);
}

/* 状態空間に存在する状態の数を返す */
State *state_space_init_state(StateSpace states)
{
  return states->init_state;
}

unsigned long state_space_num(StateSpace states)
{
  return st_num(states->tbl);
}

/* 状態空間に**すでに含まれている**状態sを最終状態として登録する */
void state_space_add_end_state(StateSpace states, State *s)
{
  vec_push(states->end_states, (LmnWord)s);
}

/* 状態空間内に存在する、最終状態を返す */
const Vector *state_space_end_states(StateSpace states)
{
  return states->end_states;
}

static int state_space_mem_encode_f(st_data_t _k, st_data_t _s, st_data_t _t)
{
  StateSpace states = (StateSpace)_t;
  State *s = (State *)_s;

  state_calc_mem_encode(s);
  st_add_direct(states->memid_tbl, (st_data_t)s, (st_data_t)s);
  return ST_CONTINUE;
}

/* 膜のIDを計算するハッシュ値(mhash)を追加する */
void state_space_add_memid_hash(StateSpace states, unsigned long hash)
{
  hashset_add(&states->memid_hashes, hash);
  st_foreach_hash(states->tbl, hash, state_space_mem_encode_f, (st_data_t)states);
}

/* hashが膜のIDを計算しているハッシュならば真、そうでなければ偽を返す */
inline BOOL state_space_is_memid_hash(StateSpace states, unsigned long hash)
{
  return hashset_contains(&states->memid_hashes, hash);
}

/**
 * 非決定実行 or LTLモデル検査終了後にStates内に存在するチェインをすべてfreeする
 * 高階関数st_foreach(c.f. st.c)に投げて使用
 */
static int kill_States_chains(st_data_t _k, st_data_t state_ptr, st_data_t rm_tbl_ptr)
{
  State *tmp = (State *)state_ptr;
  HashSet *rm_tbl = (HashSet *)rm_tbl_ptr;

  if(tmp->mem && hashset_contains(rm_tbl, (HashKeyType)tmp->mem)) {
    vec_destroy(&tmp->successor);
    LMN_FREE(tmp);
  }
  else {
    hashset_add(rm_tbl, (HashKeyType)tmp->mem);
    state_free(tmp);
  }
  return ST_CONTINUE;
}

/* 状態空間に状態sを追加する。既に等価な状態tが状態空間に存在した場合は
   追加せずにtを返し、追加し場合は、sを返す。*/
State *state_space_insert(StateSpace states, State *s)
{
  long col;
  BOOL has_mem_id;
  st_data_t t;

  has_mem_id = state_space_is_memid_hash(states, s->hash);
  if (has_mem_id) {
    state_calc_mem_encode(s);

    if (st_lookup(states->memid_tbl, (st_data_t)s, (st_data_t *)&t)) {
      /* すでに等価な状態が存在する */
      return (State *)t;
    } else {
      /* 等価な状態はない */
      st_add_direct(states->tbl, (st_data_t)s, (st_data_t)s);
      st_add_direct(states->memid_tbl, (st_data_t)s, (st_data_t)s);
      return s;
    }
  }
  else {
    if (st_lookup_with_col(states->tbl, (st_data_t)s, (st_data_t *)&t, &col)) {
      /* すでに等価な状態が存在する */
      return (State *)t;
    } else {
      /* 等価な状態はない */
      st_add_direct(states->tbl, (st_data_t)s, (st_data_t)s); /* 状態空間に追加 */

      if (col >= MEM_EQ_FAIL_THRESHOLD) {
        state_space_add_memid_hash(states, s->hash);
      } else if (!lmn_env.mem_enc) {
        /* 状態の追加時に膜のダンプを計算する */
        state_calc_mem_dump(s);
      }
      return s;
    }
  }
}

/* 状態空間内のsと等価な状態を返す。存在しない場合は、NULLを返す */
State *state_space_get(const StateSpace states, State *s)
{
  st_data_t t;
  if (st_lookup(states->tbl, (st_data_t)s, &t)) {
    return (State *)t;
  } else {
    return NULL;
  }
}

/* 状態空間から状態sを削除する。ただし、状態空間のサクセッサは更新され
   ないので注意 */
void state_space_remove(const StateSpace states, State *s)
{
  st_delete(states->tbl, (st_data_t)s, 0);
}

st_table_t state_space_tbl(StateSpace states)
{
  return states->tbl;
}


void dump_all_state_mem(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "States\n");
  st_foreach(states->tbl, print_state_mem, 0);
  fprintf(file, "\n");
}


void dump_state_transition_graph(StateSpace states, FILE *file)
{
  if (!lmn_env.dump) return;
  fprintf(file, "Transitions\n");
  fprintf(file, "init:%lu\n", (long unsigned int)state_space_init_state(states));
  st_foreach(states->tbl, print_state_transition_graph, 0);
  fprintf(file, "\n");
}


/*----------------------------------------------------------------------
 * State
 */

struct st_hash_type type_statehash = {
  state_cmp,
  state_hash
};

struct st_hash_type type_memid_statehash = {
  state_memid_cmp,
  state_memid_hash
};

/**
 * コンストラクタ
 */
State *state_make(LmnMembrane *mem, BYTE state_name, LmnRule rule) {
  State *new = LMN_MALLOC(State);
  new->mem = mem;
  new->state_name = state_name;
  new->flags = 0x00U;
  /* successorの記憶域をゼロクリアする */
  vec_init(&new->successor, 4);
  new->rule = rule;
  new->mem_id = NULL;
  new->mem_dump = NULL;
  new->hash = 0;
  new->mem_id_hash = 0;
  
  if (lmn_env.mem_enc) {
    new->mem_id = lmn_mem_encode(mem);
    new->mem_id_hash = binstr_hash(new->mem_id);
  } else {
    new->hash = mhash(new->mem);
  }

#ifdef PROFILE
  status_create_new_state();
#endif
  return new;
}

/**
 * コンストラクタ
 */
State *state_make_for_nd(LmnMembrane *mem, LmnRule rule) {
  return state_make(mem, DEFAULT_STATE_ID, rule);
}

/**
 * デストラクタ
 */
void state_free(State *s) {
  if (s->mem) {
    lmn_mem_drop(s->mem);
    lmn_mem_free(s->mem);
  }
  vec_destroy(&s->successor);
  if (s->mem_id) lmn_binstr_free(s->mem_id);
  if (s->mem_dump) lmn_binstr_free(s->mem_dump);
  LMN_FREE(s);
}

inline void state_free_mem(State *s)
{
  if (s->mem) {
    lmn_mem_drop(s->mem);
    lmn_mem_free(s->mem);
    s->mem = NULL;
  }
}


inline long state_hash(State *s) {
  return s->hash;
}

BYTE state_property_state(State *state)
{
  return state->state_name;
}

void state_set_property_state(State *state, BYTE prop_state)
{
  state->state_name = prop_state;
}

inline LmnMembrane *state_mem(State *state)
{
  return state->mem;
}

/* 状態の膜と等価な、新たに生成した膜を返す */
inline LmnMembrane *state_copied_mem(State *state)
{
  if (state->mem) return lmn_mem_copy(state->mem);
  else if (state->mem_id) return lmn_binstr_decode(state->mem_id);
  else {
    lmn_fatal("unexpected");
    return NULL;
  }
}

inline LmnRule state_rule(State *state)
{
  return state->rule;
}

inline LmnBinStr state_mem_binstr(State *state)
{
  return state->mem_id ? state->mem_id : state->mem_dump;
}

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 */
static int state_equals(HashKeyType k1, HashKeyType k2)
{
  State *s1 = (State *)k1;
  State *s2 = (State *)k2;

  int t;

  if (lmn_env.mem_enc) {
    t =
      s1->state_name == s2->state_name &&
      s1->mem_id_hash == s2->mem_id_hash &&
      binstr_comp(s1->mem_id, s2->mem_id) == 0;
  }
  else if (s1->mem_id && s2->mem_id) {
    /* 膜のIDで比較 */
    t =
      s1->state_name == s2->state_name &&
      s1->mem_id_hash == s2->mem_id_hash &&
      binstr_comp(s1->mem_id, s2->mem_id) == 0;
  } else if (s1->mem_dump || s2->mem_dump) {
    /* 同型性判定 */
      t =
        s1->state_name == s2->state_name &&
        s1->hash == s2->hash &&
        (s1->mem_id ? lmn_mem_equals_enc(s1->mem_dump, s2->mem) : lmn_mem_equals_enc(s2->mem_dump, s1->mem));
  } else {
    lmn_fatal("implementation error");
  }

  return t;
}

/**
 * 与えられた2つの状態が互いに異なっていれば真を、逆に等しい場合は偽を返す
 */
int state_cmp(HashKeyType s1, HashKeyType s2) {
  return !state_equals(s1, s2);
}

/**
 * 与えられた2つの状態を膜のIDを使い比較する。が互いに異なっていれば真
 * を、逆に等しい場合は偽を返す
 */
int state_memid_cmp(st_data_t _s1, st_data_t _s2) {
  State *s1 = (State *)_s1;
  State *s2 = (State *)_s2;

  return !(
    s1->state_name == s2->state_name &&
    s1->mem_id_hash == s2->mem_id_hash &&
    binstr_comp(s1->mem_id, s2->mem_id) == 0);

}

/* 状態が持つ膜のIDのハッシュ値を返す。膜のIDを持たない場合は有効な値を
   返さない */
inline long state_memid_hash(State *s) {
  return s->mem_id_hash;
}

/* 状態が持つ膜のダンプを計算する。 */
inline void state_free_mem_dump(State *s)
{
  if (s->mem_dump) {
    lmn_binstr_free(s->mem_dump);
    s->mem_dump = NULL;
  }
}


/* 状態が持つ膜のエンコードを計算する。mem_dumpを持っている場合は、
   mem_dumpを解放する */
inline void state_calc_mem_encode(State *s)
{
  if (!s->mem_id) {
    if (s->mem) s->mem_id = lmn_mem_encode(s->mem);
    else if (s->mem_dump) {
      LmnMembrane *m;

      m = lmn_binstr_decode(s->mem_dump);
      s->mem_id = lmn_mem_encode(m);
      lmn_mem_drop(m);
      lmn_mem_free(m);
    } else {
      lmn_fatal("implementation error");
    }
    s->mem_id_hash = binstr_hash(s->mem_id);
    state_free_mem_dump(s);
  }
}

/* 状態の膜のダンプを計算する */
inline void state_calc_mem_dump(State *s)
{
  if (s->mem_id) lmn_fatal("implementation error");
  if (!s->mem_dump) s->mem_dump = lmn_mem_to_binstr(s->mem);
}

/* 状態にサクセッサを追加 */
inline void state_succ_add(State *s, State *succ) {
  vec_push(&s->successor, (vec_data_t)succ);
}

/* 状態のサクセッサの数を返す */
inline unsigned int state_succ_num(State *s) {
  return vec_num(&s->successor);
}

/* 状態のサクセッサを取得 */
inline State *state_succ_get(State *s, unsigned int i) {
  return (State *)vec_get(&s->successor, i);
}

/* 状態の膜（LmnMembrane）を復元する。*/
inline void state_restore_mem(State *s)
{
  if (!s->mem) {
    s->mem = lmn_binstr_decode(state_mem_binstr(s));
  }
}

/**
 * --ltl_nd実行終了時に状態の内容を出力する．
 * 高階関数st_foreach(c.f. st.c)に投げて使用．
 */
static int print_state_mem(st_data_t _k, st_data_t state_ptr, st_data_t _a) {
  State *tmp = (State *)state_ptr;
  dump_state_data(tmp);
  return ST_CONTINUE;
}

void dump_state_data(State *state)
{
  if (!lmn_env.dump) return;
  fprintf(stdout, "%lu::", (long unsigned int)state); /* dump src state's ID */
  if (!state->mem)
    lmn_fatal("unexpected");
  lmn_dump_cell_stdout(state->mem); /* dump src state's global root membrane */
}
