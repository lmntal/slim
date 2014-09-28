/*
 * statespace.h
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

/** @author Masato Gocho
 *  Closed Address Hash Table / Parallel Hash Table for State Management Table
 */

#ifndef LMN_STATESPACE_H
#define LMN_STATESPACE_H

#include "lmntal.h"
#include "st.h"
#include "vector.h"
#include "queue.h"
#include "automata.h"
#include "state.h"
#include "slim_header/port.h"
#include "lmntal_thread.h"
#include "delta_membrane.h"
#include "mem_encode.h"

struct statespace_type {
  int(*compare) ( );               /* 状態の等価性判定を行う関数 */
  LmnBinStr(*compress) ( ); /* 状態sの圧縮バイト列を計算して返す関数 */
};


struct StateSpace {
  BYTE            tbl_type;       /* なんらかの特殊操作を行うためのフラグフィールド */
  BOOL            is_formated;    /* ハッシュ表の並びを崩した整列を行った場合に真 */
  /* 2bytes alignment */
  unsigned int    thread_num;     /* 本テーブルの操作スレッド数 */

  FILE            *out;           /* dump先 */
  State           *init_state;    /* 初期状態 */
  Vector          *end_states;    /* 最終状態の集合 */
  StateTable      *tbl;           /* mhash値をkeyに, 状態のアドレスを登録する状態管理表 */
  StateTable      *memid_tbl;     /* memid_hashをkeyに, 状態のアドレスを登録する状態管理表 */
  StateTable      *acc_tbl;
  StateTable      *acc_memid_tbl;

  Automata       property_automata;  /* Never Clainへのポインタ */
  Vector         *propsyms;          /* 命題記号定義へのポインタ */

#ifdef PROFILE
  HashSet memid_hashes;   /* 膜のIDで同型性の判定を行うハッシュ値(mhash)のSet */
#endif
};

#define statespace_has_property(SS)     ((SS)->property_automata)
#define statespace_automata(SS)         ((SS)->property_automata)
#define statespace_propsyms(SS)         ((SS)->propsyms)

/* the member "tbl_type" in struct StateSpace */
#define SS_MEMID_MASK           (0x01U)
#define SS_REHASHER_MASK        (0x01U << 1)

#define statespace_use_memenc(SS)       ((SS)->tbl_type &    SS_MEMID_MASK)
#define statespace_set_memenc(SS)       ((SS)->tbl_type |=   SS_MEMID_MASK)
#define statespace_unset_memenc(SS)     ((SS)->tbl_type &= (~SS_MEMID_MASK))
#define statespace_use_rehasher(SS)     ((SS)->tbl_type &    SS_REHASHER_MASK)
#define statespace_set_rehasher(SS)     ((SS)->tbl_type |=   SS_REHASHER_MASK)
#define statespace_unset_rehasher(SS)   ((SS)->tbl_type &= (~SS_REHASHER_MASK))

struct StateTable {
  BOOL             use_rehasher;
  BYTE             thread_num;
  struct statespace_type *type;
  State            **tbl;
  unsigned long    cap;
  unsigned long    cap_density;
  unsigned long    *num;
  unsigned long    *num_dummy;
  EWLock           *lock;
  StateTable       *rehash_tbl;   /* rehashした際に登録するテーブル */
};

#define DEFAULT_ARGS  (LmnWord)NULL





/** -----------
 *  StateTable
 */

void statetable_foreach(StateTable *st, void (*func) ( ),
                               LmnWord _arg1, LmnWord _arg2);
void statetable_foreach_parallel(StateTable *st, void (*mt_safe_func) ( ),
                                 LmnWord _arg1, LmnWord _arg2, int nthreads);
void statetable_format_states(StateTable *st);

static inline void          statetable_set_lock(StateTable *st, EWLock *lock);
static inline void          statetable_set_rehasher(StateTable *st);
static inline BOOL          statetable_use_rehasher(StateTable *st);
static inline unsigned long statetable_num_by_me(StateTable *st);
static inline unsigned long statetable_num(StateTable *st);
static inline unsigned long statetable_cap(StateTable *st);
static inline unsigned long statetable_cap_density(StateTable *st);
static inline void          statetable_num_add(StateTable *st, unsigned long n);
static inline void          statetable_num_sub(StateTable *st, unsigned long n);
static inline void          statetable_dummy_add(StateTable *st, unsigned long n);
static inline void          statetable_dummy_sub(StateTable *st, unsigned long n);
static inline void          statetable_set_rehash_tbl(StateTable *st,
                                                      StateTable *rehash_tbl);
static inline StateTable   *statetable_rehash_tbl(StateTable *st);
static inline unsigned long statetable_space(StateTable *tbl);



static inline void statetable_set_lock(StateTable *st, EWLock *lock) {
  st->lock = lock;
}

static inline void statetable_set_rehasher(StateTable *st) {
  st->use_rehasher = TRUE;
}

static inline BOOL statetable_use_rehasher(StateTable *st) {
  return st->use_rehasher;
}

static inline unsigned long statetable_num_by_me(StateTable *st) {
  return st->num[env_my_thread_id()];
}

static inline unsigned long statetable_num(StateTable *st) {
  unsigned long ret = 0;
  if (st) {
    unsigned int i;
    for (i = 0; i < st->thread_num; i++) {
      ret += st->num[i];
    }
  }
  return ret;
}

static inline unsigned long statetable_cap(StateTable *st) {
  return st->cap;
}

static inline unsigned long statetable_cap_density(StateTable *st) {
  return st->cap_density;
}

static inline void statetable_num_add(StateTable *st, unsigned long n) {
  st->num[env_my_thread_id()] += n;
}

static inline void statetable_num_sub(StateTable *st, unsigned long n) {
  st->num[env_my_thread_id()] -= n;
}

static inline void statetable_dummy_add(StateTable *st, unsigned long n) {
  st->num_dummy[env_my_thread_id()] += n;
}

static inline void statetable_dummy_sub(StateTable *st, unsigned long n) {
  st->num_dummy[env_my_thread_id()] -= n;
}

static inline void statetable_set_rehash_tbl(StateTable *st, StateTable *rehash_tbl) {
  st->rehash_tbl = rehash_tbl;
  statetable_set_rehasher(st);
}

static inline StateTable *statetable_rehash_tbl(StateTable *st) {
  return st->rehash_tbl;
}

static inline unsigned long statetable_space(StateTable *tbl) {
  return
    sizeof(struct StateTable)
    + (tbl->num ? tbl->thread_num * sizeof(unsigned long) : 0)
    + (tbl->num_dummy ? tbl->thread_num * sizeof(unsigned long) : 0)
    + (tbl->cap * sizeof(State *))
    + lmn_ewlock_space(tbl->lock);
}



/** -----------
 *  StateSpace
 */

StateSpace statespace_make(Automata a, Vector *psyms);
StateSpace statespace_make_for_parallel(int thread_num, Automata a, Vector *psyms);
void       statespace_free(StateSpace ss);
void       statespace_add_direct(StateSpace ss, State *s);
State     *statespace_insert(StateSpace ss, State *s);
State     *statespace_insert_delta(StateSpace ss, State *s, struct MemDeltaRoot *d);
void       statespace_foreach(StateSpace ss, void (*func) ( ),
                              LmnWord _arg1, LmnWord _arg2);
void       statespace_foreach_parallel(StateSpace ss, void (*func) ( ),
                              LmnWord _arg1, LmnWord _arg2, int nPE);
void       statespace_format_states(StateSpace ss);
void       statespace_clear(StateSpace ss);
void       statespace_ends_dumper(StateSpace ss);
void       statespace_dumper(StateSpace ss);

static inline unsigned long statespace_num_raw(StateSpace ss);
static inline unsigned long statespace_num(StateSpace ss);
static inline unsigned long statespace_dummy_num(StateSpace ss);
static inline unsigned long statespace_end_num(StateSpace ss);
static inline State        *statespace_init_state(StateSpace ss);
static inline void          statespace_set_init_state(StateSpace ss,
                                                      State *init_state,
                                                      BOOL enable_compact);
static inline void          statespace_add_end_state(StateSpace ss, State *s);
static inline const Vector *statespace_end_states(StateSpace ss);
static inline StateTable   *statespace_tbl(StateSpace ss);
static inline StateTable   *statespace_memid_tbl(StateSpace ss);
static inline StateTable   *statespace_accept_tbl(StateSpace ss);
static inline StateTable   *statespace_accept_memid_tbl(StateSpace ss);
static inline unsigned long statespace_space(StateSpace ss);

/* 初期状態を追加する MT-UNSAFE */
static inline void statespace_set_init_state(StateSpace ss, State* init_state,
                                             BOOL enable_binstr)
{
  ss->init_state = init_state;
  statespace_add_direct(ss, init_state);
  if (enable_binstr) {
    state_free_mem(init_state);
  }
}

/* 初期状態を返す */
static inline State *statespace_init_state(StateSpace ss) {
  return ss->init_state;
}

/* 状態数を返す */
static inline unsigned long statespace_num(StateSpace ss) {
  return (statespace_num_raw(ss) - statespace_dummy_num(ss));
}

/* dummyの状態数を含む, 管理している状態数を返す */
static inline unsigned long statespace_num_raw(StateSpace ss) {
  return statetable_num(statespace_tbl(ss))
       + statetable_num(statespace_memid_tbl(ss))
       + statetable_num(statespace_accept_tbl(ss))
       + statetable_num(statespace_accept_memid_tbl(ss));
}

/* memidテーブルに追加されているdummy状態数を返す */
static inline unsigned long statespace_dummy_num(StateSpace ss) {
  StateTable *tbl;
  unsigned long ret;
  unsigned int i;

  ret = 0UL;
  tbl = statespace_memid_tbl(ss);
  if (tbl) {
    for (i = 0; i < ss->thread_num; i++) {
      ret += tbl->num_dummy[i];
    }
  }

  tbl = statespace_accept_memid_tbl(ss);
  if (tbl) {
    for (i = 0; i < ss->thread_num; i++) {
      ret += tbl->num_dummy[i];
    }
  }
  return ret;
}


/* 最終状態数を返す */
static inline unsigned long statespace_end_num(StateSpace ss) {
  if (ss->thread_num > 1) {
    unsigned long sum = 0;
    unsigned int i;
    for (i = 0; i < ss->thread_num; i++) {
      sum += vec_num(&ss->end_states[i]);
    }
    return sum;

  } else {
    return vec_num(ss->end_states);
  }
}


/* 状態空間に**すでに含まれている**状態sを最終状態として登録する */
static inline void statespace_add_end_state(StateSpace ss, State *s) {
  LMN_ASSERT(env_my_thread_id() < env_threads_num());
  if (ss->thread_num > 1)
    vec_push(&ss->end_states[env_my_thread_id()], (vec_data_t)s);
  else
    vec_push(ss->end_states, (vec_data_t)s);
}


/* 最終状態のベクタを返す */
static inline const Vector *statespace_end_states(StateSpace ss)
{
  return ss->end_states;
}

static inline StateTable *statespace_tbl(StateSpace ss) {
  return ss->tbl;
}

static inline StateTable *statespace_memid_tbl(StateSpace ss) {
  return ss->memid_tbl;
}

static inline StateTable *statespace_accept_tbl(StateSpace ss) {
  return ss->acc_tbl;
}

static inline StateTable *statespace_accept_memid_tbl(StateSpace ss) {
  return ss->acc_memid_tbl;
}

static inline unsigned long statespace_space(StateSpace ss) {
  unsigned long ret = sizeof(struct StateSpace);
  if (statespace_tbl(ss)) {
    ret += statetable_space(statespace_tbl(ss));
  }
  if (statespace_memid_tbl(ss)) {
    ret += statetable_space(statespace_memid_tbl(ss));
  }
  if (statespace_accept_tbl(ss)) {
    ret += statetable_space(statespace_accept_tbl(ss));
  }
  if (statespace_accept_memid_tbl(ss)) {
    ret += statetable_space(statespace_accept_memid_tbl(ss));
  }
  if (ss->thread_num > 1) {
    unsigned int i;
    for (i = 0; i < ss->thread_num; i++)  ret += vec_space(&ss->end_states[i]);
  } else {
    ret += vec_space(ss->end_states);
  }
  return ret;
}

#endif
