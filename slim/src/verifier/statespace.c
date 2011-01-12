/*
 * statespace.c
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

#include "statespace.h"
#include "state.h"
#include "membrane.h"
#include "mem_encode.h"
#include "automata.h"
#include "rule.h"
#include "error.h"
#include "dumper.h"
#include "runtime_status.h"
#include "mc.h"
#include "lmntal_thread.h"
#include "delta_membrane.h"
#include "vector.h"
#include "queue.h"


/** ProtoTypes
 */
#ifdef PROFILE
inline BOOL state_space_is_memid_hash(StateSpace states, unsigned long hash);
void state_space_add_memid_hash(StateSpace states, unsigned long hash);
#endif
static StateTable *state_table_make(void);
static StateTable *state_table_make_with_size(unsigned long size);
static void state_table_free(StateTable *st);
inline static unsigned long state_table_num(StateTable *st);
inline static unsigned long state_table_num_by_me(StateTable *st);
inline static unsigned long state_table_cap_density(StateTable *st);
inline static unsigned long state_table_cap(StateTable *st);
inline static void state_table_num_add(StateTable *st, unsigned long n);
inline static void state_table_num_sub(StateTable *st, unsigned long n);
inline static void state_table_dummy_add(StateTable *st, unsigned long n);
inline static void state_table_dummy_sub(StateTable *st, unsigned long n);
static void state_table_add_direct(StateTable *st, State *s);
#ifdef PROFILE
static State *state_table_insert(StateTable *st, State *s, unsigned long *hash_col);
#else
static State *state_table_insert(StateTable *st, State *s);
#endif
inline static void state_table_set_rehasher(StateTable *st);
inline static BOOL state_table_use_rehasher(StateTable *st);
inline static unsigned long state_table_space(StateTable *st);
inline static void state_table_set_lock(StateTable *st, EWLock *lock);
static unsigned long table_new_size(unsigned long old_size);
static void state_table_resize(StateTable *st, unsigned long old_size);
static void state_table_set_rehash_tbl(StateTable *st, StateTable *rehash_tbl);
inline static StateTable *state_table_rehash_tbl(StateTable *st);
static void state_table_memid_rehash(State *pred, StateTable *ss);

/** Macros
 */
#define TABLE_DEFAULT_INIT_SIZE     (8192U)  /* TODO: テーブルの初期サイズはいくつが適当か. (固定サイズにしているモデル検査器は多い) */
#define TABLE_DEFAULT_MAX_DENSITY      (5U)  /* 1バケットあたりの平均長がこの値を越えた場合にresizeする */
#define MEM_EQ_FAIL_THRESHOLD          (2U)  /* 膜の同型性判定にこの回数以上失敗すると膜のエンコードを行う */

#define need_resize(EntryNum, Capacity)  (((EntryNum) / (Capacity)) > TABLE_DEFAULT_MAX_DENSITY)
#define STATE_EQUAL(Tbl, Check, Stored)  (state_hash(Check) == state_hash(Stored) \
                                          && ((Tbl)->type->compare)(Check, Stored))
#define STATE_COMPRESS(Tbl, S, BS)       \
  if (!is_encoded(S) && !(BS)) {         \
    (BS) = (((Tbl)->type->compress)(S)); \
  }
#define STATE_SET_COMPRESS(S, BS)        \
  if (!is_encoded(S)) {                  \
    state_set_compress_mem(S, BS);       \
  }


/** Global Vars
 */
struct state_space_type type_state_compress_z = {
  state_cmp_with_compress,
  state_calc_mem_dump_with_z
};


struct state_space_type type_state_compress = {
  state_cmp_with_compress,
  state_calc_mem_dump
};


struct state_space_type type_state_default = {
  state_cmp,
  state_calc_mem_dummy
};


/* Table of prime numbers 2^n+a, 2<=n<=30. */
static unsigned long primes[] = {
  8192 + 27,
  131072 + 29,
  1048576 + 7,
  4194304 + 15,
  16777216 + 43,
  67108864 + 15,
  268435456 + 3,
  536870912 + 11,
  1073741824 + 85,
  0
};


static unsigned long table_new_size(unsigned long old_size)
{
  unsigned long i, n;

  n = (unsigned long) (sizeof(primes) / sizeof(primes[0]));
  for (i = 0; i < n; i++) {
    if (primes[i] > old_size) {
      return primes[i];
    }
  }
  lmn_fatal("Size of StateSpace exceeded the limiting value");
  return 0; /* exception */
}


/* テーブルのサイズを1段階拡張する */
static void state_table_resize(StateTable *st, unsigned long old_cap)
{
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_countup(PROFILE_COUNT__HASH_RESIZE_TRIAL);
  }
#endif
  ENTER__CRITICAL_SECTION(resize, st->lock, ewlock_reject_enter, lmn_thread_id, old_cap, st->cap);
  {
    unsigned long i, new_cap, bucket;
    State *ptr, *next, **new_tbl;

    new_cap = table_new_size(old_cap);
    new_tbl = LMN_NALLOC(State *, new_cap);

    for (i = 0; i < new_cap; i++) {
      new_tbl[i] = NULL;
    }

    for (i = 0; i < old_cap; i++) {
      ptr = st->tbl[i];
      while (ptr) {
        next = ptr->next;
        bucket = state_hash(ptr) % new_cap;
        ptr->next = new_tbl[bucket];
        if (is_dummy(ptr) && is_expanded(ptr) && !is_encoded(ptr)) {
          /* オリジナルテーブルでdummy_stateが存在する状態にはバイト列は不要
           * (resize中に, 展開済み状態へのデータアクセスは発生しない) */
          state_free_compress_mem(ptr);
        }
        new_tbl[bucket] = ptr;
        ptr = next;
      }
    }

    LMN_FREE(st->tbl);
    st->tbl = new_tbl;
    st->cap = new_cap;
    st->cap_density = new_cap / lmn_env.core_num;

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_countup(PROFILE_COUNT__HASH_RESIZE_APPLY);
    }
#endif
  }
  EXIT___CRITICAL_SECTION(resize, st->lock, ewlock_permit_enter, lmn_thread_id);
}



/** StateSpace
 */
inline static StateSpace state_space_make_minimal()
{
  struct StateSpace *ss = LMN_MALLOC(struct StateSpace);
  ss->tbl_type      = 0x00U;
  ss->out           = NULL;
  ss->init_state    = NULL;
  ss->end_states    = NULL;
  ss->tbl           = NULL;
  ss->memid_tbl     = NULL;
  ss->acc_tbl       = NULL;
  ss->acc_memid_tbl = NULL;
  return ss;
}


inline static void state_space_make_table(StateSpace ss)
{
  if (lmn_env.mem_enc) {
    state_space_set_memenc(ss);
    ss->memid_tbl = state_table_make();
    if (state_space_is_concurrent(ss)) {
      state_table_set_lock(ss->memid_tbl, ewlock_make());
    }

    if (mc_data.has_property) {
      state_space_set_property(ss);
      ss->acc_memid_tbl = state_table_make();
      if (state_space_is_concurrent(ss)) {
        state_table_set_lock(state_space_accept_memid_tbl(ss), ewlock_make());
      }
    }
  }
  else {
    ss->tbl = state_table_make();

    if (mc_data.has_property) {
      state_space_set_property(ss);
      ss->acc_tbl = state_table_make();
    }

    if (lmn_env.optimize_hash) {
      state_space_set_rehasher(ss);
      ss->memid_tbl = state_table_make();
      state_table_set_rehash_tbl(state_space_tbl(ss),
                                 state_space_memid_tbl(ss));
      state_table_use_rehasher(state_space_tbl(ss));

      if (state_space_accept_tbl(ss)) {
        ss->acc_memid_tbl = state_table_make();
        state_table_set_rehash_tbl(state_space_accept_tbl(ss),
                                   state_space_accept_memid_tbl(ss));
        state_table_use_rehasher(state_space_accept_tbl(ss));
      }
    }

    if (state_space_is_concurrent(ss)) {
      state_table_set_lock(state_space_tbl(ss), ewlock_make());

      if (state_space_accept_tbl(ss)) {
        state_table_set_lock(state_space_accept_tbl(ss), ewlock_make());
      }
      if (state_space_memid_tbl(ss)) {
        state_table_set_lock(state_space_memid_tbl(ss), ewlock_make());
      }
      if (state_space_accept_memid_tbl(ss)) {
        state_table_set_lock(state_space_accept_memid_tbl(ss), ewlock_make());
      }
    }
  }
#ifdef PROFILE
  if (lmn_env.optimize_hash_old) {
    if (!state_space_memid_tbl(ss)) {
      ss->memid_tbl = state_table_make();
    }
    if (state_space_accept_tbl(ss)) {
      ss->acc_memid_tbl = state_table_make();
    }
    hashset_init(&ss->memid_hashes, 128);
  }
#endif
}


StateSpace state_space_make()
{
  struct StateSpace *ss;
//  unsigned int i, n;
//  n  = lmn_env.core_num;
  ss = state_space_make_minimal();
  state_space_make_table(ss);
//  ss->out        = LMN_NALLOC(struct LmnPort, n);
  ss->out = NULL;
  ss->end_states = vec_make(64);

  return ss;
}


/* for parallel model checker mode */
StateSpace state_space_make_for_parallel()
{
  unsigned int i;
  struct StateSpace *ss;

  ss = state_space_make_minimal();
  state_space_set_concurrent(ss);
  state_space_make_table(ss);

  ss->end_states = LMN_NALLOC(struct Vector, lmn_env.core_num);
  for (i = 0; i < lmn_env.core_num; i++) {
    vec_init(&ss->end_states[i], 48);
  }
  return ss;
}


void state_space_free(StateSpace ss)
{
  state_table_free(state_space_tbl(ss));
  state_table_free(state_space_memid_tbl(ss));
  state_table_free(state_space_accept_tbl(ss));
  state_table_free(state_space_accept_memid_tbl(ss));

  if (state_space_is_concurrent(ss)) {
    unsigned int i;
    for (i = 0; i < lmn_env.core_num; i++) {
      vec_destroy(&ss->end_states[i]);
    }
    LMN_FREE(ss->end_states);
  }
  else {
    vec_free(ss->end_states);
  }

#ifdef PROFILE
  if (lmn_env.optimize_hash_old) {
    hashset_destroy(&ss->memid_hashes);
  }
#endif
  LMN_FREE(ss);
}


/* 初期状態を追加する MT-UNSAFE */
void state_space_set_init_state(StateSpace ss, State* init_state, BOOL enable_compact)
{
  ss->init_state = init_state;
  state_space_add_direct(ss, init_state);
  if (enable_compact) {
    state_free_mem(init_state);
  }
}


/* 初期状態を返す */
State *state_space_init_state(StateSpace ss)
{
  return ss->init_state;
}


/* 状態数を返す */
unsigned long state_space_num(StateSpace ss)
{
  return (state_space_num_raw(ss) - state_space_dummy_num(ss));
}


/* dummyの状態数を含む, 管理している状態数を返す */
unsigned long state_space_num_raw(StateSpace ss)
{
  return state_table_num(state_space_tbl(ss))
       + state_table_num(state_space_memid_tbl(ss))
       + state_table_num(state_space_accept_tbl(ss))
       + state_table_num(state_space_accept_memid_tbl(ss));
}


/* memidテーブルに追加されているdummy状態数を返す */
unsigned long state_space_dummy_num(StateSpace ss)
{
  StateTable *tbl;
  unsigned long ret;
  unsigned int i;

  ret = 0UL;
  tbl = state_space_memid_tbl(ss);
  if (tbl) {
    for (i = 0; i < lmn_env.core_num; i++) {
      ret += tbl->num_dummy[i];
    }
  }

  tbl = state_space_accept_memid_tbl(ss);
  if (tbl) {
    for (i = 0; i < lmn_env.core_num; i++) {
      ret += tbl->num_dummy[i];
    }
  }
  return ret;
}


/* 最終状態数を返す */
unsigned long state_space_end_num(StateSpace ss)
{
  if (state_space_is_concurrent(ss)) {
    unsigned long sum = 0;
    unsigned int i;
    for (i = 0; i < lmn_env.core_num; i++) {
      sum += vec_num(&ss->end_states[i]);
    }
    return sum;

  } else {
    return vec_num(ss->end_states);
  }
}


/* 状態空間に**すでに含まれている**状態sを最終状態として登録する */
void state_space_add_end_state(StateSpace ss, State *s)
{
  LMN_ASSERT(lmn_thread_id < lmn_env.core_num);
  state_space_is_concurrent(ss)
                    ? vec_push(&ss->end_states[lmn_thread_id], (vec_data_t)s)
                    : vec_push(ss->end_states, (vec_data_t)s);
}


/* 最終状態のベクタを返す */
const Vector *state_space_end_states(StateSpace ss)
{
  return ss->end_states;
}


inline StateTable *state_space_tbl(StateSpace ss)
{
  return ss->tbl;
}


inline StateTable *state_space_memid_tbl(StateSpace ss)
{
  return ss->memid_tbl;
}


inline StateTable *state_space_accept_tbl(StateSpace ss)
{
  return ss->acc_tbl;
}


inline StateTable *state_space_accept_memid_tbl(StateSpace ss)
{
  return ss->acc_memid_tbl;
}


/* 状態sをエンコードした状態を新たに生成し, 生成した状態をテーブルstのrehash先テーブルへ追加する. */
static void state_table_memid_rehash(State *s, StateTable *st)
{
  StateTable *rehash_tbl;
  State *new;
  LmnMembrane *m;

  new = state_make_minimal();
  m = lmn_binstr_decode(state_mem_binstr(s)); /* ほんとは既にdecodeされているので, 呼び出し側から受け取りたい */
  new->compress_mem    =  lmn_mem_encode(m);
  new->state_name      =  s->state_name;
  new->hash            =  binstr_hash(state_mem_binstr(new));

  set_encoded(new);
  set_expanded(new);
  set_dummy(new);

  rehash_tbl = state_table_rehash_tbl(st);
  state_table_add_direct(rehash_tbl, new); /* dummy stateのparentをoriginalとして扱う */
  state_set_parent(new, s);
  state_table_dummy_add(rehash_tbl, 1);

  lmn_mem_drop(m);
  lmn_mem_free(m);
}


#ifdef PROFILE

/* 膜のIDを計算するハッシュ値(mhash)を追加する */
void state_space_add_memid_hash(StateSpace states, unsigned long hash)
{
  State *ptr, *prev;
  StateTable *org;
  unsigned long i;

  org = states->tbl;
  hashset_add(&states->memid_hashes, hash);

  for (i = 0; i < state_table_cap(org); i++) {
    ptr = org->tbl[i];
    prev = NULL;

    while (ptr) {
      State *next = ptr->next;
      if (state_hash(ptr) == hash) { /* state_space_mem_encode_f */
        state_table_memid_rehash(ptr, org);
      }
      ptr = next;
    }
  }
}


/* hashが膜のIDを計算しているハッシュならば真、そうでなければ偽を返す */
inline BOOL state_space_is_memid_hash(StateSpace states, unsigned long hash)
{
  return hashset_contains(&states->memid_hashes, hash);
}
#endif


/* 状態sが状態空間ssに既出ならばその状態を, 新規ならばs自身を返す */
inline State *state_space_insert(StateSpace ss, State *s)
{
  StateTable *insert_dst;
  State *ret;
  BOOL is_accept;
#ifdef PROFILE
  unsigned long col;
  unsigned long hashv;
  col = 0;
  hashv = state_hash(s);
#endif

  is_accept = state_space_has_property(ss) && state_is_accept(s);

  if (is_encoded(s)) { /* already calculated canonical binary strings */
    insert_dst = is_accept ? state_space_accept_memid_tbl(ss)
                           : state_space_memid_tbl(ss);
  } else {             /* default */
    insert_dst = is_accept ? state_space_accept_tbl(ss)
                           : state_space_tbl(ss);

#ifdef PROFILE
    if (lmn_env.optimize_hash_old && state_space_is_memid_hash(ss, hashv)) {
      state_calc_mem_encode(s);
      insert_dst = is_accept ? state_space_accept_memid_tbl(ss)
                             : state_space_memid_tbl(ss);
    }
#endif
  }

#ifndef PROFILE
  ret = state_table_insert(insert_dst, s);
#else
  ret = state_table_insert(insert_dst, s, &col);
  if (lmn_env.optimize_hash_old && col >= MEM_EQ_FAIL_THRESHOLD) {
    state_space_add_memid_hash(ss, hashv);
  }
#endif

  if (is_encoded(ret)) {
    /* rehasherが機能した場合, 通常のテーブルを入り口に, memidテーブルにエントリが追加されている
     * なにも考慮せず, テーブル拡張の判定を行うとマズいので, ここでテーブルを切り替える */
    insert_dst = is_accept ? state_space_accept_memid_tbl(ss)
                           : state_space_memid_tbl(ss);
  }

  if (need_resize(state_table_num_by_me(insert_dst),
                  state_table_cap_density(insert_dst))) {  /* tableのresize処理 */
    state_table_resize(insert_dst, insert_dst->cap);
  }

  return ret;
}


State *state_space_insert_delta(StateSpace ss, State *s, struct MemDeltaRoot *d)
{
  State *ret;
  /* 展開元のグラフ構造memをdが指しており, d->memをTSDとして扱っている前提が守られていればMT-Safe */

  /** restore */
  dmem_root_commit(d); /* dが指す展開元のグラフが, 自身(successor)用のグラフ構造へ */
  state_calc_hash(s, DMEM_ROOT_MEM(d), state_space_use_memenc(ss)); /* それを元にハッシュ値やmem_idを計算 */
  state_set_mem(s, DMEM_ROOT_MEM(d));

  /** do */
  ret = state_space_insert(ss, s);

  /** finish */
  state_set_mem(s, NULL);
  dmem_root_revert(d); /* 元に戻す */
  return ret;
}


/* 重複検査や排他制御なしに状態sを状態表ssに登録する */
void state_space_add_direct(StateSpace ss, State *s)
{
  StateTable *add_dst;

  add_dst = is_encoded(s) ? ss->memid_tbl : ss->tbl;
  state_table_add_direct(add_dst, s);

  if (need_resize(state_table_num_by_me(add_dst), state_table_cap_density(add_dst))) {
    state_table_resize(add_dst, add_dst->cap);
  }
}


unsigned long state_space_space(StateSpace ss)
{
  unsigned long ret;
  ret  = sizeof(struct StateSpace);
  ret += ss->tbl       ? state_table_space(ss->tbl)       : 0;
  ret += ss->memid_tbl ? state_table_space(ss->memid_tbl) : 0;
  if (state_space_is_concurrent(ss)) {
    unsigned int i;
    for (i = 0; i < lmn_env.core_num; i++) {
      ret += vec_space(&ss->end_states[i]);
    }
  } else {
    ret += vec_space(ss->end_states);
  }
  return ret;
}


/* 高階関数 */
void state_space_foreach(StateSpace ss, void (*func) ( ), LmnWord _arg)
{
  state_table_foreach(state_space_tbl(ss),              func, _arg);
  state_table_foreach(state_space_memid_tbl(ss),        func, _arg);
  state_table_foreach(state_space_accept_tbl(ss),       func, _arg);
  state_table_foreach(state_space_accept_memid_tbl(ss), func, _arg);
}



/*----------------------------------------------------------------------
 * StateTable
 */

static StateTable *state_table_make()
{
  return state_table_make_with_size(TABLE_DEFAULT_INIT_SIZE);
}


static StateTable *state_table_make_with_size(unsigned long size)
{
  unsigned long i;
  StateTable *st = LMN_MALLOC(StateTable);
  if (lmn_env.enable_compress_mem) {
    if (lmn_env.z_compress) {
      st->type = &type_state_compress_z;
    } else {
      st->type = &type_state_compress;
    }
  } else {
    st->type = &type_state_default;
  }
  st->use_rehasher = FALSE;
  size = table_new_size(size);
  st->tbl = LMN_NALLOC(State *, size);
  st->cap = size;
  st->cap_density = size / lmn_env.core_num;
  st->num  = LMN_NALLOC(unsigned long, lmn_env.core_num);
  st->num_dummy = LMN_NALLOC(unsigned long, lmn_env.core_num);
  st->lock = NULL;
  st->rehash_tbl = NULL;

  for (i = 0; i < size; i++) {
    st->tbl[i] = NULL;
  }

  for (i = 0; i < lmn_env.core_num; i++) {
    st->num[i] = 0UL;
    st->num_dummy[i] = 0UL;
  }

  return st;
}


inline static void state_table_set_lock(StateTable *st, EWLock *lock)
{
  st->lock = lock;
}


static void state_table_free(StateTable *st)
{
  if (st) {
    state_table_foreach(st, state_free, DEFAULT_ARGS);

    if (st->lock) {
      ewlock_free(st->lock);
    }
    LMN_FREE(st->num_dummy);
    LMN_FREE(st->num);
    LMN_FREE(st->tbl);
    LMN_FREE(st);
  }
}


inline static void state_table_set_rehasher(StateTable *st)
{
  st->use_rehasher = TRUE;
}


inline static BOOL state_table_use_rehasher(StateTable *st)
{
  return st->use_rehasher;
}


inline static unsigned long state_table_num_by_me(StateTable *st)
{
  return st->num[lmn_thread_id];
}


inline static unsigned long state_table_num(StateTable *st)
{
  unsigned long ret = 0;

  if (st) {
    unsigned int i;
    for (i = 0; i < lmn_env.core_num; i++) {
      ret += st->num[i];
    }
  }

  return ret;
}


inline static unsigned long state_table_cap(StateTable *st)
{
  return st->cap;
}


inline static unsigned long state_table_cap_density(StateTable *st)
{
  return st->cap_density;
}


inline static void state_table_num_add(StateTable *st, unsigned long n)
{
  st->num[lmn_thread_id] += n;
}


inline static void state_table_num_sub(StateTable *st, unsigned long n)
{
  st->num[lmn_thread_id] -= n;
}


inline static void state_table_dummy_add(StateTable *st, unsigned long n)
{
  st->num_dummy[lmn_thread_id] += n;
}


inline static void state_table_dummy_sub(StateTable *st, unsigned long n)
{
  st->num_dummy[lmn_thread_id] -= n;
}


static void state_table_set_rehash_tbl(StateTable *st, StateTable *rehash_tbl)
{
  st->rehash_tbl = rehash_tbl;
}


inline static StateTable *state_table_rehash_tbl(StateTable *st)
{
  return st->rehash_tbl;
}


/* 状態sが状態空間stに既出ならばその状態を, 新規ならばs自身を返す */
#ifdef PROFILE
static State *state_table_insert(StateTable *st, State *ins, unsigned long *col)
#else
static State *state_table_insert(StateTable *st, State *ins)
#endif
{
  State *ret = NULL;

  START__CRITICAL_SECTION(st->lock, ewlock_acquire_enter, lmn_thread_id);
  {
    LmnBinStr compress;
    State *str;
    unsigned long bucket, hash;

    compress  =  NULL;
    hash      =  state_hash(ins);
    bucket    =  hash % state_table_cap(st);
    str       =  st->tbl[bucket];

    /* case: empty bucket */
    if (!str) {
      STATE_COMPRESS(st, ins, compress);
      ENTER__CRITICAL_SECTION(st_ins_empty, st->lock, ewlock_acquire_write, bucket, NULL, st->tbl[bucket]);
      {
        STATE_SET_COMPRESS(ins, compress);
        state_table_num_add(st, 1);
        st->tbl[bucket] = ins;
        ret = ins;
      }
      EXIT___CRITICAL_SECTION(st_ins_empty, st->lock, ewlock_release_write, bucket);

      if (!ret) { /* preceded */
        str = st->tbl[bucket];
#ifdef PROFILE
        if (lmn_env.profile_level >= 3) profile_countup(PROFILE_COUNT__HASH_FAIL_TO_INSERT);
#endif
      }
    }

    /* case: non-empty bucket */
    while (!ret) {
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) profile_countup(PROFILE_COUNT__HASH_CONFLICT_ENTRY);
#endif
      if (hash == state_hash(str)) {
        /* >>>>>>> ハッシュ値が等しい状態に対する処理ここから <<<<<<<<
         *   膜のハッシュ関数(mhash)から膜のIDのハッシュ関数(memid_hash)へのrehash:
         *   膜のハッシュ関数によるテーブル   ->オリジナルテーブル
         *   膜IDのハッシュ関数によるテーブル ->memidテーブル
         *   既にオリジナルテーブルに登録済みの状態をrehashすることができるのはその状態のサクセサをinsertしている場合のみ.
         *   テーブル単位で各状態をユニークにする(すなわち状態展開時に選択する状態の重複は避ける)
         *   テーブル間で等価な状態が存在することになるため,
         *   memidテーブル側の状態のparentにオリジナルテーブルの状態を指させる.
         */

        /** A. オリジナルテーブルで自身とハッシュ値の同じ状態がdummyであることを発見したならば, memidテーブル側を探索する.
         *     (このとき, オリジナルテーブルのdummy stateのバイト列が破棄されているかもしれない) */
        if (state_table_use_rehasher(st) && is_dummy(str) && !is_encoded(str)) {
          /* TODO: opt-hashフラグはstate_tableに登録させる */
          state_calc_mem_encode(ins);
#ifndef PROFILE
          ret = state_table_insert(state_table_rehash_tbl(st), ins);
#else
          ret = state_table_insert(state_table_rehash_tbl(st), ins, col);
#endif
          break;
        }
        /** B. memidテーブルへのlookupの場合か, オリジナルテーブルへのlookupでdummyではない状態と等価な場合 */
        else if (!STATE_EQUAL(st, ins, str)) {
          ret = (is_dummy(str) && is_encoded(str)) ? state_get_parent(ins)
                                                   : str;
          LMN_ASSERT(ret);
          break;
        }
        /** C. memidテーブルへのlookupでハッシュ値が衝突 */
        else if (is_encoded(str)){
#ifdef PROFILE
          if (lmn_env.profile_level >= 3) profile_countup(PROFILE_COUNT__HASH_CONFLICT_HASHV);
#endif
        }
        /** D. オリジナルテーブルへのlookupでdummyじゃない状態とハッシュ値が衝突
         *     (ただし, インタリーブでdummyフラグを見逃した可能性あり) */
        else {
          LMN_ASSERT(!is_encoded(str));
#ifdef PROFILE
          (*col)++;
#endif
          if (state_table_use_rehasher(st)) {
            if (state_get_parent(ins) == str) {
              /* 遷移元状態と1step先の状態でハッシュ値が衝突:
               * dummyフラグは競合で見逃される可能性があるため遷移元状態のバイト列を破棄せず,
               * encodeした状態をmemidテーブルへ突っ込む */
              state_table_memid_rehash(str, st);
              set_dummy(str);
              /* CAUTION: dummyフラグは見逃される可能性があるため,
               *          直ちに古いバイト列を破棄することはできない */
            }

            /* 比較元をencode */
            state_calc_mem_encode(ins);
#ifndef PROFILE
            ret = state_table_insert(state_table_rehash_tbl(st), ins);
#else
            ret = state_table_insert(state_table_rehash_tbl(st), ins, col);
#endif

            LMN_ASSERT(ret);
            break;
          }
        }
        /* >>>>>>> ハッシュ値が等しい状態に対する処理ここまで <<<<<<<< */
      }

      /** エントリリストへの状態追加:
       * 逐次
       *   1. 各エントリである状態に対して, ハッシュ値と状態の比較を行う.
       *   2. 等価な状態を発見した場合は, その状態へのアドレスを返す.
       *   3. リスト末尾へ到達した場合は, バイト列が未計算ならば計算する.
       *   4. 計算したバイト列を状態に設定した後にリスト末尾に状態を追加し, 追加した状態のアドレスを返す.
       * 並列
       *   4. ロックを確保し, 確保後に末尾エントリのnextアドレスがNULLであるか否かを検査
       *   6. NULLでない(他のスレッドが追加した)ならば, ロックを開放した後に処理1(リスト走査)を再開する.
       *   7. NULLならば, 計算したバイト列を状態に設定した後にリスト末尾に状態を追加し, 追加した状態のアドレスを返す.
       */
      if (!str->next) { /* リスト末尾の場合 */
        STATE_COMPRESS(st, ins, compress);
        ENTER__CRITICAL_SECTION(st_ins_list, st->lock, ewlock_acquire_write, bucket, NULL, str->next);
        {
          state_table_num_add(st, 1);
          STATE_SET_COMPRESS(ins, compress);
          str->next = ins;
          ret = ins;
        }
        EXIT___CRITICAL_SECTION(st_ins_list, st->lock, ewlock_release_write, bucket);

#ifdef PROFILE
        if (!ret && lmn_env.profile_level >= 3) profile_countup(PROFILE_COUNT__HASH_FAIL_TO_INSERT);
#endif
      }
      str = str->next; /* リストを辿る */
    }
  }
  FINISH_CRITICAL_SECTION(st->lock, ewlock_release_enter, lmn_thread_id);
  return ret;
}


/* 重複検査なしに状態sを状態表stに登録する */
static void state_table_add_direct(StateTable *st, State *s)
{
  START__CRITICAL_SECTION(st->lock, ewlock_acquire_enter, lmn_thread_id);
  {
    LmnBinStr compress;
    State *ptr;
    unsigned long bucket;
    BOOL inserted;

    compress = NULL;
    bucket   = state_hash(s) % state_table_cap(st);
    ptr      = st->tbl[bucket];
    inserted = FALSE;

    STATE_COMPRESS(st, s, compress);

    if (!ptr) { /* empty */
      ENTER__CRITICAL_SECTION(st_add_empty, st->lock, ewlock_acquire_write, bucket, NULL, st->tbl[bucket]);
      {
        STATE_SET_COMPRESS(s, compress);
        state_table_num_add(st, 1);
        st->tbl[bucket] = s;
        inserted = TRUE;
      }
      EXIT___CRITICAL_SECTION(st_add_empty, st->lock, ewlock_release_write, bucket);
      if (!inserted) ptr = st->tbl[bucket];
    }

    while (!inserted) {
      if (!ptr->next) { /* リスト末尾の場合 */
        STATE_COMPRESS(st, s, compress);
        ENTER__CRITICAL_SECTION(st_add_list, st->lock, ewlock_acquire_write, bucket, NULL, ptr->next);
        {
          inserted = TRUE;
          state_table_num_add(st, 1);
          STATE_SET_COMPRESS(s, compress);
          ptr->next = s;
        }
        EXIT___CRITICAL_SECTION(st_add_list, st->lock, ewlock_release_write, bucket);
      }
      ptr = ptr->next;
    }
  }
  FINISH_CRITICAL_SECTION(st->lock, ewlock_release_enter, lmn_thread_id);
}


static unsigned long state_table_space(StateTable *tbl)
{
  /* TODO: ロックのメモリ量 */
  return
    sizeof(struct StateTable)
    + (tbl->num ? lmn_env.core_num * sizeof(unsigned long) : 0)
    + (tbl->num_dummy ? lmn_env.core_num * sizeof(unsigned long) : 0)
    + (tbl->cap * sizeof(State *))
    + lmn_ewlock_space(tbl->lock);
}


/* 高階関数  */
void state_table_foreach(StateTable *st, void (*func) ( ), LmnWord _arg)
{
  if (st) {
    unsigned long i, size;
    State *ptr, *next;

    size = st->cap;
    for (i = 0; i < size; i++) {
      ptr = st->tbl[i];
      while (ptr) {
        next = ptr->next;
        if (_arg) {
          func(ptr, _arg);
        } else {
          func(ptr);
        }
        ptr = next;
      }
    }
  }
}



/** Printer et al
 */
void state_space_dump_all_statetranses_dot(StateSpace ss, FILE *f);
void state_space_dump_all_transitions(StateSpace ss, FILE *fp);
void state_space_dump_all_transitions_fsm(StateSpace ss, FILE *fp);
void state_space_dump_all_labels(StateSpace ss, FILE *fp);
void state_space_dump_all_states(StateSpace ss, FILE *fp);
void state_space_dump_all_states_fsm(StateSpace ss, FILE *fp);


void state_space_ends_dumper(StateSpace ss, FILE *f)
{
  const Vector *ends;
  unsigned int i;

  ends = state_space_end_states(ss);
  if (state_space_is_concurrent(ss)) {
    Vector *end_i;
    unsigned int j;
    for (i = 0; i < lmn_env.core_num; i++) {
      end_i = (Vector *)(&ends[i]);
      for (j = 0; j < vec_num(end_i); j++) {
        state_print_mem((State *)vec_get(end_i, j), (LmnWord)f);
        if (lmn_env.sp_dump_format == LMN_SYNTAX) {
          printf(".\n");
        }
      }
    }
  } else {
    for (i = 0; i < vec_num(ends); i++) {
      state_print_mem((State *)vec_get(ends, i), (LmnWord)f);
      if (lmn_env.sp_dump_format == LMN_SYNTAX) {
        printf(".\n");
      }
    }
  }
}


void state_space_dumper(StateSpace ss, FILE *f)
{
  State *init = state_space_init_state(ss);

  switch (lmn_env.mc_dump_format) {
  case Dir_DOT:
    fprintf(f, "digraph StateTransition {\n");
    fprintf(f, "  node [shape = circle];\n");
    fprintf(f, "  %lu [style=filled, color = \"#ADD8E6\", shape = Msquare];\n",
                  state_format_id(init));
    state_space_dump_all_states(ss, f);
    state_space_dump_all_transitions(ss, f);
    state_space_dump_all_labels(ss, f);
    fprintf(f, "}\n");
    break;
  case FSM:
    /* TODO: under construction..
     *   一般的な状態表現方法として, 変数集合/State Vectorで表現する手段への対応
     *   現状ではとりあえず状態データを空にして状態遷移グラフを出力する */
    // state_space_print_state_data
    // state_space_print_state_vector
    fprintf(f, "Under_Constructions(2) Binay \"Yes\" \"No\"\n");
    fprintf(f, "---\n");
    state_space_dump_all_states(ss, f);
    fprintf(f, "---\n");
    state_space_dump_all_transitions(ss, f);
    break;
  case LaViT: /* FALL THROUGH */
  default:
    if (lmn_env.sp_dump_format != INCREMENTAL) {
      fprintf(f, "States\n");
      state_space_dump_all_states(ss, f);
    }
    fprintf(f, "\n");
    fprintf(f, "Transitions\n");
    fprintf(f, "init:%lu\n", state_format_id(init));
    state_space_dump_all_transitions(ss, f);
    fprintf(f, "\n");

    if (mc_data.has_property && lmn_env.mc_dump_format == LaViT) {
      fprintf(f, "Labels\n");
      state_space_dump_all_labels(ss, f);
      fprintf(f, "\n");
    }

    break;
  }
}


void state_space_dump_all_states(StateSpace ss, FILE *f)
{
  state_table_foreach(state_space_tbl(ss),        dump_state_data, (LmnWord)f);
  state_table_foreach(state_space_memid_tbl(ss),  dump_state_data, (LmnWord)f);
  state_table_foreach(state_space_accept_tbl(ss), dump_state_data, (LmnWord)f);
  state_table_foreach(state_space_accept_memid_tbl(ss), dump_state_data, (LmnWord)f);
}


void state_space_dump_all_transitions(StateSpace ss, FILE *f)
{
  state_table_foreach(state_space_tbl(ss),        state_print_transition, (LmnWord)f);
  state_table_foreach(state_space_memid_tbl(ss),  state_print_transition, (LmnWord)f);
  state_table_foreach(state_space_accept_tbl(ss), state_print_transition, (LmnWord)f);
  state_table_foreach(state_space_accept_memid_tbl(ss), state_print_transition, (LmnWord)f);
}


void state_space_dump_all_labels(StateSpace ss, FILE *f)
{
  state_table_foreach(state_space_tbl(ss),        state_print_label, (LmnWord)f);
  state_table_foreach(state_space_memid_tbl(ss),  state_print_label, (LmnWord)f);
  state_table_foreach(state_space_accept_tbl(ss), state_print_label, (LmnWord)f);
  state_table_foreach(state_space_accept_memid_tbl(ss), state_print_label, (LmnWord)f);
}



/* 注: 出力のために, ハッシュ表であることを無視した配置へ,
 *     リンクリストの先頭の状態のIDで整列をかける.
 *     リンクリストを構成する状態の整列はしない.
 *     (この処理は, 新たに状態数分の配列をmallocした場合,
 *      巨大なmallocによりメモリswapが発生することへの対抗措置である)  */
void state_space_format_states(StateSpace ss)
{
#ifndef __CYGWIN__
  /* cygwinテスト時に, ボトルネックになっていた */
  if (!mc_data.is_format_states && lmn_env.sp_dump_format != INCREMENTAL) {
    state_table_format_states(state_space_tbl(ss));
    state_table_format_states(state_space_memid_tbl(ss));
    state_table_format_states(state_space_accept_tbl(ss));
    state_table_format_states(state_space_accept_memid_tbl(ss));
    mc_data.is_format_states = TRUE;
  }
#endif
}


inline static int state_table_cmp_state_id_gr_f(const void *a_, const void *b_)
{
  State *p1, *p2;

  p1 = *(State **)a_;
  p2 = *(State **)b_;

  if (p1 && p2) {
    return state_id(p1) - state_id(p2);
  } else if (p1 || p2) {
    return p1 ? state_id(p1) : (-1 * state_id(p2));
  } else {
    return 1;
  }
}


inline static void state_table_issue_state_id_f(State *s, LmnWord _d)
{
  static unsigned long id = 1;
  state_set_format_id(s, id++);
}


void state_table_format_states(StateTable *st)
{
  if (st) {
    qsort(st->tbl, st->cap, sizeof(struct State *), state_table_cmp_state_id_gr_f);
    state_table_foreach(st, state_table_issue_state_id_f, 0);
  }
}


inline static void state_table_vec_push_f(State *s, LmnWord _v)
{
  vec_push((Vector *)_v, (vec_data_t)s);
}


void state_table_to_state_vec(StateTable *st, Vector *v)
{
  state_table_foreach(st, state_table_vec_push_f, (LmnWord)v);
}


inline static void state_table_enqueue_f(State *s, LmnWord _q)
{
  enqueue((Queue *)_q, (LmnWord)s);
}


void state_table_to_state_queue(StateTable *st, Queue *q)
{
  state_table_foreach(st, state_table_enqueue_f, (LmnWord)q);
}
