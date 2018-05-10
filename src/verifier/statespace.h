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

/**
 * @ingroup  Verifier
 * @defgroup StateSpace
 * @{
 */

#include "../lmntal.h"
#include "element/element.h"
#include "automata.h"
#include "state.h"
#include "delta_membrane.h"
#include "mem_encode.h"
#include "tree_compress.h"

struct statespace_type {
  int(*compare) (State *, State *);               /* 状態の等価性判定を行う関数 */
  LmnBinStrRef(*compress) (State *); /* 状態sの圧縮バイト列を計算して返す関数 */
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

  AutomataRef       property_automata;  /* Never Clainへのポインタ */
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

void statetable_foreach(StateTable *st, void (*func) (ANYARGS ),
                               LmnWord _arg1, LmnWord _arg2);
void statetable_foreach_parallel(StateTable *st, void (*mt_safe_func) (ANYARGS ),
                                 LmnWord _arg1, LmnWord _arg2, int nthreads);
void statetable_format_states(StateTable *st);

void          statetable_set_lock(StateTable *st, EWLock *lock);
void          statetable_set_rehasher(StateTable *st);
BOOL          statetable_use_rehasher(StateTable *st);
unsigned long statetable_num_by_me(StateTable *st);
unsigned long statetable_num(StateTable *st);
unsigned long statetable_cap(StateTable *st);
unsigned long statetable_cap_density(StateTable *st);
void          statetable_num_add(StateTable *st, unsigned long n);
void          statetable_num_sub(StateTable *st, unsigned long n);
void          statetable_dummy_add(StateTable *st, unsigned long n);
void          statetable_dummy_sub(StateTable *st, unsigned long n);
void          statetable_set_rehash_tbl(StateTable *st,
                                                      StateTable *rehash_tbl);
StateTable   *statetable_rehash_tbl(StateTable *st);
unsigned long statetable_space(StateTable *tbl);


/** -----------
 *  StateSpace
 */

StateSpaceRef statespace_make(AutomataRef a, Vector *psyms);
StateSpaceRef statespace_make_for_parallel(int thread_num, AutomataRef a, Vector *psyms);
void       statespace_free(StateSpaceRef ss);
void       statespace_add_direct(StateSpaceRef ss, State *s);
State     *statespace_insert(StateSpaceRef ss, State *s);
State     *statespace_insert_delta(StateSpaceRef ss, State *s, struct MemDeltaRoot *d);
void       statespace_foreach(StateSpaceRef ss, void (*func) (ANYARGS ),
                              LmnWord _arg1, LmnWord _arg2);
void       statespace_foreach_parallel(StateSpaceRef ss, void (*func) (ANYARGS ),
                              LmnWord _arg1, LmnWord _arg2, int nPE);
void       statespace_format_states(StateSpaceRef ss);
void       statespace_clear(StateSpaceRef ss);
void       statespace_ends_dumper(StateSpaceRef ss);
void       statespace_dumper(StateSpaceRef ss);

unsigned long statespace_num_raw(StateSpaceRef ss);
unsigned long statespace_num(StateSpaceRef ss);
unsigned long statespace_dummy_num(StateSpaceRef ss);
unsigned long statespace_end_num(StateSpaceRef ss);
State        *statespace_init_state(StateSpaceRef ss);
void          statespace_set_init_state(StateSpaceRef ss,
                                                      State *init_state,
                                                      BOOL enable_compact);
void          statespace_add_end_state(StateSpaceRef ss, State *s);
const Vector *statespace_end_states(StateSpaceRef ss);
StateTable   *statespace_tbl(StateSpaceRef ss);
StateTable   *statespace_memid_tbl(StateSpaceRef ss);
StateTable   *statespace_accept_tbl(StateSpaceRef ss);
StateTable   *statespace_accept_memid_tbl(StateSpaceRef ss);
unsigned long statespace_space(StateSpaceRef ss);

/* @} */

#endif
