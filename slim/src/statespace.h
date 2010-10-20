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
#include "state.h"
#include "st.h"
#include "vector.h"
#include "./slim_header/port.h"
#include "lmntal_thread.h"
#include "delta_membrane.h"

/** Structures
 */
typedef struct StateSpace *StateSpace;
typedef struct StateTable StateTable;

struct state_space_type {
  int(*compare) ( );               /* 状態の等価性判定を行う関数 */
  LmnBinStr(*compress) (State *s); /* 状態sの圧縮バイト列を計算して返す関数 */
};


struct StateSpace {
  BOOL             is_concurrent; /* 並列ハッシュ表として扱うか否か */
  LmnPort         *out;           /* dump先 */
  State           *init_state;    /* 初期状態 */
  Vector          *end_states;    /* 最終状態の集合 */
  StateTable      *tbl;           /* mhash値をkeyに, 状態のアドレスを登録する状態管理表 */
  StateTable      *memid_tbl;     /* memid_hashをkeyに, 状態のアドレスを登録する状態管理表 */
#ifdef PROFILE
  HashSet memid_hashes;   /* 膜のIDで同型性の判定を行うハッシュ値(mhash)のSet */
#endif
};

struct StateTable {
  struct state_space_type *type;
  State            **tbl;
  unsigned long    cap;
  unsigned long    cap_density;
  unsigned long    *num;
  unsigned long    *num_dummy;
  EWLock           *lock;
  StateTable       *rehash_tbl;   /* rehashした際に登録するテーブル */
};

#define DEFAULT_ARGS  (LmnWord)NULL

/** StateSpce */
StateSpace state_space_make(void);
StateSpace state_space_make_for_parallel(void);
void state_space_free(StateSpace ss);
unsigned long state_space_num_raw(StateSpace ss);
unsigned long state_space_num(StateSpace ss);
unsigned long state_space_dummy_num(StateSpace ss);
unsigned long state_space_end_num(StateSpace ss);
State *state_space_init_state(StateSpace ss);
void state_space_set_init_state(StateSpace ss, State* init_state, BOOL enable_compact);
void state_space_add_end_state(StateSpace ss, State *s);
const Vector *state_space_end_states(StateSpace ss);
StateTable *state_space_tbl(StateSpace ss);
StateTable *state_space_memid_tbl(StateSpace ss);
inline BOOL state_space_is_memid_hash(StateSpace ss, unsigned long hash);
void state_space_add_direct(StateSpace ss, State *s);
inline State *state_space_insert(StateSpace ss, State *s);
State *state_space_insert_delta(StateSpace ss, State *s, struct MemDeltaRoot *d);
unsigned long state_space_space(StateSpace ss);
void state_space_foreach(StateSpace ss, void (*func) ( ), LmnWord _arg);
void state_table_foreach(StateTable *st, void (*func) ( ), LmnWord _arg);
void state_space_format_states(StateSpace ss);
void state_table_format_states(StateTable *st);

/** Printer */
void state_space_ends_dumper(StateSpace ss, FILE *f);
void state_space_dumper(StateSpace ss, FILE *f);
void state_space_dump_all_error_paths(StateSpace ss, FILE *f);
#endif
