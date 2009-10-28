/*
 * state.h
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

#ifndef LMN_STATE_H
#define LMN_STATE_H

#include "lmntal.h"
#include "mem_encode.h"
#include "membrane.h"
#include "rule.h"
#include "vector.h"


typedef struct State State;

/* ----------------------------------------------------------------------
 * State Space
 */ 

typedef struct StateSpace *StateSpace;
State *state_space_get(const StateSpace states, State *s);
StateSpace state_space_make(void);
void state_space_free(StateSpace states);
unsigned long state_space_num(StateSpace states);
void state_space_set_init_state(StateSpace states, State* init_state);
void state_space_add_end_state(StateSpace states, State *s);
const Vector *state_space_end_states(StateSpace states);
void state_space_remove(const StateSpace states, State *s);
st_table_t state_space_tbl(StateSpace states);
inline BOOL state_space_calc_memid_hash(StateSpace states, unsigned long hash);
State *insert_state(StateSpace states, State *s);

void dump_all_state_mem(StateSpace states, FILE *file);
void dump_state_transition_graph(StateSpace states, FILE *file);
void dump_state_data(State *state);


/* ----------------------------------------------------------------------
 * State
 */ 

struct State {
  LmnMembrane *mem;     /* グローバルルート膜 */
  unsigned long hash; /* mhash(mem) */
  BYTE state_name;
  BOOL flags;           /* nested DFSの際にDFS{1,2}いずれの走査を受けたかや，successorが展開済であるか否かを管理するフラグ */
  Vector successor;     /* 通常時: Vector of States，ample(s)計算中: Vector of StateTransitions */
  LmnRule rule;
  LmnBinStr mem_id;
  unsigned long mem_id_hash;
  LmnBinStr mem_dump;
};

/* 膜同型性判定にこの回数以上失敗すると膜のエンコードを行う */
#define MEM_EQ_FAIL_THRESHOLD 2

/* 状態IDが本来不必要な場合に使用する状態ID */
#define DEFAULT_STATE_ID 0

extern struct st_hash_type type_statehash;
extern struct st_hash_type type_memid_statehash;

State *state_make(LmnMembrane *mem, BYTE state_name, LmnRule rule);
State *state_make_for_nd(LmnMembrane *mem, LmnRule rule);
inline LmnMembrane *state_copied_mem(State *state);
void state_free(State *s);
inline void state_free_mem(State *s);
BYTE state_property_state(State *state);
void state_set_property_state(State *state, BYTE prop_state);
inline LmnMembrane *state_mem(State *state);
inline LmnRule state_rule(State *state);
inline LmnBinStr state_mem_binstr(State *state);
inline long state_hash(State *s);
inline int state_cmp(HashKeyType s1, HashKeyType s2);

inline void state_calc_mem_dump(State *s);
inline void state_free_mem_dump(State *s);
inline int state_memid_cmp(st_data_t _s1, st_data_t _s2);
inline long state_memid_hash(State *s);
inline void state_succ_add(State *s, State *succ);
inline unsigned int state_succ_num(State *s);
inline State *state_succ_get(State *s, unsigned int i);
inline void state_restore_mem(State *s);
inline void state_calc_mem_encode(State *s);
inline void state_calc_mem_dump(State *s);

/* flag of the first DFS (nested DFS, on-stack state) */
#define FST_MASK                   (0x01U)
/* flag of the second DFS (nested DFS, visited state) */
#define SND_MASK                   (0x01U << 1)
/* flag to show that it is on the search stack */
#define ON_STACK_MASK              (0x01U << 2)
/* flag to show that all its successor states(and transitions) are expanded */
#define EXPANDED_MASK              (0x01U << 3)
/* flag to show that all the independency relations between transitions enabled on the state are checked */
#define INDEPENDENCY_CHECKED_MASK  (0x01U << 4)
/* flag to show that it was not reduced by the partial order reduction */
#define REPRESENTATIVE_MASK        (0x01U << 5)
/* macros for nested DFS */
#define set_fst(S)                     ((S)->flags |= FST_MASK)
#define unset_fst(S)                   ((S)->flags &= (~FST_MASK))
#define is_fst(S)                      ((S)->flags & FST_MASK)
#define set_snd(S)                     ((S)->flags |= SND_MASK)
#define unset_snd(S)                   ((S)->flags &= (~SND_MASK))
#define is_snd(S)                      ((S)->flags & SND_MASK)
#define set_open(S)                    ((S)->flags |= ON_STACK_MASK)
#define unset_open(S)                  ((S)->flags &= (~ON_STACK_MASK))
#define is_open(S)                     ((S)->flags & ON_STACK_MASK)
#define set_expanded(S)                ((S)->flags |= EXPANDED_MASK)
#define unset_expanded(S)              ((S)->flags &= (~EXPANDED_MASK))
#define is_expanded(S)                 ((S)->flags & EXPANDED_MASK)
#define set_independency_checked(S)    ((S)->flags |= INDEPENDENCY_CHECKED_MASK)
#define unset_independency_checked(S)  ((S)->flags &= (~INDEPENDENCY_CHECKED_MASK))
#define is_independency_checked(S)     ((S)->flags & INDEPENDENCY_CHECKED_MASK)
#define set_ample(S)                   ((S)->flags |= REPRESENTATIVE_MASK)
#define is_ample(S)                    ((S)->flags & REPRESENTATIVE_MASK)

#endif
