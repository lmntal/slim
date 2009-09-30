/*
 * nd.h
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

#ifndef LMN_ND_H
#define LMN_ND_H

#include "lmntal.h"
#include "st.h"
#include "membrane.h"
#include "mem_encode.h"
#include "rule.h"
#include "vector.h"

typedef struct State State;

struct State {
  LmnMembrane *mem;     /* グローバルルート膜 */
  unsigned long hash; /* mhash(mem) */
  BOOL flags;           /* nested DFSの際にDFS{1,2}いずれの走査を受けたかや，successorが展開済であるか否かを管理するフラグ */
  Vector successor;     /* 通常時: Vector of States，ample(s)計算中: Vector of StateTransitions */
  LmnRule rule;
  BYTE state_name;
  LmnBinStr mem_id;
};

typedef struct StateSpace *StateSpace;

#define state_mem_id(s)  ((s)->mem_id)

Vector *nd_expand(const StateSpace states, State *state);
void run_nd(Vector *start_rulesets);
StateSpace do_nd(LmnMembrane *world_mem);
StateSpace do_nd_dump(LmnMembrane *world_mem_org);
State *insert_state(StateSpace states, State *s);
State *state_space_get(const StateSpace states, State *s);
StateSpace state_space_make(void);
void state_space_free(StateSpace states);
unsigned long state_space_num(StateSpace states);
void state_space_set_init_state(StateSpace states, State* init_state);
void state_space_add_end_state(StateSpace states, State *s);
const Vector *state_space_end_states(StateSpace states);
void state_space_remove(const StateSpace states, State *s);
st_table_t state_space_tbl(StateSpace states);

void dump_all_state_mem(StateSpace states, FILE *file);
void dump_state_transition_graph(StateSpace states, FILE *file);
void dump_state_name(StateSpace states, FILE *file);

#endif
