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
#include "rule.h"
#include "vector.h"

typedef st_table_t StateSpace;

typedef struct State State;

struct State {
  LmnMembrane *mem;     /* グローバルルート膜 */
  unsigned long hash; /* mhash(mem) */
  BOOL flags;           /* nested DFSの際にDFS{1,2}いずれの走査を受けたかや，successorが展開済であるか否かを管理するフラグ */
  Vector successor;     /* 通常時: Vector of States，ample(s)計算中: Vector of StateTransitions */
  LmnRule rule;
  BYTE state_name;
};

/* 訪問済みノードが格納される */
extern st_table *States;

Vector *nd_expand(State *state);
void run_nd(LmnRuleSet start_ruleset);
State *insert_state(StateSpace states, State *s);
void dump_state_transition_graph(State *init_state, FILE *file);
void state_space_free(StateSpace states);

#endif
