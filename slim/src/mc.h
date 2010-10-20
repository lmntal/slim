/*
 * mc.h
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

#ifndef LMN_MC_H
#define LMN_MC_H

#include "lmntal.h"
#include "lmntal_thread.h"
#include "automata.h"
#include "react_context.h"
#include "nd.h"
#include "state.h"
#include "statespace.h"

struct MCData {
  BOOL has_property;
  BOOL do_search;
  BOOL do_exhaustive;
  BOOL do_parallel;
  BOOL mc_exit;
  Automata  property_automata;
  Vector   *propsyms;
  Vector   *errors;          /* safety  : 反例を示す頂点の集合,
                                liveness: 受理サイクル上に存在する頂点の集合 */
  Vector   *cycles;
  BOOL is_format_states;     /* 出力直前のformatを行っている場合は真 */
} mc_data;

int mc_load_property(Automata *a, PVector *prop_defs);
void mc_explain_error(int error_id);
char *mc_error_msg(int error_id);
void mc_gen_successors(State            *s,
                       LmnMembrane      *mem,
                       AutomataState    prop_atm_s,
                       struct ReactCxt  *rc,
                       BOOL             flags);
void mc_nested_dfs(StateSpace states,
                   Vector     *list,
                   Vector     *cycle,
                   State      *seed,
                   BOOL       flags);
void mc_violate(StateSpace   states,
                State        *seed,
                const Vector *list,
                BOOL         flags);
void mc_violate_broadcast(void);
void mc_dump_all_errors(StateSpace ss, FILE *f);
unsigned long mc_get_error_num(void);

#endif
