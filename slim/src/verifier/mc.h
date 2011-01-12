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
#include "state.h"
#include "statespace.h"


/* TODO: 整理 */
struct MCData {
  BOOL has_property;            /* 性質オートマトンを持つ場合に真 */
  BOOL do_search;               /* 探索を行う場合に真 */
  BOOL do_exhaustive;           /* 全ての反例を探索する場合に真 */
  BOOL do_parallel;             /* 並列アルゴリズムを使用する場合に真 */
  BOOL mc_exit;                 /* 反例の発見により探索を打ち切る場合に真 */
  BOOL error_exist;             /* 反例が存在する場合に真 */
  BOOL is_format_states;        /* 出力直前の状態番号のformatを行った場合に真 */

  Automata  property_automata;  /* Never Clainのストラクチャ */
  Vector   *propsyms;           /* 命題記号定義 */

  Vector   **invalid_seeds;
  Vector   **cycles;
} mc_data;


#ifdef DEBUG
#  define MC_DEBUG(Pr) if (lmn_env.debug_mc) { Pr; }
#else
#  define MC_DEBUG(Pr)
#endif


inline static BOOL mc_vec_states_valid(Vector *v) {
  unsigned int i, j;
  for (i = 0, j = 1; i < vec_num(v) && j < vec_num(v); i++, j++) {
    State *s, *t;
    s = (State *)vec_get(v, i);
    t = (State *)vec_get(v, j);
    if (!state_succ_contains(s, t)) return FALSE;
  }

  return TRUE;
}

void mc_print_vec_states(FILE *f, Vector *v, State *seed);
void mc_expand(const StateSpace states,
               State            *state,
               AutomataState    property_automata_state,
               struct ReactCxt  *rc,
               Vector           *new_s,
               BOOL             flag);
void mc_gen_successors(State           *src,
                       LmnMembrane     *mem,
                       BYTE            prop_labels,
                       struct ReactCxt *rc,
                       BOOL            flags);
void run_mc(Vector *start_rulesets);
StateSpace do_mc(LmnMembrane *world_mem, BOOL flags);


int mc_load_property(Automata *a, PVector *prop_defs);
void mc_explain_error(int error_id);
char *mc_error_msg(int error_id);

void mc_found_invalid_state(State *seed);
void mc_found_invalid_path(Vector *path);
unsigned long mc_invalids_get_num(void);
void mc_dump_all_errors(StateSpace ss, FILE *f);

#endif
