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

/**
 * @addtogroup  Verifier
 * @{
 */

#include "../lmntal.h"
#include "element/element.h"
#include "automata.h"
#include "vm/vm.h"
#include "verifier.h"
#include "statespace.h"
#include "mc_worker.h"


#ifdef DEBUG
# define MC_DEBUG(Pr) if (lmn_env.debug_mc) { Pr; }
#else
# define MC_DEBUG(Pr)
#endif

#define MC_GET_PROPERTY(S, A)  ((A) ? automata_get_state(A, state_property_state(S)) : DEFAULT_PROP_AUTOMATA)

BOOL mc_vec_states_valid(Vector *v);

void mc_print_vec_states(StateSpaceRef ss,
                         Vector     *v,
                         State      *seed);
void mc_expand(const StateSpaceRef states,
               State            *state,
               AutomataStateRef    property_automata_state,
               LmnReactCxtRef      rc,
               Vector           *new_s,
               Vector           *psyms,
               BOOL             flag);
void mc_update_cost(State *s, Vector *new_ss, EWLock *ewlock);
void mc_gen_successors_with_property(State         *s,
                                     LmnMembraneRef   mem,
                                     AutomataStateRef prop_atm_s,
                                     LmnReactCxtRef   rc,
                                     Vector        *psyms,
                                     BOOL          flags);
void mc_gen_successors(State       *src,
                       LmnMembraneRef mem,
                       BYTE        prop_labels,
                       LmnReactCxtRef rc,
                       BOOL        flags);
void mc_store_successors(const StateSpaceRef ss,
                         State            *s,
                         LmnReactCxtRef      rc,
                         Vector           *new_ss,
                         BOOL             f);
BOOL mc_expand_inner(LmnReactCxtRef rc, LmnMembraneRef cur_mem);

void run_mc(Vector *start_rulesets, AutomataRef a, Vector *psyms);

int mc_load_property(AutomataRef *a, PVector *prop_defs);
void mc_explain_error(int error_id);
const char *mc_error_msg(int error_id);

void mc_found_invalid_state(LmnWorkerGroup *wp, State *seed);
void mc_found_invalid_path(LmnWorkerGroup *wp, Vector *path);
unsigned long mc_invalids_get_num(LmnWorkerGroup *wp);
void mc_dump_all_errors(LmnWorkerGroup *wp, FILE *f);

/* @} */

#endif
