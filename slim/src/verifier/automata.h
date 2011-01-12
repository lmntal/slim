/*
 * automata.h
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

#ifndef LMN_PROP_AUTOMATA
#define LMN_PROP_AUTOMATA

#include "lmntal.h"
#include "vector.h"
#include "st.h"

typedef struct Automata            *Automata;
typedef struct AutomataState       *AutomataState;
typedef struct AutomataTransition  *AutomataTransition;
typedef struct AutomataSCC         AutomataSCC;

typedef BYTE atmstate_id_t; /* 性質ラベル(状態)数は256個まで */

/* Propositional Logic Formula */
typedef struct PLFormula *PLFormula;

enum SCC_ACCEPTING_TYPE {
  SCC_TYPE_UNKNOWN     = 0U,
  SCC_TYPE_FULLY       = 1U,  /* 構成するサイクルが全て受理サイクル */
  SCC_TYPE_PARTIALLY   = 2U,  /* 構成するサイクルが非受理サイクルも含む */
  SCC_TYPE_NON_ACCEPT  = 3U,  /* 受理サイクルを含まない */
};


/* automata */
Automata           automata_make(void);
void               automata_free(Automata a);
atmstate_id_t      automata_state_id(Automata a, char *state_name);
const char        *automata_state_name(Automata a, atmstate_id_t id);
atmstate_id_t      automata_state_scc_id(Automata a, atmstate_id_t id);
const char        *automata_state_scc_name(Automata a, atmstate_id_t id);
AutomataState      automata_get_state(Automata a, BYTE state_id);
void               automata_set_init_state(Automata a, atmstate_id_t id);
atmstate_id_t      automata_get_init_state(Automata a);
unsigned int       automata_propsym_to_id(Automata a, char *prop_name);
AutomataState      atmstate_make(unsigned int id,
                                 BOOL is_accept_state,
                                 BOOL is_end_state);

/* state of automata */
void               atmstate_add_transition(AutomataState s, AutomataTransition t);
void               automata_add_state(Automata a, AutomataState s);
atmstate_id_t      atmstate_id(AutomataState s);
unsigned int       atmstate_transition_num(AutomataState s);
AutomataTransition atmstate_get_transition(AutomataState s, unsigned int index);
BOOL               atmstate_is_accept(AutomataState s);
BOOL               atmstate_is_end(AutomataState s);
inline void        atmstate_set_scc(AutomataState s, AutomataSCC *scc);
inline BYTE        atmstate_scc_type(AutomataState s);
inline AutomataSCC *atmstate_scc(AutomataState s);


/* transition of automata */

AutomataTransition atm_transition_make(unsigned int next, PLFormula f);
BYTE               atm_transition_next(AutomataTransition t);
PLFormula          atm_transition_get_formula(AutomataTransition t);

/* SCC analysis of automata */
void               automata_analysis(Automata a);
AutomataSCC       *atmscc_make(void);
void               atmscc_free(AutomataSCC *s);
char              *atmscc_name(AutomataSCC *s);
void               print_property_automata(Automata a);

/* propositional Logic Formula */
PLFormula          true_node_make(void);
PLFormula          false_node_make(void);
PLFormula          sym_node_make(int sym_id);
PLFormula          negation_node_make(PLFormula f0);
PLFormula          and_node_make(PLFormula f0, PLFormula f1);
PLFormula          or_node_make(PLFormula f0, PLFormula f1);
void               free_formula(PLFormula f);
BOOL               eval_formula(LmnMembrane *mem, Vector *prop_defs, PLFormula f);

/* never claim */
int                never_claim_load(FILE *f, Automata *a);

#endif /* LMN_PROP_AUTOMATA */
