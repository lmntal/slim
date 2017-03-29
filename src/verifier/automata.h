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

/**
 * @ingroup  Verifier
 * @defgroup Automata
 * @{
 */

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"

typedef struct Automata            *AutomataRef;
typedef struct AutomataState       *AutomataStateRef;
typedef struct AutomataTransition  *AutomataTransitionRef;
typedef struct AutomataSCC         AutomataSCC;

typedef BYTE atmstate_id_t; /* 性質ラベル(状態)数は256個まで */

/* Propositional Logic Formula */
typedef struct PLFormula *PLFormulaRef;

enum SCC_ACCEPTING_TYPE {
  SCC_TYPE_UNKNOWN     = 0U,
  SCC_TYPE_FULLY       = 1U,  /* 構成するサイクルが全て受理サイクル */
  SCC_TYPE_PARTIALLY   = 2U,  /* 構成するサイクルが非受理サイクルも含む */
  SCC_TYPE_NON_ACCEPT  = 3U,  /* 受理サイクルを含まない */
};


/* automata */
AutomataRef           automata_make(void);
void               automata_free(AutomataRef a);
atmstate_id_t      automata_state_id(AutomataRef a, char *state_name);
const char        *automata_state_name(AutomataRef a, atmstate_id_t id);
atmstate_id_t      automata_state_scc_id(AutomataRef a, atmstate_id_t id);
const char        *automata_state_scc_name(AutomataRef a, atmstate_id_t id);
AutomataStateRef      automata_get_state(AutomataRef a, BYTE state_id);
void               automata_set_init_state(AutomataRef a, atmstate_id_t id);
atmstate_id_t      automata_get_init_state(AutomataRef a);
unsigned int       automata_propsym_to_id(AutomataRef a, char *prop_name);
AutomataStateRef      atmstate_make(unsigned int id,
                                 BOOL is_accept_state,
                                 BOOL is_end_state);

/* state of automata */
void               atmstate_add_transition(AutomataStateRef s, AutomataTransitionRef t);
void               automata_add_state(AutomataRef a, AutomataStateRef s);
atmstate_id_t      atmstate_id(AutomataStateRef s);
unsigned int       atmstate_transition_num(AutomataStateRef s);
AutomataTransitionRef atmstate_get_transition(AutomataStateRef s, unsigned int index);
BOOL               atmstate_is_accept(AutomataStateRef s);
BOOL               atmstate_is_end(AutomataStateRef s);
void        atmstate_set_scc(AutomataStateRef s, AutomataSCC *scc);
BYTE        atmstate_scc_type(AutomataStateRef s);
AutomataSCC *atmstate_scc(AutomataStateRef s);


/* transition of automata */

AutomataTransitionRef atm_transition_make(unsigned int next, PLFormulaRef f);
BYTE               atm_transition_next(AutomataTransitionRef t);
PLFormulaRef          atm_transition_get_formula(AutomataTransitionRef t);

/* SCC analysis of automata */
void               automata_analysis(AutomataRef a);
AutomataSCC       *atmscc_make(void);
void               atmscc_free(AutomataSCC *s);
const char        *atmscc_name(AutomataSCC *s);
void               print_property_automata(AutomataRef a);

/* propositional Logic Formula */
PLFormulaRef          true_node_make(void);
PLFormulaRef          false_node_make(void);
PLFormulaRef          sym_node_make(int sym_id);
PLFormulaRef          negation_node_make(PLFormulaRef f0);
PLFormulaRef          and_node_make(PLFormulaRef f0, PLFormulaRef f1);
PLFormulaRef          or_node_make(PLFormulaRef f0, PLFormulaRef f1);
void               free_formula(PLFormulaRef f);
BOOL               eval_formula(LmnMembraneRef mem, Vector *prop_defs, PLFormulaRef f);

/* never claim */
int                never_claim_load(FILE *f, AutomataRef *a);

/* @} */

#endif /* LMN_PROP_AUTOMATA */
