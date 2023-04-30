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

using AutomataRef = struct Automata*;
using AutomataStateRef = struct AutomataState*;
using AutomataTransitionRef = struct AutomataTransition*;
using AutomataSCC = struct AutomataSCC;

using atmstate_id_t = BYTE; /* 性質ラベル(状態)数は256個まで */

struct Automata {
  atmstate_id_t init_state;
  unsigned int prop_num;
  Vector states; /* Vector of AutomataState */
  st_table_t state_name_to_id;
  st_table_t id_to_state_name;
  st_table_t prop_to_id;
  Vector sccs;

  Automata(void);
  ~Automata(void);
  atmstate_id_t state_id(const char *);
  const char *state_name(atmstate_id_t);
  void add_state(AutomataStateRef);
  AutomataStateRef get_state(atmstate_id_t);
  atmstate_id_t get_init_state();
  void set_init_state(atmstate_id_t);
  unsigned int propsym_to_id(char *prop_name);
  void analysis();
  void print_property();
};

struct AutomataState {
  atmstate_id_t id;
  BOOL is_accept;
  BOOL is_end;
  Vector transitions; /* Vector of Successors (AutomataTransition) */
  AutomataSCC *scc;

  AutomataState(unsigned int, BOOL, BOOL);
  ~AutomataState();
  void add_transition(AutomataTransitionRef t);
  atmstate_id_t get_id();
  unsigned int get_transition_num();
  AutomataTransitionRef get_transition(unsigned int index);
  BOOL get_is_accept();
  BOOL get_is_end();
  void set_scc(AutomataSCC *scc);
  BYTE scc_type();
  AutomataSCC *get_scc();
};

/* Propositional Logic Formula */
using PLFormulaRef = struct PLFormula*;

struct AutomataTransition {
  atmstate_id_t next;
  PLFormulaRef f; /* 実際は命題論理式 */

  AutomataTransition(atmstate_id_t next, PLFormulaRef f);
  ~AutomataTransition();
  atmstate_id_t get_next();
  PLFormulaRef get_formula();
};

struct AutomataSCC {
private:
  unsigned int id;
  BYTE type;
  static unsigned int unsafe_id_counter;
public:
  AutomataSCC();
  ~AutomataSCC();
  const char *get_name();
/** CAUTION: MT-Unsafe */
  void issue_id(){
  	id = unsafe_id_counter++;
	//Is it checked by test cases? by sumiya
  }
  unsigned int get_id(){
  	return id;
  }
  BYTE get_type(){
  	return type;
  }
  void set_type(BYTE t){
  	type = t;
  }
};

enum SCC_ACCEPTING_TYPE {
  SCC_TYPE_UNKNOWN = 0U,
  SCC_TYPE_FULLY = 1U, /* 構成するサイクルが全て受理サイクル */
  SCC_TYPE_PARTIALLY = 2U, /* 構成するサイクルが非受理サイクルも含む */
  SCC_TYPE_NON_ACCEPT = 3U, /* 受理サイクルを含まない */
};

/* propositional Logic Formula */
PLFormulaRef true_node_make(void);
PLFormulaRef false_node_make(void);
PLFormulaRef sym_node_make(int sym_id);
PLFormulaRef negation_node_make(PLFormulaRef f0);
PLFormulaRef and_node_make(PLFormulaRef f0, PLFormulaRef f1);
PLFormulaRef or_node_make(PLFormulaRef f0, PLFormulaRef f1);
void free_formula(PLFormulaRef f);
BOOL eval_formula(LmnMembraneRef mem, Vector *prop_defs, PLFormulaRef f);

/* never claim */
int never_claim_load(FILE *f, AutomataRef *a);

/* @} */

#endif /* LMN_PROP_AUTOMATA */
