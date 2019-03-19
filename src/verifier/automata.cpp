/*
 * automata.cpp
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

#include "automata.h"
#include "propositional_symbol.h"
#include "nc_lexer.hpp"
#include "nc_parser.hpp"

static int free_key_str_f(st_data_t key_, st_data_t v_, st_data_t x_);
static int free_val_str_f(st_data_t key_, st_data_t v_, st_data_t x_);
static void automata_analysis_dfs1(AutomataRef a, BYTE *on_stack_list,
                                   AutomataStateRef s);
static void automata_analysis_dfs2(AutomataRef a, AutomataStateRef s);
static inline unsigned int atmscc_id(AutomataSCC *s);
static inline BYTE atmscc_type(AutomataSCC *s);
static inline void atmscc_set_type(AutomataSCC *s, BYTE type);

/*----------------------------------------------------------------------
 * automata
 */

Automata::Automata() {
  vec_init(&this->states, 32);
  vec_init(&this->sccs, 4);
  this->state_name_to_id = st_init_strtable();
  this->id_to_state_name = st_init_numtable();
  this->prop_to_id = st_init_strtable();
  this->prop_num = 0;
  this->init_state = 0; /* デフォルトでは最初の状態が初期状態 */
}

Automata::~Automata() {
  unsigned int i;

  /* free key strings */
  st_foreach(this->state_name_to_id, (st_iter_func)free_key_str_f, (st_data_t)0);
  st_free_table(this->state_name_to_id);

  /* free value strings */
  st_foreach(this->id_to_state_name, (st_iter_func)free_val_str_f, (st_data_t)0);
  st_free_table(this->id_to_state_name);

  /* free key strings */
  st_foreach(this->prop_to_id, (st_iter_func)free_key_str_f, (st_data_t)0);
  st_free_table(this->prop_to_id);

  /* free states */
  for (i = 0; i < vec_num(&this->states); i++) {
    delete (AutomataStateRef)vec_get(&this->states, i);
  }

  /* free sccs */
  for (i = 0; i < vec_num(&this->sccs); i++) {
    atmscc_free((AutomataSCC *)vec_get(&this->sccs, i));
  }

  vec_destroy(&this->states);
  vec_destroy(&this->sccs);
}

static int free_key_str_f(st_data_t key_, st_data_t v_, st_data_t x_) {
  free((char *)key_);
  return ST_CONTINUE;
}

static int free_val_str_f(st_data_t key_, st_data_t v_, st_data_t x_) {
  free((char *)v_);
  return ST_CONTINUE;
}

atmstate_id_t Automata::state_id(const char *state_name) {
  st_data_t id;

  if (st_lookup(this->state_name_to_id, (st_data_t)state_name, &id)) {
    return id;
  } else {
    /* 0から順にIDを付ける */
    char *str0;
    char *str1;
    int new_id;

    new_id = st_num(this->state_name_to_id);
    str0 = strdup(state_name);
    str1 = strdup(state_name);

    st_add_direct(this->state_name_to_id, (st_data_t)str0, (st_data_t)new_id);
    st_add_direct(this->id_to_state_name, (st_data_t)new_id, (st_data_t)str1);
    return new_id;
  }
}

const char *Automata::state_name(atmstate_id_t id) {
  char *name;

  if (st_lookup(this->id_to_state_name, (st_data_t)(int)id, (st_data_t *)&name)) {
    return name;
  } else {
    lmn_fatal("implementation error\n");
    return NULL;
  }
}

void Automata::add_state(AutomataStateRef s) {
  if (vec_num(&this->states) <= s->id) {
    vec_resize(&this->states, s->id + 1, (vec_data_t)0);
  }
  vec_set(&this->states, s->id, (vec_data_t)s);
}

AutomataStateRef Automata::get_state(atmstate_id_t state_id) {
  LMN_ASSERT(vec_get(&this->states, state_id) != 0);
  return (AutomataStateRef)vec_get(&this->states, state_id);
}

atmstate_id_t Automata::get_init_state() { return this->init_state; }

void Automata::set_init_state(atmstate_id_t id) {
  this->init_state = id;
}

unsigned int Automata::propsym_to_id(char *prop_name) {
  st_data_t id;

  if (st_lookup(this->prop_to_id, (st_data_t)prop_name, &id)) {
    return id;
  } else {
    unsigned int new_id;
    char *str;

    /* 0から順にIDを付ける */
    new_id = st_num(this->prop_to_id);
    str = strdup(prop_name);
    st_add_direct(this->prop_to_id, (st_data_t)str, (st_data_t)new_id);
    return new_id;
  }
}

/*----------------------------------------------------------------------
 * state
 */

AutomataState::AutomataState(unsigned int id, BOOL is_accept_state,
                               BOOL is_end_state) {
  vec_init(&this->transitions, 16);
  this->id = id;
  this->is_accept = is_accept_state;
  this->is_end = is_end_state;
  this->scc = NULL;
}

AutomataState::~AutomataState() {
  unsigned int i;

  for (i = 0; i < vec_num(&this->transitions); i++) {
    delete (AutomataTransitionRef)vec_get(&this->transitions, i);
  }
  vec_destroy(&this->transitions);
}

void AutomataState::add_transition(AutomataTransitionRef t) {
  vec_push(&this->transitions, (vec_data_t)t);
}

atmstate_id_t AutomataState::get_id() { return this->id; }

unsigned int AutomataState::get_transition_num() {
  return vec_num(&this->transitions);
}

AutomataTransitionRef AutomataState::get_transition(unsigned int index) {
  return (AutomataTransitionRef)vec_get(&this->transitions, index);
}

BOOL AutomataState::get_is_accept() { return this->is_accept; }

BOOL AutomataState::get_is_end() { return this->is_end; }

void inline AutomataState::set_scc(AutomataSCC *scc) {
  this->scc = scc;
}

BYTE AutomataState::scc_type() {
  return atmscc_type(this->get_scc());
}

AutomataSCC inline *AutomataState::get_scc() { return this->scc; }

/*----------------------------------------------------------------------
 * SCC analysis for property automata
 */
/* 処理系にロードした性質オートマトンaを解析し, SCC IDなどを追加する */
void Automata::analysis() {
  AutomataStateRef init_s;
  BYTE *on_stack_list;

  LMN_ASSERT(vec_num(&this->states) > 0);
  init_s = this->get_state((unsigned int)this->get_init_state());
  on_stack_list = LMN_CALLOC(BYTE, vec_num(&this->states));
  on_stack_list[(unsigned int)init_s->get_id()] = 0xffU;

  automata_analysis_dfs1(this, on_stack_list, init_s);
  LMN_FREE(on_stack_list);
}

/* for debug */
const char *atmscc_name(AutomataSCC *s) {
  const char *ret = NULL;
  switch (atmscc_type(s)) {
  case SCC_TYPE_UNKNOWN:
    ret = "Still_UnKnown.";
    break;
  case SCC_TYPE_FULLY:
    ret = "Fully_Accepting";
    break;
  case SCC_TYPE_PARTIALLY:
    ret = "Partially_Accepting";
    break;
  case SCC_TYPE_NON_ACCEPT:
    ret = "Non_Accepting";
    break;
  default:
    lmn_fatal("unexpected.");
    break;
  }
  return ret;
}

AutomataSCC::AutomataSCC() : id(0), type(SCC_TYPE_UNKNOWN) {}

void atmscc_free(AutomataSCC *m) { LMN_FREE(m); }

/** CAUTION: MT-Unsafe */
static inline void atmscc_issue_id(AutomataSCC *s) {
  static unsigned int unsafe_id_counter = 0;
  s->id = unsafe_id_counter++;
}

static inline unsigned int atmscc_id(AutomataSCC *s) { return s->id; }

static inline BYTE atmscc_type(AutomataSCC *s) { return s->type; }

static inline void atmscc_set_type(AutomataSCC *s, BYTE type) {
  s->type = type;
}

/* for debug only
 * 通常の状態遷移グラフと同様の形式で性質オートマトンを出力する.
 * そのままlavitに喰わせて解析することが目的 */
void print_property_automata(AutomataRef a) {
  AutomataStateRef init;
  unsigned long i, n;

  fprintf(stdout, "States\n");
  n = vec_num(&(a->states));

  for (i = 0; i < n; i++) {
    AutomataStateRef s = a->get_state(i);
    fprintf(stdout, "%lu::%s{scc(id=%d, name=%s)}.\n",
            (unsigned long)s->get_id(), a->state_name(i),
            atmscc_id(s->get_scc()), atmscc_name(s->get_scc()));
  }

  fprintf(stdout, "\nTransitions\n");
  init = a->get_state((unsigned int)a->get_init_state());
  fprintf(stdout, "init:%lu\n", (unsigned long)init->get_id());
  for (i = 0; i < n; i++) {
    AutomataStateRef s;
    unsigned long j, m;

    s = a->get_state(i);
    fprintf(stdout, "%lu::", (unsigned long)s->get_id());
    m = s->get_transition_num();
    for (j = 0; j < m; j++) {
      fprintf(stdout, "%lu",
              (unsigned long)
                  a->get_state(s->get_transition(j)->get_next())->get_id());
      if (j + 1 < m)
        fprintf(stdout, ",");
    }
    fprintf(stdout, "\n");
  }
}

/* dfs postorder順を求め, postorder順に2nd DFSを行う.
 * 性質頂点に, SCC-TYPEを割り当てる.
 * 真面目に書いてないのでFullyとPartiallyの判定が間違っている気がする. */
static void automata_analysis_dfs1(AutomataRef a, BYTE *on_stack_list,
                                   AutomataStateRef s) {
  unsigned long i, n;

  n = s->get_transition_num();
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = a->get_state(
        s->get_transition(i)->get_next());
    if (!on_stack_list[(unsigned int)succ->get_id()]) {
      on_stack_list[(unsigned int)succ->get_id()] = 0xffU;
      automata_analysis_dfs1(a, on_stack_list, succ);
    }
  }

  if (!s->get_scc()) { /* entering 2nd dfs */
    AutomataSCC *scc = new AutomataSCC();
    atmscc_issue_id(scc);
    s->set_scc(scc);
    vec_push(&a->sccs, (vec_data_t)scc);
    if (s->get_is_end()) {
      atmscc_set_type(scc, SCC_TYPE_NON_ACCEPT);
    } else {
      if (s->get_is_accept()) {
        atmscc_set_type(scc, SCC_TYPE_FULLY);
      } else {
        atmscc_set_type(scc, SCC_TYPE_NON_ACCEPT);
      }
      automata_analysis_dfs2(a, s);
    }
  }
}

static void automata_analysis_dfs2(AutomataRef a, AutomataStateRef s) {
  unsigned long i, n;
  n = s->get_transition_num();
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = a->get_state(
        s->get_transition(i)->get_next());
    if (!succ->get_scc()) {
      AutomataSCC *scc = s->get_scc();
      if ((!succ->get_is_accept() && atmscc_type(scc) == SCC_TYPE_FULLY) ||
          (succ->get_is_accept() &&
           atmscc_type(scc) == SCC_TYPE_NON_ACCEPT)) {
        atmscc_set_type(scc, SCC_TYPE_PARTIALLY);
      }
      succ->set_scc(scc);
      automata_analysis_dfs2(a, succ);
    }
  }
}

/*----------------------------------------------------------------------
 * transition
 */

AutomataTransition::AutomataTransition(atmstate_id_t next, PLFormulaRef f)
  : next(next), f(f) {}

AutomataTransition::~AutomataTransition () {
  free_formula(this->f);
}

atmstate_id_t AutomataTransition::get_next() { return this->next; }

PLFormulaRef AutomataTransition::get_formula() {
  return this->f;
}

/*----------------------------------------------------------------------
 * never claim
 */

int ncparse(nc::lexer *, AutomataRef);

/* 正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
static int nc_parse(FILE *in, AutomataRef *automata) {
  nc::lexer scanner(in);

  *automata = new Automata();

  return ncparse(&scanner, *automata);
}

int never_claim_load(FILE *f, AutomataRef *a) { return nc_parse(f, a); }

/*----------------------------------------------------------------------
 * propositional logic formula
 */

enum PLNode { N_AND, N_OR, N_NEGATION, N_SYMBOL, N_TRUE, N_FALSE };
typedef enum PLNode PLNode;

struct PLFormula {
  PLNode node_type;

  /* 式の構成に必要なデータ */
  PLFormulaRef arg0;   /* for AND,OR,NEGATION */
  PLFormulaRef arg1;   /* for AND,OR */
  unsigned int sym_id; /* for SYMBOL */
};

static PLFormulaRef make_unary_op(PLNode node_type, PLFormulaRef f0) {
  PLFormulaRef f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  f->arg0 = f0;
  return f;
}

static PLFormulaRef make_binary_op(PLNode node_type, PLFormulaRef f0,
                                   PLFormulaRef f1) {
  PLFormulaRef f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  f->arg0 = f0;
  f->arg1 = f1;
  return f;
}

void free_formula(PLFormulaRef f) {
  switch (f->node_type) {
  case N_NEGATION:
    free_formula(f->arg0);
    break;
  case N_AND:
  case N_OR:
    free_formula(f->arg0);
    free_formula(f->arg1);
    break;
  case N_TRUE:
  case N_FALSE:
  case N_SYMBOL:
    break;
  }
  free(f);
}

static PLFormulaRef ltl_formula_make(PLNode node_type) {
  PLFormulaRef f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  return f;
}

PLFormulaRef true_node_make() { return ltl_formula_make(N_TRUE); }

PLFormulaRef false_node_make() { return ltl_formula_make(N_FALSE); }

PLFormulaRef sym_node_make(int sym_id) {
  PLFormulaRef f = ltl_formula_make(N_SYMBOL);

  f->sym_id = sym_id;
  return f;
}

PLFormulaRef negation_node_make(PLFormulaRef f0) {
  return make_unary_op(N_NEGATION, f0);
}

PLFormulaRef and_node_make(PLFormulaRef f0, PLFormulaRef f1) {
  return make_binary_op(N_AND, f0, f1);
}

PLFormulaRef or_node_make(PLFormulaRef f0, PLFormulaRef f1) {
  return make_binary_op(N_OR, f0, f1);
}

/* 式fとシンボル定義prop_defsを膜memで評価する */
BOOL eval_formula(LmnMembraneRef mem, Vector *prop_defs, PLFormulaRef f) {
  switch (f->node_type) {
  case N_NEGATION:
    return !eval_formula(mem, prop_defs, f->arg0);
  case N_AND:
    return eval_formula(mem, prop_defs, f->arg0) &&
           eval_formula(mem, prop_defs, f->arg1);
  case N_OR:
    return eval_formula(mem, prop_defs, f->arg0) ||
           eval_formula(mem, prop_defs, f->arg1);
  case N_TRUE:
    return TRUE;
  case N_FALSE:
    return FALSE;
  case N_SYMBOL:
    return proposition_eval(
        propsym_get_proposition(propsyms_get(prop_defs, f->sym_id)), mem);
  default:
    lmn_fatal("implementation error");
  }
}
