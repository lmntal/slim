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
static void atmstate_free(AutomataStateRef s);
static void atm_transition_free(AutomataTransitionRef t);
static void automata_analysis_dfs1(AutomataRef a, BYTE *on_stack_list,
                                   AutomataStateRef s);
static void automata_analysis_dfs2(AutomataRef a, AutomataStateRef s);
static inline unsigned int atmscc_id(AutomataSCC *s);
static inline BYTE atmscc_type(AutomataSCC *s);
static inline void atmscc_set_type(AutomataSCC *s, BYTE type);

struct AutomataState {
  atmstate_id_t id;
  BOOL is_accept;
  BOOL is_end;
  Vector transitions; /* Vector of Successors (AutomataTransition) */
  AutomataSCC *scc;
};

struct AutomataTransition {
  unsigned int next;
  PLFormulaRef f; /* 実際は命題論理式 */
};

struct AutomataSCC {
  unsigned int id;
  BYTE type;
};

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
    atmstate_free((AutomataStateRef)vec_get(&this->states, i));
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

atmstate_id_t automata_state_scc_id(AutomataRef a, atmstate_id_t id) {
  return atmscc_id(atmstate_scc(automata_get_state(a, id)));
}

const char *automata_state_scc_name(AutomataRef a, atmstate_id_t id) {
  return atmscc_name(atmstate_scc(automata_get_state(a, id)));
}

void automata_add_state(AutomataRef a, AutomataStateRef s) {
  if (vec_num(&a->states) <= s->id) {
    vec_resize(&a->states, s->id + 1, (vec_data_t)0);
  }
  vec_set(&a->states, s->id, (vec_data_t)s);
}

AutomataStateRef automata_get_state(AutomataRef a, BYTE state_id) {
  LMN_ASSERT(vec_get(&a->states, state_id) != 0);
  return (AutomataStateRef)vec_get(&a->states, state_id);
}

atmstate_id_t automata_get_init_state(AutomataRef a) { return a->init_state; }

void automata_set_init_state(AutomataRef a, atmstate_id_t id) {
  a->init_state = id;
}

unsigned int automata_propsym_to_id(AutomataRef a, char *prop_name) {
  st_data_t id;

  if (st_lookup(a->prop_to_id, (st_data_t)prop_name, &id)) {
    return id;
  } else {
    unsigned int new_id;
    char *str;

    /* 0から順にIDを付ける */
    new_id = st_num(a->prop_to_id);
    str = strdup(prop_name);
    st_add_direct(a->prop_to_id, (st_data_t)str, (st_data_t)new_id);
    return new_id;
  }
}

/*----------------------------------------------------------------------
 * state
 */

AutomataStateRef atmstate_make(unsigned int id, BOOL is_accept_state,
                               BOOL is_end_state) {
  AutomataStateRef s = LMN_MALLOC(struct AutomataState);

  vec_init(&s->transitions, 16);
  s->id = id;
  s->is_accept = is_accept_state;
  s->is_end = is_end_state;
  s->scc = NULL;
  return s;
}

static void atmstate_free(AutomataStateRef s) {
  unsigned int i;

  for (i = 0; i < vec_num(&s->transitions); i++) {
    atm_transition_free((AutomataTransitionRef)vec_get(&s->transitions, i));
  }
  vec_destroy(&s->transitions);
  LMN_FREE(s);
}

void atmstate_add_transition(AutomataStateRef s, AutomataTransitionRef t) {
  vec_push(&s->transitions, (vec_data_t)t);
}

atmstate_id_t atmstate_id(AutomataStateRef s) { return s->id; }

unsigned int atmstate_transition_num(AutomataStateRef s) {
  return vec_num(&s->transitions);
}

AutomataTransitionRef atmstate_get_transition(AutomataStateRef s,
                                              unsigned int index) {
  return (AutomataTransitionRef)vec_get(&s->transitions, index);
}

BOOL atmstate_is_accept(AutomataStateRef s) { return s->is_accept; }

BOOL atmstate_is_end(AutomataStateRef s) { return s->is_end; }

void inline atmstate_set_scc(AutomataStateRef s, AutomataSCC *scc) {
  s->scc = scc;
}

BYTE atmstate_scc_type(AutomataStateRef s) {
  return atmscc_type(atmstate_scc(s));
}

AutomataSCC inline *atmstate_scc(AutomataStateRef s) { return s->scc; }

/*----------------------------------------------------------------------
 * SCC analysis for property automata
 */
/* 処理系にロードした性質オートマトンaを解析し, SCC IDなどを追加する */
void automata_analysis(AutomataRef a) {
  AutomataStateRef init_s;
  BYTE *on_stack_list;

  LMN_ASSERT(vec_num(&a->states) > 0);
  init_s = automata_get_state(a, (unsigned int)automata_get_init_state(a));
  on_stack_list = LMN_CALLOC(BYTE, vec_num(&a->states));
  on_stack_list[(unsigned int)atmstate_id(init_s)] = 0xffU;

  automata_analysis_dfs1(a, on_stack_list, init_s);
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

AutomataSCC *atmscc_make() {
  AutomataSCC *m = LMN_MALLOC(AutomataSCC);
  m->id = 0;
  m->type = SCC_TYPE_UNKNOWN;
  return m;
}

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
    AutomataStateRef s = automata_get_state(a, i);
    fprintf(stdout, "%lu::%s{scc(id=%d, name=%s)}.\n",
            (unsigned long)atmstate_id(s), a->state_name(i),
            atmscc_id(atmstate_scc(s)), atmscc_name(atmstate_scc(s)));
  }

  fprintf(stdout, "\nTransitions\n");
  init = automata_get_state(a, (unsigned int)automata_get_init_state(a));
  fprintf(stdout, "init:%lu\n", (unsigned long)atmstate_id(init));
  for (i = 0; i < n; i++) {
    AutomataStateRef s;
    unsigned long j, m;

    s = automata_get_state(a, i);
    fprintf(stdout, "%lu::", (unsigned long)atmstate_id(s));
    m = atmstate_transition_num(s);
    for (j = 0; j < m; j++) {
      fprintf(stdout, "%lu",
              (unsigned long)atmstate_id(
                  automata_get_state(a, (unsigned int)atm_transition_next(
                                            atmstate_get_transition(s, j)))));
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

  n = atmstate_transition_num(s);
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = automata_get_state(
        a, (unsigned int)atm_transition_next(atmstate_get_transition(s, i)));
    if (!on_stack_list[(unsigned int)atmstate_id(succ)]) {
      on_stack_list[(unsigned int)atmstate_id(succ)] = 0xffU;
      automata_analysis_dfs1(a, on_stack_list, succ);
    }
  }

  if (!atmstate_scc(s)) { /* entering 2nd dfs */
    AutomataSCC *scc = atmscc_make();
    atmscc_issue_id(scc);
    atmstate_set_scc(s, scc);
    vec_push(&a->sccs, (vec_data_t)scc);
    if (atmstate_is_end(s)) {
      atmscc_set_type(scc, SCC_TYPE_NON_ACCEPT);
    } else {
      if (atmstate_is_accept(s)) {
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
  n = atmstate_transition_num(s);
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = automata_get_state(
        a, (unsigned int)atm_transition_next(atmstate_get_transition(s, i)));
    if (!atmstate_scc(succ)) {
      AutomataSCC *scc = atmstate_scc(s);
      if ((!atmstate_is_accept(succ) && atmscc_type(scc) == SCC_TYPE_FULLY) ||
          (atmstate_is_accept(succ) &&
           atmscc_type(scc) == SCC_TYPE_NON_ACCEPT)) {
        atmscc_set_type(scc, SCC_TYPE_PARTIALLY);
      }
      atmstate_set_scc(succ, scc);
      automata_analysis_dfs2(a, succ);
    }
  }
}

/*----------------------------------------------------------------------
 * transition
 */

AutomataTransitionRef atm_transition_make(unsigned int next, PLFormulaRef f) {
  AutomataTransitionRef t = LMN_MALLOC(struct AutomataTransition);

  t->next = next;
  t->f = f;
  return t;
}

void atm_transition_free(AutomataTransitionRef t) {
  free_formula(t->f);
  LMN_FREE(t);
}

BYTE atm_transition_next(AutomataTransitionRef t) { return t->next; }

PLFormulaRef atm_transition_get_formula(AutomataTransitionRef t) {
  return t->f;
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
