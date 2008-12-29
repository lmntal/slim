/*
 * automata.c
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
#include "nc_parser.h"
#include "nc_lexer.h"
#include "vector.h"
#include "st.h"
#include "propositional_symbol.h"
#include "error.h"

static int free_key_str_f(st_data_t key_, st_data_t v_, st_data_t x_);
static int free_val_str_f(st_data_t key_, st_data_t v_, st_data_t x_);
static void automata_state_free(AutomataState s);
static void transition_free(Transition t);

struct Automata {
/*   atmstate_id_t init_state; */
  int init_state;
  Vector states;  /* Vector of AutomataState */
  st_table_t state_name_to_id;
  st_table_t id_to_state_name;
  st_table_t prop_to_id;
  unsigned int prop_num;
};

struct AutomataState {
  atmstate_id_t id;
  BOOL is_accept;
  BOOL is_end;
  Vector transitions; /* Vector of Transition */
};

struct Transition {
  unsigned int next;
  PLFormula f; /* 実際は命題論理式 */
};

/*----------------------------------------------------------------------
 * automata
 */

Automata automata_make()
{
  Automata a = LMN_MALLOC(struct Automata);

  vec_init(&a->states, 32);
  a->state_name_to_id = st_init_strtable();
  a->id_to_state_name = st_init_numtable();
  a->prop_to_id = st_init_strtable();
  a->prop_num = 0;
  a->init_state = 0; /* デフォルトでは最初の状態が初期状態 */

  return a;
}

void automata_free(Automata a)
{
  unsigned int i;
  
  /* free key strings */
  st_foreach(a->state_name_to_id, free_key_str_f, (st_data_t)0);
  st_free_table(a->state_name_to_id);

  /* free value strings */
  st_foreach(a->id_to_state_name, free_val_str_f, (st_data_t)0);
  st_free_table(a->id_to_state_name);

  /* free key strings */
  st_foreach(a->prop_to_id, free_key_str_f, (st_data_t)0);
  st_free_table(a->prop_to_id);

  /* free states */
  for (i = 0; i < vec_num(&a->states); i++) {
    automata_state_free((AutomataState)vec_get(&a->states, i));
  }
  vec_destroy(&a->states);
  LMN_FREE(a);
}

static int free_key_str_f(st_data_t key_, st_data_t v_, st_data_t x_)
{
  free((char*)key_);
  return ST_CONTINUE;
}

static int free_val_str_f(st_data_t key_, st_data_t v_, st_data_t x_)
{
  free((char*)v_);
  return ST_CONTINUE;
}

unsigned int automata_state_id(Automata a, char *state_name)
{
  unsigned int id;

  if (st_lookup(a->state_name_to_id, state_name, (st_data_t*)&id)) {
    return id;
  } else {
    /* 0から順にIDを付ける */
    int new_id = st_num(a->state_name_to_id);
    char *str0 = strdup(state_name);
    char *str1 = strdup(state_name);

    st_add_direct(a->state_name_to_id, str0, (st_data_t)new_id);
    st_add_direct(a->id_to_state_name, (st_data_t)new_id, str1);
    return new_id;
  }
}

const char *automata_state_name(Automata a, atmstate_id_t id)
{
  char *name;
  
  if (st_lookup(a->id_to_state_name, (st_data_t)(int)id, (st_data_t*)&name)) {
    return name;
  } else {
    lmn_fatal("implementation error\n");
    return NULL;
  }
}

void automata_add_state(Automata a, AutomataState s)
{
  if (vec_num(&a->states) <= s->id) {
    vec_resize(&a->states, s->id+1, (vec_data_t)0);
  }
  vec_set(&a->states, s->id, (vec_data_t)s);
}

AutomataState automata_get_state(Automata a, BYTE state_id)
{
  LMN_ASSERT(vec_get(a->states, state_id) != 0);
  return (AutomataState)vec_get(&a->states, state_id);
}

atmstate_id_t automata_get_init_state(Automata a)
{
  return a->init_state;
}

void automata_set_init_state(Automata a, atmstate_id_t id)
{
  a->init_state = id;
}

unsigned int automata_propsym_to_id(Automata a, char *prop_name)
{
  unsigned int id;

  if (st_lookup(a->prop_to_id, prop_name, (st_data_t*)&id)) {
    return id;
  } else {
    /* 0から順にIDを付ける */
    unsigned int new_id = st_num(a->prop_to_id);
    char *str = strdup(prop_name);
    st_add_direct(a->prop_to_id, str, (st_data_t)new_id);
    return new_id;
  }
}


/*----------------------------------------------------------------------
 * state
 */

AutomataState atmstate_make(unsigned int id,
                                  BOOL is_accept_state,
                                  BOOL is_end_state)
{
  AutomataState s = LMN_MALLOC(struct AutomataState);

  vec_init(&s->transitions, 16);
  s->id = id;
  s->is_accept = is_accept_state;
  s->is_end = is_end_state;
  return s;
}

static void automata_state_free(AutomataState s)
{
  unsigned int i;

  for (i = 0; i < vec_num(&s->transitions); i++) {
    transition_free((Transition)vec_get(&s->transitions, i));
  }
  vec_destroy(&s->transitions);
  LMN_FREE(s);
}

void atmstate_add_transition(AutomataState s, Transition t)
{
  vec_push(&s->transitions, (vec_data_t)t);
}

atmstate_id_t atmstate_id(AutomataState s)
{
  return s->id;
}

unsigned int atmstate_transition_num(AutomataState s)
{
  return vec_num(&s->transitions);
}

Transition atmstate_get_transition(AutomataState s, unsigned int index)
{
  return (Transition)vec_get(&s->transitions, index);
}

BOOL atmstate_is_accept(AutomataState s)
{
  return s->is_accept;
}

BOOL atmstate_is_end(AutomataState s)
{
  return s->is_end;
}


/*----------------------------------------------------------------------
 * transition
 */

Transition transition_make(unsigned int next, PLFormula f)
{
  Transition t = LMN_MALLOC(struct Transition);

  t->next = next;
  t->f = f;
  return t;
}

void transition_free(Transition t)
{
  free_formula(t->f);
  LMN_FREE(t);
}

BYTE transition_next(Transition t)
{
  return t->next;
}

PLFormula transition_get_formula(Transition t)
{
  return t->f;
}


/*----------------------------------------------------------------------
 * never claim
 */

int ncparse(yyscan_t, Automata);

/* 正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
static int nc_parse(FILE *in, Automata *automata)
{
  int r;
  yyscan_t scanner;
  
  *automata = automata_make();
  
  nclex_init(&scanner);
  ncset_in(in, scanner);
  r = ncparse(scanner, *automata);
  nclex_destroy(scanner);

  return r;
}

int never_claim_load(FILE *f, Automata *a)
{
  return nc_parse(f, a);
}


/*----------------------------------------------------------------------
 * propositional logic formula
 */

enum PLNode {N_AND, N_OR, N_NEGATION, N_SYMBOL, N_TRUE, N_FALSE};
typedef enum PLNode PLNode;

struct PLFormula {
  PLNode node_type;

  /* 式の構成に必要なデータ */
  PLFormula arg0;       /* for AND,OR,NEGATION */
  PLFormula arg1;       /* for AND,OR */
  unsigned int sym_id;  /* for SYMBOL */
};

static PLFormula make_unary_op(PLNode node_type, PLFormula f0)
{
  PLFormula f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  f->arg0 = f0;
  return f;
}

static PLFormula make_binary_op(PLNode node_type, PLFormula f0, PLFormula f1)
{
  PLFormula f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  f->arg0 = f0;
  f->arg1 = f1;
  return f;
}

void free_formula(PLFormula f)
{
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

static PLFormula ltl_formula_make(PLNode node_type)
{
  PLFormula f = LMN_MALLOC(struct PLFormula);

  f->node_type = node_type;
  return f;
}

PLFormula true_node_make()
{
  return ltl_formula_make(N_TRUE);
}

PLFormula false_node_make()
{
  return ltl_formula_make(N_FALSE);
}

PLFormula sym_node_make(int sym_id)
{
  PLFormula f = ltl_formula_make(N_SYMBOL);

  f->sym_id = sym_id;
  return f;
}

PLFormula negation_node_make(PLFormula f0)
{
  return make_unary_op(N_NEGATION, f0);
}

PLFormula and_node_make(PLFormula f0, PLFormula f1)
{
  return make_binary_op(N_AND, f0, f1);
}

PLFormula or_node_make(PLFormula f0, PLFormula f1)
{
  return make_binary_op(N_OR, f0, f1);
}

/* 式fとシンボル定義prop_defsを膜memで評価する */
BOOL eval_formula(LmnMembrane *mem, Vector *prop_defs, PLFormula f)
{
  switch (f->node_type) {
  case N_NEGATION:
    return !eval_formula(mem, prop_defs, f->arg0);
  case N_AND:
    return eval_formula(mem, prop_defs, f->arg0) && eval_formula(mem, prop_defs, f->arg1);
  case N_OR:
    return eval_formula(mem, prop_defs, f->arg0) || eval_formula(mem, prop_defs, f->arg1);
  case N_TRUE: return TRUE;
  case N_FALSE: return FALSE;
  case N_SYMBOL:
    return proposition_eval(propsym_get_proposition(propsyms_get(prop_defs, f->sym_id)), mem);
  default:
    lmn_fatal("implementation error");
  }
}
