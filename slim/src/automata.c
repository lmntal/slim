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
static void atmstate_free(AutomataState s);
static void atm_transition_free(AutomataTransition t);
static void automata_analysis_dfs1(Automata a, BYTE *on_stack_list, AutomataState s);
static void automata_analysis_dfs2(Automata a, AutomataState s);
inline static unsigned int atmscc_id(AutomataSCC *s);
inline static BYTE atmscc_type(AutomataSCC *s);
inline static void atmscc_set_type(AutomataSCC *s, BYTE type);

struct Automata {
/*   atmstate_id_t init_state; */
  atmstate_id_t  init_state;
  unsigned int   prop_num;
  Vector         states;  /* Vector of AutomataState */
  st_table_t     state_name_to_id;
  st_table_t     id_to_state_name;
  st_table_t     prop_to_id;
  Vector         sccs;
};

struct AutomataState {
  atmstate_id_t  id;
  BOOL           is_accept;
  BOOL           is_end;
  Vector         transitions; /* Vector of Successors (AutomataTransition) */
  AutomataSCC   *scc;
};

struct AutomataTransition {
  unsigned int next;
  PLFormula    f; /* 実際は命題論理式 */
};

struct AutomataSCC {
  unsigned int id;
  BYTE type;
};

/*----------------------------------------------------------------------
 * automata
 */

Automata automata_make()
{
  Automata a = LMN_MALLOC(struct Automata);

  vec_init(&a->states, 32);
  vec_init(&a->sccs, 4);
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
    atmstate_free((AutomataState)vec_get(&a->states, i));
  }

  /* free sccs */
  for (i = 0; i < vec_num(&a->sccs); i++) {
    atmscc_free((AutomataSCC *)vec_get(&a->sccs, i));
  }

  vec_destroy(&a->states);
  vec_destroy(&a->sccs);
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

atmstate_id_t automata_state_id(Automata a, char *state_name)
{
  st_data_t id;

  if (st_lookup(a->state_name_to_id, (st_data_t)state_name, &id)) {
    return id;
  } else {
    /* 0から順にIDを付ける */
    char *str0;
    char *str1;
    int new_id;

    new_id = st_num(a->state_name_to_id);
    str0 = strdup(state_name);
    str1 = strdup(state_name);

    st_add_direct(a->state_name_to_id, (st_data_t)str0, (st_data_t)new_id);
    st_add_direct(a->id_to_state_name, (st_data_t)new_id, (st_data_t)str1);
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

atmstate_id_t automata_state_scc_id(Automata a, atmstate_id_t id)
{
  return atmscc_id(atmstate_scc(automata_get_state(a, id)));
}

const char *automata_state_scc_name(Automata a, atmstate_id_t id)
{
  return atmscc_name(atmstate_scc(automata_get_state(a, id)));
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
  LMN_ASSERT(vec_get(&a->states, state_id) != 0);
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

AutomataState atmstate_make(unsigned int id,
                            BOOL         is_accept_state,
                            BOOL         is_end_state)
{
  AutomataState s = LMN_MALLOC(struct AutomataState);

  vec_init(&s->transitions, 16);
  s->id = id;
  s->is_accept = is_accept_state;
  s->is_end = is_end_state;
  s->scc = NULL;
  return s;
}

static void atmstate_free(AutomataState s)
{
  unsigned int i;

  for (i = 0; i < vec_num(&s->transitions); i++) {
    atm_transition_free((AutomataTransition)vec_get(&s->transitions, i));
  }
  vec_destroy(&s->transitions);
  LMN_FREE(s);
}

void atmstate_add_transition(AutomataState s, AutomataTransition t)
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

AutomataTransition atmstate_get_transition(AutomataState s, unsigned int index)
{
  return (AutomataTransition)vec_get(&s->transitions, index);
}

BOOL atmstate_is_accept(AutomataState s)
{
  return s->is_accept;
}

BOOL atmstate_is_end(AutomataState s)
{
  return s->is_end;
}

inline void atmstate_set_scc(AutomataState s, AutomataSCC *scc)
{
  s->scc = scc;
}

inline BYTE atmstate_scc_type(AutomataState s)
{
  return atmscc_type(atmstate_scc(s));
}

inline AutomataSCC *atmstate_scc(AutomataState s)
{
  return s->scc;
}

/*----------------------------------------------------------------------
 * SCC analysis for property automata
 */
/* 処理系にロードした性質オートマトンaを解析し, SCC IDなどを追加する */
void automata_analysis(Automata a)
{
  AutomataState init_s;
  BYTE *on_stack_list;

  LMN_ASSERT(vec_num(&a->states) > 0);
  init_s = automata_get_state(a, (unsigned int)automata_get_init_state(a));
  on_stack_list = LMN_CALLOC(BYTE, vec_num(&a->states));
  on_stack_list[(unsigned int)atmstate_id(init_s)] = 0xffU;

  automata_analysis_dfs1(a, on_stack_list, init_s);
  //print_property_automata(a);

  LMN_FREE(on_stack_list);
}

char *atmscc_name(AutomataSCC *s)
{
  char *ret = NULL;
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
  case SCC_TYPE_SAFETY:
    ret = "Safety_Accepting";
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

AutomataSCC *atmscc_make()
{
  AutomataSCC *m = LMN_MALLOC(AutomataSCC);
  m->id = 0;
  m->type = SCC_TYPE_UNKNOWN;
  return m;
}

void atmscc_free(AutomataSCC *m)
{
  LMN_FREE(m);
}

/** CAUTION: MT-Unsafe */
inline static void atmscc_issue_id(AutomataSCC *s)
{
  static unsigned int unsafe_id_counter = 0;
  s->id = unsafe_id_counter++;
}

inline static unsigned int atmscc_id(AutomataSCC *s)
{
  return s->id;
}

inline static BYTE atmscc_type(AutomataSCC *s)
{
  return s->type;
}

inline static void atmscc_set_type(AutomataSCC *s, BYTE type)
{
  s->type = type;
}

/* for debug only
 * 通常の状態遷移グラフと同様の形式で性質オートマトンを出力する.
 * そのままlavitに喰わせて解析することが目的 */
void print_property_automata(Automata a)
{
  AutomataState init;
  unsigned long i, n;

  fprintf(stdout, "States\n");
  n = vec_num(&(a->states));

  for (i = 0; i < n; i++) {
    AutomataState s = automata_get_state(a, i);
    fprintf(stdout, "%lu::%s{scc(id=%d, name=%s)}.\n",
                                         (unsigned long)atmstate_id(s),
                                         automata_state_name(a, i),
                                         atmscc_id(atmstate_scc(s)),
                                         atmscc_name(atmstate_scc(s)));
  }

  fprintf(stdout, "\nTransitions\n");
  init = automata_get_state(a, (unsigned int)automata_get_init_state(a));
  fprintf(stdout, "init:%lu\n", (unsigned long)atmstate_id(init));
  for (i = 0; i < n; i++) {
    AutomataState s;
    unsigned long j, m;

    s = automata_get_state(a, i);
    fprintf(stdout, "%lu::", (unsigned long)atmstate_id(s));
    m = atmstate_transition_num(s);
    for (j = 0; j < m; j++) {
      fprintf(stdout, "%lu",
          (unsigned long)atmstate_id(automata_get_state(a, (unsigned int)atm_transition_next(atmstate_get_transition(s, j)))));
      if (j + 1 < m) fprintf(stdout, ",");
    }
    fprintf(stdout, "\n");
  }
}

/* dfs postorder順を求め, postorder順に2nd DFSを行う */
static void automata_analysis_dfs1(Automata a, BYTE *on_stack_list, AutomataState s)
{
  unsigned long i, n;

  n = atmstate_transition_num(s);
  for (i = 0; i < n; i++) {
    AutomataState succ = automata_get_state(a, (unsigned int)atm_transition_next(atmstate_get_transition(s, i)));
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
      atmscc_set_type(scc, SCC_TYPE_SAFETY);
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

/* 性質状態sから到達可能かつSCCオブジェクトを所持しない全ての性質状態に同一のSCCオブジェクトを参照させる */
static void automata_analysis_dfs2(Automata a, AutomataState s)
{
  unsigned long i, n;
  n = atmstate_transition_num(s);
  for (i = 0; i < n; i++) {
    AutomataState succ = automata_get_state(a, (unsigned int)atm_transition_next(atmstate_get_transition(s, i)));
    if (!atmstate_scc(succ)) {
      AutomataSCC *scc = atmstate_scc(s);
      LMN_ASSERT(!atmstate_is_end(succ)); /* postorder順なので, dfs2では訪問されないはず */
      if ((!atmstate_is_accept(succ) && atmscc_type(scc) == SCC_TYPE_FULLY) ||
          ( atmstate_is_accept(succ) && atmscc_type(scc) == SCC_TYPE_NON_ACCEPT)) {
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

AutomataTransition atm_transition_make(unsigned int next, PLFormula f)
{
  AutomataTransition t = LMN_MALLOC(struct AutomataTransition);

  t->next = next;
  t->f = f;
  return t;
}

void atm_transition_free(AutomataTransition t)
{
  free_formula(t->f);
  LMN_FREE(t);
}

BYTE atm_transition_next(AutomataTransition t)
{
  return t->next;
}

PLFormula atm_transition_get_formula(AutomataTransition t)
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
