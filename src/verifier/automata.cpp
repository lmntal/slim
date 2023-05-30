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

#include "fmt/core.h"

#include "lmntal.h"
#include "nc_lexer.hpp"
#include "nc_parser.hpp"
#include "propositional_symbol.h"
#include <string_view>

static void automata_analysis_dfs1(AutomataRef a, BYTE *on_stack_list, AutomataStateRef s);
static void automata_analysis_dfs2(AutomataRef a, AutomataStateRef s);

/*----------------------------------------------------------------------
 * automata
 */

Automata::Automata() {
  this->states.reserve(32);
  this->sccs.reserve(4);
  this->prop_num   = 0;
  this->init_state = 0; /* デフォルトでは最初の状態が初期状態 */
}

Automata::~Automata() {
  unsigned int i;

  /* free states */
  for (i = 0; i < this->states.size(); i++) {
    delete this->states.at(i);
  }

  /* free sccs */
  for (i = 0; i < this->sccs.size(); i++) {
    delete this->sccs.at(i);
  }
}

atmstate_id_t Automata::state_id(std::string_view state_name) {
  st_data_t id;

  if (auto it = this->state_name_to_id2.find(state_name); it != this->state_name_to_id2.end()) {
    return it->second;
  }
  /* 0から順にIDを付ける */
  auto new_id = static_cast<int>(this->state_name_to_id2.size());
  auto str0   = std::string{state_name};
  auto str1   = std::string{state_name};

  this->state_name_to_id2[str0]   = new_id;
  this->id_to_state_name2[new_id] = str1;
  return new_id;
}

std::string_view Automata::state_name(atmstate_id_t id) const {
  static std::string empty;
  if (auto it = this->id_to_state_name2.find(id); it != this->id_to_state_name2.end()) {
    return it->second;
  }
  lmn_fatal("implementation error\n");
  return empty;
}

void Automata::add_state(AutomataStateRef s) {
  if (this->states.size() <= s->id) {
    this->states.resize(s->id + 1);
  }
  this->states[s->id] = s;
}

AutomataStateRef Automata::get_state(atmstate_id_t state_id) const {
  LMN_ASSERT(this->states.at(state_id) != nullptr);
  return (AutomataStateRef)this->states.at(state_id);
}

atmstate_id_t Automata::get_init_state() const { return this->init_state; }

void Automata::set_init_state(atmstate_id_t id) { this->init_state = id; }

unsigned int Automata::propsym_to_id(std::string_view prop_name) {
  if (auto it = this->prop_to_id2.find(prop_name); it != this->prop_to_id2.end()) {
    return it->second;
  }

  // 0から順にIDを付ける
  auto new_id = static_cast<unsigned int>(this->prop_to_id2.size());
  auto str    = std::string{prop_name};

  this->prop_to_id2[str] = new_id;
  return new_id;
}

/* for debug only
 * 通常の状態遷移グラフと同様の形式で性質オートマトンを出力する.
 * そのままlavitに喰わせて解析することが目的 */
void Automata::print_property() const {
  fprintf(stdout, "States\n");
  auto n = this->states.size();

  for (auto i = 0; i < n; i++) {
    AutomataStateRef s = this->get_state(i);
    fmt::print("{}::{}{{scc(id={}, name={})}}.\n", (unsigned long)s->get_id(), this->state_name(i),
               s->get_scc()->get_id(), s->get_scc()->get_name());
  }

  fprintf(stdout, "\nTransitions\n");
  auto *init = this->get_state((unsigned int)this->get_init_state());
  fprintf(stdout, "init:%lu\n", (unsigned long)init->get_id());
  for (auto i = 0; i < n; i++) {
    AutomataStateRef s;
    unsigned long    j, m;

    s = this->get_state(i);
    fprintf(stdout, "%lu::", (unsigned long)s->get_id());
    m = s->get_transition_num();
    for (j = 0; j < m; j++) {
      fprintf(stdout, "%lu", (unsigned long)this->get_state(s->get_transition(j)->get_next())->get_id());
      if (j + 1 < m)
        fprintf(stdout, ",");
    }
    fprintf(stdout, "\n");
  }
}

/*----------------------------------------------------------------------
 * state
 */

AutomataState::AutomataState(unsigned int id, bool is_accept_state, bool is_end_state)
    : id(id), is_accept(is_accept_state), is_end(is_end_state) {
  this->transitions.reserve(16);
}

AutomataState::~AutomataState() {
  unsigned int i;

  for (i = 0; i < this->transitions.size(); i++) {
    delete (AutomataTransitionRef)this->transitions.at(i);
  }
}

void AutomataState::add_transition(AutomataTransitionRef t) { this->transitions.push_back(t); }

atmstate_id_t AutomataState::get_id() const { return this->id; }

unsigned int AutomataState::get_transition_num() const { return this->transitions.size(); }

AutomataTransitionRef AutomataState::get_transition(unsigned int index) const {
  return (AutomataTransitionRef)this->transitions.at(index);
}

bool AutomataState::get_is_accept() const { return this->is_accept; }

bool AutomataState::get_is_end() const { return this->is_end; }

void inline AutomataState::set_scc(AutomataSCC *scc) { this->scc = scc; }

BYTE AutomataState::scc_type() const { return this->get_scc()->get_type(); }

AutomataSCC inline *AutomataState::get_scc() const { return this->scc; }

/*----------------------------------------------------------------------
 * SCC analysis for property automata
 */
unsigned int AutomataSCC::unsafe_id_counter = 0;
/* 処理系にロードした性質オートマトンaを解析し, SCC IDなどを追加する */
void Automata::analysis() {
  AutomataStateRef init_s;
  BYTE            *on_stack_list;

  LMN_ASSERT(!this->states.empty());
  init_s                                        = this->get_state((unsigned int)this->get_init_state());
  on_stack_list                                 = LMN_CALLOC<BYTE>(this->states.size());
  on_stack_list[(unsigned int)init_s->get_id()] = 0xffU;

  automata_analysis_dfs1(this, on_stack_list, init_s);
  LMN_FREE(on_stack_list);
}

/* for debug */
char const *AutomataSCC::get_name() const {
  char const *ret = nullptr;
  switch (this->get_type()) {
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

/* dfs postorder順を求め, postorder順に2nd DFSを行う.
 * 性質頂点に, SCC-TYPEを割り当てる.
 * 真面目に書いてないのでFullyとPartiallyの判定が間違っている気がする. */
static void automata_analysis_dfs1(AutomataRef a, BYTE *on_stack_list, AutomataStateRef s) {
  unsigned long i, n;

  n = s->get_transition_num();
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = a->get_state(s->get_transition(i)->get_next());
    if (!on_stack_list[(unsigned int)succ->get_id()]) {
      on_stack_list[(unsigned int)succ->get_id()] = 0xffU;
      automata_analysis_dfs1(a, on_stack_list, succ);
    }
  }

  if (!s->get_scc()) { /* entering 2nd dfs */
    auto *scc = new AutomataSCC();
    scc->issue_id();
    s->set_scc(scc);
    a->sccs.push_back(scc);
    if (s->get_is_end()) {
      scc->set_type(SCC_TYPE_NON_ACCEPT);
    } else {
      if (s->get_is_accept()) {
        scc->set_type(SCC_TYPE_FULLY);
      } else {
        scc->set_type(SCC_TYPE_NON_ACCEPT);
      }
      automata_analysis_dfs2(a, s);
    }
  }
}

static void automata_analysis_dfs2(AutomataRef a, AutomataStateRef s) {
  unsigned long i, n;
  n = s->get_transition_num();
  for (i = 0; i < n; i++) {
    AutomataStateRef succ = a->get_state(s->get_transition(i)->get_next());
    if (!succ->get_scc()) {
      AutomataSCC *scc = s->get_scc();
      if ((!succ->get_is_accept() && scc->get_type() == SCC_TYPE_FULLY) ||
          (succ->get_is_accept() && scc->get_type() == SCC_TYPE_NON_ACCEPT)) {
        scc->set_type(SCC_TYPE_PARTIALLY);
      }
      succ->set_scc(scc);
      automata_analysis_dfs2(a, succ);
    }
  }
}

/*----------------------------------------------------------------------
 * transition
 */

AutomataTransition::AutomataTransition(atmstate_id_t next, PLFormulaRef f) : next(next), f(f) {}

AutomataTransition::~AutomataTransition() { free_formula(this->f); }

atmstate_id_t AutomataTransition::get_next() const { return this->next; }

PLFormulaRef AutomataTransition::get_formula() const { return this->f; }

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

struct PLFormula {
  PLNode node_type;

  /* 式の構成に必要なデータ */
  PLFormulaRef arg0;   /* for AND,OR,NEGATION */
  PLFormulaRef arg1;   /* for AND,OR */
  unsigned int sym_id; /* for SYMBOL */
};

static PLFormulaRef make_unary_op(PLNode node_type, PLFormulaRef f0) {
  auto *f = LMN_MALLOC<struct PLFormula>();

  f->node_type = node_type;
  f->arg0      = f0;
  return f;
}

static PLFormulaRef make_binary_op(PLNode node_type, PLFormulaRef f0, PLFormulaRef f1) {
  auto *f = LMN_MALLOC<struct PLFormula>();

  f->node_type = node_type;
  f->arg0      = f0;
  f->arg1      = f1;
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
  auto *f = LMN_MALLOC<struct PLFormula>();

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

PLFormulaRef negation_node_make(PLFormulaRef f0) { return make_unary_op(N_NEGATION, f0); }

PLFormulaRef and_node_make(PLFormulaRef f0, PLFormulaRef f1) { return make_binary_op(N_AND, f0, f1); }

PLFormulaRef or_node_make(PLFormulaRef f0, PLFormulaRef f1) { return make_binary_op(N_OR, f0, f1); }

/* 式fとシンボル定義prop_defsを膜memで評価する */
BOOL eval_formula(LmnMembraneRef mem, Vector *prop_defs, PLFormulaRef f) {
  switch (f->node_type) {
  case N_NEGATION:
    return !eval_formula(mem, prop_defs, f->arg0);
  case N_AND:
    return eval_formula(mem, prop_defs, f->arg0) && eval_formula(mem, prop_defs, f->arg1);
  case N_OR:
    return eval_formula(mem, prop_defs, f->arg0) || eval_formula(mem, prop_defs, f->arg1);
  case N_TRUE:
    return TRUE;
  case N_FALSE:
    return FALSE;
  case N_SYMBOL:
    return proposition_eval(propsym_get_proposition(propsyms_get(prop_defs, f->sym_id)), mem);
  default:
    lmn_fatal("implementation error");
  }
}
