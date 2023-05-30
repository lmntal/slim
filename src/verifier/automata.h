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

#pragma once
#ifndef LMN_PROP_AUTOMATA
#define LMN_PROP_AUTOMATA

/**
 * @ingroup  Verifier
 * @defgroup Automata
 * @{
 */

#include <string>
#include <string_view>
#include <vector>

#include "ankerl/unordered_dense.hpp"

#include "element/element.h"
#include "lmntal.h"
#include "vm/vm.h"

using AutomataRef           = struct Automata *;
using AutomataStateRef      = struct AutomataState *;
using AutomataTransitionRef = struct AutomataTransition *;
using AutomataSCC           = struct AutomataSCC;

using atmstate_id_t = BYTE; /* 性質ラベル(状態)数は256個まで */

struct string_hash {
  using is_transparent = void; // enable heterogeneous overloads
  using is_avalanching = void; // mark class as high quality avalanching hash

  [[nodiscard]] auto operator()(std::string_view str) const noexcept -> uint64_t {
    return ankerl::unordered_dense::hash<std::string_view>{}(str);
  }
};

template <typename T> using mapStrKey = ankerl::unordered_dense::map<std::string, T, string_hash, std::equal_to<>>;

struct Automata {
  atmstate_id_t                 init_state;
  unsigned int                  prop_num;
  std::vector<AutomataStateRef> states; /* Vector of AutomataState */
  
  using strMap = mapStrKey<unsigned int>;
  using intMap = ankerl::unordered_dense::map<int, std::string>;

  strMap state_name_to_id2{};
  intMap id_to_state_name2{};
  strMap prop_to_id2{};

  std::vector<AutomataSCC *> sccs;

  Automata();
  ~Automata();
  atmstate_id_t    state_id(std::string_view);
  std::string_view state_name(atmstate_id_t) const;
  void             add_state(AutomataStateRef);
  AutomataStateRef get_state(atmstate_id_t) const;
  atmstate_id_t    get_init_state() const;
  void             set_init_state(atmstate_id_t);
  unsigned int     propsym_to_id(std::string_view prop_name);
  void             analysis();
  void             print_property() const;
};

struct AutomataState {
  atmstate_id_t                      id;
  bool                               is_accept;
  bool                               is_end;
  AutomataSCC                       *scc{nullptr};
  std::vector<AutomataTransitionRef> transitions{}; /* Vector of Successors (AutomataTransition) */

  AutomataState(unsigned int, bool, bool);
  ~AutomataState();
  void                  add_transition(AutomataTransitionRef t);
  atmstate_id_t         get_id() const;
  unsigned int          get_transition_num() const;
  AutomataTransitionRef get_transition(unsigned int index) const;
  bool                  get_is_accept() const;
  bool                  get_is_end() const;
  void                  set_scc(AutomataSCC *scc);
  BYTE                  scc_type() const;
  AutomataSCC          *get_scc() const;
};

/* Propositional Logic Formula */
using PLFormulaRef = struct PLFormula *;

struct AutomataTransition {
  atmstate_id_t next;
  PLFormulaRef  f; /* 実際は命題論理式 */

  AutomataTransition(atmstate_id_t next, PLFormulaRef f);
  ~AutomataTransition();
  atmstate_id_t get_next() const;
  PLFormulaRef  get_formula() const;
};

struct AutomataSCC {
private:
  unsigned int        id;
  BYTE                type;
  static unsigned int unsafe_id_counter;

public:
  AutomataSCC();
  ~AutomataSCC() = default;
  char const *get_name() const;
  /** CAUTION: MT-Unsafe */
  void issue_id() {
    id = unsafe_id_counter++;
    // Is it checked by test cases? by sumiya
  }
  unsigned int get_id() const { return id; }
  BYTE         get_type() const { return type; }
  void         set_type(BYTE t) { type = t; }
};

enum SCC_ACCEPTING_TYPE {
  SCC_TYPE_UNKNOWN    = 0U,
  SCC_TYPE_FULLY      = 1U, /* 構成するサイクルが全て受理サイクル */
  SCC_TYPE_PARTIALLY  = 2U, /* 構成するサイクルが非受理サイクルも含む */
  SCC_TYPE_NON_ACCEPT = 3U, /* 受理サイクルを含まない */
};

/* propositional Logic Formula */
PLFormulaRef true_node_make();
PLFormulaRef false_node_make();
PLFormulaRef sym_node_make(int sym_id);
PLFormulaRef negation_node_make(PLFormulaRef f0);
PLFormulaRef and_node_make(PLFormulaRef f0, PLFormulaRef f1);
PLFormulaRef or_node_make(PLFormulaRef f0, PLFormulaRef f1);
void         free_formula(PLFormulaRef f);
BOOL         eval_formula(LmnMembraneRef mem, Vector *prop_defs, PLFormulaRef f);

/* never claim */
int never_claim_load(FILE *f, AutomataRef *a);

/* @} */

#endif /* LMN_PROP_AUTOMATA */
