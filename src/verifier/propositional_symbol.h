/*
 * propositional_symbol.h - Propositional symbol definition
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

#ifndef LMN_PROP_DEFINITION_H
#define LMN_PROP_DEFINITION_H

/**
 * @ingroup  Verifier
 * @defgroup Proposition
 * @{
 */

#include <stdio.h>
#include "element/element.h"
#include "automata.h"
#include "vm/vm.h"

typedef struct SymbolDefinition *SymbolDefinitionRef;
typedef struct Proposition *PropositionRef;
typedef Vector *PropSyms;

/* propositional symbol definition */
 
SymbolDefinitionRef propsym_make(unsigned int sym_id, PropositionRef p);
void propsym_free(SymbolDefinitionRef s);
unsigned int propsym_symbol_id(SymbolDefinitionRef s);
int propsym_load_file(FILE *in, AutomataRef a, PropSyms *propsyms);
void propsym_dump(SymbolDefinitionRef s);
PropositionRef propsym_get_proposition(SymbolDefinitionRef s);

/* proposition */

PropositionRef proposition_make(const char *head,
                             const char *guard,
                             const char *body);
void proposition_free(PropositionRef p);
LmnRuleRef proposition_get_rule(PropositionRef p);
BOOL proposition_eval(PropositionRef prop, LmnMembraneRef mem);
  
/* propositional symbol definitions */

PropSyms propsyms_make(void);
void propsyms_set(PropSyms props,
                      unsigned int id,
                      SymbolDefinitionRef symdef);
unsigned int propsyms_num(PropSyms props);
SymbolDefinitionRef propsyms_get(PropSyms props, unsigned int i);
void propsyms_free(PropSyms props);

/* @} */

#endif
