/*
 * propositional_symbol.cpp - Propositional symbol definition
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

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif
extern "C" {
#include "propositional_symbol.h"
#include "../lmntal.h"
#include "propsym_parser.h"
#include "propsym_lexer.h"
#include "ffi/lmntal_system_adapter.h"
#include "loader/loader.h"
}
struct SymbolDefinition {
  unsigned int sym_id;
  PropositionRef prop;
};

struct Proposition {
  char *head;
  char *guard;
  char *body;
  LmnRuleRef rule;
};

static char *rule_str_for_compile(const char *head, const char *guard,
                                  const char *body);
static int propsym_parse(FILE *in, AutomataRef a, PVector *definitions);

PropositionRef proposition_make(const char *head, const char *guard,
                                const char *body) {
  PropositionRef p = LMN_MALLOC(struct Proposition);
  RuleRef rule;
  FILE *fp;
  char *rule_str;
  BYTE optimization_level_org;

  optimization_level_org = lmn_env.optimization_level;
  lmn_env.optimization_level = OPTIMIZE_LEVEL_MAX;

  p->head = strdup(head);
  p->guard = (guard == NULL ? strdup("") : strdup(guard));
  p->body = (body == NULL ? strdup("") : strdup(body));

  rule_str = rule_str_for_compile(head, guard, body);

  fp = lmntal_compile_rule_str(rule_str);
  LMN_FREE(rule_str);

  lmn_env.optimization_level = optimization_level_org;

  if (!il_parse_rule(fp, &rule)) {
    p->rule = load_rule(rule);
    stx_rule_free(rule);
    fclose(fp);
  } else {
    lmn_fatal("Implementation Error: failed to compile rule");
  }
  return p;
}

void proposition_free(PropositionRef p) {
  LMN_FREE(p->head);
  LMN_FREE(p->guard);
  LMN_FREE(p->body);
  lmn_rule_free(p->rule);
  LMN_FREE(p);
}

LmnRuleRef proposition_get_rule(PropositionRef p) { return p->rule; }

static char *rule_str_for_compile(const char *head, const char *guard,
                                  const char *body) {
  char *buf;
  const char *head_s = head;
  const char *guard_s = (guard == NULL ? "" : guard);
  const char *body_s = (body == NULL ? head : body);

  buf =
      LMN_NALLOC(char, strlen(head_s) + strlen(guard_s) + strlen(body_s) + 32);
  buf[0] = '\0';
  strcat(buf, head_s);
  strcat(buf, " :- ");
  strcat(buf, guard_s);
  strcat(buf, " | ");
  strcat(buf, body_s);
  strcat(buf, ".");

  return buf;
}

/*----------------------------------------------------------------------
 * propositional symbol definition
 */

SymbolDefinitionRef propsym_make(unsigned int sym_id, PropositionRef p) {
  SymbolDefinitionRef s = LMN_MALLOC(struct SymbolDefinition);

  s->sym_id = sym_id;
  s->prop = p;
  return s;
}

void propsym_free(SymbolDefinitionRef s) {
  if (s) {
    proposition_free(s->prop);
    LMN_FREE(s);
  }
}

unsigned int propsym_symbol_id(SymbolDefinitionRef s) { return s->sym_id; }

extern "C" int propsymparse(yyscan_t, AutomataRef, Vector **);

int propsym_parse(FILE *in, AutomataRef a, PVector *definitions) {
  int r;
  yyscan_t scanner;

  propsymlex_init(&scanner);
  propsymset_extra(NULL, scanner);
  propsymset_in(in, scanner);
  r = propsymparse(scanner, a, definitions);
  propsymlex_destroy(scanner);

  return r;
}

/* 正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
int propsym_load_file(FILE *in, AutomataRef a, Vector **definitions) {
  return propsym_parse(in, a, definitions);
}

void propsym_dump(SymbolDefinitionRef s) {
  fprintf(stdout, "%d := %s | %s. \n", s->sym_id, s->prop->head,
          s->prop->guard);
}

PropositionRef propsym_get_proposition(SymbolDefinitionRef s) {
  return s ? s->prop : NULL;
}

BOOL proposition_eval(PropositionRef prop, LmnMembraneRef mem) {
  LmnReactCxtRef rc = react_context_alloc();
  BOOL b;

  if (!prop)
    return FALSE;

  property_react_cxt_init(rc);
  RC_SET_GROOT_MEM(rc, mem);
  b = react_rule(rc, mem, proposition_get_rule(prop));
  property_react_cxt_destroy(rc);

  react_context_dealloc(rc);
  return b;
}

/*----------------------------------------------------------------------
 * propositional symbol definitions
 */

PropSyms propsyms_make() { return vec_make(32); }

unsigned int propsyms_num(PropSyms props) { return vec_num(props); }

SymbolDefinitionRef propsyms_get(PropSyms props, unsigned int i) {
  return (SymbolDefinitionRef)vec_get(props, i);
}

void propsyms_free(PropSyms props) {
  unsigned int i;
  for (i = 0; i < vec_num(props); i++) {
    propsym_free((SymbolDefinitionRef)vec_get(props, i));
  }
  vec_free(props);
}

void propsyms_set(PropSyms props, unsigned int id, SymbolDefinitionRef symdef) {
  if (vec_num(props) <= id) {
    vec_resize(props, id + 1, (vec_data_t)NULL);
  }
  vec_set(props, id, (vec_data_t)symdef);
}
