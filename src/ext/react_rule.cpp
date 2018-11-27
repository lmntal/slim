/*
 * react_rule.c
 *
 *   Copyright (c) 2017, Ueda Laboratory LMNtal Group
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

#include "vm/vm.h"


void cb_react_rule(LmnReactCxtRef rc,
                   LmnMembraneRef mem,
                   LmnAtomRef rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
                   LmnAtomRef graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
                   LmnAtomRef return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
                   LmnAtomRef react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembraneRef rule_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rule_mem_proxy, 0));
  LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)graph_mem_proxy, 0));
  LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(lmn_mem_get_rulesets(rule_mem), 0);
  auto r = rs->get_rule(0);
  MemReactContext tmp_rc;

  int reacted = react_rule(&tmp_rc, graph_mem, r);
  lmn_interned_str str = (reacted) ? lmn_intern("success") : lmn_intern("fail");
  LmnSymbolAtomRef result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, str, 2));

  lmn_mem_newlink(mem,
                  result, LMN_ATTR_MAKE_LINK(0), 0,
                  graph_mem_proxy, graph_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));
  lmn_mem_newlink(mem,
                  react_judge_atom, react_judge_link_attr,
                  LMN_ATTR_GET_VALUE(react_judge_link_attr),
                  result, LMN_ATTR_MAKE_LINK(0), 1);
  lmn_mem_newlink(mem,
                  return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
                  rule_mem_proxy, rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));
}

/**
 * apply rules in rulesets by one-step.
 *
 * the reacted graphs are added to {\c pos} of the list {\c head}.
 */
static void apply_rules_in_rulesets(LmnMembraneRef mem,
                                    LmnMembraneRef src_graph, Vector *rulesets,
                                    LmnSymbolAtomRef *head, int *pos)
{
  for (int i = 0; i < vec_num(rulesets); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(rulesets, i);

    for (auto r : *rs) {
      MCReactContext rc;
      RC_SET_GROOT_MEM(&rc, src_graph);
      rc.keep_process_id_in_nd_mode = true;
      react_rule(&rc, src_graph, r);
      int n_of_results = vec_num(RC_EXPANDED(&rc));

      for (int k = n_of_results - 1; k >= 0; k--) {
        LmnSymbolAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
        LmnMembraneRef m = (LmnMembraneRef)vec_get(RC_EXPANDED(&rc), k);
        LmnSymbolAtomRef in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR); 
        LmnSymbolAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        LmnSymbolAtomRef plus = lmn_mem_newatom(m, LMN_UNARY_PLUS_FUNCTOR);
        lmn_mem_add_child_mem(mem, m);
        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(in, 1, plus, 0);
        lmn_newlink_in_symbols(out, 1, cons, 0);
        lmn_newlink_in_symbols(cons, 1, *head, *pos);
        *head = cons;
        *pos = 2;
      }
    }
  }
}

void cb_react_ruleset_nd(LmnReactCxtRef &rc,
                         LmnMembraneRef mem,
                         LmnAtomRef rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
                         LmnAtomRef graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
                         LmnAtomRef return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
                         LmnAtomRef react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembraneRef rule_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rule_mem_proxy, 0));
  LmnAtomRef in_mem = LMN_SATOM_GET_LINK((LmnSymbolAtomRef)graph_mem_proxy, 0);
  LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in_mem);

  lmn_mem_delete_atom(graph_mem, LMN_SATOM_GET_LINK((LmnSymbolAtomRef)in_mem, 1), LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)in_mem, 1));
  lmn_mem_delete_atom(graph_mem, in_mem, LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)graph_mem_proxy, 0));

  LmnSymbolAtomRef head = lmn_mem_newatom(mem, LMN_NIL_FUNCTOR);
  int pos = 0;

  Vector *rulesets = lmn_mem_get_rulesets(rule_mem);
  apply_rules_in_rulesets(mem, graph_mem, rulesets, &head, &pos);

#ifdef USE_FIRSTCLASS_RULE
  Vector *fstclass_rules = lmn_mem_firstclass_rulesets(rule_mem);
  apply_rules_in_rulesets(mem, graph_mem, fstclass_rules, &head, &pos);
#endif

  lmn_mem_newlink(mem, head, LMN_ATTR_MAKE_LINK(pos), pos,
                  react_judge_atom, react_judge_link_attr,
                  LMN_ATTR_GET_VALUE(react_judge_link_attr));

  lmn_mem_remove_mem(mem, graph_mem);

  lmn_mem_newlink(mem,
                  return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
                  rule_mem_proxy, rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr); 
}



void init_react_rule(void)
{
  lmn_register_c_fun("cb_react_rule", (void *)cb_react_rule, 4);
  lmn_register_c_fun("cb_react_ruleset_nd", (void *)cb_react_ruleset_nd, 4);
}
