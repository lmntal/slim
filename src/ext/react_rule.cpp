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

#include <algorithm>
#include <iostream>
#include <thread>
#include <mutex>
// #include "vm/atom.h"
#include "vm/functor.h"
void cb_react_rule(
    LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef rule_mem_proxy,
    LmnLinkAttr rule_mem_proxy_link_attr, LmnAtomRef graph_mem_proxy,
    LmnLinkAttr graph_mem_proxy_link_attr, LmnAtomRef return_rule_mem_proxy,
    LmnLinkAttr return_rule_mem_proxy_link_attr, LmnAtomRef react_judge_atom,
    LmnLinkAttr react_judge_link_attr) {
  LmnMembraneRef rule_mem = LMN_PROXY_GET_MEM(
      (LmnSymbolAtomRef)((LmnSymbolAtomRef)rule_mem_proxy)->get_link(0));
  LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM(
      (LmnSymbolAtomRef)((LmnSymbolAtomRef)graph_mem_proxy)->get_link(0));
  LmnRuleSetRef rs = rule_mem->get_rulesets()[0];
  auto r = rs->get_rule(0);
  MemReactContext tmp_rc(nullptr);

  int reacted = Task::react_rule(&tmp_rc, graph_mem, r);
  lmn_interned_str str = (reacted) ? lmn_intern("success") : lmn_intern("fail");
  LmnSymbolAtomRef result =
      lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS, str, 2));

  lmn_mem_newlink(mem, result, LMN_ATTR_MAKE_LINK(0), 0, graph_mem_proxy,
                  graph_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));
  lmn_mem_newlink(mem, react_judge_atom, react_judge_link_attr,
                  LMN_ATTR_GET_VALUE(react_judge_link_attr), result,
                  LMN_ATTR_MAKE_LINK(0), 1);
  lmn_mem_newlink(mem, return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
                  rule_mem_proxy, rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));
}

/**
 * apply rules in rulesets by one-step.
 *
 * the reacted graphs are added to {\c pos} of the list {\c head}.
 */
template <typename C>
static void apply_rules_in_rulesets(LmnMembraneRef mem,
                                    LmnMembraneRef src_graph, C *rulesets,
                                    LmnSymbolAtomRef *head, int *pos) {
  for (auto &rs : *rulesets) {
    for (auto r : *rs) {
      MCReactContext rc(src_graph);
      rc.keep_process_id_in_nd_mode = true;
      Task::react_rule(&rc, src_graph, r);
      auto &states = rc.expanded_states();
      int n_of_results = rc.expanded_states().size();

      std::for_each(states.rbegin(), states.rend(), [=](void *v) {
        LmnSymbolAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
        LmnMembraneRef m = (LmnMembraneRef)v;
        LmnSymbolAtomRef in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
        LmnSymbolAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        LmnSymbolAtomRef plus = lmn_mem_newatom(m, LMN_UNARY_PLUS_FUNCTOR);
        mem->add_child_mem(m);
        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(in, 1, plus, 0);
        lmn_newlink_in_symbols(out, 1, cons, 0);
        lmn_newlink_in_symbols(cons, 1, *head, *pos);
        *head = cons;
        *pos = 2;
      });
    }
  }
}

void cb_react_ruleset_nd(
    LmnReactCxtRef &rc, LmnMembraneRef mem, LmnAtomRef rule_mem_proxy,
    LmnLinkAttr rule_mem_proxy_link_attr, LmnAtomRef graph_mem_proxy,
    LmnLinkAttr graph_mem_proxy_link_attr, LmnAtomRef return_rule_mem_proxy,
    LmnLinkAttr return_rule_mem_proxy_link_attr, LmnAtomRef react_judge_atom,
    LmnLinkAttr react_judge_link_attr) {
  LmnMembraneRef rule_mem = LMN_PROXY_GET_MEM(
      (LmnSymbolAtomRef)((LmnSymbolAtomRef)rule_mem_proxy)->get_link(0));
  LmnAtomRef in_mem = ((LmnSymbolAtomRef)graph_mem_proxy)->get_link(0);
  LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in_mem);

  lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in_mem)->get_link(1),
                      ((LmnSymbolAtomRef)in_mem)->get_attr(1));
  lmn_mem_delete_atom(graph_mem, in_mem,
                      ((LmnSymbolAtomRef)graph_mem_proxy)->get_attr(0));

  LmnSymbolAtomRef head = lmn_mem_newatom(mem, LMN_NIL_FUNCTOR);
  int pos = 0;

  auto rulesets = &rule_mem->get_rulesets();
  apply_rules_in_rulesets(mem, graph_mem, rulesets, &head, &pos);

#ifdef USE_FIRSTCLASS_RULE
  auto fstclass_rules = &rule_mem->get_firstclass_rulesets();
  apply_rules_in_rulesets(mem, graph_mem, fstclass_rules, &head, &pos);
#endif

  lmn_mem_newlink(mem, head, LMN_ATTR_MAKE_LINK(pos), pos, react_judge_atom,
                  react_judge_link_attr,
                  LMN_ATTR_GET_VALUE(react_judge_link_attr));

  mem->remove_mem(graph_mem);

  lmn_mem_newlink(mem, return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
                  rule_mem_proxy, rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr);
}
// template <typename C>
std::mutex react_mtx;
static void apply_rules_para(unsigned int id,
                             std::vector<LmnRuleSet *> *rulesets,
                             std::vector<LmnMembraneRef> *mems,
                             std::vector<std::vector<LmnMembraneRef>> *ret,
                             int begin, int end) {
  //std::lock_guard<std::mutex> lock(react_mtx);
  if (lmn_env.normal_para) {
    env_my_TLS_init(id);
    lmn_thread_set_CPU_affinity(id);
  }
  // std::cout << "id: "<<id << std::endl;
  // std::cout << "pool size "<< id <<": "<<lmn_id_pool[lmn_tls.thread_id].size() << std::endl;
  for (int i = begin; i < end; i++) {
    for (auto &rs : *rulesets) {
      for (auto r : *rs) {
        MCReactContext rc(mems->at(i));
        rc.keep_process_id_in_nd_mode = true;
        Task::react_rule(&rc, mems->at(i), r);
        auto &states = rc.expanded_states();
        int n_of_results = rc.expanded_states().size();
        for (int j = 0; j < n_of_results; j++) {
          ret->at(i).push_back((LmnMembraneRef)states[j]);
        }
      }
    }
  }
  delete rulesets;
}

void cb_react_ruleset_nd_para(LmnReactCxtRef &rc, LmnMembraneRef mem,
                              LmnAtomRef rule_mem_proxy,
                              LmnLinkAttr rule_mem_proxy_link_attr,
                              LmnAtomRef cons, LmnLinkAttr cons_link_attr,
                              LmnAtomRef n_of_list, LmnLinkAttr n_of_list_attr,
                              LmnAtomRef return_rule_mem_proxy,
                              LmnLinkAttr return_rule_mem_proxy_link_attr,
                              LmnAtomRef react_judge_atom,
                              LmnLinkAttr react_judge_link_attr) {
  long long n = (long long)n_of_list;
  //std::cout << n << std::endl;
  std::vector<LmnMembraneRef> mems(n);

  std::vector<std::vector<LmnMembraneRef>> ret(n, std::vector<LmnMembraneRef>());

  std::vector<std::thread> threads;

  LmnMembraneRef rule_mem = LMN_PROXY_GET_MEM(
      (LmnSymbolAtomRef)((LmnSymbolAtomRef)rule_mem_proxy)->get_link(0));
  LmnAtomRef it = cons;
  if (n > 1) {
    // 2 para
    for (int i = 0; i < n / 2; i++) {
      LmnAtomRef out = ((LmnSymbolAtomRef)it)->get_link(0);
      LmnAtomRef in = ((LmnSymbolAtomRef)out)->get_link(0);
      LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
      // delete plus
      lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in)->get_link(1),
                          ((LmnSymbolAtomRef)in)->get_attr(1));
      // delte in
      lmn_mem_delete_atom(graph_mem, in, ((LmnSymbolAtomRef)out)->get_attr(0));
      // delete out
      lmn_mem_delete_atom(mem, out, ((LmnSymbolAtomRef)it)->get_attr(0));
      mems[i] = graph_mem;
      it = ((LmnSymbolAtomRef)it)->get_link(1);
    }
    if (lmn_env.normal_para)
      threads.push_back(std::thread(apply_rules_para, 1, new std::vector<LmnRuleSet *>(rule_mem->get_rulesets()), &mems, &ret, 0, n/2));
    else
      apply_rules_para(1, new std::vector<LmnRuleSet *>(rule_mem->get_rulesets()), &mems, &ret, 0, n/2);
    for (int i = n / 2; i < n; i++) {
      LmnAtomRef out = ((LmnSymbolAtomRef)it)->get_link(0);
      LmnAtomRef in = ((LmnSymbolAtomRef)out)->get_link(0);
      LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
      // delete plus
      lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in)->get_link(1),
                          ((LmnSymbolAtomRef)in)->get_attr(1));
      // delte in
      lmn_mem_delete_atom(graph_mem, in, ((LmnSymbolAtomRef)out)->get_attr(0));
      // delete out
      lmn_mem_delete_atom(mem, out, ((LmnSymbolAtomRef)it)->get_attr(0));
      mems[i] = graph_mem;
      it = ((LmnSymbolAtomRef)it)->get_link(1);
    }
    if (lmn_env.normal_para)
      threads.push_back(std::thread(apply_rules_para, 2, new std::vector<LmnRuleSet *>(rule_mem->get_rulesets()), &mems, &ret, n/2, n));
    else
      apply_rules_para(2, new std::vector<LmnRuleSet *>(rule_mem->get_rulesets()), &mems, &ret, n/2, n);

    if (lmn_env.normal_para) {
      threads[0].join();
      threads[1].join();
    }
    it = cons;

    for (int i = 0; i < n; i++) {
      LmnSymbolAtomRef head = lmn_mem_newatom(mem, LMN_NIL_FUNCTOR);
      int pos = 0;

      for (auto j = ret[i].rbegin(); j!=ret[i].rend(); j++ ) {
	LmnSymbolAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
	LmnMembraneRef m = (LmnMembraneRef)*j;
	LmnSymbolAtomRef in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
	LmnSymbolAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
	LmnSymbolAtomRef plus = lmn_mem_newatom(m, LMN_UNARY_PLUS_FUNCTOR);
	mem->add_child_mem(m);
	lmn_newlink_in_symbols(in, 0, out, 0);
	lmn_newlink_in_symbols(in, 1, plus, 0);
	lmn_newlink_in_symbols(out, 1, cons, 0);
	lmn_newlink_in_symbols(cons, 1, head, pos);
	head = cons;
	pos = 2;
      }
      lmn_mem_newlink(mem, head, LMN_ATTR_MAKE_LINK(pos), pos, it,
                      LMN_ATTR_MAKE_LINK(0), 0);
      it = ((LmnSymbolAtomRef)it)->get_link(1);
      mem->remove_mem(mems[i]);
    }
  } else {
    while (((LmnSymbolAtomRef)it)->get_functor() != LMN_NIL_FUNCTOR) {
      LmnSymbolAtomRef head = lmn_mem_newatom(mem, LMN_NIL_FUNCTOR);
      int pos = 0;
      LmnAtomRef out = ((LmnSymbolAtomRef)it)->get_link(0);
      LmnAtomRef in = ((LmnSymbolAtomRef)out)->get_link(0);
      LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
      // delete plus
      lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in)->get_link(1),
                          ((LmnSymbolAtomRef)in)->get_attr(1));
      // delte in
      lmn_mem_delete_atom(graph_mem, in, ((LmnSymbolAtomRef)out)->get_attr(0));
      // delete out
      lmn_mem_delete_atom(mem, out, ((LmnSymbolAtomRef)it)->get_attr(0));

      apply_rules_in_rulesets(mem, graph_mem, &rule_mem->get_rulesets(), &head,
                              &pos);
#ifdef USE_FIRSTCLASS_RULE
      auto fstclass_rules = &rule_mem->get_firstclass_rulesets();
      apply_rules_in_rulesets(mem, graph_mem, fstclass_rules, &head, &pos);
#endif

      lmn_mem_newlink(mem, head, LMN_ATTR_MAKE_LINK(pos), pos, it,
                      LMN_ATTR_MAKE_LINK(0), 0);
      it = ((LmnSymbolAtomRef)it)->get_link(1);
      mem->remove_mem(graph_mem);
    }
  }

  lmn_mem_newlink(mem, cons, cons_link_attr, LMN_ATTR_GET_VALUE(cons_link_attr),
                  react_judge_atom, react_judge_link_attr,
                  LMN_ATTR_GET_VALUE(react_judge_link_attr));

  lmn_mem_newlink(mem, return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
                  rule_mem_proxy, rule_mem_proxy_link_attr,
                  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));
}

void init_react_rule(void) {
  CCallback::lmn_register_c_fun("cb_react_rule", (void *)cb_react_rule, 4);
  CCallback::lmn_register_c_fun("cb_react_ruleset_nd",
                                (void *)cb_react_ruleset_nd, 4);
  CCallback::lmn_register_c_fun("cb_react_ruleset_nd_para",
                                (void *)cb_react_ruleset_nd_para, 5);
}
