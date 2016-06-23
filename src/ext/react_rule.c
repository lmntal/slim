#include <stdio.h>
#include "../lmntal_ext.h"
#include "../dumper.h"

void cb_react_rule(LmnReactCxt *rc,
			  LmnMembrane *mem,
			  LmnAtom rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
			  LmnAtom graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
			  LmnAtom return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
			  LmnAtom react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembrane *rule_mem = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(rule_mem_proxy, 0));
  LmnMembrane *graph_mem = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(graph_mem_proxy, 0));
  LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(&(rule_mem->rulesets), 0);
  LmnRuleRef r = lmn_ruleset_get_rule(rs, 0);
  LmnSAtom result;
  LmnReactCxt tmp_rc;
  //    tmp_rc = *rc;
  //    tmp_rc.work_arry = lmn_register_make(rc->warry_cap);
  //    lmn_register_copy(tmp_rc.work_arry, rc->work_arry, rc->warry_num);
  mem_react_cxt_init(&tmp_rc);

  if(react_rule(&tmp_rc, graph_mem, r)) {
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 2));
  } else {
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 2));
  }
  //  lmn_dump_cell_stdout(graph_mem);
  lmn_mem_newlink(mem,
		  LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 0,
		  graph_mem_proxy, graph_mem_proxy_link_attr,
		  LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));
  lmn_mem_newlink(mem,
		  react_judge_atom, react_judge_link_attr,
		  LMN_ATTR_GET_VALUE(react_judge_link_attr),
		  LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 1);
  lmn_mem_newlink(mem,
		  return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
		  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
		  rule_mem_proxy, rule_mem_proxy_link_attr,
		  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  mem_react_cxt_destroy(&tmp_rc);
}

void cb_react_rule_nd(LmnReactCxt *rc,
			     LmnMembrane *mem,
			     LmnAtom rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
			     LmnAtom graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
			     LmnAtom return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
			     LmnAtom react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembrane *rule_mem = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(rule_mem_proxy, 0));
  LmnAtom in_mem = LMN_SATOM_GET_LINK(graph_mem_proxy, 0);
  LmnMembrane *graph_mem = LMN_PROXY_GET_MEM(in_mem);

  lmn_mem_delete_atom(graph_mem, LMN_SATOM_GET_LINK(in_mem, 1), LMN_SATOM_GET_ATTR(in_mem, 1));
  lmn_mem_delete_atom(graph_mem, in_mem, LMN_SATOM_GET_ATTR(LMN_SATOM(graph_mem_proxy), 0));

  LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(&(rule_mem->rulesets), 0);
  LmnRuleRef r = lmn_ruleset_get_rule(rs, 0);
  LmnReactCxt tmp_rc;
  mc_react_cxt_init(&tmp_rc);
  RC_SET_GROOT_MEM(&tmp_rc, graph_mem);

  //  tmp_rc = *rc;
  //  tmp_rc.work_arry = lmn_register_make(rc->warry_cap);
  //  printf("warry made=%d\n", tmp_rc.work_arry); // ueda
  //  tmp_rc.mode = REACT_ND;
  //  tmp_rc.v = v;
  //  lmn_register_copy(tmp_rc.work_arry, rc->work_arry, rc->warry_num);

  react_rule(&tmp_rc, graph_mem, r);
  lmn_mem_remove_mem(mem, graph_mem);

  LmnSAtom cons, prev_cons, nil, in, out, plus;
  LmnMembrane *m;
  int n_of_results = vec_num(RC_EXPANDED(&tmp_rc));
  //  int i;
  //  for (int i = 0; i < n_of_results; i++) {
  //    lmn_dump_cell_stdout((LmnMembrane *)vec_get(RC_EXPANDED(&tmp_rc), i));
  //  }
  nil = lmn_mem_newatom(mem, LMN_NIL_FUNCTOR);

  if (n_of_results == 0) {
    lmn_mem_newlink(mem, LMN_ATOM(nil), LMN_ATTR_MAKE_LINK(0), 0,
		    react_judge_atom, react_judge_link_attr,
		    LMN_ATTR_GET_VALUE(react_judge_link_attr));
  } else {
    int n1 = n_of_results - 1;
    for (int i = n1; i >= 0; i--) {
      cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
      m = (LmnMembrane *)vec_get(RC_EXPANDED(&tmp_rc), i);
      lmn_mem_add_child_mem(mem, m);
      in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR); 
      out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
      plus = lmn_mem_newatom(m, LMN_UNARY_PLUS_FUNCTOR);
      lmn_newlink_in_symbols(in, 0, out, 0);
      lmn_newlink_in_symbols(in, 1, plus, 0);
      lmn_newlink_in_symbols(out, 1, cons, 0);
      if (i == n1) {
	lmn_newlink_in_symbols(cons, 1, nil, 0);
      } else {
	lmn_newlink_in_symbols(cons, 1, prev_cons, 2);
      }
      prev_cons = cons;
    }

    lmn_mem_newlink(mem, LMN_ATOM(prev_cons), LMN_ATTR_MAKE_LINK(0), 2,
		    react_judge_atom, react_judge_link_attr,
		    LMN_ATTR_GET_VALUE(react_judge_link_attr));
  }

  lmn_mem_newlink(mem,
		  return_rule_mem_proxy, return_rule_mem_proxy_link_attr,
		  LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
		  rule_mem_proxy, rule_mem_proxy_link_attr,
		  LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  mc_react_cxt_destroy(&tmp_rc);
  lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr); 
}

void init_react_rule(void)
{
  lmn_register_c_fun("cb_react_rule", (void *)cb_react_rule, 4);
  lmn_register_c_fun("cb_react_rule_nd", (void *)cb_react_rule_nd, 4);
}