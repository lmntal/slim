#include <stdio.h>
#include "../lmntal_ext.h"
#include "../task.h"
#include "../atom.h"

void prototype_react_rule(LmnReactCxt *rc,
			  LmnMembrane *mem,
			  LmnAtom rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
			  LmnAtom graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
			  LmnAtom return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
			  LmnAtom react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembrane *rule_mem = LMN_PROXY_GET_MEM(LMN_SATOM(LMN_SATOM_GET_LINK(LMN_SATOM(rule_mem_proxy), 0)));
  LmnMembrane *graph_mem = LMN_PROXY_GET_MEM(LMN_SATOM(LMN_SATOM_GET_LINK(LMN_SATOM(graph_mem_proxy), 0)));
  struct Vector rulesets = rule_mem->rulesets;
  LmnRuleSet rs = (LmnRuleSet)vec_get(&rulesets, 0);
  LmnRule r = lmn_ruleset_get_rule(rs, 0);
  
  if(react_rule(rc, graph_mem, r))
    {
      LmnSAtom success = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 2));
      lmn_mem_newlink(mem,
		      LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 0,
		      graph_mem_proxy, graph_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));
      lmn_mem_newlink(mem,
		      react_judge_atom, react_judge_link_attr, LMN_ATTR_GET_VALUE(react_judge_link_attr),
		      LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 1);
    }
  else
    {
      LmnSAtom fail = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 2));
      lmn_mem_newlink(mem,
		      LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 0,
		      graph_mem_proxy, graph_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));      
      lmn_mem_newlink(mem,
		      react_judge_atom, react_judge_link_attr, LMN_ATTR_GET_VALUE(react_judge_link_attr),
		      LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 1);
    }
  
  lmn_mem_newlink(mem,
		  return_rule_mem_proxy, return_rule_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
		  rule_mem_proxy, rule_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  lmn_mem_delete_atom(mem, rule_mem_proxy, rule_mem_proxy_link_attr);
  lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr);
}

void init_react_rule(void)
{
  lmn_register_c_fun("prototype_react_rule", prototype_react_rule, 4);
}
