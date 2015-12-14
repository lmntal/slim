#include <stdio.h>
#include "../lmntal_ext.h"
#include "../task.h"
#include "../atom.h"
#include "../react_context.h"
#include "../dumper.h"
void fuga(LmnReactCxt *rc,
	  LmnMembrane *mem,
	  LmnAtom rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr)
{
  /* struct McReactCxtData *v = mc_react_data_make(); */
  mc_react_cxt_init(rc);
  /* struct McReactCxtData *v = LMN_MALLOC(struct McReactCxtData); */
  /* v->succ_tbl       = st_init_ptrtable(); */
  /* v->roots          = vec_make(32); */
  /* v->rules          = vec_make(32); */
  /* v->props          = vec_make(8); */
  /* v->mem_deltas     = NULL; */
  /* v->mem_delta_tmp  = NULL; */
  /* v->opt_mode       = 0x00U; */
  /* v->org_succ_num   = 0; */
  /* v->d_cur          = 0; */

  /* react_context_init(rc, REACT_ND); */
  /* rc->v = v; */
  printf("fuga!!!!!!\n");
}

void prototype_ver_2_react_rule(LmnReactCxt *rc,
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

  react_rule(rc, graph_mem, r);
  /* if(react_rule(rc, graph_mem, r)) */
  /*   { */
  /*     /\* LmnSAtom success = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 2)); *\/ */
  /*     /\* lmn_mem_newlink(mem, *\/ */
  /*     /\* 		      LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 0, *\/ */
  /*     /\* 		      graph_mem_proxy, graph_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr)); *\/ */

  /*   } */
  /* else */
  /*   { */
  /*     /\* LmnSAtom fail = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 2)); *\/ */
  /*     /\* lmn_mem_newlink(mem, *\/ */
  /*     /\* 		      LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 0, *\/ */
  /*     /\* 		      graph_mem_proxy, graph_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));       *\/ */
  /*     lmn_mem_newlink(mem, */
  /* 		      react_judge_atom, react_judge_link_attr, LMN_ATTR_GET_VALUE(react_judge_link_attr), */
  /* 		      LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 1); */
  /*   } */
  
  lmn_mem_newlink(mem,
		  react_judge_atom, react_judge_link_attr, LMN_ATTR_GET_VALUE(react_judge_link_attr),
		  graph_mem_proxy, graph_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(graph_mem_proxy_link_attr));

  lmn_mem_newlink(mem,
		  return_rule_mem_proxy, return_rule_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(return_rule_mem_proxy_link_attr),
		  rule_mem_proxy, rule_mem_proxy_link_attr, LMN_ATTR_GET_VALUE(rule_mem_proxy_link_attr));

  lmn_mem_delete_atom(mem, rule_mem_proxy, rule_mem_proxy_link_attr);
  lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr);  
}

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
  /* struct McReactCxtData *v = mc_react_data_make(); */
  /* react_context_init(rc, REACT_ND); */
  /* rc->v = v; */

  /* mc_react_cxt_init(rc); */

  if(react_rule(rc, graph_mem, r))
    {
      printf("success!!\n");
      lmn_dump_cell_stdout(graph_mem);
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
      printf("fail!!!\n");
      lmn_dump_cell_stdout(graph_mem);
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

  /* lmn_mem_delete_atom(mem, rule_mem_proxy, rule_mem_proxy_link_attr); */
  /* lmn_mem_delete_atom(mem, graph_mem_proxy, graph_mem_proxy_link_attr); */
}

void yueno_string100(LmnReactCxt *rc,
		     LmnMembrane *mem,
		     LmnAtom a0, LmnLinkAttr t0,
		     LmnAtom a1, LmnLinkAttr t1)
 {
   LmnString sn = lmn_string_make(int_to_str(100));
   LmnString s  = lmn_string_concat(LMN_STRING(a0), sn);
   LmnSAtom sa  = lmn_mem_newatom(mem,
                      lmn_functor_intern(ANONYMOUS,
                        lmn_intern((const char *)lmn_string_c_str(s)), 1));
   lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                   LMN_ATOM(sa), LMN_ATTR_MAKE_LINK(0), 0);
   lmn_mem_delete_atom(mem, a0, t0);
 }

void init_react_rule(void)
{

  lmn_register_c_fun("prototype_react_rule", prototype_react_rule, 4);
  lmn_register_c_fun("fuga", fuga, 1);
  lmn_register_c_fun("prototype_ver_2_react_rule", prototype_ver_2_react_rule, 4);
  lmn_register_c_fun("yueno_string100", yueno_string100, 2);
}
