#include <stdio.h>
#include <string.h>
#include "lmntal_ext.h"
#include "dumper.h"
#include "hyperlink.h"


#if SIZEOF_LONG == 4
#  define EMPTY_KEY   0xffffffffUL
#  define DELETED_KEY 0xfffffffeUL
#elif SIZEOF_LONG == 8
#  define EMPTY_KEY   0xffffffffffffffffUL
#  define DELETED_KEY 0xfffffffffffffffeUL
#endif

#define GEN_PREFIX ":G"

static int gen_c = 0;

#include "lmntal_ext.h"
#include "utility/util.h"
#include "slim_header/string.h"
#include "dumper.h"
#include "atom.h"
#include "membrane.h"
/*
 * Internal Constructor
 */
static LmnSAtom lmn_make_atom(LmnMembrane *mem, LmnAtom s, LmnAtom size)
{
  LmnSAtom a;
  int k;
    a = lmn_mem_newatom(mem,
         lmn_functor_intern(ANONYMOUS, 
			    lmn_intern(lmn_string_c_str(LMN_STRING(s))),
			    size));
  for (k = 0; k < (int)size-1; k++) {
    lmn_mem_newlink(mem,
                    LMN_ATOM(a), LMN_ATTR_MAKE_LINK(0), k,
                    0, LMN_INT_ATTR, 0);
    lmn_mem_push_atom(mem, k, LMN_INT_ATTR);
  }
  return a;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 * ファンクタ名や要素数にかかわらず必ず記号アトムが生成される
 *
 * +a0: ファンクタ名（文字列）
 * +a1: 要素数
 * -a2: アトム
 */
void cb_atom_new(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0,
    LmnAtom a1, LmnLinkAttr t1,
    LmnAtom a2, LmnLinkAttr t2)
{
  LmnSAtom atom, junk, res;
   
  /* a1 is assumed to be an integer data atom */
  if (a1 > 0 && a1 <= 127) {
    atom = lmn_make_atom(mem, a0, a1);
    res = lmn_mem_newatom(mem,lmn_functor_intern(ANONYMOUS, lmn_intern("some"), 2));
    lmn_mem_newlink(mem,
		    LMN_ATOM(atom), LMN_ATTR_MAKE_LINK(0), a1-1,
		    LMN_ATOM(res), LMN_ATTR_MAKE_LINK(0), 0);
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    LMN_ATOM(res), LMN_ATTR_MAKE_LINK(0), 1);
    lmn_mem_delete_atom(mem, a1, t1);
    lmn_mem_delete_atom(mem, a0, t0);
  } else if (a1 == 0) {
    atom = lmn_make_atom(mem, a0, a1);
    res = lmn_mem_newatom(mem,lmn_functor_intern(ANONYMOUS, lmn_intern("nullary"), 1));
    lmn_mem_newlink(mem,
      a2, t2, LMN_ATTR_GET_VALUE(t2),
      LMN_ATOM(res), LMN_ATTR_MAKE_LINK(0), 0);
    lmn_mem_delete_atom(mem, a1, t1);
    lmn_mem_delete_atom(mem, a0, t0);
  } else {
    lmn_fatal("Atom's arity must be between 0 and 127.");
  }
}


void cb_gensym_test(LmnReactCxt *rc,
		    LmnMembrane *mem,
		    LmnAtom atom, LmnLinkAttr atom_attr)
{
  LmnSAtom satom = LMN_SATOM(atom);
  if(LMN_IS_HL(satom))
    {
      printf("GENSYM TEST!!\n");
      HyperLink *hl = lmn_hyperlink_at_to_hl(satom);
      HyperLink *root = hl->parent;
      LmnSAtom root_linked_satom = LMN_SATOM(LMN_SATOM_GET_LINK(root->atom, 0));
      printf("root_linked_satom = %lu\n", (unsigned long)root_linked_satom);
      printf("root-linked-atom-name = %s\n", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(root_linked_satom))));
    }
}

void cb_gensym(LmnReactCxt *rc,
	       LmnMembrane *mem,
	       LmnAtom hl_atom, LmnLinkAttr hl_atom_attr)
{
  LmnSAtom hl_satom = LMN_SATOM(hl_atom);
  if(LMN_IS_HL(hl_satom))
    {
      printf("CB_GENSYM!!!!\n");
      HyperLink *hl = lmn_hyperlink_at_to_hl(hl_satom);

      HyperLink *root = hl->parent;

      LmnSAtom root_linked_atom = LMN_SATOM(LMN_SATOM_GET_LINK(root->atom, 0));

      /* printf("root-linked-atom-name = %s\n", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(root_linked_atom)))), fflush(stdout); */
      /* HyperLink *child = hyperlink_head_child(root); */
      /* LmnSAtom child_linked_satom = LMN_SATOM(LMN_SATOM_GET_LINK(child->atom, 0)); */
      HashSet *children = root->children;
      printf("HOGE!!!\n"), fflush(stdout);
      if(children)
	{
	  HashSetIterator it;
	  for(it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it))
	    {
	      HyperLink *child = (HyperLink *)hashsetiter_entry(&it);
	      LmnSAtom child_linked_atom = LMN_SATOM(LMN_SATOM_GET_LINK(child->atom, 0));
	      LmnMembrane *child_m = child->mem;
	      if((HashKeyType)child < DELETED_KEY)
		{
		  if(strcmp(lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(child_linked_atom))), "$callback") != 0)
		    {
		      LmnSAtom gensym_atom = lmn_mem_newatom(child_m, 
							     lmn_functor_intern(ANONYMOUS, 
										lmn_intern(lmn_string_c_str("hoge")),
										0));

		      lmn_mem_push_atom(child_m, LMN_ATOM(gensym_atom), LMN_INT_ATTR);
		      lmn_mem_delete_atom(child_m, LMN_SATOM_GET_LINK(child->atom, 0), LMN_SATOM_GET_ATTR(child->atom, 0));
		      lmn_mem_delete_atom(child_m, LMN_ATOM(child->atom), LMN_SATOM_GET_ATTR(child->atom, 0));
		      lmn_hyperlink_delete(child->atom);

		      /* printf("child-linked-satom-name = %s\n", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(child_linked_atom))));*/
		    }
		}
	    }
	}
      if(strcmp(lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(root_linked_atom))), "$callback") != 0)
      	{

      	  LmnMembrane *root_m = root->mem;
      	  LmnSAtom gensym_atom = lmn_mem_newatom(root_m,
      	  					 lmn_functor_intern(ANONYMOUS,
      	  							    lmn_intern(GEN_PREFIX),
      	  							    0));
      	  lmn_mem_push_atom(root_m, LMN_ATOM(gensym_atom), LMN_INT_ATTR);
      	  lmn_mem_delete_atom(root_m, LMN_SATOM_GET_LINK(root->atom, 0), LMN_SATOM_GET_ATTR(root->atom, 0));
      	  lmn_mem_delete_atom(root_m, LMN_ATOM(root->atom), LMN_SATOM_GET_ATTR(root->atom, 0));

      	  /* lmn_hyperlink_delete(root->atom); */

      	}
    }
  gen_c++;
  /* printf("HOGE\n"),fflush(stdout); */
}


void cb_react_rule(LmnReactCxt *rc,
			  LmnMembrane *mem,
			  LmnAtom rule_mem_proxy, LmnLinkAttr rule_mem_proxy_link_attr,
			  LmnAtom graph_mem_proxy, LmnLinkAttr graph_mem_proxy_link_attr,
			  LmnAtom return_rule_mem_proxy, LmnLinkAttr return_rule_mem_proxy_link_attr,
			  LmnAtom react_judge_atom, LmnLinkAttr react_judge_link_attr)
{
  LmnMembrane *rule_mem = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(rule_mem_proxy, 0));
  LmnMembrane *graph_mem = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(graph_mem_proxy, 0));
  LmnRuleSet rs = (LmnRuleSet)vec_get(&(rule_mem->rulesets), 0);
  LmnRule r = lmn_ruleset_get_rule(rs, 0);
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

  LmnRuleSet rs = (LmnRuleSet)vec_get(&(rule_mem->rulesets), 0);
  LmnRule r = lmn_ruleset_get_rule(rs, 0);
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
    int n1 = n_of_results - 1, i;
    for (i = n1; i >= 0; i--) {
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


void cb_mem_eq(LmnReactCxt *rc,
	       LmnMembrane *mem,
	       LmnAtom mem0_proxy, LmnLinkAttr mem0_proxy_link_attr,
	       LmnAtom mem1_proxy, LmnLinkAttr mem1_proxy_link_attr,
	       LmnAtom judge_atom, LmnLinkAttr judge_atom_link_attr)
{
  LmnMembrane *mem0 = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(mem0_proxy, 0));
  LmnMembrane *mem1 = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(mem1_proxy, 0));


  
  LmnSAtom result;
  if(lmn_mem_equals(mem0, mem1) == TRUE)
    {
      result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("true"), 1));
      //      printf("TRUE\n");
    }
  else
    {
      result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("false"), 1));
      //      printf("FALSE\n");
    }
  
  lmn_mem_newlink(mem,
		  LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 0,
		  judge_atom, judge_atom_link_attr,
		  LMN_ATTR_GET_VALUE(judge_atom_link_attr));

}


void init_react_rule(void)
{
  lmn_register_c_fun("cb_react_rule", cb_react_rule, 4);
  lmn_register_c_fun("cb_react_rule_nd", cb_react_rule_nd, 4);
  lmn_register_c_fun("cb_gensym", cb_gensym, 1);
  lmn_register_c_fun("cb_gensym_test", cb_gensym_test, 1);
  lmn_register_c_fun("cb_atom_new", cb_atom_new, 3);
  lmn_register_c_fun("cb_mem_eq", cb_mem_eq, 3);
}

