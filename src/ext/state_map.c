#include "state_map.h"
#include "../vm/react_context.h"
#include "../verifier/mc_worker.h"

static int state_map_atom_type;
static BYTE mc_flag = 0x10U;

static LmnStateMapRef lmn_make_state_map(LmnMembraneRef mem)
{
  LmnStateMapRef s = LMN_MALLOC(struct LmnStateMap);
  LMN_SP_ATOM_SET_TYPE(s, state_map_atom_type);
  s->states = statespace_make(NULL, NULL);
  s->id_tbl = st_init_table(&type_id_hash);
  return s;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * -a0 Map
 */
void cb_state_map_init(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtom a0, LmnLinkAttr t0)
{
  LmnStateMapRef atom = lmn_make_state_map(mem);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  LMN_SP_ATOM_SET_TYPE(atom, state_map_atom_type);
  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  LMN_ATOM(atom), attr, 0);
}

/*
 * 状態->ID
 * +a0 Map
 * +a1 状態
 * -a2 ID
 * -a3 Map
 */
void cb_state_map_id_find(LmnReactCxtRef rc,
                          LmnMembraneRef mem,
                          LmnAtom a0, LmnLinkAttr t0,
                          LmnAtom a1, LmnLinkAttr t1,
                          LmnAtom a2, LmnLinkAttr t2,
                          LmnAtom a3, LmnLinkAttr t3)
{
  LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
  LmnSAtom in = LMN_SATOM_GET_LINK(a1, 0);
  LmnSAtom out = a1;
  LmnSAtom plus = LMN_SATOM_GET_LINK(in, 1);
  LmnLinkAttr in_attr = LMN_SATOM_GET_ATTR(a1, 0);
  StateSpaceRef ss = LMN_STATE_MAP(a0)->states;
  st_table_t i_tbl = LMN_STATE_MAP(a0)->id_tbl;

  lmn_mem_delete_atom(m, in, in_attr);

  LmnSAtom at = lmn_mem_newatom(m, lmn_functor_intern(ANONYMOUS, lmn_intern("@"), 1));
  lmn_newlink_in_symbols(plus, 0, at, 0);

  State *new_s = state_make(m, NULL, mc_use_canonical(mc_flag));

  State *succ = statespace_insert(ss, new_s);

  if(succ == new_s){
    /* new state */
    state_id_issue(succ);
    mem_remove_symbol_atom(m, at);
    lmn_delete_atom(at);
    in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
    lmn_newlink_in_symbols(in, 0, out, 0);
    lmn_newlink_in_symbols(in, 1, plus, 0);
    st_insert(i_tbl, (st_data_t)new_s, (st_data_t)m);
  }
  lmn_mem_push_atom(mem, succ, LMN_INT_ATTR);
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  (LmnWord)succ, LMN_INT_ATTR, 0);

  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  a3, t3, LMN_ATTR_GET_VALUE(t3));

  lmn_mem_delete_atom(mem, a1, t1);

  lmn_mem_remove_mem(mem, m);
}

/*
 * ID->状態
 * +a0 Map
 * +a1 ID
 * -a2 状態
 * -a3 Map
 */
void cb_state_map_state_find(LmnReactCxtRef rc,
			     LmnMembraneRef mem,
			     LmnAtom a0, LmnLinkAttr t0,
			     LmnAtom a1, LmnLinkAttr t1,
			     LmnAtom a2, LmnLinkAttr t2,
			     LmnAtom a3, LmnLinkAttr t3)
{
  st_table_t i_tbl=LMN_STATE_MAP(a0)->id_tbl;
  State *s=(State *)a1;
  st_data_t entry;
  int res=st_lookup(i_tbl, (st_data_t)s, &entry);
  LmnSAtom result;
  if(res){
    LmnMembraneRef val=lmn_mem_copy((LmnMembraneRef)entry);
    AtomListEntryRef ent;
    LmnFunctor f;
    LmnSAtom in;
    LmnSAtom out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
    EACH_ATOMLIST_WITH_FUNC(val, ent, f, ({
	  LmnSAtom satom;
	  EACH_ATOM(satom, ent, ({
		if(f==LMN_IN_PROXY_FUNCTOR){
		  in=satom;
		}
	      }))
	    }));
    lmn_newlink_in_symbols(out, 0, in, 0);
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    out, LMN_ATTR_MAKE_LINK(1),1);
  }else{
    result=lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("none"), 1));
    lmn_mem_newlink(mem,
		    LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 0,
		    a2, t2, LMN_ATTR_GET_VALUE(t2));
  }
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t1),
		  a3, t3, LMN_ATTR_GET_VALUE(t3));
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_state_map_copy(void *data)
{
  return data;
}

void sp_cb_state_map_free(void *data)
{
}

void sp_cb_state_map_eq(void *_p1, void *_p2)
{
}

void sp_cb_state_map_dump(void *state_map, LmnPortRef port)
{
  port_put_raw_s(port, "<state_map>");
}

void sp_cb_state_map_is_ground(void *data)
{
}

void init_state_map(void)
{
  state_map_atom_type = lmn_sp_atom_register("state_map",
                                         sp_cb_state_map_copy,
                                         sp_cb_state_map_free,
                                         sp_cb_state_map_eq,
                                         sp_cb_state_map_dump,
                                         sp_cb_state_map_is_ground);
  lmn_register_c_fun("cb_state_map_init", (void *)cb_state_map_init, 1);
  lmn_register_c_fun("cb_state_map_id_find", (void *)cb_state_map_id_find, 4);
  lmn_register_c_fun("cb_state_map_state_find", (void *)cb_state_map_state_find, 4);
}
