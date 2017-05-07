#include "state_map.h"
#include "../vm/react_context.h"

static int state_map_atom_type;

static LmnStateMapRef lmn_make_state_map(LmnMembraneRef mem)
{
  LmnStateMapRef s = LMN_MALLOC(struct LmnStateMap);
  LMN_SP_ATOM_SET_TYPE(s, state_map_atom_type);
  s->states = statespace_make(NULL, NULL);
  s->id_tbl = st_init_table(&type_id_hash);
  return s;
}

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
}
