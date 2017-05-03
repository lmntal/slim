#include "set.h"
#include "element/element.h"
#include "vm/vm.h"


/**
 * @memberof LmnSet
 * @private
 */
static int set_atom_type; /* special atom type */

/**
 * @memberof LmnSet
 * @private
 */
#define LMN_SET_DATA(obj) (LMN_SET(obj)->tbl)

/**
 * @brief Internal Constructor
 * @memberof LmnSet
 * @private
 */
static LmnSetRef make_id_set(LmnMembraneRef mem)
{
  LmnSetRef s = LMN_MALLOC(struct LmnSet);
  LMN_SP_ATOM_SET_TYPE(s, set_atom_type);
  s->tbl = st_init_table(&type_id_hash);
  return s;
}
/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 挿入
 *
 * +a0: 集合
 * +a1: 要素
 * -a2: 新集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_insert(LmnReactCxtRef rc,
		   LmnMembraneRef mem,
		   LmnAtomRef a0, LmnLinkAttr t0,
		   LmnAtomRef a1, LmnLinkAttr t1,
		   LmnAtomRef a2, LmnLinkAttr t2)
{
  if(LMN_INT_ATTR == t1) {	/* id set */
    if(t0 != LMN_SP_ATOM_ATTR) {
      LmnSetRef s = make_id_set(mem);
      LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
      lmn_mem_push_atom(mem, LMN_ATOM(s), attr);
      lmn_mem_delete_atom(mem, a0, t0);
      a0 = LMN_ATOM(s);
      t0 = attr;
    }
    st_insert(LMN_SET_DATA(a0), (st_data_t)a1, (st_data_t)a1);
    lmn_mem_delete_atom(mem, a1, t1);
  }
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  a2, t2, LMN_ATTR_GET_VALUE(t2));
}

/*----------------------------------------------------------------------
 * Initialization
 */

/**
 * @memberof LmnSet
 * @private
 */
void *sp_cb_set_copy(void *data)
{
  return data;
}

/**
 * @memberof LmnSet
 * @private
 */
void sp_cb_set_free(void *data)
{

}

/**
 * @memberof LmnSet
 * @private
 */
void sp_cb_set_eq(void *data1, void *data2)
{

}

/**
 * @memberof LmnSet
 * @private
 */
void sp_cb_set_dump(void *set, LmnPortRef port)
{
  port_put_raw_s(port, "<set>");
}

/**
 * @memberof LmnSet
 * @private
 */
void sp_cb_set_is_ground(void *data)
{

}

/**
 * @memberof LmnSet
 * @private
 */
void init_set(void)
{
  set_atom_type = lmn_sp_atom_register("set",
                                       sp_cb_set_copy,
                                       sp_cb_set_free,
                                       sp_cb_set_eq,
                                       sp_cb_set_dump,
                                       sp_cb_set_is_ground);

  lmn_register_c_fun("cb_set_insert", (void *)cb_set_insert, 3);
}

