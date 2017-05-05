/*
 * set.c - set implementation
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

/*
 * 検索
 *
 * +a0: 集合
 * +a1: 要素
 * -a2: some/none
 * -a2: 新集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_find(LmnReactCxtRef *rc,
		 LmnMembraneRef mem,
		 LmnAtomRef a0, LmnLinkAttr t0,
		 LmnAtomRef a1, LmnLinkAttr t1,
		 LmnAtomRef a2, LmnLinkAttr t2,
		 LmnAtomRef a3, LmnLinkAttr t3)
{
  st_data_t entry;
  LmnAtomRef result;
  int res;
  if(LMN_ATTR_IS_DATA(t1)) {
    if(LMN_INT_ATTR == t1) {
      res = st_lookup(LMN_SET_DATA(a0), (st_data_t)a1, &entry);
    }
  }

  if(res)
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("some"), 1));
  else
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("none"), 1));

  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  a3, t3, LMN_ATTR_GET_VALUE(t3));
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 0);

  lmn_mem_delete_atom(mem, a1, t1);
}


/* inner_set_to_listで使用するためだけの構造体 */
struct InnerToList{
  LmnMembraneRef mem;
  LmnAtomRef cons;
  LmnAtomRef prev;
};

typedef struct InnerToList *InnerToListRef;

#define ITL(obj) ((InnerToListRef)(obj))
#define ITL_MEM(obj) (ITL(obj)->mem)
#define ITL_CONS(obj) (ITL(obj)->cons)
#define ITL_PREV(obj) (ITL(obj)->prev)

/* プロトタイプ宣言 */
int inner_set_to_list(st_data_t, st_data_t, st_data_t);

/*
 * リストへの変換
 *
 * +a0: 集合
 * -a1: リスト
 * -a2: 集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_to_list(LmnReactCxtRef rc,
		    LmnMembraneRef mem,
		    LmnAtomRef a0, LmnLinkAttr t0,
		    LmnAtomRef a1, LmnLinkAttr t1,
		    LmnAtomRef a2, LmnLinkAttr t2)
{
  LmnAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
  lmn_mem_newlink(mem,
		  a1, t1, LMN_ATTR_GET_VALUE(t1),
		  cons, LMN_ATTR_MAKE_LINK(2), 2);
  st_table_t tbl = LMN_SET_DATA(a0);
  InnerToListRef *itl = LMN_MALLOC(struct InnerToList);
  if(tbl->type == &type_id_hash) {
    ITL_CONS(itl) = cons;
    ITL_MEM(itl) = mem;
    st_foreach(tbl, inner_set_to_list, (st_data_t)itl);
  }
  lmn_mem_delete_atom(ITL_MEM(itl), ITL_CONS(itl), LMN_ATTR_MAKE_LINK(2));
  LmnAtomRef nil = lmn_mem_newatom(ITL_MEM(itl), LMN_NIL_FUNCTOR);
  lmn_newlink_in_symbols(nil, 0, ITL_PREV(itl), 1);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a2, t2, LMN_ATTR_GET_VALUE(t2));
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_to_list(st_data_t key, st_data_t rec, st_data_t itl)
{
  lmn_mem_newlink(ITL_MEM(itl),
		  ITL_CONS(itl), LMN_ATTR_MAKE_LINK(0), 0,
		  (LmnWord)key, LMN_INT_ATTR, 0);
  lmn_mem_push_atom(ITL_MEM(itl), (LmnWord)key, LMN_INT_ATTR);
  ITL_PREV(itl) = ITL_CONS(itl);
  ITL_CONS(itl) = lmn_mem_newatom(ITL_MEM(itl), LMN_LIST_FUNCTOR);
  lmn_mem_newlink(ITL_MEM(itl),
		  ITL_PREV(itl), LMN_ATTR_MAKE_LINK(1), 1,
		  ITL_CONS(itl), LMN_ATTR_MAKE_LINK(2), 2);
  return ST_CONTINUE;
}

/*
 * 複製
 *
 * +a0: 集合
 * -a1: コピー元の集合
 * -a2: コピー集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_copy(LmnReactCxtRef rc,
		 LmnMembraneRef mem,
		 LmnAtomRef a0, LmnLinkAttr t0,
		 LmnAtomRef a1, LmnLinkAttr t1,
		 LmnAtomRef a2, LmnLinkAttr t2)
{
  LmnSetRef s;
  LmnLinkAttr attr;
  if(LMN_SET_DATA(a0)->type == &type_id_hash) {
    s = make_id_set(mem);
    attr = LMN_SP_ATOM_ATTR;
    LMN_SET_DATA(s) = st_copy(LMN_SET_DATA(a0));
  }
  lmn_mem_push_atom(mem, LMN_ATOM(s), attr);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a1, t1, LMN_ATTR_GET_VALUE(t1));
  lmn_mem_newlink(mem,
		  a2, t2, LMN_ATTR_GET_VALUE(t2),
		  s, attr, LMN_ATTR_GET_VALUE(attr));
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
  lmn_register_c_fun("cb_set_find", (void *)cb_set_find, 4);
  lmn_register_c_fun("cb_set_to_list", (void *)cb_set_to_list, 3);
  lmn_register_c_fun("cb_set_copy", (void *)cb_set_copy, 3);
}

