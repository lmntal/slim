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
#include "../vm/vm.h"


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
  LMN_SET_DATA(s) = st_init_table(&type_id_hash);
  return s;
}

/**
 * @brief Internal Constructor
 * @memberof LmnSet
 * @private
 */
static LmnSetRef make_mem_set(LmnMembraneRef mem)
{
  LmnSetRef s = LMN_MALLOC(struct LmnSet);
  LMN_SP_ATOM_SET_TYPE(s, set_atom_type);
  LMN_SET_DATA(s) = st_init_table(&type_mem_hash);
  return s;
}

/**
 * @brief Internal Constructor
 * @memberof LmnSet
 * @private
 */
void lmn_set_free(LmnSetRef set)
{
  st_free_table(LMN_SET_DATA(set));
}
/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 解放
 *
 * +a0: 集合
 */
void cb_set_free(LmnReactCxtRef rc,
		 LmnMembraneRef mem,
		 LmnAtom a0, LmnLinkAttr t0)
{
  lmn_set_free(LMN_SET(a0));
  lmn_mem_remove_data_atom(mem, a0, t0);
}

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
  } else {
    LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a1);
    if(f == LMN_OUT_PROXY_FUNCTOR) { /* mem set */
      if(t0 != LMN_SP_ATOM_ATTR) {
        LmnSetRef s = make_mem_set(mem);
        LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
        lmn_mem_push_atom(mem, LMN_ATOM(s), attr);
        lmn_mem_delete_atom(mem, a0, t0);
        a0 = LMN_ATOM(s);
        t0 = attr;
      }
      LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
      st_insert(LMN_SET_DATA(a0), (st_data_t)m, (st_data_t)m);
      lmn_mem_remove_mem(mem, m);
      lmn_mem_delete_atom(mem, a1, t1);
    }
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
  } else {
    LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a1);
    if(f == LMN_OUT_PROXY_FUNCTOR) { /* mem set */
      LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
      res = st_lookup(LMN_SET_DATA(a0), (st_data_t)m, &entry);
      lmn_mem_remove_mem(mem, m);
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
  struct st_hash_type *ht;
};

typedef struct InnerToList *InnerToListRef;

#define ITL(obj) ((InnerToListRef)(obj))
#define ITL_MEM(obj) (ITL(obj)->mem)
#define ITL_CONS(obj) (ITL(obj)->cons)
#define ITL_PREV(obj) (ITL(obj)->prev)
#define ITL_HT(obj) (ITL(obj)->ht)

/* cb_set_to_list内で使用する関数のプロトタイプ宣言 */
int inner_set_to_list(st_data_t, st_data_t, st_data_t);

/*
 * リストへの変換
 *
 * +a0: 集合
 * -a1: リスト
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_to_list(LmnReactCxtRef rc,
		    LmnMembraneRef mem,
		    LmnAtomRef a0, LmnLinkAttr t0,
		    LmnAtomRef a1, LmnLinkAttr t1)
{
  LmnAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
  lmn_mem_newlink(mem,
		  a1, t1, LMN_ATTR_GET_VALUE(t1),
		  cons, LMN_ATTR_MAKE_LINK(2), 2);
  st_table_t tbl = LMN_SET_DATA(a0);
  InnerToListRef *itl = LMN_MALLOC(struct InnerToList);
  ITL_CONS(itl) = cons;
  ITL_MEM(itl) = mem;
  ITL_HT(itl) = LMN_SET_DATA(a0)->type;
  st_foreach(tbl, inner_set_to_list, (st_data_t)itl);

  lmn_mem_delete_atom(ITL_MEM(itl), ITL_CONS(itl), LMN_ATTR_MAKE_LINK(2));
  LmnAtomRef nil = lmn_mem_newatom(ITL_MEM(itl), LMN_NIL_FUNCTOR);
  lmn_newlink_in_symbols(nil, 0, ITL_PREV(itl), 1);
  LMN_FREE(itl);
  lmn_set_free(a0);
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_to_list(st_data_t key, st_data_t rec, st_data_t itl)
{
  if(ITL_HT(itl) == &type_id_hash) {
    lmn_mem_newlink(ITL_MEM(itl),
		    ITL_CONS(itl), LMN_ATTR_MAKE_LINK(0), 0,
		    (LmnWord)key, LMN_INT_ATTR, 0);
    lmn_mem_push_atom(ITL_MEM(itl), (LmnWord)key, LMN_INT_ATTR);
  } else if(ITL_HT(itl) == &type_mem_hash) {
    AtomListEntryRef ent;
    LmnFunctor f;
    LmnAtomRef in;
    LmnAtomRef out;
    EACH_ATOMLIST_WITH_FUNC(key, ent, f, ({
	  LmnAtomRef satom;
	  EACH_ATOM(satom, ent, ({
		if(f==LMN_IN_PROXY_FUNCTOR){
		  in = satom;
		}
	      }))
	    }));
    out = lmn_mem_newatom(ITL_MEM(itl), LMN_OUT_PROXY_FUNCTOR);
    lmn_newlink_in_symbols(in, 0, out, 0);
    lmn_mem_newlink(ITL_MEM(itl),
		    ITL_CONS(itl), LMN_ATTR_MAKE_LINK(0), 0,
		    out, LMN_ATTR_MAKE_LINK(1), 1);
    lmn_mem_add_child_mem(ITL_MEM(itl), (LmnMembraneRef)key);
  }
  ITL_PREV(itl) = ITL_CONS(itl);
  ITL_CONS(itl) = lmn_mem_newatom(ITL_MEM(itl), LMN_LIST_FUNCTOR);
  lmn_mem_newlink(ITL_MEM(itl),
		  ITL_PREV(itl), LMN_ATTR_MAKE_LINK(1), 1,
		  ITL_CONS(itl), LMN_ATTR_MAKE_LINK(2), 2);
  return ST_CONTINUE;
}

int inner_set_mem_copy(st_data_t, st_data_t, st_data_t);

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
  } else if(LMN_SET_DATA(a0)->type == &type_mem_hash) {
    s = make_mem_set(mem);
    attr = LMN_SP_ATOM_ATTR;
    st_foreach(LMN_SET_DATA(a0), (int)inner_set_mem_copy, s);
  }
  lmn_mem_push_atom(mem, LMN_ATOM(s), attr);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a1, t1, LMN_ATTR_GET_VALUE(t1));
  lmn_mem_newlink(mem,
		  a2, t2, LMN_ATTR_GET_VALUE(t2),
		  s, attr, LMN_ATTR_GET_VALUE(attr));
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_mem_copy(st_data_t key, st_data_t rec, st_data_t arg)
{
  LmnMembraneRef m = lmn_mem_copy(key);
  st_insert(LMN_SET_DATA(arg), m, m);
  return ST_CONTINUE;
}

/*
 * 消去
 *
 * +a0: 集合
 * +a1: 要素
 * -a2: 新集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_erase(LmnReactCxtRef rc,
		  LmnMembraneRef mem,
		  LmnAtomRef a0, LmnLinkAttr t0,
		  LmnAtomRef a1, LmnLinkAttr t1,
		  LmnAtomRef a2, LmnLinkAttr t2)
{
  st_data_t entry;
  if(LMN_SET_DATA(a0)->type == &type_id_hash) {
    st_delete(LMN_SET_DATA(a0), (st_data_t)a1, &entry);
  } else if(LMN_SET_DATA(a0)->type == &type_mem_hash) {
    LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
    if(st_lookup(LMN_SET_DATA(a0), (st_data_t)m, &entry)) {
      st_delete(LMN_SET_DATA(a0), (st_data_t)m, &entry);
      lmn_mem_free_rec((LmnMembraneRef)entry);
    }
    lmn_mem_delete_mem(mem, m);
  }
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a2, t2, LMN_ATTR_GET_VALUE(t2));
}

/* cb_set_union内で使用する関数のプロトタイプ宣言 */
int inner_set_union(st_data_t, st_data_t, st_data_t);

/*
 * 和集合
 *
 * +a0: 集合X
 * +a1: 集合Y
 * -a2: XとYの和集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_union(LmnReactCxtRef rc,
		  LmnMembraneRef mem,
		  LmnAtomRef a0, LmnLinkAttr t0,
		  LmnAtomRef a1, LmnLinkAttr t1,
		  LmnAtomRef a2, LmnLinkAttr t2)
{
  st_foreach(LMN_SET_DATA(a0), (int)inner_set_union, a1);
  lmn_mem_newlink(mem,
		  a1, t1, LMN_ATTR_GET_VALUE(t1),
		  a2, t2, LMN_ATTR_GET_VALUE(t2));
  lmn_set_free(a0);
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_union(st_data_t key, st_data_t rec, st_data_t arg)
{
  st_table_t tbl = LMN_SET_DATA(arg);
  if(tbl->type == &type_id_hash)
    st_insert(tbl, key, rec);
  else if(tbl->type == &type_mem_hash) {
    if(st_lookup(tbl, key, rec)) {
      LMN_FREE(key);
      LMN_FREE(rec);
    } else {
      st_insert(tbl, key, rec);
    }
  }
  return ST_CONTINUE;
}

/* cb_set_intersect内で使用する関数のプロトタイプ宣言 */
int inner_set_intersect(st_data_t, st_data_t, st_data_t);

/* inner_set_intersectで使用するためだけの構造体 */
struct InnerIntersect{
  LmnSetRef set;
  LmnSetRef new_set;
  int empty_check;
};

typedef struct InnerIntersect *InnerIntersectRef;

#define II(obj) ((InnerIntersectRef)(obj))
#define II_S(obj) (II(obj)->set)
#define II_NS(obj) (II(obj)->new_set)
#define II_EC(obj) (II(obj)->empty_check)
/*
 * 積集合
 *
 * +a0: 集合X
 * +a1: 集合Y
 * -a2: XとYの積集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_intersect(LmnReactCxtRef rc,
		      LmnMembraneRef mem,
		      LmnAtomRef a0, LmnLinkAttr t0,
		      LmnAtomRef a1, LmnLinkAttr t1,
		      LmnAtomRef a2, LmnLinkAttr t2)
{
  st_table_t tbl = LMN_SET_DATA(a0);
  st_foreach(tbl, (int)inner_set_intersect, a1);
  lmn_set_free(a1);
  if(st_num(tbl))
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    a0, t0, LMN_ATTR_GET_VALUE(t0));
  else {
    LmnAtomRef empty_set = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("set_empty"), 1));
    lmn_mem_newlink(mem,
  		    a2, t2, LMN_ATTR_GET_VALUE(t2),
  		    LMN_ATOM(empty_set), LMN_ATTR_MAKE_LINK(0), 0);
    lmn_set_free(a0);
  }
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_intersect(st_data_t key, st_data_t rec, st_data_t arg)
{
  st_table_t tbl = LMN_SET_DATA(arg);
  st_data_t entry;
  if(tbl->type == &type_id_hash)
    if(!st_lookup(tbl, key, &entry))
      return ST_DELETE;

  return ST_CONTINUE;
}

int inner_set_diff(st_data_t, st_data_t, st_data_t);

/*
 * 差集合
 *
 * +a0: 集合X
 * +a1: 集合Y
 * -a2: XとYの差集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void cb_set_diff(LmnReactCxtRef rc,
		 LmnMembraneRef mem,
		 LmnAtomRef a0, LmnLinkAttr t0,
		 LmnAtomRef a1, LmnLinkAttr t1,
		 LmnAtomRef a2, LmnLinkAttr t2)
{
  LmnSetRef set = make_id_set(mem);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  InnerIntersectRef *ii = LMN_MALLOC(struct InnerIntersect);
  II_S(ii) = LMN_SET(a1);
  II_NS(ii) = set;
  II_EC(ii) = 0;
  st_foreach(LMN_SET_DATA(a0), (int)inner_set_diff, ii);
  if(II_EC(ii)) {
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    LMN_ATOM(set), attr, LMN_ATTR_GET_VALUE(attr));
    lmn_mem_push_atom(mem, LMN_ATOM(set), attr);
  } else {
    LmnAtomRef empty_set = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("set_empty"), 1));
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    LMN_ATOM(empty_set), LMN_ATTR_MAKE_LINK(0), 0);
    lmn_set_free(set);
  }
  LMN_FREE(ii);
  lmn_set_free(a1);
  lmn_set_free(a0);
}

/**
 * @memberof LmnSet
 * @private
 */
int inner_set_diff(st_data_t key, st_data_t rec, st_data_t arg)
{
  st_data_t d;
  if(!st_lookup(LMN_SET_DATA(II_S(arg)), key, &d)) {
    st_insert(LMN_SET_DATA(II_NS(arg)), key, key);
    II_EC(arg) = 1;
  }
  return ST_CONTINUE;
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

  lmn_register_c_fun("cb_set_free", (void *)cb_set_free, 1);
  lmn_register_c_fun("cb_set_insert", (void *)cb_set_insert, 3);
  lmn_register_c_fun("cb_set_find", (void *)cb_set_find, 4);
  lmn_register_c_fun("cb_set_to_list", (void *)cb_set_to_list, 2);
  lmn_register_c_fun("cb_set_copy", (void *)cb_set_copy, 3);
  lmn_register_c_fun("cb_set_erase", (void *)cb_set_erase, 3);
  lmn_register_c_fun("cb_set_union", (void *)cb_set_union, 3);
  lmn_register_c_fun("cb_set_intersect", (void *)cb_set_intersect, 3);
  lmn_register_c_fun("cb_set_diff", (void *)cb_set_diff, 3);
}

