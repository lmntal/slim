/*
 * set.c - set implementation
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

#include "set.h"
#include "element/element.h"
#include "verifier/verifier.h"
#include "vm/vm.h"
/**
 * @ingroup  Ext
 * @struct LmnSet set.c "ext/set.c"
 */

/* id set */
/**
 * @memberof LmnSet
 * @private
 */
unsigned long LmnSet::id_hash(st_data_t a) { return (unsigned long)a; }

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::id_cmp(st_data_t a, st_data_t b) { return a != b; }

/* tuple set */
/**
 * @memberof LmnSet
 * @private
 */
unsigned long LmnSet::tuple_hash(LmnSymbolAtomRef cons) {
  unsigned long ret = 0;
  int i;
  for(i = 0; i < cons->get_arity() - 1; i++)
    ret +=(unsigned long)(cons->get_link(i));
  return ret;
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::tuple_cmp(LmnSymbolAtomRef cons0, LmnSymbolAtomRef cons1)
{
  int num0 = cons0->get_arity();
  int num1 = cons1->get_arity();
  int i;
  int ret = 0;
  if (num0 != num1)
    return 1;
  for(i = 0; i < num0 - 1; i++)
    ret = ret || (cons0->get_link(i) != cons1->get_link(i));
  return ret;
}

/* mem set */
/**
 * @memberof LmnSet
 * @private
 */
LmnBinStrRef LmnSet::lmn_inner_mem_encode(LmnMembraneRef m) {
  AtomListEntryRef plus_atom_list =
      m->get_atomlist(LMN_UNARY_PLUS_FUNCTOR);
  LMN_ASSERT(plus_atom_list != NULL);
  LmnAtomRef plus = (LmnAtomRef)atomlist_head(plus_atom_list);
  LmnAtomRef in = ((LmnSymbolAtomRef)plus)->get_link(0);
  LmnAtomRef out =((LmnSymbolAtomRef)in)->get_link(0);

  mem_remove_symbol_atom(m, (LmnSymbolAtomRef)in);
  lmn_delete_atom((LmnSymbolAtomRef)in);

  LmnAtomRef at =
      lmn_mem_newatom(m, lmn_functor_table->intern(ANONYMOUS, lmn_intern("@"), 1));
  lmn_newlink_in_symbols((LmnSymbolAtomRef)plus, 0, (LmnSymbolAtomRef)at, 0);

  LmnBinStrRef s = lmn_mem_encode(m);

  mem_remove_symbol_atom(m, (LmnSymbolAtomRef)at);
  lmn_delete_atom((LmnSymbolAtomRef)at);

  LmnAtomRef new_in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
  lmn_newlink_in_symbols((LmnSymbolAtomRef)new_in, 0, (LmnSymbolAtomRef)out, 0);
  lmn_newlink_in_symbols((LmnSymbolAtomRef)new_in, 1, (LmnSymbolAtomRef)plus,
                         0);

  return s;
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::mem_cmp(LmnMembraneRef m0, LmnMembraneRef m1) {
  LmnBinStrRef s0 = LmnSet::lmn_inner_mem_encode(m0);
  LmnBinStrRef s1 = LmnSet::lmn_inner_mem_encode(m1);
  int res = binstr_compare(s0, s1);
  lmn_binstr_free(s0);
  lmn_binstr_free(s1);
  return res;
}

/**
 * @memberof LmnSet
 * @private
 */
unsigned long LmnSet::mem_hash(LmnMembraneRef m) { return mhash(m); }

/**
 * @memberof LmnSet
 * @private
 */

struct st_hash_type LmnSet::type_id_hash = {(st_cmp_func)LmnSet::id_cmp,
                                    (st_hash_func)LmnSet::id_hash};

/**
 * @memberof LmnSet
 * @private
 */

struct st_hash_type LmnSet::type_mem_hash = {(st_cmp_func)LmnSet::mem_cmp,
                                     (st_hash_func)LmnSet::mem_hash};

/**
 * @memberof LmnSet
 * @private
 */

struct st_hash_type LmnSet::type_tuple_hash = {(st_cmp_func)LmnSet::tuple_cmp,
                                       (st_hash_func)LmnSet::tuple_hash};

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::set_atom_type; /* special atom type */ 
//初期化はされてないようなのだが一応このように記述しておかねばならないらしい

/**
 * @brief Internal Constructor
 * @memberof LmnSet
 * @private
 */
LmnSet::LmnSet(struct st_hash_type *ht) {
  LMN_SP_ATOM_SET_TYPE(this, set_atom_type);
  this->tbl = st_init_table(ht);
}

/**
 * @brief Internal Constructor
 * @memberof LmnSet
 * @private
 */
void LmnSet::lmn_set_free(LmnSetRef set) {
  st_table_t tbl = set->tbl;
  if (tbl->type != &(LmnSet::type_id_hash))
    st_foreach(tbl, (st_iter_func)(LmnSet::inner_set_free), (st_data_t)tbl->type);
  st_free_table(tbl);
}

LmnSet::~LmnSet() {
  st_table_t tbl = this->tbl;
  if (tbl->type != &(LmnSet::type_id_hash))
    st_foreach(tbl, (st_iter_func)(LmnSet::inner_set_free), (st_data_t)tbl->type);
  st_free_table(tbl);
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_free(st_data_t key, st_data_t rec, st_data_t arg) {
  if (arg == (st_data_t)&(LmnSet::type_mem_hash))
    ((LmnMembraneRef)key)->free_rec();
  else
    free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)key);
  return ST_DELETE;
}
/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 解放
 *
 * +a0: 集合
 */
/**
 * @memberof LmnSet
 * @private
 */
void LmnSet::cb_set_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0) {
  lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a0, t0);
  LmnSet::lmn_set_free((LmnSetRef)a0);
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
void LmnSet::cb_set_insert(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                   LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                   LmnLinkAttr t2) {
  if (t0 != LMN_SP_ATOM_ATTR) {
    lmn_mem_delete_atom(mem, a0, t0);
    t0 = LMN_SP_ATOM_ATTR;
    if(LMN_INT_ATTR == t1)
      a0 = (LmnAtomRef)new LmnSet(&(LmnSet::type_id_hash));
    else if(((LmnSymbolAtomRef)a1)->get_functor() == LMN_OUT_PROXY_FUNCTOR)
      a0 = (LmnAtomRef)new LmnSet(&(LmnSet::type_mem_hash));
    else
      a0 = (LmnAtomRef) new LmnSet(&(LmnSet::type_tuple_hash));
  }
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  LmnAtomRef v = (tbl->type == &(LmnSet::type_mem_hash)) ? LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0)) : a1;
  st_insert(tbl, (st_data_t)v, (st_data_t)v);
  if (tbl->type == &(LmnSet::type_tuple_hash)) {
    mem_remove_symbol_atom_with_buddy_data(mem, (LmnSymbolAtomRef)a1);
  } else {
    lmn_mem_remove_atom(mem, a1, t1);
    if (tbl->type == &(LmnSet::type_mem_hash))
      mem->remove_mem((LmnMembraneRef)v);
  }
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a2, t2,
                  LMN_ATTR_GET_VALUE(t2));
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
void LmnSet::cb_set_find(LmnReactCxtRef *rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3) {
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  LmnAtomRef key = (tbl->type == &(LmnSet::type_mem_hash)) ? (LmnAtomRef)LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0)) : a1;
  st_data_t entry;
  int res = st_lookup(tbl, (st_data_t)key, &entry);
  lmn_interned_str s = (res) ? lmn_intern("some") : lmn_intern("none");
  LmnAtomRef result = lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS, s, 1));
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3,
                  LMN_ATTR_GET_VALUE(t3));
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), result,
                  LMN_ATTR_MAKE_LINK(0), 0);
  lmn_mem_delete_atom(mem, a1, t1);
  if (tbl->type == &(LmnSet::type_mem_hash))
    mem->delete_mem((LmnMembraneRef)key);
}

/* set.cpp専用クラス用getter/setter */
// 本当はOOP的にsetterは存在しちゃいけないそうなのだけれど仕方がない
LmnMembraneRef InnerToList::mem(){
  return mem_data;
}
void InnerToList::set_mem_as(LmnMembraneRef data){
  mem_data = data;
}
LmnAtomRef InnerToList::cons(){
  return cons_data;
}
void InnerToList::set_cons_as(LmnAtomRef data){
  cons_data = data;
}
LmnAtomRef InnerToList::prev(){
  return prev_data;
}
void InnerToList::set_prev_as(LmnAtomRef data){
  prev_data = data;
}
struct st_hash_type *InnerToList::ht(){
  return ht_data;
}
void InnerToList::set_ht_as(struct st_hash_type *data){
  ht_data = data;
}

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
void LmnSet::cb_set_to_list(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                    LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1) {
  LmnAtomRef cons = lmn_mem_newatom(mem, LMN_LIST_FUNCTOR);
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), cons,
                  LMN_ATTR_MAKE_LINK(2), 2);
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  InnerToList::InnerToListRef itl = LMN_MALLOC(struct InnerToList);
  itl->set_cons_as(cons);
  itl->set_mem_as(mem);
  itl->set_ht_as(tbl->type);
  st_foreach(tbl, (st_iter_func)LmnSet::inner_set_to_list, (st_data_t)itl);

  lmn_mem_delete_atom(itl->mem(), itl->cons(), LMN_ATTR_MAKE_LINK(2));
  LmnAtomRef nil = lmn_mem_newatom(itl->mem(), LMN_NIL_FUNCTOR);
  lmn_newlink_in_symbols((LmnSymbolAtomRef)nil, 0, (LmnSymbolAtomRef)itl->prev(),
                         1);
  LMN_FREE(itl);
  st_free_table(((LmnSet::LmnSetRef)a0)->tbl);
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_to_list(st_data_t key, st_data_t rec, st_data_t obj) {
  InnerToList::InnerToListRef itl = (InnerToList::InnerToListRef)obj;
  if (itl->ht() == &(LmnSet::type_id_hash)) {
    lmn_mem_newlink(itl->mem(), itl->cons(), LMN_ATTR_MAKE_LINK(0), 0,
                    (LmnAtomRef)key, LMN_INT_ATTR, 0);
    lmn_mem_push_atom(itl->mem(), (LmnAtomRef)key, LMN_INT_ATTR);
  } else if (itl->ht() == &(LmnSet::type_mem_hash)) {
    AtomListEntryRef in_atom_list =
      ((LmnMembraneRef)key)->get_atomlist(LMN_IN_PROXY_FUNCTOR);
    LMN_ASSERT(in_atom_list != NULL);
    LmnAtomRef in = (LmnAtomRef)atomlist_head(in_atom_list);
    LmnAtomRef out = lmn_mem_newatom(itl->mem(), LMN_OUT_PROXY_FUNCTOR);
    lmn_newlink_in_symbols((LmnSymbolAtomRef)in, 0, (LmnSymbolAtomRef)out, 0);
    lmn_mem_newlink(itl->mem(), itl->cons(), LMN_ATTR_MAKE_LINK(0), 0, out,
                    LMN_ATTR_MAKE_LINK(1), 1);
    (itl->mem())->add_child_mem((LmnMembraneRef)key);
  } else if (itl->ht() == &(LmnSet::type_tuple_hash)) {
    int i;
    lmn_mem_push_atom(itl->mem(), (LmnAtomRef)key, LMN_ATTR_MAKE_LINK(3));
    for(i = 0; i < ((LmnSymbolAtomRef)key)->get_arity() - 1; i++)
      lmn_mem_push_atom(itl->mem(), ((LmnSymbolAtomRef)key)->get_link(i), LMN_INT_ATTR);
    lmn_mem_newlink(itl->mem(),
		    itl->cons(), LMN_ATTR_MAKE_LINK(0), 0,
		    (LmnAtomRef)key, LMN_ATTR_MAKE_LINK(i), i);
  }
  itl->set_prev_as(itl->cons());
  itl->set_cons_as( lmn_mem_newatom(itl->mem(), LMN_LIST_FUNCTOR) );
  lmn_mem_newlink(itl->mem(), itl->prev(), LMN_ATTR_MAKE_LINK(1), 1, itl->cons(),
                  LMN_ATTR_MAKE_LINK(2), 2);
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
void LmnSet::cb_set_copy(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2) {
  LmnSet::LmnSetRef s;
  LmnLinkAttr at = LMN_SP_ATOM_ATTR;
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  if (tbl->type == &(LmnSet::type_id_hash)) {
    s = new LmnSet(tbl->type);
    s->tbl = st_copy(tbl);
  } else {
    s = new LmnSet(tbl->type);
    st_foreach(tbl, (st_iter_func)LmnSet::inner_set_copy, (st_data_t)s);
  }
  lmn_mem_push_atom(mem, (LmnAtomRef)s, at);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a1, t1,
                  LMN_ATTR_GET_VALUE(t1));
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), s, at,
                  LMN_ATTR_GET_VALUE(at));
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_copy(st_data_t key, st_data_t rec, st_data_t arg) {
  st_table_t tbl = ((LmnSet::LmnSetRef)arg)->tbl;
  st_data_t val =
      (tbl->type == &(LmnSet::type_mem_hash))
    ? (st_data_t)((LmnMembraneRef)key)->copy()
          : (st_data_t)lmn_copy_satom_with_data((LmnSymbolAtomRef)key, FALSE);
  st_insert(tbl, val, val);
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
void LmnSet::cb_set_erase(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                  LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                  LmnLinkAttr t2) {
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  st_data_t entry;
  if (tbl->type == &(LmnSet::type_id_hash)) {
    st_delete(((LmnSet::LmnSetRef)a0)->tbl, (st_data_t)a1, &entry);
  } else if(tbl->type == &(LmnSet::type_mem_hash)) {
    LmnMembraneRef m = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0));
    if(st_delete(tbl, (st_data_t)m, &entry))
      ((LmnMembraneRef)entry)->free_rec();
    mem->delete_mem(m);
  } else if (tbl->type == &(LmnSet::type_tuple_hash)) {
    if (st_delete(tbl, (st_data_t)a1, &entry))
      free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)entry);
  }
  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a2, t2,
                  LMN_ATTR_GET_VALUE(t2));
}

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
void LmnSet::cb_set_union(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                  LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                  LmnLinkAttr t2) {
  st_foreach(((LmnSet::LmnSetRef)a0)->tbl, (st_iter_func)LmnSet::inner_set_union,
             (st_data_t)a1);
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a2, t2,
                  LMN_ATTR_GET_VALUE(t2));
  LmnSet::lmn_set_free((LmnSet::LmnSetRef)a0);
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_union(st_data_t key, st_data_t rec, st_data_t arg) {
  st_table_t tbl = ((LmnSet::LmnSetRef)arg)->tbl;
  st_data_t entry;
  if (tbl->type == &(LmnSet::type_id_hash)) {
    st_insert(tbl, key, rec);
  } else if (tbl->type == &(LmnSet::type_mem_hash) || tbl->type == &(LmnSet::type_tuple_hash)) {
    if (!st_lookup(tbl, key, &entry)) {
      st_insert(tbl, key, rec);
      return ST_DELETE;
    }
  }
  return ST_CONTINUE;
}

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
void LmnSet::cb_set_intersect(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                      LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                      LmnAtomRef a2, LmnLinkAttr t2) {
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  st_foreach(tbl, (st_iter_func)inner_set_intersect, (st_data_t)a1);
  LmnSet::lmn_set_free((LmnSet::LmnSetRef)a1);
  if (st_num(tbl) > 0) {
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0,
                    LMN_ATTR_GET_VALUE(t0));
  } else {
    LmnAtomRef empty_set = lmn_mem_newatom(
        mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern("set_empty"), 1));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), empty_set,
                    LMN_ATTR_MAKE_LINK(0), 0);
    LmnSet::lmn_set_free((LmnSet::LmnSetRef)a0);
  }
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_intersect(st_data_t key, st_data_t rec, st_data_t arg) {
  st_table_t tbl = ((LmnSet::LmnSetRef)arg)->tbl;
  st_data_t entry;
  int found = st_lookup(tbl, key, &entry);
  if (found)
    return ST_CONTINUE;
  if (tbl->type == &(LmnSet::type_mem_hash))
    ((LmnMembraneRef)key)->free_rec();
  else if (tbl->type == &(LmnSet::type_tuple_hash))
    free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)key);
  return ST_DELETE;
}

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
void LmnSet::cb_set_diff(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                 LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2,
                 LmnLinkAttr t2) {
  st_table_t tbl = ((LmnSet::LmnSetRef)a0)->tbl;
  st_foreach(tbl, (st_iter_func)inner_set_diff, (st_data_t)a1);
  LmnSet::lmn_set_free((LmnSet::LmnSetRef)a1);
  if (st_num(tbl) > 0) {
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0,
                    LMN_ATTR_GET_VALUE(t0));
  } else {
    LmnAtomRef empty_set = lmn_mem_newatom(
        mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern("set_empty"), 1));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), (LmnAtomRef)empty_set,
                    LMN_ATTR_MAKE_LINK(0), 0);
    LmnSet::lmn_set_free((LmnSet::LmnSetRef)a0);
  }
}

/**
 * @memberof LmnSet
 * @private
 */
int LmnSet::inner_set_diff(st_data_t key, st_data_t rec, st_data_t arg) {
  st_table_t tbl = ((LmnSet::LmnSetRef)arg)->tbl;
  st_data_t entry;
  int found = st_lookup(tbl, key, &entry);
  if (!found)
    return ST_CONTINUE;
  if (tbl->type == &(LmnSet::type_mem_hash))
    ((LmnMembraneRef)key)->free_rec();
  else if (tbl->type == &(LmnSet::type_tuple_hash))
    free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)key);
  return ST_DELETE;
}
/*----------------------------------------------------------------------
 * Initialization
 */

/**
 * @memberof LmnSet
 * @private
 */
void *LmnSet::sp_cb_set_copy(void *data) { return data; }

/**
 * @memberof LmnSet
 * @private
 */
void LmnSet::sp_cb_set_free(void *data) {}

/**
 * @memberof LmnSet
 * @private
 */
unsigned char LmnSet::sp_cb_set_eq(void *data1, void *data2) { return 0; }

/**
 * @memberof LmnSet
 * @private
 */
void LmnSet::sp_cb_set_dump(void *set, LmnPortRef port) {
  port_put_raw_s(port, "<set>");
}

/**
 * @memberof LmnSet
 * @private
 */
unsigned char LmnSet::sp_cb_set_is_ground(void *data) { return 1; }

/**
 * @memberof LmnSet
 * @private
 */
void LmnSet::init_set(void) {
  LmnSet::set_atom_type =
      lmn_sp_atom_register("set", LmnSet::sp_cb_set_copy, LmnSet::sp_cb_set_free, LmnSet::sp_cb_set_eq,
                           LmnSet::sp_cb_set_dump, LmnSet::sp_cb_set_is_ground);

  CCallback::lmn_register_c_fun("cb_set_free", (void *)LmnSet::cb_set_free, 1);
  CCallback::lmn_register_c_fun("cb_set_insert", (void *)LmnSet::cb_set_insert, 3);
  CCallback::lmn_register_c_fun("cb_set_find", (void *)LmnSet::cb_set_find, 4);
  CCallback::lmn_register_c_fun("cb_set_to_list", (void *)LmnSet::cb_set_to_list, 2);
  CCallback::lmn_register_c_fun("cb_set_copy", (void *)LmnSet::cb_set_copy, 3);
  CCallback::lmn_register_c_fun("cb_set_erase", (void *)LmnSet::cb_set_erase, 3);
  CCallback::lmn_register_c_fun("cb_set_union", (void *)LmnSet::cb_set_union, 3);
  CCallback::lmn_register_c_fun("cb_set_intersect", (void *)LmnSet::cb_set_intersect, 3);
  CCallback::lmn_register_c_fun("cb_set_diff", (void *)LmnSet::cb_set_diff, 3);
}
