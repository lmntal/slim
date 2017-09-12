/*
 * hashmap.c - set implementation
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
#include "element/element.h"
#include "vm/vm.h"
#include "set.h"

struct LmnHashMap{
  LMN_SP_ATOM_HEADER;
  st_table_t tbl;
};

typedef struct LmnHashMap *LmnHashMapRef;

static int hashmap_atom_type;	/* special atom type */

LmnHashMapRef lmn_hashmap_make(struct st_hash_type *ht)
{
  LmnHashMapRef h = LMN_MALLOC(struct LmnHashMap);
  LMN_SP_ATOM_SET_TYPE(h, hashmap_atom_type);
  h->tbl = st_init_table(ht);
  return h;
}

int inner_hashmap_free(st_data_t key, st_data_t rec, st_data_t arg)
{
  lmn_mem_free_rec((LmnMembraneRef)rec);
  return ST_DELETE;
}

/*
 * 解放
 *
 * +a0: ハッシュマップ
 */
void cb_hashmap_free(LmnReactCxtRef rc,
		     LmnMembraneRef mem,
		     LmnAtomRef a0, LmnLinkAttr t0)
{
  st_table_t tbl = ((LmnHashMapRef)a0)->tbl;
  st_foreach(tbl, (st_iter_func)inner_hashmap_free, (st_data_t)0);
  st_free_table(tbl);
  lmn_mem_delete_atom(mem, a0, t0);
}

/*
 * 挿入
 *
 * +a0: ハッシュマップ
 * +a1: キー
 * +a2: エントリ
 * -a3: ハッシュマップ
 */
void cb_hashmap_insert(LmnReactCxtRef rc,
		       LmnMembraneRef mem,
		       LmnAtomRef a0, LmnLinkAttr t0,
		       LmnAtomRef a1, LmnLinkAttr t1,
		       LmnAtomRef a2, LmnLinkAttr t2,
		       LmnAtomRef a3, LmnLinkAttr t3)
{
  if(t0 != LMN_SP_ATOM_ATTR) {
    lmn_mem_delete_atom(mem, a0, t0);
    t0 = LMN_SP_ATOM_ATTR;
    a0 = (LmnAtomRef)lmn_hashmap_make(&type_id_hash);
  }

  st_table_t tbl = ((LmnHashMapRef)a0)->tbl;
  LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a2, 0));
  int ret = st_insert_safe(tbl, (st_data_t)a1, (st_data_t)m);

  if(!ret)
    {
      /* key is duplicated */
      st_data_t *val;
      st_delete(tbl, (st_data_t)a1, &val);
      lmn_mem_free_rec(val);
      st_insert(tbl, (st_data_t)a1, (st_data_t)m);
    }
  
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_mem_delete_atom(mem, a2, t2);
  lmn_mem_remove_mem(mem, m);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a3, t3, LMN_ATTR_GET_VALUE(t3));
}

/*
 * 検索
 *
 * +a0: ハッシュマップ
 * +a1: キー
 * -a2: some(エントリ)/none
 * -a3: ハッシュマップ
 */
void cb_hashmap_find(LmnReactCxtRef rc,
		     LmnMembraneRef mem,
		     LmnAtomRef a0, LmnLinkAttr t0,
		     LmnAtomRef a1, LmnLinkAttr t1,
		     LmnAtomRef a2, LmnLinkAttr t2,
		     LmnAtomRef a3, LmnLinkAttr t3)
{
  st_table_t tbl = ((LmnHashMapRef)a0)->tbl;
  st_data_t entry;
  int res = st_lookup(tbl, (st_data_t)a1, &entry);
  LmnAtomRef result;

  if(res) {
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("some"), 2));
    LmnMembraneRef m = lmn_mem_copy((LmnMembraneRef)entry);
    LmnAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
    AtomListEntryRef in_atom_list = lmn_mem_get_atomlist(m, LMN_IN_PROXY_FUNCTOR);
    LMN_ASSERT(in_atom_list != NULL);
    LmnAtomRef in = (LmnAtomRef)atomlist_head(in_atom_list);
    lmn_newlink_in_symbols(in, 0, out, 0);
    lmn_mem_add_child_mem(mem, m);
    lmn_mem_newlink(mem,
		    out, LMN_ATTR_MAKE_LINK(1), 1,
		    result, LMN_ATTR_MAKE_LINK(1), 1);
  } else {
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("none"), 1));
  }
  lmn_mem_newlink(mem,
		  a2, t2, LMN_ATTR_GET_VALUE(t3),
		  result, LMN_ATTR_MAKE_LINK(0), 0);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  a3, t3, LMN_ATTR_GET_VALUE(t3));
}

/* 
 * Initialization
 */

void *sp_cb_hashmap_copy(void *data)
{
  return data;
}

void sp_cb_hashmap_free(void *data)
{

}

unsigned char sp_cb_hashmap_eq(void *data1, void *data2)
{
  return 0;
}

void sp_cb_hashmap_dump(void *hashmap, LmnPortRef port)
{
  port_put_raw_s(port, "<hashmap>");
}

unsigned char sp_cb_hashmap_is_ground(void *data)
{
  return 1;
}

void init_hashmap(void)
{
  hashmap_atom_type = lmn_sp_atom_register("hashmap",
				       sp_cb_hashmap_copy,
				       sp_cb_hashmap_free,
				       sp_cb_hashmap_eq,
				       sp_cb_hashmap_dump,
				       sp_cb_hashmap_is_ground);

  lmn_register_c_fun("cb_hashmap_insert", (void *)cb_hashmap_insert, 4);
  lmn_register_c_fun("cb_hashmap_find", (void *)cb_hashmap_find, 4);
  lmn_register_c_fun("cb_hashmap_free", (void *)cb_hashmap_free, 1);
}
