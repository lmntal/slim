#include <stdio.h>
#include <util.h>
#include <st.h>
#include "../dumper.h"
#include "../slim_header/string.h"
#include "../lmntal_ext.h"
#include "../atom.h"
#include "hash.h"
#include "../special_atom.h"
#include "../verifier/mem_encode.h"

static int hash_atom_type;

#define LMN_HASH_DATA(obj) (LMN_HASH(obj)->tbl)

/*
 * Internal Constructor
 */

static LmnHashRef lmn_make_hash(LmnMembrane *mem)
{
  LmnHashRef h = LMN_MALLOC(struct LmnHash);
  LMN_SP_ATOM_SET_TYPE(h, hash_atom_type);
  h->tbl = st_init_table(&type_mem_hash);
  return h;
}

void lmn_hash_free(LmnHashRef hash, LmnMembrane *mem)
{
  st_free_table(LMN_HASH_DATA(hash));
  LMN_FREE(hash);
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/* 
 *生成
 *
 * -a0: ハッシュテーブル
 */
void cb_hash_init(LmnReactCxt *rc,
		  LmnMembrane *mem,
		  LmnAtom a0, LmnLinkAttr t0)
{
  LmnHashRef atom = lmn_make_hash(mem);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  LMN_SP_ATOM_SET_TYPE(atom, hash_atom_type);
  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t0),
		  LMN_ATOM(atom), attr, 0);
}

/* 
 * 解放
 *
 * +a0: ハッシュテーブル
 */
void cb_hash_free(LmnReactCxt *rc,
		  LmnMembrane *mem,
		  LmnAtom a0, LmnLinkAttr t0)
{
  lmn_hash_free(LMN_HASH(a0), mem);
  lmn_mem_remove_data_atom(mem, a0, t0);
}

/* 
 *要素取得
 *
 * +a0: ハッシュテーブル
 * +a1: 膜
 * -a2: 判定
 * -a3: 膜
 * -a4: 新ハッシュテーブル
 */
void cb_hash_get(LmnReactCxt *rc,
		 LmnMembrane *mem,
		 LmnAtom a0, LmnLinkAttr t0,
		 LmnAtom a1, LmnLinkAttr t1,
		 LmnAtom a2, LmnLinkAttr t2,
		 LmnAtom a3, LmnLinkAttr t3,
		 LmnAtom a4, LmnLinkAttr t4)
{
  st_data_t entry;
  LmnSAtom result;
  LmnMembrane *m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
  if(st_lookup(LMN_HASH_DATA(a0), (st_data_t)m, &entry))
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 1));
  else
    result = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 1));

  lmn_mem_newlink(mem,
		  LMN_ATOM(result), LMN_ATTR_MAKE_LINK(0), 0,
		  a2, t2, LMN_ATTR_GET_VALUE(t2));

  lmn_mem_newlink(mem,
		  a1, t1, LMN_ATTR_GET_VALUE(t1),
		  a3, t3, LMN_ATTR_GET_VALUE(t3));

  lmn_mem_newlink(mem,
		   a0, t0, LMN_ATTR_GET_VALUE(t0),
		   a4, t4, LMN_ATTR_GET_VALUE(t4));
}

/* 
 * 要素挿入
 * 
 * +a0: ハッシュテーブル
 * +a1: 膜
 * -a2: 膜
 * -a3: 新ハッシュテーブル
 */
void cb_hash_put(LmnReactCxt *rc,
		 LmnMembrane *mem,
		 LmnAtom a0, LmnLinkAttr t0,
		 LmnAtom a1, LmnLinkAttr t1,
		 LmnAtom a2, LmnLinkAttr t2,
		 LmnAtom a3, LmnLinkAttr t3)
{

  LmnMembrane *m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
  st_insert(LMN_HASH_DATA(a0), (st_data_t)m, (st_data_t)m);

  lmn_mem_newlink(mem,
		  a3, t3, LMN_ATTR_GET_VALUE(t3),
		  a0, t0, LMN_ATTR_GET_VALUE(t0));

  lmn_mem_newlink(mem,
		  a2, t2, LMN_ATTR_GET_VALUE(t2),
		  a1, t1, LMN_ATTR_GET_VALUE(t1));
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_hash_copy(void *data)
{
  LmnHashRef h = LMN_MALLOC(struct LmnHash);
  LMN_SP_ATOM_SET_TYPE(h, hash_atom_type);
  h->tbl = st_copy(LMN_HASH_DATA(LMN_HASH(data)));
  return h;
}

void sp_cb_hash_free(void *data)
{
  lmn_hash_free(LMN_HASH(data), NULL);
}

void sp_cb_hash_eq(void *_p1, void *_p2)
{
  return FALSE;
}

void sp_cb_hash_dump(void *hash, LmnPortRef port)
{
  port_put_raw_s(port, "<hash>");
}

void sp_cp_hash_is_ground(void *data)
{
  return FALSE;
}

void init_hash(void)
{
  hash_atom_type = lmn_sp_atom_register("hash",
					sp_cb_hash_copy,
					sp_cb_hash_free,
					sp_cb_hash_eq,
					sp_cb_hash_dump,
					sp_cp_hash_is_ground);
  lmn_register_c_fun("cb_hash_init", (void *)cb_hash_init, 1);
  lmn_register_c_fun("cb_hash_get", (void *)cb_hash_get, 5);
  lmn_register_c_fun("cb_hash_put", (void *)cb_hash_put, 4);
}
