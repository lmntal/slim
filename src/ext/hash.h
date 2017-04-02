#ifndef LMN_HASH_H
#define LMN_HASH_H

#include "../lmntal.h"
#include <util.h>
#include <st.h>
#include "../verifier/mem_encode.h"
#include "../verifier/statespace.h"
#include "../atom.h"
#include "../membrane.h"
#include "../verifier/state.h"
struct LmnHash{
  LMN_SP_ATOM_HEADER;		/* スペシャルアトムの約束事 */
  st_table_t tbl;		/* ハッシュ本体 */
};

struct LmnStateMap{
  LMN_SP_ATOM_HEADER;
  StateSpaceRef states;
  st_table_t id_tbl;
};


// 膜mの内側のプロセスのみを対象にバイナリストリングを計算する
// 注意(1)!!! 膜mは膜外への自由リンクを必ず1本のみ持っていることが条件
// 注意(2)!!! 膜内に本来存在しないアトム(atアトム)を入れてバイナリストリングを計算しているため，
//            比較の際は相手の膜もlmn_inner_mem_encodeでエンコードされている必要がある
static LmnBinStrRef lmn_inner_mem_encode(LmnMembrane *m)
{
  AtomListEntry *ent;
  LmnFunctor f;
  LmnSAtom in;
  LmnSAtom out;
  LmnSAtom plus;
  LmnBinStrRef s;

  EACH_ATOMLIST_WITH_FUNC(m, ent, f, ({
  	LmnSAtom satom;
  	EACH_ATOM(satom, ent, ({
  	      if(f==LMN_UNARY_PLUS_FUNCTOR){
		plus = satom;
  		in = LMN_SATOM(LMN_SATOM_GET_LINK(plus, 0));
  		out = LMN_SATOM(LMN_SATOM_GET_LINK(in, 0));
  	      }
  	    }))
      }));

  mem_remove_symbol_atom(m, in);
  lmn_delete_atom(in);

  LmnSAtom at = lmn_mem_newatom(m, lmn_functor_intern(ANONYMOUS, lmn_intern("@"), 1));
  lmn_newlink_in_symbols(plus, 0, at, 0);

  s = lmn_mem_encode(m);

  mem_remove_symbol_atom(m, at);
  lmn_delete_atom(at);

  in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
  lmn_newlink_in_symbols(in, 0, out, 0);
  lmn_newlink_in_symbols(in, 1, plus, 0);

  return s;
}

static int inner_mem_cmp(LmnMembrane *m0, LmnMembrane *m1)
{
  LmnBinStrRef s0 = lmn_inner_mem_encode(m0);
  LmnBinStrRef s1 = lmn_inner_mem_encode(m1);
  int res = binstr_compare(s0, s1);
  lmn_binstr_free(s0);
  lmn_binstr_free(s1);
  return res;
}

static unsigned long mem_hash(LmnMembrane *m)
{
  return mhash(m);
}

static unsigned long id_hash(int a)
{
  return (unsigned long)a;
}

static int id_cmp(int a, int b)
{
  return a != b;
}

static tuple_hash(LmnSAtom cons)
{
  int x = LMN_SATOM_GET_LINK(cons, 0);
  int y = LMN_SATOM_GET_LINK(cons, 1);
  return x+y;
}

static tuple_cmp(LmnSAtom cons0, LmnSAtom cons1)
{
  int x0 = LMN_SATOM_GET_LINK(cons0, 0);
  int y0 = LMN_SATOM_GET_LINK(cons0, 1);
  int x1 = LMN_SATOM_GET_LINK(cons1, 0);
  int y1 = LMN_SATOM_GET_LINK(cons1, 1);
  return ((x0 != x1) && (y0 != y1));
}

static struct st_hash_type type_mem_hash =
  {
    (st_cmp_func)inner_mem_cmp,
    (st_hash_func)mem_hash
  };

static struct st_hash_type type_id_hash =
  {
    (st_cmp_func)id_cmp,
    (st_hash_func)id_hash
  };

static struct st_hash_type type_tuple_hash =
  {
    (st_cmp_func)tuple_cmp,
    (st_hash_func)tuple_hash
  };

typedef struct LmnHash *LmnHashRef;
typedef struct LmnStateMap *LmnStateMapRef;

#define LMN_HASH(obj) ((LmnHashRef)(obj))
#define LMN_STATE_MAP(obj) ((LmnStateMapRef)(obj))

#endif
  
