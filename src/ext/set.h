#ifndef LMN_SET_H
#define LMN_SET_H

#include "../element/st.h"
#include "element/element.h"
#include "../vm/membrane.h"
#include "../verifier/mem_encode.h"

/**
 * @ingroup  Ext
 * @struct LmnSet set.h "ext/set.h"
 */
struct LmnSet{
  LMN_SP_ATOM_HEADER;
  st_table_t tbl;		/* hash table */
};

/* id set */
static unsigned long id_hash(st_data_t a)
{
  return (unsigned long)a;
}

static int id_cmp(st_data_t a, st_data_t b)
{
  return a != b;
}

struct st_hash_type type_id_hash =
  {
    (st_cmp_func)id_cmp,
    (st_hash_func)id_hash
  };

/* tuple set */
static unsigned long tuple_hash(LmnSymbolAtomRef cons)
{
  unsigned long ret = 0;
  int i;
  for(i = 0; i < LMN_SATOM_GET_ARITY(cons) - 1; i++)
    ret +=(unsigned long)(LMN_SATOM_GET_LINK(cons, i));
  return ret;
}

static int tuple_cmp(LmnSymbolAtomRef cons0, LmnSymbolAtomRef cons1)
{
  int num0 = LMN_SATOM_GET_ARITY(cons0);
  int num1 = LMN_SATOM_GET_ARITY(cons1);
  int i;
  int ret = 0;
  if(num0 != num1)
    return 1;
  for(i = 0; i < num0 - 1; i++)
    ret = ret || (LMN_SATOM_GET_LINK(cons0, i) != LMN_SATOM_GET_LINK(cons1, i));
  return ret;
}

struct st_hash_type type_tuple_hash =
  {
    (st_cmp_func)tuple_cmp,
    (st_hash_func)tuple_hash
  };


/* mem set */
static LmnBinStrRef lmn_inner_mem_encode(LmnMembraneRef m)
{
  AtomListEntryRef ent;
  LmnFunctor f;
  LmnAtomRef in;
  LmnAtomRef out;
  LmnAtomRef plus;
  LmnBinStrRef s;

  EACH_ATOMLIST_WITH_FUNC(m, ent, f, ({
  	LmnAtomRef satom;
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

  LmnAtomRef at = lmn_mem_newatom(m, lmn_functor_intern(ANONYMOUS, lmn_intern("@"), 1));
  lmn_newlink_in_symbols(plus, 0, at, 0);

  s = lmn_mem_encode(m);

  mem_remove_symbol_atom(m, at);
  lmn_delete_atom(at);

  in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
  lmn_newlink_in_symbols(in, 0, out, 0);
  lmn_newlink_in_symbols(in, 1, plus, 0);

  return s;
}

static int mem_cmp(LmnMembraneRef m0, LmnMembraneRef m1)
{
  LmnBinStrRef s0 = lmn_inner_mem_encode(m0);
  LmnBinStrRef s1 = lmn_inner_mem_encode(m1);
  int res = binstr_compare(s0, s1);
  lmn_binstr_free(s0);
  lmn_binstr_free(s1);
  return res;
}

static unsigned long mem_hash(LmnMembraneRef m)
{
  return mhash(m);
}

struct st_hash_type type_mem_hash =
  {
    (st_cmp_func)mem_cmp,
    (st_hash_func)mem_hash
  };

typedef struct LmnSet *LmnSetRef;

#define LMN_SET(obj) ((LmnSetRef)(obj))

#endif
