#pragma once

#include "../lmntal.h"
#include "../element/st.h"

#define LMN_STATE_MAP(obj) ((LmnStateMapRef)(obj))

typedef struct LmnStateMap *LmnStateMapRef;

struct LmnStateMap{
  LMN_SP_ATOM_HEADER;
  StateSpaceRef states;
  st_table_t id_tbl;
};

static int id_cmp(int a, int b)
{
  return a != b;
}

static unsigned long id_hash(int a)
{
  return (unsigned long)a;
}

static struct st_hash_type type_id_hash =
  {
    (st_cmp_func)id_cmp,
    (st_hash_func)id_hash
  };
