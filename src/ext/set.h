#ifndef LMN_SET_H
#define LMN_SET_H

#include <st.h>

struct LmnSet{
  LMN_SP_ATOM_HEADER;
  st_table_t tbl;
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

static struct st_hash_type type_id_hash =
  {
    (st_cmp_func)id_cmp,
    (st_hash_func)id_hash
  };

typedef struct LmnSet *LmnSetRef;

#define LMN_SET(obj) ((LmnSetRef)(obj))

#endif
