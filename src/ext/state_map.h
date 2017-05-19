#pragma once

#include "lmntal.h"
#include "element/st.h"

#define LMN_STATE_MAP(obj) ((LmnStateMapRef)(obj))

typedef struct LmnStateMap *LmnStateMapRef;

struct LmnStateMap{
  LMN_SP_ATOM_HEADER;
  StateSpaceRef states;
  st_table_t id_tbl;
};
