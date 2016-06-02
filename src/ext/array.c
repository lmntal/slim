/*
 * array.c - array implementation
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

#include "array.h"
#include "lmntal_ext.h"
#include "special_atom.h"
#include "utility/util.h"
#include "slim_header/string.h"
#include "dumper.h"
#include "atom.h"

static int array_atom_type; /* special atom type */

#define LMN_ARRAY_SIZE(obj) (LMN_ARRAY(obj)->size)
#define LMN_ARRAY_TYPE(obj) (LMN_ARRAY(obj)->type)
#define LMN_ARRAY_OWNER(obj) (LMN_ARRAY(obj)->owner)
#define LMN_ARRAY_DATA(obj) (LMN_ARRAY(obj)->data)

/*
 * Internal Constructor
 */
static LmnArray make_array(LmnMembrane *mem, LmnAtom size, LmnAtom init_value, LmnLinkAttr init_type)
{
  unsigned long i;
  LmnAtom v;

  LmnArray a = LMN_MALLOC(struct LmnArray);
  LMN_SP_ATOM_SET_TYPE(a, array_atom_type);
  LMN_ARRAY_SIZE(a) = size;
  LMN_ARRAY_TYPE(a) = init_type;
  LMN_ARRAY_OWNER(a) = TRUE;
  LMN_ARRAY_DATA(a) = LMN_NALLOC(LmnWord, size);
  switch (init_type) {
    case LMN_INT_ATTR:
      for (i = 0; i < size; i++) {
        LMN_ARRAY_DATA(a)[i] = (long)init_value;
      }
      break;
    case LMN_DBL_ATTR:
      for (i = 0; i < size; i++) {
        LMN_COPY_DBL_ATOM(v, init_value);
        LMN_ARRAY_DATA(a)[i] = LMN_WORD(v);
      }
      break;
    case LMN_STRING_ATTR:
      for (i = 0; i < size; i++) {
        LMN_ARRAY_DATA(a)[i] = LMN_WORD(lmn_string_copy(LMN_STRING(init_value)));
        // copied, could be shared
      }
      break;
    case LMN_HL_ATTR:
      LMN_ARRAY_DATA(a)[0]=init_value;
      for (i = 1; i < size; i++) {
        LMN_ARRAY_DATA(a)[i] = lmn_copy_atom(init_value, init_type);
        lmn_mem_push_atom(mem, LMN_ARRAY_DATA(a)[i], init_type);
      }
      break;

    default: lmn_fatal("array of this element type not implemented");
  }
  return a;
}

LmnArray lmn_make_array(LmnMembrane *mem, LmnAtom size, LmnAtom init_value, LmnLinkAttr init_type)
{
  return make_array(mem, size, init_value, init_type);
}

void lmn_array_free(LmnArray array, LmnMembrane *mem)
{
  unsigned long i;

  if (LMN_ARRAY_OWNER(array)) {
    switch (LMN_ARRAY_TYPE(array)) {
      case LMN_DBL_ATTR:
        for (i = 0; i < LMN_ARRAY_SIZE(array); i++) {
          lmn_free_atom(LMN_ARRAY_DATA(array)[i], LMN_DBL_ATTR);
        }
        break;
      case LMN_STRING_ATTR:
        for (i = 0; i < LMN_ARRAY_SIZE(array); i++) {
          lmn_string_free(LMN_STRING(LMN_ARRAY_DATA(array)[i]));
        }
        break;
      case LMN_HL_ATTR:
        for (i = 0; i < LMN_ARRAY_SIZE(array); i++) {
          lmn_free_atom((LmnAtom)LMN_ARRAY_DATA(array)[i],LMN_HL_ATTR);
        }
        break;
    }
    LMN_FREE(LMN_ARRAY_DATA(array));
  }
  LMN_FREE(array);
}

LmnArray lmn_array_copy(LmnArray array)
{
  /* copy the descriptor and share data */
  /* ownership is tranferred to the new copy and the old copy */
  /* is supposed NOT to be used, just waiting to be freed first */

  if (!LMN_ARRAY_OWNER(array)) lmn_fatal("attempt to copy old array");

  LmnArray a = LMN_MALLOC(struct LmnArray);
  memcpy(a, array, sizeof(struct LmnArray));
  /*
     LMN_SP_ATOM_SET_TYPE(a, array_atom_type);
     LMN_ARRAY_SIZE(a) = LMN_ARRAY_SIZE(array);
     LMN_ARRAY_TYPE(a) = LMN_ARRAY_TYPE(array);
     LMN_ARRAY_OWNER(a) = LMN_ARRAY_OWNER(array);
     LMN_ARRAY_DATA(a) = LMN_ARRAY_DATA(array);
     */
  LMN_ARRAY_OWNER(array) = FALSE;

  return a;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 *
 * +a0: 要素数
 * +a1: 初期値
 * -a2: 配列
 */
void cb_array_new(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0,
    LmnAtom a1, LmnLinkAttr t1,
    LmnAtom a2, LmnLinkAttr t2)
{
  /* a0 is assumed to be an integer data atom */
  LmnArray atom = lmn_make_array(mem, a0, a1, t1);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  if(t1==LMN_HL_ATTR){
    unsigned long i;
    for(i = 0 ; i < a0 ; i++){
      lmn_mem_newlink(mem,
          LMN_ATOM(atom), attr, 0,
          LMN_ARRAY_DATA(atom)[i], t1, 0);
    }
  }
  lmn_mem_newlink(mem,
      a2, t2, LMN_ATTR_GET_VALUE(t2),
      LMN_ATOM(atom), attr, 0);
}

/*
 * 解放
 *
 * +a0: 配列
 */
void cb_array_free(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0)
{
  lmn_array_free(LMN_ARRAY(a0), mem);
  lmn_mem_remove_data_atom(mem, a0, t0);
}

/*
 * 要素数取得
 *
 * +a0: 配列
 * -a1: 要素数
 * -a2: 新配列
 */
void cb_array_size(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0,
    LmnAtom a1, LmnLinkAttr t1,
    LmnAtom a2, LmnLinkAttr t2)
{
  long s = LMN_ARRAY_SIZE(a0);

  lmn_mem_newlink(mem,
      a1, t1, LMN_ATTR_GET_VALUE(t1),
      s, LMN_INT_ATTR, 0);
  lmn_mem_newlink(mem,
      a0, t0, LMN_ATTR_GET_VALUE(t0),
      a2, t2, LMN_ATTR_GET_VALUE(t2));
  lmn_mem_push_atom(mem, LMN_ATOM(s), LMN_INT_ATTR);
}

/*
 * 要素取得
 *
 * +a0: 配列
 * +a1: 添字
 * -a2: 要素値
 * -a3: 新配列
 */
void cb_array_get(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0,
    LmnAtom a1, LmnLinkAttr t1,
    LmnAtom a2, LmnLinkAttr t2,
    LmnAtom a3, LmnLinkAttr t3)
{
  LmnAtom ai;
  LmnAtom v;

  if (a1 < LMN_ARRAY_SIZE(a0)) {   /* a1 is unsigned and hence nonnegative */
    switch (LMN_ARRAY_TYPE(a0)) {
      case LMN_INT_ATTR:
        ai = LMN_ATOM(LMN_ARRAY_DATA(a0)[a1]);
        break;
      case LMN_DBL_ATTR:
        LMN_COPY_DBL_ATOM(v, LMN_ARRAY_DATA(a0)[a1]);
        ai = v;
        break;
      case LMN_HL_ATTR:
        ai = lmn_copy_atom(LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR);
        break;
      default: /* must be LMN_STRINGL_ATTR) */
        ai = LMN_ATOM(lmn_string_copy(LMN_STRING(LMN_ARRAY_DATA(a0)[a1])));
        break;
    }

    lmn_mem_newlink(mem,
        a2, t2, LMN_ATTR_GET_VALUE(t2),
        ai, LMN_ARRAY_TYPE(a0), 0);
    lmn_mem_push_atom(mem, ai, LMN_ARRAY_TYPE(a0));
    lmn_mem_newlink(mem,
        a0, t0, LMN_ATTR_GET_VALUE(t0),
        a3, t3, LMN_ATTR_GET_VALUE(t3));

    lmn_mem_remove_data_atom(mem, a1, t1);
  }
  else lmn_fatal("array.get out of bound");
}

/*
 * 要素変更
 *
 * +a0: 配列
 * +a1: 添字
 * +a2: 要素値
 * -a3: 新配列
 */
void cb_array_put(LmnReactCxt *rc,
    LmnMembrane *mem,
    LmnAtom a0, LmnLinkAttr t0,
    LmnAtom a1, LmnLinkAttr t1,
    LmnAtom a2, LmnLinkAttr t2,
    LmnAtom a3, LmnLinkAttr t3)
{
  if (a1 < LMN_ARRAY_SIZE(a0)) {   /* a1 is unsigned and hence nonnegative */
    if (LMN_ARRAY_TYPE(a0) == t2) {
      switch (LMN_ARRAY_TYPE(a0)) {
        case LMN_INT_ATTR:
          break;
        case LMN_DBL_ATTR:
          lmn_free_atom(LMN_ARRAY_DATA(a0)[a1], LMN_DBL_ATTR);
          break;
        case LMN_STRING_ATTR:
          lmn_string_free(LMN_STRING(LMN_ARRAY_DATA(a0)[a1]));
          break;
        case LMN_HL_ATTR:
          lmn_mem_remove_atom(mem, (LmnAtom)LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR);
          lmn_free_atom((LmnAtom)LMN_ARRAY_DATA(a0)[a1],LMN_HL_ATTR);

          break;
      }
    } else lmn_fatal("array.put type mismatch");

    LMN_ARRAY_DATA(a0)[a1] = a2;

    if(LMN_ARRAY_TYPE(a0)==LMN_HL_ATTR){

      lmn_mem_newlink(mem,
          a0, t0, LMN_ATTR_GET_VALUE(t0),
          LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR, 0);

    }
    lmn_mem_newlink(mem,
        a0, t0, LMN_ATTR_GET_VALUE(t0),
        a3, t3, LMN_ATTR_GET_VALUE(t3));
    lmn_mem_remove_data_atom(mem, a1, t1);
    if(!LMN_ARRAY_TYPE(a0)==LMN_HL_ATTR)lmn_mem_remove_data_atom(mem, a2, t2);
  }
  else lmn_fatal("array.put out of bound");  
}


/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_array_copy(void *data)
{
  return lmn_array_copy(LMN_ARRAY(data));
}

void sp_cb_array_free(void *data)
{
  lmn_array_free(LMN_ARRAY(data),NULL);
}

/* てきとーに定義した */
BOOL sp_cb_array_eq(void *_p1, void *_p2)
{
  return FALSE;
}

void sp_cb_array_dump(void *array, LmnPort port)
{
  unsigned long i, size;
  LmnLinkAttr type;
  LmnWord *data;
  char buf[64];

  port_put_raw_s(port, "<");
  size = LMN_ARRAY_SIZE(array);
  type = LMN_ARRAY_TYPE(array);
  data = LMN_ARRAY_DATA(array);
  if (size > 0) {
    if (type == LMN_INT_ATTR) {
      port_put_raw_s(port, int_to_str(data[0]));
    } else if (type == LMN_DBL_ATTR) {
      sprintf(buf, "%#g", lmn_get_double(data[0]));
      port_put_raw_s(port, buf);
    } else if (type == LMN_HL_ATTR) {
      lmn_dump_atom(port, data[0], type);
    }else {
      sp_cb_string_dump(data[0], port);
    }
    for (i = 1; i < size ; i++) {
      port_put_raw_s(port, ",");
      if (type == LMN_INT_ATTR) {
        port_put_raw_s(port, int_to_str(data[i]));
      } else if (type == LMN_DBL_ATTR) {
        sprintf(buf, "%#g", lmn_get_double(data[i]));
        port_put_raw_s(port, buf);
      } else if (type == LMN_HL_ATTR) {
        lmn_dump_atom(port, data[i], type);
      } else {
        sp_cb_string_dump(data[i], port);
      }
    }
  }
  port_put_raw_s(port, ">");
}

BOOL sp_cp_array_is_ground(void *data)
{
  return FALSE;  /* since deep copying is not implemented */
}

void init_array()
{
  array_atom_type = lmn_sp_atom_register("array",
      sp_cb_array_copy,
      sp_cb_array_free,
      sp_cb_array_eq,
      sp_cb_array_dump,
      sp_cp_array_is_ground);

  lmn_register_c_fun("cb_array_free", cb_array_free, 1);
  lmn_register_c_fun("cb_array_new", cb_array_new, 3);
  lmn_register_c_fun("cb_array_size", cb_array_size, 3);
  lmn_register_c_fun("cb_array_get", cb_array_get, 4);
  lmn_register_c_fun("cb_array_put", cb_array_put, 4);
}

/*
void array_finalize()
{
}
*/
