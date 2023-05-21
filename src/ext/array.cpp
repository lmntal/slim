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
#include <string>

#include "array.h"
#include "element/element.h"
#include "element/lmnstring.cpp"
#include "vm/vm.h"
int LmnArray::array_atom_type;
/**
 * @memberof LmnArray
 * @private
 */
#define LMN_ARRAY_TYPE(obj) (LMN_ARRAY(obj)->type)
/**
 * @memberof LmnArray
 * @private
 */
#define LMN_ARRAY_OWNER(obj) (LMN_ARRAY(obj)->owner)
/**
 * @memberof LmnArray
 * @private
 */
#define LMN_ARRAY_DATA(obj) (*(LMN_ARRAY(obj)->impl))

/**
 * @memberof LmnArray
 * @private
 */
LmnArray::~LmnArray() {
  unsigned long i;

  if (LMN_ARRAY_OWNER(this)) {
    switch (LMN_ARRAY_TYPE(this)) {
    case LMN_DBL_ATTR:
      for (i = 0; i < this->impl->size(); i++) {
        lmn_free_atom((*(this->impl))[i], LMN_DBL_ATTR);
      }
      break;
    case LMN_STRING_ATTR:
      for (i = 0; i < this->impl->size(); i++) {
        delete (reinterpret_cast<LmnString *>(LMN_ARRAY_DATA(this)[i]));
      }
      break;
    case LMN_HL_ATTR:
      for (i = 0; i < this->impl->size(); i++) {
        lmn_free_atom(LMN_ARRAY_DATA(this)[i], LMN_HL_ATTR);
      }
      break;
    }
    LMN_FREE(this->impl);
  }
}

/**
 * @memberof LmnArray
 * @private
 */
LmnArray::LmnArrayRef LmnArray::lmn_array_copy() {
  /* copy the descriptor and share data */
  /* ownership is tranferred to the new copy and the old copy */
  /* is supposed NOT to be used, just waiting to be freed first */

  if (!LMN_ARRAY_OWNER(this))
    lmn_fatal("attempt to copy old array");

  LmnArrayRef a = LMN_MALLOC<class LmnArray>();
  memcpy(a, this, sizeof(class LmnArray));
  /*
     LMN_SP_ATOM_SET_TYPE(a, array_atom_type);
     a->impl.size() = array->impl.size();
     LMN_ARRAY_TYPE(a) = LMN_ARRAY_TYPE(array);
     LMN_ARRAY_OWNER(a) = LMN_ARRAY_OWNER(array);
     a->impl = array->impl;
     */
  LMN_ARRAY_OWNER(this) = FALSE;

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
/**
 * @memberof LmnArray
 * @private
 */
LmnArray::LmnArray(LmnMembraneRef mem, LmnWord size, LmnAtomRef init_value, LmnLinkAttr init_type) {
  unsigned long i;
  LmnAtomRef    v;

  LMN_SP_ATOM_SET_TYPE(this, array_atom_type);
  this->impl = new std::vector<LmnAtomRef>();
  this->impl->resize(size);
  LMN_ARRAY_TYPE(this)  = init_type;
  LMN_ARRAY_OWNER(this) = TRUE;
  switch (init_type) {
  case LMN_INT_ATTR:
    for (unsigned long i = 0; i < size; i++) {
      LMN_ARRAY_DATA(this)[i] = (LmnAtomRef)init_value;
    }
    break;
  case LMN_DBL_ATTR:
    for (i = 0; i < size; i++) {
      v                       = (LmnAtomRef)lmn_create_double_atom(lmn_get_double((LmnDataAtomRef)init_value));
      LMN_ARRAY_DATA(this)[i] = v;
    }
    break;
  case LMN_STRING_ATTR:
    for (i = 0; i < size; i++) {
      LMN_ARRAY_DATA(this)
      [i] = new LmnString(*reinterpret_cast<LmnString *>(init_value));
      // copied, could be shared
    }
    break;
  case LMN_HL_ATTR:
    LMN_ARRAY_DATA(this)[0] = init_value;
    for (i = 1; i < size; i++) {
      LMN_ARRAY_DATA(this)[i] = lmn_copy_atom(init_value, init_type);
      lmn_mem_push_atom(mem, LMN_ARRAY_DATA(this)[i], init_type);
    }
    break;

  default:
    lmn_fatal("array of this element type not implemented");
  }
}

LmnArray::LmnArray(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                   LmnAtomRef a2, LmnLinkAttr t2) {
  LmnWord     size = (LmnWord)a0; /**< a0 is assumed to be an integer data atom */
  LmnArrayRef atom = new LmnArray(mem, size, a1, t1);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  lmn_mem_push_atom(mem, atom, attr);
  if (t1 == LMN_HL_ATTR) {
    unsigned long i;
    for (i = 0; i < size; i++) {
      lmn_mem_newlink(mem, atom, attr, 0, LMN_ARRAY_DATA(atom)[i], t1, 0);
    }
  }
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), atom, attr, 0);
}

/*
 * new
 *
 * +a0: size
 * +a1: initial value
 * -a2: array
 */
/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::cb_array_new(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                            LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnWord     size = (LmnWord)a0; /**< a0 is assumed to be an integer data atom */
  LmnArrayRef atom = new LmnArray(mem, size, a1, t1);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  lmn_mem_push_atom(mem, atom, attr);
  if (t1 == LMN_HL_ATTR) {
    unsigned long i;
    for (i = 0; i < size; i++) {
      lmn_mem_newlink(mem, atom, attr, 0, LMN_ARRAY_DATA(atom)[i], t1, 0);
    }
  }
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), atom, attr, 0);
}

/*
 * 解放
 *
 * +a0: 配列
 */
/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::cb_array_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a0, t0);
  delete LMN_ARRAY(a0);
}

/*
 * 要素数取得
 *
 * +a0: 配列
 * -a1: 要素数
 * -a2: 新配列
 */
/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::cb_array_size(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                             LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnAtomRef s = (LmnAtomRef)(LmnWord)LMN_ARRAY_DATA(a0).size();

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), s, LMN_INT_ATTR, 0);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a2, t2, LMN_ATTR_GET_VALUE(t2));
  lmn_mem_push_atom(mem, (LmnAtomRef)s, LMN_INT_ATTR);
}

/*
 * 要素取得
 *
 * +a0: 配列
 * +a1: 添字
 * -a2: 要素値
 * -a3: 新配列
 */
/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::cb_array_get(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                            LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3) {
  LmnAtomRef ai;
  LmnWord    i = (LmnWord)a1;

  if (i < LMN_ARRAY_DATA(a0).size()) { /* i is unsigned and hence nonnegative */
    switch (LMN_ARRAY_TYPE(a0)) {
    case LMN_INT_ATTR:
      ai = LMN_ARRAY_DATA(a0)[i];
      break;
    case LMN_DBL_ATTR:
      ai = (LmnAtomRef)lmn_create_double_atom(lmn_get_double((LmnDataAtomRef)LMN_ARRAY_DATA(a0)[i]));
      break;
    case LMN_HL_ATTR:
      ai = lmn_copy_atom(LMN_ARRAY_DATA(a0)[i], LMN_HL_ATTR);
      break;
    default: /* must be LMN_STRINGL_ATTR) */
      ai = new LmnString(*reinterpret_cast<LmnString *>(LMN_ARRAY_DATA(a0)[i]));
      break;
    }

    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), ai, LMN_ARRAY_TYPE(a0), 0);
    lmn_mem_push_atom(mem, ai, LMN_ARRAY_TYPE(a0));
    lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3, LMN_ATTR_GET_VALUE(t3));

    lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a1, t1);
  } else
    lmn_fatal("array.get out of bound");
}

/*
 * 要素変更
 *
 * +a0: 配列
 * +a1: 添字
 * +a2: 要素値
 * -a3: 新配列
 */
/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::cb_array_put(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1_,
                            LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3) {
  LmnWord a1 = (LmnWord)a1_;
  if (a1 < LMN_ARRAY_DATA(a0).size()) { /* a1 is unsigned and hence nonnegative */
    if (LMN_ARRAY_TYPE(a0) == t2) {
      switch (LMN_ARRAY_TYPE(a0)) {
      case LMN_INT_ATTR:
        break;
      case LMN_DBL_ATTR:
        lmn_free_atom(LMN_ARRAY_DATA(a0)[a1], LMN_DBL_ATTR);
        break;
      case LMN_STRING_ATTR:
        delete (reinterpret_cast<LmnString *>(LMN_ARRAY_DATA(a0)[a1]));
        break;
      case LMN_HL_ATTR:
        lmn_mem_remove_atom(mem, (LmnAtomRef)LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR);
        lmn_free_atom((LmnAtomRef)LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR);

        break;
      }
    } else
      lmn_fatal("array.put type mismatch");

    LMN_ARRAY_DATA(a0)[a1] = a2;

    if (LMN_ARRAY_TYPE(a0) == LMN_HL_ATTR) {

      lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), LMN_ARRAY_DATA(a0)[a1], LMN_HL_ATTR, 0);
    }
    lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3, LMN_ATTR_GET_VALUE(t3));
    lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a1, t1);
    if (LMN_ARRAY_TYPE(a0) != LMN_HL_ATTR)
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a2, t2);
  } else
    lmn_fatal("array.put out of bound");
}

/*----------------------------------------------------------------------
 * Initialization
 */

/**
 * @memberof LmnArray
 * @private
 */
void *LmnArray::sp_cb_array_copy(void *data) { return LMN_ARRAY(data)->lmn_array_copy(); }

/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::sp_cb_array_free(void *data) { delete LMN_ARRAY(data); }

/* てきとーに定義した */
/**
 * @memberof LmnArray
 * @private
 */
BOOL LmnArray::sp_cb_array_eq(void *_p1, void *_p2) { return FALSE; }

/**
 * @memberof LmnArray
 * @private
 */
void LmnArray::sp_cb_array_dump(void *array, LmnPortRef port) {
  unsigned long i, size;
  LmnLinkAttr   type;
  LmnAtomRef   *data;
  char          buf[64];

  port_put_raw_s(port, "<");
  size = LMN_ARRAY_DATA(array).size();
  type = LMN_ARRAY_TYPE(array);
  data = LMN_ARRAY_DATA(array).data();
  if (size > 0) {
    if (type == LMN_INT_ATTR) {
      port_put_raw_s(port, std::to_string((LmnWord)data[0]));
    } else if (type == LMN_DBL_ATTR) {
      sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)data[0]));
      port_put_raw_s(port, buf);
    } else if (type == LMN_HL_ATTR) {
      lmn_dump_atom(port, data[0], type);
    } else {
      sp_cb_string_dump((void *)data[0], port);
    }
    for (i = 1; i < size; i++) {
      port_put_raw_s(port, ",");
      if (type == LMN_INT_ATTR) {
        port_put_raw_s(port, std::to_string((LmnWord)data[i]));
      } else if (type == LMN_DBL_ATTR) {
        sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)data[i]));
        port_put_raw_s(port, buf);
      } else if (type == LMN_HL_ATTR) {
        lmn_dump_atom(port, (LmnAtomRef)data[i], type);
      } else {
        sp_cb_string_dump((void *)data[i], port);
      }
    }
  }
  port_put_raw_s(port, ">");
}

/**
 * @memberof LmnArray
 * @private
 */
BOOL LmnArray::sp_cb_array_is_ground(void *data) { return FALSE; /* since deep copying is not implemented */ }

/**
 * @memberof LmnArray
 * @private
 */

void LmnArray::init_array() {
  array_atom_type =
      lmn_sp_atom_register("array", LmnArray::sp_cb_array_copy, LmnArray::sp_cb_array_free, LmnArray::sp_cb_array_eq,
                           LmnArray::sp_cb_array_dump, LmnArray::sp_cb_array_is_ground);
  CCallback::lmn_register_c_fun("cb_array_free", (void *)LmnArray::cb_array_free, 1);
  CCallback::lmn_register_c_fun("cb_array_new", (void *)LmnArray::cb_array_new, 3);
  CCallback::lmn_register_c_fun("cb_array_size", (void *)LmnArray::cb_array_size, 3);
  CCallback::lmn_register_c_fun("cb_array_get", (void *)LmnArray::cb_array_get, 4);
  CCallback::lmn_register_c_fun("cb_array_put", (void *)LmnArray::cb_array_put, 4);
}

/*
void array_finalize()
{
}
*/
