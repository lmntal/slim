/*
 * array.h - Port API
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

#ifndef LMN_ARRAY_H
#define LMN_ARRAY_H

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"

/**
 * @ingroup  Ext
 * @struct LmnArray array.h "ext/array.h"
 */
class LmnArray {
  LMN_SP_ATOM_HEADER;
  using LmnArrayRef = class LmnArray *;
  /**
   * @memberof LmnArray
   * @private
   */
  static int  array_atom_type; /* special atom type */
  LmnArrayRef lmn_array_copy();
  LmnArray(LmnMembraneRef mem, LmnWord size, LmnAtomRef init_value, LmnLinkAttr init_type);
  LmnArray(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
           LmnAtomRef a2, LmnLinkAttr t2);
  ~LmnArray();
  static void         cb_array_new(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                                   LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2);
  static void         cb_array_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0);
  static void         cb_array_size(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                                    LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2);
  static void         cb_array_get(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                                   LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3);
  static void         cb_array_put(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1_,
                                   LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3, LmnLinkAttr t3);
  static LmnArrayRef *sp_cb_array_new();
  static void        *sp_cb_array_copy(void *data);
  static BOOL         sp_cb_array_is_ground(void *data);
  static void         sp_cb_array_dump(void *array, LmnPortRef port);
  static BOOL         sp_cb_array_eq(void *_p1, void *_p2);
  static void         sp_cb_array_free(void *data);

public:
  // callbacks cannot be methods
  static void init_array();
  // uint32_t size;    /* array size */
  LmnLinkAttr type; /* element type, currently either
                       LMN_INT_ATTR | LMN_DBL_ATTR | LMN_STRING_ATTR */
  BOOL owner;       /* am I the owner of array data? */
  // LmnAtomRef *data; /* array data */
  std::vector<LmnAtomRef> *impl;
};

static void sp_cb_string_dump(void *s, LmnPortRef port); /* これはlmnstring.hにあるべきものでは？？？ */

#define LMN_ARRAY(obj) ((LmnArrayRef)(obj))

#endif
