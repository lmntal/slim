/*
 * state_map.h
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

#ifndef EXT_STATE_MAP_H
#define EXT_STATE_MAP_H

#include "lmntal.h"
#include "element/element.h"
#include "set.h"
#include "verifier/verifier.h"
#include "vm/vm.h"

class LmnStateMap {
  LMN_SP_ATOM_HEADER;
  static int state_map_atom_type;
  typedef class LmnStateMap *LmnStateMapRef;
  StateSpaceRef states;
  LmnStateMap(LmnMembraneRef mem);
  ~LmnStateMap();
  static void cb_state_map_init(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                       LmnLinkAttr t0);
  static void cb_state_map_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                       LmnLinkAttr t0);
  static void cb_state_map_id_find(LmnReactCxtRef rc,
                          LmnMembraneRef mem,
                          LmnAtomRef a0, LmnLinkAttr t0,
                          LmnAtomRef a1, LmnLinkAttr t1,
                          LmnAtomRef a2, LmnLinkAttr t2,
                          LmnAtomRef a3, LmnLinkAttr t3);
  static void cb_state_map_state_find(LmnReactCxtRef rc, LmnMembraneRef mem,
                             LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                             LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2,
                             LmnAtomRef a3, LmnLinkAttr t3);
  static void *sp_cb_state_map_copy(void *data);
  static void sp_cb_state_map_free(void *data);
  static unsigned char sp_cb_state_map_eq(void *_p1, void *_p2);
  static void sp_cb_state_map_dump(void *state_map, LmnPortRef port);
  static unsigned char sp_cb_state_map_is_ground(void *data);
  public:
  static void init_state_map(void);
};



#endif /* EXT_STATE_MAP_H */
