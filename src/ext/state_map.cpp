/*
 * state_map.c
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

#include "state_map.h"

#include "set.h"
#include "verifier/verifier.h"
#include "vm/vm.h"

int LmnStateMap::state_map_atom_type;

/* constructor */
LmnStateMap::LmnStateMap(LmnMembraneRef mem) {
  LMN_SP_ATOM_SET_TYPE(this, state_map_atom_type);
  this->states = new StateSpace(NULL, NULL);
}
LmnStateMap::~LmnStateMap() {
  delete (this->states);
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 * -a0 Map
 */

void LmnStateMap::cb_state_map_init(
    LmnReactCxtRef rc,
    LmnMembraneRef mem,
    LmnAtomRef a0,
    LmnLinkAttr t0) {
  LmnStateMapRef atom = new LmnStateMap(mem);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  LMN_SP_ATOM_SET_TYPE(atom, state_map_atom_type);
  lmn_mem_push_atom(mem, atom, attr);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), atom, attr, 0);
}

/*
 * 解放
 * +a0 Map
 */

void LmnStateMap::cb_state_map_free(
    LmnReactCxtRef rc,
    LmnMembraneRef mem,
    LmnAtomRef a0,
    LmnLinkAttr t0) {
  delete ((LmnStateMapRef)a0)->states, mem;
  lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a0, t0);
}

/*
 * 状態->ID
 * +a0 Map
 * +a1 状態
 * -a2 ID
 * -a3 Map
 */
void LmnStateMap::cb_state_map_id_find(
    LmnReactCxtRef rc,
    LmnMembraneRef mem,
    LmnAtomRef a0,
    LmnLinkAttr t0,
    LmnAtomRef a1,
    LmnLinkAttr t1,
    LmnAtomRef a2,
    LmnLinkAttr t2,
    LmnAtomRef a3,
    LmnLinkAttr t3) {
  LmnMembraneRef m = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0));
  StateSpaceRef ss = ((LmnStateMap::LmnStateMapRef)a0)->states;
  LmnSymbolAtomRef out = (LmnSymbolAtomRef)a1;
  LmnSymbolAtomRef in = (LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0);
  LmnLinkAttr in_attr = ((LmnSymbolAtomRef)a1)->get_attr(0);

  LmnSymbolAtomRef at =
      lmn_mem_newatom(m, lmn_functor_table->intern(ANONYMOUS, lmn_intern("@"), 1));
  LmnSymbolAtomRef plus = (LmnSymbolAtomRef)in->get_link(1);
  lmn_newlink_in_symbols(plus, 0, at, 0);

  lmn_mem_delete_atom(m, in, in_attr);
  ((MemReactContext *)rc)->memstack_remove(m);
  mem->remove_mem(m);

  State *new_s = new State(m, 0, TRUE);
  State *succ = ss->insert(new_s);

  if (succ == new_s) { /* new state */
    state_id_issue(succ);
  } else {
    delete (new_s);
  }

  lmn_mem_push_atom(mem, succ, LMN_INT_ATTR);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), succ, LMN_INT_ATTR, 0);

  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3, LMN_ATTR_GET_VALUE(t3));

  lmn_mem_delete_atom(mem, a1, t1);
}

/*
 * ID->状態
 * +a0 Map
 * +a1 ID
 * -a2 状態
 * -a3 Map
 */
void LmnStateMap::cb_state_map_state_find(
    LmnReactCxtRef rc,
    LmnMembraneRef mem,
    LmnAtomRef a0,
    LmnLinkAttr t0,
    LmnAtomRef a1,
    LmnLinkAttr t1,
    LmnAtomRef a2,
    LmnLinkAttr t2,
    LmnAtomRef a3,
    LmnLinkAttr t3) {
  State *s = (State *)a1;
  st_data_t entry;

  LmnMembraneRef new_mem = s->duplicate_membrane();
  LmnFunctor at_functor = lmn_functor_table->intern(ANONYMOUS, lmn_intern("@"), 1);

  AtomListEntryRef ent;
  LmnFunctor f;
  LmnSymbolAtomRef at_atom;
  EACH_ATOMLIST_WITH_FUNC(new_mem, ent, f, {
    if (f != at_functor)
      continue;

    LMN_ASSERT(ent->n == 1);
    at_atom = atomlist_head(ent);
  });

  LmnSymbolAtomRef plus = (LmnSymbolAtomRef)at_atom->get_link(0);
  lmn_mem_delete_atom(new_mem, at_atom, 0);

  LmnSymbolAtomRef in = lmn_mem_newatom(new_mem, LMN_IN_PROXY_FUNCTOR);
  LmnSymbolAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);

  lmn_newlink_in_symbols(plus, 0, in, 1);
  lmn_newlink_in_symbols(in, 0, out, 0);

  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), out, LMN_ATTR_MAKE_LINK(1), 1);

  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t1), a3, t3, LMN_ATTR_GET_VALUE(t3));

  mem->add_child_mem(new_mem);
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *LmnStateMap::sp_cb_state_map_copy(void *data) {
  return data;
}

void LmnStateMap::sp_cb_state_map_free(void *data) {
}

unsigned char LmnStateMap::sp_cb_state_map_eq(void *_p1, void *_p2) {
  return 0;
}

void LmnStateMap::sp_cb_state_map_dump(void *state_map, LmnPortRef port) {
  port_put_raw_s(port, "<state_map>");
}

unsigned char LmnStateMap::sp_cb_state_map_is_ground(void *data) {
  return 1;
}

void LmnStateMap::init_state_map(void) {
  state_map_atom_type = lmn_sp_atom_register(
      "state_map",
      sp_cb_state_map_copy,
      sp_cb_state_map_free,
      sp_cb_state_map_eq,
      sp_cb_state_map_dump,
      sp_cb_state_map_is_ground);
  CCallback::lmn_register_c_fun("cb_state_map_init", (void *)cb_state_map_init, 1);
  CCallback::lmn_register_c_fun("cb_state_map_free", (void *)cb_state_map_free, 1);
  CCallback::lmn_register_c_fun("cb_state_map_id_find", (void *)cb_state_map_id_find, 4);
  CCallback::lmn_register_c_fun("cb_state_map_state_find", (void *)cb_state_map_state_find, 4);
}
