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
#include <iostream>
#include <thread>
#include <vector>
#include <mutex>

int LmnStateMap::state_map_atom_type;
struct Profile_state_map {
  long long id_find_num;
  long long graph_find_num;
  double id_find_time;
  double graph_find_time;
  Profile_state_map() {
    id_find_num = 0;
    graph_find_num = 0;
    id_find_time = 0;
    graph_find_time = 0;
  };
};

Profile_state_map psm;
/* constructor */
LmnStateMap::LmnStateMap(LmnMembraneRef mem) {
  LMN_SP_ATOM_SET_TYPE(this, state_map_atom_type);
  this->states = new StateSpace(NULL, NULL);
}
LmnStateMap::~LmnStateMap() { delete (this->states); }

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 * -a0 Map
 */

void LmnStateMap::cb_state_map_init(LmnReactCxtRef rc, LmnMembraneRef mem,
                                    LmnAtomRef a0, LmnLinkAttr t0) {
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

void LmnStateMap::cb_state_map_free(LmnReactCxtRef rc, LmnMembraneRef mem,
                                    LmnAtomRef a0, LmnLinkAttr t0) {
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
void LmnStateMap::cb_state_map_id_find(LmnReactCxtRef rc, LmnMembraneRef mem,
                                       LmnAtomRef a0, LmnLinkAttr t0,
                                       LmnAtomRef a1, LmnLinkAttr t1,
                                       LmnAtomRef a2, LmnLinkAttr t2,
                                       LmnAtomRef a3, LmnLinkAttr t3) {
  psm.id_find_num++;
  auto start = get_wall_time();
  LmnMembraneRef m =
      LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0));
  StateSpaceRef ss = ((LmnStateMap::LmnStateMapRef)a0)->states;
  LmnSymbolAtomRef out = (LmnSymbolAtomRef)a1;
  LmnSymbolAtomRef in = (LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0);
  // LmnLinkAttr in_attr = ((LmnSymbolAtomRef)a1)->get_attr(0);
  // LmnSymbolAtomRef at = lmn_mem_newatom(m,
  // lmn_functor_table->intern(ANONYMOUS, lmn_intern("@"), 1));
  // LmnSymbolAtomRef plus = (LmnSymbolAtomRef)in->get_link(1);
  lmn_mem_delete_atom(m, ((LmnSymbolAtomRef)in)->get_link(1),
                      ((LmnSymbolAtomRef)in)->get_attr(1));
  lmn_mem_delete_atom(m, in, ((LmnSymbolAtomRef)out)->get_attr(0));

  // lmn_newlink_in_symbols(plus, 0, at, 0);

  // lmn_mem_delete_atom(m, in, in_attr);
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

  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a3, t3,
                  LMN_ATTR_GET_VALUE(t3));

  lmn_mem_delete_atom(mem, a1, t1);
  auto finish = get_wall_time();
  psm.id_find_time += finish - start;
}
std::mutex insert_para_mtx;
static void insert_state_para(LmnReactCxtRef rc, LmnMembraneRef mem, unsigned int id,
                              StateSpaceRef ss, int num, LmnAtomRef begin,
                              std::vector<LmnMembraneRef> *vm,
                              std::vector<LmnAtomRef> *vo,
                              std::vector<long long> *vs) {
  printf("%s:%d thread id = %d\n", __FUNCTION__, __LINE__, id);

  // if (lmn_env.normal_para) {
  //   env_my_TLS_init(id);
  //   lmn_thread_set_CPU_affinity(id);
  // }
  std::lock_guard<std::mutex> lock(insert_para_mtx);
  LmnAtomRef it = begin;
  while (((LmnSymbolAtomRef)it)->get_functor() != LMN_NIL_FUNCTOR and num > 0) {
    LmnAtomRef head = ((LmnSymbolAtomRef)it)->get_link(0);
    while (((LmnSymbolAtomRef)head)->get_functor() != LMN_NIL_FUNCTOR) {
      LmnAtomRef out = ((LmnSymbolAtomRef)head)->get_link(0);
      LmnAtomRef in = ((LmnSymbolAtomRef)out)->get_link(0);
      LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
      // delete plus
      lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in)->get_link(1),
                          ((LmnSymbolAtomRef)in)->get_attr(1));
      // delete in
      lmn_mem_delete_atom(graph_mem, in, ((LmnSymbolAtomRef)out)->get_attr(0));

      ((MemReactContext *)rc)->memstack_remove(graph_mem);
      mem->remove_mem(graph_mem);

      State *new_s = new State(graph_mem, 0, TRUE);
      State *succ = ss->insert(new_s);

      if (succ != new_s) {
        delete (new_s);
      }
      lmn_mem_delete_atom(mem, out, ((LmnSymbolAtomRef)head)->get_attr(0));
      lmn_mem_newlink(mem, head, ((LmnSymbolAtomRef)head)->get_attr(0), 0, succ,
                      LMN_INT_ATTR, 0);
      // vm->push_back(graph_mem);
      // vo->push_back(out);
      // vs->push_back((long long)succ);
      lmn_mem_push_atom(mem, succ, LMN_INT_ATTR);
      // delete out

      head = ((LmnSymbolAtomRef)head)->get_link(1);
    }
    num--;
    it = ((LmnSymbolAtomRef)it)->get_link(1);
  }
  printf("%s:%d thread id = %d\n", __FUNCTION__, __LINE__, id);
}

/*
 * 状態->ID
 * +a0 Map
 * +a1 状態のリストのリスト
 * +a2 リストの長さ
 * -a3 IDのリストのリスト
 * -a4 Map
 */
void LmnStateMap::cb_state_map_id_find_para(
    LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0,
    LmnAtomRef a1, LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2, LmnAtomRef a3,
    LmnLinkAttr t3, LmnAtomRef a4, LmnLinkAttr t4) {

  long long n = (long long)a2;
  if (n > 1 and lmn_env.normal_para
      ) {
    long long t_num = 0;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::vector<std::vector<LmnMembraneRef> *> ret_mem;
    std::vector<std::vector<LmnAtomRef> *> ret_out;
    std::vector<std::vector<long long> *> ret_succ;
    std::vector<std::thread> threads;
    long long cores = ((lmn_env.core_num - 1) < 1) ? 1 : (lmn_env.core_num - 1);
    long long pat = ceil((float)n / (float)cores);
    long long begin = 0;
    long long end = std::min(pat, n);

    LmnAtomRef it = a1;
    StateSpaceRef ss = ((LmnStateMap::LmnStateMapRef)a0)->states;
    do {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      t_num++;
      std::vector<LmnMembraneRef> *ret_m = new std::vector<LmnMembraneRef>();
      std::vector<LmnAtomRef> *ret_o = new std::vector<LmnAtomRef>();
      std::vector<long long> *ret_s = new std::vector<long long>();
      ret_mem.push_back(ret_m);
      ret_out.push_back(ret_o);
      ret_succ.push_back(ret_s);
      if (lmn_env.normal_para)
        threads.push_back(std::thread(insert_state_para, rc, mem, t_num, ss, pat,
                                      it, ret_m, ret_o, ret_s));
      else
      insert_state_para(rc, mem, t_num, ss, pat, it, ret_m, ret_o, ret_s);
      for (int i = 0;
           i < pat and ((LmnSymbolAtomRef)it)->get_functor() != LMN_NIL_FUNCTOR;
           i++) {
        it = ((LmnSymbolAtomRef)it)->get_link(1);
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
    } while (((LmnSymbolAtomRef)it)->get_functor() != LMN_NIL_FUNCTOR);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (lmn_env.normal_para) {
      for (int i = 0; i < t_num; i++) {
        threads[i].join();
      }
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for (auto i = ret_mem.begin(); i != ret_mem.end(); i++) {
      // for (auto j = (*i)->begin(); j != (*i)->end(); j++) {
      //   ((MemReactContext *)rc)->memstack_remove(*j);
      //   mem->remove_mem(*j);
      // }
      delete (*i);
    }
    for (auto i = ret_out.begin(); i != ret_out.end(); i++) {
      // for (auto j = (*i)->begin(); j != (*i)->end(); j++) {
      //   lmn_mem_delete_atom(mem, *j, 0);
      // }
      delete (*i);
    }
    for (auto i = ret_succ.begin(); i != ret_succ.end(); i++) {
      // for (auto j = (*i)->begin(); j != (*i)->end(); j++) {
      //   lmn_mem_push_atom(mem, (LmnAtomRef)*j, LMN_INT_ATTR);
      // }
      delete (*i);
    }
  } else {
    LmnAtomRef it = a1;
    StateSpaceRef ss = ((LmnStateMap::LmnStateMapRef)a0)->states;
    while (((LmnSymbolAtomRef)it)->get_functor() != LMN_NIL_FUNCTOR) {
      LmnAtomRef head = ((LmnSymbolAtomRef)it)->get_link(0);
      while (((LmnSymbolAtomRef)head)->get_functor() != LMN_NIL_FUNCTOR) {
        LmnAtomRef out = ((LmnSymbolAtomRef)head)->get_link(0);
        LmnAtomRef in = ((LmnSymbolAtomRef)out)->get_link(0);
        LmnMembraneRef graph_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);

        // delete plus
        lmn_mem_delete_atom(graph_mem, ((LmnSymbolAtomRef)in)->get_link(1),
                            ((LmnSymbolAtomRef)in)->get_attr(1));
        // delete in
        lmn_mem_delete_atom(graph_mem, in,
                            ((LmnSymbolAtomRef)out)->get_attr(0));

        State *new_s = new State(graph_mem, 0, TRUE);
        State *succ = ss->insert(new_s);

        if (succ != new_s) {
          delete (new_s);
        }

        // delete out
        lmn_mem_delete_atom(mem, out, ((LmnSymbolAtomRef)head)->get_attr(0));
        lmn_mem_newlink(mem, head, ((LmnSymbolAtomRef)head)->get_attr(0), 0,
                        succ, LMN_INT_ATTR, 0);

        lmn_mem_push_atom(mem, succ, LMN_INT_ATTR);
        ((MemReactContext *)rc)->memstack_remove(graph_mem);
        mem->remove_mem(graph_mem);
        head = ((LmnSymbolAtomRef)head)->get_link(1);
      }
      it = ((LmnSymbolAtomRef)it)->get_link(1);
    }
  }

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a3, t3,
                  LMN_ATTR_GET_VALUE(t3));

  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), a4, t4,
                  LMN_ATTR_GET_VALUE(t4));
}

/*
 * ID->状態
 * +a0 Map
 * +a1 ID
 * -a2 状態
 * -a3 Map
 */
void LmnStateMap::cb_state_map_state_find(LmnReactCxtRef rc, LmnMembraneRef mem,
                                          LmnAtomRef a0, LmnLinkAttr t0,
                                          LmnAtomRef a1, LmnLinkAttr t1,
                                          LmnAtomRef a2, LmnLinkAttr t2,
                                          LmnAtomRef a3, LmnLinkAttr t3) {
  psm.graph_find_num++;
  auto start = get_wall_time();
  State *s = (State *)a1;
  st_data_t entry;

  LmnMembraneRef new_mem = s->duplicate_membrane();

  LmnSymbolAtomRef in = lmn_mem_newatom(new_mem, LMN_IN_PROXY_FUNCTOR);
  LmnSymbolAtomRef out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
  LmnSymbolAtomRef plus = lmn_mem_newatom(new_mem, LMN_UNARY_PLUS_FUNCTOR);
  lmn_newlink_in_symbols(plus, 0, in, 1);
  lmn_newlink_in_symbols(in, 0, out, 0);

  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), out,
                  LMN_ATTR_MAKE_LINK(1), 1);

  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t1), a3, t3,
                  LMN_ATTR_GET_VALUE(t3));

  mem->add_child_mem(new_mem);
  auto finish = get_wall_time();
  psm.graph_find_time += finish - start;
}

void cb_profile_state_map(LmnReactCxtRef rc, LmnMembraneRef mem) {
  std::cout << "----STATE MAP PROFILE----" << std::endl;
  std::cout << "id_find total time: " << psm.id_find_time << std::endl;
  std::cout << "id_find total num: " << psm.id_find_num << std::endl;
  std::cout << "graph_find total time: " << psm.graph_find_time << std::endl;
  std::cout << "graph_find total num: " << psm.graph_find_num << std::endl;
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *LmnStateMap::sp_cb_state_map_copy(void *data) { return data; }

void LmnStateMap::sp_cb_state_map_free(void *data) {}

unsigned char LmnStateMap::sp_cb_state_map_eq(void *_p1, void *_p2) {
  return 0;
}

void LmnStateMap::sp_cb_state_map_dump(void *state_map, LmnPortRef port) {
  port_put_raw_s(port, "<state_map>");
}

unsigned char LmnStateMap::sp_cb_state_map_is_ground(void *data) { return 1; }

void LmnStateMap::init_state_map(void) {
  state_map_atom_type = lmn_sp_atom_register(
      "state_map", sp_cb_state_map_copy, sp_cb_state_map_free,
      sp_cb_state_map_eq, sp_cb_state_map_dump, sp_cb_state_map_is_ground);
  CCallback::lmn_register_c_fun("cb_state_map_init", (void *)cb_state_map_init,
                                1);
  CCallback::lmn_register_c_fun("cb_state_map_free", (void *)cb_state_map_free,
                                1);
  CCallback::lmn_register_c_fun("cb_state_map_id_find",
                                (void *)cb_state_map_id_find, 4);
  CCallback::lmn_register_c_fun("cb_state_map_state_find",
                                (void *)cb_state_map_state_find, 4);
  CCallback::lmn_register_c_fun("cb_state_map_id_find_para",
                                (void *)cb_state_map_id_find_para, 5);
  CCallback::lmn_register_c_fun("cb_profile_state_map",
                                (void *)cb_profile_state_map, 0);
}
