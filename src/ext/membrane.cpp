/*
 * ext/membrane.c
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

#include "membrane.h"
#include "lmntal.h"
#include "set.h"
#include "verifier/verifier.h"
#include "vm/vm.h"

void Membrane::cb_mhash(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef mem_proxy, LmnLinkAttr mem_proxy_link_attr,
                        LmnAtomRef ret_mem_proxy, LmnLinkAttr ret_mem_proxy_link_attr, LmnAtomRef ret_hash_atom,
                        LmnLinkAttr ret_hash_atom_link_attr) {
  LmnMembraneRef m = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)mem_proxy)->get_link(0));

  unsigned long h = mhash(m);

  lmn_mem_newlink(mem, ret_hash_atom, LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(ret_hash_atom_link_attr), (LmnAtomRef)h,
                  LMN_INT_ATTR, 0);

  lmn_mem_push_atom(mem, (LmnAtomRef)h, LMN_INT_ATTR);

  lmn_mem_newlink(mem, ret_mem_proxy, LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(ret_mem_proxy_link_attr), mem_proxy,
                  mem_proxy_link_attr, LMN_ATTR_GET_VALUE(mem_proxy_link_attr));
}

void Membrane::cb_mem_equals(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef mem0_proxy,
                             LmnLinkAttr mem0_proxy_link_attr, LmnAtomRef mem1_proxy, LmnLinkAttr mem1_proxy_link_attr,
                             LmnAtomRef ret_mem0_link, LmnLinkAttr ret_mem0_link_attr, LmnAtomRef ret_mem1_link,
                             LmnLinkAttr ret_mem1_link_attr, LmnAtomRef res_link, LmnLinkAttr res_link_attr) {
  LmnMembraneRef   m0     = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)mem0_proxy)->get_link(0));
  LmnMembraneRef   m1     = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)((LmnSymbolAtomRef)mem1_proxy)->get_link(0));
  LmnFunctor       judge  = (LmnSet::mem_cmp(m0, m1) == 0) ? LMN_TRUE_FUNCTOR : LMN_FALSE_FUNCTOR;
  LmnSymbolAtomRef result = lmn_mem_newatom(mem, judge);

  lmn_mem_newlink(mem, result, LMN_ATTR_MAKE_LINK(0), 0, res_link, res_link_attr, LMN_ATTR_GET_VALUE(res_link_attr));

  lmn_mem_newlink(mem, mem0_proxy, mem0_proxy_link_attr, LMN_ATTR_GET_VALUE(mem0_proxy_link_attr), ret_mem0_link,
                  LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(ret_mem0_link_attr));

  lmn_mem_newlink(mem, mem1_proxy, mem1_proxy_link_attr, LMN_ATTR_GET_VALUE(mem1_proxy_link_attr), ret_mem1_link,
                  LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(ret_mem1_link_attr));
}

void Membrane::init_membrane(void) {
  CCallback::lmn_register_c_fun("cb_mhash", (void *)cb_mhash, 3);
  CCallback::lmn_register_c_fun("cb_mem_equals", (void *)cb_mem_equals, 5);
}
