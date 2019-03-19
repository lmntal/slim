/*
 * system_ruleset.cpp - default System Ruleset
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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
 * $Id: system_ruleset.c,v 1.8 2008/09/29 05:23:40 taisuke Exp $
 */

#include "functor.h"
#include "lmntal.h"
#include "memstack.h"
#include "react_context.hpp"
#include "symbol.h"
#include "membrane.hpp"
#include "atomlist.hpp"
#include "rule.hpp"

/* prototypes */


/* delete out proxies connected each other */
static BOOL delete_redundant_outproxies(LmnReactCxtRef rc, LmnMembraneRef mem,
                                        LmnRuleRef rule) {
  AtomListEntryRef ent;
  LmnSymbolAtomRef o0;

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent)
    return FALSE;

  EACH_ATOM(o0, ent, ({
              LmnSymbolAtomRef o1;

              if (o0->get_functor() == LMN_RESUME_FUNCTOR)
                continue;

              if (LMN_ATTR_IS_DATA(o0->get_attr(1)))
                return FALSE;
              o1 = (LmnSymbolAtomRef)o0->get_link(1);
              if (o1->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
                LmnSymbolAtomRef i0;
                LmnSymbolAtomRef i1;
                LmnMembraneRef m0;
                LmnMembraneRef m1;

                i0 = (LmnSymbolAtomRef)o0->get_link(0);
                i1 = (LmnSymbolAtomRef)o1->get_link(0);
                m0 = LMN_PROXY_GET_MEM(i0);
                m1 = LMN_PROXY_GET_MEM(i1);

                if (m0 == m1) {
                  ent -> remove(o0); /* for efficiency */
                  ent -> remove(o1);
                  lmn_delete_atom(o0);
                  lmn_delete_atom(o1);
                  lmn_mem_unify_atom_args(m0, i0, 1, i1, 1);
                  ent -> remove(i0);
                  ent -> remove(i1);
                  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
                    lmn_memstack_push(RC_MEMSTACK((MemReactContext *)rc), m0);
                  }
                  return TRUE;
                }
              }
            }));
  return FALSE;
}

/* delete in proxies connected each other */
static BOOL delete_redundant_inproxies(LmnReactCxtRef rc, LmnMembraneRef mem,
                                       LmnRuleRef rule) {
  AtomListEntryRef ent;
  LmnSymbolAtomRef o0;

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent)
    return FALSE;

  EACH_ATOM(o0, ent, ({
              LmnSymbolAtomRef i0, i1;

              if (o0->get_functor() == LMN_RESUME_FUNCTOR)
                continue;

              i0 = (LmnSymbolAtomRef)o0->get_link(0);

              if (LMN_ATTR_IS_DATA(i0->get_attr(1)))
                return FALSE;
              i1 = (LmnSymbolAtomRef)i0->get_link(1);
              if (i1->get_functor() == LMN_IN_PROXY_FUNCTOR) {
                LmnSymbolAtomRef o1 =
                    (LmnSymbolAtomRef)i1->get_link(0);
                ent -> remove(o0);
                ent -> remove(o1);
                lmn_delete_atom(o0);
                lmn_delete_atom(o1);
                lmn_mem_unify_atom_args(mem, o0, 1, o1, 1);
                ent -> remove(i0);
                ent -> remove(i1);
                return TRUE;
              }
            }));
  return FALSE;
}

static BOOL mem_eq(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule) {
  AtomListEntryRef ent;
  LmnSymbolAtomRef op;

  ent = lmn_mem_get_atomlist(mem, LMN_MEM_EQ_FUNCTOR);
  if (!ent)
    return FALSE;

  EACH_ATOM(op, ent, ({
              LmnMembraneRef mem0, mem1;
              LmnSymbolAtomRef out0, in0, out1, in1, ret, result_atom;
              LmnSymbolAtomRef temp0, temp1;
              LmnLinkAttr out_attr0, out_attr1, ret_attr;

              out_attr0 = op->get_attr(0);
              if (LMN_ATTR_IS_DATA(out_attr0))
                return FALSE;

              out0 = (LmnSymbolAtomRef)op->get_link(0);
              if (out0->get_functor() != LMN_OUT_PROXY_FUNCTOR) {
                return FALSE;
              }

              in0 = (LmnSymbolAtomRef)out0->get_link(0);
              out_attr1 = op->get_attr(1);
              if (LMN_ATTR_IS_DATA(out_attr1)) {
                return FALSE;
              }

              out1 = (LmnSymbolAtomRef)op->get_link(1);
              if (out1->get_functor() != LMN_OUT_PROXY_FUNCTOR) {
                return FALSE;
              }

              in1 = (LmnSymbolAtomRef)out1->get_link(0);

              mem0 = LMN_PROXY_GET_MEM(in0);
              mem1 = LMN_PROXY_GET_MEM(in1);

              /* roots of mem0 and mem1, connected to their outside proxies,
                 are temporarily set to unary atoms with the same functor */
              temp0 = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
              ret = (LmnSymbolAtomRef)op->get_link(0);
              ret_attr = op->get_attr(0);
              temp0->set_link(0, ret);
              temp0->set_attr(0, ret_attr);
              ret->set_link(LMN_ATTR_GET_VALUE(ret_attr), temp0);
              ret->set_attr(LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));

              temp1 = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
              ret = (LmnSymbolAtomRef)op->get_link(1);
              ret_attr = op->get_attr(1);
              temp1->set_link(0, ret);
              temp1->set_attr(0, ret_attr);
              ret->set_link(LMN_ATTR_GET_VALUE(ret_attr), temp1);
              ret->set_attr(LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));

              if (lmn_mem_equals(mem0, mem1)) {
                result_atom = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
              } else {
                result_atom = lmn_mem_newatom(mem, LMN_FALSE_FUNCTOR);
              }

              lmn_mem_unify_atom_args(mem, temp0, 0, op, 2);
              lmn_mem_unify_atom_args(mem, temp1, 0, op, 3);

              ret = (LmnSymbolAtomRef)op->get_link(4);
              ret_attr = op->get_attr(4);
              if (LMN_ATTR_IS_DATA(ret_attr)) {
                result_atom->set_link(0, ret);
                result_atom->set_attr(0, ret_attr);
              } else {
                result_atom->set_link(0, ret);
                result_atom->set_attr(0, ret_attr);
                ret->set_link(LMN_ATTR_GET_VALUE(ret_attr), result_atom);
                ret->set_attr(LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));
              }

              lmn_mem_delete_atom(mem, op, LMN_ATTR_MAKE_LINK(0));
              lmn_mem_delete_atom(mem, temp0, LMN_ATTR_MAKE_LINK(0));
              lmn_mem_delete_atom(mem, temp1, LMN_ATTR_MAKE_LINK(0));

              return TRUE;
            }));
  return FALSE;
}

/* -------------------------------------------------------------- */

void init_default_system_ruleset() {
  lmn_add_system_rule(
      new LmnRule(delete_redundant_outproxies, ANONYMOUS));
  lmn_add_system_rule(
      new LmnRule(delete_redundant_inproxies, ANONYMOUS));
  lmn_add_system_rule(new LmnRule(mem_eq, ANONYMOUS));
}
