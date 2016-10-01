/*
 * system_ruleset.c - default System Ruleset
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: system_ruleset.c,v 1.8 2008/09/29 05:23:40 taisuke Exp $
 */

#include "lmntal.h"
#include "react_context.h"
#include "functor.h"
#include "symbol.h"
#include "slim_header/memstack.h"

/* prototypes */

void init_default_system_ruleset(void);

/* delete out proxies connected each other */
static BOOL delete_redundant_outproxies(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule)
{
  AtomListEntryRef ent;
  LmnSAtom o0;

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent) return FALSE;

  EACH_ATOM(o0, ent, ({
    LmnSAtom o1;

    if (LMN_SATOM_GET_FUNCTOR(o0) == LMN_RESUME_FUNCTOR) continue;

    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(o0, 1))) return FALSE;
    o1 = LMN_SATOM(LMN_SATOM_GET_LINK(o0, 1));
    if (LMN_SATOM_GET_FUNCTOR(o1) == LMN_OUT_PROXY_FUNCTOR) {
      LmnSAtom i0;
      LmnSAtom i1;
      LmnMembraneRef m0;
      LmnMembraneRef m1;

      i0 = LMN_SATOM(LMN_SATOM_GET_LINK(o0, 0));
      i1 = LMN_SATOM(LMN_SATOM_GET_LINK(o1, 0));
      m0 = LMN_PROXY_GET_MEM(i0);
      m1 = LMN_PROXY_GET_MEM(i1);

      if (m0 == m1) {
        remove_from_atomlist(o0, ent); /* for efficiency */
        remove_from_atomlist(o1, ent);
        lmn_delete_atom(o0);
        lmn_delete_atom(o1);
        lmn_mem_unify_atom_args(m0, i0, 1, i1, 1);
        remove_from_atomlist(i0, ent);
        remove_from_atomlist(i1, ent);
        if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
          lmn_memstack_push(RC_MEMSTACK(rc), m0);
        }
        return TRUE;
      }
    }
  }));
  return FALSE;
}

/* delete in proxies connected each other */
static BOOL delete_redundant_inproxies(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule)
{
  AtomListEntryRef ent;
  LmnSAtom o0;

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent) return FALSE;

  EACH_ATOM(o0, ent, ({
    LmnSAtom i0, i1;

    if (LMN_SATOM_GET_FUNCTOR(o0) == LMN_RESUME_FUNCTOR) continue;

    i0 = LMN_SATOM(LMN_SATOM_GET_LINK(o0, 0));

    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(i0, 1))) return FALSE;
    i1 = LMN_SATOM(LMN_SATOM_GET_LINK(i0, 1));
    if (LMN_SATOM_GET_FUNCTOR(i1) == LMN_IN_PROXY_FUNCTOR) {
      LmnSAtom o1 = LMN_SATOM(LMN_SATOM_GET_LINK(i1, 0));
      remove_from_atomlist(o0, ent);
      remove_from_atomlist(o1, ent);
      lmn_delete_atom(o0);
      lmn_delete_atom(o1);
      lmn_mem_unify_atom_args(mem, o0, 1, o1, 1);
      remove_from_atomlist(i0, ent);
      remove_from_atomlist(i1, ent);
      return TRUE;
    }
  }));
  return FALSE;
}

static BOOL mem_eq(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule)
{
  AtomListEntryRef ent;
  LmnSAtom op;

  ent = lmn_mem_get_atomlist(mem, LMN_MEM_EQ_FUNCTOR);
  if (!ent) return FALSE;

  EACH_ATOM(op, ent, ({
    LmnMembraneRef mem0, mem1;
    LmnSAtom out0, in0, out1, in1, ret, result_atom;
    LmnSAtom temp0, temp1;
    LmnLinkAttr out_attr0, out_attr1, ret_attr;

    out_attr0 = LMN_SATOM_GET_ATTR(op, 0);
    if (LMN_ATTR_IS_DATA(out_attr0)) return FALSE;

    out0 = LMN_SATOM(LMN_SATOM_GET_LINK(op, 0));
    if (LMN_SATOM_GET_FUNCTOR(out0) != LMN_OUT_PROXY_FUNCTOR) {
      return FALSE;
    }

    in0 = LMN_SATOM(LMN_SATOM_GET_LINK(out0, 0));
    out_attr1 = LMN_SATOM_GET_ATTR(op, 1);
    if (LMN_ATTR_IS_DATA(out_attr1)) {
      return FALSE;
    }

    out1 = LMN_SATOM(LMN_SATOM_GET_LINK(op, 1));
    if (LMN_SATOM_GET_FUNCTOR(out1) != LMN_OUT_PROXY_FUNCTOR) {
      return FALSE;
    }

    in1 = LMN_SATOM(LMN_SATOM_GET_LINK(out1, 0));

    mem0 = LMN_PROXY_GET_MEM(in0);
    mem1 = LMN_PROXY_GET_MEM(in1);

    /* roots of mem0 and mem1, connected to their outside proxies, 
       are temporarily set to unary atoms with the same functor */
    temp0 = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
    ret = LMN_SATOM(LMN_SATOM_GET_LINK(op, 0));
    ret_attr = LMN_SATOM_GET_ATTR(op, 0);
    LMN_SATOM_SET_LINK(temp0, 0, ret);
    LMN_SATOM_SET_ATTR(temp0, 0, ret_attr);
    LMN_SATOM_SET_LINK(ret, LMN_ATTR_GET_VALUE(ret_attr), temp0);
    LMN_SATOM_SET_ATTR(ret, LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));

    temp1 = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
    ret = LMN_SATOM(LMN_SATOM_GET_LINK(op, 1));
    ret_attr = LMN_SATOM_GET_ATTR(op, 1);
    LMN_SATOM_SET_LINK(temp1, 0, ret);
    LMN_SATOM_SET_ATTR(temp1, 0, ret_attr);
    LMN_SATOM_SET_LINK(ret, LMN_ATTR_GET_VALUE(ret_attr), temp1);
    LMN_SATOM_SET_ATTR(ret, LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));

    if (lmn_mem_equals(mem0, mem1)) {
      result_atom = lmn_mem_newatom(mem, LMN_TRUE_FUNCTOR);
    } else {
      result_atom = lmn_mem_newatom(mem, LMN_FALSE_FUNCTOR);
    }

    lmn_mem_unify_atom_args(mem, temp0, 0, op, 2);
    lmn_mem_unify_atom_args(mem, temp1, 0, op, 3);

    ret = LMN_SATOM(LMN_SATOM_GET_LINK(op, 4));
    ret_attr = LMN_SATOM_GET_ATTR(op, 4);
    if (LMN_ATTR_IS_DATA(ret_attr)) {
      LMN_SATOM_SET_LINK(result_atom, 0, ret);
      LMN_SATOM_SET_ATTR(result_atom, 0, ret_attr);
    } else {
      LMN_SATOM_SET_LINK(result_atom, 0, ret);
      LMN_SATOM_SET_ATTR(result_atom, 0, ret_attr);
      LMN_SATOM_SET_LINK(ret, LMN_ATTR_GET_VALUE(ret_attr), result_atom);
      LMN_SATOM_SET_ATTR(ret, LMN_ATTR_GET_VALUE(ret_attr), LMN_ATTR_MAKE_LINK(0));
    }

    lmn_mem_delete_atom(mem, LMN_ATOM(op), LMN_ATTR_MAKE_LINK(0));
    lmn_mem_delete_atom(mem, LMN_ATOM(temp0), LMN_ATTR_MAKE_LINK(0));
    lmn_mem_delete_atom(mem, LMN_ATOM(temp1), LMN_ATTR_MAKE_LINK(0));

    return TRUE;

  }));
  return FALSE;
}

/* -------------------------------------------------------------- */

void init_default_system_ruleset()
{
  lmn_add_system_rule(lmn_rule_make_translated(delete_redundant_outproxies, ANONYMOUS));
  lmn_add_system_rule(lmn_rule_make_translated(delete_redundant_inproxies, ANONYMOUS));
  lmn_add_system_rule(lmn_rule_make_translated(mem_eq, ANONYMOUS));
}
