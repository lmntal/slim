/*
 * nlmem.c - Nonlinear-Membrane
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

#include "../lmntal.h"
#include "element/element.h"
#include "vm/vm.h"
#include "verifier/verifier.h"



extern "C" void init_nlmem(void);

void nlmem_copy(LmnReactCxtRef rc,
                LmnMembraneRef mem,
                LmnAtomRef a0, LmnLinkAttr t0,
                LmnAtomRef a1, LmnLinkAttr t1,
                LmnAtomRef a2, LmnLinkAttr t2)
{
  LmnMembraneRef org_mem, trg_mem;
  ProcessTableRef atom_map;
  lmn_interned_str copy_tag_name;
  LmnFunctor copy_tag_func;

  copy_tag_name = LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)a1));
  copy_tag_func = lmn_functor_intern(ANONYMOUS, copy_tag_name, 3);
  org_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)a0, 0));
  trg_mem = lmn_mem_make();
  atom_map = lmn_mem_copy_cells(trg_mem, org_mem);
  lmn_mem_add_child_mem(mem, trg_mem);

  {
    AtomListEntryRef ent = lmn_mem_get_atomlist(org_mem, LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnSymbolAtomRef org_in, org_out, trg_in, trg_out;
      LmnSymbolAtomRef tag_atom;
      LmnWord t = 0;

      EACH_ATOM(org_in, ent, ({
        /* タグアトムを作り、リンクの接続を行う */
        proc_tbl_get_by_atom(atom_map, org_in, &t);
        trg_in = (LmnSymbolAtomRef)(t);
        org_out = (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(org_in, 0));
        trg_out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(trg_in, 0, trg_out, 0);
        tag_atom = lmn_mem_newatom(mem, copy_tag_func);
        lmn_relink_symbols(tag_atom, 2, org_out, 1);
        lmn_newlink_in_symbols(tag_atom, 0, org_out, 1);
        lmn_newlink_in_symbols(tag_atom, 1, trg_out, 1);
      }));
    }

    proc_tbl_free(atom_map);
    lmn_mem_delete_atom(mem, a1, t1);
    /* 第一引数に接続されたタグアトムと第三引数を接続する */
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_SATOM_GET_LINK((LmnSymbolAtomRef)a0, 1), LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)a0, 1),
                    2);
  }
}

void nlmem_kill(LmnReactCxtRef rc,
                LmnMembraneRef mem,
                LmnAtomRef a0, LmnLinkAttr t0,
                LmnAtomRef a1, LmnLinkAttr t1)
{
  LmnFunctor kill_tag_func = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)a1);
  LmnSymbolAtomRef org_in;
  LmnMembraneRef org_mem;

  if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)a0) != LMN_OUT_PROXY_FUNCTOR) {
    fprintf(stderr, "NLMEM.C, nlmem_kill: first argument must be a membrane");
    return;
  }

  org_in = (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK((LmnSymbolAtomRef)a0, 0));
  org_mem = LMN_PROXY_GET_MEM(org_in);
  {
    AtomListEntryRef ent = lmn_mem_get_atomlist(org_mem, LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnSymbolAtomRef in, out;
      LmnLinkAttr out_attr;
      LmnSymbolAtomRef tag_atom;

      EACH_ATOM(in, ent, ({
        if (in == org_in) continue;
        out = (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(in, 0));
        out_attr = LMN_SATOM_GET_ATTR(in, 0);
        tag_atom = lmn_mem_newatom(mem, kill_tag_func);
        lmn_relink_symbols(tag_atom, 0, out, 1);
        lmn_mem_delete_atom(mem, out, out_attr);
      }));
    }
  }

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    lmn_memstack_delete(RC_MEMSTACK(rc), org_mem);
  }
  lmn_mem_delete_mem(mem, org_mem);
  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);
}

void init_nlmem(void)
{
  lmn_register_c_fun("nlmem_copy", (void *)nlmem_copy, 3);
  lmn_register_c_fun("nlmem_kill", (void *)nlmem_kill, 2);
}
