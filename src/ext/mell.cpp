/*
 * mell.c - Nonlinear-Membrane
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
#include "verifier/verifier.h"
#include "vm/vm.h"

void mell_copy(LmnReactCxtRef rc, LmnMembraneRef mem, 
                LmnAtomRef a0, LmnLinkAttr t0, 
                LmnAtomRef a1, LmnLinkAttr t1, 
                LmnAtomRef a2, LmnLinkAttr t2,
                LmnAtomRef a3, LmnLinkAttr t3, 
                LmnAtomRef a4, LmnLinkAttr t4, 
                LmnAtomRef a5, LmnLinkAttr t5, 
                LmnAtomRef a6, LmnLinkAttr t6, 
                LmnAtomRef a7, LmnLinkAttr t7 
              ) {

  // copy対象の膜
  LmnSymbolAtomRef org_port_in;
  LmnMembraneRef org_mem, trg_mem;
  ProcessTableRef atom_map;
  org_port_in = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a0)->get_link(0));
  org_mem = LMN_PROXY_GET_MEM(org_port_in);
  trg_mem = new LmnMembrane();
  atom_map = lmn_mem_copy_cells(trg_mem, org_mem);
  mem->add_child_mem(trg_mem);

  // contractionの膜
  LmnSymbolAtomRef cont_in0, cont_in1, cont_in2;
  LmnMembraneRef cont_mem;
  cont_in0 = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a1)->get_link(0));
  cont_in1 = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a2)->get_link(0));
  cont_in2 = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a3)->get_link(0));
  cont_mem = LMN_PROXY_GET_MEM(cont_in0);
  if(LMN_PROXY_GET_MEM(cont_in1) != LMN_PROXY_GET_MEM(cont_in1) || 
     LMN_PROXY_GET_MEM(cont_in1) != LMN_PROXY_GET_MEM(cont_in2)) {
      fprintf(stderr, "mell.C, mell_copy: illegal argumetns in cont mem\n");
      return;
    }
  
  // cutの膜
  LmnSymbolAtomRef cut_in_port, cut_in_free;
  LmnMembraneRef cut_mem;
  cut_in_port = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a4)->get_link(0));
  cut_in_free = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a5)->get_link(0));
  cut_mem = LMN_PROXY_GET_MEM(cut_in_port);
  if(LMN_PROXY_GET_MEM(cut_in_port) != LMN_PROXY_GET_MEM(cut_in_free)) {
      fprintf(stderr, "mell.C, mell_copy: illegal argumetns in cut mem\n");
      return;
    }

  // 自由リンク
  LmnSymbolAtomRef free0, free1;
  free0 = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a6));
  free1 = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a7));

  { 
    AtomListEntryRef ent = org_mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnSymbolAtomRef org_in, org_out, trg_in, trg_out;
      LmnSymbolAtomRef org_port_out, trg_port_in, trg_port_out;
      LmnWord t = 0;

      EACH_ATOM(org_in, ent, ({

        if (org_in == org_port_in) {
          LmnSymbolAtomRef cut_copy_in_port0, cut_copy_in_port1, cut_copy_in_free0, cut_copy_in_free1;
          LmnSymbolAtomRef cut_copy_out_port0, cut_copy_out_port1, cut_copy_out_free0, cut_copy_out_free1;
          LmnMembraneRef cut_copy_mem0, cut_copy_mem1;
          ProcessTableRef cut_atom_map0, cut_atom_map1;
          LmnWord t_cut0, t_cut1, t_cut2, t_cut3;
          
          proc_tbl_get_by_atom(atom_map, org_port_in, &t);
          trg_port_in = (LmnSymbolAtomRef)(t);
          org_port_out = (LmnSymbolAtomRef)(org_in->get_link(0));
          trg_port_out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          lmn_newlink_in_symbols(trg_port_in, 0, trg_port_out, 0);

          // cutをコピー
          cut_copy_mem0 = new LmnMembrane();
          cut_copy_mem1 = new LmnMembrane();
          cut_atom_map0 = lmn_mem_copy_cells(cut_copy_mem0, cut_mem);
          cut_atom_map1 = lmn_mem_copy_cells(cut_copy_mem1, cut_mem);
          mem->add_child_mem(cut_copy_mem0);
          mem->add_child_mem(cut_copy_mem1);

          // cut_copyのproxyアトムを用意する
          proc_tbl_get_by_atom(cut_atom_map0, cut_in_port, &t_cut0);
          proc_tbl_get_by_atom(cut_atom_map0, cut_in_free, &t_cut1);
          cut_copy_in_port0 = (LmnSymbolAtomRef)(t_cut0);
          cut_copy_in_free0 = (LmnSymbolAtomRef)(t_cut1);
          proc_tbl_get_by_atom(cut_atom_map1, cut_in_port, &t_cut2);
          proc_tbl_get_by_atom(cut_atom_map1, cut_in_free, &t_cut3);
          cut_copy_in_port1 = (LmnSymbolAtomRef)(t_cut2);
          cut_copy_in_free1 = (LmnSymbolAtomRef)(t_cut3);

          cut_copy_out_port0 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          cut_copy_out_free0 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          cut_copy_out_port1 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          cut_copy_out_free1 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);

          lmn_newlink_in_symbols(cut_copy_in_port0, 0, cut_copy_out_port0, 0);
          lmn_newlink_in_symbols(cut_copy_in_free0, 0, cut_copy_out_free0, 0);
          lmn_newlink_in_symbols(cut_copy_in_port1, 0, cut_copy_out_port1, 0);
          lmn_newlink_in_symbols(cut_copy_in_free1, 0, cut_copy_out_free1, 0);

          // portとfreeをcutでつなげる
          lmn_newlink_in_symbols(cut_copy_out_port0, 1, org_port_out, 1);
          newlink_symbol_and_something(cut_copy_out_free0, 1, free0, t6);
          lmn_newlink_in_symbols(cut_copy_out_port1, 1, trg_port_out, 1);
          newlink_symbol_and_something(cut_copy_out_free1, 1, free1, t7);

          delete cut_atom_map0;
          delete cut_atom_map1;
          mem->remove_mem(cut_copy_mem0);
          mem->remove_mem(cut_copy_mem1);
          cut_copy_mem0->remove_proxies();
          cut_copy_mem1->remove_proxies();
          mem->move_cells(cut_copy_mem0);
          mem->move_cells(cut_copy_mem1);

        } else {
          proc_tbl_get_by_atom(atom_map, org_in, &t);
          trg_in = (LmnSymbolAtomRef)(t);
          org_out = (LmnSymbolAtomRef)(org_in->get_link(0));
          trg_out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          lmn_newlink_in_symbols(trg_in, 0, trg_out, 0);

          // cont_memをコピーする
          LmnSymbolAtomRef cont_copy_in0, cont_copy_in1, cont_copy_in2;
          LmnSymbolAtomRef cont_copy_out0, cont_copy_out1, cont_copy_out2;
          LmnMembraneRef cont_copy_mem;
          ProcessTableRef cont_atom_map;
          LmnWord t_cont0, t_cont1, t_cont2;
         
          cont_copy_mem = new LmnMembrane();
          cont_atom_map = lmn_mem_copy_cells(cont_copy_mem, cont_mem);
          mem->add_child_mem(cont_copy_mem);

          // cont_copyのproxyアトムを用意する
          proc_tbl_get_by_atom(cont_atom_map, cont_in0, &t_cont0);
          proc_tbl_get_by_atom(cont_atom_map, cont_in1, &t_cont1);
          proc_tbl_get_by_atom(cont_atom_map, cont_in2, &t_cont2);
          cont_copy_in0 = (LmnSymbolAtomRef)(t_cont0);
          cont_copy_in1 = (LmnSymbolAtomRef)(t_cont1);
          cont_copy_in2 = (LmnSymbolAtomRef)(t_cont2);

          cont_copy_out0 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          cont_copy_out1 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          cont_copy_out2 = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);

          lmn_newlink_in_symbols(cont_copy_in0, 0, cont_copy_out0, 0);
          lmn_newlink_in_symbols(cont_copy_in1, 0, cont_copy_out1, 0);
          lmn_newlink_in_symbols(cont_copy_in2, 0, cont_copy_out2, 0);

          // orgとtrgをcontでつなげる
          lmn_relink_symbols(cont_copy_out2, 1, org_out, 1);
          lmn_newlink_in_symbols(cont_copy_out0, 1, org_out, 1);
          lmn_newlink_in_symbols(cont_copy_out1, 1, trg_out, 1);

          delete cont_atom_map;
          mem->remove_mem(cont_copy_mem);
          cont_copy_mem->remove_proxies();
          mem->move_cells(cont_copy_mem);
        }
        mem->remove_temporary_proxies();
      }));
    }

    delete atom_map;

    if (rc->has_mode(REACT_MEM_ORIENTED)) {
      ((MemReactContext *)rc)->memstack_remove(cont_mem);
      ((MemReactContext *)rc)->memstack_remove(cut_mem);
    }

    mem->delete_mem(cont_mem);
    mem->delete_mem(cut_mem);

    lmn_mem_delete_atom(mem, a1, t1);
    lmn_mem_delete_atom(mem, a2, t2);
    lmn_mem_delete_atom(mem, a3, t3);
    lmn_mem_delete_atom(mem, a4, t4);
    lmn_mem_delete_atom(mem, a5, t5);
  }
}

void mell_kill(LmnReactCxtRef rc,
                LmnMembraneRef mem,
                LmnAtomRef a0, LmnLinkAttr t0,
                LmnAtomRef a1, LmnLinkAttr t1)
{
  // 削除対象の膜
  LmnSymbolAtomRef org_in;
  LmnMembraneRef org_mem;

  // タグの膜
  LmnSymbolAtomRef tag_in;
  LmnMembraneRef tag_mem;

  if (((LmnSymbolAtomRef)a0)->get_functor() != LMN_OUT_PROXY_FUNCTOR) {
    fprintf(stderr, "mell.C, mell_kill: first argument must be a membrane");
    return;
  }
  if (((LmnSymbolAtomRef)a1)->get_functor() != LMN_OUT_PROXY_FUNCTOR) {
    fprintf(stderr, "mell.C, mell_kill: second argument must be a membrane");
    return;
  }

  org_in = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a0)->get_link(0));
  org_mem = LMN_PROXY_GET_MEM(org_in);

  tag_in = (LmnSymbolAtomRef)(((LmnSymbolAtomRef)a1)->get_link(0));
  tag_mem = LMN_PROXY_GET_MEM(tag_in);
  {   
    AtomListEntryRef ent = org_mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnSymbolAtomRef in, out;
      LmnLinkAttr out_attr;
      EACH_ATOM(in, ent, ({
        if (in == org_in) continue;
        out = (LmnSymbolAtomRef)(in->get_link(0));
        out_attr = in->get_attr(0);

        LmnMembraneRef tag_copy_mem;
        ProcessTableRef atom_map;
        tag_copy_mem = new LmnMembrane();
        atom_map = lmn_mem_copy_cells(tag_copy_mem, tag_mem);
        mem->add_child_mem(tag_copy_mem);
        {
          LmnSymbolAtomRef tag_copy_in, tag_copy_out, dummy;
          LmnLinkAttr tag_copy_out_attr;
          LmnWord t = 0;

          proc_tbl_get_by_atom(atom_map, tag_in, &t);
          tag_copy_in = (LmnSymbolAtomRef)(t);
          tag_copy_out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
          lmn_newlink_in_symbols(tag_copy_in, 0, tag_copy_out, 0);

          lmn_relink_symbols(tag_copy_out, 1, out, 1);

          delete atom_map;
          mem->remove_mem(tag_copy_mem);
          tag_copy_mem->remove_proxies();
          mem->move_cells(tag_copy_mem);
          mem->remove_temporary_proxies();
        }
        lmn_mem_delete_atom(mem, out, out_attr);
      }));
    }
  }

  if (rc->has_mode(REACT_MEM_ORIENTED)) {
    ((MemReactContext *)rc)->memstack_remove(org_mem);
    ((MemReactContext *)rc)->memstack_remove(tag_mem);
  }

  mem->delete_mem(org_mem);
  mem->delete_mem(tag_mem);
  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);
}

void init_mell(void) {
  CCallback::lmn_register_c_fun("mell_copy", (void *)mell_copy, 8);
  CCallback::lmn_register_c_fun("mell_kill", (void *)mell_kill, 2);
}
