/* Nonlinear-Membrane */

#include <stdio.h>
#include "../lmntal_ext.h"

LMN_EXTERN void init_nlmem(void);

void nlmem_copy(LmnMembrane *mem,
                LmnWord a0, LmnLinkAttr t0,
                LmnWord a1, LmnLinkAttr t1,
                LmnWord a2, LmnLinkAttr t2)
{
  lmn_interned_str copy_tag_name =
    LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(a1));
  LmnFunctor copy_tag_func = lmn_functor_intern(ANONYMOUS, copy_tag_name, 3);
  LmnMembrane *org_mem, *trg_mem;
  SimpleHashtbl *atom_map;

  org_mem = LMN_PROXY_GET_MEM(LMN_ATOM_GET_LINK(a0, 0));
  trg_mem = lmn_mem_make();
  atom_map = lmn_mem_copy_cells(trg_mem, org_mem);
  lmn_mem_add_child_mem(mem, trg_mem);

  {
    AtomListEntry *ent = lmn_mem_get_atomlist(org_mem, LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnAtomPtr org_in, org_out, trg_in, trg_out;
      LmnAtomPtr tag_atom;
      
      for (org_in = atomlist_head(ent);
           org_in != lmn_atomlist_end(ent);
           org_in = LMN_ATOM_GET_NEXT(org_in)) {
        /* タグアトムを作り、リンクの接続を行う */
        trg_in = LMN_ATOM(hashtbl_get(atom_map, (HashKeyType)org_in));
        org_out = LMN_ATOM(LMN_ATOM_GET_LINK(org_in, 0));
        trg_out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(trg_in, 0, trg_out, 0);
        tag_atom = lmn_mem_newatom(mem, copy_tag_func);
        lmn_relink_symbols(tag_atom, 2, org_out, 1);
        lmn_newlink_in_symbols(tag_atom, 0, org_out, 1);
        lmn_newlink_in_symbols(tag_atom, 1, trg_out, 1);
      }
    }

    hashtbl_free(atom_map);
    lmn_mem_remove_atom(mem, a1, t1);
    lmn_free_atom(a1, t1);
    /* 第一引数に接続されたタグアトムと第三引数を接続する */
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_ATOM_GET_LINK(a0, 1), LMN_ATOM_GET_ATTR(a0, 1),
                    2);
  }
}

void nlmem_kill(LmnMembrane *mem,
                LmnWord a0, LmnLinkAttr t0,
                LmnWord a1, LmnLinkAttr t1)
{
  LmnFunctor kill_tag_func = LMN_ATOM_GET_FUNCTOR(a1);
  LmnAtomPtr org_in;
  LmnMembrane *org_mem;

  if (LMN_ATOM_GET_FUNCTOR(a0) != LMN_OUT_PROXY_FUNCTOR) {
    fprintf(stderr, "NLMEM.C, nlmem_kill: first argument must be a membrane");
    return;
  }

  org_in = LMN_ATOM(LMN_ATOM_GET_LINK(a0, 0));
  org_mem = LMN_PROXY_GET_MEM(org_in);
  {
    AtomListEntry *ent = lmn_mem_get_atomlist(org_mem, LMN_IN_PROXY_FUNCTOR);

    if (ent) {
      LmnAtomPtr in, out;
      LmnLinkAttr out_attr;
      LmnAtomPtr tag_atom;
      
      for (in = atomlist_head(ent);
           in != lmn_atomlist_end(ent);
           in = LMN_ATOM_GET_NEXT(in)) {
        if (in == org_in) continue;
        out = LMN_ATOM(LMN_ATOM_GET_LINK(in, 0));
        out_attr = LMN_ATOM_GET_ATTR(in, 0);
        tag_atom = lmn_mem_newatom(mem, kill_tag_func);
        lmn_relink_symbols(tag_atom, 0, out, 1);
        lmn_mem_remove_atom(mem, (LmnWord)out, out_attr);
        lmn_free_atom((LmnWord)out, out_attr);
      }
    }
  }
  
  lmn_mem_remove_mem(mem, org_mem);
  lmn_memstack_delete(org_mem);
  lmn_mem_drop(org_mem);
  lmn_mem_free(org_mem);
  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_free_atom(a1, t1);
}

void init_nlmem(void)
{
  lmn_register_c_fun("nlmem_copy", nlmem_copy, 3);
  lmn_register_c_fun("nlmem_kill", nlmem_kill, 2);
}
