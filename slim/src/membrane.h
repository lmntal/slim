/*
 * membrane.h
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
 * $Id: membrane.h,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_MEMBRANE_H
#define LMN_MEMBRANE_H

#include "lmntal.h"
#include "atom.h"
#include "internal_hash.h"
#include "vector.h"
#include "rule.h"

struct LmnMembrane {
  LmnMembrane 	   *parent;
  LmnMembrane 	   *child_head;
  LmnMembrane 	   *prev,
                   *next;
  struct SimpleHashtbl atomset;
  unsigned int     atom_num; /* # of atom except proxy */
  struct Vector	   rulesets;
  lmn_interned_str name;
  BOOL             is_activated;
};

/* この構造体をAtomとして扱うことで,この構造体自身が
   HeadとTailの両方の役目を果たしている */
typedef struct AtomListEntry {
  LmnWord tail, head;
  struct SimpleHashtbl record;
} AtomListEntry;

#define atomlist_head(LIST)    (LMN_ATOM((LIST)->head))

#define LMN_MEM_NAME_ID(MP) ((MP)->name)
#define LMN_MEM_NAME(MP) LMN_SYMBOL_STR(LMN_MEM_NAME_ID(MP))

LMN_EXTERN LmnMembrane *lmn_mem_make(void);
LMN_EXTERN void lmn_mem_free(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_drop(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem);
LMN_EXTERN LmnAtomPtr lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f);
LMN_EXTERN void lmn_mem_push_atom(LmnMembrane *mem, LmnWord atom, LmnLinkAttr attr);
LMN_EXTERN void mem_push_symbol_atom(LmnMembrane *mem, LmnAtomPtr atom);
LMN_EXTERN void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset);
LMN_EXTERN BOOL lmn_mem_natoms(LmnMembrane *mem, unsigned int count);
LMN_EXTERN AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f);
LMN_EXTERN LmnAtomPtr* atomlist_get_record(AtomListEntry *atomlist, int findatomid);
LMN_EXTERN void lmn_mem_remove_atom(LmnMembrane *mem, LmnWord atom, LmnLinkAttr attr);
LMN_EXTERN inline unsigned int lmn_mem_count_descendants(LmnMembrane *mem);
LMN_EXTERN inline unsigned int lmn_mem_count_children(LmnMembrane *mem);

/* 同型性判定 */
LMN_EXTERN BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);

/* アトムをアトムリストから削除する.
   リストのつなぎ変えだけを行う */
#define REMOVE_FROM_ATOMLIST(atom)                 \
  do { \
    LMN_ATOM_SET_PREV(LMN_ATOM_GET_NEXT(atom), LMN_ATOM_GET_PREV(atom)); \
    LMN_ATOM_SET_NEXT(LMN_ATOM_GET_PREV(atom), LMN_ATOM_GET_NEXT(atom)); \
  } while (0)

LMN_EXTERN BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count);
LMN_EXTERN BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count);
LMN_EXTERN void lmn_mem_movecells(LmnMembrane *destmem, LmnMembrane *srcmem);
LMN_EXTERN void lmn_mem_newlink(LmnMembrane *mem,
                                LmnWord atom0,
                                LmnLinkAttr attr0,
                                int pos0,
                                LmnWord atom1,
                                LmnLinkAttr attr1,
                                int pos1);
LMN_EXTERN void lmn_newlink_in_symbols(LmnAtomPtr atom0,
                                       int pos0,
                                       LmnAtomPtr atom1,
                                       int pos1);
LMN_EXTERN void lmn_mem_link_data_atoms(LmnMembrane *mem,
                                        LmnWord d1,
                                        LmnLinkAttr attr1,
                                        LmnWord d2,
                                        LmnLinkAttr attr2);
LMN_EXTERN void lmn_mem_unify_atom_args(LmnMembrane *mem,
                             LmnAtomPtr atom1,
                             int pos1,
                             LmnAtomPtr atom2,
                             int pos2);
LMN_EXTERN void lmn_mem_unify_symbol_atom_args(LmnAtomPtr atom1,
                                               int pos1,
                                               LmnAtomPtr atom2,
                                               int pos2);
LMN_EXTERN void lmn_mem_relink_atom_args(LmnMembrane *mem,
                                         LmnWord atom0,
                                         LmnLinkAttr attr0,
                                         int pos0,
                                         LmnWord atom1,
                                         LmnLinkAttr attr1,
                                         int pos1);
LMN_EXTERN void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LMN_EXTERN SimpleHashtbl *lmn_mem_copy_cells(LmnMembrane *dest, LmnMembrane *srcmem);
LMN_EXTERN void lmn_mem_remove_proxies(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem);
LMN_EXTERN void lmn_mem_remove_temporary_proxies(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem);
/* LmnAtomPtr* lmn_atomlist_end(AtomSetEntry * ent); */
#define lmn_atomlist_end(p_atomset_entry) ((LmnAtomPtr)p_atomset_entry)

#endif /* LMN_MEMBRANE_H */
