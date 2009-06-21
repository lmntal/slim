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

/* TODO: 構造体の定義を隠すために.cファイルに定義を移動する */
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

#define atomlist_head(LIST)    (LMN_SATOM((LIST)->head))

/* アトムリストENTのアトムに対してCODEを実行する。
   それぞれのループでCODEを実行する前に、Vにアトムが割り当てられる。
   履歴アトムがアトムリストにある場合は、読み飛ばす */
#define EACH_ATOM(V, ENT, CODE)     \
  for ((V) = atomlist_head((ENT)); \
       (V) != lmn_atomlist_end((ENT)); \
       (V) = LMN_SATOM_GET_NEXT_RAW((V))) { \
    if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) { \
      (CODE);                                               \
    } \
  }

#define LMN_MEM_NAME_ID(MP) ((MP)->name)
#define LMN_MEM_NAME(MP) LMN_SYMBOL_STR(LMN_MEM_NAME_ID(MP))

LMN_EXTERN LmnMembrane *lmn_mem_make(void);
LMN_EXTERN void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem);
LMN_EXTERN void lmn_mem_free(LmnMembrane *mem);
LMN_EXTERN inline void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem);
LMN_EXTERN void lmn_mem_drop(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem);
LMN_EXTERN LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f);
LMN_EXTERN void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
LMN_EXTERN void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
LMN_EXTERN void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset);
LMN_EXTERN inline int lmn_mem_ruleset_num(LmnMembrane *mem);
LMN_EXTERN inline LmnRuleSet  lmn_mem_get_ruleset(LmnMembrane *mem, int i);
LMN_EXTERN BOOL lmn_mem_natoms(LmnMembrane *mem, unsigned int count);
LMN_EXTERN AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f);
LMN_EXTERN LmnSAtom atomlist_get_record(AtomListEntry *atomlist, int findatomid);
LMN_EXTERN void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
LMN_EXTERN inline void lmn_mem_remove_data_atom(LmnMembrane *mem,
                                                LmnAtom atom,
                                                LmnLinkAttr attr);
LMN_EXTERN inline void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
LMN_EXTERN inline unsigned int lmn_mem_count_descendants(LmnMembrane *mem);
LMN_EXTERN inline unsigned int lmn_mem_count_children(LmnMembrane *mem);
LMN_EXTERN inline LmnMembrane *lmn_mem_parent(LmnMembrane *mem);
LMN_EXTERN inline void lmn_mem_set_active(LmnMembrane *mem, BOOL active);
LMN_EXTERN inline BOOL lmn_mem_is_active(LmnMembrane *mem);


/* 同型性判定 */
LMN_EXTERN BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);

/* アトムをアトムリストから削除する.
   リストのつなぎ変えだけを行う */
#define REMOVE_FROM_ATOMLIST(atom)                 \
  do { \
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(atom), LMN_SATOM_GET_PREV(atom)); \
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(atom), LMN_SATOM_GET_NEXT_RAW(atom)); \
  } while (0)

LMN_EXTERN BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count);
LMN_EXTERN BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count);
LMN_EXTERN void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LMN_EXTERN void lmn_mem_newlink(LmnMembrane *mem,
                                LmnAtom atom0,
                                LmnLinkAttr attr0,
                                int pos0,
                                LmnAtom atom1,
                                LmnLinkAttr attr1,
                                int pos1);
LMN_EXTERN void lmn_newlink_in_symbols(LmnSAtom atom0,
                                       int pos0,
                                       LmnSAtom atom1,
                                       int pos1);
LMN_EXTERN void lmn_mem_link_data_atoms(LmnMembrane *mem,
                                        LmnAtom d1,
                                        LmnLinkAttr attr1,
                                        LmnAtom d2,
                                        LmnLinkAttr attr2);
LMN_EXTERN void lmn_mem_unify_atom_args(LmnMembrane *mem,
                             LmnSAtom atom1,
                             int pos1,
                             LmnSAtom atom2,
                             int pos2);
LMN_EXTERN void lmn_mem_unify_symbol_atom_args(LmnSAtom atom1,
                                               int pos1,
                                               LmnSAtom atom2,
                                               int pos2);
LMN_EXTERN void lmn_relink_symbols(LmnSAtom atom0,
                                   int pos0,
                                   LmnSAtom atom1,
                                   int pos1);
LMN_EXTERN void lmn_mem_relink_atom_args(LmnMembrane *mem,
                                         LmnAtom atom0,
                                         LmnLinkAttr attr0,
                                         int pos0,
                                         LmnAtom atom1,
                                         LmnLinkAttr attr1,
                                         int pos1);
LMN_EXTERN void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LMN_EXTERN LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *srcmem, SimpleHashtbl **copymap);
LMN_EXTERN LmnMembrane *lmn_mem_copy(LmnMembrane *srcmem);
LMN_EXTERN SimpleHashtbl *lmn_mem_copy_cells(LmnMembrane *dest, LmnMembrane *srcmem);
LMN_EXTERN void lmn_mem_remove_proxies(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem);
LMN_EXTERN void lmn_mem_remove_temporary_proxies(LmnMembrane *mem);
LMN_EXTERN void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem);

BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
void lmn_mem_copy_ground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         SimpleHashtbl **ret_atommap);
void lmn_mem_remove_ground(LmnMembrane *mem, Vector *srcvec);
void lmn_mem_free_ground(Vector *srcvec);
void lmn_mem_delete_ground(LmnMembrane *mem, Vector *srcvec);

/* リンクオブジェクトの代替 */
typedef struct LinkObj {
  LmnAtom ap;
  LmnLinkAttr pos;
} *LinkObj;

LinkObj LinkObj_make(LmnAtom ap, LmnLinkAttr pos);
/* LmnSAtom* lmn_atomlist_end(AtomSetEntry * ent); */
#define lmn_atomlist_end(p_atomset_entry) (LMN_SATOM(p_atomset_entry))

extern struct st_hash_type type_memhash;

#endif /* LMN_MEMBRANE_H */
