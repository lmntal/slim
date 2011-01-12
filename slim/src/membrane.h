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
#include "slim_header/port.h"
#include "error.h"
#include "functor.h"

#ifdef TIME_OPT
typedef struct AtomListEntry **AtomSet;
#else
typedef struct SimpleHashtbl AtomSet;
#endif

/* この構造体をAtomとして扱うことで,この構造体自身が
   HeadとTailの両方の役目を果たしている */
typedef struct AtomListEntry {
  LmnWord tail, head;
  struct SimpleHashtbl record;
} AtomListEntry;

/* TODO: 構造体の定義を隠すために.cファイルに定義を移動する */
struct LmnMembrane {
  LmnMembrane 	       *parent;
  LmnMembrane 	       *child_head;
  LmnMembrane          *prev, *next;
#ifdef TIME_OPT
  struct AtomListEntry **atomset;
#else
  struct SimpleHashtbl atomset;
#endif
  unsigned int         max_functor;
  unsigned int         atomset_size;
  BOOL                 is_activated;
  unsigned long        atom_num; /* # of atom except proxy */
  lmn_interned_str     name;
  struct Vector	       rulesets;
  ProcessID            id;
};

inline static int lmn_mem_ruleset_num(LmnMembrane *mem) {
  return vec_num(&mem->rulesets);
}

inline static LmnRuleSet lmn_mem_get_ruleset(LmnMembrane *mem, int i) {
  return (LmnRuleSet)vec_get(&mem->rulesets, i);
}

inline static Vector *lmn_mem_get_rulesets(LmnMembrane *mem) {
  return &(mem->rulesets);
}

inline static LmnMembrane *lmn_mem_parent(LmnMembrane *mem) {
  return mem->parent;
}

inline static void lmn_mem_set_active(LmnMembrane *mem, BOOL active) {
  mem->is_activated = active;
}

inline static BOOL lmn_mem_is_active(LmnMembrane *mem) {
  return mem->is_activated;
}

/* 自身を含めた全ての先祖膜を起こす */
inline static void lmn_mem_activate_ancestors(LmnMembrane *mem) {
  LmnMembrane *cur;
  for (cur = mem; cur; cur = cur->parent) {
    lmn_mem_set_active(mem, TRUE);
  }
}


#define atomlist_head(LIST)    (LMN_SATOM((LIST)->head))
AtomListEntry *make_atomlist(void);
void free_atomlist(AtomListEntry *as);

#ifdef TIME_OPT
#define EACH_ATOMLIST_WITH_FUNC(MEM, ENT, F, CODE)                             \
  do {                                                                         \
    int __i_atomlist;                                                          \
    for (__i_atomlist = 0;                                                     \
         __i_atomlist < (MEM)->max_functor;                                    \
         __i_atomlist++) {                                                     \
      (ENT) = lmn_mem_get_atomlist((MEM), __i_atomlist);                       \
      (F)   = __i_atomlist;                                                    \
      if (!(ENT)) continue;                                                    \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#define EACH_ATOMLIST(MEM, ENT, CODE)                                          \
  do {                                                                         \
    int __i_atomlist;                                                          \
    for (__i_atomlist = 0;                                                     \
         __i_atomlist < (MEM)->max_functor;                                    \
         __i_atomlist++) {                                                     \
      (ENT) = lmn_mem_get_atomlist((MEM), __i_atomlist);                       \
      if (!(ENT)) continue;                                                    \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#else
#define EACH_ATOMLIST_WITH_FUNC(MEM, ENT, F, CODE)                             \
  do {                                                                         \
    HashIterator __iter;                                                       \
    for (__iter = hashtbl_iterator(&((MEM)->atomset));                         \
         !hashtbliter_isend(&__iter);                                          \
         hashtbliter_next(&__iter)) {                                          \
      (ENT) = (AtomListEntry *)hashtbliter_entry(&__iter)->data;               \
      (F)   = hashtbliter_entry(&__iter)->key;                                 \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#define EACH_ATOMLIST(MEM, ENT, CODE)                                          \
  do {                                                                         \
    HashIterator __iter;                                                       \
    for (__iter = hashtbl_iterator(&((MEM)->atomset));                         \
         !hashtbliter_isend(&__iter);                                          \
         hashtbliter_next(&__iter)) {                                          \
      (ENT) = (AtomListEntry *)hashtbliter_entry(&__iter)->data;               \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#endif

/* アトムリストENTのアトムに対してCODEを実行する。
   それぞれのループでCODEを実行する前に、Vにアトムが割り当てられる。
   履歴アトムがアトムリストにある場合は、読み飛ばす */
#define EACH_ATOM(V, ENT, CODE)                                                \
  if ((ENT)) {                                                                 \
    for ((V)  = atomlist_head((ENT));                                          \
         (V) != lmn_atomlist_end((ENT));                                       \
         (V)  = LMN_SATOM_GET_NEXT_RAW((V))) {                                 \
      if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                  \
        (CODE);                                                                \
      }                                                                        \
    }                                                                          \
  }

#define EACH_FUNC_ATOM(MEM, F, V, CODE)                                        \
  do {                                                                         \
    AtomListEntry *__ent  = lmn_mem_get_atomlist((MEM), (F));                  \
    if (__ent) {                                                               \
      for ((V) = atomlist_head(__ent);                                         \
           (V) != lmn_atomlist_end(__ent);                                     \
           (V) = LMN_SATOM_GET_NEXT_RAW((V))) {                                \
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while(0)

#define ALL_ATOMS(MEM, V, CODE)                                                \
  do {                                                                         \
    AtomListEntry *__ent;                                                      \
    EACH_ATOMLIST((MEM), __ent, ({                                             \
      for ((V)  = atomlist_head(__ent);                                        \
           (V) != lmn_atomlist_end(__ent);                                     \
           (V)  = LMN_SATOM_GET_NEXT_RAW((V))) {                               \
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR) {                \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }));                                                                       \
  } while (0)


#define LMN_MEM_NAME_ID(MP)   ((MP)->name)
#define LMN_MEM_NAME(MP)      LMN_SYMBOL_STR(LMN_MEM_NAME_ID(MP))

#ifdef TIME_OPT
#  define lmn_mem_id(M)         ((M)->id)
#  define lmn_mem_set_id(M, n)  ((M)->id = (n))
#else
#  define lmn_mem_id(M)         ((LmnWord)(M))
#  define lmn_mem_set_id(M, n)
#endif

LmnMembrane *lmn_mem_make(void);
void lmn_mem_set_name(LmnMembrane *mem, lmn_interned_str name);
int lmn_mem_max_functor(LmnMembrane *mem);
void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem);
void lmn_mem_rulesets_destroy(Vector *rulesets);
void lmn_mem_clearrules(LmnMembrane *src);
void lmn_mem_free(LmnMembrane *mem);
void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem);
void lmn_mem_drop(LmnMembrane *mem);
void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem);
LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f);
void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
void lmn_mem_add_ruleset_sort(Vector *rulesets, LmnRuleSet ruleset);
void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset);
BOOL lmn_mem_natoms(LmnMembrane *mem, unsigned int count);
unsigned long lmn_mem_atom_num(LmnMembrane *mem);
AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f);
LmnSAtom atomlist_get_record(AtomListEntry *atomlist, int findatomid);
void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom);

unsigned long lmn_mem_root_space(LmnMembrane *mem);
unsigned long lmn_mem_space(LmnMembrane *mem);

void lmn_mem_remove_data_atom(LmnMembrane *mem,
                                         LmnAtom atom,
                                         LmnLinkAttr attr);
void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
unsigned int lmn_mem_count_descendants(LmnMembrane *mem);
unsigned int lmn_mem_count_children(LmnMembrane *mem);
BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);


/* アトムをアトムリストから削除する.
   リストのつなぎ変えだけを行う */
#define REMOVE_FROM_ATOMLIST(atom)                 \
  do { \
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(atom), LMN_SATOM_GET_PREV(atom)); \
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(atom), LMN_SATOM_GET_NEXT_RAW(atom)); \
  } while (0)

BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count);
int lmn_mem_child_mem_num(LmnMembrane *mem);
BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count);
void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
void lmn_mem_newlink(LmnMembrane *mem,
                     LmnAtom atom0,
                     LmnLinkAttr attr0,
                     int pos0,
                     LmnAtom atom1,
                     LmnLinkAttr attr1,
                     int pos1);
void lmn_newlink_in_symbols(LmnSAtom atom0,
                            int pos0,
                            LmnSAtom atom1,
                            int pos1);
void lmn_mem_link_data_atoms(LmnMembrane *mem,
                             LmnAtom d1,
                             LmnLinkAttr attr1,
                             LmnAtom d2,
                             LmnLinkAttr attr2);
void lmn_mem_unify_atom_args(LmnMembrane *mem,
                             LmnSAtom atom1,
                             int pos1,
                             LmnSAtom atom2,
                             int pos2);
void lmn_mem_unify_symbol_atom_args(LmnSAtom atom1,
                                    int pos1,
                                    LmnSAtom atom2,
                                    int pos2);
void lmn_relink_symbols(LmnSAtom atom0,
                        int pos0,
                        LmnSAtom atom1,
                        int pos1);
void lmn_mem_relink_atom_args(LmnMembrane *mem,
                              LmnAtom atom0,
                              LmnLinkAttr attr0,
                              int pos0,
                              LmnAtom atom1,
                              LmnLinkAttr attr1,
                              int pos1);
void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *srcmem, ProcessTbl *copymap);
LmnMembrane *lmn_mem_copy(LmnMembrane *srcmem);
ProcessTbl lmn_mem_copy_cells(LmnMembrane *dest, LmnMembrane *srcmem);
void lmn_mem_remove_proxies(LmnMembrane *mem);
void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem);
void lmn_mem_remove_temporary_proxies(LmnMembrane *mem);
void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem);

BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec);
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
void lmn_mem_copy_ground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTbl *ret_atommap);
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
void lmn_mem_copy_rules(LmnMembrane *dest, LmnMembrane *src);
BOOL ground_atoms(Vector *srcvec,
                  Vector *avovec,
                  HashSet **atoms,
                  unsigned long *natoms);
BOOL ground_atoms_old(Vector *srcvec,
                      Vector *avovec,
                      HashSet **atoms,
                      unsigned long *natoms);
void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f);

#endif /* LMN_MEMBRANE_H */
