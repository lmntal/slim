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

/* cldoc:begin-category(Lmntal::Membrane) */

#include "lmntal.h"
#include "atom.h"
#include "utility/internal_hash.h"
#include "utility/vector.h"
#include "rule.h"
#include "slim_header/port.h"
#include "error.h"
#include "functor.h"

typedef struct AtomListEntry *AtomListEntryRef;
typedef struct AtomListEntry **AtomSet;

#define NEW_ATOMLIST

LmnSAtom atomlist_head(AtomListEntryRef lst);
LmnSAtom lmn_atomlist_end(AtomListEntryRef lst);
//#define atomlist_head(L)                    (LMN_SATOM((L)->head))
//#define lmn_atomlist_end(p_atomset_entry)   (LMN_SATOM(p_atomset_entry))
int atomlist_ent_num(AtomListEntryRef lst);
void atomlist_set_num(AtomListEntryRef lst, int n);
void atomlist_add_num(AtomListEntryRef lst, int n);

void atomlist_modify_num(AtomListEntryRef ent, int n);
BOOL atomlist_is_empty(AtomListEntryRef ent);

/* アトムリストasを空にする. */
void atomlist_set_empty(AtomListEntryRef ent);

/* アトムリストALからアトムAを削除する.
 * ただし, リストのつなぎ変えだけを行い, 膜からのアトムAのdeleteやatomのfreeはしない */
void remove_from_atomlist(LmnSAtom a, AtomListEntryRef ent);

/* アトムリストentにおいて, アトムprvとアトムnxtの間にアトムinsを挿入する.
 * ただし, prvにNULLを渡した場合はnxtのprevポイント先をprvとして扱う. */
void insert_to_atomlist(LmnSAtom prv, LmnSAtom ins, LmnSAtom nxt,
                                      AtomListEntryRef ent);
/* アトムリストALの末尾にアトムAを追加する. */
void push_to_atomlist(LmnSAtom a, AtomListEntryRef ent);

int atomlist_get_entries_num(AtomListEntryRef ent);

/* append e2 to e1 */
void atomlist_append(AtomListEntryRef e1, AtomListEntryRef e2);

/* return NULL when atomlist doesn't exist. */
LmnSAtom atomlist_get_record(AtomListEntryRef atomlist, int findatomid);

void atomlist_put_record(AtomListEntryRef lst, int id, LmnAtom record);

void move_atom_to_atomlist_tail(LmnSAtom a, LmnMembrane *mem);
void move_atom_to_atomlist_head(LmnSAtom a, LmnMembrane *mem);
void move_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembrane *mem);
void move_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembrane *mem);

/** -----
 *  リンクオブジェクトの代替
 */
typedef struct LinkObj *LinkObjRef;

LmnAtom LinkObjGetAtom(LinkObjRef o);
LmnLinkAttr LinkObjGetPos(LinkObjRef o);
LinkObjRef LinkObj_make(LmnAtom ap, LmnLinkAttr pos);

/** -----
 *  膜
 */

extern struct st_hash_type type_memhash;

struct LmnMembrane {
  AtomSet              atomset;
  ProcessID            id;
  unsigned int         max_functor;
  unsigned int         atomset_size;
  unsigned int         atom_symb_num;  /* # of symbol atom except proxy */
  unsigned int         atom_data_num;
  lmn_interned_str     name;
  BOOL                 is_activated;
  LmnMembrane          *parent;
  LmnMembrane          *child_head;
  LmnMembrane          *prev, *next;
  struct Vector        rulesets;
};

#define LMN_MEM_NAME_ID(MP)          ((MP)->name)
#define LMN_MEM_NAME(MP)             LMN_SYMBOL_STR(LMN_MEM_NAME_ID(MP))
#define lmn_mem_parent(M)            ((M)->parent)
#define lmn_mem_is_active(M)         ((M)->is_activated)
#define lmn_mem_max_functor(M)       ((M)->max_functor)
#define lmn_mem_set_name(M, N)       ((M)->name = (N))
#define lmn_mem_set_active(M, F)     ((M)->is_activated = (F))
#define lmn_mem_get_rulesets(M)      (&((M)->rulesets))
#define lmn_mem_ruleset_num(M)       (vec_num(lmn_mem_get_rulesets(M)))
#define lmn_mem_get_ruleset(M, I)    ((LmnRuleSetRef)vec_get(lmn_mem_get_rulesets(M), (I)))
#define lmn_mem_symb_atom_num(M)     ((M)->atom_symb_num)
#define lmn_mem_symb_atom_set(M, N)  ((M)->atom_symb_num = (N))
#define lmn_mem_data_atom_num(M)     ((M)->atom_data_num)
#define lmn_mem_data_atom_set(M, N)  ((M)->atom_data_num = (N))
#define lmn_mem_atom_num(M)          (lmn_mem_symb_atom_num(M) + lmn_mem_data_atom_num(M))
#define lmn_mem_natoms(M, N)         (lmn_mem_atom_num(M) == (N))
#define lmn_mem_natoms_copy(M, N)    (lmn_mem_symb_atom_set(M, lmn_mem_symb_atom_num(N)), \
                                      lmn_mem_data_atom_set(M, lmn_mem_data_atom_num(N)))
#define lmn_mem_symb_atom_add(M, N)  ((M)->atom_symb_num += (N))
#define lmn_mem_symb_atom_sub(M, N)  ((M)->atom_symb_num -= (N))
#define lmn_mem_symb_atom_inc(M)     (lmn_mem_symb_atom_add(M, 1))
#define lmn_mem_symb_atom_dec(M)     (lmn_mem_symb_atom_sub(M, 1))

#define lmn_mem_data_atom_add(M, N)  ((M)->atom_data_num += (N))
#define lmn_mem_data_atom_sub(M, N)  ((M)->atom_data_num -= (N))
#define lmn_mem_data_atom_inc(M)     (lmn_mem_data_atom_add(M, 1))
#define lmn_mem_data_atom_dec(M)     (lmn_mem_data_atom_sub(M, 1))

#define lmn_mem_child_head(M)        ((M)->child_head)
#define lmn_mem_next(M)              ((M)->next)
#define lmn_mem_prev(M)              ((M)->prev)
#define lmn_mem_id(M)               ((M)->id)
#define lmn_mem_set_id(M, n)        ((M)->id = (n))

LmnMembrane *lmn_mem_make(void);
void lmn_mem_free(LmnMembrane *mem);
void lmn_mem_rulesets_destroy(Vector *rulesets);
void lmn_mem_drop(LmnMembrane *mem);
void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
void lmn_mem_add_ruleset_sort(Vector *rulesets, LmnRuleSetRef ruleset);

unsigned long lmn_mem_root_space(LmnMembrane *mem);
unsigned long lmn_mem_space(LmnMembrane *mem);
BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);

void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LmnMembrane *lmn_mem_copy_with_map_ex(LmnMembrane *srcmem, ProcessTableRef  *copymap);
LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *srcmem, ProcessTableRef *copymap);
LmnMembrane *lmn_mem_copy(LmnMembrane *srcmem);
LmnMembrane *lmn_mem_copy_ex(LmnMembrane *src);

ProcessTableRef lmn_mem_copy_cells_ex(LmnMembrane *dest,
                                 LmnMembrane *src,
                                 BOOL        hl_nd);
ProcessTableRef lmn_mem_copy_cells(LmnMembrane *dest, LmnMembrane *srcmem);
void lmn_mem_remove_proxies(LmnMembrane *mem);
void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem);
void lmn_mem_remove_temporary_proxies(LmnMembrane *mem);
void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem);

BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec);
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
BOOL lmn_mem_is_hlground(Vector *srcvec,
                         Vector *avovec,
                         unsigned long *natoms,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_copy_ground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTableRef *ret_atommap);
void lmn_mem_copy_hlground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTableRef *ret_atommap,
                         ProcessTableRef *ret_hlinkmap,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_remove_ground(LmnMembrane *mem, Vector *srcvec);
void lmn_mem_remove_hlground(LmnMembrane *mem,
                             Vector *srcvec,
                             ProcessTableRef *attr_sym,
                             Vector *attr_data,
                             Vector *attr_data_at);
void lmn_mem_free_ground(Vector *srcvec);
void lmn_mem_free_hlground(Vector *srcvec,
                           ProcessTableRef *attr_sym,
                           Vector *attr_data,
                           Vector *attr_data_at);
void lmn_mem_delete_ground(LmnMembrane *mem, Vector *srcvec);
BOOL ground_atoms(Vector *srcvec,
                  Vector *avovec,
                  ProcessTableRef *atoms,
                  unsigned long *natoms,
                  ProcessTableRef *hlinks,
                  ProcessTableRef *attr_functors,
                  Vector *attr_dataAtoms,
                  Vector *attr_dataAtom_attrs);
BOOL ground_atoms_old(Vector *srcvec,
                      Vector *avovec,
                      HashSet **atoms,
                      unsigned long *natoms);

void move_symbol_atom_to_atomlist_head(LmnSAtom a, LmnMembrane *mem);
void move_symbol_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembrane * mem);
void move_symbol_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembrane *mem);

void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem);
void lmn_mem_free_rec(LmnMembrane *mem);
void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem);
AtomListEntryRef lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f);
void lmn_mem_activate_ancestors(LmnMembrane *mem);
BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count);
int  lmn_mem_child_mem_num(LmnMembrane *mem);
void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem);
LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f);
unsigned int lmn_mem_count_children(LmnMembrane *mem);
unsigned int lmn_mem_count_descendants(LmnMembrane *mem);
BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count);
void lmn_mem_remove_data_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
void mem_remove_symbol_atom_with_buddy_data(LmnMembrane *mem, LmnSAtom atom);
void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f);
void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSetRef ruleset);
void lmn_mem_copy_rules(LmnMembrane *dest, LmnMembrane *src);
void lmn_mem_clearrules(LmnMembrane *src);
void newlink_symbol_and_something(LmnSAtom atom0, int pos, LmnAtom atom1, LmnLinkAttr attr);



void lmn_mem_newlink(LmnMembrane *mem,
                     LmnAtom atom0, LmnLinkAttr attr0, int pos0,
                     LmnAtom atom1, LmnLinkAttr attr1, int pos1);
void lmn_newlink_in_symbols(LmnSAtom atom0, int pos0,
                            LmnSAtom atom1, int pos1);
void lmn_newlink_with_ex(LmnMembrane *mem,
                         LmnSAtom atom0, LmnLinkAttr attr0, int pos0,
                         LmnSAtom atom1, LmnLinkAttr attr1, int pos1);
void lmn_mem_link_data_atoms(LmnMembrane *mem,
                             LmnAtom d1, LmnLinkAttr attr1,
                             LmnAtom d2, LmnLinkAttr attr2);
void lmn_mem_unify_atom_args(LmnMembrane *mem,
                             LmnSAtom atom1, int pos1,
                             LmnSAtom atom2, int pos2);
void lmn_mem_unify_symbol_atom_args(LmnSAtom atom1, int pos1,
                                    LmnSAtom atom2, int pos2);
void lmn_relink_symbols(LmnSAtom atom0, int pos0,
                        LmnSAtom atom1, int pos1);
void lmn_mem_relink_atom_args(LmnMembrane *mem,
                              LmnAtom atom0, LmnLinkAttr attr0, int pos0,
                              LmnAtom atom1, LmnLinkAttr attr1, int pos1);

typedef int AtomListIter;
#define atomlist_iter_initializer(AS)      (0)
#define atomlist_iter_condition(Mem, Iter) ((Iter) < lmn_mem_max_functor(Mem))
#define atomlist_iter_next(Iter)           ((Iter)++)
#define atomlist_iter_get_entry(Mem, Iter) lmn_mem_get_atomlist(Mem, Iter)
#define atomlist_iter_get_functor(Iter)    (Iter)


#define EACH_ATOMLIST_WITH_FUNC(MEM, ENT, F, CODE)                             \
  do {                                                                         \
    AtomListIter __iter;                                                       \
    for (__iter = atomlist_iter_initializer((MEM)->atomset);                   \
         atomlist_iter_condition(MEM, __iter);                                 \
         atomlist_iter_next(__iter)) {                                         \
      (ENT) = atomlist_iter_get_entry(MEM, __iter);                            \
      (F)   = atomlist_iter_get_functor(__iter);                               \
      if (!(ENT)) continue;                                                    \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)
#define EACH_ATOMLIST(MEM, ENT, CODE)                                          \
  do {                                                                         \
    AtomListIter __iter;                                                       \
    for (__iter = atomlist_iter_initializer((MEM)->atomset);                   \
         atomlist_iter_condition(MEM, __iter);                                 \
         atomlist_iter_next(__iter)) {                                         \
      (ENT) = atomlist_iter_get_entry(MEM, __iter);                            \
      if (!(ENT)) continue;                                                    \
      (CODE);                                                                  \
    }                                                                          \
  } while (0)

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

#define EACH_ATOM_THREAD(V, ENT, ID, NUM, CODE)				\
    int id = (ID);							\
    if ((ENT)) {							\
      for ((V)  = atomlist_head((ENT));					\
	   (V) != lmn_atomlist_end((ENT));				\
	   (V)  = LMN_SATOM_GET_NEXT_RAW((V))) {			\
	if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR && id == 0) { \
	  (CODE);							\
	  id=(NUM);							\
	}								\
	id--;								\
      }									\
    }

#define EACH_ATOM_THREAD_OPT(V, ENT, ID, NUM, START, CODE)	\
    int id = (ID);						\
    int flag = 1;						\
    if ((ENT)) {						\
      if ((START) == NULL ) {					\
	(V)  = atomlist_head((ENT));				\
	flag--;							\
      }else{							\
	(V)  = (START);						\
      }								\
      for (;							\
	   (V) != lmn_atomlist_end((ENT)) || flag ;		\
	   (V)  = LMN_SATOM_GET_NEXT_RAW((V))) {		\
	if((V) == lmn_atomlist_end((ENT))){			\
	  (V)  = atomlist_head((ENT));				\
	  id = (ID);						\
	  flag--;						\
	}							\
	if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR	\
	    && id == 0) {					\
	  (CODE);						\
	  id=(NUM);						\
	}							\
	id--;							\
      }								\
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
    AtomListEntryRef __ent;                                                      \
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


/* LmnSAtom* lmn_atomlist_end(AtomSetEntry * ent); */

/* cldoc:end-category() */

#endif /* LMN_MEMBRANE_H */
