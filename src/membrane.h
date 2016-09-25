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

typedef struct LmnMembrane *LmnMembraneRef;


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

void move_atom_to_atomlist_tail(LmnSAtom a, LmnMembraneRef mem);
void move_atom_to_atomlist_head(LmnSAtom a, LmnMembraneRef mem);
void move_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembraneRef mem);
void move_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembraneRef mem);

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


lmn_interned_str LMN_MEM_NAME_ID(LmnMembraneRef m);
const char *LMN_MEM_NAME(LmnMembraneRef m);
LmnMembraneRef lmn_mem_parent(LmnMembraneRef m);
void lmn_mem_set_parent(LmnMembraneRef m, LmnMembraneRef parent);

BOOL lmn_mem_is_active(LmnMembraneRef m);
unsigned int lmn_mem_max_functor(LmnMembraneRef m);
void lmn_mem_set_name(LmnMembraneRef m, lmn_interned_str name);
void lmn_mem_set_active(LmnMembraneRef m, BOOL is_activated);
struct Vector * lmn_mem_get_rulesets(LmnMembraneRef m);
int lmn_mem_ruleset_num(LmnMembraneRef m);
LmnRuleSetRef lmn_mem_get_ruleset(LmnMembraneRef m, int i);
unsigned int lmn_mem_symb_atom_num(LmnMembraneRef m);
void lmn_mem_symb_atom_set(LmnMembraneRef m, unsigned int n);
unsigned int lmn_mem_data_atom_num(LmnMembraneRef m);
void lmn_mem_data_atom_set(LmnMembraneRef m, unsigned int n);
unsigned int lmn_mem_atom_num(LmnMembraneRef m);
BOOL lmn_mem_natoms(LmnMembraneRef m, unsigned int n);
void lmn_mem_natoms_copy(LmnMembraneRef m, LmnMembraneRef n);
void lmn_mem_symb_atom_add(LmnMembraneRef m, int n);
void lmn_mem_symb_atom_sub(LmnMembraneRef m, int n);
void lmn_mem_symb_atom_inc(LmnMembraneRef m);
void lmn_mem_symb_atom_dec(LmnMembraneRef m);

void lmn_mem_data_atom_add(LmnMembraneRef m, int n);
void lmn_mem_data_atom_sub(LmnMembraneRef m, int n);
void lmn_mem_data_atom_inc(LmnMembraneRef m);
void lmn_mem_data_atom_dec(LmnMembraneRef m);

LmnMembraneRef lmn_mem_child_head(LmnMembraneRef m);
LmnMembraneRef lmn_mem_next(LmnMembraneRef m);
LmnMembraneRef lmn_mem_prev(LmnMembraneRef m);
ProcessID lmn_mem_id(LmnMembraneRef m);
void lmn_mem_set_id(LmnMembraneRef m, ProcessID n);

LmnMembraneRef lmn_mem_make(void);
void lmn_mem_free(LmnMembraneRef mem);
void lmn_mem_rulesets_destroy(Vector *rulesets);
void lmn_mem_drop(LmnMembraneRef mem);
void mem_push_symbol_atom(LmnMembraneRef mem, LmnSAtom atom);
void lmn_mem_add_ruleset_sort(Vector *rulesets, LmnRuleSetRef ruleset);

unsigned long lmn_mem_root_space(LmnMembraneRef mem);
unsigned long lmn_mem_space(LmnMembraneRef mem);
BOOL lmn_mem_equals(LmnMembraneRef mem1, LmnMembraneRef mem2);

void lmn_mem_move_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem);
LmnMembraneRef lmn_mem_copy_with_map_ex(LmnMembraneRef srcmem, ProcessTableRef  *copymap);
LmnMembraneRef lmn_mem_copy_with_map(LmnMembraneRef srcmem, ProcessTableRef *copymap);
LmnMembraneRef lmn_mem_copy(LmnMembraneRef srcmem);
LmnMembraneRef lmn_mem_copy_ex(LmnMembraneRef src);

ProcessTableRef lmn_mem_copy_cells_ex(LmnMembraneRef dest,
                                 LmnMembraneRef src,
                                 BOOL        hl_nd);
ProcessTableRef lmn_mem_copy_cells(LmnMembraneRef dest, LmnMembraneRef srcmem);
void lmn_mem_remove_proxies(LmnMembraneRef mem);
void lmn_mem_insert_proxies(LmnMembraneRef mem, LmnMembraneRef child_mem);
void lmn_mem_remove_temporary_proxies(LmnMembraneRef mem);
void lmn_mem_remove_toplevel_proxies(LmnMembraneRef mem);

BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec);
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
BOOL lmn_mem_is_hlground(Vector *srcvec,
                         Vector *avovec,
                         unsigned long *natoms,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_copy_ground(LmnMembraneRef mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTableRef *ret_atommap);
void lmn_mem_copy_hlground(LmnMembraneRef mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTableRef *ret_atommap,
                         ProcessTableRef *ret_hlinkmap,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_remove_ground(LmnMembraneRef mem, Vector *srcvec);
void lmn_mem_remove_hlground(LmnMembraneRef mem,
                             Vector *srcvec,
                             ProcessTableRef *attr_sym,
                             Vector *attr_data,
                             Vector *attr_data_at);
void lmn_mem_free_ground(Vector *srcvec);
void lmn_mem_free_hlground(Vector *srcvec,
                           ProcessTableRef *attr_sym,
                           Vector *attr_data,
                           Vector *attr_data_at);
void lmn_mem_delete_ground(LmnMembraneRef mem, Vector *srcvec);
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

void move_symbol_atom_to_atomlist_head(LmnSAtom a, LmnMembraneRef mem);
void move_symbol_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembraneRef mem);
void move_symbol_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembraneRef mem);

void lmn_mem_remove_mem(LmnMembraneRef parent, LmnMembraneRef mem);
void lmn_mem_free_rec(LmnMembraneRef mem);
void lmn_mem_delete_mem(LmnMembraneRef parent, LmnMembraneRef mem);
AtomListEntryRef lmn_mem_get_atomlist(LmnMembraneRef mem, LmnFunctor f);
void lmn_mem_activate_ancestors(LmnMembraneRef mem);
BOOL lmn_mem_nmems(LmnMembraneRef mem, unsigned int count);
int  lmn_mem_child_mem_num(LmnMembraneRef mem);
void lmn_mem_add_child_mem(LmnMembraneRef parentmem, LmnMembraneRef newmem);
LmnSAtom lmn_mem_newatom(LmnMembraneRef mem, LmnFunctor f);
unsigned int lmn_mem_count_children(LmnMembraneRef mem);
unsigned int lmn_mem_count_descendants(LmnMembraneRef mem);
BOOL lmn_mem_nfreelinks(LmnMembraneRef mem, unsigned int count);
void lmn_mem_remove_data_atom(LmnMembraneRef mem, LmnAtom atom, LmnLinkAttr attr);
void mem_remove_symbol_atom(LmnMembraneRef mem, LmnSAtom atom);
void mem_remove_symbol_atom_with_buddy_data(LmnMembraneRef mem, LmnSAtom atom);
void lmn_mem_remove_atom(LmnMembraneRef mem, LmnAtom atom, LmnLinkAttr attr);
void lmn_mem_delete_atom(LmnMembraneRef mem, LmnAtom atom, LmnLinkAttr attr);
void lmn_mem_push_atom(LmnMembraneRef mem, LmnAtom atom, LmnLinkAttr attr);
void alter_functor(LmnMembraneRef mem, LmnSAtom atom, LmnFunctor f);
void lmn_mem_add_ruleset(LmnMembraneRef mem, LmnRuleSetRef ruleset);
void lmn_mem_copy_rules(LmnMembraneRef dest, LmnMembraneRef src);
void lmn_mem_clearrules(LmnMembraneRef src);
void newlink_symbol_and_something(LmnSAtom atom0, int pos, LmnAtom atom1, LmnLinkAttr attr);



void lmn_mem_newlink(LmnMembraneRef mem,
                     LmnAtom atom0, LmnLinkAttr attr0, int pos0,
                     LmnAtom atom1, LmnLinkAttr attr1, int pos1);
void lmn_newlink_in_symbols(LmnSAtom atom0, int pos0,
                            LmnSAtom atom1, int pos1);
void lmn_newlink_with_ex(LmnMembraneRef mem,
                         LmnSAtom atom0, LmnLinkAttr attr0, int pos0,
                         LmnSAtom atom1, LmnLinkAttr attr1, int pos1);
void lmn_mem_link_data_atoms(LmnMembraneRef mem,
                             LmnAtom d1, LmnLinkAttr attr1,
                             LmnAtom d2, LmnLinkAttr attr2);
void lmn_mem_unify_atom_args(LmnMembraneRef mem,
                             LmnSAtom atom1, int pos1,
                             LmnSAtom atom2, int pos2);
void lmn_mem_unify_symbol_atom_args(LmnSAtom atom1, int pos1,
                                    LmnSAtom atom2, int pos2);
void lmn_relink_symbols(LmnSAtom atom0, int pos0,
                        LmnSAtom atom1, int pos1);
void lmn_mem_relink_atom_args(LmnMembraneRef mem,
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
