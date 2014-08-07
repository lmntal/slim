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

#define NEW_ATOMLIST

/** ----
 *  AtomListEntry.
 *  同一ファンクタのアトムをリスト単位でまとめておくための機構
 */

/* この構造体をAtomとして扱うことで,この構造体自身が
   HeadとTailの両方の役目を果たしている */
typedef struct AtomListEntry {
  LmnWord tail, head;
#ifdef NEW_ATOMLIST
  int n;
#endif
  struct SimpleHashtbl *record;
} AtomListEntry;


#define atomlist_head(L)                    (LMN_SATOM((L)->head))
#define lmn_atomlist_end(p_atomset_entry)   (LMN_SATOM(p_atomset_entry))
#ifdef NEW_ATOMLIST
# define atomlist_ent_num(L)                 ((L)->n)
# define atomlist_set_num(L, N)              ((L)->n = (N))
# define atomlist_add_num(L, N)              ((L)->n += (N))
#else
# define atomlist_ent_num(L)
# define atomlist_set_num(L, N)
# define atomlist_add_num(L, N)
#endif

static inline void atomlist_modify_num(AtomListEntry *ent, int n) {
  atomlist_add_num(ent, n);
}

static inline BOOL atomlist_is_empty(AtomListEntry *ent) {
  return atomlist_head(ent) == LMN_SATOM(ent);
}

/* アトムリストasを空にする. */
static inline void atomlist_set_empty(AtomListEntry *ent) {
  LMN_SATOM_SET_PREV(ent, ent);
  LMN_SATOM_SET_NEXT(ent, ent);
  atomlist_set_num(ent, 0);
}

/* アトムリストALからアトムAを削除する.
 * ただし, リストのつなぎ変えだけを行い, 膜からのアトムAのdeleteやatomのfreeはしない */
static inline void remove_from_atomlist(LmnSAtom a, AtomListEntry *ent) {
  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a),     LMN_SATOM_GET_NEXT_RAW(a));
  if (ent) {
    atomlist_modify_num(ent, -1);
  }
}

/* アトムリストentにおいて, アトムprvとアトムnxtの間にアトムinsを挿入する.
 * ただし, prvにNULLを渡した場合はnxtのprevポイント先をprvとして扱う. */
static inline void insert_to_atomlist(LmnSAtom prv, LmnSAtom ins, LmnSAtom nxt,
                                      AtomListEntry *ent)
{
  if (!prv) {
    prv = LMN_SATOM_GET_PREV(nxt);
  }

  LMN_SATOM_SET_NEXT(prv, ins);
  LMN_SATOM_SET_PREV(ins, prv);
  LMN_SATOM_SET_NEXT(ins, nxt);
  LMN_SATOM_SET_PREV(nxt, ins);

  if (ent) {
    atomlist_modify_num(ent, +1);
  }
}

/* アトムリストALの末尾にアトムAを追加する. */
static inline void push_to_atomlist(LmnSAtom a, AtomListEntry *ent) {
  LMN_SATOM_SET_NEXT(a, ent);
  LMN_SATOM_SET_PREV(a, ent->tail);
  LMN_SATOM_SET_NEXT(ent->tail, a);
  ent->tail = (LmnWord)a;
  atomlist_modify_num(ent, +1);
}

static inline int atomlist_get_entries_num(AtomListEntry *ent) {
  if (!ent) {
    return 0;
  } else {
#ifdef NEW_ATOMLIST
    /* O(1) */
    return ent->n;

#else
    /* O(N) */
    LmnSAtom atom;
    int ret = 0;
    for (atom = atomlist_head(ent);
         atom != lmn_atomlist_end(ent);
         atom = LMN_SATOM_GET_NEXT_RAW(atom)) {
      if (LMN_SATOM_GET_FUNCTOR(atom) != LMN_RESUME_FUNCTOR) {
        ret++;
      }
    }

    return ret;
#endif

  }
}

/* append e2 to e1 */
static inline void atomlist_append(AtomListEntry *e1, AtomListEntry *e2)
{
  if (atomlist_head(e2) != lmn_atomlist_end(e2)) {/* true if e2 is not empty */
    LMN_SATOM_SET_NEXT(e1->tail, e2->head);
    LMN_SATOM_SET_PREV(e2->head, e1->tail);
    LMN_SATOM_SET_NEXT(e2->tail, e1);
    e1->tail = e2->tail;
    atomlist_modify_num(e1, atomlist_get_entries_num(e2));
  }
  atomlist_set_empty(e2);
}

/* return NULL when atomlist doesn't exist. */
static inline LmnSAtom atomlist_get_record(AtomListEntry *atomlist, int findatomid) {
  if (atomlist->record) {
    return (LmnSAtom)hashtbl_get_default(atomlist->record, findatomid, 0);
  } else {
    atomlist->record = hashtbl_make(4);
    return NULL;
  }
}

/** -----
 *  リンクオブジェクトの代替
 */
typedef struct LinkObj {
  LmnAtom ap;
  LmnLinkAttr pos;
} *LinkObj;

LinkObj LinkObj_make(LmnAtom ap, LmnLinkAttr pos);

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
#define lmn_mem_get_ruleset(M, I)    ((LmnRuleSet)vec_get(lmn_mem_get_rulesets(M), (I)))
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
#ifdef TIME_OPT
# define lmn_mem_id(M)               ((M)->id)
# define lmn_mem_set_id(M, n)        ((M)->id = (n))
#else
# define lmn_mem_id(M)               ((LmnWord)(M))
# define lmn_mem_set_id(M, n)
#endif

LmnMembrane *lmn_mem_make(void);
void lmn_mem_free(LmnMembrane *mem);
void lmn_mem_rulesets_destroy(Vector *rulesets);
void lmn_mem_drop(LmnMembrane *mem);
void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
void lmn_mem_add_ruleset_sort(Vector *rulesets, LmnRuleSet ruleset);

unsigned long lmn_mem_root_space(LmnMembrane *mem);
unsigned long lmn_mem_space(LmnMembrane *mem);
BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);

void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem);
LmnMembrane *lmn_mem_copy_with_map_ex(LmnMembrane *srcmem, ProcessTbl  *copymap);
LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *srcmem, ProcessTbl *copymap);
LmnMembrane *lmn_mem_copy(LmnMembrane *srcmem);
LmnMembrane *lmn_mem_copy_ex(LmnMembrane *src);
inline
ProcessTbl lmn_mem_copy_cells_ex(LmnMembrane *dest,
                                 LmnMembrane *src,
                                 BOOL        hl_nd);
ProcessTbl lmn_mem_copy_cells(LmnMembrane *dest, LmnMembrane *srcmem);
void lmn_mem_remove_proxies(LmnMembrane *mem);
void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem);
void lmn_mem_remove_temporary_proxies(LmnMembrane *mem);
void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem);

BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec);
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms);
BOOL lmn_mem_is_hlground(Vector *srcvec,
                         Vector *avovec,
                         unsigned long *natoms,
                         ProcessTbl *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_copy_ground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTbl *ret_atommap);
void lmn_mem_copy_hlground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         ProcessTbl *ret_atommap,
                         ProcessTbl *ret_hlinkmap,
                         ProcessTbl *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs);
void lmn_mem_remove_ground(LmnMembrane *mem, Vector *srcvec);
void lmn_mem_remove_hlground(LmnMembrane *mem,
                             Vector *srcvec,
                             ProcessTbl *attr_sym,
                             Vector *attr_data,
                             Vector *attr_data_at);
void lmn_mem_free_ground(Vector *srcvec);
void lmn_mem_free_hlground(Vector *srcvec,
                           ProcessTbl *attr_sym,
                           Vector *attr_data,
                           Vector *attr_data_at);
void lmn_mem_delete_ground(LmnMembrane *mem, Vector *srcvec);
BOOL ground_atoms(Vector *srcvec,
                  Vector *avovec,
                  ProcessTbl *atoms,
                  unsigned long *natoms,
                  ProcessTbl *hlinks,
                  ProcessTbl *attr_functors,
                  Vector *attr_dataAtoms,
                  Vector *attr_dataAtom_attrs);
BOOL ground_atoms_old(Vector *srcvec,
                      Vector *avovec,
                      HashSet **atoms,
                      unsigned long *natoms);

void move_symbol_atom_to_atomlist_head(LmnSAtom a, LmnMembrane *mem);
void move_symbol_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembrane * mem);
void move_symbol_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembrane *mem);

static inline void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem);
static inline void lmn_mem_free_rec(LmnMembrane *mem);
static inline void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem);
static inline AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f);
static inline void lmn_mem_activate_ancestors(LmnMembrane *mem);
static inline BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count);
static inline int  lmn_mem_child_mem_num(LmnMembrane *mem);
static inline void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem);
static inline LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f);
static inline unsigned int lmn_mem_count_children(LmnMembrane *mem);
static inline unsigned int lmn_mem_count_descendants(LmnMembrane *mem);
static inline BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count);
static inline void lmn_mem_remove_data_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
static inline void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
static inline void mem_remove_symbol_atom_with_buddy_data(LmnMembrane *mem, LmnSAtom atom);
static inline void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
static inline void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
static inline void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr);
static inline void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f);
static inline void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset);
static inline void lmn_mem_copy_rules(LmnMembrane *dest, LmnMembrane *src);
static inline void lmn_mem_clearrules(LmnMembrane *src);

/* 膜parentから膜memを取り除く.
 * memのメモリ管理は呼び出し側で行う. */
static inline void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem) {
  LMN_ASSERT(parent);
  if (lmn_mem_child_head(parent) == mem) parent->child_head = lmn_mem_next(mem);
  if (lmn_mem_prev(mem)) mem->prev->next = lmn_mem_next(mem);
  if (lmn_mem_next(mem)) mem->next->prev = lmn_mem_prev(mem);
//  mem->parent = NULL; /* removeproxies のために必要. */
  /* --> removeproxiesでmem->parentを使うようになったためコメントアウト
   * (2011/01/23 meguro) */
}

/* 膜mem以下全ての階層のメモリを破棄する */
static inline void lmn_mem_free_rec(LmnMembrane *mem) {
  lmn_mem_drop(mem);
  lmn_mem_free(mem);
}

/* 膜parentから膜memを取り除き, mem以下の階層全てを解放する. */
static inline void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem) {
  lmn_mem_remove_mem(parent, mem);
  lmn_mem_free_rec(mem);
}

static inline AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f) {
#ifdef TIME_OPT
  if ((f < mem->atomset_size) && mem->atomset[f]) {
    return mem->atomset[f];
  } else {
    return NULL;
  }
#else
  return (AtomListEntry *)hashtbl_get_default(&mem->atomset, f, 0);
#endif
}

/* 自身を含めた全ての先祖膜を起こす */
static inline void lmn_mem_activate_ancestors(LmnMembrane *mem) {
  LmnMembrane *cur;
  for (cur = mem; cur; cur = lmn_mem_parent(cur)) {
    lmn_mem_set_active(mem, TRUE);
  }
}

static inline BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count) {
  unsigned int i;
  LmnMembrane *mp = lmn_mem_child_head(mem);
  for(i = 0; mp && i <= count; mp = lmn_mem_next(mp), i++);
  return i == count;
}


/* 子膜の数を返す */
static inline int lmn_mem_child_mem_num(LmnMembrane *mem) {
  unsigned int i;
  LmnMembrane *mp = lmn_mem_child_head(mem);
  for(i = 0; mp; mp = lmn_mem_next(mp), i++);
  return i;
}

/* add newmem to parent child membranes */
static inline void lmn_mem_add_child_mem(LmnMembrane *parentmem,
                                         LmnMembrane *newmem) {
  newmem->prev   = NULL;
  newmem->next   = lmn_mem_child_head(parentmem);
  newmem->parent = parentmem;
  LMN_ASSERT(parentmem);
  if (lmn_mem_child_head(parentmem)) {
    parentmem->child_head->prev = newmem;
  }
  parentmem->child_head = newmem;
}


/* make atom which functor is f, and push atom into mem */
static inline LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f) {
  LmnSAtom atom = lmn_new_atom(f);
  mem_push_symbol_atom(mem, atom);
  return atom;
}

/* return # of child membranes */
static inline unsigned int lmn_mem_count_children(LmnMembrane *mem) {
  LmnMembrane *c;
  unsigned int n = 0;
  for (c = lmn_mem_child_head(mem); c; c = lmn_mem_next(c)) n++;
  return n;
}

/* return # of descendant membranes */
static inline unsigned int lmn_mem_count_descendants(LmnMembrane *mem) {
  LmnMembrane *c;
  unsigned int n = 0;

  for (c = lmn_mem_child_head(mem); c; c = lmn_mem_next(c)) {
    n += 1 + lmn_mem_count_descendants(c);
  }
  return n;
}


/* return TRUE if # of freelinks in mem is equal to count */
static inline BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count) {
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);
  if (!ent) {
    return count == 0;
  } else {
#ifdef NEW_ATOMLIST
    return count == ent->n;

#else
    LmnSAtom atom;
    unsigned int n;
    /* EFFICIENCY: リストをたどって数を数えているのでO(n)。
       countがそれほど大きくならなければ問題はないが */
    for (atom = atomlist_head(ent), n = 0;
         atom != lmn_atomlist_end(ent) && n <= count;
         atom = LMN_SATOM_GET_NEXT_RAW(atom), n++);
    return count == n;
#endif
  }
}

static inline void lmn_mem_remove_data_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr) {
  lmn_mem_data_atom_dec(mem);
}

static inline void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom) {
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);
#ifdef NEW_ATOMLIST
  {
    AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);
    remove_from_atomlist(atom, ent);
  }
#else
  remove_from_atomlist(atom, NULL);
#endif

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, (LmnWord)NULL);
  } else if (f != LMN_UNIFY_FUNCTOR) {
    lmn_mem_symb_atom_dec(mem);
  }
}

/* 膜memからアトムatomを取り除く.
 * atomの接続先データアトムが存在するならば, そのデータアトムも取り除く.
 * atom自体のメモリ管理は呼び出し側が行う. */
static inline void mem_remove_symbol_atom_with_buddy_data(LmnMembrane *mem, LmnSAtom atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(LMN_SATOM_GET_ATTR(atom, i))) {
      lmn_mem_remove_data_atom(mem, LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    } else if (LMN_ATTR_IS_HL(LMN_SATOM_GET_ATTR(atom, i))) {
      mem_remove_symbol_atom(mem, LMN_SATOM(LMN_SATOM_GET_LINK(atom, i)));
    }
  }
  mem_remove_symbol_atom(mem, atom);
}

static inline void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    lmn_mem_remove_data_atom(mem, atom, attr);
  } else {
    mem_remove_symbol_atom(mem, LMN_SATOM(atom));
  }
}

static inline void move_atom_to_atomlist_tail(LmnSAtom a, LmnMembrane *mem){
  //move_symbol_atom_to_atomlist_tail(LMN_SATOM(a), mem);
  mem_remove_symbol_atom(mem,LMN_SATOM(a));
  mem_push_symbol_atom(mem,LMN_SATOM(a));
}

static inline void move_atom_to_atomlist_head(LmnSAtom a, LmnMembrane *mem){
  //  move_symbol_atom_to_atomlist_head(LMN_SATOM(a), mem); // ueda
  move_symbol_atom_to_atomlist_head(a, mem);
  }

static inline void move_atomlist_to_atomlist_tail(LmnSAtom a, LmnMembrane *mem){
  move_symbol_atomlist_to_atomlist_tail(a, mem);
}

static inline void move_atom_to_atom_tail(LmnSAtom a, LmnSAtom a1, LmnMembrane *mem){
  move_symbol_atom_to_atom_tail(a, a1, mem);
  }

static inline void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr) {
  lmn_mem_remove_atom(mem, atom, attr);
  lmn_free_atom(atom, attr);
}

static inline void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    lmn_mem_data_atom_inc(mem);
  } else { /* symbol atom */
    mem_push_symbol_atom(mem, LMN_SATOM(atom));
  }
}

static inline void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f) {
  mem_remove_symbol_atom(mem, atom);
  LMN_SATOM_SET_FUNCTOR(atom, f);
  mem_push_symbol_atom(mem, atom);
}

/* ルールセットnewを膜memに追加する */
static inline void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset) {
  LMN_ASSERT(ruleset);
  lmn_mem_add_ruleset_sort(&(mem->rulesets), ruleset);
}

static inline void lmn_mem_copy_rules(LmnMembrane *dest, LmnMembrane *src) {
  int i;
  for (i = 0; i< lmn_mem_ruleset_num(src); i++) {
    lmn_mem_add_ruleset(dest, lmn_ruleset_copy(lmn_mem_get_ruleset(src, i)));
  }
}

static inline void lmn_mem_clearrules(LmnMembrane *src) {
  unsigned int i;
  for (i = 0; i < vec_num(&src->rulesets); i++) {
    LmnRuleSet rs = (LmnRuleSet)vec_get(&src->rulesets, i);
    if (lmn_ruleset_is_copy(rs)) {
      lmn_ruleset_copied_free(rs);
    }
  }
  vec_clear(&src->rulesets);
}


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

#ifdef TIME_OPT
typedef int AtomListIter;
#define atomlist_iter_initializer(AS)      (0)
#define atomlist_iter_condition(Mem, Iter) ((Iter) < lmn_mem_max_functor(Mem))
#define atomlist_iter_next(Iter)           ((Iter)++)
#define atomlist_iter_get_entry(Mem, Iter) lmn_mem_get_atomlist(Mem, Iter)
#define atomlist_iter_get_functor(Iter)    (Iter)
#else
typedef HashIterator AtomListIter;
#define atomlist_iter_initalizer(AS)       hashtbl_iterator(&(AS))
#define atomlist_iter_condition(Mem, Iter) (!hashtbliter_isend(&Iter))
#define atomlist_iter_next(Iter)           hashtbliter_next(&Iter)
#define atomlist_iter_get_entry(Mem, Iter) ((AtomListEntry *)hashtbliter_entry(&(Iter))->data)
#define atomlist_iter_get_functor(Iter)    ((LmnFunctor)hashtbliter_entry(&(Iter))->key)
#endif


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

#define EACH_ATOM_THREAD(V, ENT, ID, CODE)				       \
    int id = (ID);							       \
  if ((ENT)) {                                                                 \
    for ((V)  = atomlist_head((ENT));                                          \
         (V) != lmn_atomlist_end((ENT));                                       \
         (V)  = LMN_SATOM_GET_NEXT_RAW((V))) {                                 \
      if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR && id == 0) {     \
        (CODE);								       \
        id=lmn_env.core_num; 				                       \
      }                                                                        \
      id--;								       \ 
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


/* LmnSAtom* lmn_atomlist_end(AtomSetEntry * ent); */


#endif /* LMN_MEMBRANE_H */
