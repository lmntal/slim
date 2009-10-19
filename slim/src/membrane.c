/*
 * membrane.c
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
 *    3. Neither the name of the Ueda Laboratory LMNtal Groupy LMNtal
 *       Group nor the names of its contributors may be used to
 *       endorse or promote products derived from this software
 *       without specific prior written permission.
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
 * $Id: membrane.c,v 1.34 2008/10/16 18:12:27 sasaki Exp $
 */

#include "membrane.h"
#include "atom.h"
#include "rule.h"
#include "dumper.h" /* for debug */
#include "functor.h"
#include "st.h"
#include "mhash.h"
#include "error.h"
#include "util.h"
#include <ctype.h>
#include <limits.h>

#ifdef PROFILE
#include "runtime_status.h"
#endif


BOOL mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);
inline static void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom);
BOOL ground_atoms(Vector *srcvec,
                  Vector *avovec,
                  HashSet **atoms,
                  unsigned long *natoms);
static int mem_cmp(LmnMembrane *mem1, LmnMembrane *mem2);

struct st_hash_type type_memhash = {
  mem_cmp,
  mhash
};

/* ルールセットを膜に追加する。ルールセットは、比較のためにポインタの値
   の昇順に並べるようにする */
void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset)
{
  Vector *v = &mem->rulesets;
  int i, n=vec_num(v);

  if (ruleset==NULL) LMN_ASSERT(FALSE);

  for (i = 0; i<n; i++) {
    LmnRuleSet r = (LmnRuleSet)vec_get(v, i);
    if (r == ruleset) break;
    else if (r < ruleset) continue;
    else if (r > ruleset) {
      int j;
      LmnRuleSet pre = ruleset;
      vec_push(v, 0);
      for (j = i; j<n+1; j++) {
        LmnRuleSet t = (LmnRuleSet)vec_get(v, j);
        vec_set(v, j, (LmnWord)pre);
        pre = t;
      }
      break;
    }
  }
  if (i == n) {
    vec_push(v, (LmnWord)ruleset);
  }
}

inline int lmn_mem_ruleset_num(LmnMembrane *mem)
{
 return vec_num(&mem->rulesets);
}

inline LmnRuleSet lmn_mem_get_ruleset(LmnMembrane *mem, int i)
{
  return (LmnRuleSet)vec_get(&mem->rulesets, i);
}

inline LmnMembrane *lmn_mem_parent(LmnMembrane *mem)
{
  return mem->parent;
}

inline void lmn_mem_set_active(LmnMembrane *mem, BOOL active)
{
  mem->is_activated = active;
}

inline BOOL lmn_mem_is_active(LmnMembrane *mem)
{
  return mem->is_activated;
}

/*----------------------------------------------------------------------
 * Atom Set
 */

/* static BOOL atom_list_is_empty(AtomSetEntry *entry) */
/* { */
/*   return entry->head == LMN_SATOM(entry); */
/* } */

/* アトムリストを空にする. */
#define EMPTY_ATOMLIST(X)                       \
  do {                                          \
    LMN_SATOM_SET_PREV((X), (X));                \
    LMN_SATOM_SET_NEXT((X), (X));                \
  } while (0)

/* 新しいアトムリストを作る */
static AtomListEntry *make_atomlist()
{
  AtomListEntry *as = LMN_MALLOC(struct AtomListEntry);
  hashtbl_init(&as->record, 4);
  EMPTY_ATOMLIST(as);

  return as;
}

/* アトムリストの解放処理 */
static void free_atomlist(AtomListEntry *as)
{
  /* lmn_mem_move_cellsでアトムリスとの再利用を行っていてポインタがNLL
     になる場合があるので、検査を行う必要がある。*/
  if (as) {
    hashtbl_destroy(&as->record);
    LMN_FREE(as);
  }
}

void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom)
{
  AtomListEntry *as;
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);

  as = (AtomListEntry *)hashtbl_get_default(&mem->atomset, f, 0);
  if (!as) { /* 本膜内に初めてアトムatomがPUSHされた場合 */
    as = make_atomlist();
    hashtbl_put(&mem->atomset, (HashKeyType)f, (HashValueType)as);
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, mem);
  }
  else if (f != LMN_UNIFY_FUNCTOR) {
    /* symbol atom except proxy and unify */
    mem->atom_num++;
  }

  LMN_SATOM_SET_NEXT(atom, as);
  LMN_SATOM_SET_PREV(atom, as->tail);
  LMN_SATOM_SET_NEXT(as->tail, atom);
  as->tail = (LmnWord)atom;
}

void lmn_mem_push_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    mem->atom_num++;
  }
  else { /* symbol atom */
    mem_push_symbol_atom(mem, LMN_SATOM(atom));
  }
}

/* append e2 to e1 */
static inline void append_atomlist(AtomListEntry *e1, AtomListEntry *e2)
{
  if (atomlist_head(e2) != lmn_atomlist_end(e2)) {/* true if e2 is not empty */
    LMN_SATOM_SET_NEXT(e1->tail, e2->head);
    LMN_SATOM_SET_PREV(e2->head, e1->tail);
    LMN_SATOM_SET_NEXT(e2->tail, e1);
    e1->tail = e2->tail;
  }
  EMPTY_ATOMLIST(e2);
}


static inline void mem_remove_symbol_atom_with_buddy_data(LmnMembrane *mem, LmnSAtom atom)
{
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      lmn_mem_remove_data_atom(mem, LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    }
  }
  mem_remove_symbol_atom(mem, atom);
}

static inline void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom)
{
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);

  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(atom), LMN_SATOM_GET_PREV(atom));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(atom), LMN_SATOM_GET_NEXT_RAW(atom));

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, (LmnWord)NULL);
  }
  else if (f != LMN_UNIFY_FUNCTOR) {
    mem->atom_num--;
  }
}

inline void lmn_mem_remove_data_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  mem->atom_num--;
}

void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    lmn_mem_remove_data_atom(mem, atom, attr);
  }
  else {
    mem_remove_symbol_atom(mem, LMN_SATOM(atom));
  }
}

inline void lmn_mem_delete_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  lmn_mem_remove_atom(mem, atom, attr);
  lmn_free_atom(atom, attr);
}

/*----------------------------------------------------------------------
 * Membrane
 */

LmnMembrane *lmn_mem_make(void)
{
  LmnMembrane *mem;

  mem = LMN_MALLOC(LmnMembrane);

  memset(mem, 0, sizeof(LmnMembrane)); /* set all data to 0 */
  vec_init(&mem->rulesets, 1);
  hashtbl_init(&mem->atomset, 16); /* EFFICIENCY: 初期サイズはいくつが適当？ */

#ifdef PROFILE
  status_add_membrane_space(sizeof(LmnMembrane));
#endif
  return mem;
}

inline void lmn_mem_set_name(LmnMembrane *mem, lmn_interned_str name)
{
  mem->name = name;
}

void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem)
{
  LMN_ASSERT(parent);
  if (parent->child_head == mem) parent->child_head = mem->next;
  if (mem->prev) mem->prev->next = mem->next;
  if (mem->next) mem->next->prev = mem->prev;
  mem->parent = NULL; /* removeproxies のために必要 */
}

inline void lmn_mem_delete_mem(LmnMembrane *parent, LmnMembrane *mem)
{
  lmn_mem_remove_mem(parent, mem);
  lmn_mem_drop(mem);
  lmn_mem_free(mem);
}

/* 膜内のプロセスと子膜を破棄する */
void lmn_mem_drop(LmnMembrane *mem)
{
  HashIterator iter;
  LmnMembrane *m, *n;

  /* drop and free child mems */
  m = mem->child_head;
  while (m) {
    n = m;
    m = m->next;
    lmn_mem_drop(n);
    lmn_mem_free(n);
  }
  mem->child_head = NULL;

  /* free all atoms */
  for (iter = hashtbl_iterator(&mem->atomset);
       !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    AtomListEntry *ent = (AtomListEntry *)hashtbliter_entry(&iter)->data;
    LmnSAtom a = LMN_SATOM(ent->head), b;
    while (a != lmn_atomlist_end(ent)) {
      b = a;
      a = LMN_SATOM_GET_NEXT_RAW(a);
      free_symbol_atom_with_buddy_data(b);
    }
    EMPTY_ATOMLIST(ent);
  }
  mem->atom_num = 0;
}

/* 膜memの解放を行う */
void lmn_mem_free(LmnMembrane *mem)
{
  HashIterator iter;

  LMN_ASSERT(mem->atom_num == 0);
  /* free all atomlists  */
  for (iter = hashtbl_iterator(&mem->atomset);
       !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    free_atomlist((AtomListEntry*)hashtbliter_entry(&iter)->data);
  }

  hashtbl_destroy(&mem->atomset);
  vec_destroy(&mem->rulesets);

  LMN_FREE(mem);

#ifdef PROFILE
  status_remove_membrane_space(sizeof(LmnMembrane));
#endif

}

/* add newmem to parent child membranes */
void lmn_mem_add_child_mem(LmnMembrane *parentmem, LmnMembrane *newmem)
{
  newmem->prev = NULL;
  newmem->next = parentmem->child_head;
  newmem->parent = parentmem;
  LMN_ASSERT(parentmem);
  if(parentmem->child_head) parentmem->child_head->prev = newmem;
  parentmem->child_head = newmem;
}

/* return NULL when atomlist don't exists. */
AtomListEntry* lmn_mem_get_atomlist(LmnMembrane *mem, LmnFunctor f)
{
  return (AtomListEntry*)hashtbl_get_default(&mem->atomset, f, 0);
}

/* return NULL when atomlist don't exists. */
LmnSAtom atomlist_get_record(AtomListEntry *atomlist, int findatomid)
{
  return (LmnSAtom)hashtbl_get_default(&atomlist->record, findatomid, 0);
}

/* make atom which functor is f, and push atom into mem */
LmnSAtom lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f)
{
  LmnSAtom atom = lmn_new_atom(f);
  mem_push_symbol_atom(mem, atom);
  return atom;
}

BOOL lmn_mem_natoms(LmnMembrane *mem, unsigned int count)
{
  return mem->atom_num == count;
}

BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count)
{
  unsigned int i;
  LmnMembrane *mp = mem->child_head;
  for(i = 0; mp && i <= count; mp = mp->next, i++);
  return i == count;
}

/* return TRUE if # of freelinks in mem is equal to count */
/* EFFICIENCY: リストをたどって数を数えているのでO(n)。
   countがそれほど大きくならなければ問題はないが */
BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count)
{
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
                                                          LMN_IN_PROXY_FUNCTOR,
                                                          0);
  unsigned int n;
  LmnSAtom atom;

  if (!ent) return count == 0;
  for (atom = atomlist_head(ent), n = 0;
       atom != lmn_atomlist_end(ent) && n<=count;
       atom = LMN_SATOM_GET_NEXT_RAW(atom), n++) {}
  return count == n;
}

void lmn_mem_link_data_atoms(LmnMembrane *mem,
                             LmnAtom d0,
                             LmnLinkAttr attr0,
                             LmnAtom d1,
                             LmnLinkAttr attr1)
{
  LmnSAtom ap = lmn_new_atom(LMN_UNIFY_FUNCTOR);

  LMN_SATOM_SET_LINK(ap, 0, d0);
  LMN_SATOM_SET_LINK(ap, 1, d1);
  LMN_SATOM_SET_ATTR(ap, 0, attr0);
  LMN_SATOM_SET_ATTR(ap, 1, attr1);
  mem_push_symbol_atom(mem, ap);
}

/* atom1, atom2をシンボルアトムに限定した unify link */
void lmn_mem_unify_symbol_atom_args(LmnSAtom atom1,
                                    int pos1,
                                    LmnSAtom atom2,
                                    int pos2)
{
  LmnAtom ap1 = LMN_SATOM_GET_LINK(atom1, pos1);
  LmnLinkAttr attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
  LmnAtom ap2 = LMN_SATOM_GET_LINK(atom2, pos2);
  LmnLinkAttr attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);

  LMN_SATOM_SET_LINK(ap2, attr2, ap1);
  LMN_SATOM_SET_ATTR(ap2, attr2, attr1);
  LMN_SATOM_SET_LINK(ap1, attr1, ap2);
  LMN_SATOM_SET_ATTR(ap1, attr1, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void lmn_mem_unify_atom_args(LmnMembrane *mem,
                   LmnSAtom atom1,
                   int pos1,
                   LmnSAtom atom2,
                   int pos2)
{
  LmnAtom ap1 = LMN_SATOM_GET_LINK(atom1, pos1);
  LmnLinkAttr attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
  LmnAtom ap2 = LMN_SATOM_GET_LINK(atom2, pos2);
  LmnLinkAttr attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);

  if(LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
    lmn_mem_link_data_atoms(mem, ap1, attr1, ap2, attr2);
  }
  else if (LMN_ATTR_IS_DATA(attr1)) {
    LMN_SATOM_SET_LINK(ap2, attr2, ap1);
    LMN_SATOM_SET_ATTR(ap2, attr2, attr1);
  }
  else if (LMN_ATTR_IS_DATA(attr2)) {
    LMN_SATOM_SET_LINK(ap1, attr1, ap2);
    LMN_SATOM_SET_ATTR(ap1, attr1, attr2);
  }
  else {
    LMN_SATOM_SET_LINK(ap2, attr2, ap1);
    LMN_SATOM_SET_ATTR(ap2, attr2, attr1);
    LMN_SATOM_SET_LINK(ap1, attr1, ap2);
    LMN_SATOM_SET_ATTR(ap1, attr1, attr2);
  }
}

/* シンボルアトムに限定したnewlink */
void lmn_newlink_in_symbols(LmnSAtom atom0,
                            int pos0,
                            LmnSAtom atom1,
                            int pos1)
{
  LMN_SATOM_SET_LINK(atom0, pos0, atom1);
  LMN_SATOM_SET_LINK(atom1, pos1, atom0);
  LMN_SATOM_SET_ATTR(atom0, pos0, LMN_ATTR_MAKE_LINK(pos1));
  LMN_SATOM_SET_ATTR(atom1, pos1, LMN_ATTR_MAKE_LINK(pos0));
}

/* シンボルアトムatom0と, シンボルorデータアトム atom1 の間にリンクを張る
   このコードが重複して現れたので,関数に分割した */
static inline void newlink_symbol_and_something(LmnSAtom atom0,
                                                int pos,
                                                LmnAtom atom1,
                                                LmnLinkAttr attr)
{
  LMN_SATOM_SET_LINK(atom0, pos, atom1);
  LMN_SATOM_SET_ATTR(atom0, pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    LMN_SATOM_SET_LINK(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), atom0);
    LMN_SATOM_SET_ATTR(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos));
  }
}

void lmn_mem_newlink(LmnMembrane *mem,
                     LmnAtom atom0,
                     LmnLinkAttr attr0,
                     int pos0,
                     LmnAtom atom1,
                     LmnLinkAttr attr1,
                     int pos1)
{
  if (LMN_ATTR_IS_DATA(attr0)) {
    if (LMN_ATTR_IS_DATA(attr1)) { /* both data */
      lmn_mem_link_data_atoms(mem, atom0, attr0, atom1, attr1);
    }
    else { /* atom0 data, atom1 symbol */
      LMN_SATOM_SET_LINK(LMN_SATOM(atom1), pos1, atom0);
      LMN_SATOM_SET_ATTR(LMN_SATOM(atom1), pos1, attr0);
    }
  }
  else if (LMN_ATTR_IS_DATA(attr1)) { /* atom0 symbol, atom1 data */
    LMN_SATOM_SET_LINK(LMN_SATOM(atom0), pos0, atom1);
    LMN_SATOM_SET_ATTR(LMN_SATOM(atom0), pos0, attr1);
  }
  else { /* both symbol */
    lmn_newlink_in_symbols(LMN_SATOM(atom0), pos0, LMN_SATOM(atom1), pos1);
  }
}

void lmn_relink_symbols(LmnSAtom atom0,
                        int pos0,
                        LmnSAtom atom1,
                        int pos1)
{
  newlink_symbol_and_something(LMN_SATOM(atom0),
                               pos0,
                               LMN_SATOM_GET_LINK(LMN_SATOM(atom1), pos1),
                               LMN_SATOM_GET_ATTR(LMN_SATOM(atom1), pos1));
}

void lmn_mem_relink_atom_args(LmnMembrane *mem,
                              LmnAtom atom0,
                              LmnLinkAttr attr0,
                              int pos0,
                              LmnAtom atom1,
                              LmnLinkAttr attr1,
                              int pos1)
{
  /* TODO: relinkではatom0,atom1がデータになることはないはず
           このことを確認する */
  LMN_ASSERT(!LMN_ATTR_IS_DATA(attr0) &&
             !LMN_ATTR_IS_DATA(attr1));

  newlink_symbol_and_something(LMN_SATOM(atom0),
                               pos0,
                               LMN_SATOM_GET_LINK(LMN_SATOM(atom1), pos1),
                               LMN_SATOM_GET_ATTR(LMN_SATOM(atom1), pos1));
}

void lmn_mem_move_cells(LmnMembrane *destmem, LmnMembrane *srcmem)
{
  /* move atoms */
  {
    HashIterator iter;

    for (iter = hashtbl_iterator(&srcmem->atomset);
         !hashtbliter_isend(&iter);
         hashtbliter_next(&iter)) {
      LmnFunctor f = (LmnFunctor)hashtbliter_entry(&iter)->key;
      AtomListEntry *srcent = (AtomListEntry *)hashtbliter_entry(&iter)->data;
      AtomListEntry *destent = lmn_mem_get_atomlist(destmem, f);

      if (LMN_IS_PROXY_FUNCTOR(f)) {
        LmnSAtom a;
        EACH_ATOM(a, srcent, {
          LMN_PROXY_SET_MEM(a, destmem);
        });
      }

      if (destent) { /* 同じファンクタのアトムリストがある場合 */
        append_atomlist(destent, srcent);
      }
      else {
        /* アトムリストを再利用してdestに移す */
        hashtbliter_entry(&iter)->data = 0; /* freeされないように NULL にする */
        hashtbl_clear(&srcent->record);
        hashtbl_put(&destmem->atomset, (HashKeyType)f, (HashValueType)srcent);
      }
    }
    destmem->atom_num += srcmem->atom_num;
    srcmem->atom_num = 0;
  }

  /* move membranes */
  {
    LmnMembrane *m, *next;

    for (m = srcmem->child_head; m; m = next) {
      next = m->next;
      lmn_mem_add_child_mem(destmem, m);
    }
  }
}

#define REMOVE            1
#define STATE(ATOM)        (LMN_SATOM_GET_ATTR((ATOM), 2))
#define SET_STATE(ATOM,S)  (LMN_SATOM_SET_ATTR((ATOM), 2, (S)))

static inline void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f)
{
  mem_remove_symbol_atom(mem, atom);
  LMN_SATOM_SET_FUNCTOR(atom, f);
  mem_push_symbol_atom(mem, atom);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void lmn_mem_remove_proxies(LmnMembrane *mem)
{
  unsigned int i;
  Vector remove_list, change_list;
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_OUT_PROXY_FUNCTOR,
      0);

  vec_init(&remove_list, 16);
  vec_init(&change_list, 16);

  if (ent) {
    LmnSAtom opxy;

    EACH_ATOM(opxy, ent, {
      LmnSAtom a0 = LMN_SATOM(LMN_SATOM_GET_LINK(opxy, 0));
      if (LMN_PROXY_GET_MEM(a0)->parent != mem && /* opxyのリンク先が子膜でない場合 */
          !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(opxy, 1))) {
        LmnSAtom a1 = LMN_SATOM(LMN_SATOM_GET_LINK(opxy, 1));
        LmnFunctor f1 = LMN_SATOM_GET_FUNCTOR(a1);
        if (f1 == LMN_IN_PROXY_FUNCTOR) { /* (1) */
          lmn_mem_unify_atom_args(mem, opxy, 0, a1, 0);
          vec_push(&remove_list, (LmnWord)opxy);
          vec_push(&remove_list, (LmnWord)a1);
        }
        else {
          if (f1 == LMN_OUT_PROXY_FUNCTOR &&
              LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0))->parent != mem) { /* (3) */
            if (!vec_contains(&remove_list, (LmnWord)opxy)) {
              lmn_mem_unify_atom_args(mem, opxy, 0, a1, 0);
              vec_push(&remove_list, (LmnWord)opxy);
              vec_push(&remove_list, (LmnWord)a1);
            }
          } else { /* (2) */
            vec_push(&change_list, (LmnWord)opxy);
          }
        }
      }
    });
  }

  for (i = 0; i < vec_num(&remove_list); i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);


  /* add inside proxy to change list */
  ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_IN_PROXY_FUNCTOR,
      0);
  if (ent) {
    LmnSAtom a;
    /* clear mem attribute */
    EACH_ATOM(a, ent, {
      vec_push(&change_list, (LmnWord)a);
    });
  }

  { /* change to star proxy */
    for (i = 0; i < change_list.num; i++) {
      alter_functor(mem, LMN_SATOM(vec_get(&change_list, i)), LMN_STAR_PROXY_FUNCTOR);
    }
  }
  vec_destroy(&change_list);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void lmn_mem_insert_proxies(LmnMembrane *mem, LmnMembrane *child_mem)
{
  unsigned int i;
  Vector remove_list, change_list;
  LmnSAtom star, oldstar;
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&child_mem->atomset,
      LMN_STAR_PROXY_FUNCTOR,
      0);

  if (!ent) return;

  vec_init(&remove_list, 16);
  vec_init(&change_list, 16); /* inside proxy にするアトム */

  EACH_ATOM(star, ent, {
    oldstar = LMN_SATOM(LMN_SATOM_GET_LINK(star, 0));
    if (LMN_PROXY_GET_MEM(oldstar) == child_mem) { /* (1) */
      if (!vec_contains(&remove_list, (LmnWord)star)) {
        lmn_mem_unify_atom_args(child_mem, star, 1, oldstar, 1);
        vec_push(&remove_list, (LmnWord)star);
        vec_push(&remove_list, (LmnWord)oldstar);
      }
    }
    else {
      vec_push(&change_list, (LmnWord)star);

      if (LMN_PROXY_GET_MEM(oldstar) == mem) { /* (2) */
        alter_functor(mem, oldstar, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(star, 0, oldstar, 0);
      } else { /* (3) */
        LmnSAtom outside = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        LmnSAtom newstar = lmn_mem_newatom(mem, LMN_STAR_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(outside, 1, newstar, 1);
        lmn_mem_relink_atom_args(mem,
                                 LMN_ATOM(newstar),
                                 LMN_ATTR_MAKE_LINK(0),
                                 0,
                                 LMN_ATOM(star),
            LMN_ATTR_MAKE_LINK(0),
            0);
        lmn_newlink_in_symbols(star, 0, outside, 0);
      }
    }
  });

  {
    for (i = 0; i < vec_num(&change_list); i++) {
      alter_functor(child_mem, LMN_SATOM(vec_get(&change_list, i)), LMN_IN_PROXY_FUNCTOR);
    }
  }
  vec_destroy(&change_list);

  for (i = 0; i < vec_num(&remove_list); i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void lmn_mem_remove_temporary_proxies(LmnMembrane *mem)
{
  unsigned int i;
  Vector remove_list;
  LmnSAtom star, outside;
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_STAR_PROXY_FUNCTOR,
      0);

  if (!ent) return;

  vec_init(&remove_list, 16);

  EACH_ATOM(star, ent, {
    outside = LMN_SATOM(LMN_SATOM_GET_LINK(star, 0));
    if (!vec_contains(&remove_list, (LmnWord)star)) {
      lmn_mem_unify_atom_args(mem, star, 1, outside, 1);
      vec_push(&remove_list, (LmnWord)star);
      vec_push(&remove_list, (LmnWord)outside);
    }
  });
  for (i = 0; i < remove_list.num; i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i)));
  }

  vec_destroy(&remove_list);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void lmn_mem_remove_toplevel_proxies(LmnMembrane *mem)
{
  Vector remove_list;
  AtomListEntry *ent;
  LmnSAtom outside;
  unsigned int i;

  ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_OUT_PROXY_FUNCTOR,
      0);
  if (!ent) return;

  vec_init(&remove_list, 16);

  EACH_ATOM(outside, ent, {
    LmnSAtom a0;
    a0 = LMN_SATOM(LMN_SATOM_GET_LINK(outside, 0));
    if (LMN_PROXY_GET_MEM(a0) &&
        LMN_PROXY_GET_MEM(a0)->parent != mem) {
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(outside, 1))) {
        LmnSAtom a1 = LMN_SATOM(LMN_SATOM_GET_LINK(outside, 1));
        if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR) {
          LmnSAtom a10 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, 0));
          if (LMN_PROXY_GET_MEM(a10) &&
              LMN_PROXY_GET_MEM(a10)->parent != mem) {
            if (!vec_contains(&remove_list, (LmnWord)outside)) {
              lmn_mem_unify_atom_args(mem, outside, 0, a1, 0);
              vec_push(&remove_list, (LmnWord)outside);
              vec_push(&remove_list, (LmnWord)a1);
            }
          }
        }
      }
    }
  });

  for (i = 0; i < remove_list.num; i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);
}

LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *src, SimpleHashtbl **ret_copymap)
{
  unsigned int i;
  SimpleHashtbl *copymap;
  LmnMembrane *new_mem = lmn_mem_make();

  copymap = lmn_mem_copy_cells(new_mem, src);
  for (i = 0; i < src->rulesets.num; i++) {
    vec_push(&new_mem->rulesets, vec_get(&src->rulesets, i));
  }
  *ret_copymap = copymap;
  return new_mem;
}

LmnMembrane *lmn_mem_copy(LmnMembrane *src)
{
  SimpleHashtbl *copymap;
  LmnMembrane *copied;

  copied = lmn_mem_copy_with_map(src, &copymap);
  hashtbl_free(copymap);
  return copied;
}

SimpleHashtbl *lmn_mem_copy_cells(LmnMembrane *destmem, LmnMembrane *srcmem)
{
  SimpleHashtbl *atoms;
  LmnMembrane *m;
  HashIterator iter;
  unsigned int i;

  atoms = hashtbl_make(srcmem->atom_num * 2);

  /* copy child mems */
  for (m = srcmem->child_head; m; m = m->next) {
    LmnMembrane *new_mem = lmn_mem_make();
    SimpleHashtbl *child_mem_atoms = lmn_mem_copy_cells(new_mem, m);
    lmn_mem_add_child_mem(destmem, new_mem);

    hashtbl_put(atoms, (HashKeyType)m, (HashValueType)new_mem);
    for (iter = hashtbl_iterator(child_mem_atoms); !hashtbliter_isend(&iter); hashtbliter_next(&iter)) {
      hashtbl_put(atoms, hashtbliter_entry(&iter)->key, hashtbliter_entry(&iter)->data);
    }
    hashtbl_free(child_mem_atoms);
    /* copy name */
    new_mem->name = m->name;
    /* copy rulesets */
    for (i = 0; i < m->rulesets.num; i++) {
      vec_push(&new_mem->rulesets, vec_get(&m->rulesets, i));
    }
  }

  /* copy atoms */
  for (iter = hashtbl_iterator(&srcmem->atomset);
       !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    AtomListEntry *ent = (AtomListEntry *)hashtbliter_entry(&iter)->data;
    LmnSAtom srcatom;

    LMN_ASSERT(ent);

    EACH_ATOM(srcatom, ent, ({
      LmnFunctor f;
      LmnSAtom newatom;
      unsigned int start, end;

      if (hashtbl_contains(atoms, (HashKeyType)srcatom)) continue;
      f = LMN_SATOM_GET_FUNCTOR(srcatom);
      newatom = lmn_mem_newatom(destmem, f);
      hashtbl_put(atoms, (HashKeyType)srcatom, (HashValueType)newatom);
      start = 0;
      end = LMN_SATOM_GET_ARITY(srcatom);

      if (LMN_IS_PROXY_FUNCTOR(f)) {
        start = 1, end = 2;
        LMN_PROXY_SET_MEM(newatom, destmem);
        if (f == LMN_OUT_PROXY_FUNCTOR) {
          LmnSAtom srcinside = LMN_SATOM(LMN_SATOM_GET_LINK(srcatom, 0));
          LmnSAtom newinside = LMN_SATOM(hashtbl_get(atoms, (HashKeyType)srcinside));
          /* 必ず子膜につながっているはず */
          LMN_ASSERT(LMN_SATOM_GET_FUNCTOR(srcinside) == LMN_IN_PROXY_FUNCTOR &&
              LMN_PROXY_GET_MEM(srcinside)->parent == LMN_PROXY_GET_MEM(srcatom));
          lmn_newlink_in_symbols(newatom, 0, newinside, 0);
        }
      }

      /* リンク先と接続 */
      for (i = start; i < end; i++) {
        LmnLinkAttr attr = LMN_SATOM_GET_ATTR(srcatom, i);
        LmnAtom a = LMN_SATOM_GET_LINK(srcatom, i);
        if (LMN_ATTR_IS_DATA(attr)) {
          LmnAtom newargatom = lmn_copy_data_atom(a, attr);
          newlink_symbol_and_something(newatom, i, newargatom, attr);
        } else if(hashtbl_contains(atoms, a)) {
          newlink_symbol_and_something(newatom, i, hashtbl_get(atoms, a), attr);
        }
      }
        }));
  }
  destmem->atom_num = srcmem->atom_num;

  /* copy activated flag */
  destmem->is_activated = srcmem->is_activated; /* MC */

  return atoms;
}

/* to get # of descendant membranes */
inline unsigned int lmn_mem_count_descendants(LmnMembrane *mem) {
  unsigned int n = 0;
  LmnMembrane *m_child;

  for (m_child = mem->child_head; m_child; m_child = m_child->next) {
    n += 1 + lmn_mem_count_descendants(m_child);
  }
  return n;
}

/* to get # of child membranes */
inline unsigned int lmn_mem_count_children(LmnMembrane *mem) {
  unsigned int n = 0;
  LmnMembrane *m_child;

  for (m_child = mem->child_head; m_child; m_child = m_child->next) {
    ++n;
  }
  return n;
}

LinkObj LinkObj_make(LmnAtom ap, LmnLinkAttr pos) {
  LinkObj ret = LMN_MALLOC(struct LinkObj);
  ret->ap = ap;
  ret->pos = pos;
  return ret;
}

/* 膜memのsrcvecを根に持つgroundプロセスを
   コピーする。srcvecはリンクオブジェクトのベクタ。
   ret_dstlovecはコピーされた根のリンクオブジェクトのベクタ。
   ret_atommapはコピー元とコピー先のアトムの対応 */
void lmn_mem_copy_ground(LmnMembrane *mem,
                         Vector *srcvec,
                         Vector **ret_dstlovec,
                         SimpleHashtbl **ret_atommap)
{
  SimpleHashtbl *atommap = hashtbl_make(64);
  Vector *stack = vec_make(16);
  unsigned int i;
  *ret_dstlovec = vec_make(16);

  /* 根をスタックに積む。スタックにはリンクオブジェクトではなくアトムを
     積むため、ここで根の先のアトムをコピーしスタックに積むする必要があ
     る */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    LmnAtom cpatom;

    if (LMN_ATTR_IS_DATA(l->pos)) {
      cpatom = lmn_copy_data_atom(l->ap, l->pos);
      lmn_mem_push_atom(mem, cpatom, l->pos);
    } else { /* symbol atom */
      /* コピー済みでなければコピーする */
      if ((cpatom = hashtbl_get_default(atommap, l->ap, 0)) == 0) {
        cpatom = LMN_ATOM(lmn_copy_satom_with_data(LMN_SATOM(l->ap)));
        hashtbl_put(atommap, (HashKeyType)l->ap, (HashValueType)cpatom);
        mem_push_symbol_atom(mem, LMN_SATOM(cpatom));
      }
      /* 根のリンクのリンクポインタを0に設定する */
      LMN_SATOM_SET_LINK(cpatom, l->pos, 0);
      vec_push(stack, l->ap);
    }
    vec_push(*ret_dstlovec, (LmnWord)LinkObj_make(cpatom, l->pos));
  }

  while (vec_num(stack) > 0) {
    LmnSAtom src_atom = LMN_SATOM(vec_pop(stack));
    LmnSAtom copied = LMN_SATOM(hashtbl_get(atommap, (HashKeyType)src_atom));

    for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
      LmnAtom next_src = LMN_SATOM_GET_LINK(src_atom, i);
      LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

      /* LMN_SATOM_GET_LINK(copied, i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        lmn_mem_push_atom(mem, next_src, next_attr);
      }
      else if (LMN_SATOM_GET_LINK(copied, i) != 0) {
        LmnAtom next_copied = hashtbl_get_default(atommap, next_src, 0);
        if (next_copied == 0) { /* next_srcは未訪問 */
          next_copied = LMN_ATOM(lmn_copy_satom_with_data(LMN_SATOM(next_src)));
          mem_push_symbol_atom(mem, LMN_SATOM(next_copied));
          hashtbl_put(atommap, (HashKeyType)next_src, (HashValueType)next_copied);
          vec_push(stack, next_src);
        }
        LMN_SATOM_SET_LINK(copied, i, next_copied);

      }
    }
  }

  vec_free(stack);
  *ret_atommap = atommap;
}

/* srcvec,dstvecは比較元,比較先grounの明示的自由リンクLinkObj
   ground検査はすんでいるものとする
   srcとdstが同じ形なら真を返す */
BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec)
{
  unsigned int i, j;
  BOOL ret_flag = TRUE;
  Vector stack1, stack2;
  SimpleHashtbl map; /* 比較元→比較先 */
  LinkObj start1, start2;

  hashtbl_init(&map, 256);

  vec_init(&stack1, 16);
  vec_init(&stack2, 16);

  /* startはstackにつまれるので処理中に壊されるためコピー */
  start1 = LinkObj_make(((LinkObj)vec_get(srcvec,0))->ap, ((LinkObj)vec_get(srcvec,0))->pos);
  start2 = LinkObj_make(((LinkObj)vec_get(dstvec,0))->ap, ((LinkObj)vec_get(dstvec,0))->pos);

  if (!LMN_ATTR_IS_DATA(start1->pos) && !LMN_ATTR_IS_DATA(start2->pos)) { /* ともにシンボルアトムの場合 */
    vec_push(&stack1, (LmnWord)start1);
    vec_push(&stack2, (LmnWord)start2);
  }
  else { /* data atom は積まない */
    if(!lmn_data_atom_eq(start1->ap, start1->pos, start2->ap, start2->pos)) ret_flag = FALSE;
    LMN_FREE(start1);
    LMN_FREE(start2);
  }

  while(stack1.num != 0) { /* main loop: start */
    LinkObj l1 = (LinkObj )vec_pop(&stack1);
    LinkObj l2 = (LinkObj )vec_pop(&stack2);
    BOOL contains1 = FALSE;
    BOOL contains2 = FALSE;

    for(i = 0; i < srcvec->num; i++) {
      LinkObj lobj = (LinkObj)vec_get(srcvec, i);
      if (l1->ap == LMN_SATOM_GET_LINK(lobj->ap, lobj->pos)
          && l1->pos == LMN_SATOM_GET_ATTR(lobj->ap, lobj->pos)) {
        contains1 = TRUE;
        break;
      }
    }
    for(j = 0; j < dstvec->num; j++) {
      LinkObj lobj = (LinkObj)vec_get(dstvec, j);
      if (l2->ap == LMN_SATOM_GET_LINK(lobj->ap, lobj->pos)
          && l2->pos == LMN_SATOM_GET_ATTR(lobj->ap, lobj->pos)) {
        contains2 = TRUE;
        break;
      }
    }
    if(i != j){ /* 根の位置が違う */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }
    if(contains1) { /* 根に到達した場合 */
      LMN_FREE(l1); LMN_FREE(l2);
      continue;
    }

    if(l1->pos != l2->pos){ /* 引数検査 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if(LMN_SATOM_GET_FUNCTOR(l1->ap) != LMN_SATOM_GET_FUNCTOR(l2->ap)){ /* ファンクタ検査 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if(!hashtbl_contains(&map, l1->ap)) hashtbl_put(&map, l1->ap, l2->ap); /* 未出 */
    else if(hashtbl_get(&map, l1->ap) != l2->ap) { /* 既出で不一致 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }
    else continue; /* 既出で一致 */

    for(i = 0; i < LMN_SATOM_GET_ARITY(l1->ap); i++) {
      LinkObj n1, n2;
      if(i == l1->pos) continue;
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(l1->ap, i)) && !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(l1->ap, i))) {
        n1 = LinkObj_make(LMN_SATOM_GET_LINK(l1->ap, i), LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(l1->ap, i)));
        n2 = LinkObj_make(LMN_SATOM_GET_LINK(l2->ap, i), LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(l2->ap, i)));
        vec_push(&stack1, (LmnWord)n1);
        vec_push(&stack2, (LmnWord)n2);
      }
      else { /* data atom は積まない */
        if(!lmn_data_atom_eq(LMN_SATOM_GET_LINK(l1->ap, i), LMN_SATOM_GET_ATTR(l1->ap, i),
                             LMN_SATOM_GET_LINK(l2->ap, i), LMN_SATOM_GET_ATTR(l2->ap, i))) {
          LMN_FREE(l1); LMN_FREE(l2);
          ret_flag = FALSE;
          goto CMPGROUND_BREAK;
        }
      }
    }
    LMN_FREE(l1); LMN_FREE(l2);
  } /* main loop: end */

CMPGROUND_BREAK:
  for(i=0; i<vec_num(&stack1); ++i) LMN_FREE((LinkObj)vec_get(&stack1, i));
  for(i=0; i<vec_num(&stack2); ++i) LMN_FREE((LinkObj)vec_get(&stack2, i));
  vec_destroy(&stack1);
  vec_destroy(&stack2);
  hashtbl_destroy(&map);
  
  return ret_flag;
}

/* srcvecのリンクの列が基底項プロセスに到達(avovecのリンクに到達する場
   合は基底項プロセスではない)している場合、真を返し、natomsに基底項プ
   ロセスないのアトムの数を格納する。*/
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms)
{
  BOOL b;
  HashSet *atoms;

  b = ground_atoms(srcvec, avovec, &atoms, natoms);
  if (b) { hashset_free(atoms); }
  return b;
}

/* srcvecから出るリンクのリストが基底項プロセスに到達している場合、
   avovecに基底項プロセスに存在するシンボルアトム、natomsにアトムの数、戻り値に真を返す。
   リストが基底項プロセスに到達していない場合にはatomsはNULLとなり、偽
   を返す。ただし、リンクがavovecに到達している場合には、基底項プロセスとはならない。 */
BOOL ground_atoms(Vector *srcvec,
                  Vector *avovec,
                  HashSet **atoms,
                  unsigned long *natoms)
{
  HashSet *visited = hashset_make(16);
  SimpleHashtbl *guard = NULL;
  HashSet *root = hashset_make(16);
  Vector *stack = vec_make(16);
  unsigned int i;
  unsigned int n; /* 到達したアトムの数 */
  unsigned int n_root_data;   /* 根にあるデータアトムの数 */
  unsigned int n_reach_root;  /* 到達した根の数 */
  BOOL ok;

  n = 0;
  n_reach_root = 0;
  n_root_data = 0;
  ok = TRUE;
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      if (lmn_data_atom_is_ground(l->ap, l->pos)) {
        n_reach_root++;
        n++;
        n_root_data++;
        continue;
      } else {
        ok = FALSE;
        break;
      }
    }

    /* 自己ループしているリンクは無視する */
    if (l->ap == LMN_SATOM_GET_LINK(l->ap, l->pos)) {
      n_reach_root++;
      continue;
    }

    hashset_add(root, (HashKeyType)LMN_SATOM_GET_LINK(l->ap, l->pos));
    if (vec_num(stack) == 0) {
      vec_push(stack, l->ap);
    }
  }

  if (ok) {
    if (avovec != NULL && vec_num(avovec) > 0) {
      guard = hashtbl_make(16);
      for (i = 0; i < vec_num(avovec); i++) {
        LinkObj l = (LinkObj)vec_get(avovec, i);
        if (!LMN_ATTR_IS_DATA(l->pos)) {
          hashtbl_put(guard, l->ap, (HashValueType)l->pos);
        }
      }
    }

    if (n_root_data == 0) {
      while (vec_num(stack) > 0) {
        LmnSAtom src_atom = LMN_SATOM(vec_pop(stack));
        if (LMN_SATOM_IS_PROXY(src_atom)) { ok = FALSE; break; }
        if (hashset_contains(visited, (HashKeyType)src_atom)) continue;
        if (hashset_contains(root, (HashKeyType)src_atom)) {n_reach_root++; continue; }
        hashset_add(visited, (HashKeyType)src_atom);
        n++;
        for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
          LmnAtom next_src = LMN_SATOM_GET_LINK(src_atom, i);
          LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

          if (!LMN_ATTR_IS_DATA(next_attr)) {
            if (guard) {
              int t = hashtbl_get_default(guard, (HashKeyType)next_src, -1);
              if (t >= 0 && t == LMN_ATTR_GET_VALUE(next_attr)) {
                ok = FALSE;
                break;
              }
            }
            vec_push(stack, next_src);
          } else if (lmn_data_atom_is_ground(next_src, next_attr)) {
            n++;
          } else {
            ok = FALSE;
            break;
          }
        }
      }
    } else if (vec_num(stack) >= 2 && n_root_data > 0) {
      ok = FALSE;
    } else if (vec_num(stack) >= 3) {
      ok = FALSE;
    }
  }

  vec_free(stack);
  hashset_free(root);
  if (guard) hashtbl_free(guard);

  if (ok && n_reach_root == vec_num(srcvec)) {
    *natoms = n;
    *atoms = visited;
    return TRUE;
  }
  else {
    hashset_free(visited);
    *atoms = NULL;
    return FALSE;
  }
}

void lmn_mem_remove_ground(LmnMembrane *mem, Vector *srcvec)
{
  HashSet *atoms;
  unsigned long i, t;
  HashSetIterator it;

  ground_atoms(srcvec, NULL, &atoms, &t);

  for (it = hashset_iterator(atoms);
       !hashsetiter_isend(&it);
       hashsetiter_next(&it)) {
    mem_remove_symbol_atom_with_buddy_data(mem, LMN_SATOM(hashsetiter_entry(&it)));
   }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続してい場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos))
      lmn_mem_remove_data_atom(mem, l->ap, l->pos);
  }
  hashset_free(atoms);
}

void lmn_mem_free_ground(Vector *srcvec)
{
  HashSet *atoms;
  unsigned long i, t;
  HashSetIterator it;

  ground_atoms(srcvec, NULL, &atoms, &t);
  for (it = hashset_iterator(atoms);
       !hashsetiter_isend(&it);
       hashsetiter_next(&it)) {
    free_symbol_atom_with_buddy_data(LMN_SATOM(hashsetiter_entry(&it)));
  }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続してい場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) lmn_free_atom(l->ap, l->pos);
  }
  hashset_free(atoms);
}

void lmn_mem_delete_ground(LmnMembrane *mem, Vector *srcvec)
{
  HashSet *atoms;
  unsigned long i, t;
  HashSetIterator it;

  if (!ground_atoms(srcvec, NULL, &atoms, &t)) {
    fprintf(stderr, "remove ground false\n");
  };
  for (it = hashset_iterator(atoms);
       !hashsetiter_isend(&it);
       hashsetiter_next(&it)) {
    mem_remove_symbol_atom_with_buddy_data(mem, LMN_SATOM(hashsetiter_entry(&it)));
    free_symbol_atom_with_buddy_data(LMN_SATOM(hashsetiter_entry(&it)));
   }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続してい場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      lmn_mem_remove_data_atom(mem, l->ap, l->pos);
      lmn_free_atom(l->ap, l->pos);
    }
  }
  hashset_free(atoms);
}


/* 膜の同型性判定 ここから */
/*----------------------------------------------------------------------*/
#define CHECKED_MEM_DEPTH 0

typedef struct AtomVecData {
  LmnFunctor fid;
  Vector *atom_ptrs;
} atomvec_data;

/* 構造体'atomvec_data'に関係するメモリーの領域を解放する */
static void free_atomvec_data(Vector *vec) {
  unsigned int i;

  for (i = 0; i < vec_num(vec); ++i) {
    if (vec_get(vec, i)) {
      vec_free(((atomvec_data *)vec_get(vec, i))->atom_ptrs);
      LMN_FREE((atomvec_data *)vec_get(vec, i));
    }
  }
  vec_free(vec);
}

/* 本膜直下のすべてのアトム(子孫膜内のアトムは含まない)について、まずファンクタごとにグループ分けを行う。
 * グループ分けには構造体'atomvec_data'を用いる。構造体'atomvec_data'は整理されるアトムのファンクタのID(= 0,1,2,...)と、
 * そのファンクタを持つ全アトムのアドレスを管理するベクター'atom_ptrs'とを情報として持つ。
 * 本膜直下のすべてのアトムのアドレスを、それぞれ対応する構造体'atomvec_data'内に整理し、
 * この構造体を本メソッドの戻り値となるベクターの要素として格納していく。
 *
 * 本同型性判定のアルゴリズムでは、「少数派のアトム」からマッチングを開始できるようにすることを
 * 目標としているため、先程構造体を整理したベクターについて、構造体内のアトム数が「多い順」にソートしてやる必要がある。
 * ここで「多い順」とした理由は、後のステップで本ベクターを(結果的に)POPしながらアトムのアドレス情報を取り出すことに
 * なるためである。(少数派のアトムから取り出されることになることに注意せよ) */
static Vector *lmn_mem_mk_matching_vec(LmnMembrane *mem) {
  Vector *vec, *v_tmp;
  HashIterator atom_iter;
  LmnFunctor f;
  LmnSAtom a;
  AtomListEntry *ent;
  unsigned int anum_max; /* 膜内に存在するアトムをファンクタ毎にグループ化した際の、集合の大きさの最大値 */
  unsigned int i, j;

  vec = vec_make(1);
  memset(vec->tbl, 0, sizeof(atomvec_data *) * vec->cap);
  anum_max = 0;

  for (atom_iter = hashtbl_iterator(&mem->atomset);
       !hashtbliter_isend(&atom_iter);
       hashtbliter_next(&atom_iter)) {
    f = hashtbliter_entry(&atom_iter)->key;
    ent = (AtomListEntry *)hashtbliter_entry(&atom_iter)->data;

    atomvec_data *ad = LMN_MALLOC(atomvec_data);
    ad->fid = f;
    ad->atom_ptrs = vec_make(1);
    vec_push(vec, (LmnWord)ad);

    /* 本膜直下のアトムの内、ファンクタがfであるもののアドレスをベクターatom_ptrs内に整理する。
     * 後でソートする関係で、最も多くのアトムのアドレスを管理する構造体(atomvec_data)内のアトム数を求めている。 */
    EACH_ATOM(a, ent, {
      vec_push(((atomvec_data *)vec_peek(vec))->atom_ptrs, (LmnWord)a);
      if (vec_num(((atomvec_data *)vec_peek(vec))->atom_ptrs) > anum_max) {
        anum_max = vec_num(((atomvec_data *)vec_peek(vec))->atom_ptrs);
      }
    });
    /* ファンクタfを持つアトムが本膜内に1つも存在しない場合、このファンクタのために割いたメモリーの領域を解放する。
     * これを怠るとメモリリークが起こるので注意!! */
    if (vec_peek(vec) && vec_is_empty(((atomvec_data *)vec_peek(vec))->atom_ptrs)) {
      vec_free(((atomvec_data *)vec_peek(vec))->atom_ptrs);
      LMN_FREE((atomvec_data *)vec_pop(vec));
    }
  }

  /* sort */
  if (anum_max > 0) {
    v_tmp = vec_make(vec_num(vec));

    for (i = 1; i <= anum_max; ++i) {
      for (j = 0; j < vec_num(vec); ++j) {
        if (vec_num(((atomvec_data *)vec_get(vec, j))->atom_ptrs) == i) {
          vec_push(v_tmp, vec_get(vec, j));
        }
      }
    }
    vec_clear(vec);
    /* 構造体内のアトム数が「多い順」にソート */
    while (!vec_is_empty(v_tmp)) {
      vec_push(vec, vec_pop(v_tmp));
    }
    vec_free(v_tmp);
  }

  return vec;
}

/* ある膜の直下のすべての子膜へのポインタを保持するベクターvecを
 * 子孫膜数の多い順にソートする。ソートされたvec内の要素は後のステップで
 * POPされるため、子孫膜数の少ない子膜から順にマッチングの対象となることになる。 */
static void lmn_mem_mk_sorted_children(Vector *vec) {
  unsigned int num_descendants_max;
  unsigned int i, n;
  Vector *v_mems_tmp;

  assert(vec_num(vec));

  for (i = 0, num_descendants_max = 0; i < vec_num(vec); ++i) {
    if ((n = lmn_mem_count_descendants((LmnMembrane *)vec_get(vec, i)))
          > num_descendants_max) {
      num_descendants_max = n;
    }
  }
  v_mems_tmp = vec_make(vec_num(vec));
  for (n = 0; n <= num_descendants_max; ++n) {
    for (i = 0; i < vec_num(vec); ++i) {
      if (n == lmn_mem_count_descendants((LmnMembrane *)vec_get(vec, i))) {
        vec_push(v_mems_tmp, vec_get(vec, i));
      }
    }
  }
  vec_clear(vec);
  while (!vec_is_empty(v_mems_tmp)) {
    vec_push(vec, vec_pop(v_mems_tmp));
  }
  vec_free(v_mems_tmp);
}

/*
 * lmn_mem_mk_matching_vec/1 の戻り値である2つのVectorの比較を行う
 * 2つの膜内に存在するプロセスの種類および個数が完全に一致するならばTRUEを返す
 */
static BOOL lmn_mem_is_the_same_matching_vec(Vector *vec1, Vector *vec2) {
  BOOL is_the_same_functor;
  unsigned int i, j;

  for (i = 0; i < vec_num(vec1); ++i) {
    is_the_same_functor = FALSE;
    for (j = 0; j < vec_num(vec2); ++j) {
      if (((atomvec_data *)vec_get(vec1, i))->fid
           == ((atomvec_data *)vec_get(vec2, j))->fid) {
        is_the_same_functor = TRUE;
        break;
      }
    }
    if (!is_the_same_functor ||
         (is_the_same_functor &&
               vec_num(((atomvec_data *)vec_get(vec1, i))->atom_ptrs)
            != vec_num(((atomvec_data *)vec_get(vec2, j))->atom_ptrs)
         ))
    {
      return FALSE;
    }
  }

  return TRUE;
}

/* アトムa1、a2を起点とする分子(= あるアトムからリンクによって直接辿ることのできるプロセスの集合)
 * の構造が互いに一致するか否かを判定する。同型性判定を行う上での中心的役割を担っている。
 * 両分子構造が完全に一致する場合は、走査中に通過した全アトム(i.e. 分子内の全アトム)のアドレスが
 * ログ用のベクター(v_log1、v_log2)に保存される。
 *
 * なお、第5引数の 'current_depth' は、同型性判定対象となっている膜の親膜にまで
 * 走査が及ぶのを防ぐためのもの。子膜内のプロセスに走査対象が移る際に current_depth は1増加し、
 * 逆に親膜内のプロセスに移る際は1減少する。同型性判定対象の膜の深さは0になっているため、
 * 0未満の深さに存在するプロセスは走査しないようにする。 */
static BOOL lmn_mem_trace_links(LmnSAtom a1, LmnSAtom a2, Vector *v_log1, Vector *v_log2, int current_depth, BOOL need_to_check_this_membrane_processes) {
  LmnSAtom l1, l2;
  LmnLinkAttr attr1, attr2;
  unsigned int arity;
  unsigned int i;
  BOOL ret_next_step; /* a1, a2と直接接続されているアトムに対して再帰的に本メソッドを適用するために使用 */
  BOOL need_to_check_next_membrane_processes;
  int next_depth;

  /* 本メソッドを再帰的に呼び出していく過程で，a1(,a2)へのリンクを辿ることが親膜から子膜への遷移を意味する場合，
   * a1, a2の所属膜を対象とした同型性判定を行う必要がある */
  if (need_to_check_this_membrane_processes) {
    LmnMembrane *mem1, *mem2;

    /* ここの処理をする際，a1およびa2は共にプロキシアトムのはず */
    assert(LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a1)) && LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a2)));

    mem1 = LMN_PROXY_GET_MEM(a1);
    mem2 = LMN_PROXY_GET_MEM(a2);

    if (!mem_equals(mem1, mem2)) {
      return FALSE;
    }
  }

  vec_push(v_log1, (LmnWord)a1);
  vec_push(v_log2, (LmnWord)a2);

  /* a1、a2のファンクタが一致することを確認(不一致の場合は無条件に偽を返す) */
  if (LMN_SATOM_GET_FUNCTOR(a1) != LMN_SATOM_GET_FUNCTOR(a2)) {
    return FALSE;
  }

  /* 090808同型性判定バグ2対応
   * 親膜内のプロセスのうち、$outにつながっているものについては調べる必要がある */
  if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR && current_depth == CHECKED_MEM_DEPTH) {
    attr1 = LMN_SATOM_GET_ATTR(LMN_SATOM(LMN_SATOM_GET_LINK(a1, 0U)), 1U);
    attr2 = LMN_SATOM_GET_ATTR(LMN_SATOM(LMN_SATOM_GET_LINK(a2, 0U)), 1U);
    if (attr1 != attr2)
       return FALSE;
  }

  arity = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(a1));

  /* a1(= a2) = $in かつa1の所属膜が同型性判定対象膜である場合，a1の第0リンクの接続先である$outは同型性判定対象膜の親膜内の
   * プロセスということになり，トレースの対象に含めてはならない．この基準に基づき，次の変数iの初期値（0 or 1）が決定される． */
  for (i = ((LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR && current_depth == CHECKED_MEM_DEPTH) ? 1U : 0U); i < arity; ++i) {
    attr1 = LMN_SATOM_GET_ATTR(a1, i);
    attr2 = LMN_SATOM_GET_ATTR(a2, i);

    /* アトムa1、a2の第iリンクの接続先が共にシンボル(or プロキシ)アトムのケース
     * (この場合はまず、両アトムの第iリンクが接続先アトムにおける何本目のリンクに相当するのかを確認し、
     * これが一致することを確認する(不一致の場合は偽を返す)。続いて、接続先シンボル(or プロキシ)アトムを
     * 取得し、これがログ上に存在するかどうかをチェック。ログ上にまだ存在しない新規のアトムである場合は、
     * これに対して再帰的に本メソッドを適用し、a1およびa2を起点とする分子全体のマッチングを行う) */
    if (!LMN_ATTR_IS_DATA(attr1) && !LMN_ATTR_IS_DATA(attr2))
    {
      /* {c(X,Y), c(X,Y)} vs. {c(X,Y), c(Y,X)}
       * の例のように、2アトム間のリンクの接続順序が異なっている場合はFALSEを返す */
      if (attr1 != attr2) {
        return FALSE;
      }

      l1 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, i));
      l2 = LMN_SATOM(LMN_SATOM_GET_LINK(a2, i));

      if ((vec_contains(v_log1, (LmnWord)l1) && !vec_contains(v_log2, (LmnWord)l2))
        || (!vec_contains(v_log1, (LmnWord)l1) && vec_contains(v_log2, (LmnWord)l2))) {
         /* 片方の膜においては、これまでのトレースで通過済みのアトムに還ってきた (i.e. 分子内に環状の構造(= 閉路)が存在した)
          * ものの、もう片方の膜では同様の閉路が確認できず、構造の不一致が認められたために偽を返す */
        return FALSE;
      } else if (vec_contains(v_log1, (LmnWord)l1) && vec_contains(v_log2, (LmnWord)l2)) {
         /* 膜1、2内の対応する分子が共に閉路を形成した場合は、第(i+1)リンクの接続先のチェックに移る */
        continue;
      }

      /* a1-->l1 (, a2-->l2) なるリンクのトレースが親膜-->子膜 or 子膜-->親膜なる
       * トレース対象プロセスの所属膜の切り替えを意味する際に，現在トレースしているプロセスの深さを表す変数の値を更新する．
       * (子膜への遷移の際に値が1増加し，逆に親膜内に戻る際は値が1減少する)
       *
       * (注) "a1=$out かつ l1=$in ならば子膜への遷移" としてはいけない．
       *      必ずa1, l1の所属膜が異なっていることも必要条件に含めるようにする．
       *      "{{'+'(L)}}, a(L)."のようなケースでは，間に膜の境界を含まないにも関わらず$inと$outが隣接する．
       */
      if (LMN_SATOM_GET_FUNCTOR(l1) == LMN_IN_PROXY_FUNCTOR && LMN_SATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR
          && LMN_PROXY_GET_MEM(l1) != LMN_PROXY_GET_MEM(a1)) {
        next_depth = current_depth + 1;
      } else if (LMN_SATOM_GET_FUNCTOR(l1) == LMN_OUT_PROXY_FUNCTOR && LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR
          && LMN_PROXY_GET_MEM(l1) != LMN_PROXY_GET_MEM(a1)) {
        next_depth = current_depth - 1;
      } else {
        next_depth = current_depth;
      }

      /* "i = ((LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR && current_depth == CHECKED_MEM_DEPTH) ? 1U : 0U)"
       * なる本forループにおけるiの初期化処理により，直後の探索対象アトム(l1, l2)の所属膜が同型性判定対象外の膜になることはないはず */
      assert(next_depth >= CHECKED_MEM_DEPTH);

      /*
       * a1-->l1 (, a2-->l2) なるリンクが存在し，かつl1の所属膜がa1の所属膜の子膜である場合，
       * 遷移先の階層(i.e. l1およびl2それぞれの所属膜)直下に含まれるプロセスの集合が互いに一致していることを確認する必要が生じる．
       *   例)
       *   mem1 = {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3), {-L3, +R4}, p(L4, R4), {+L4, +R5}, p(L5, R5), {+L5, -R1}.}
       *   mem2 = {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3), {+L3, +R4}, p(L4, R4), {-L4, +R5}, p(L5, R5), {+L5, -R1}.}
       *   のようなmem1とmem2の比較を正しく行う上で必要な措置．
       *
       * (注) "ここをa1の所属膜とl1の所属膜が異なる場合" なる判定を行うと処理が無限ループする可能性があるので注意すること．
       *   無限ループする例)
       *   mem1 = mem2 = {r(lambda(cp(cp(L3,L4,L0),cp(L5,L6,L1),L2),lambda(L7,apply(L3,apply(L5,apply(L4,apply(L6,L7))))))). {top. '+'(L0). '+'(L1). '+'(L2). }.}
       */
      need_to_check_next_membrane_processes = FALSE;
      if (next_depth > current_depth) {
        need_to_check_next_membrane_processes = TRUE;
      }
      ret_next_step = lmn_mem_trace_links(l1, l2, v_log1, v_log2, next_depth, need_to_check_next_membrane_processes);

      if (!ret_next_step) {
        return FALSE;
      }
    }
    /* アトムa1、a2の第iリンクの接続先が共にデータアトムのケース
     * (この場合は接続先データアトムの値を比較し、互いに等しければ次のリンク(i.e. 第(i+1)リンク)の接続先アトム
     * の比較作業に移り、等しくない場合は偽を返す) */
    else if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2))
    {
      if (LMN_SATOM_GET_LINK(a1, i) != LMN_SATOM_GET_LINK(a2, i)) {
        return FALSE;
      }
    }
    /* アトムa1、a2の第iリンクの接続先アトムの種類が互いに一致しないケース
     * (この場合は無条件に偽を返す) */
    else
    {
      return FALSE;
    }
  }

  return TRUE;
}

static BOOL lmn_mem_equals_rec(LmnMembrane *mem1, LmnMembrane *mem2, int current_depth) {
  Vector *atomvec_mem1, *atomvec_mem2; /* atomvec_memX (X = 1,2)は、膜memX 直下のアトムの情報を保持するVector。
                                        * 膜内のアトムをファンクタ毎に整理し、少数派のアトムからマッチングを開始できるようにする目的で使用する。 */
  unsigned int i;

  /* Step1. 両膜の子孫膜の個数、両膜内のアトムの個数、膜名が互いに等しいことを確認 */
  if (lmn_mem_count_descendants(mem1) != lmn_mem_count_descendants(mem2)
      || mem1->atom_num != mem2->atom_num
      || mem1->name != mem2->name) return FALSE;

  {
    atomvec_mem1 = lmn_mem_mk_matching_vec(mem1);
    atomvec_mem2 = lmn_mem_mk_matching_vec(mem2);

    /* Step2. 両膜内のアトムの種類数が互いに等しいことを確認 */
    if (vec_num(atomvec_mem1) != vec_num(atomvec_mem2)) {
      goto STEP_1_FALSE;
    }

    /* Step3. 両膜内に含まれるアトムのファンクタおよび個数が互いに等しいことを確認 */
    if (!lmn_mem_is_the_same_matching_vec(atomvec_mem1, atomvec_mem2)) {
      goto STEP_1_FALSE;
    }

    /* Step3.5. 両膜内に含まれるルールセットが等しいことを確認 */
    if (vec_num(&mem1->rulesets) != vec_num(&mem2->rulesets)) goto STEP_1_FALSE;

    /* ルールセットはポインタの値で昇順にソートされている */
    for (i = 0; i < vec_num(&mem1->rulesets); i++) {
      if (vec_get(&mem1->rulesets, i) != vec_get(&mem2->rulesets, i)) goto STEP_1_FALSE;
    }

  }

  /* この段階で両膜は互いに等しい数の子孫膜を持ち、両膜内のアトムのファンクタの種類
   * およびその個数が完全に一致することが確認されている。
   * (i.e. 結果が「同型でない(偽)」になるならば、本膜におけるリンクの接続関係 or 子孫膜が異なっていることを意味する)
   * 以降、少数派のアトムから順に根に定めていき、アトムを起点とする走査の実行に移っていく。 */
  {
    LmnSAtom a1, a2;
    BOOL matched;
    BOOL has_atoms; /* 本膜が少なくとも1つのシンボル(or プロキシ)アトムを持つことを表すフラグ */
    BOOL has_descendants; /* 本膜が子孫膜を持つことを表すフラグ */
    Vector *v_log1, *v_log2; /* 走査中に通過したアトムのログを管理するVector */
    Vector *v_atoms_not_checked1, *v_atoms_not_checked2; /* 同型性の判定の判定が済んでいないアトムの集合を管理するVector (各アトムへのポインタを保存) */
    Vector *v_mems_children1, *v_mems_children2; /* 本膜直下の子膜を管理するVector (このVectorが空になるまで子膜を起点とする走査が続く) */
    LmnMembrane *m;
    unsigned int n;
    unsigned int length;

    /* init */
    {
      /* mem1, mem2直下の子膜を取得し、その個数が等しいことを確認 */
      {
        length = lmn_mem_count_children(mem1);
        if (length) {
          has_descendants = TRUE;
        } else {
          has_descendants = FALSE;
        }

        if (has_descendants) {
          v_mems_children1 = vec_make(length);
          memset(v_mems_children1->tbl, 0, sizeof(LmnMembrane *) * v_mems_children1->cap);
          v_mems_children2 = vec_make(length);
          memset(v_mems_children2->tbl, 0, sizeof(LmnMembrane *) * v_mems_children2->cap);

          for (m = mem1->child_head; m; m = m->next) {
            vec_push(v_mems_children1, (LmnWord)m);
          }
          for (m = mem2->child_head; m; m = m->next) {
            vec_push(v_mems_children2, (LmnWord)m);
          }
          /* 本膜直下の子膜数が互いに一致しない場合は直ちに偽を返す */
          if (vec_num(v_mems_children1) != vec_num(v_mems_children2)) {
            free_atomvec_data(atomvec_mem1); free_atomvec_data(atomvec_mem2);
            vec_free(v_mems_children1); vec_free(v_mems_children2);

            return FALSE;
          }
        }
      }

      /* 以降、未走査／走査済アトムを管理するvectorの初期化 */
      length = mem1->atom_num;
      assert(length == mem2->atom_num);
      if (length) {
        has_atoms = TRUE;
      } else {
        has_atoms = FALSE;
      }

      if (has_atoms) {
        v_atoms_not_checked1 = vec_make(length);
        memset(v_atoms_not_checked1->tbl, 0, sizeof(LmnSAtom) * v_atoms_not_checked1->cap);
        v_atoms_not_checked2 = vec_make(length);
        memset(v_atoms_not_checked2->tbl, 0, sizeof(LmnSAtom) * v_atoms_not_checked2->cap);

        assert(vec_num(atomvec_mem1) == vec_num(atomvec_mem2));

        /* ベクターatomvec_mem{1,2}には多数派のアトムから順に放りこまれているため、
         * ベクターv_atoms_not_checked{1,2}には多数派のアトムのポインタから順に
         * 放りこまれていくことになる。ゆえに、v_atoms_not_checked{1,2}をPOPしていく
         * ことで少数派のアトムから順に取り出していくことができるようになる。 */
        for (i = 0; i < vec_num(atomvec_mem1); ++i) {
          for (n = 0; n < vec_num(((atomvec_data *)vec_get(atomvec_mem1, i))->atom_ptrs); ++n) {
            vec_push(v_atoms_not_checked1,
                     vec_get(((atomvec_data *)vec_get(atomvec_mem1, i))->atom_ptrs, n));
            vec_push(v_atoms_not_checked2,
                     vec_get(((atomvec_data *)vec_get(atomvec_mem2, i))->atom_ptrs, n));
          }
        }
        v_log1 = vec_make(length);
        v_log2 = vec_make(length);
      }
    }

    /* 以降atomvec_mem1/2は使用しないためこの段階でメモリーを解放しておく */
    free_atomvec_data(atomvec_mem1); free_atomvec_data(atomvec_mem2);

    /* Step4. アトムを起点とする走査 (has_atomsが真の場合のみ行えばよい) */
    if (has_atoms) {
      matched = FALSE;

      assert(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

      while (!vec_is_empty(v_atoms_not_checked1)) {
        /* 膜1内から1つアトムを取り出しこれを根とする。
         * 膜1内のアトムの内、(ファンクタが)少数派のものから順に根に定められていくことに注意せよ。 */
        a1 = LMN_SATOM(vec_pop(v_atoms_not_checked1));

        /*fprintf(stdout, "fid(a1):%u\n", (unsigned int)LMN_SATOM_GET_FUNCTOR(a1));*/

        for (i = vec_num(v_atoms_not_checked2); i > 0 && !matched; --i) {
          a2 = LMN_SATOM(vec_get(v_atoms_not_checked2, i-1)); /* 膜2内から根a1に対応するアトムの候補を取得 (注: ここの実装にvec_popは使用不可!!) */
          vec_clear(v_log1);
          memset(v_log1->tbl, 0, sizeof(LmnSAtom) * v_log1->cap);
          vec_clear(v_log2);
          memset(v_log2->tbl, 0, sizeof(LmnSAtom) * v_log2->cap);

          /* a2が本当にa1に対応するアトムであるか否かを実際にグラフ構造をトレースして確認する。
           * a2とa1とが1:1に対応する場合に限って matched に真が返り、
           * v_log{1,2}内にはa{1,2}を起点とする分子内の全アトムのアドレスが走査ログとして記録される。 */
          matched = lmn_mem_trace_links(a1, a2, v_log1, v_log2, current_depth, FALSE);
          if (matched) {
            /*fprintf(stdout, "fid(a2):%u\n", (unsigned int)LMN_SATOM_GET_FUNCTOR(a2));*/

            /* 両膜内に存在するある分子同士のマッチングに成功した場合にここに入る。
             * 膜2内の未マッチングのアトムを管理していたベクター(v_atoms_not_checked2)
             * から根a1に対応するアトムa2を除去する。 */
            assert(vec_num(v_log1) == vec_num(v_log2));
            for (n = 0; n < vec_num(v_atoms_not_checked2); ++n) {
              if (LMN_SATOM(vec_get(v_atoms_not_checked2, n)) == a2) {
                vec_pop_n(v_atoms_not_checked2, n);
                break;
              }
            }
            assert(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

            /* ログ上に存在するすべてのアトムを、未チェックアトムのリストからPOPする */
            {
              for (n = 0; n < vec_num(v_log1); ++n) {
                for (i = 0; i < vec_num(v_atoms_not_checked1); ++i) {
                  if (LMN_SATOM(vec_get(v_log1, n)) == LMN_SATOM(vec_get(v_atoms_not_checked1, i))) {
                    vec_pop_n(v_atoms_not_checked1, i);
                    break;
                  }
                }
              }
              for (n = 0; n < vec_num(v_log2); ++n) {
                for (i = 0; i < vec_num(v_atoms_not_checked2); ++i) {
                  if (LMN_SATOM(vec_get(v_log2, n)) == LMN_SATOM(vec_get(v_atoms_not_checked2, i))) {
                    vec_pop_n(v_atoms_not_checked2, i);
                    break;
                  }
                }
              }
            }
            assert(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));
          }
        }
        if (!matched) {
          /* 膜1内におけるa1を根とする分子に対応する分子が膜2内に存在しない場合にここに入る (この場合は無条件に偽を返す) */
          vec_free(v_log1); vec_free(v_log2);
          vec_free(v_atoms_not_checked1); vec_free(v_atoms_not_checked2);
          if (has_descendants) {
            vec_free(v_mems_children1); vec_free(v_mems_children2);
          }
          return FALSE;
        }
        /* 膜1直下のアトムの中にまだ未チェックのものが含まれている場合は、
         * マッチングを継続するためにフラグmatchedを偽にセットしておく */
        if (!vec_is_empty(v_atoms_not_checked1)) {
          matched = FALSE;
        }
      }
    }


    /* この段階で本膜内の「アトムを起点とする走査」はすべて完了し、
     * 両膜直下のグラフ構造は完全に一致することが保証されている。
     * ここからは、両膜内に存在するすべての子膜について、その構造が一致するかどうかについて調べていく。
     * 以降、mem1内の子膜を1つ固定し、mem2直下の子膜から対応するものを特定する作業に
     * 移っていくが、結果が偽となるケースにおける処理速度を向上させるため、
     * 子孫膜数が少ない子膜から優先的に固定する方針を取る。 */
    if (has_descendants) { /* 子膜が存在する場合のみ以降の処理を行う */
      LmnMembrane *cm1, *cm2;


      /* 子孫膜数の多い順にv_mems_children1, v_mems_children2をソート */
      {
        assert(vec_num(v_mems_children1) == vec_num(v_mems_children2));
        lmn_mem_mk_sorted_children(v_mems_children1);
        lmn_mem_mk_sorted_children(v_mems_children2);
      }

      /* Step5. 子膜を起点とする走査 */
      matched = FALSE;
      while (!vec_is_empty(v_mems_children1)) {
        cm1 = (LmnMembrane *)vec_pop(v_mems_children1);

        /* fprintf(stderr, "\t-- start to test a descendant membrane --\n\t\t# of descendants of mem(%u): %u\n", (unsigned int)cm1, lmn_mem_count_descendants(cm1)); */

        for (i = vec_num(v_mems_children2); i > 0 && !matched; --i) {
          cm2 = (LmnMembrane *)vec_get(v_mems_children2, i-1);
          matched = lmn_mem_equals_rec(cm1, cm2, current_depth + 1);
          if (matched) {

            /* cm1と同型の膜(=cm2)がv_mems_children2内に見つかった場合にここに入る。
             * v_mems_children2からcm2を取り除く。 */
            for (n = 0; n < vec_num(v_mems_children2); ++n) {
              if (cm2 == (LmnMembrane *)vec_get(v_mems_children2, n)) {
                vec_pop_n(v_mems_children2, n);
                break;
              }
            }
          }
        }
        if (!matched) {

          /* cm1と同型の膜がv_mems_children2内に存在しない場合 */
          if (has_atoms) {
            vec_free(v_log1); vec_free(v_log2);
            vec_free(v_atoms_not_checked1); vec_free(v_atoms_not_checked2);
          }
          vec_free(v_mems_children1); vec_free(v_mems_children2);
          return FALSE;
        }
        if (!vec_is_empty(v_mems_children1)) {
          matched = FALSE;
        }
      }
    }

    /* mem1, mem2内の子孫膜を含むすべてのプロセスの同型性判定に成功 */
    if (has_atoms) {
      vec_free(v_log1); vec_free(v_log2);
      vec_free(v_atoms_not_checked1); vec_free(v_atoms_not_checked2);
    }
    if (has_descendants) {
      vec_free(v_mems_children1); vec_free(v_mems_children2);
    }
  }

  return TRUE;

 STEP_1_FALSE:
  free_atomvec_data(atomvec_mem1);
  free_atomvec_data(atomvec_mem2);
  return FALSE;
}

int mem_cmp(LmnMembrane *mem1, LmnMembrane *mem2)
{
  return !lmn_mem_equals(mem1, mem2);
}

BOOL mem_equals(LmnMembrane *mem1, LmnMembrane *mem2)
{
  return lmn_mem_equals_rec(mem1, mem2, CHECKED_MEM_DEPTH);
}

BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2)
{
  BOOL t;

#ifdef PROFILE
  status_start_mem_equals_calc();
#endif

  t = mem_equals(mem1, mem2);

#ifdef PROFILE
  status_finish_mem_equals_calc();
#endif

  return t;
}
/*----------------------------------------------------------------------*/
/* 膜の同型性判定 ここまで */

