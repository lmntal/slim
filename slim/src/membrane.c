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
#include <ctype.h>

#ifdef PROFILE
#include "runtime_status.h"
#endif


/* ルールセットを膜に追加する */
void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset)
{
  Vector *v = &mem->rulesets;
  int i, n=vec_num(v);

  if (ruleset==NULL) LMN_ASSERT(FALSE);
  /* 重複検査．線形探索をしている */
  for (i=0; i<n&&(vec_get(v, i)!=(LmnWord)ruleset); i++) ;
  if (i==n) {
    vec_push(&mem->rulesets, (LmnWord)ruleset);
  }
}

/*----------------------------------------------------------------------
 * Atom Set
 */

/* static BOOL atom_list_is_empty(AtomSetEntry *entry) */
/* { */
/*   return entry->head == (LmnAtomPtr)entry; */
/* } */

/* アトムリストを空にする. */
#define EMPTY_ATOMLIST(X)                       \
  do {                                          \
    LMN_ATOM_SET_PREV((X), (X));                \
    LMN_ATOM_SET_NEXT((X), (X));                \
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

void mem_push_symbol_atom(LmnMembrane *mem, LmnAtomPtr atom)
{
  AtomListEntry *as;
  LmnFunctor f = LMN_ATOM_GET_FUNCTOR(atom);

  as = (AtomListEntry *)hashtbl_get_default(&mem->atomset, f, 0);
  if (!as) { /* 本膜内に初めてアトムatomがPUSHされた場合 */
    as = make_atomlist();
    hashtbl_put(&mem->atomset, (HashKeyType)f, (HashValueType)as);
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, (LmnWord)mem);
  }
  else if (f != LMN_UNIFY_FUNCTOR) {
    /* symbol atom except proxy and unify */
    mem->atom_num++;
  }

  LMN_ATOM_SET_NEXT(atom, as);
  LMN_ATOM_SET_PREV(atom, as->tail);
  LMN_ATOM_SET_NEXT(as->tail, atom);
  as->tail = (LmnWord)atom;
}

void lmn_mem_push_atom(LmnMembrane *mem, LmnWord atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    mem->atom_num++;
  }
  else { /* symbol atom */
    mem_push_symbol_atom(mem, LMN_ATOM(atom));
  }
}

/* append e2 to e1 */
static inline void append_atomlist(AtomListEntry *e1, AtomListEntry *e2)
{
  if (atomlist_head(e2) != lmn_atomlist_end(e2)) {/* true if e2 is not empty */
    LMN_ATOM_SET_NEXT(e1->tail, e2->head);
    LMN_ATOM_SET_PREV(e2->head, e1->tail);
    LMN_ATOM_SET_NEXT(e2->tail, e1);
    e1->tail = e2->tail;
  }
  EMPTY_ATOMLIST(e2);
}

static inline void mem_remove_symbol_atom(LmnMembrane *mem, LmnAtomPtr atom)
{
  LmnFunctor f = LMN_ATOM_GET_FUNCTOR(atom);

  LMN_ATOM_SET_PREV(LMN_ATOM_GET_NEXT(atom), LMN_ATOM_GET_PREV(atom));
  LMN_ATOM_SET_NEXT(LMN_ATOM_GET_PREV(atom), LMN_ATOM_GET_NEXT(atom));

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom,(LmnWord)NULL);
  }
  else if (f != LMN_UNIFY_FUNCTOR) {
    mem->atom_num--;
  }
}

void lmn_mem_remove_atom(LmnMembrane *mem, LmnWord atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    mem->atom_num--;
  }
  else {
    mem_remove_symbol_atom(mem, LMN_ATOM(atom));
  }
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
    LmnAtomPtr a = LMN_ATOM(ent->head), b;
    while (a != lmn_atomlist_end(ent)) {
      b = a;
      a = LMN_ATOM_GET_NEXT(a);
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
LmnAtomPtr* atomlist_get_record(AtomListEntry *atomlist, int findatomid)
{
  return (LmnAtomPtr*)hashtbl_get_default(&atomlist->record, findatomid, 0);
}

/* make atom which functor is f, and push atom into mem */
LmnAtomPtr lmn_mem_newatom(LmnMembrane *mem, LmnFunctor f)
{
  LmnAtomPtr atom = lmn_new_atom(f);
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
  LmnAtomPtr atom;

  if (!ent) return count == 0;
  for (atom = atomlist_head(ent), n = 0;
       atom != lmn_atomlist_end(ent) && n<=count;
       atom = LMN_ATOM_GET_NEXT(atom), n++) {}
  return count == n;
}

void lmn_mem_link_data_atoms(LmnMembrane *mem,
                             LmnWord d0,
                             LmnLinkAttr attr0,
                             LmnWord d1,
                             LmnLinkAttr attr1)
{
  LmnAtomPtr ap = lmn_new_atom(LMN_UNIFY_FUNCTOR);

  LMN_ATOM_SET_LINK(ap, 0, d0);
  LMN_ATOM_SET_LINK(ap, 1, d1);
  LMN_ATOM_SET_ATTR(ap, 0, attr0);
  LMN_ATOM_SET_ATTR(ap, 1, attr1);
  mem_push_symbol_atom(mem, ap);
}

/* atom1, atom2をシンボルアトムに限定した unify link */
void lmn_mem_unify_symbol_atom_args(LmnAtomPtr atom1,
                                    int pos1,
                                    LmnAtomPtr atom2,
                                    int pos2)
{
  LmnWord ap1 = LMN_ATOM_GET_LINK(atom1, pos1);
  LmnLinkAttr attr1 = LMN_ATOM_GET_ATTR(atom1, pos1);
  LmnWord ap2 = LMN_ATOM_GET_LINK(atom2, pos2);
  LmnLinkAttr attr2 = LMN_ATOM_GET_ATTR(atom2, pos2);

  LMN_ATOM_SET_LINK(ap2, attr2, (LmnWord)ap1);
  LMN_ATOM_SET_ATTR(ap2, attr2, attr1);
  LMN_ATOM_SET_LINK(ap1, attr1, (LmnWord)ap2);
  LMN_ATOM_SET_ATTR(ap1, attr1, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void lmn_mem_unify_atom_args(LmnMembrane *mem,
                   LmnAtomPtr atom1,
                   int pos1,
                   LmnAtomPtr atom2,
                   int pos2)
{
  LmnWord ap1 = LMN_ATOM_GET_LINK(atom1, pos1);
  LmnLinkAttr attr1 = LMN_ATOM_GET_ATTR(atom1, pos1);
  LmnWord ap2 = LMN_ATOM_GET_LINK(atom2, pos2);
  LmnLinkAttr attr2 = LMN_ATOM_GET_ATTR(atom2, pos2);

  if(LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
    lmn_mem_link_data_atoms(mem, ap1, attr1, ap2, attr2);
  }
  else if (LMN_ATTR_IS_DATA(attr1)) {
    LMN_ATOM_SET_LINK(ap2, attr2, (LmnWord)ap1);
    LMN_ATOM_SET_ATTR(ap2, attr2, attr1);
  }
  else if (LMN_ATTR_IS_DATA(attr2)) {
    LMN_ATOM_SET_LINK(ap1, attr1, (LmnWord)ap2);
    LMN_ATOM_SET_ATTR(ap1, attr1, attr2);
  }
  else {
    LMN_ATOM_SET_LINK(ap2, attr2, (LmnWord)ap1);
    LMN_ATOM_SET_ATTR(ap2, attr2, attr1);
    LMN_ATOM_SET_LINK(ap1, attr1, (LmnWord)ap2);
    LMN_ATOM_SET_ATTR(ap1, attr1, attr2);
  }
}

/* シンボルアトムに限定したnewlink */
void lmn_newlink_in_symbols(LmnAtomPtr atom0,
                            int pos0,
                            LmnAtomPtr atom1,
                            int pos1)
{
  LMN_ATOM_SET_LINK(atom0, pos0, (LmnWord)atom1);
  LMN_ATOM_SET_LINK(atom1, pos1, (LmnWord)atom0);
  LMN_ATOM_SET_ATTR(atom0, pos0, pos1);
  LMN_ATOM_SET_ATTR(atom1, pos1, pos0);
}

/* シンボルアトムatom0と, シンボルorデータアトム atom1 の間にリンクを張る
   このコードが重複して現れたので,関数に分割した */
static inline void newlink_symbol_and_something(LmnAtomPtr atom0,
                                                int pos,
                                                LmnWord atom1,
                                                LmnLinkAttr attr)
{
  LMN_ATOM_SET_LINK(atom0, pos, atom1);
  LMN_ATOM_SET_ATTR(atom0, pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    LMN_ATOM_SET_LINK(LMN_ATOM(atom1), LMN_ATTR_GET_VALUE(attr), (LmnWord)atom0);
    LMN_ATOM_SET_ATTR(LMN_ATOM(atom1), LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos));
  }
}

void lmn_mem_newlink(LmnMembrane *mem,
                     LmnWord atom0,
                     LmnLinkAttr attr0,
                     int pos0,
                     LmnWord atom1,
                     LmnLinkAttr attr1,
                     int pos1)
{
  if (LMN_ATTR_IS_DATA(attr0)) {
    if (LMN_ATTR_IS_DATA(attr1)) { /* both data */
      LMN_ASSERT(pos0 == 0 && pos1 == 0);
      lmn_mem_link_data_atoms(mem, atom0, pos0, atom1, pos1);
    }
    else { /* atom0 data, atom1 symbol */
      LMN_ATOM_SET_LINK(LMN_ATOM(atom1), pos1, atom0);
      LMN_ATOM_SET_ATTR(LMN_ATOM(atom1), pos1, attr0);
    }
  }
  else if (LMN_ATTR_IS_DATA(attr1)) { /* atom0 symbol, atom1 data */
    LMN_ATOM_SET_LINK(LMN_ATOM(atom0), pos0, atom1);
    LMN_ATOM_SET_ATTR(LMN_ATOM(atom0), pos0, attr1);
  }
  else { /* both symbol */
    lmn_newlink_in_symbols(LMN_ATOM(atom0), pos0, LMN_ATOM(atom1), pos1);
  }
}

void lmn_mem_relink_atom_args(LmnMembrane *mem,
                              LmnWord atom0,
                              LmnLinkAttr attr0,
                              int pos0,
                              LmnWord atom1,
                              LmnLinkAttr attr1,
                              int pos1)
{
  /* TODO: relinkではatom0,atom1がデータになることはないはず
           このことを確認する */
  LMN_ASSERT(!LMN_ATTR_IS_DATA(attr0) &&
             !LMN_ATTR_IS_DATA(attr1));

  newlink_symbol_and_something(LMN_ATOM(atom0),
                               pos0,
                               LMN_ATOM_GET_LINK(LMN_ATOM(atom1), pos1),
                               LMN_ATOM_GET_ATTR(LMN_ATOM(atom1), pos1));
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
        LmnAtomPtr a;
        for (a = atomlist_head(srcent);
             a != lmn_atomlist_end(srcent);
             a = LMN_ATOM_GET_NEXT(a)) {
          LMN_PROXY_SET_MEM(a, (LmnWord)destmem);
        }
      }

      if (destent) { /* 同じファンクタのアトムリストがある場合 */
        append_atomlist(destent, srcent);
      }
      else {
        /* アトムリストを再利用してdestに移す */
        hashtbliter_entry(&iter)->data = 0; /* freeされないように NULL にする */
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
#define STATE(ATOM)        (LMN_ATOM_GET_ATTR((ATOM), 2))
#define SET_STATE(ATOM,S)  (LMN_ATOM_SET_ATTR((ATOM), 2, (S)))

static inline void alter_functor(LmnMembrane *mem, LmnAtomPtr atom, LmnFunctor f)
{
  mem_remove_symbol_atom(mem, atom);
  LMN_ATOM_SET_FUNCTOR(atom, f);
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
    LmnAtomPtr opxy;

    for (opxy = atomlist_head(ent);
        opxy != lmn_atomlist_end(ent);
        opxy = LMN_ATOM_GET_NEXT(opxy)) {
      LmnAtomPtr a0 = LMN_ATOM(LMN_ATOM_GET_LINK(opxy, 0));
      if (LMN_PROXY_GET_MEM(a0)->parent != mem && /* opxyのリンク先が子膜でない場合 */
          !LMN_ATTR_IS_DATA(LMN_ATOM_GET_ATTR(opxy, 1))) {
        LmnAtomPtr a1 = LMN_ATOM(LMN_ATOM_GET_LINK(opxy, 1));
        LmnFunctor f1 = LMN_ATOM_GET_FUNCTOR(a1);
        if (f1 == LMN_IN_PROXY_FUNCTOR) { /* (1) */
          lmn_mem_unify_atom_args(mem, opxy, 0, a1, 0);
          vec_push(&remove_list, (LmnWord)opxy);
          vec_push(&remove_list, (LmnWord)a1);
        }
        else {
          if (f1 == LMN_OUT_PROXY_FUNCTOR &&
              LMN_PROXY_GET_MEM(LMN_ATOM_GET_LINK(a1, 0))->parent != mem) { /* (3) */
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
    }
  }

  for (i = 0; i < vec_num(&remove_list); i++) {
    mem_remove_symbol_atom(mem, LMN_ATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_ATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);


  /* add inside proxy to change list */
  ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_IN_PROXY_FUNCTOR,
      0);
  if (ent) {
    LmnAtomPtr a;
    /* clear mem attribute */
    for (a = atomlist_head(ent);
        a != lmn_atomlist_end(ent);
        a = LMN_ATOM_GET_NEXT(a)) {
      vec_push(&change_list, (LmnWord)a);
    }
  }

  { /* change to star proxy */
    for (i = 0; i < change_list.num; i++) {
      alter_functor(mem, LMN_ATOM(vec_get(&change_list, i)), LMN_STAR_PROXY_FUNCTOR);
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
  LmnAtomPtr star, oldstar;
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&child_mem->atomset,
      LMN_STAR_PROXY_FUNCTOR,
      0);

  if (!ent) return;

  vec_init(&remove_list, 16);
  vec_init(&change_list, 16); /* inside proxy にするアトム */

  for (star = atomlist_head(ent);
      star != lmn_atomlist_end(ent);
      star = LMN_ATOM_GET_NEXT(star)) {
    oldstar = LMN_ATOM(LMN_ATOM_GET_LINK(star, 0));
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
        LmnAtomPtr outside = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        LmnAtomPtr newstar = lmn_mem_newatom(mem, LMN_STAR_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(outside, 1, newstar, 1);
        lmn_mem_relink_atom_args(mem,
            (LmnWord)newstar,
            LMN_ATTR_MAKE_LINK(0),
            0,
            (LmnWord)star,
            LMN_ATTR_MAKE_LINK(0),
            0);
        lmn_newlink_in_symbols(star, 0, outside, 0);
      }
    }
  }

  {
    for (i = 0; i < vec_num(&change_list); i++) {
      alter_functor(child_mem, LMN_ATOM(vec_get(&change_list, i)), LMN_IN_PROXY_FUNCTOR);
    }
  }
  vec_destroy(&change_list);

  for (i = 0; i < vec_num(&remove_list); i++) {
    mem_remove_symbol_atom(mem, LMN_ATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_ATOM(vec_get(&remove_list, i)));
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
  LmnAtomPtr star, outside;
  AtomListEntry *ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_STAR_PROXY_FUNCTOR,
      0);

  if (!ent) return;

  vec_init(&remove_list, 16);

  for (star = atomlist_head(ent);
      star != lmn_atomlist_end(ent);
      star = LMN_ATOM_GET_NEXT(star)) {
    outside = LMN_ATOM(LMN_ATOM_GET_LINK(star, 0));
    if (!vec_contains(&remove_list, (LmnWord)star)) {
      lmn_mem_unify_atom_args(mem, star, 1, outside, 1);
      vec_push(&remove_list, (LmnWord)star);
      vec_push(&remove_list, (LmnWord)outside);
    }
  }
  for (i = 0; i < remove_list.num; i++) {
    mem_remove_symbol_atom(mem, LMN_ATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_ATOM(vec_get(&remove_list, i)));
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
  LmnAtomPtr outside;
  unsigned int i;

  ent = (AtomListEntry *)hashtbl_get_default(&mem->atomset,
      LMN_OUT_PROXY_FUNCTOR,
      0);
  if (!ent) return;

  vec_init(&remove_list, 16);

  for (outside = atomlist_head(ent);
      outside != lmn_atomlist_end(ent);
      outside = LMN_ATOM_GET_NEXT(outside)) {
    LmnAtomPtr a0;
    a0 = LMN_ATOM(LMN_ATOM_GET_LINK(outside, 0));
    if (LMN_PROXY_GET_MEM(a0) &&
        LMN_PROXY_GET_MEM(a0)->parent != mem) {
      if (!LMN_ATTR_IS_DATA(LMN_ATOM_GET_ATTR(outside, 1))) {
        LmnAtomPtr a1 = LMN_ATOM(LMN_ATOM_GET_LINK(outside, 1));
        if (LMN_ATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR) {
          LmnAtomPtr a10 = LMN_ATOM(LMN_ATOM_GET_LINK(a1, 0));
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
  }

  for (i = 0; i < remove_list.num; i++) {
    mem_remove_symbol_atom(mem, LMN_ATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_ATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);
}

/* mem -> atoms のhashtblはold atom -> newatomのhashtbl一つに統合できる.
   子膜を必ず先にコピーする。しかし,これだと、一つのhashtblが大きくなってしまう
   問題がある
   071204 oldatom->newatomのhashtbl一つに統合した
*/
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
    LmnAtomPtr srcatom;

    LMN_ASSERT(ent);

    for (srcatom = atomlist_head(ent);
         srcatom != lmn_atomlist_end(ent);
         srcatom = LMN_ATOM_GET_NEXT(srcatom)) {
      LmnFunctor f;
      LmnAtomPtr newatom;
      unsigned int start, end;

      if (hashtbl_contains(atoms, (HashKeyType)srcatom)) continue;
      f = LMN_ATOM_GET_FUNCTOR(srcatom);
      newatom = lmn_mem_newatom(destmem, f);
      hashtbl_put(atoms, (HashKeyType)srcatom, (HashValueType)newatom);
      start = 0;
      end = LMN_ATOM_GET_ARITY(srcatom);

      if (LMN_IS_PROXY_FUNCTOR(f)) {
        start = 1, end = 2;
        LMN_PROXY_SET_MEM(newatom, (LmnWord)destmem);
        if (f == LMN_OUT_PROXY_FUNCTOR) {
          LmnAtomPtr srcinside = LMN_ATOM(LMN_ATOM_GET_LINK(srcatom, 0));
          LmnAtomPtr newinside = LMN_ATOM(hashtbl_get(atoms, (HashKeyType)srcinside));
          /* 必ず子膜につながっているはず */
          LMN_ASSERT(LMN_ATOM_GET_FUNCTOR(srcinside) == LMN_IN_PROXY_FUNCTOR &&
              LMN_PROXY_GET_MEM(srcinside)->parent == LMN_PROXY_GET_MEM(srcatom));
          lmn_newlink_in_symbols(newatom, 0, newinside, 0);
        }
      }

      /* リンク先と接続 */
      for (i = start; i < end; i++) {
        LmnLinkAttr attr = LMN_ATOM_GET_ATTR(srcatom, i);
        if(!(LMN_INT_ATTR == attr) && hashtbl_contains(atoms, LMN_ATOM_GET_LINK(srcatom, i))) {
          newlink_symbol_and_something(newatom, i, hashtbl_get(atoms, LMN_ATOM_GET_LINK(srcatom, i)), attr);
        } else { /* LMN_INT_ATTR == attr || !hashtbl_contains() */
          LmnWord newargatom = lmn_copy_atom(LMN_ATOM_GET_LINK(srcatom, i), attr);
          newlink_symbol_and_something(newatom, i, newargatom, attr);
          if (!(LMN_INT_ATTR == attr)) {
            /* TODO: データアトムの扱いが怪しい？ */
            /*hashtbl_put(atoms, LMN_ATOM_GET_LINK(srcatom, i), newargatom);*/
          }
        }
      }
    }
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

/* 膜の同型性判定 ここから */
/*----------------------------------------------------------------------*/
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
  LmnAtomPtr a;
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
    for (a = atomlist_head(ent);
         a != lmn_atomlist_end(ent);
         a = LMN_ATOM_GET_NEXT(a)) {
      vec_push(((atomvec_data *)vec_peek(vec))->atom_ptrs, (LmnWord)a);
      if (vec_num(((atomvec_data *)vec_peek(vec))->atom_ptrs) > anum_max) {
        anum_max = vec_num(((atomvec_data *)vec_peek(vec))->atom_ptrs);
      }
    }
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
      free_atomvec_data(vec1); free_atomvec_data(vec2);
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
static BOOL lmn_mem_trace_links(LmnAtomPtr a1, LmnAtomPtr a2, Vector *v_log1, Vector *v_log2, int current_depth, BOOL need_to_check_this_membrane_processes) {
  LmnAtomPtr l1, l2;
  LmnLinkAttr attr1, attr2;
  unsigned int arity;
  unsigned int i;
  BOOL ret_next_step; /* a1, a2と直接接続されているアトムに対して再帰的に本メソッドを適用するために使用 */
  BOOL need_to_check_next_membrane_processes;
  int next_depth;

  /* 階層の切り替えを検知した場合，両膜内のプロセスの種類および個数が一致するかの確認を行う */
  if (need_to_check_this_membrane_processes) {
    Vector *atomvec_mem1, *atomvec_mem2;

    /* ここの処理をする際，a1およびa2は共にプロキシアトムのはず */
    assert(LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a1)) && LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a2)));

    atomvec_mem1 = lmn_mem_mk_matching_vec(LMN_PROXY_GET_MEM(a1));
    atomvec_mem2 = lmn_mem_mk_matching_vec(LMN_PROXY_GET_MEM(a2));

    /* 両膜内のアトムの種類数が互いに等しいことを確認 */
    if (vec_num(atomvec_mem1) != vec_num(atomvec_mem2)) {
      free_atomvec_data(atomvec_mem1); free_atomvec_data(atomvec_mem2);
      return FALSE;
    }
    /* 両膜内に含まれるアトムのファンクタおよび個数が互いに等しいことを確認 */
    if (!lmn_mem_is_the_same_matching_vec(atomvec_mem1, atomvec_mem2)) {
      return FALSE;
    }

    free_atomvec_data(atomvec_mem1); free_atomvec_data(atomvec_mem2);
  }

  vec_push(v_log1, (LmnWord)a1);
  vec_push(v_log2, (LmnWord)a2);

  /* a1、a2のファンクタが一致することを確認(不一致の場合は無条件に偽を返す) */
  if (LMN_ATOM_GET_FUNCTOR(a1) != LMN_ATOM_GET_FUNCTOR(a2)) {
    return FALSE;
  }

  arity = LMN_ATOM_GET_ARITY(a1);
  if (LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a1))) {
    /* プロキシアトムの第3引数は所属膜へのポインタなのでその分を除外 */
    --arity;
  }
  for (i = 0; i < arity; ++i) {
    attr1 = LMN_ATOM_GET_ATTR(a1, i);
    attr2 = LMN_ATOM_GET_ATTR(a2, i);

    /* アトムa1、a2の第iリンクの接続先が共にシンボル(or プロキシ)アトムのケース
     * (この場合はまず、両アトムの第iリンクが接続先アトムにおける何本目のリンクに相当するのかを確認し、
     * これが一致することを確認する(不一致の場合は偽を返す)。続いて、接続先シンボル(or プロキシ)アトムを
     * 取得し、これがログ上に存在するかどうかをチェック。ログ上にまだ存在しない新規のアトムである場合は、
     * これに対して再帰的に本メソッドを適用し、a1およびa2を起点とする分子全体のマッチングを行う) */
    if (!LMN_ATTR_IS_DATA(attr1) && !LMN_ATTR_IS_DATA(attr2))
    {
      if (attr1 != attr2) {
          /* {c(X,Y), c(X,Y)} vs. {c(X,Y), c(Y,X)}
            * の例のように、2アトム間のリンクの接続順序が異なっている場合はFALSEを返す */
        return FALSE;
      }
      l1 = (LmnAtomPtr)LMN_ATOM_GET_LINK(a1, i);
      l2 = (LmnAtomPtr)LMN_ATOM_GET_LINK(a2, i);

      if ((vec_contains(v_log1, (LmnWord)l1) && !vec_contains(v_log2, (LmnWord)l2))
        || (!vec_contains(v_log1, (LmnWord)l1) && vec_contains(v_log2, (LmnWord)l2))) {
         /* 片方の膜においては、これまでのトレースで通過済みのアトムに還ってきた (i.e. 分子内に環状の構造(= 閉路)が存在した)
          * ものの、もう片方の膜では同様の閉路が確認できず、構造の不一致が認められたために偽を返す */
        return FALSE;
      } else if (vec_contains(v_log1, (LmnWord)l1) && vec_contains(v_log2, (LmnWord)l2)) {
         /* 膜1、2内の対応する分子が共に閉路を形成した場合は、第(i+1)リンクの接続先のチェックに移る */
        continue;
      }

      if (LMN_ATOM_GET_FUNCTOR(l1) == LMN_IN_PROXY_FUNCTOR) {
        next_depth = current_depth + 1;
      } else if (LMN_ATOM_GET_FUNCTOR(l1) == LMN_OUT_PROXY_FUNCTOR) {
        next_depth = current_depth - 1;
      } else {
        next_depth = current_depth;
      }
      if (next_depth < 0) {
        /* 次の走査対象アトムがそもそもの同型性判定対象となっていた膜の親膜以上の階層に
         * 所属しているため、これを無視して次のアトムを走査対象に選び直す */
        continue;
      }

      /*
       * a1-->l1 (, a2-->l2) なる遷移の際に階層が切り換わる場合，遷移先の階層(i.e. l1およびl2それぞれの所属膜)直下に
       * 含まれるプロセスの集合が互いに一致していることを確認する必要が生じる．
       *   例)
       *   mem1 = {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3), {-L3, +R4}, p(L4, R4), {+L4, +R5}, p(L5, R5), {+L5, -R1}.}
       *   mem2 = {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3), {+L3, +R4}, p(L4, R4), {-L4, +R5}, p(L5, R5), {+L5, -R1}.}
       *   のようなmem1とmem2の比較を正しく行う上で必要な措置．
       */
      need_to_check_next_membrane_processes = FALSE;
      if (LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a1)) && LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(l1))) {
        assert(LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a2)) && LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(l2)));
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
      if (LMN_ATOM_GET_LINK(a1, i) != LMN_ATOM_GET_LINK(a2, i)) {
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
      free_atomvec_data(atomvec_mem1); free_atomvec_data(atomvec_mem2);
      return FALSE;
    }
    /* Step3. 両膜内に含まれるアトムのファンクタおよび個数が互いに等しいことを確認 */
    if (!lmn_mem_is_the_same_matching_vec(atomvec_mem1, atomvec_mem2)) {
      return FALSE;
    }
  }
  /* この段階で両膜は互いに等しい数の子孫膜を持ち、両膜内のアトムのファンクタの種類
   * およびその個数が完全に一致することが確認されている。
   * (i.e. 結果が「同型でない(偽)」になるならば、本膜におけるリンクの接続関係 or 子孫膜が異なっていることを意味する)
   * 以降、少数派のアトムから順に根に定めていき、アトムを起点とする走査の実行に移っていく。 */
  {
    LmnAtomPtr a1, a2;
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
        memset(v_atoms_not_checked1->tbl, 0, sizeof(LmnAtomPtr) * v_atoms_not_checked1->cap);
        v_atoms_not_checked2 = vec_make(length);
        memset(v_atoms_not_checked2->tbl, 0, sizeof(LmnAtomPtr) * v_atoms_not_checked2->cap);

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
        a1 = (LmnAtomPtr)vec_pop(v_atoms_not_checked1);

        /* fprintf(stdout, "fid(a1):%u\n", (unsigned int)LMN_ATOM_GET_FUNCTOR(a1)); */

        for (i = vec_num(v_atoms_not_checked2); i > 0 && !matched; --i) {
          a2 = (LmnAtomPtr)vec_get(v_atoms_not_checked2, i-1); /* 膜2内から根a1に対応するアトムの候補を取得 (注: ここの実装にvec_popは使用不可!!) */
          vec_clear(v_log1);
          memset(v_log1->tbl, 0, sizeof(LmnAtomPtr) * v_log1->cap);
          vec_clear(v_log2);
          memset(v_log2->tbl, 0, sizeof(LmnAtomPtr) * v_log2->cap);

          /* a2が本当にa1に対応するアトムであるか否かを実際にグラフ構造をトレースして確認する。
           * a2とa1とが1:1に対応する場合に限って matched に真が返り、
           * v_log{1,2}内にはa{1,2}を起点とする分子内の全アトムのアドレスが走査ログとして記録される。 */
          matched = lmn_mem_trace_links(a1, a2, v_log1, v_log2, current_depth, FALSE);
          if (matched) {
           /* fprintf(stdout, "fid(a2):%u\n", (unsigned int)LMN_ATOM_GET_FUNCTOR(a2)); */

            /* 両膜内に存在するある分子同士のマッチングに成功した場合にここに入る。
             * 膜2内の未マッチングのアトムを管理していたベクター(v_atoms_not_checked2)
             * から根a1に対応するアトムa2を除去する。 */
            assert(vec_num(v_log1) == vec_num(v_log2));
            for (n = 0; n < vec_num(v_atoms_not_checked2); ++n) {
              if ((LmnAtomPtr)vec_get(v_atoms_not_checked2, n) == a2) {
                vec_pop_n(v_atoms_not_checked2, n);
                break;
              }
            }
            assert(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

            /* ログ上に存在するすべてのアトムを、未チェックアトムのリストからPOPする */
            {
              for (n = 0; n < vec_num(v_log1); ++n) {
                for (i = 0; i < vec_num(v_atoms_not_checked1); ++i) {
                  if ((LmnAtomPtr)vec_get(v_log1, n) == (LmnAtomPtr)vec_get(v_atoms_not_checked1, i)) {
                    vec_pop_n(v_atoms_not_checked1, i);
                    break;
                    }
                  }
                }
              for (n = 0; n < vec_num(v_log2); ++n) {
                for (i = 0; i < vec_num(v_atoms_not_checked2); ++i) {
                  if ((LmnAtomPtr)vec_get(v_log2, n) == (LmnAtomPtr)vec_get(v_atoms_not_checked2, i)) {
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
}

BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2) {
  return lmn_mem_equals_rec(mem1, mem2, 0);
}
/*----------------------------------------------------------------------*/
/* 膜の同型性判定 ここまで */
