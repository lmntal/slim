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
#include "visitlog.h"
#include "lmntal_thread.h"
#include <ctype.h>
#include <limits.h>

#ifdef PROFILE
#  include "runtime_status.h"
#endif


BOOL mem_equals(LmnMembrane *mem1, LmnMembrane *mem2);
static void lmn_mem_copy_cells_sub(LmnMembrane *destmem,
                                   LmnMembrane *srcmem,
                                   ProcessTbl atoms);

/* ルールセットadd_rsをルールセット配列src_vへ追加する.
 * グラフ同型性判定処理などのために整数IDの昇順を維持するよう追加する. */
void lmn_mem_add_ruleset_sort(Vector *src_v, LmnRuleSet add_rs)
{
  int i, j, n;
  LmnRulesetId add_id;
  add_id = lmn_ruleset_get_id(add_rs);
  n = vec_num(src_v);
  for (i = 0; i < n; i++) {
    LmnRuleSet rs_i;
    LmnRulesetId dst_id;

    rs_i   = (LmnRuleSet)vec_get(src_v, i);
    dst_id = lmn_ruleset_get_id(rs_i);

    if (dst_id == add_id && !lmn_ruleset_has_uniqrule(add_rs)) {
      /* 同じ階層にuniqでない同一のルールセットが既に存在するならば追加する必要はない */
      break;
    } else if (dst_id >= add_id) {
      LmnRuleSet prev = add_rs;
      vec_push(src_v, 0);

      for (j = i; j < (n + 1); j++) {
        LmnRuleSet tmp = (LmnRuleSet)vec_get(src_v, j);
        vec_set(src_v, j, (LmnWord)prev);
        prev = tmp;
      }
      break;
    }
  }
  if (i == n) {
    vec_push(src_v, (LmnWord)add_rs);
  }
}

/* ルールセットnewを膜memに追加する */
void lmn_mem_add_ruleset(LmnMembrane *mem, LmnRuleSet ruleset)
{
  LMN_ASSERT(ruleset);
  lmn_mem_add_ruleset_sort(&(mem->rulesets), ruleset);
}

void lmn_mem_copy_rules(LmnMembrane *dest, LmnMembrane *src)
{
  int i;

  for (i = 0; i< lmn_mem_ruleset_num(src); i++) {
    lmn_mem_add_ruleset(dest, lmn_ruleset_copy(lmn_mem_get_ruleset(src, i)));
  }
}

void lmn_mem_clearrules(LmnMembrane *src)
{
  unsigned int i;
  for (i = 0; i < vec_num(&src->rulesets); i++) {
    LmnRuleSet rs = (LmnRuleSet)vec_get(&src->rulesets, i);
    if (lmn_ruleset_is_copy(rs)) {
      lmn_ruleset_copied_free(rs);
    }
  }
  vec_clear(&src->rulesets);
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
AtomListEntry *make_atomlist()
{
  AtomListEntry *as = LMN_MALLOC(struct AtomListEntry);
  hashtbl_init(&as->record, 4);
  EMPTY_ATOMLIST(as);

  return as;
}

/* アトムリストの解放処理 */
void free_atomlist(AtomListEntry *as)
{
  /* lmn_mem_move_cellsでアトムリストの再利用を行っていてポインタがNULL
     になる場合があるので、検査を行う必要がある。*/
  if (as) {
    hashtbl_destroy(&as->record);
    LMN_FREE(as);
  }
}

inline void mem_push_symbol_atom(LmnMembrane *mem, LmnSAtom atom)
{
  AtomListEntry *as;
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);

  if (LMN_SATOM_ID(atom) == 0) { /* 膜にpushしたならばidを割り当てる */
    LMN_SATOM_SET_ID(atom, env_gen_next_id());
  }

  as = lmn_mem_get_atomlist(mem, f);
  if (!as) { /* 本膜内に初めてアトムatomがPUSHされた場合 */
#ifdef TIME_OPT
    LMN_ASSERT(mem->atomset); /* interpreter側で値がオーバーフローすると発生するので, ここで止める */
    if (mem->max_functor < f + 1) {
      mem->max_functor = f + 1;
      while (mem->atomset_size - 1 < mem->max_functor) {
        int org_size = mem->atomset_size;
        mem->atomset_size *= 2;
        LMN_ASSERT(mem->atomset_size > 0);
        mem->atomset = LMN_REALLOC(struct AtomListEntry*, mem->atomset, mem->atomset_size);
        memset(mem->atomset+org_size, 0, (mem->atomset_size - org_size)*sizeof(struct AtomListEntry*));
      }
    }
    as = mem->atomset[f] = make_atomlist();
#else
    as = make_atomlist();
    hashtbl_put(&mem->atomset, (HashKeyType)f, (HashValueType)as);
#endif
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, mem);
  }
  else if (LMN_FUNC_IS_HL(f)) {
    LMN_HL_MEM(lmn_hyperlink_at_to_hl(atom)) = mem;
    mem->atom_num++;
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
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
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


inline void mem_remove_symbol_atom_with_buddy_data(LmnMembrane *mem, LmnSAtom atom)
{
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(LMN_SATOM_GET_ATTR(atom, i))) {
      lmn_mem_remove_data_atom(mem, LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    }
  }
  mem_remove_symbol_atom(mem, atom);
}

void mem_remove_symbol_atom(LmnMembrane *mem, LmnSAtom atom)
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

void lmn_mem_remove_data_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  mem->atom_num--;
}

void lmn_mem_remove_atom(LmnMembrane *mem, LmnAtom atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
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
  mem->parent       =  NULL;
  mem->child_head   =  NULL;
  mem->prev         =  NULL;
  mem->next         =  NULL;
  mem->max_functor  =  0U;
  mem->atomset_size =  32;
  mem->is_activated =  FALSE;
  mem->atom_num     =  0UL;
  mem->name         =  ANONYMOUS;
  mem->id           =  0UL;
#ifdef TIME_OPT
  mem->atomset      =  LMN_CALLOC(struct AtomListEntry *, mem->atomset_size);
#else
  hashtbl_init(&mem->atomset, mem->atomset_size);
#endif
  vec_init(&mem->rulesets, 1);
  lmn_mem_set_id(mem, env_gen_next_id());

  return mem;
}

inline void lmn_mem_set_name(LmnMembrane *mem, lmn_interned_str name)
{
  mem->name = name;
}

int lmn_mem_max_functor(LmnMembrane *mem)
{
  return mem->max_functor;
}

void lmn_mem_remove_mem(LmnMembrane *parent, LmnMembrane *mem)
{
  LMN_ASSERT(parent);
  if (parent->child_head == mem) parent->child_head = mem->next;
  if (mem->prev) mem->prev->next = mem->next;
  if (mem->next) mem->next->prev = mem->prev;
  //2011/01/23 removeproxiesでmem->parentを使うようになったのでコメントアウトしました。
  //mem->parent = NULL; /* removeproxies のために必要 */
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
  AtomListEntry *ent;
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

  EACH_ATOMLIST(mem, ent, ({
    LmnSAtom a, b;
    a = atomlist_head(ent);
    b = a;
    if (LMN_IS_HL(a)) continue; // hyperlinkはbuddy symbol atomと一緒に削除されるため
    while (a != lmn_atomlist_end(ent)) {
      b = a;
      a = LMN_SATOM_GET_NEXT_RAW(a);
      free_symbol_atom_with_buddy_data(b);
    }
    EMPTY_ATOMLIST(ent);
  }));

  mem->atom_num = 0;
}

/* ルールセット配列rulesetsを解放する */
void lmn_mem_rulesets_destroy(Vector *rulesets)
{
  unsigned int i, n = vec_num(rulesets);

  for (i = 0; i < n; i++) {
    LmnRuleSet rs = (LmnRuleSet)vec_get(rulesets, i);

    if (lmn_ruleset_is_copy(rs)) {
      lmn_ruleset_copied_free(rs);
    }
  }
  vec_destroy(rulesets);
}


/* 膜memの解放を行う */
void lmn_mem_free(LmnMembrane *mem)
{
  AtomListEntry *ent;

  /* free all atomlists  */
  EACH_ATOMLIST(mem, ent, ({
    free_atomlist(ent);
  }));

  lmn_mem_rulesets_destroy(&mem->rulesets);
#ifdef TIME_OPT
  env_return_id(lmn_mem_id(mem));
  LMN_FREE(mem->atomset);
#else
  hashtbl_destroy(&mem->atomset);
#endif
  LMN_FREE(mem);
}

unsigned long lmn_mem_space(LmnMembrane *mem)
{
  AtomListEntry *ent;
  unsigned long ret;
  unsigned int i;

  ret = 0;
  ret += sizeof(struct LmnMembrane);
#ifdef TIME_OPT
  ret += sizeof(struct AtomListEntry*) * mem->atomset_size;
#else
  ret += internal_hashtbl_space_inner(&mem->atomset);
#endif
  /* atomset */
  EACH_ATOMLIST(mem, ent, ({
    LmnSAtom atom;
    ret += sizeof(struct AtomListEntry);
    ret += internal_hashtbl_space_inner(&ent->record);
    EACH_ATOM(atom, ent, ({
      ret += sizeof(LmnWord) * LMN_SATOM_WORDS(LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom)));
    }));
  }));

  /* ruleset */
  ret += vec_space_inner(&mem->rulesets);
  for (i = 0; i < vec_num(&mem->rulesets); i++) {
    LmnRuleSet rs = (LmnRuleSet)vec_get(&mem->rulesets, i);
    if (lmn_ruleset_is_copy(rs)) {
      ret += lmn_ruleset_space(rs);
    }
  }

  return ret;
}

unsigned long lmn_mem_root_space(LmnMembrane *src)
{
  unsigned long ret;
  LmnMembrane *ptr;

  ret = lmn_mem_space(src);
  for (ptr = src->child_head; ptr != NULL; ptr = ptr->next) {
    ret += lmn_mem_root_space(ptr);
  }

  return ret;
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
#ifdef TIME_OPT
  return ((f < mem->atomset_size) && mem->atomset[f]) ?  mem->atomset[f]: 0;
#else
  return (AtomListEntry *)hashtbl_get_default(&mem->atomset, f, 0);
#endif
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

/* 膜内の（子膜以下は含まない）アトムの数を返す */
unsigned long lmn_mem_atom_num(LmnMembrane *mem)
{
  return mem->atom_num;
}

BOOL lmn_mem_nmems(LmnMembrane *mem, unsigned int count)
{
  unsigned int i;
  LmnMembrane *mp = mem->child_head;
  for(i = 0; mp && i <= count; mp = mp->next, i++);
  return i == count;
}

/* 子膜の数を返す */
int lmn_mem_child_mem_num(LmnMembrane *mem)
{
  unsigned int i;
  LmnMembrane *mp = mem->child_head;
  for(i = 0; mp; mp = mp->next, i++);
  return i;
}

/* return TRUE if # of freelinks in mem is equal to count */
/* EFFICIENCY: リストをたどって数を数えているのでO(n)。
   countがそれほど大きくならなければ問題はないが */
BOOL lmn_mem_nfreelinks(LmnMembrane *mem, unsigned int count)
{
  AtomListEntry *ent;
  unsigned int n;
  LmnSAtom atom;

  ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);

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

void lmn_newlink_with_ex(LmnMembrane *mem,
                         LmnSAtom atom0,
                         LmnLinkAttr attr0,
                         int pos0,
                         LmnSAtom atom1,
                         LmnLinkAttr attr1,
                         int pos1)
{
  /* both symbol */
  LMN_SATOM_SET_LINK(atom0, pos0, atom1);
  LMN_SATOM_SET_LINK(atom1, pos1, atom0);
  if (LMN_ATTR_IS_EX(attr0)) {
    if (LMN_ATTR_IS_EX(attr1)) { /* 0, 1 are ex */
//      LMN_SATOM_SET_ATTR(atom0, pos0, attr1);
//      LMN_SATOM_SET_ATTR(atom1, pos1, attr0);
      /* 現状では、hyperlinkアトム同士が接続されると消去される */
      lmn_mem_delete_atom(mem, LMN_ATOM(atom0), attr0);
      lmn_mem_delete_atom(mem, LMN_ATOM(atom1), attr1);
    } else { /* 0 is ex */
      LMN_SATOM_SET_ATTR(atom0, pos0, LMN_ATTR_MAKE_LINK(pos1));
      LMN_SATOM_SET_ATTR(atom1, pos1, attr0);
    }
  } else { /* 1 is ex */
    LMN_SATOM_SET_ATTR(atom0, pos0, attr1);
    LMN_SATOM_SET_ATTR(atom1, pos1, LMN_ATTR_MAKE_LINK(pos0));
  }

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
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr0)) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) { /* both data */
      lmn_mem_link_data_atoms(mem, atom0, attr0, atom1, attr1);
    }
    else { /* atom0 data, atom1 symbol */
      LMN_SATOM_SET_LINK(LMN_SATOM(atom1), pos1, atom0);
      LMN_SATOM_SET_ATTR(LMN_SATOM(atom1), pos1, attr0);
    }
  }
  else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) { /* atom0 symbol, atom1 data */
    LMN_SATOM_SET_LINK(LMN_SATOM(atom0), pos0, atom1);
    LMN_SATOM_SET_ATTR(LMN_SATOM(atom0), pos0, attr1);
  }
  else { /* both symbol */
    if (!LMN_ATTR_IS_EX(attr0) && !LMN_ATTR_IS_EX(attr1))
      lmn_newlink_in_symbols(LMN_SATOM(atom0), pos0, LMN_SATOM(atom1), pos1);
    else
      lmn_newlink_with_ex(mem, LMN_SATOM(atom0), attr0, pos0, LMN_SATOM(atom1), attr1, pos1);
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
  AtomListEntry *srcent;
  LmnMembrane *m, *next;

  /* move atoms */
  EACH_ATOMLIST(srcmem, srcent, ({
    LmnSAtom a, next;
    for (a  = atomlist_head((srcent));
         a != lmn_atomlist_end((srcent));
         a  = next) {
      next = LMN_SATOM_GET_NEXT_RAW(a);
      if (LMN_SATOM_GET_FUNCTOR((a)) != LMN_RESUME_FUNCTOR) {
        int i, arity;

        mem_remove_symbol_atom(srcmem, a);
        mem_push_symbol_atom(destmem, a);
        arity = LMN_SATOM_GET_LINK_NUM(a);
        for (i = 0; i < arity; i++) {
          if (LMN_ATTR_IS_DATA_WITHOUT_EX(LMN_SATOM_GET_ATTR(a, i))) {
            lmn_mem_push_atom(destmem,
                              LMN_SATOM_GET_LINK(a, i),
                              LMN_SATOM_GET_ATTR(a, i));
          }
        }
      }
    }
  }));

  /* move membranes */
  for (m = srcmem->child_head; m; m = next) {
    next = m->next;
    lmn_mem_remove_mem(srcmem, m);
    lmn_mem_add_child_mem(destmem, m);
  }
}

#define REMOVE            1
#define STATE(ATOM)        (LMN_SATOM_GET_ATTR((ATOM), 2))
#define SET_STATE(ATOM,S)  (LMN_SATOM_SET_ATTR((ATOM), 2, (S)))

void alter_functor(LmnMembrane *mem, LmnSAtom atom, LmnFunctor f)
{
  mem_remove_symbol_atom(mem, atom);
  LMN_SATOM_SET_FUNCTOR(atom, f);
  mem_push_symbol_atom(mem, atom);
}

/* cf. Java処理系
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 *
 * 2011/01/23  処理を変更 meguro
 */
void lmn_mem_remove_proxies(LmnMembrane *mem)
{
  Vector remove_list_p, remove_list_m, change_list;
  AtomListEntry *ent;
  unsigned int i;

  ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);

  vec_init(&remove_list_p, 16);//parent用
  vec_init(&remove_list_m, 16);//mem用
  vec_init(&change_list, 16);

  if (ent) {
    LmnSAtom ipxy;

    EACH_ATOM(ipxy, ent, ({
      LmnSAtom a0, a1;
      LmnFunctor f0, f1;
      
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(ipxy, 1))) {      
        a0 = LMN_SATOM(LMN_SATOM_GET_LINK(ipxy, 1));
        f0 = LMN_SATOM_GET_FUNCTOR(a0);

        if (f0 == LMN_STAR_PROXY_FUNCTOR) {
          /* -$*-$in- → ----- */
          lmn_mem_unify_atom_args(mem, a0, 0, ipxy, 0);
          vec_push(&remove_list_m, (LmnWord)a0);
          vec_push(&remove_list_m, (LmnWord)ipxy);
        }
        else {
          /* -$in- → -$*- */
          vec_push(&change_list, (LmnWord)ipxy);
        }
      }else{
        /* -$in- → -$*- */
        vec_push(&change_list, (LmnWord)ipxy);
      }

      a1 = LMN_SATOM(LMN_SATOM_GET_LINK(ipxy, 0));
      f1 = LMN_SATOM_GET_FUNCTOR(a1);

      if (f1 == LMN_OUT_PROXY_FUNCTOR) {
        if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(a1, 1))) {
          LmnSAtom a2;
          LmnFunctor f2;

          a2 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, 1));
          f2 = LMN_SATOM_GET_FUNCTOR(a2);

          if(f2 == LMN_STAR_PROXY_FUNCTOR) {
            lmn_mem_unify_atom_args(mem->parent, a1, 0, a2, 0);
            vec_push(&remove_list_p, (LmnWord)a1);
            vec_push(&remove_list_p, (LmnWord)a2);
          }
          else {
            vec_push(&change_list, (LmnWord)a1);
          }
        }
      }
    }));
  }

  for (i = 0; i < vec_num(&remove_list_p); i++) {
    mem_remove_symbol_atom(mem->parent, LMN_SATOM(vec_get(&remove_list_p, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list_p, i)));
  }
  vec_destroy(&remove_list_p);

  for (i = 0; i < vec_num(&remove_list_m); i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list_m, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list_m, i)));
  }
  vec_destroy(&remove_list_m);

  /* change to star proxy */
  for (i = 0; i < change_list.num; i++) {
    alter_functor(LMN_PROXY_GET_MEM(LMN_SATOM(vec_get(&change_list, i))),
                  LMN_SATOM(vec_get(&change_list, i)), LMN_STAR_PROXY_FUNCTOR);
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
  AtomListEntry *ent = lmn_mem_get_atomlist(child_mem, LMN_STAR_PROXY_FUNCTOR);

  if (!ent) return;

  vec_init(&remove_list, 16);
  vec_init(&change_list, 16); /* inside proxy にするアトム */

  EACH_ATOM(star, ent, ({
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
  }));

  for (i = 0; i < vec_num(&change_list); i++) {
    alter_functor(child_mem, LMN_SATOM(vec_get(&change_list, i)), LMN_IN_PROXY_FUNCTOR);
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
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, LMN_STAR_PROXY_FUNCTOR);

  if (!ent) return;

  vec_init(&remove_list, 16);

  EACH_ATOM(star, ent, ({
    outside = LMN_SATOM(LMN_SATOM_GET_LINK(star, 0));
    if (!vec_contains(&remove_list, (LmnWord)star)) {
      lmn_mem_unify_atom_args(mem, star, 1, outside, 1);
      vec_push(&remove_list, (LmnWord)star);
      vec_push(&remove_list, (LmnWord)outside);
    }
  }));

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

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent) return;

  vec_init(&remove_list, 16);

  EACH_ATOM(outside, ent, ({
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
  }));

  for (i = 0; i < remove_list.num; i++) {
    mem_remove_symbol_atom(mem, LMN_SATOM(vec_get(&remove_list, i)));
    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i)));
  }
  vec_destroy(&remove_list);
}

LmnMembrane *lmn_mem_copy_with_map(LmnMembrane *src, ProcessTbl *ret_copymap)
{
  unsigned int i;
  ProcessTbl copymap;
  LmnMembrane *new_mem;

  env_reset_proc_ids();

  new_mem = lmn_mem_make();
  copymap = lmn_mem_copy_cells(new_mem, src);
  for (i = 0; i < src->rulesets.num; i++) {
    vec_push(&new_mem->rulesets,
        (LmnWord)lmn_ruleset_copy((LmnRuleSet)vec_get(&src->rulesets, i)));
  }
  *ret_copymap = copymap;

  return new_mem;
}

LmnMembrane *lmn_mem_copy(LmnMembrane *src)
{
  ProcessTbl copymap;
  LmnMembrane *copied;

  copied = lmn_mem_copy_with_map(src, &copymap);
  proc_tbl_free(copymap);
  return copied;
}

ProcessTbl lmn_mem_copy_cells(LmnMembrane *destmem, LmnMembrane *srcmem)
{
  ProcessTbl atoms;

  atoms = proc_tbl_make_with_size(64);

  lmn_mem_copy_cells_sub(destmem, srcmem, atoms);

  return atoms;
}

/* srcmemの実データ構造をdestmemへコピー生成する. atomsは訪問済みの管理に用いる */
static void lmn_mem_copy_cells_sub(LmnMembrane *destmem, LmnMembrane *srcmem, ProcessTbl atoms)
{
  LmnMembrane *m;
  AtomListEntry *ent;
  unsigned int i;

  /* copy child mems */
  for (m = srcmem->child_head; m; m = m->next) {
    LmnMembrane *new_mem = lmn_mem_make();
    lmn_mem_copy_cells_sub(new_mem, m, atoms);
    lmn_mem_add_child_mem(destmem, new_mem);

    proc_tbl_put_mem(atoms, m, (LmnWord)new_mem);
    /* copy name */
    new_mem->name = m->name;
    /* copy rulesets */
    for (i = 0; i < m->rulesets.num; i++) {
      vec_push(&new_mem->rulesets,
          (LmnWord)lmn_ruleset_copy((LmnRuleSet)vec_get(&m->rulesets, i)));
    }
  }

  /* copy atoms */
  EACH_ATOMLIST(srcmem, ent, ({
    LmnSAtom srcatom;
    LmnWord t = 0;

    EACH_ATOM(srcatom, ent, ({
      LmnSAtom newatom;
      unsigned int start, end;
      LmnFunctor f;

      LMN_ASSERT(LMN_SATOM_ID(srcatom) > 0);

      if (proc_tbl_get_by_atom(atoms, srcatom, NULL)) continue;
      f = LMN_SATOM_GET_FUNCTOR(srcatom);
      newatom = lmn_mem_newatom(destmem, f);

      proc_tbl_put_atom(atoms, srcatom, (LmnWord)newatom);

      start = 0;
      end = LMN_SATOM_GET_ARITY(srcatom);

      if (LMN_IS_PROXY_FUNCTOR(f)) {
        start = 1, end = 2;
        LMN_PROXY_SET_MEM(newatom, destmem);

        if (f == LMN_OUT_PROXY_FUNCTOR) {
          LmnSAtom srcinside;
          LmnSAtom newinside;

          srcinside = LMN_SATOM(LMN_SATOM_GET_LINK(srcatom, 0));
          proc_tbl_get_by_atom(atoms, srcinside, &t);
          newinside = LMN_SATOM(t);

          /* 必ず子膜につながっているはず */
          LMN_ASSERT(LMN_SATOM_GET_FUNCTOR(srcinside) == LMN_IN_PROXY_FUNCTOR);
          LMN_ASSERT(LMN_PROXY_GET_MEM(srcinside)->parent == LMN_PROXY_GET_MEM(srcatom));
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
          lmn_mem_push_atom(destmem, newargatom, attr);
        } else if (proc_tbl_get_by_atom(atoms, LMN_SATOM(a), &t)) {
          newlink_symbol_and_something(newatom, i, LMN_ATOM(t), attr);
        }
      }
    }));
  }));

  destmem->atom_num = srcmem->atom_num;

  /* copy activated flag */
  destmem->is_activated = srcmem->is_activated; /* MC */
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
                         ProcessTbl *ret_atommap)
{
  ProcessTbl atommap;
  Vector *stack;
  unsigned int i;
  LmnWord t = 0;

  atommap = proc_tbl_make_with_size(64);
  stack = vec_make(16);
  *ret_dstlovec = vec_make(16);

  /* 根をスタックに積む。スタックにはリンクオブジェクトではなくアトムを
     積むため、ここで根の先のアトムをコピーしスタックに積む必要がある */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    LmnAtom cpatom;

    if (LMN_ATTR_IS_DATA(l->pos)) {
      cpatom = lmn_copy_data_atom(l->ap, l->pos);
      lmn_mem_push_atom(mem, cpatom, l->pos);
    } else { /* symbol atom */
      /* コピー済みでなければコピーする */
      if (!proc_tbl_get_by_atom(atommap, LMN_SATOM(l->ap), &t)) {
        cpatom = LMN_ATOM(lmn_copy_satom_with_data(LMN_SATOM(l->ap)));

        mem_push_symbol_atom(mem, LMN_SATOM(cpatom));
        proc_tbl_put_atom(atommap, LMN_SATOM(l->ap), (LmnWord)cpatom);

        /* 根のリンクのリンクポインタを0に設定する */
        LMN_SATOM_SET_LINK(cpatom, l->pos, 0);
        vec_push(stack, l->ap);
      } else {
        /* コピー済みの場合はスタックには追加しない */
        cpatom = LMN_ATOM(t);
        LMN_SATOM_SET_LINK(cpatom, l->pos, 0);
      }
    }
    vec_push(*ret_dstlovec, (vec_data_t)LinkObj_make(cpatom, l->pos));
  }

  while (vec_num(stack) > 0) {
    LmnSAtom src_atom = LMN_SATOM(vec_pop(stack));
    LmnSAtom copied;

    proc_tbl_get_by_atom(atommap, src_atom, &t);
    copied = LMN_SATOM(t);

    for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
      LmnAtom next_src = LMN_SATOM_GET_LINK(src_atom, i);
      LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

      /* LMN_SATOM_GET_LINK(copied, i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        lmn_mem_push_atom(mem, next_src, next_attr);
      }
      else if (LMN_SATOM_GET_LINK(copied, i) != 0) {
        LmnAtom next_copied;
        if (!proc_tbl_get_by_atom(atommap, LMN_SATOM(next_src), &t)) { /* next_srcは未訪問 */
          next_copied = LMN_ATOM(lmn_copy_satom_with_data(LMN_SATOM(next_src)));
          mem_push_symbol_atom(mem, LMN_SATOM(next_copied));
          proc_tbl_put_atom(atommap, LMN_SATOM(next_src), next_copied);
          vec_push(stack, next_src);
        } else {
          next_copied = LMN_ATOM(t);
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
  ProcessTbl atoms;
  BOOL b;

  b = ground_atoms(srcvec, avovec, &atoms, natoms);
  if (b) {
    proc_tbl_free(atoms);
  }

  return b;
}


/* xとyが1つのリンクの逆向き表現かどうか */
#define IS_BUDDY(xap, xattr, yap, yattr)             \
      ( !LMN_ATTR_IS_DATA(xattr)                &&   \
        !LMN_ATTR_IS_DATA(yattr)                &&   \
        (LMN_SATOM_GET_LINK(xap, xattr) == yap) &&   \
        (LMN_SATOM_GET_ATTR(xap, xattr) == yattr) )


/* srcvecから出るリンクのリストが基底項プロセスに到達している場合、
 * avovecに基底項プロセスに存在するシンボルアトム、natomsにアトムの数、戻り値に真を返す。
 * リストが基底項プロセスに到達していない場合にはatomsはNULLとなり、偽を返す。
 * ただし、リンクがavovecに到達している場合には、基底項プロセスとはならない。 */
BOOL ground_atoms(Vector        *srcvec,
                  Vector        *avovec,
                  ProcessTbl    *atoms,
                  unsigned long *natoms)
{
  Vector *unsearched_link_stack;         /* 探索待ちリンク */
  ProcessTbl found_ground_symbol_atoms;  /* ground内の発見済みのシンボルアトム */
  unsigned long count_of_ground_atoms;   /* ground内のアトムの個数 */
  int i;
  int reached_root_count; /* 到達した根の個数(1つは始点) */
  BOOL result;

  unsearched_link_stack = vec_make(16);
  reached_root_count = 1;
  found_ground_symbol_atoms = proc_tbl_make_with_size(64);
  result = TRUE;
  count_of_ground_atoms = 0;

  /* groundはつながったグラフなので1つの根からだけたどればよい */

  {
    LinkObj l = (LinkObj)vec_get(srcvec, 0);
    vec_push(unsearched_link_stack, (LmnWord)LinkObj_make(l->ap, l->pos));
  }

  while (!vec_is_empty(unsearched_link_stack)) {
    LinkObj l;
    LmnAtom l_ap;
    LmnLinkAttr l_pos;

    l = (LinkObj)vec_pop(unsearched_link_stack);
    l_ap = l->ap;
    l_pos = l->pos;

    LMN_FREE(l);

    if (LMN_ATTR_IS_DATA(l_pos)) { /* lがデータなら行き止まり */
      if (lmn_data_atom_is_ground(l_ap, l_pos)) {
        count_of_ground_atoms++;
        continue;
      } else {
        result = FALSE; /* groundでないデータが出現したら終了 */
        goto returning;
      }
    } else { /* lがシンボルアトムを指していれば */

      /* lがavovecにつながっていれば */
      for (i = 0; avovec != NULL && i < vec_num(avovec); i++) {
        LinkObj a = (LinkObj)vec_get(avovec, i);
        if(IS_BUDDY(l_ap, l_pos, a->ap, a->pos)){
          result = FALSE;
          goto returning;
        }

      }
      /* lがsrcvecにつながっていれば */
      {
        BOOL continue_flag = FALSE;
        for (i = 0; i < vec_num(srcvec); i++) {
          LinkObj a = (LinkObj)vec_get(srcvec, i);
          if (IS_BUDDY(l_ap, l_pos, a->ap, a->pos)) {
            reached_root_count++;
            continue_flag = TRUE;
            break;
          }
        }
        if (continue_flag) continue;
      }

      /* lがプロキシを指していれば */
      if (LMN_SATOM_IS_PROXY(l_ap)) {
        result = FALSE;
        goto returning;
      }

      /* lの指すアトムが訪問済みなら */
      if (proc_tbl_get_by_atom(found_ground_symbol_atoms, (LmnSAtom)l_ap, NULL)) {
        continue;
      }

      /* lはシンボルアトムで初出のアトムを指すため,その先を探索する必要がある */
      proc_tbl_put_atom(found_ground_symbol_atoms, (LmnSAtom)l_ap, (LmnWord)l_ap);
      count_of_ground_atoms++;

      for(i = 0; i < LMN_SATOM_GET_ARITY(l_ap); i++) {
        if (i == l_pos || LMN_ATTR_IS_EX(l_pos)) continue;
        vec_push(unsearched_link_stack,
                 (LmnWord)LinkObj_make(LMN_SATOM_GET_LINK(l_ap, i),
                                       LMN_SATOM_GET_ATTR(l_ap, i)));
      }
    }
  }

  /* もし未到達の根があれば結合グラフになっていないのでgroundでない */
  result = (reached_root_count == vec_num(srcvec));

 returning:
  for (i = 0; i < vec_num(unsearched_link_stack); i++) {
    LMN_FREE(vec_get(unsearched_link_stack, i));
  }
  vec_free(unsearched_link_stack);

  if (result) {
    *atoms = found_ground_symbol_atoms;
    *natoms = count_of_ground_atoms;
  }else{
    proc_tbl_free(found_ground_symbol_atoms);
    *atoms = NULL;
    *natoms = 0;
  }

  return result;
}

/* 前の実装.しばらく残しておく */
BOOL ground_atoms_old(Vector *srcvec,
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


int mem_remove_symbol_atom_with_buddy_data_f(LmnWord _k,
                                             LmnWord _v,
                                             LmnWord _arg)
{
  mem_remove_symbol_atom_with_buddy_data((LmnMembrane *)_arg, (LmnSAtom)_v);
  return 1;
}



void lmn_mem_remove_ground(LmnMembrane *mem, Vector *srcvec)
{
  ProcessTbl atoms;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t);

  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos)) {
      lmn_mem_remove_data_atom(mem, l->ap, l->pos);
    } else if (LMN_ATTR_IS_EX(l->pos)) {
      mem_remove_symbol_atom(mem, LMN_SATOM(l->ap));
    }
  }
  proc_tbl_free(atoms);
}


int free_symbol_atom_with_buddy_data_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  free_symbol_atom_with_buddy_data((LmnSAtom)_v);
  return 1;
}


void lmn_mem_free_ground(Vector *srcvec)
{
  ProcessTbl atoms;
  unsigned long i, t;


  if (ground_atoms(srcvec, NULL, &atoms, &t)) {
    proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);
    proc_tbl_free(atoms);
  }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) lmn_free_atom(l->ap, l->pos);
  }
}

void lmn_mem_delete_ground(LmnMembrane *mem, Vector *srcvec)
{
  ProcessTbl atoms;
  unsigned long i, t;

  if (!ground_atoms(srcvec, NULL, &atoms, &t)) {
    fprintf(stderr, "remove ground false\n");
  }

  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);
  proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObj l = (LinkObj)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      lmn_mem_remove_data_atom(mem, l->ap, l->pos);
      lmn_free_atom(l->ap, l->pos);
    }
  }

  proc_tbl_free(atoms);
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
  LmnFunctor f;
  LmnSAtom a;
  AtomListEntry *ent;
  unsigned int anum_max; /* 膜内に存在するアトムをファンクタ毎にグループ化した際の、集合の大きさの最大値 */
  unsigned int i, j;

  vec = vec_make(1);
  memset(vec->tbl, 0, sizeof(atomvec_data *) * vec->cap);
  anum_max = 0;

  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
    atomvec_data *ad;

    ad = LMN_MALLOC(atomvec_data);
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
  }));

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

  LMN_ASSERT(vec_num(vec));

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
static BOOL lmn_mem_trace_links(LmnSAtom a1,
                                LmnSAtom a2,
                                Vector *v_log1,
                                Vector *v_log2,
                                int current_depth,
                                BOOL need_to_check_this_membrane_processes) {
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
    LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a1)) && LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a2)));

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
      LMN_ASSERT(next_depth >= CHECKED_MEM_DEPTH);

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

    /* Step3.5. 両膜内に含まれるルールが等価であることを確認 */
    if (!lmn_rulesets_equals(&mem1->rulesets, &mem2->rulesets))
      goto STEP_1_FALSE;

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

    v_log1 = NULL;
    v_log2 = NULL;
    v_atoms_not_checked1 = NULL;
    v_atoms_not_checked2 = NULL;
    v_mems_children1 = NULL;
    v_mems_children2 = NULL;

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
      LMN_ASSERT(length == mem2->atom_num);
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

        LMN_ASSERT(vec_num(atomvec_mem1) == vec_num(atomvec_mem2));

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

      LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

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
            LMN_ASSERT(vec_num(v_log1) == vec_num(v_log2));
            for (n = 0; n < vec_num(v_atoms_not_checked2); ++n) {
              if (LMN_SATOM(vec_get(v_atoms_not_checked2, n)) == a2) {
                vec_pop_n(v_atoms_not_checked2, n);
                break;
              }
            }
            LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

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
            LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));
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
        LMN_ASSERT(vec_num(v_mems_children1) == vec_num(v_mems_children2));
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

BOOL mem_equals(LmnMembrane *mem1, LmnMembrane *mem2)
{
  return lmn_mem_equals_rec(mem1, mem2, CHECKED_MEM_DEPTH);
}

BOOL lmn_mem_equals(LmnMembrane *mem1, LmnMembrane *mem2)
{
  BOOL t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  t = mem_equals(mem1, mem2);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  return t;
}
/*----------------------------------------------------------------------*/
/* 膜の同型性判定 ここまで */

