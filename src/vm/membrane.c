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
#include "verifier/verifier.h"
#include <ctype.h>
#include <limits.h>

#ifdef PROFILE
#  include "verifier/runtime_status.h"
#endif

#ifdef USE_FIRSTCLASS_RULE
#  include "firstclass_rule.h"
#endif

/** ----
 *  AtomListEntry.
 *  同一ファンクタのアトムをリスト単位でまとめておくための機構
 */

/* この構造体をAtomとして扱うことで,この構造体自身が
   HeadとTailの両方の役目を果たしている */
typedef struct AtomListEntry {
  LmnSymbolAtomRef tail, head;
#ifdef NEW_ATOMLIST
  int n;
#endif
  struct SimpleHashtbl *record;
} AtomListEntry;

LmnSymbolAtomRef atomlist_head(AtomListEntryRef lst) {
  return LMN_SATOM(lst->head);
}
LmnSymbolAtomRef lmn_atomlist_end(AtomListEntryRef lst) {
  return LMN_SATOM(lst);
}

#ifdef NEW_ATOMLIST
int atomlist_ent_num(AtomListEntryRef lst) {
  return lst->n;
}
void atomlist_set_num(AtomListEntryRef lst, int n) {
  lst->n = n;
}
void atomlist_add_num(AtomListEntryRef lst, int n) {
  lst->n += n;
}
#else
int atomlist_ent_num(AtomListEntryRef lst) {
  return -1;
}
void atomlist_set_num(AtomListEntryRef lst, int n) {
  
}
void atomlist_add_num(AtomListEntryRef lst, int n) {
  
}
#endif

void atomlist_modify_num(AtomListEntry *ent, int n) {
  atomlist_add_num(ent, n);
}

BOOL atomlist_is_empty(AtomListEntry *ent) {
  return atomlist_head(ent) == LMN_SATOM(ent);
}

/* アトムリストasを空にする. */
void atomlist_set_empty(AtomListEntry *ent) {
  LMN_SATOM_SET_PREV((LmnSymbolAtomRef)ent, (LmnSymbolAtomRef)ent);
  LMN_SATOM_SET_NEXT((LmnSymbolAtomRef)ent, (LmnSymbolAtomRef)ent);
  atomlist_set_num(ent, 0);
}

/* アトムリストALからアトムAを削除する.
 * ただし, リストのつなぎ変えだけを行い, 膜からのアトムAのdeleteやatomのfreeはしない */
void remove_from_atomlist(LmnSymbolAtomRef a, AtomListEntry *ent) {
  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a),     LMN_SATOM_GET_NEXT_RAW(a));
  if (ent) {
    atomlist_modify_num(ent, -1);
  }
}

/* アトムリストentにおいて, アトムprvとアトムnxtの間にアトムinsを挿入する.
 * ただし, prvにNULLを渡した場合はnxtのprevポイント先をprvとして扱う. */
void insert_to_atomlist(LmnSymbolAtomRef prv, LmnSymbolAtomRef ins, LmnSymbolAtomRef nxt,
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
void push_to_atomlist(LmnSymbolAtomRef a, AtomListEntry *ent) {
  LMN_SATOM_SET_NEXT(a, (LmnSymbolAtomRef)ent);
  LMN_SATOM_SET_PREV(a, ent->tail);
  LMN_SATOM_SET_NEXT(ent->tail, a);
  ent->tail = a;
  atomlist_modify_num(ent, +1);
}

int atomlist_get_entries_num(AtomListEntry *ent) {
  if (!ent) {
    return 0;
  } else {
#ifdef NEW_ATOMLIST
    /* O(1) */
    return ent->n;

#else
    /* O(N) */
    LmnSymbolAtomRef atom;
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
void atomlist_append(AtomListEntry *e1, AtomListEntry *e2)
{
  if (atomlist_head(e2) != lmn_atomlist_end(e2)) {/* true if e2 is not empty */
    LMN_SATOM_SET_NEXT(e1->tail, e2->head);
    LMN_SATOM_SET_PREV(e2->head, e1->tail);
    LMN_SATOM_SET_NEXT(e2->tail, (LmnSymbolAtomRef)e1);
    e1->tail = e2->tail;
    atomlist_modify_num(e1, atomlist_get_entries_num(e2));
  }
  atomlist_set_empty(e2);
}

/* return NULL when atomlist doesn't exist. */
LmnSymbolAtomRef atomlist_get_record(AtomListEntry *atomlist, int findatomid) {
  if (atomlist->record) {
    return (LmnSymbolAtomRef)hashtbl_get_default(atomlist->record, findatomid, 0);
  } else {
    atomlist->record = hashtbl_make(4);
    return NULL;
  }
}

void atomlist_put_record(AtomListEntryRef lst, int id, LmnAtomRef record) {
  hashtbl_put(lst->record, id, (HashKeyType)record);
}


static void lmn_mem_copy_cells_sub(LmnMembraneRef destmem,
                                   LmnMembraneRef srcmem,
                                   ProcessTableRef  atoms,
                                   BOOL        hl_nd);

/* ルールセットadd_rsをルールセット配列src_vへ追加する.
 * グラフ同型性判定処理などのために整数IDの昇順を維持するよう追加する. */
void lmn_mem_add_ruleset_sort(Vector *src_v, LmnRuleSetRef add_rs)
{
  int i, j, n;
  LmnRulesetId add_id;
  add_id = lmn_ruleset_get_id(add_rs);
  n = vec_num(src_v);
  for (i = 0; i < n; i++) {
    LmnRuleSetRef rs_i;
    LmnRulesetId dst_id;

    rs_i   = (LmnRuleSetRef)vec_get(src_v, i);
    dst_id = lmn_ruleset_get_id(rs_i);

    if (dst_id == add_id && !lmn_ruleset_has_uniqrule(add_rs)) {
      /* 同じ階層にuniqでない同一のルールセットが既に存在するならば追加する必要はない */
      break;
    } else if (dst_id >= add_id) {
      LmnRuleSetRef prev = add_rs;
      vec_push(src_v, 0);

      for (j = i; j < (n + 1); j++) {
        LmnRuleSetRef tmp = (LmnRuleSetRef)vec_get(src_v, j);
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


/*----------------------------------------------------------------------
 * Atom Set
 */

static inline AtomListEntry *make_atomlist(void);
static inline void free_atomlist(AtomListEntry *as);

/* 新しいアトムリストを作る */
static inline AtomListEntry *make_atomlist()
{
  AtomListEntry *as = LMN_MALLOC(struct AtomListEntry);
  as->record = NULL; /* 全てのアトムの種類に対してfindatom2用ハッシュ表が必要なわけではないので動的にmallocさせる */
  atomlist_set_empty(as);

  return as;
}

/* アトムリストの解放処理 */
static inline void free_atomlist(AtomListEntry *as)
{
  /* lmn_mem_move_cellsでアトムリストの再利用を行っていて
   * ポインタがNULLになる場合があるので、検査を行う必要がある。*/
  if (as) {
    if (as->record) {
      hashtbl_free(as->record);
    }
    LMN_FREE(as);
  }
}


/*----------------------------------------------------------------------
 * Membrane
 */

struct LmnMembrane {
  AtomSet              atomset;
  ProcessID            id;
  unsigned int         max_functor;
  unsigned int         atomset_size;
  unsigned int         atom_symb_num;  /* # of symbol atom except proxy */
  unsigned int         atom_data_num;
  lmn_interned_str     name;
  BOOL                 is_activated;
  LmnMembraneRef          parent;
  LmnMembraneRef          child_head;
  LmnMembraneRef          prev, next;
  struct Vector        rulesets;
#ifdef USE_FIRSTCLASS_RULE
  Vector *             firstclass_rulesets;
#endif
};

LmnMembraneRef lmn_mem_make(void)
{
  LmnMembraneRef mem;

  mem = LMN_MALLOC(struct LmnMembrane);
  mem->parent        =  NULL;
  mem->child_head    =  NULL;
  mem->prev          =  NULL;
  mem->next          =  NULL;
  mem->max_functor   =  0U;
  mem->atomset_size  =  32;
  mem->is_activated  =  TRUE;
  mem->atom_symb_num =  0U;
  mem->atom_data_num =  0U;
  mem->name          =  ANONYMOUS;
  mem->id            =  0UL;
  mem->atomset       =  LMN_CALLOC(struct AtomListEntry *, mem->atomset_size);
  vec_init(&mem->rulesets, 1);
  lmn_mem_set_id(mem, env_gen_next_id());

#ifdef USE_FIRSTCLASS_RULE
  mem->firstclass_rulesets = vec_make(4);
#endif

  return mem;
}

lmn_interned_str LMN_MEM_NAME_ID(LmnMembraneRef m) {
  return m->name;
}
const char *LMN_MEM_NAME(LmnMembraneRef m) {
  return LMN_SYMBOL_STR(LMN_MEM_NAME_ID(m));
}
LmnMembraneRef lmn_mem_parent(LmnMembraneRef m) {
  return m->parent;
}
void lmn_mem_set_parent(LmnMembraneRef m, LmnMembraneRef parent) {
  m->parent = parent;
}

BOOL lmn_mem_is_active(LmnMembraneRef m) {
  return m->is_activated;
}
unsigned int lmn_mem_max_functor(LmnMembraneRef m) {
  return m->max_functor;
}
void lmn_mem_set_name(LmnMembraneRef m, lmn_interned_str name) {
  m->name = name;
}
void lmn_mem_set_active(LmnMembraneRef m, BOOL is_activated) {
  m->is_activated = is_activated;
}
struct Vector * lmn_mem_get_rulesets(LmnMembraneRef m) {
  return &(m->rulesets);
}
int lmn_mem_ruleset_num(LmnMembraneRef m) {
  return vec_num(lmn_mem_get_rulesets(m));
}
LmnRuleSetRef lmn_mem_get_ruleset(LmnMembraneRef m, int i) {
  return (LmnRuleSetRef)vec_get(lmn_mem_get_rulesets(m), i);
}
unsigned int lmn_mem_symb_atom_num(LmnMembraneRef m) {
  return m->atom_symb_num;
}
void lmn_mem_symb_atom_set(LmnMembraneRef m, unsigned int n) {
  m->atom_symb_num = n;
}
unsigned int lmn_mem_data_atom_num(LmnMembraneRef m) {
  return m->atom_data_num;
}
void lmn_mem_data_atom_set(LmnMembraneRef m, unsigned int n) {
  m->atom_data_num = n;
}
unsigned int lmn_mem_atom_num(LmnMembraneRef m) {
  return lmn_mem_symb_atom_num(m) + lmn_mem_data_atom_num(m);
}
BOOL lmn_mem_natoms(LmnMembraneRef m, unsigned int n) {
  return lmn_mem_atom_num(m) == n;
}
void lmn_mem_natoms_copy(LmnMembraneRef m, LmnMembraneRef n) {
  lmn_mem_symb_atom_set(m, lmn_mem_symb_atom_num(n));
  lmn_mem_data_atom_set(m, lmn_mem_data_atom_num(n));
}
void lmn_mem_symb_atom_add(LmnMembraneRef m, int n) {
  m->atom_symb_num += n;
}
void lmn_mem_symb_atom_sub(LmnMembraneRef m, int n) {
  m->atom_symb_num -= n;
}
void lmn_mem_symb_atom_inc(LmnMembraneRef m) {
  m->atom_symb_num++;
}
void lmn_mem_symb_atom_dec(LmnMembraneRef m) {
  m->atom_symb_num--;
}
void lmn_mem_data_atom_add(LmnMembraneRef m, int n) {
  m->atom_data_num += n;
}
void lmn_mem_data_atom_sub(LmnMembraneRef m, int n) {
  m->atom_data_num -= n;
}
void lmn_mem_data_atom_inc(LmnMembraneRef m) {
  m->atom_data_num++;
}
void lmn_mem_data_atom_dec(LmnMembraneRef m) {
  m->atom_data_num--;
}

LmnMembraneRef lmn_mem_child_head(LmnMembraneRef m) {
  return m->child_head;
}
LmnMembraneRef lmn_mem_next(LmnMembraneRef m) {
  return m->next;
}
LmnMembraneRef lmn_mem_prev(LmnMembraneRef m) {
  return m->prev;
}
ProcessID lmn_mem_id(LmnMembraneRef m) {
  return m->id;
}
void lmn_mem_set_id(LmnMembraneRef m, ProcessID n) {
  m->id = n;
}

/* 膜memの解放を行う.
 * 膜memに所属する子膜とアトムのメモリ管理は呼び出し側で行う. */
void lmn_mem_free(LmnMembraneRef mem)
{
  AtomListEntry *ent;

  /* free all atomlists  */
  EACH_ATOMLIST(mem, ent, ({
    free_atomlist(ent);
  }));

  lmn_mem_rulesets_destroy(&mem->rulesets);
  env_return_id(lmn_mem_id(mem));
#ifdef USE_FIRSTCLASS_RULE
  vec_free(mem->firstclass_rulesets);
#endif
  LMN_FREE(mem->atomset);
  LMN_FREE(mem);
}


/* 膜mem内のアトム, 子膜のメモリを解放する.
 * 子膜が存在する場合は, その子膜に再帰する. */
void lmn_mem_drop(LmnMembraneRef mem)
{
  AtomListEntry *ent;
  LmnMembraneRef m, n;

  /* drop and free child mems */
  m = mem->child_head;
  while (m) {
    n = m;
    m = m->next;
    lmn_mem_free_rec(n);
  }
  mem->child_head = NULL;

  EACH_ATOMLIST(mem, ent, ({
    LmnSymbolAtomRef a;
    a = atomlist_head(ent);
    if (LMN_IS_HL(a)) {
      continue; /* hyperlinkはbuddy symbol atomと一緒に削除されるため */
    }
    while (a != lmn_atomlist_end(ent)) {
      LmnSymbolAtomRef b = a;
      a = LMN_SATOM_GET_NEXT_RAW(a);
      free_symbol_atom_with_buddy_data(b);
    }
    atomlist_set_empty(ent);
  }));

  mem->atom_symb_num =  0U;
  mem->atom_data_num =  0U;
}


/* ルールセット配列rulesetsを解放する */
void lmn_mem_rulesets_destroy(Vector *rulesets)
{
  unsigned int i, n = vec_num(rulesets);

  for (i = 0; i < n; i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(rulesets, i);

    if (lmn_ruleset_is_copy(rs)) {
      lmn_ruleset_copied_free(rs);
    }
  }
  vec_destroy(rulesets);
}

void move_symbol_atom_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem){
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a);
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);

  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a),     LMN_SATOM_GET_NEXT_RAW(a));

  LMN_SATOM_SET_NEXT(a, (LmnSymbolAtomRef)ent);
  LMN_SATOM_SET_PREV(a, ent->tail);
  LMN_SATOM_SET_NEXT(ent->tail, a);
  ent->tail = a;
}

void move_symbol_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem){
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a);
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);

  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a),     LMN_SATOM_GET_NEXT_RAW(a));


  LMN_SATOM_SET_NEXT(a, ent->head);
  LMN_SATOM_SET_PREV(a, (LmnSymbolAtomRef)ent);
  LMN_SATOM_SET_PREV(ent->head, a);
  ent->head = a;
}
void move_symbol_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem){
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a);
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);
  
  if(a != ent->head) {
    LMN_SATOM_SET_NEXT(ent->tail, ent->head);
    LMN_SATOM_SET_PREV(ent->head, ent->tail);
    ent->tail = LMN_SATOM_GET_PREV(a);
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a), (LmnSymbolAtomRef)ent);
    LMN_SATOM_SET_PREV(a, (LmnSymbolAtomRef)ent);
    ent->head = a;
    }
}
void move_symbol_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1, LmnMembraneRef mem){
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(a);
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);
  
  if (ent->tail == a1) ent->tail = LMN_SATOM_GET_PREV(a1);
  else if (ent->head == a1) ent->head = LMN_SATOM_GET_NEXT_RAW(a1);

  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a1), LMN_SATOM_GET_PREV(a1));
  LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a1),     LMN_SATOM_GET_NEXT_RAW(a1));

  if (ent->tail == LMN_SATOM_GET_NEXT_RAW(a)) ent->tail = a1;

  LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), a1);
  LMN_SATOM_SET_NEXT(a1, LMN_SATOM_GET_NEXT_RAW(a));
  LMN_SATOM_SET_PREV(a1, a);
  LMN_SATOM_SET_NEXT(a, a1);
  
}

void mem_push_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom)
{
  AtomListEntry *as;
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);

  if (LMN_SATOM_ID(atom) == 0) { /* 膜にpushしたならばidを割り当てる */
    LMN_SATOM_SET_ID(atom, env_gen_next_id());
  }

  as = lmn_mem_get_atomlist(mem, f);
  if (!as) { /* 本膜内に初めてアトムatomがPUSHされた場合 */
    LMN_ASSERT(mem->atomset); /* interpreter側で値がオーバーフローすると発生するので, ここで止める */
    if (mem->max_functor < f + 1) {
      mem->max_functor = f + 1;
      while (mem->atomset_size - 1 < mem->max_functor) {
        int org_size = mem->atomset_size;
        mem->atomset_size *= 2;
        LMN_ASSERT(mem->atomset_size > 0);
        mem->atomset = LMN_REALLOC(struct AtomListEntry*, mem->atomset, mem->atomset_size);
        memset(mem->atomset + org_size, 0, (mem->atomset_size - org_size) * sizeof(struct AtomListEntry *));
      }
    }
    as = mem->atomset[f] = make_atomlist();
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, mem);
  }
  else if (LMN_FUNC_IS_HL(f)) {
    LMN_HL_MEM(lmn_hyperlink_at_to_hl(atom)) = mem;
    lmn_mem_symb_atom_inc(mem);
  }
  else if (f != LMN_UNIFY_FUNCTOR) {
    /* symbol atom except proxy and unify */
    lmn_mem_symb_atom_inc(mem);
  }

  push_to_atomlist(atom, as);
}


unsigned long lmn_mem_space(LmnMembraneRef mem)
{
  AtomListEntry *ent;
  unsigned long ret;
  unsigned int i;

  ret = 0;
  ret += sizeof(struct LmnMembrane);
  ret += sizeof(struct AtomListEntry*) * mem->atomset_size;
  /* atomset */
  EACH_ATOMLIST(mem, ent, ({
    LmnSymbolAtomRef atom;
    ret += sizeof(struct AtomListEntry);
    if (ent->record) {
      ret += internal_hashtbl_space(ent->record);
    }
    EACH_ATOM(atom, ent, ({
      ret += LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom)));
    }));
  }));

  /* ruleset */
  ret += vec_space_inner(&mem->rulesets);
  for (i = 0; i < vec_num(&mem->rulesets); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(&mem->rulesets, i);
    if (lmn_ruleset_is_copy(rs)) {
      ret += lmn_ruleset_space(rs);
    }
  }

  return ret;
}

unsigned long lmn_mem_root_space(LmnMembraneRef src)
{
  unsigned long ret;
  LmnMembraneRef ptr;

  ret = lmn_mem_space(src);
  for (ptr = src->child_head; ptr != NULL; ptr = ptr->next) {
    ret += lmn_mem_root_space(ptr);
  }

  return ret;
}


void lmn_mem_link_data_atoms(LmnMembraneRef mem,
                             LmnAtomRef d0,
                             LmnLinkAttr attr0,
                             LmnAtomRef d1,
                             LmnLinkAttr attr1)
{
  LmnSymbolAtomRef ap = lmn_new_atom(LMN_UNIFY_FUNCTOR);

  LMN_SATOM_SET_LINK(ap, 0, d0);
  LMN_SATOM_SET_LINK(ap, 1, d1);
  LMN_SATOM_SET_ATTR(ap, 0, attr0);
  LMN_SATOM_SET_ATTR(ap, 1, attr1);
  mem_push_symbol_atom(mem, ap);
}

/* atom1, atom2をシンボルアトムに限定した unify link */
void lmn_mem_unify_symbol_atom_args(LmnSymbolAtomRef atom1, int pos1,
                                    LmnSymbolAtomRef atom2, int pos2)
{
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  ap1   = LMN_SATOM_GET_LINK(atom1, pos1);
  attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
  ap2   = LMN_SATOM_GET_LINK(atom2, pos2);
  attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);

  LMN_SATOM_SET_LINK(ap2, attr2, ap1);
  LMN_SATOM_SET_ATTR(ap2, attr2, attr1);
  LMN_SATOM_SET_LINK(ap1, attr1, ap2);
  LMN_SATOM_SET_ATTR(ap1, attr1, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void lmn_mem_unify_atom_args(LmnMembraneRef mem,
                             LmnSymbolAtomRef atom1, int pos1,
                             LmnSymbolAtomRef atom2, int pos2)
{
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  ap1   = LMN_SATOM_GET_LINK(atom1, pos1);
  attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
  ap2   = LMN_SATOM_GET_LINK(atom2, pos2);
  attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);

  if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
    lmn_mem_link_data_atoms(mem, ap1, attr1, ap2, attr2);
  }
  else if (LMN_ATTR_IS_DATA(attr1)) {
    LMN_SATOM_SET_LINK(ap2, attr2, ap1);
    LMN_SATOM_SET_ATTR(ap2, attr2, attr1);

    /*below two line: alimjan yasin 2016-3-8*/
    LMN_SATOM_SET_LINK(ap1, 0, ap2);
    LMN_SATOM_SET_ATTR(ap1, 0, attr2);
  }
  else if (LMN_ATTR_IS_DATA(attr2)) {
    LMN_SATOM_SET_LINK(ap1, attr1, ap2);
    LMN_SATOM_SET_ATTR(ap1, attr1, attr2);

    /*below two line: alimjan yasin 2016-3-8*/
    LMN_SATOM_SET_LINK(ap2, 0, ap1);
    LMN_SATOM_SET_ATTR(ap2, 0, attr1);
  }
  else {
    LMN_SATOM_SET_LINK(ap2, attr2, ap1);
    LMN_SATOM_SET_ATTR(ap2, attr2, attr1);
    LMN_SATOM_SET_LINK(ap1, attr1, ap2);
    LMN_SATOM_SET_ATTR(ap1, attr1, attr2);
  }
}

/* シンボルアトムに限定したnewlink */
void lmn_newlink_in_symbols(LmnSymbolAtomRef atom0, int pos0,
                            LmnSymbolAtomRef atom1, int pos1)
{
  LMN_SATOM_SET_LINK(atom0, pos0, atom1);
  LMN_SATOM_SET_LINK(atom1, pos1, atom0);
  LMN_SATOM_SET_ATTR(atom0, pos0, LMN_ATTR_MAKE_LINK(pos1));
  LMN_SATOM_SET_ATTR(atom1, pos1, LMN_ATTR_MAKE_LINK(pos0));
}


void lmn_newlink_with_ex(LmnMembraneRef mem,
                         LmnSymbolAtomRef atom0, LmnLinkAttr attr0, int pos0,
                         LmnSymbolAtomRef atom1, LmnLinkAttr attr1, int pos1)
{
  /* both symbol */
  LMN_SATOM_SET_LINK(atom0, pos0, atom1);
  LMN_SATOM_SET_LINK(atom1, pos1, atom0);

  if (LMN_ATTR_IS_EX(attr0)) {
    if (LMN_ATTR_IS_EX(attr1)) { /* 0, 1 are ex */
//      LMN_SATOM_SET_ATTR(atom0, pos0, attr1);
//      LMN_SATOM_SET_ATTR(atom1, pos1, attr0);
      /* 現状では、hyperlinkアトム同士が接続されると消去される */
      //      lmn_mem_delete_atom(mem, LMN_ATOM(atom0), attr0);
      //      lmn_mem_delete_atom(mem, LMN_ATOM(atom1), attr1);
      lmn_fatal("Two hyperlinks cannot be connected using = .");
    } else { /* 0 is ex */
      LMN_SATOM_SET_ATTR(atom0, pos0, LMN_ATTR_MAKE_LINK(pos1));
      LMN_SATOM_SET_ATTR(atom1, pos1, attr0);
    }
  }
  else { /* 1 is ex */
    LMN_SATOM_SET_ATTR(atom0, pos0, attr1);
    LMN_SATOM_SET_ATTR(atom1, pos1, LMN_ATTR_MAKE_LINK(pos0));
  }

}

/* シンボルアトムatom0と、シンボルorデータアトムatom1の間にリンクを張る。
 * このコードが重複して現れたので、関数に分割した */
/* static inline void newlink_symbol_and_something(LmnSymbolAtomRef atom0,
                                                int pos,
                                                LmnAtomRef atom1,
                                                LmnLinkAttr attr)
{
  LMN_SATOM_SET_LINK(atom0, pos, atom1);
  LMN_SATOM_SET_ATTR(atom0, pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    LMN_SATOM_SET_LINK(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), atom0);
    LMN_SATOM_SET_ATTR(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos));
  }
} */

static inline void newlink_symbol_and_hlink(LmnSymbolAtomRef atom, int pos, LmnSymbolAtomRef hlAtom) LMN_UNUSED;
/* シンボルアトムatomと、ハイパーリンクアトムhlAtomの間にリンクを張る。
 * ハイパーリンクは接続の引数が0だったりと特殊なので関数も用意した。*/
static inline void newlink_symbol_and_hlink(LmnSymbolAtomRef atom,
                                            int pos,
                                            LmnSymbolAtomRef hlAtom)
{
  LMN_SATOM_SET_LINK(atom, pos, hlAtom);
  LMN_SATOM_SET_LINK(hlAtom, 0, atom);
}


void lmn_mem_newlink(LmnMembraneRef mem,
                     LmnAtomRef atom0, LmnLinkAttr attr0, int pos0,
                     LmnAtomRef atom1, LmnLinkAttr attr1, int pos1)
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


void lmn_relink_symbols(LmnSymbolAtomRef atom0, int pos0,
                        LmnSymbolAtomRef atom1, int pos1)
{
  newlink_symbol_and_something(LMN_SATOM(atom0),
                               pos0,
                               LMN_SATOM_GET_LINK(LMN_SATOM(atom1), pos1),
                               LMN_SATOM_GET_ATTR(LMN_SATOM(atom1), pos1));
}


void lmn_mem_relink_atom_args(LmnMembraneRef mem,
                              LmnAtomRef atom0, LmnLinkAttr attr0, int pos0,
                              LmnAtomRef atom1, LmnLinkAttr attr1, int pos1)
{
  /* TODO: relinkではatom0,atom1がデータになることはないはず
   *       このことを確認する */
  LMN_ASSERT(!LMN_ATTR_IS_DATA(attr0) &&
             !LMN_ATTR_IS_DATA(attr1));

  newlink_symbol_and_something(LMN_SATOM(atom0),
                               pos0,
                               LMN_SATOM_GET_LINK(LMN_SATOM(atom1), pos1),
                               LMN_SATOM_GET_ATTR(LMN_SATOM(atom1), pos1));
}

void lmn_mem_move_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem)
{
  AtomListEntry *srcent;
  LmnMembraneRef m, next;
  int dst_data_atom_n, src_data_atom_n;

  dst_data_atom_n = lmn_mem_data_atom_num(destmem);
  src_data_atom_n = lmn_mem_data_atom_num(srcmem);

  /* move atoms */
  EACH_ATOMLIST(srcmem, srcent, ({
    LmnSymbolAtomRef a, next;

    for (a  = atomlist_head((srcent));
         a != lmn_atomlist_end((srcent));
         a  = next) {
      next = LMN_SATOM_GET_NEXT_RAW(a);

#ifdef USE_FIRSTCLASS_RULE
      if (LMN_SATOM_GET_FUNCTOR(a) == LMN_COLON_MINUS_FUNCTOR) {
        LmnRuleSetRef rs = firstclass_ruleset_lookup(a);
        lmn_mem_remove_firstclass_ruleset(srcmem, rs);
        lmn_mem_add_firstclass_ruleset(destmem, rs);
      }
#endif

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

  if (src_data_atom_n > lmn_mem_data_atom_num(destmem) - dst_data_atom_n) {
    lmn_mem_data_atom_set(destmem, dst_data_atom_n + src_data_atom_n);
  }
}

//#define REMOVE            1
//#define STATE(ATOM)        (LMN_SATOM_GET_ATTR((ATOM), 2))
//#define SET_STATE(ATOM,S)  (LMN_SATOM_SET_ATTR((ATOM), 2, (S)))

/* cf. Java処理系
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 *
 * 2011/01/23  処理を変更 meguro
 */
void lmn_mem_remove_proxies(LmnMembraneRef mem)
{
  struct Vector remove_list_p, remove_list_m, change_list;
  AtomListEntry *ent;
  unsigned int i;

  ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);

  vec_init(&remove_list_p, 16); /* parent用 */
  vec_init(&remove_list_m, 16); /* mem用 */
  vec_init(&change_list, 16);

  if (ent) {
    LmnSymbolAtomRef ipxy;

    EACH_ATOM(ipxy, ent, ({
      LmnSymbolAtomRef a0, a1;
      LmnFunctor f0, f1;

      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(ipxy, 1))) {
        a0 = LMN_SATOM(LMN_SATOM_GET_LINK(ipxy, 1));
        f0 = LMN_SATOM_GET_FUNCTOR(a0);

        if (f0 == LMN_STAR_PROXY_FUNCTOR) {
          /* -$*-$in- → ----- */
          lmn_mem_unify_atom_args(mem, a0, 0, ipxy, 0);
          vec_push(&remove_list_m, (LmnWord)a0);
          vec_push(&remove_list_m, (LmnWord)ipxy);
        } else {
          /* -$in- → -$*- */
          vec_push(&change_list, (LmnWord)ipxy);
        }
      }
      else {
        /* -$in- → -$*- */
        vec_push(&change_list, (LmnWord)ipxy);
      }

      a1 = LMN_SATOM(LMN_SATOM_GET_LINK(ipxy, 0));
      f1 = LMN_SATOM_GET_FUNCTOR(a1);

      if (f1 == LMN_OUT_PROXY_FUNCTOR) {
        if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(a1, 1))) {
          LmnSymbolAtomRef a2;
          LmnFunctor f2;

          a2 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, 1));
          f2 = LMN_SATOM_GET_FUNCTOR(a2);

          if (f2 == LMN_STAR_PROXY_FUNCTOR) {
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
void lmn_mem_insert_proxies(LmnMembraneRef mem, LmnMembraneRef child_mem)
{
  unsigned int i;
  Vector remove_list, change_list;
  LmnSymbolAtomRef star, oldstar;
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
        LmnSymbolAtomRef outside = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        LmnSymbolAtomRef newstar = lmn_mem_newatom(mem, LMN_STAR_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(outside, 1, newstar, 1);
        lmn_mem_relink_atom_args(mem,
                                 newstar,
                                 LMN_ATTR_MAKE_LINK(0),
                                 0,
                                 star,
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
void lmn_mem_remove_temporary_proxies(LmnMembraneRef mem)
{
  unsigned int i;
  Vector remove_list;
  LmnSymbolAtomRef star, outside;
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
void lmn_mem_remove_toplevel_proxies(LmnMembraneRef mem)
{
  Vector remove_list;
  AtomListEntry *ent;
  LmnSymbolAtomRef outside;
  unsigned int i;

  ent = lmn_mem_get_atomlist(mem, LMN_OUT_PROXY_FUNCTOR);
  if (!ent) return;

  vec_init(&remove_list, 16);

  EACH_ATOM(outside, ent, ({
    LmnSymbolAtomRef a0;
    a0 = LMN_SATOM(LMN_SATOM_GET_LINK(outside, 0));
    if (LMN_PROXY_GET_MEM(a0) &&
        LMN_PROXY_GET_MEM(a0)->parent != mem) {
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(outside, 1))) {
        LmnSymbolAtomRef a1 = LMN_SATOM(LMN_SATOM_GET_LINK(outside, 1));
        if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR) {
          LmnSymbolAtomRef a10 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, 0));
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


LmnMembraneRef lmn_mem_copy(LmnMembraneRef src)
{
  ProcessTableRef copymap;
  LmnMembraneRef copied;

  copied = lmn_mem_copy_with_map(src, &copymap);
  proc_tbl_free(copymap);
  return copied;
}

LmnMembraneRef lmn_mem_copy_ex(LmnMembraneRef src)
{
  ProcessTableRef copymap;
  LmnMembraneRef copied;

  copied = lmn_mem_copy_with_map_ex(src, &copymap);
  proc_tbl_free(copymap);
  return copied;
}

static inline
LmnMembraneRef lmn_mem_copy_with_map_inner(LmnMembraneRef src,
                                         ProcessTableRef  *ret_copymap,
                                         BOOL        hl_nd)
{
  unsigned int i;
  ProcessTableRef copymap;
  LmnMembraneRef new_mem;

  /* (ueda, 2013-09-21) コミット前に newhlink で作られたハイパーリンク
   * のIDとの衝突を避けるために，グローバルルート膜のコピー時にIDのリセ
   * ットをしないで新たな膜やアトムやハイパーリンクを作ってゆく．
   * 本来は，ハイパーリンクの作成 (new) をルールのガードで行うのでなく，
   * コミット直後に作成するほうが論理的に望ましく，こうすればID衝突問題
   * は起きないが，コンパイラ改訂までは次の１行をコメントアウトすること
   * で対処．*/
  //  env_reset_proc_ids();  

  new_mem = lmn_mem_make();

  if (hl_nd) {
    copymap = lmn_mem_copy_cells_ex(new_mem, src, TRUE);
  } else {
    copymap = lmn_mem_copy_cells(new_mem, src);
  }

  for (i = 0; i < src->rulesets.num; i++) {
    vec_push(&new_mem->rulesets,
        (LmnWord)lmn_ruleset_copy((LmnRuleSetRef)vec_get(&src->rulesets, i)));
  }
  *ret_copymap = copymap;

  return new_mem;
}

LmnMembraneRef lmn_mem_copy_with_map_ex(LmnMembraneRef src, ProcessTableRef *ret_copymap)
{
  return lmn_mem_copy_with_map_inner(src, ret_copymap, TRUE);
}

LmnMembraneRef lmn_mem_copy_with_map(LmnMembraneRef src, ProcessTableRef *ret_copymap)
{
  return lmn_mem_copy_with_map_inner(src, ret_copymap, FALSE);
}


ProcessTableRef lmn_mem_copy_cells_ex(LmnMembraneRef dst,
                                        LmnMembraneRef src,
                                        BOOL        hl_nd)
{
  ProcessTableRef atoms = proc_tbl_make_with_size(64);
  lmn_mem_copy_cells_sub(dst, src, atoms, hl_nd);
  return atoms;
}

ProcessTableRef lmn_mem_copy_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem)
{
  return lmn_mem_copy_cells_ex(destmem, srcmem, FALSE);
}

/* srcmemの実データ構造をdestmemへコピー生成する. atomsは訪問済みの管理に用いる.
 * hl_ndフラグが真の場合, コピー前とコピー後のハイパーリンクのunifyをしない.  */
static void lmn_mem_copy_cells_sub(LmnMembraneRef destmem,
                                  LmnMembraneRef  srcmem,
                                  ProcessTableRef   atoms,
                                  BOOL         hl_nd)
{
  unsigned int i;
  LmnMembraneRef m;
  AtomListEntry *ent;

  /* copy child mems */
  for (m = srcmem->child_head; m; m = m->next) {
    LmnMembraneRef new_mem = lmn_mem_make();
    lmn_mem_copy_cells_sub(new_mem, m, atoms, hl_nd);
    lmn_mem_add_child_mem(destmem, new_mem);

    proc_tbl_put_mem(atoms, m, (LmnWord)new_mem);
    /* copy name */
    new_mem->name = m->name;
    /* copy rulesets */
    for (i = 0; i < m->rulesets.num; i++) {
      vec_push(&new_mem->rulesets,
               (LmnWord)lmn_ruleset_copy((LmnRuleSetRef)vec_get(&m->rulesets, i)));
    }
  }

  /* copy atoms */
  EACH_ATOMLIST(srcmem, ent, ({
    LmnSymbolAtomRef srcatom;
    LmnWord t = 0;

    EACH_ATOM(srcatom, ent, ({
      LmnSymbolAtomRef newatom;
      unsigned int start, end;
      LmnFunctor f;

      LMN_ASSERT(LMN_SATOM_ID(srcatom) > 0);
      if (proc_tbl_get_by_atom(atoms, srcatom, NULL) || LMN_IS_HL(srcatom)) {
        continue;
      }

      f       = LMN_SATOM_GET_FUNCTOR(srcatom);
      newatom = lmn_mem_newatom(destmem, f);

      proc_tbl_put_atom(atoms, srcatom, (LmnWord)newatom);

      start = 0;
      end   = LMN_SATOM_GET_ARITY(srcatom);

      if (LMN_IS_PROXY_FUNCTOR(f)) {
        start = 1;
        end   = 2;
        LMN_PROXY_SET_MEM(newatom, destmem);

        if (f == LMN_OUT_PROXY_FUNCTOR) {
          LmnSymbolAtomRef srcinside;
          LmnSymbolAtomRef newinside;

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
        LmnAtomRef a = LMN_SATOM_GET_LINK(srcatom, i);
        if (LMN_ATTR_IS_DATA(attr)) {
          LmnAtomRef newargatom;

          if (LMN_ATTR_IS_HL(attr) && hl_nd) {
            /* unifyせずにコピーする */
            HyperLink *newhl, *orihl;
            LmnAtomRef ori_attr_atom;
            LmnLinkAttr ori_attr;

            orihl         = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)a);
            ori_attr_atom = LMN_HL_ATTRATOM(orihl);
            ori_attr      = LMN_HL_ATTRATOM_ATTR(orihl);
            newargatom    = (LmnAtomRef)lmn_hyperlink_new_with_attr(ori_attr_atom, ori_attr);
            newhl         = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)newargatom);

            /* 子への接続 */
            if (orihl->children) {
              HashSetIterator it;
              for (it = hashset_iterator(orihl->children);
                   !hashsetiter_isend(&it);
                   hashsetiter_next(&it)) {
                HyperLink *hl = (HyperLink *)hashsetiter_entry(&it);
                if (proc_tbl_get_by_hlink(atoms, hl, &t)) {
                  hyperlink_unify(newhl, (HyperLink *)t, ori_attr_atom, ori_attr);
                }
              }
            }

            /* 親への接続 */
            if (orihl->parent &&
                proc_tbl_get_by_hlink(atoms, orihl->parent, &t)) {
              hyperlink_unify((HyperLink *)t, newhl, ori_attr_atom, ori_attr);
            }
            proc_tbl_put_new_hlink(atoms, orihl, (LmnWord)newhl);
          }
          else {
            newargatom = (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)a, attr);
          }

          if (LMN_ATTR_IS_HL(attr)) {
            lmn_mem_newlink(destmem, (LmnAtomRef)newatom,
                            LMN_ATTR_GET_VALUE((LmnWord)newatom), i,
                            newargatom, LMN_HL_ATTR, 0);
          } else {
            newlink_symbol_and_something(newatom, i, newargatom, attr);
          }

          lmn_mem_push_atom(destmem, newargatom, attr);
        } else if (proc_tbl_get_by_atom(atoms, LMN_SATOM(a), &t)) {
          newlink_symbol_and_something(newatom, i, (LmnAtomRef)t, attr);
        }
      }
    }));
  }));

  /* copy activated flag */
  destmem->is_activated = vec_num(&destmem->rulesets) || srcmem->is_activated;
}

struct LinkObj {
  LmnAtomRef ap;
  LmnLinkAttr pos;
};

LmnAtomRef LinkObjGetAtom(LinkObjRef o) {
  return o->ap;
}
LmnLinkAttr LinkObjGetPos(LinkObjRef o) {
  return o->pos;
}
LinkObjRef LinkObj_make(LmnAtomRef ap, LmnLinkAttr pos)
{
  LinkObjRef ret = LMN_MALLOC(struct LinkObj);
  ret->ap = ap;
  ret->pos = pos;
  return ret;
}


/* lmn_copy_ground_subで使う関数
 * root_hlAtomのハイパーリンクにつながるアトム達をコピーする
 * root_hlAtomは探索を行うハイパーリンクのハイパーリンクアトム
 * copied_root_hlAtomはroot_hlAtomのコピー。
 * stackは探索待ちのシンボルアトム
 * atommapはコピー元とコピー先のアトムの対応
 * hlinkmapはコピー元とコピー先のハイパーリンクの対応
 * attr_functorsはハイパーリンクの属性（シンボルアトムの場合）
 * attr_dataAtomsはハイパーリンクの属性（データアトム）
 * attr_dataAtom_attrsはハイパーリンクの属性（データアトムの属性) 
 */ 
static inline void mem_map_hlink(LmnMembraneRef mem,
                             LmnSymbolAtomRef root_hlAtom,
                             LmnSymbolAtomRef copied_root_hlAtom,
                             Vector *stack,
                             ProcessTableRef *global_hlinks,
                             ProcessTableRef atommap,
                             ProcessTableRef hlinkmap,
                             ProcessTableRef *attr_functors,
                             Vector *attr_dataAtoms,
                             Vector *attr_dataAtom_attrs)

{
  LmnWord t=0; 
  HyperLink *hl = lmn_hyperlink_at_to_hl(root_hlAtom);
  BOOL flg_search_hl=FALSE;

  BOOL flg_local_hl=TRUE;
  if(proc_tbl_get_by_hlink(*global_hlinks, lmn_hyperlink_get_root(hl), NULL))
  {//this hyperlink is a global hyperlink
	  flg_local_hl=FALSE;
  }

  if (!LMN_HL_HAS_ATTR(hl)) //if hlink has no attribute
  {
	  if(vec_num(attr_dataAtoms) == 0)  //hlground has no attribute
	  {// matched (no attribute vs no attribute)
		  flg_search_hl=TRUE;
	  }
  }
  

  {
    LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
    LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
    int i;

    for (i = 0; i < vec_num(attr_dataAtoms); i++) 
    {
        if (lmn_eq_func(attrAtom, attr, (LmnAtomRef)vec_get(attr_dataAtoms, i), vec_get(attr_dataAtom_attrs, i))) {
          flg_search_hl = TRUE;
          break;
        } 
        else 
        {
          continue;
        }
    }

  }

  if (flg_search_hl  && flg_local_hl)
  {
    if (!proc_tbl_get_by_hlink(hlinkmap, lmn_hyperlink_get_root(hl), &t)) {//同じハイパーリンクが接続されたアトムがスタックに積まれてる場合がある
      int j, element_num;
      proc_tbl_put_new_hlink(hlinkmap, lmn_hyperlink_get_root(hl), (LmnWord)(lmn_hyperlink_at_to_hl(copied_root_hlAtom)));
      Vector *hl_childs = vec_make(16);
      lmn_hyperlink_get_elements(hl_childs, hl);
      element_num = vec_num(hl_childs) - 1;
      for (j = 0; j < element_num; j++) {//ハイパーリンクにつながるすべての接続先を探索
        if (hl->mem!=((HyperLink *)vec_get(hl_childs, j))->mem) {//root_hlAtomの所属膜と異なる膜内はコピーしない
          continue;
        }
        LmnSymbolAtomRef hlAtom = ((HyperLink *)vec_get(hl_childs, j))->atom;
        if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(hlAtom, 0))) {
          LmnSymbolAtomRef copied_hlAtom = lmn_copy_satom_with_data((hlAtom), FALSE);
          lmn_hyperlink_copy(copied_hlAtom, copied_root_hlAtom);
          lmn_mem_push_atom(mem, copied_hlAtom, LMN_HL_ATTR);
        } else {
          LmnSymbolAtomRef linked_hlAtom = LMN_SATOM_GET_LINK(hlAtom, 0);
          if (!proc_tbl_get_by_atom(atommap, linked_hlAtom, &t)) {//ハイパーリンクアトム及びそれにつながるシンボルアトムをコピー
	    // まずシンボルアトムをコピー
            LmnSymbolAtomRef copied_linked_hlAtom = lmn_copy_satom_with_data(linked_hlAtom, TRUE);
            LmnSymbolAtomRef copied_hlAtom = LMN_SATOM_GET_LINK(copied_linked_hlAtom, LMN_SATOM_GET_ATTR(hlAtom, 0));
	    // ここではハイパーリンク構造体は未作成、lmn_hyperlink_copyで作成
            lmn_hyperlink_copy(copied_hlAtom, copied_root_hlAtom);
	    // ここで作られた copied_hlAtom はあとでシンボルアトム側から逆アクセスされるはずなので
	    // ここでは push しなくてよい（はず）
	    //            lmn_mem_push_atom(mem, copied_hlAtom, LMN_HL_ATTR);
            mem_push_symbol_atom(mem, copied_linked_hlAtom);
            proc_tbl_put_atom(atommap, linked_hlAtom, (LmnWord)copied_linked_hlAtom);
            vec_push(stack, (LmnWord)linked_hlAtom);
          } else {//つまり既にスタックに積まれている場合
            //ハイパーリンクへの接続は、スタックに積まれているアトム側からの探索時に行う。
	    // そのとき、アトム側からたどったハイパーリンクはまだ atomlist に登録されて
	    // いないはず。新たに atomlist に登録する必要がある。
          }
        }
      }
      vec_free(hl_childs);
    } else {//既にハイパーリンクをコピーしていればunify
      lmn_hyperlink_unify(lmn_hyperlink_at_to_hl(copied_root_hlAtom),
          (HyperLink *)t,
          LMN_HL_ATTRATOM((HyperLink *)t),
          LMN_HL_ATTRATOM_ATTR((HyperLink *)t));
    }
  } else {
    lmn_hyperlink_unify(hl, lmn_hyperlink_at_to_hl(copied_root_hlAtom), LMN_HL_ATTRATOM(hl), LMN_HL_ATTRATOM_ATTR(hl));
  }
  lmn_mem_push_atom(mem, copied_root_hlAtom, LMN_HL_ATTR);
}


/* 膜memのsrcvecを根に持つgroundプロセスをコピーする.
 * srcvecはリンクオブジェクトのベクタ.
 * ret_dstlovecはコピーされた根のリンクオブジェクトのベクタ.
 * ret_atommapはコピー元とコピー先のアトムの対応
 * ret_hlinkmapはハイパーリンクのコピー元と先であり
 * hlgroundのフラグも兼ねている */
static inline void mem_copy_ground_sub(LmnMembraneRef mem,
                             Vector *srcvec,
                             ProcessTableRef *global_hlink,
                             Vector **ret_dstlovec,
                             ProcessTableRef *ret_atommap,
                             ProcessTableRef *ret_hlinkmap,
                             ProcessTableRef *attr_functors,
                             Vector *attr_dataAtoms,
                             Vector *attr_dataAtom_attrs)
{
  //printf("----------------------mem_copy_ground_sub-----------------------  \n");
  //fflush(stdout);

  ProcessTableRef atommap;
  ProcessTableRef hlinkmap;
  Vector *stack;
  unsigned int i;
  LmnWord t = 0;

  atommap = proc_tbl_make_with_size(64);
  hlinkmap = proc_tbl_make_with_size(64);
  stack = vec_make(16);
  *ret_dstlovec = vec_make(16);

  /* 根をスタックに積む.
   * スタックにはリンクオブジェクトではなくアトムを積むため,
   * ここで根の先のアトムをコピーしスタックに積む必要がある */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    LmnAtomRef cpatom;

    if (LMN_ATTR_IS_DATA(l->pos)) {
      if (ret_hlinkmap != NULL && l->pos == LMN_HL_ATTR) {//hlgroundならハイパーリンクの先を辿ってコピーする
        cpatom = (LmnAtomRef)lmn_hyperlink_new_with_attr(LMN_HL_ATTRATOM(lmn_hyperlink_at_to_hl(LMN_SATOM(l->ap))),
                                             LMN_HL_ATTRATOM_ATTR(lmn_hyperlink_at_to_hl(LMN_SATOM(l->ap))));
        mem_map_hlink(mem, LMN_SATOM(l->ap),
                                     LMN_SATOM(cpatom),
                                     stack,
                                     global_hlink,
                                     atommap,
                                     hlinkmap,
                                     attr_functors,
                                     attr_dataAtoms,
                                     attr_dataAtom_attrs);
      } else {
        cpatom = (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)l->ap, l->pos);
        lmn_mem_push_atom(mem, cpatom, l->pos);
      }
    }
    else { /* symbol atom */
      /* コピー済みでなければコピーする */
      if (!proc_tbl_get_by_atom(atommap, l->ap, &t)) {
        if (ret_hlinkmap != NULL) {//hlgroundなら
          cpatom = lmn_copy_satom_with_data(l->ap, TRUE);
        } else {
          cpatom = lmn_copy_satom_with_data(l->ap, FALSE);
        }

        mem_push_symbol_atom(mem, cpatom);
        proc_tbl_put_atom(atommap, l->ap, (LmnWord)cpatom);

        /* 根のリンクのリンクポインタを0に設定する */
        LMN_SATOM_SET_LINK(cpatom, l->pos, 0);
        vec_push(stack, (LmnWord)l->ap);
      } else {
        /* コピー済みの場合はスタックには追加しない */
        cpatom = (LmnAtomRef)t;
        LMN_SATOM_SET_LINK(cpatom, l->pos, 0);
      }
    }
    vec_push(*ret_dstlovec, (vec_data_t)LinkObj_make(cpatom, l->pos));
  }

  while (!vec_is_empty(stack)) {
    LmnSymbolAtomRef src_atom, copied;

    src_atom = (LmnSymbolAtomRef)vec_pop(stack);
    proc_tbl_get_by_atom(atommap, src_atom, &t);
    copied = (LmnSymbolAtomRef)t;

    for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
      LmnAtomRef     next_src  = LMN_SATOM_GET_LINK(src_atom, i);
      LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

      /* LMN_SATOM_GET_LINK(copied, i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        if (ret_hlinkmap != NULL && next_attr == LMN_HL_ATTR) {//hlgroundならハイパーリンクの先を辿ってコピーする
          mem_map_hlink(mem,
                        LMN_SATOM(next_src),
                        LMN_SATOM(LMN_SATOM_GET_LINK(copied, i)),
                        stack,
                        global_hlink,
                        atommap,
                        hlinkmap,
                        attr_functors,
                        attr_dataAtoms,
                        attr_dataAtom_attrs);
        } else {
          if (next_attr == LMN_HL_ATTR) {
            LmnAtomRef next_src_copied = LMN_SATOM_GET_LINK(copied, i);
            lmn_mem_push_atom(mem, next_src_copied, next_attr);
          } else {
            lmn_mem_push_atom(mem, next_src, next_attr);
          }
        }
      }
      else if (LMN_SATOM_GET_LINK(copied, i) != 0) {
        LmnAtomRef next_copied;
        if (!proc_tbl_get_by_atom(atommap, next_src, &t)) { /* next_srcは未訪問 */
          if (ret_hlinkmap != NULL) {//hlgroundなら
            next_copied = lmn_copy_satom_with_data(next_src, TRUE);
          } else {
            next_copied = lmn_copy_satom_with_data(next_src, FALSE);
          }
          mem_push_symbol_atom(mem, next_copied);
          proc_tbl_put_atom(atommap, next_src, (LmnWord)next_copied);
          vec_push(stack, (LmnWord)next_src);
        } else {
          next_copied = (LmnAtomRef)t;
        }
        LMN_SATOM_SET_LINK(copied, i, next_copied);
      }
    }
  }

  vec_free(stack);
  *ret_atommap = atommap;
  if (ret_hlinkmap != NULL) {
    *ret_hlinkmap = hlinkmap;
  }
}

void lmn_mem_copy_ground( LmnMembraneRef mem,
                          Vector *srcvec,
                          Vector **ret_dstlovec,
                          ProcessTableRef *ret_atommap,
                          ProcessTableRef *ret_hlinkmap,
                          ProcessTableRef *attr_functors,
                          Vector *attr_dataAtoms,
                          Vector *attr_dataAtom_attrs)
{
  ProcessTableRef hlinks=hlground_data.global_hlinks;

	mem_copy_ground_sub(mem,
                      srcvec,
					            &hlinks,
                      ret_dstlovec,
                      ret_atommap,
                      ret_hlinkmap,
                      attr_functors,
                      attr_dataAtoms,
                      attr_dataAtom_attrs);
  //mem_copy_ground_sub(mem, srcvec,NULL, ret_dstlovec, ret_atommap, NULL, NULL, NULL, NULL);
}

void lmn_mem_copy_hlground(LmnMembraneRef mem,
                             Vector *srcvec,
                             Vector **ret_dstlovec,
                             ProcessTableRef *ret_atommap,
                             ProcessTableRef *ret_hlinkmap,
                             ProcessTableRef *attr_functors,
                             Vector *attr_dataAtoms,
                             Vector *attr_dataAtom_attrs)
{
  mem_copy_ground_sub(mem,
                      srcvec,
                      NULL,
                      ret_dstlovec,
                      ret_atommap,
                      ret_hlinkmap,
                      attr_functors,
                      attr_dataAtoms,
                      attr_dataAtom_attrs);
}

/* srcvec,dstvecは比較元,比較先groundの明示的自由リンクLinkObj.
 * ground検査はすんでいるものとする. srcとdstが同じ形なら真を返す.
 *
 * TODO: 構造化 */
BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec)
{
  unsigned int i, j;
  BOOL ret_flag = TRUE;
  Vector stack1, stack2;
  SimpleHashtbl map; /* 比較元->比較先 */
  LinkObjRef start1, start2;

  hashtbl_init(&map, 256);

  vec_init(&stack1, 16);
  vec_init(&stack2, 16);

  /* startはstackにつまれるので処理中に壊されるためコピー */
  start1 = LinkObj_make(((LinkObjRef)vec_get(srcvec,0))->ap,
                        ((LinkObjRef)vec_get(srcvec,0))->pos);
  start2 = LinkObj_make(((LinkObjRef)vec_get(dstvec,0))->ap,
                        ((LinkObjRef)vec_get(dstvec,0))->pos);

  if (!LMN_ATTR_IS_DATA(start1->pos) && !LMN_ATTR_IS_DATA(start2->pos)) {
    /* ともにシンボルアトムの場合 */
    vec_push(&stack1, (LmnWord)start1);
    vec_push(&stack2, (LmnWord)start2);
  }
  else { /* data atom は積まない */
    if (!lmn_data_atom_eq((LmnDataAtomRef)start1->ap, start1->pos, (LmnDataAtomRef)start2->ap, start2->pos)) {
      ret_flag = FALSE;
    }
    LMN_FREE(start1);
    LMN_FREE(start2);
  }

  while (!vec_is_empty(&stack1)) { /* main loop: start */
    LinkObjRef l1, l2;
    BOOL contains1, contains2;

    l1 = (LinkObjRef )vec_pop(&stack1);
    l2 = (LinkObjRef )vec_pop(&stack2);
    contains1 = FALSE;
    contains2 = FALSE;

    for (i = 0; i < srcvec->num; i++) {
      LinkObjRef lobj = (LinkObjRef)vec_get(srcvec, i);
      if (l1->ap == LMN_SATOM_GET_LINK(lobj->ap, lobj->pos)
          && l1->pos == LMN_SATOM_GET_ATTR(lobj->ap, lobj->pos)) {
        contains1 = TRUE;
        break;
      }
    }
    for (j = 0; j < dstvec->num; j++) {
      LinkObjRef lobj = (LinkObjRef)vec_get(dstvec, j);
      if (l2->ap == LMN_SATOM_GET_LINK(lobj->ap, lobj->pos)
          && l2->pos == LMN_SATOM_GET_ATTR(lobj->ap, lobj->pos)) {
        contains2 = TRUE;
        break;
      }
    }
    if (i != j) { /* 根の位置が違う */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }
    if (contains1) { /* 根に到達した場合 */
      LMN_FREE(l1); LMN_FREE(l2);
      continue;
    }

    if (l1->pos != l2->pos) { /* 引数検査 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if (LMN_SATOM_GET_FUNCTOR(l1->ap) != LMN_SATOM_GET_FUNCTOR(l2->ap)) {
      /* ファンクタ検査 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if (!hashtbl_contains(&map, (HashKeyType)l1->ap)) {
      /* 未出 */
      hashtbl_put(&map, (HashKeyType)l1->ap, (HashValueType)l2->ap);
    }
    else if ((LmnAtomRef)hashtbl_get(&map, (HashKeyType)l1->ap) != l2->ap) {
      /* 既出で不一致 */
      LMN_FREE(l1); LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }
    else {
      /* 既出で一致 */
      continue;
    }

    for (i = 0; i < LMN_SATOM_GET_ARITY(l1->ap); i++) {
      LinkObjRef n1, n2;
      if (i == l1->pos) continue;
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(l1->ap, i)) &&
          !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(l1->ap, i))) {
        n1 = LinkObj_make(LMN_SATOM_GET_LINK(l1->ap, i),
                          LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(l1->ap, i)));
        n2 = LinkObj_make(LMN_SATOM_GET_LINK(l2->ap, i),
                          LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(l2->ap, i)));
        vec_push(&stack1, (LmnWord)n1);
        vec_push(&stack2, (LmnWord)n2);
      }
      else { /* data atom は積まない */
        if (!lmn_data_atom_eq((LmnDataAtomRef)LMN_SATOM_GET_LINK(l1->ap, i),
                              LMN_SATOM_GET_ATTR(l1->ap, i),
                              (LmnDataAtomRef)LMN_SATOM_GET_LINK(l2->ap, i),
                              LMN_SATOM_GET_ATTR(l2->ap, i))) {
          LMN_FREE(l1);
          LMN_FREE(l2);
          ret_flag = FALSE;
          goto CMPGROUND_BREAK;
        }
      }
    }

    LMN_FREE(l1);
    LMN_FREE(l2);
  } /* main loop: end */

CMPGROUND_BREAK:
  for (i = 0; i < vec_num(&stack1); i++) LMN_FREE((LinkObjRef)vec_get(&stack1, i));
  for (i = 0; i < vec_num(&stack2); i++) LMN_FREE((LinkObjRef)vec_get(&stack2, i));
  vec_destroy(&stack1);
  vec_destroy(&stack2);
  hashtbl_destroy(&map);

  return ret_flag;
}

/* srcvecのリンクの列が基底項プロセスに到達(avovecのリンクに到達する場
   合は基底項プロセスではない)している場合、真を返し、natomsに基底項プ
   ロセスないのアトムの数を格納する。*/
//BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms)
BOOL lmn_mem_is_ground(Vector *srcvec,
                         Vector *avovec,
                         unsigned long *natoms,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs)
{

  BOOL b;
  b = extended_ground_atoms(srcvec, avovec, NULL, natoms, NULL, attr_functors, attr_dataAtoms, attr_dataAtom_attrs);

  if(!b)
  {
	  proc_tbl_free(hlground_data.local_atoms);
	  proc_tbl_free(hlground_data.global_hlinks);
  }

  /*
  ProcessTableRef atoms;
  BOOL b;

  b = ground_atoms(srcvec, avovec, &atoms, natoms, NULL, NULL, NULL, NULL);

  if (b) {
    proc_tbl_free(atoms);
  }
  */
  return b;
}

/* hlground版。*/
BOOL lmn_mem_is_hlground(Vector *srcvec,
                         Vector *avovec,
                         unsigned long *natoms,
                         ProcessTableRef *attr_functors,
                         Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs)
{
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  BOOL b;

  b = ground_atoms(srcvec, avovec, &atoms, natoms, &hlinks, attr_functors, attr_dataAtoms, attr_dataAtom_attrs);

  if (b) {
    proc_tbl_free(atoms);
    proc_tbl_free(hlinks);
  }

  return b;
}

/* xとyが1つのリンクの逆向き表現かどうか */
#define IS_BUDDY(xap, xattr, yap, yattr)                                       \
      ( !LMN_ATTR_IS_DATA(xattr)                &&                             \
        !LMN_ATTR_IS_DATA(yattr)                &&                             \
        (LMN_SATOM_GET_LINK(xap, xattr) == yap) &&                             \
        (LMN_SATOM_GET_ATTR(xap, xattr) == yattr) )

/* srcvecから出るリンクのリストが基底項プロセスに到達している場合、
 * avovecに基底項プロセスに存在するシンボルアトム、natomsにアトムの数、戻り値に真を返す。
 * リストが基底項プロセスに到達していない場合にはatomsはNULLとなり、偽を返す。
 * ただし、リンクがavovecに到達している場合には、基底項プロセスとはならない。
 * hlinksがNULLでなければhlgroundとして探索 
 * attr_functors, attr_dataAtoms, attr_dataAtoms_attrはhlgroundの属性 */
BOOL ground_atoms(Vector        *srcvec,
                  Vector        *avovec,
                  ProcessTableRef    *atoms,/* ground内の発見済みのシンボルアトム */
                  unsigned long *natoms,/* ground内のアトムの個数 */
                  ProcessTableRef    *hlinks,/* hlinks!=NULLなら、hlgroundとして探索 */
                  ProcessTableRef    *attr_functors,/* hlgroundの属性（unary atom）*/
                  Vector        *attr_dataAtoms,/* hlgroundの属性（data atom）*/
                  Vector        *attr_dataAtom_attrs/* hlgroundの属性(data atomの属性) */
                  )
{

  Vector *unsearched_link_stack;         /* 探索待ちリンク */
  //ProcessTbl found_ground_symbol_atoms;  /* ground内の発見済みのシンボルアトム */
  //unsigned long count_of_ground_atoms;   /* ground内のアトムの個数 */
  int i;
  int reached_root_count; /* 到達した根の個数(1つは始点) */
  BOOL result;
  HyperLink *hl;
  LmnMembraneRef mem;
  Vector *hl_childs;
  int element_num;

  *natoms = 0;
  *atoms = proc_tbl_make_with_size(64);
  unsearched_link_stack = vec_make(16);
  reached_root_count = 1;
  //found_ground_symbol_atoms = proc_tbl_make_with_size(64);
  result = TRUE;
  //count_of_ground_atoms = 0;

  if (hlinks!=NULL) {
    *hlinks = proc_tbl_make_with_size(64);
  }

  /* groundはつながったグラフなので1つの根からだけたどればよい */
  {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, 0);
    vec_push(unsearched_link_stack, (LmnWord)LinkObj_make(l->ap, l->pos));
  }

  while (!vec_is_empty(unsearched_link_stack)) {
    LinkObjRef l;
    LmnAtomRef l_ap;
    LmnLinkAttr l_pos;

    l = (LinkObjRef)vec_pop(unsearched_link_stack);
    l_ap = l->ap;
    l_pos = l->pos;

    LMN_FREE(l);

    if (LMN_ATTR_IS_DATA(l_pos)) { /* lがデータなら行き止まり 
                                      hlgroundの場合はハイパーリンクアトムを探索*/
      if (lmn_data_atom_is_ground((LmnDataAtomRef)l_ap, l_pos, hlinks)) {
        if (l_pos == LMN_HL_ATTR) {
          hl = lmn_hyperlink_at_to_hl(LMN_SATOM(l_ap));
          if (hlinks != NULL && !proc_tbl_get_by_hlink(*hlinks, lmn_hyperlink_get_root(hl), NULL)) {
            BOOL flg_search_hl = FALSE;
            BOOL continue_flag = FALSE; 
            /* lがavovecにつながっていれば */
            for (i = 0; avovec != NULL && i < vec_num(avovec); i++) {
              LinkObjRef a = (LinkObjRef)vec_get(avovec, i);
              if (IS_BUDDY(l_ap, 0, a->ap, a->pos)) {
                result = FALSE;
                goto returning;
              }
            }

            /* lがsrcvecにつながっていれば */
            for (i = 0; i < vec_num(srcvec); i++) {
              LinkObjRef a = (LinkObjRef)vec_get(srcvec, i);
              if (IS_BUDDY(l_ap, 0, a->ap, a->pos)) {
                reached_root_count++;
                continue_flag = TRUE;
                break;
              }
            }
            if (!continue_flag) {
              if (!LMN_HL_HAS_ATTR(hl)) {//属性を持っていない場合は無条件に探索
                flg_search_hl = TRUE;
              } else {
                LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
                LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
                if (LMN_ATTR_IS_DATA(attr)) {
                  for (i = 0; i < vec_num(attr_dataAtoms); i++) {
                    if (lmn_eq_func(attrAtom, attr, (LmnAtomRef)vec_get(attr_dataAtoms, i), vec_get(attr_dataAtom_attrs, i))) {
                      flg_search_hl = TRUE;
                      break;
                    } else {
                      continue;
                    }
                  }
                } else {
                  if (proc_tbl_get(*attr_functors, LMN_SATOM_GET_FUNCTOR(LMN_SATOM(attrAtom)), NULL)) {
                    flg_search_hl = TRUE;
                  }
                } 
              }
            }

            if (flg_search_hl) {
              proc_tbl_put_new_hlink(*hlinks, lmn_hyperlink_get_root(hl), (LmnWord)hl);
              hl_childs = vec_make(16);

              lmn_hyperlink_get_elements(hl_childs, hl);
              element_num = vec_num(hl_childs) - 1;
              mem = hl->mem;
              for (i = 0; i < element_num; i++) {
                if (mem!=((HyperLink *)vec_get(hl_childs, i))->mem) {//別の膜に移動したらFALSEに
                  // 当初していたが，ハイパーリンク接続の場合は同一膜内の構造とマッチするように変更
                  // result = FALSE;
                  // goto returning;
                  continue;
                }
                LmnSymbolAtomRef hlAtom = ((HyperLink *)vec_get(hl_childs, i))->atom;
                LmnAtomRef linked_hlAtom = LMN_SATOM_GET_LINK(hlAtom, 0);
                LmnLinkAttr linked_attr = LMN_SATOM_GET_ATTR(hlAtom, 0);
                vec_push(unsearched_link_stack,
                    (LmnWord)LinkObj_make(LMN_SATOM_GET_LINK(hlAtom, 0),
                      LMN_SATOM_GET_ATTR(hlAtom, 0)));
                if (!LMN_ATTR_IS_DATA(linked_attr)) {
                  int j;
                  /* lがavovecにつながっていれば */
                  for (j = 0; avovec != NULL && j < vec_num(avovec); j++) {
                    LinkObjRef a = (LinkObjRef)vec_get(avovec, j);
                    if (IS_BUDDY(linked_hlAtom, linked_attr, a->ap, a->pos)) {
                      result = FALSE;
                      goto returning;
                    }
                  }

                  /* lがsrcvecにつながっていれば */
                  continue_flag = FALSE;
                  for (j = 0; j < vec_num(srcvec); j++) {
                    LinkObjRef a = (LinkObjRef)vec_get(srcvec, j);
                    if (IS_BUDDY(linked_hlAtom, linked_attr, a->ap, a->pos)) {
                      reached_root_count++;
                      continue_flag = TRUE;
                      break;
                    }
                  }
                  if (continue_flag) vec_pop(unsearched_link_stack);
                }
              }
              vec_free(hl_childs);
            }
          }
        }
        (*natoms)++;
        continue;
      } else {
        result = FALSE; /* groundでないデータが出現したら終了 */
        goto returning;
      }
    } else { /* lがシンボルアトムを指していれば */

      /* lがavovecにつながっていれば */
      for (i = 0; avovec != NULL && i < vec_num(avovec); i++) {
        LinkObjRef a = (LinkObjRef)vec_get(avovec, i);
        if (IS_BUDDY(l_ap, l_pos, a->ap, a->pos)) {
          result = FALSE;
          goto returning;
        }

      }

      /* lがsrcvecにつながっていれば */
      {
        if (LMN_SATOM_GET_ATTR(l_ap, l_pos)!=LMN_HL_ATTR) {
          BOOL continue_flag = FALSE;
          for (i = 0; i < vec_num(srcvec); i++) {
            LinkObjRef a = (LinkObjRef)vec_get(srcvec, i);
            if (IS_BUDDY(l_ap, l_pos, a->ap, a->pos)) {
              reached_root_count++;
              continue_flag = TRUE;
              break;
            }
          }
        if (continue_flag) continue;
        }
      }

      /* lがプロキシを指していれば */
      if (LMN_SATOM_IS_PROXY(l_ap)) {
        result = FALSE;
        goto returning;
      }

      /* lの指すアトムが訪問済みなら */
      if (proc_tbl_get_by_atom(*atoms, (LmnSymbolAtomRef)l_ap, NULL)) {
        continue;
      }

      /* lはシンボルアトムで初出のアトムを指すため,その先を探索する必要がある */
      proc_tbl_put_atom(*atoms, (LmnSymbolAtomRef)l_ap, (LmnWord)l_ap);
      (*natoms)++;

      for (i = 0; i < LMN_SATOM_GET_ARITY(l_ap); i++) {
        if (i == l_pos) continue;
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
    //*atoms = found_ground_symbol_atoms;
    //*natoms += count_of_ground_atoms;
  } else {
    if (hlinks !=NULL) {
      proc_tbl_free(*hlinks);
      *hlinks = NULL;
    }
    proc_tbl_free(*atoms);
    *atoms = NULL;
    *natoms = 0;
  }

  return result;
}




/************extended ground begin***********/

BOOL extended_ground_atoms( Vector          *srcvec, 				//store root link
                            Vector          *avovec,   				//other links of source atom
                            ProcessTableRef *atoms,    				/* collects atom within hlground */
                            unsigned long   *natoms,   				/* number of atoms within hlground */
                            ProcessTableRef *hlinks,   				/* hlinks!=NULL縺ｪ繧峨�”lground縺ｨ縺励※謗｢邏｢ collects hyperlinks local to hlground  */
                            ProcessTableRef *attr_functors,			/* hlground縺ｮ螻樊�ｧ�ｼ�unary atom�ｼ� used when attribute is specified */
                            Vector          *attr_dataAtoms,		/* hlground縺ｮ螻樊�ｧ�ｼ�data atom�ｼ� used when attribute is specified */
                            Vector          *attr_dataAtom_attrs	/* hlground縺ｮ螻樊�ｧ(data atom縺ｮ螻樊�ｧ) used when attribute is specified */
                  )
{

  BOOL result= TRUE;		//TRUE if it is hlground
  *natoms = 0; 				//number of symbol atoms + local hyperlinks within hlground

  LinkObjRef t_link = (LinkObjRef)vec_get(srcvec, 0);
  LmnAtomRef root_ap = t_link->ap;
  LmnLinkAttr root_pos = t_link->pos;

  //printf("root_ap= %d  \n",root_ap);
  //printf("root_pos= %d  \n",root_pos);

  if(cycle_exist(srcvec,avovec,attr_functors,attr_dataAtoms,attr_dataAtom_attrs))
  {//there is at least one cycle
	  if(purecycle_exit(srcvec,avovec))
    {
		  result = FALSE;
	  }
  }

  if(result)
  { //hlground exist
      init_grounddata(); 
      dfs_scope_finder(LinkObj_make(root_ap,root_pos),
                       srcvec,
                       avovec,
                       natoms,
                       attr_functors,
                       attr_dataAtoms,
                       attr_dataAtom_attrs);
      
      //free_hlgrounddata();   //release memory immediately
      free_grounddata();
  }

  return result;
}




void init_grounddata()
{
	hlground_data.global_hlinks =proc_tbl_make_with_size(64);
	hlground_data.local_atoms=proc_tbl_make_with_size(64);
}

void free_grounddata()
{
	
}


void dfs_scope_finder(      LinkObjRef root_link,
                            Vector *src,
              							Vector *avovec,
              							unsigned long * natoms,
              							ProcessTableRef *attr_functors,
              							Vector *attr_dataAtoms,
              							Vector *attr_dataAtom_attrs)
{
	*natoms=0;
  //ProcessTbl visited_hl=proc_tbl_make_with_size(10);   //visited hlinks

  Vector *stack=vec_make(10);                          //visited atoms 
  vec_push(stack,(LmnWord)LinkObj_make(root_link->ap,root_link->pos));   //push the rootlink

  while(!vec_is_empty(stack))                                        //dfs stack
  {
    LinkObjRef cur_link=vec_pop(stack);                         
    LmnAtomRef m_atom=cur_link->ap;
    LmnLinkAttr m_pos=cur_link->pos;
    LMN_FREE(cur_link);                                              //free current link                                            

    //printf("m_app= %d  \n",m_atom);
    //printf("m_pos= %d  \n",m_pos);


    if(LMN_ATTR_IS_DATA(m_pos))                         // current link is a hyperlink
    {
      if(m_pos == LMN_HL_ATTR)
      {
        HyperLink * hl = lmn_hyperlink_at_to_hl(LMN_SATOM(m_atom));    //get hyperlink 

                                    // judge hyperlink attribute

        BOOL flg_search_hl = FALSE;  //True is hlink attribute matches with hlground attribute
                                     
        if (!LMN_HL_HAS_ATTR(hl))    //if hlink has no attribute, treat it as a global hyperlink
        {
          proc_tbl_put_new_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl), (LmnWord)1);
        }

        if(LMN_HL_HAS_ATTR(hl))     //hyperlink has attribute 
        {
          LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
          LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
          int i;
          for (i = 0; i < vec_num(attr_dataAtoms); i++)
          {
            if (lmn_eq_func(attrAtom, attr, vec_get(attr_dataAtoms, i), vec_get(attr_dataAtom_attrs, i)))
            {
              flg_search_hl = TRUE;
              break;
            }
            else
            {
              continue;
            }
          }
        }

        if (flg_search_hl)  // only count endpoints of hyperlinks which has the specified attribute
        {

          if(!proc_tbl_get_by_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl), NULL))   
          { // if not visited
            unsigned long occurs =  lmn_hyperlink_element_num(hl);  // get number of the hyperlink sublinks
            if(occurs > 1)  //so we try to visit other endpoints
            {
              proc_tbl_put_new_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl), (LmnWord)1);  //start counting endpoints
            }

          }
          else //if visited already
          {
             unsigned long occurs =  lmn_hyperlink_element_num(hl);  // get number of the hyperlink sublinks 
             LmnWord count;
             proc_tbl_get_by_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl),&count );  //get counter
             count++ ;                                                                                
             //printf("count= %d  \n",count);

             if ( occurs == count )   //all endpoints are visited, means local hyperlink
             {     //remove form hashtable
                proc_tbl_unput_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl));
             }
             else  //not all endpoints are visited
             {  
                proc_tbl_unput_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl));
                proc_tbl_put_new_hlink(hlground_data.global_hlinks, lmn_hyperlink_get_root(hl),(LmnWord)count);
             }
          }
        }


      }
      else  
      {
          printf("neither a regular link or a hyperlink: means an undefined link  \n");
      }
      (*natoms)++; // count just like previous version of hlground
    }
    else                        //current link is a regular link
    {
      if (proc_tbl_get_by_atom(hlground_data.local_atoms, (LmnSAtom)m_atom, NULL))
      { //not visited yet
        continue;
      }
      proc_tbl_put_atom(hlground_data.local_atoms, (LmnSAtom)m_atom, (LmnWord)m_atom);  //set as visited and set as an local atom
      (*natoms)++; 

      //keep following regular links
      int i;
      for (i = 0; i < LMN_SATOM_GET_ARITY(m_atom); i++)
      {
        if (i == m_pos)
        {
          if(LMN_SATOM_GET_ARITY(m_atom)==1)
          {
            //pure_path=TRUE;
            //printf(" ***    a local path is ended ***\n");
          }
          continue;
        }
        vec_push(stack,(LmnWord)LinkObj_make(LMN_SATOM_GET_LINK(m_atom, i),
                                             LMN_SATOM_GET_ATTR(m_atom, i)));

      }//end for       

    }

  } //dfs while end

  //proc_tbl_free(visited_hl);
  vec_free(stack);

}


BOOL purecycle_exit(Vector *srcvec, Vector *avovec)
{
	BOOL m_find=FALSE;

	int m=vec_num(srcvec);
	int i;
	for(i=0;i<m;i++)
	{
		Vector * stack=vec_make(10);
		LinkObjRef root = LinkObj_make(((LinkObjRef)vec_get(srcvec,i))->ap,((LinkObjRef)vec_get(srcvec,i))->pos);
		ProcessTableRef atoms=proc_tbl_make_with_size(64);
		BOOL m_first=TRUE;

		vec_push(stack,(LmnWord)LinkObj_make(root->ap,root->pos));

		while(!vec_is_empty(stack))
		{
			LinkObjRef cur_link=(LinkObjRef)vec_pop(stack);

			//printf(" cur_link =%d , %d --------\n",cur_link->ap,cur_link->pos);
			if(cur_link->ap == root->ap  && cur_link->pos ==root->pos && !m_first)
			{
				m_find= TRUE;
				break;
			}

			if(LMN_ATTR_IS_DATA(cur_link->pos))
				continue;			//only go on regular links

			 if (proc_tbl_get_by_atom(atoms, (LmnSAtom)cur_link->ap, NULL))
			 {
			        continue;
			 }

			proc_tbl_put_atom(atoms, (LmnSAtom)cur_link->ap, (LmnWord)cur_link->ap);

			Vector * neighbours = vec_make(4);
			get_neighbours(NULL,
					neighbours,
					cur_link->ap,
					cur_link->pos,
					NULL,
					NULL,
					NULL);
			LMN_FREE(cur_link);

			while(!vec_is_empty(neighbours))
			{
				LinkObjRef Link=vec_pop(neighbours);
				//printf(" next_link =%d , %d --------\n",Link->ap,Link->pos);
				vec_push(stack,(LmnWord)Link);
			}
			vec_free(neighbours);
			m_first=FALSE;
		}

		LMN_FREE(root);
		proc_tbl_free(atoms);

		if(!stack)
		{
			while(!vec_is_empty(stack) )
			{
				LinkObjRef Link=(LinkObjRef)vec_pop(stack);
				LMN_FREE(Link);
			}
			vec_free(stack);
		}
		if(m_find)
			break;
	}

	return m_find;
}


BOOL cycle_exist(   Vector *srcvec,
                    Vector *avovec,
                    ProcessTableRef  *attr_functors,
                    Vector   *attr_dataAtoms,
                    Vector   *attr_dataAtom_attrs)
{
	BOOL m_find=FALSE;

	int m=vec_num(srcvec);
	int i;
	for(i=0;i<m;i++)
	{
		Vector * stack=vec_make(10);
		LinkObjRef root = LinkObj_make(((LinkObjRef)vec_get(srcvec,i))->ap,((LinkObjRef)vec_get(srcvec,i))->pos);
		ProcessTableRef atoms=proc_tbl_make_with_size(64);
		ProcessTableRef hlinks=proc_tbl_make_with_size(64);
		BOOL m_first=TRUE;
		BOOL m_tempfind=FALSE;
		vec_push(stack,(LmnWord)LinkObj_make(root->ap,root->pos));

		while(!vec_is_empty(stack))
		{
			LinkObjRef cur_link=(LinkObjRef)vec_pop(stack);
			//printf(" cur_link =%d , %d --------\n",cur_link->ap,cur_link->pos);
			if(cur_link->ap == root->ap  && cur_link->pos ==root->pos && !m_first )
			{
				m_find= TRUE;
				//printf(" find a cycle --------\n");
				break;
			}

			if(LMN_ATTR_IS_DATA(cur_link->pos))
			{
				HyperLink *hl = lmn_hyperlink_at_to_hl(LMN_SATOM(cur_link->ap));
				if(proc_tbl_get_by_hlink(hlinks, lmn_hyperlink_get_root(hl), NULL))
					continue;
				else
					proc_tbl_put_new_hlink(hlinks, lmn_hyperlink_get_root(hl), (LmnWord)hl);
			}
			else
			{
				if (proc_tbl_get_by_atom(atoms, (LmnSAtom)cur_link->ap, NULL))
					continue;
				else
					proc_tbl_put_atom(atoms, (LmnSAtom)cur_link->ap, (LmnWord)cur_link->ap);
			}



			Vector * neighbours = vec_make(4);
			get_neighbours(NULL,
						neighbours,
						cur_link->ap,
						cur_link->pos,
						attr_functors,
						attr_dataAtoms,
						attr_dataAtom_attrs);
			LMN_FREE(cur_link);

			while(!vec_is_empty(neighbours))
			{
				LinkObjRef Link=vec_pop(neighbours);
				vec_push(stack,(LmnWord)Link);
			}
			vec_free(neighbours);
			m_first=FALSE;

			if(m_find)
				break;
		}

		if(!stack)
		{
			while(!vec_is_empty(stack) )
			{
				LinkObjRef Link=(LinkObjRef)vec_pop(stack);
				LMN_FREE(Link);
			}
			vec_free(stack);
		}

		LMN_FREE(root);
		proc_tbl_free(atoms);
		proc_tbl_free(hlinks);

		if(m_find)
			break;
	}

	return m_find;
}



void get_neighbours(  Vector  *avovec,
                      Vector *neighbours,
                      LmnAtomRef atom,
                      LmnLinkAttr pos,
                      ProcessTableRef  *attr_functors,
                      Vector   *attr_dataAtoms,
                      Vector   *attr_dataAtom_attrs)
{
  if (LMN_ATTR_IS_DATA(pos))
  {
    HyperLink *hl = lmn_hyperlink_at_to_hl(LMN_SATOM(atom));
    BOOL flg_search_hl = FALSE;  //True is hlink attribute matches with hlground attribute

    if (!LMN_HL_HAS_ATTR(hl)) //if hlink has no attribute
    {
      if(vec_num(attr_dataAtoms) == 0)  //hlground has no attribute
      {//means,  matched
        flg_search_hl=TRUE;
      }
    }

    {   // check if hlink attribute matches with hlground attribute
      LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
      LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
      int i;
      for (i = 0; i < vec_num(attr_dataAtoms); i++)
      {
        if (lmn_eq_func(attrAtom, attr, vec_get(attr_dataAtoms, i), vec_get(attr_dataAtom_attrs, i)))
        {
          flg_search_hl = TRUE;
          break;
        }
        else
        {
          continue;
        }
      }
    }

    

    //explore this hyperlink sublinks
    if (flg_search_hl)
    {
      Vector * hl_childs = vec_make(16);

      lmn_hyperlink_get_elements(hl_childs, hl);
      int child_num = vec_num(hl_childs) - 1;
      LmnMembraneRef *mem = hl->mem;

      int i;
      for (i = 0; i < child_num; i++)
      {
        if (mem!=((HyperLink *)vec_get(hl_childs, i))->mem)
        {
          continue;
        }
        /*   a(!H),b(!H).
         *   a--->!---H--->!--->b
         *   symbol atoms : a,b
         *   data atoms   : !
         *   'hlAtom' is a part of linkobject       : H--->!
         *   'linked_hlAtom' is a part of linkobject: !--->b
         * */
        LmnSAtom hlAtom = ((HyperLink *)vec_get(hl_childs, i))->atom; /* from hyperlink core points to ! atom  */
        LmnAtomRef linked_hlAtom;
        LmnLinkAttr linked_attr;

        if(hlAtom != NULL)
        {
          linked_hlAtom = LMN_SATOM_GET_LINK(hlAtom, 0);
          linked_attr = LMN_SATOM_GET_ATTR(hlAtom, 0);
          vec_push(neighbours,(LmnWord)LinkObj_make(linked_hlAtom,linked_attr));
        }

      }
      vec_free(hl_childs);
    }
  }
  else
  {  //get regular link children
    int i;
      for (i = 0; i < LMN_SATOM_GET_ARITY(atom); i++)
      {
        if (i == pos) //don't add this link itself
          continue;

        LinkObjRef new_obj;
        new_obj=LinkObj_make(LMN_SATOM_GET_LINK(atom, i),LMN_SATOM_GET_ATTR(atom, i));

        vec_push(neighbours,(LmnWord)LinkObj_make(new_obj->ap,new_obj->pos));

      }
  }
}


/***********extended ground end********/

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
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      if (lmn_data_atom_is_ground((LmnDataAtomRef)l->ap, l->pos, NULL)) {
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
      vec_push(stack, (LmnWord)l->ap);
    }
  }

  if (ok) {
    if (avovec != NULL && vec_num(avovec) > 0) {
      guard = hashtbl_make(16);
      for (i = 0; i < vec_num(avovec); i++) {
        LinkObjRef l = (LinkObjRef)vec_get(avovec, i);
        if (!LMN_ATTR_IS_DATA(l->pos)) {
          hashtbl_put(guard, (HashKeyType)l->ap, (HashValueType)l->pos);
        }
      }
    }

    if (n_root_data == 0) {
      while (vec_num(stack) > 0) {
        LmnSymbolAtomRef src_atom = LMN_SATOM(vec_pop(stack));
        if (LMN_SATOM_IS_PROXY(src_atom)) { ok = FALSE; break; }
        if (hashset_contains(visited, (HashKeyType)src_atom)) continue;
        if (hashset_contains(root, (HashKeyType)src_atom)) {n_reach_root++; continue; }
        hashset_add(visited, (HashKeyType)src_atom);
        n++;
        for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
          LmnAtomRef next_src = LMN_SATOM_GET_LINK(src_atom, i);
          LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

          if (!LMN_ATTR_IS_DATA(next_attr)) {
            if (guard) {
              int t = hashtbl_get_default(guard, (HashKeyType)next_src, -1);
              if (t >= 0 && t == LMN_ATTR_GET_VALUE(next_attr)) {
                ok = FALSE;
                break;
              }
            }
            vec_push(stack, (LmnWord)next_src);
          } else if (lmn_data_atom_is_ground((LmnDataAtomRef)next_src, next_attr, NULL)) {
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
  mem_remove_symbol_atom_with_buddy_data((LmnMembraneRef)_arg, (LmnSymbolAtomRef)_v);
  return 1;
}

void lmn_mem_remove_ground( LmnMembraneRef mem,
                            Vector *srcvec,
                            ProcessTableRef *attr_sym,
                            Vector *attr_data,
                            Vector *attr_data_at)
{

  ProcessTableRef atoms=hlground_data.local_atoms;
  unsigned long i, t;
  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);

  for (i = 0; i < vec_num(srcvec); i++)
  {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos))
    {
      //printf(" removed data atom: atom= %d, pos=%d \n",LMN_SATOM(l->ap),l->pos);
      lmn_mem_remove_data_atom(mem, l->ap, l->pos);
    }
    else if (LMN_ATTR_IS_EX(l->pos))
    {
      //printf(" removed symbol atom: atom= %d, pos=%d \n",LMN_SATOM(l->ap),l->pos);
      mem_remove_symbol_atom(mem, LMN_SATOM(l->ap));
    }
  }

 /*
  ProcessTableRef atoms;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);
  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);


  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos)) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)l->ap, l->pos);
    } else if (LMN_ATTR_IS_EX(l->pos)) {
      mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)l->ap);
    }
  }
  proc_tbl_free(atoms);
  */

}

void lmn_mem_remove_hlground(LmnMembraneRef mem,
                             Vector *srcvec,
                             ProcessTableRef *attr_sym,
                             Vector *attr_data,
                             Vector *attr_data_at)
{
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  unsigned long i, t;
  
  ground_atoms(srcvec, NULL, &atoms, &t, &hlinks, attr_sym, attr_data, attr_data_at);
  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos)) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)l->ap, l->pos);
    } else if (LMN_ATTR_IS_EX(l->pos)) {
      mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)l->ap);
    }
  }
  proc_tbl_free(atoms);
}

int free_symbol_atom_with_buddy_data_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)_v);
  return 1;
}

void lmn_mem_free_ground( Vector *srcvec,
                          ProcessTableRef *attr_sym,
                          Vector *attr_data,
                          Vector *attr_data_at)
{
  ProcessTableRef atoms=hlground_data.local_atoms;

  unsigned long i, t;

  proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);


  proc_tbl_free(hlground_data.global_hlinks);
  proc_tbl_free(hlground_data.local_atoms);

  for (i = 0; i < vec_num(srcvec); i++)
  {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos))
    	lmn_free_atom(l->ap, l->pos);
  }

  /*
  ProcessTableRef atoms;
  unsigned long i, t;

  if (ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL)) {
    proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);
    proc_tbl_free(atoms);
  }

  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) lmn_free_atom(l->ap, l->pos);
  }
  */

}

void lmn_mem_free_hlground(Vector *srcvec,
                           ProcessTableRef *attr_sym,
                           Vector *attr_data,
                           Vector *attr_data_at)
{
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  unsigned long i, t;

  hlinks = NULL;
  if (ground_atoms(srcvec, NULL, &atoms, &t, &hlinks, attr_sym, attr_data, attr_data_at)) {
    proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);
    proc_tbl_free(atoms);
  }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) lmn_free_atom(l->ap, l->pos);
  }
}

void lmn_mem_delete_ground(LmnMembraneRef mem, Vector *srcvec)
{
  ProcessTableRef atoms;
  unsigned long i, t;

  if (!ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL)) {
    fprintf(stderr, "remove ground false\n");
  }

  proc_tbl_foreach(atoms, mem_remove_symbol_atom_with_buddy_data_f, (LmnWord)mem);
  proc_tbl_foreach(atoms, free_symbol_atom_with_buddy_data_f, (LmnWord)0);

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)l->ap, l->pos);
      lmn_free_atom(l->ap, l->pos);
    }
  }

  proc_tbl_free(atoms);
}

/** ===========================
 *  膜の同型性判定
 *  ---------------------------
 *  created  by Takayuki Sasaki
 *  modified by Masato Gocho
 *  ---------------------------
 */

#define CHECKED_MEM_DEPTH (0)
static BOOL mem_equals_rec(LmnMembraneRef mem1, TraceLogRef  log1,
                           LmnMembraneRef mem2, SimplyLog log2,
                           int current_depth);


/* 階層グラフ構造mem1, mem2が同型ならば真を返す. */
BOOL lmn_mem_equals(LmnMembraneRef mem1, LmnMembraneRef mem2)
{
  BOOL t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  // 非TIME-OPTではTraceLogのストラクチャでトレースすることができない（実装してない）
  struct TraceLog log1;
  struct SimplyTraceLog log2;

  tracelog_init(&log1);
  simplylog_init(&log2);

  t = mem_equals_rec(mem1, &log1, mem2, &log2, CHECKED_MEM_DEPTH);

  simplylog_destroy(&log2);
  tracelog_destroy(&log1);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  return t;
}


static BOOL mem_equals_atomlists(LmnMembraneRef mem1, LmnMembraneRef mem2);
static BOOL mem_equals_isomorphism(LmnMembraneRef mem1, TraceLogRef log1,
                                   LmnMembraneRef mem2, SimplyLog log2,
                                   int current_depth);

/* 膜mem1から到達可能な全プロセスと
 * 膜mem2から到達可能な全プロセスが
 * 互いに過不足なくマッチングする(同型の階層グラフ構造)ならば真を返す. */
static BOOL mem_equals_rec(LmnMembraneRef mem1, TraceLogRef  log1,
                           LmnMembraneRef mem2, SimplyLog log2,
                           int current_depth)
{
#ifndef LMN_MEMEQ_OLD
  /* mem1, mem2が未訪問の場合 */
  if (tracelog_contains_mem(log1, mem1)) {
    if (!simplylog_contains_mem(log2, mem2)) {
      return FALSE;
    }
    else {
      return lmn_mem_id(mem2) == tracelog_get_memMatched(log1, mem1);
    }
  }
  else if (simplylog_contains_mem(log2, mem2)) {
    return FALSE;
  }

  tracelog_put_mem(log1, mem1, lmn_mem_id(mem2));
  simplylog_put_mem(log2, mem2);
#endif

  /*  グラフ同形成判定の各STEP:
   *  Step ID | Complexity  | Description
   *  ==========================================================================
   *      1.1 | O(1)        | 文字列に対応した整数ID(LmnMembraneのメンバ)同士の比較
   *      1.2 | O(1)        | シンボルアトム数(LmnMembraneのメンバ)同士の比較
   *      1.3 | O(1)        | データアトム数(LmnMembraneのメンバ)同士の比較
   *      1.4 | O(N)        | N本のAtomListEntryの種類と各アトム数の比較
   *      1.5 | O(N)        | 子膜数の比較(N個の要素を持ったリスト走査)
   *      1.6 | O(N*M)      | 子孫膜数の比較(N:子膜数, M:階層数: 階層数だけ子膜カウント)
   *      2.0 | O(N)        | 通常 : N本のRuleSet ID比較(IDは整数かつ整列済み故に線形)
   *          | O(N*M)      | uniq: N本のRuleSet IDおよび履歴の比較
   *          |             | 各履歴の比較はO(1), 履歴はハッシュ表管理->M個の履歴比較:O(M)
   *      3.1 | O(?)        | アトム起点のグラフ構造の同形成判定
   *      3.2 | O(?)        | 子膜起点のグラフ構造の同形成判定
   *  --------------------------------------------------------------------------
   *  Step 1.X| O(N + M)    | N:子膜数, M;アトムリスト数. Step 1.6廃止につき,線形の計算量
   *  Step 2.X| O(N)        | 標準仕様(uniq未使用)ならば線形の計算量
   *  Step 3.X| O(?)        | TODO: 調査
   *
   *   子孫膜の比較(Step 1.6)はコストが高いため廃止.
   *     実際にはハッシュ関数mhashとの組合わせで同形成判定を実施しており,
   *     ハッシュ関数mhashは, 階層の深さに関するハッシュコンフリクトを招きやすいものではない.
   */
  if (/* 1.1 */ LMN_MEM_NAME_ID(mem1) != LMN_MEM_NAME_ID(mem2)                ||
      /* 1.2 */ lmn_mem_symb_atom_num(mem1) != lmn_mem_symb_atom_num(mem2)    ||
      /* 1.3 */ lmn_mem_data_atom_num(mem1) != lmn_mem_data_atom_num(mem2)    ||
      /* 1.4 */ !mem_equals_atomlists(mem1, mem2)                             ||
      /* 1.5 */ lmn_mem_child_mem_num(mem1) != lmn_mem_child_mem_num(mem2)    ||
      /* 1.6 lmn_mem_count_descendants(mem1) != lmn_mem_count_descendants(mem2)) ||*/
      /* 2.0 */ !lmn_rulesets_equals(lmn_mem_get_rulesets(mem1),
                                     lmn_mem_get_rulesets(mem2))              ||
      /* 3.X */ !mem_equals_isomorphism(mem1, log1, mem2, log2, current_depth))
  {
    return FALSE;
  }
  else {
    return TRUE;
  }
}


/* 膜mem1, mem2直下のアトムリストを比較(アトムの種類毎にアトム数を比較)した結果の真偽値を返す */
static BOOL mem_equals_atomlists(LmnMembraneRef mem1, LmnMembraneRef mem2)
{
  AtomListEntry *ent1;
  LmnFunctor f;

  EACH_ATOMLIST_WITH_FUNC(mem1, ent1, f, ({
    AtomListEntry *ent2 = lmn_mem_get_atomlist(mem2, f);
    if (atomlist_get_entries_num(ent1) != atomlist_get_entries_num(ent2)) {
      return FALSE;
    }
  }));

  return TRUE;
}


/** ---------------------------
 *  Step 3.X
 */


#define ISOMOR_PHASE_ATOM   (0U)
#define ISOMOR_PHASE_CHILD  (1U)
#define ISOMOR_PHASE_END    (2U)

typedef struct MemIsomorIter MemIsomorIter;
struct MemIsomorIter {
  BYTE          phase;
  AtomListIter  pos;
  LmnMembraneRef   mem;
  LmnSymbolAtomRef      atom;
  LmnMembraneRef   child;
};


static inline BOOL mem_equals_molecules(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth);
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth);

static inline void memIsomorIter_init(MemIsomorIter *ma_iter, LmnMembraneRef mem);
static inline void memIsomorIter_destroy(MemIsomorIter *iter);
static inline LmnSymbolAtomRef     memIsomorIter_atom_traversed(MemIsomorIter *iter);
static inline LmnMembraneRef memIsomorIter_child_traversed(MemIsomorIter *iter);
static BOOL mem_isomor_mols(LmnMembraneRef mem1, TraceLogRef  log1,
                            LmnMembraneRef mem2, SimplyLog log2,
                            MemIsomorIter *iter);

/* 未訪問の膜mem1および膜mem2を対象に, アトム起点および子膜起点でグラフ構造をトレースする.
 * 膜mem1とmem2以下の階層グラフが同型と判定されたなれば真を返し, 異形なばら偽を返す. */
static BOOL mem_equals_isomorphism(LmnMembraneRef mem1, TraceLogRef  log1,
                                   LmnMembraneRef mem2, SimplyLog log2,
                                   int current_depth)
{
  BOOL ret;
#ifdef LMN_MEMEQ_OLD
  /* !!CAUTION!!:
   *   旧同形成判定アルゴリズムは, 特定のケースで正しい結果を返すことができない.
   */
  ret = /* Step 3.1 */ mem_equals_molecules(mem1, mem2, current_depth) &&
        /* Step 3.2 */ mem_equals_children(mem1, mem2, current_depth);
#else
  MemIsomorIter iter;

  memIsomorIter_init(&iter, mem1);
  ret = /* Step 3.X */ mem_isomor_mols(mem1, log1, mem2, log2, &iter);
  memIsomorIter_destroy(&iter);
#endif

  return ret;
}

static inline void memIsomorIter_init(MemIsomorIter *ma_iter, LmnMembraneRef mem)
{
  ma_iter->pos   = atomlist_iter_initializer(mem->atomset);
  ma_iter->phase = ISOMOR_PHASE_ATOM;
  ma_iter->mem   = mem;
  ma_iter->atom  = NULL;
  ma_iter->child = NULL;
}

static inline void memIsomorIter_destroy(MemIsomorIter *iter) {}

static inline BOOL memIsomorIter_is_root_atom(LmnSymbolAtomRef atom)
{
  LmnFunctor f;
  f = LMN_SATOM_GET_FUNCTOR(atom);
  if (f == LMN_RESUME_FUNCTOR     ||
      f == LMN_OUT_PROXY_FUNCTOR  ||
      (f == LMN_IN_PROXY_FUNCTOR &&
       !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 1)))) {
    /* データアトムと接続するinside proxyだけはルートアトムとする */
    return FALSE;
  }

  return TRUE;
}

/* イテレータiterに記録した情報を基に,
 * 前回訪問したアトムiter->atomから次に訪問するアトムを求めて返し, ない場合はNULL返す.
 *
 * 出力: iter->pos, iter->atom (iter->memはRead Only) */
static inline LmnSymbolAtomRef memIsomorIter_atom_traversed(MemIsomorIter *iter)
{
  BOOL changed_lists;

  if (iter->phase != ISOMOR_PHASE_ATOM) {
    return NULL;
  }

  changed_lists = FALSE;
  for ( ;
       atomlist_iter_condition(iter->mem, iter->pos);
       atomlist_iter_next(iter->pos), changed_lists = TRUE, iter->atom = NULL)
       /* OUTER LOOP */
  {
    AtomListEntry *ent;
    LmnFunctor f;
    f   = atomlist_iter_get_functor(iter->pos);
    ent = atomlist_iter_get_entry(iter->mem, iter->pos);

    if (!ent || atomlist_is_empty(ent) || f == LMN_OUT_PROXY_FUNCTOR) {
      /* アトムリストが空の場合, 次の候補リストを取得する.
       * outside proxyは候補としない */
      continue; /* OUTER LOOP */
    }
    else {
      BOOL cur_is_tail;

      if (changed_lists || !iter->atom) {
        /* 走査先のアトムリストが変更された場合 or イテレータアトムが未設定の場合:
         *   リストの先頭アトムをcurとする */
        iter->atom = atomlist_head(ent);
      }
      else if (iter->atom == lmn_atomlist_end(ent)) {
        /* 一応, 想定外 */
        continue;
      }
      else {
        /* 既にイテレータアトムが設定されている場合は, リストから次のアトムを確保する */
        iter->atom = LMN_SATOM_GET_NEXT_RAW(iter->atom);
        if (iter->atom == lmn_atomlist_end(ent)) {
          /* 結果, curが末尾に到達したならば次のアトムリストを求める. */
          continue; /* OUTER LOOP */
        }
      }

      /* リストを辿り, 根の候補とするアトムを求める */
      cur_is_tail = FALSE;
      while (!memIsomorIter_is_root_atom(iter->atom)) { /* INNER LOOP */
        /* Resumeアトムを読み飛ばす */
        iter->atom = LMN_SATOM_GET_NEXT_RAW(iter->atom);

        if (iter->atom == lmn_atomlist_end(ent)) {
          /* tailに到達してしまった場合はフラグを立ててからループを抜ける */
          cur_is_tail = TRUE;
          break; /* INNER LOOP */
        }
      } /* INNER LOOP END */

      if (!cur_is_tail) {
        /* atomlistentryのtailへの到達以外でループを抜けた場合:
         *   アトムへの参照取得に成功したのでループを抜ける */
        break; /* OUTER LOOP */
      }
    }
  } /* OUTER LOOP END */

  if (iter->atom) {
    return iter->atom;
  } else {
    iter->phase = ISOMOR_PHASE_CHILD;
    return NULL;
  }
}


static inline LmnMembraneRef memIsomorIter_child_traversed(MemIsomorIter *iter)
{
  if (iter->phase != ISOMOR_PHASE_CHILD) {
    return NULL;
  }
  else {
    if (!iter->child) {
      iter->child = lmn_mem_child_head(iter->mem);
    } else {
      iter->child = lmn_mem_next(iter->child);
    }

    if (!iter->child) {
      iter->phase = ISOMOR_PHASE_END;
      return NULL;
    } else {
      return iter->child;
    }
  }
}

#define MEM_ISOMOR_FIRST_TRACE          (-1)
#define MEM_ISOMOR_MATCH_WITHOUT_MEM    (1U)
#define MEM_ISOMOR_MATCH_WITHIN_MEM     (2U)


static inline BOOL mem_isomor_mol_atoms(LmnMembraneRef mem1, TraceLogRef  log1,
                                        LmnMembraneRef mem2, SimplyLog log2,
                                        MemIsomorIter *iter, LmnSymbolAtomRef root1);
static inline BOOL mem_isomor_mol_mems(LmnMembraneRef mem1, TraceLogRef  log1,
                                       LmnMembraneRef mem2, SimplyLog log2,
                                       MemIsomorIter *iter, LmnMembraneRef root1);


/* mem1およびmem2直下から等価な分子構造を再帰DFSで探索する.
 * 全ての分子が過不足なく1対1にした場合, 真を返す. */
static BOOL mem_isomor_mols(LmnMembraneRef mem1, TraceLogRef  log1,
                            LmnMembraneRef mem2, SimplyLog log2,
                            MemIsomorIter *iter)
{
  LmnSymbolAtomRef root1;

  /* アトム起点の探索のための根を取得 */
  do {
    root1 = memIsomorIter_atom_traversed(iter);
    /* 未トレースのアトムが現れるか, 末尾(NULL)に到達するまで根の候補探索を繰り返す. */
  } while (root1 && tracelog_contains_atom(log1, root1));

  if (root1) {
    /* アトムの根がある場合: アトム起点のグラフ同形成判定 */
    return mem_isomor_mol_atoms(mem1, log1,
                                mem2, log2, iter, root1);
  }
  else {
    LmnMembraneRef child;
    do {
      child = memIsomorIter_child_traversed(iter);
    } while (child && tracelog_contains_mem(log1, child));

    if (child) {
      /* 子膜の根がある場合: 子膜起点のグラフ同形成判定 */
      return mem_isomor_mol_mems(mem1, log1,
                                 mem2, log2,
                                 iter, child);
    }
    else {
      /* アトムの根も子膜の根がない場合:
       *   全ての根を候補としたグラフ同形成判定が終了.
       *   比較済プロセス数と所持プロセス数を比較し, 対応漏れがないか検査する. */

      return tracelog_eq_traversed_proc_num(log1,
                                            mem1,
                                            lmn_mem_get_atomlist(mem1, LMN_IN_PROXY_FUNCTOR),
                                            NULL);
    }
  }
}


static inline int mem_isomor_trace(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                   LmnFunctor f,  int from_i,
                                   LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2);

static inline BOOL mem_isomor_mol_atoms(LmnMembraneRef mem1, TraceLogRef  log1,
                                        LmnMembraneRef mem2, SimplyLog log2,
                                        MemIsomorIter *iter, LmnSymbolAtomRef root1)
{
  MemIsomorIter current; /* for keeping current state (snap shot) */
  AtomListEntry *ent2;
  LmnSymbolAtomRef      root2;
  LmnFunctor    f;

  f    = LMN_SATOM_GET_FUNCTOR(root1);
  ent2 = lmn_mem_get_atomlist(mem2, f);
  if (!ent2) return FALSE;
  current = (*iter); /* shallow copy */

  EACH_ATOM(root2, ent2, ({
    if (simplylog_contains_atom(log2, root2)) continue;
    tracelog_set_btpoint(log1);
    simplylog_set_btpoint(log2);

    /* compare a molecule of root1 with a molecule of root2 */
    switch (mem_isomor_trace((LmnAtomRef)root1, mem1, log1,
                             f, MEM_ISOMOR_FIRST_TRACE,
                             (LmnAtomRef)root2, mem2, log2))
    {
      case MEM_ISOMOR_MATCH_WITHOUT_MEM:
      {
        tracelog_continue_trace(log1);
        simplylog_continue_trace(log2);
        return mem_isomor_mols(mem1, log1, mem2, log2, iter);
      }
      case MEM_ISOMOR_MATCH_WITHIN_MEM:
        /* keep this backtrack point */
        if (mem_isomor_mols(mem1, log1, mem2, log2, iter)) {
          tracelog_continue_trace(log1);
          simplylog_continue_trace(log2);
          return TRUE;
        } /*
        else FALL THROUGH */
      case FALSE:
        /*   FALL THROUGH */
      default:
        break;
    }

    (*iter) = current;
    tracelog_backtrack(log1);
    simplylog_backtrack(log2);
  }));

  /* matchingする分子がなかった: FALSE */

  return FALSE;
}


static inline BOOL mem_isomor_mol_mems(LmnMembraneRef mem1, TraceLogRef  log1,
                                       LmnMembraneRef mem2, SimplyLog log2,
                                       MemIsomorIter *iter, LmnMembraneRef root1)
{
  LmnMembraneRef root2;
  for (root2 = lmn_mem_child_head(mem2); root2; root2 = lmn_mem_next(root2)) {
    if (simplylog_contains_mem(log2, root2)) continue;
    else {
      tracelog_set_btpoint(log1);
      simplylog_set_btpoint(log2);
      if (!mem_equals_rec(root1, log1, root2, log2, CHECKED_MEM_DEPTH) ||
          !mem_isomor_mols(mem1, log1, mem2, log2, iter)) {
        tracelog_backtrack(log1);
        simplylog_backtrack(log2);
      }
      else {
        tracelog_continue_trace(log1);
        simplylog_continue_trace(log2);
        return TRUE;
      }
    }
  }

  return FALSE;
}



static inline BOOL mem_isomor_trace_proxies(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                            LmnFunctor f,
                                            LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2);
static inline int mem_isomor_trace_symbols(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                           LmnFunctor f, int from_i,
                                           LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2);


/* from link番号だけは受け取る.
 * 等価なアトムcur1, cur2から1step先の構造を比較して再帰する. */
static inline int mem_isomor_trace(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                   LmnFunctor f,  int from_i,
                                   LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2)
{
  if (LMN_IS_PROXY_FUNCTOR(f)) {
    /* proxy atomの場合 */
    if (!mem_isomor_trace_proxies(cur1, mem1, log1, f,
                                  cur2, mem2, log2)) {
      return FALSE;
    } else {
      return MEM_ISOMOR_MATCH_WITHIN_MEM;
    }
  }
  else {
    /* 非proxy(symbol)アトムの場合 */
    return mem_isomor_trace_symbols(cur1, mem1, log1, f, from_i,
                                    cur2, mem2, log2);
  }
}

static inline BOOL mem_isomor_trace_proxies(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                            LmnFunctor f,
                                            LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2)
{
  LmnAtomRef pair1, pair2, atom1, atom2;
  LmnLinkAttr attr1, attr2;
  LmnMembraneRef nxtM1, nxtM2;

  /* proxy atomの場合
   * -----------------+
   * ...-0--1-[in]-0--|--0-[out]-1--..
   * -----------------+
   */
  pair1 = LMN_SATOM_GET_LINK(cur1, 0);
  pair2 = LMN_SATOM_GET_LINK(cur2, 0);
  nxtM1 = LMN_PROXY_GET_MEM(pair1);
  nxtM2 = LMN_PROXY_GET_MEM(pair2);

  atom1 = LMN_SATOM_GET_LINK(pair1, 1);
  atom2 = LMN_SATOM_GET_LINK(pair2, 1);
  attr1 = LMN_SATOM_GET_ATTR(pair1, 1);
  attr2 = LMN_SATOM_GET_ATTR(pair2, 1);

  /* 1. inside proxy atomに対する訪問関係をチェック */
  {
    LmnAtomRef in1, in2;
    if (f == LMN_OUT_PROXY_FUNCTOR) {
      in1   = pair1;
      in2   = pair2;
    }
    else if (f == LMN_IN_PROXY_FUNCTOR) {
      in1   = cur1;
      in2   = cur2;
    }
    else {
      lmn_fatal("sorry, unrecognized proxy functor found: unrecognized proxy");
      return FALSE;
    }

    if (tracelog_contains_atom(log1, (LmnSymbolAtomRef)in1)) {
      if (!simplylog_contains_atom(log2, (LmnSymbolAtomRef)in2) ||
          LMN_SATOM_ID(in2) != tracelog_get_atomMatched(log1, (LmnSymbolAtomRef)in1)) {
        return FALSE;
      } else {
        return TRUE;
      }
    }
    else if (simplylog_contains_atom(log2, (LmnSymbolAtomRef)in2)) {
      return FALSE;
    }
    else {
      /* in1は互いに未訪問であるため, トレースする必要がある */
      tracelog_put_atom(log1, (LmnSymbolAtomRef)in1, LMN_SATOM_ID(in2), LMN_PROXY_GET_MEM(in1));
      simplylog_put_atom(log2, (LmnSymbolAtomRef)in2);
    }
  }

  /* 2. 膜の訪問関係をチェック */
  if (!tracelog_contains_mem(log1, nxtM1)) {
    if (simplylog_contains_mem(log2, nxtM2)) {
      /* nxtM1が未訪問でnxtM2が訪問済ならば異形 */
      return FALSE;
    }
  }
  else if (!simplylog_contains_mem(log2, nxtM2) ||
           lmn_mem_id(nxtM2) != tracelog_get_memMatched(log1, nxtM1)) {
    /* nxtM1が訪問済で, { nxtM2が未訪問 || 対応する膜の関係が合わない } ならば異形 */
    return FALSE;
  } /*
  else {
    膜nxtM1, nxtM2は互いに訪問済 or 未訪問
  }
  */

  /* 3. 比較 */
  if ((LMN_ATTR_IS_DATA(attr1) || LMN_ATTR_IS_DATA(attr2))) {
    /* proxyアトムがデータアトムに接続している場合 */
    if (!lmn_data_atom_eq((LmnDataAtomRef)atom1, attr1, (LmnDataAtomRef)atom2, attr2)) {
      return FALSE;
    }
    else {
      BOOL ret = TRUE;
      if (f == LMN_OUT_PROXY_FUNCTOR && !tracelog_contains_mem(log1, nxtM1)) {
        /* 初出の子膜に入る(out proxyからトレースする)場合は, 子膜の同形成判定を行う. */
        ret = mem_equals_rec(nxtM1, log1, nxtM2, log2, CHECKED_MEM_DEPTH);
      }
      return ret;
    }
  }
  else { /* proxyアトムがシンボルアトムに接続している場合 */
    LmnFunctor f_new = LMN_SATOM_GET_FUNCTOR(atom1);
    if (f_new != LMN_SATOM_GET_FUNCTOR(atom2) || attr1 != attr2) {
      return FALSE;
    }
    else {
      BOOL ret = mem_isomor_trace(atom1, nxtM1, log1,
                                  f_new, LMN_ATTR_GET_VALUE(attr1),
                                  atom2, nxtM2, log2);
      if (ret &&
          f == LMN_OUT_PROXY_FUNCTOR && !tracelog_contains_mem(log1, nxtM1)) {
        /* 初出の子膜に入る(out proxyからトレースする)場合は, 子膜の同形成判定を行う. */
        ret = mem_equals_rec(nxtM1, log1,
                             nxtM2, log2, CHECKED_MEM_DEPTH);
      }

      return ret;
    }
  }
}


static inline int mem_isomor_trace_symbols(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                                           LmnFunctor f, int from_i,
                                           LmnAtomRef cur2, LmnMembraneRef mem2, SimplyLog log2)
{
  int to_i, global_ret;

  tracelog_put_atom(log1, (LmnSymbolAtomRef)cur1, LMN_SATOM_ID(cur2), mem1);
  simplylog_put_atom(log2, (LmnSymbolAtomRef)cur2);

  global_ret = MEM_ISOMOR_MATCH_WITHOUT_MEM;

  /* リンク先を辿る */
  for (to_i = 0; to_i < LMN_FUNCTOR_ARITY(f); to_i++) {
    LmnLinkAttr attr1, attr2;
    LmnAtomRef atom1, atom2;

    /* 辿ってきたリンクは検査済み: skip */
    if (to_i == from_i) continue;

    attr1 = LMN_SATOM_GET_ATTR(cur1, to_i);
    attr2 = LMN_SATOM_GET_ATTR(cur2, to_i);
    atom1 = LMN_SATOM_GET_LINK(cur1, to_i);
    atom2 = LMN_SATOM_GET_LINK(cur2, to_i);

    if (LMN_ATTR_IS_DATA(attr1) ||
        LMN_ATTR_IS_DATA(attr2)) { /* data atomの場合 */
      if (!lmn_data_atom_eq((LmnDataAtomRef)atom1, attr1, (LmnDataAtomRef)atom2, attr2)) {
        return FALSE;
      }/*
      else continue
      */
    }
    else { /* symbol atomの場合 */
      LmnFunctor f_new = LMN_SATOM_GET_FUNCTOR(atom1);

      if (f_new != LMN_SATOM_GET_FUNCTOR(atom2) ||
          attr1 != attr2)
      {
        /* 接続先アトムの種類(functor)や接続先リンクの番号が異なる場合は異形 */
        return FALSE;
      }

      if (tracelog_contains_atom(log1, (LmnSymbolAtomRef)atom1)) {
        /* 接続先のアトムが既に訪問済みの場合 */
        if (!simplylog_contains_atom(log2, (LmnSymbolAtomRef)atom2) ||
            LMN_SATOM_ID(atom2) != tracelog_get_atomMatched(log1, (LmnSymbolAtomRef)atom1))
        {
          /* 互いに訪問済み関係でない場合や訪問済みアトムへの対応関係が一致していない場合は異形 */
          return FALSE;
        }/*
        else continue
        */
      }
      else if (simplylog_contains_atom(log2, (LmnSymbolAtomRef)atom2)) {
        /* 接続先のアトムへ初めて訪問する場合: 対応するアトムが訪問済みのため異形 */
        return FALSE;
      }
      else {
        /* 互いに接続先のアトムへ初めて訪問する場合 */
        switch (mem_isomor_trace(atom1, mem1, log1,
                                 f_new, LMN_ATTR_GET_VALUE(attr1),
                                 atom2, mem2, log2)) {
        case FALSE:
          return FALSE;
        case MEM_ISOMOR_MATCH_WITHIN_MEM:
          global_ret = MEM_ISOMOR_MATCH_WITHIN_MEM;
          break;
        case MEM_ISOMOR_MATCH_WITHOUT_MEM:
          /* FALLTHROUTH */
        default:
          break;
        }
      }
    }
  }

  /* cur1, cur2を起点に到達可能なLMNtalプロセスの接続関係が全て一致した
   * 真を返す. */

  return global_ret;
}


/** -------------------
 *  Step 3.2: 子膜起点のグラフトレース (old)
 */

static void mem_mk_sorted_children(Vector *vec);
static inline BOOL mem_equals_children_inner(Vector *v_mems_children1,
                                             Vector *v_mems_children2,
                                             int current_depth);

/* この段階で本膜内の「アトムを起点とする走査」はすべて完了し, 両膜直下のグラフ構造は同型である.
 * ここからは, 両膜内に存在するすべての子膜について, その構造が一致するかどうかについて調べていく.
 * 以降、mem1内の子膜を1つ固定し、mem2直下の子膜から対応するものを特定する作業に移っていくが,
 * 結果が偽となるケースにおける処理速度を向上させるため, 子孫膜数が少ない子膜から優先的に固定する */
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth) LMN_UNUSED;
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth)
{
  int child_n;

  child_n = lmn_mem_count_children(mem1);
  if (child_n != lmn_mem_count_children(mem2)) {
    return FALSE;
  }
  else if (child_n == 0) {
    return TRUE;
  }
  else {
    Vector *v_mems_children1, *v_mems_children2; /* 本膜直下の子膜を管理するVector (このVectorが空になるまで子膜を起点とする走査が続く) */
    BOOL matched;

    v_mems_children1 = vec_make(child_n);
    v_mems_children2 = vec_make(child_n);
    memset(v_mems_children1->tbl, 0, sizeof(vec_data_t) * vec_cap(v_mems_children1));
    memset(v_mems_children2->tbl, 0, sizeof(vec_data_t) * vec_cap(v_mems_children2));

    matched = TRUE;

    /* mem1, mem2直下の子膜を取得し、その個数が等しいことを確認 */
    {
      LmnMembraneRef m;
      for (m = mem1->child_head; m; m = m->next) vec_push(v_mems_children1, (LmnWord)m);
      for (m = mem2->child_head; m; m = m->next) vec_push(v_mems_children2, (LmnWord)m);

      if (vec_num(v_mems_children1) != vec_num(v_mems_children2)) {
        /* 本膜直下の子膜数が互いに一致しない場合は偽 */
        matched = FALSE;
      }
      else {
        /* 子孫膜数の多い順にv_mems_children1, v_mems_children2をソート */
        mem_mk_sorted_children(v_mems_children1);
        mem_mk_sorted_children(v_mems_children2);
      }
    }

    if (matched) {
      matched = mem_equals_children_inner(v_mems_children1,
                                          v_mems_children2,
                                          current_depth);
    }


    vec_free(v_mems_children1);
    vec_free(v_mems_children2);

    return matched;
  }
}

static inline BOOL mem_equals_children_inner(Vector *v_mems_children1,
                                             Vector *v_mems_children2,
                                             int current_depth)
{
  int i, j;
  BOOL matched;

  /* 子膜を起点とする走査 */
  matched = TRUE;
  while (matched && !vec_is_empty(v_mems_children1)) {
    LmnMembraneRef cm1 = (LmnMembraneRef)vec_pop(v_mems_children1);

    /* fprintf(stderr, "\t-- start to test a descendant membrane --\n\t\t# of descendants of mem(%u): %u\n"
     *               , (unsigned int)cm1
     *               , lmn_mem_count_descendants(cm1)); */

    for (i = vec_num(v_mems_children2); i > 0; i--) {
      LmnMembraneRef cm2 = (LmnMembraneRef)vec_get(v_mems_children2, i-1);

      matched = mem_equals_rec(cm1, NULL, cm2, NULL, current_depth + 1);
      if (!matched) {
        continue; /* INNER1 LOOP */
      }
      else {
        /* cm1と同型の膜(=cm2)がv_mems_children2内に見つかった場合にここに入る。
         * v_mems_children2からcm2を取り除く。 */
        for (j = 0; j < vec_num(v_mems_children2); j++) {
          if (cm2 == (LmnMembraneRef)vec_get(v_mems_children2, j)) {
            vec_pop_n(v_mems_children2, j);
            break; /* INNER2 LOOP */
          }
        }
        break; /* INNER1 LOOP */
      }
    }
  }

  return matched;
}

/* ある膜の直下のすべての子膜へのポインタを保持するベクターvecを
 * 子孫膜数の多い順にソートする。ソートされたvec内の要素は後のステップで
 * POPされるため、子孫膜数の少ない子膜から順にマッチングの対象となることになる。 */
static void mem_mk_sorted_children(Vector *vec)
{
  unsigned int num_descendants_max;
  unsigned int i, n;
  Vector *v_mems_tmp;

  LMN_ASSERT(vec_num(vec));

  num_descendants_max = 0;
  for (i = 0; i < vec_num(vec); i++) {
    n = lmn_mem_count_descendants((LmnMembraneRef)vec_get(vec, i));
    if (n > num_descendants_max) {
      num_descendants_max = n;
    }
  }
  v_mems_tmp = vec_make(vec_num(vec));
  for (n = 0; n <= num_descendants_max; n++) {
    for (i = 0; i < vec_num(vec); i++) {
      if (n == lmn_mem_count_descendants((LmnMembraneRef)vec_get(vec, i))) {
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




/** -------------------
 * Step 3.1: アトム起点のグラフトレース (old)
 */

typedef struct AtomVecData {
  LmnFunctor fid;
  Vector *atom_ptrs;
} atomvec_data;


static Vector *mem_mk_matching_vec(LmnMembraneRef mem);
static void free_atomvec_data(Vector *vec);
static inline BOOL mem_equals_molecules_inner(Vector *v_log1, Vector *v_atoms_not_checked1,
                                              Vector *v_log2, Vector *v_atoms_not_checked2,
                                              int current_depth);

/* この段階で両膜は互いに等しい数の子孫膜を持ち、両膜内のアトムのファンクタの種類
 * およびその個数が完全に一致することが確認されている。
 * (i.e. 結果が「同型でない(偽)」になるならば、本膜におけるリンクの接続関係 or 子孫膜が異なっていることを意味する)
 * 以降、少数派のアトムから順に根に定めていき、アトムを起点とする走査の実行に移っていく。 */
static inline BOOL mem_equals_molecules(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth) LMN_UNUSED;
static inline BOOL mem_equals_molecules(LmnMembraneRef mem1, LmnMembraneRef mem2, int current_depth)
{
  unsigned int atom_n = lmn_mem_atom_num(mem1);
  LMN_ASSERT(atom_n == lmn_mem_atom_num(mem2));

  if (atom_n != lmn_mem_atom_num(mem2)) {
    return FALSE;
  }
  else if (atom_n == 0) {
    return TRUE;
  }
  else {
    Vector *v_log1, *v_log2; /* 走査中に通過したアトムのログを管理するVector */
    Vector *v_atoms_not_checked1, *v_atoms_not_checked2; /* 同型性の判定の判定が済んでいないアトムの集合を管理するVector (各アトムへのポインタを保存) */
    int i, j;
    BOOL ret;

    /* 以降、未走査／走査済アトムを管理するvectorの初期化 */

    v_atoms_not_checked1 = vec_make(atom_n);
    v_atoms_not_checked2 = vec_make(atom_n);
    memset(v_atoms_not_checked1->tbl, 0, sizeof(vec_data_t) * vec_cap(v_atoms_not_checked1));
    memset(v_atoms_not_checked2->tbl, 0, sizeof(vec_data_t) * vec_cap(v_atoms_not_checked2));

    {
      Vector *atomvec_mem1, *atomvec_mem2;

      /* atomvec_memX (X = 1,2)は、膜memX 直下のアトムの情報を保持するVector。
       * 膜内のアトムをファンクタ毎に整理し、少数派のアトムからマッチングを開始できるようにする目的で使用する。 */
      atomvec_mem1 = mem_mk_matching_vec(mem1);
      atomvec_mem2 = mem_mk_matching_vec(mem2);

      LMN_ASSERT(vec_num(atomvec_mem1) == vec_num(atomvec_mem2));

      /* ベクターatomvec_mem{1,2}には多数派のアトムから順に放りこまれているため、
       * ベクターv_atoms_not_checked{1,2}には多数派のアトムのポインタから順に
       * 放りこまれていくことになる。ゆえに、v_atoms_not_checked{1,2}をPOPしていく
       * ことで少数派のアトムから順に取り出していくことができるようになる。 */
      for (i = 0; i < vec_num(atomvec_mem1); i++) {
        for (j = 0; j < vec_num(((atomvec_data *)vec_get(atomvec_mem1, i))->atom_ptrs); j++) {
          vec_push(v_atoms_not_checked1,
                   vec_get(((atomvec_data *)vec_get(atomvec_mem1, i))->atom_ptrs, j));
          vec_push(v_atoms_not_checked2,
                   vec_get(((atomvec_data *)vec_get(atomvec_mem2, i))->atom_ptrs, j));
        }
      }

      free_atomvec_data(atomvec_mem1);
      free_atomvec_data(atomvec_mem2);
    }

    LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

    v_log1 = vec_make(atom_n);
    v_log2 = vec_make(atom_n);

    ret = mem_equals_molecules_inner(v_log1, v_atoms_not_checked1,
                                     v_log2, v_atoms_not_checked2,
                                     current_depth);
    vec_free(v_log1);
    vec_free(v_log2);
    vec_free(v_atoms_not_checked1);
    vec_free(v_atoms_not_checked2);

    return ret;
  }
}

/* 構造体'atomvec_data'に関係するメモリーの領域を解放する */
static void free_atomvec_data(Vector *vec)
{
  unsigned int i;

  for (i = 0; i < vec_num(vec); ++i) {
    atomvec_data *ad = (atomvec_data *)vec_get(vec, i);
    if (ad) {
      vec_free(ad->atom_ptrs);
      LMN_FREE(ad);
    }
  }
  vec_free(vec);
}


static BOOL mem_trace_links(LmnSymbolAtomRef a1,
                            LmnSymbolAtomRef a2,
                            Vector *v_log1,
                            Vector *v_log2,
                            int current_depth,
                            BOOL need_to_check_this_membrane_processes);

static inline BOOL mem_equals_molecules_inner(Vector *v_log1, Vector *v_atoms_not_checked1,
                                              Vector *v_log2, Vector *v_atoms_not_checked2,
                                              int current_depth)
{
  BOOL matched = TRUE;
  while (matched && !vec_is_empty(v_atoms_not_checked1)) {
    LmnSymbolAtomRef a1;
    int i, j, k;

    /* 膜1内から1つアトムを取り出しこれを根とする。
     * 膜1内のアトムの内、(ファンクタが)少数派のものから順に根に定められていくことに注意せよ。 */
    a1 = (LmnSymbolAtomRef)vec_pop(v_atoms_not_checked1);

    /*fprintf(stdout, "fid(a1):%u\n", (unsigned int)LMN_SATOM_GET_FUNCTOR(a1));*/

    for (i = vec_num(v_atoms_not_checked2); i > 0; i--) {
      LmnSymbolAtomRef a2 = (LmnSymbolAtomRef)vec_get(v_atoms_not_checked2, i-1); /* 膜2内から根a1に対応するアトムの候補を取得 (注: ここの実装にvec_popは使用不可!!) */

      vec_clear(v_log1);
      vec_clear(v_log2);
      memset(v_log1->tbl, 0, sizeof(vec_data_t) * vec_cap(v_log1));
      memset(v_log2->tbl, 0, sizeof(vec_data_t) * vec_cap(v_log2));

      /* a2が本当にa1に対応するアトムであるか否かを実際にグラフ構造をトレースして確認する。
       * a2とa1とが1:1に対応する場合に限って matched に真が返り、
       * v_log{1,2}内にはa{1,2}を起点とする分子内の全アトムのアドレスが走査ログとして記録される。 */
      matched = mem_trace_links(a1, a2, v_log1, v_log2, current_depth, FALSE);
      if (!matched) {
        continue;
      }
      else {
        /*fprintf(stdout, "fid(a2):%u\n", (unsigned int)LMN_SATOM_GET_FUNCTOR(a2));*/

        /* 両膜内に存在するある分子同士のマッチングに成功した場合にここに入る。
         * 膜2内の未マッチングのアトムを管理していたベクター(v_atoms_not_checked2)
         * から根a1に対応するアトムa2を除去する。 */
        LMN_ASSERT(vec_num(v_log1) == vec_num(v_log2));
        for (j = 0; j < vec_num(v_atoms_not_checked2); j++) {
          if (LMN_SATOM(vec_get(v_atoms_not_checked2, j)) == a2) {
            vec_pop_n(v_atoms_not_checked2, j);
            break;
          }
        }
        LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));

        /* ログ上に存在するすべてのアトムを、未チェックアトムのリストからPOPする */
        for (j = 0; j < vec_num(v_log1); j++) {
          for (k = 0; k < vec_num(v_atoms_not_checked1); k++) {
            if (LMN_SATOM(vec_get(v_log1, j)) == LMN_SATOM(vec_get(v_atoms_not_checked1, k))) {
              vec_pop_n(v_atoms_not_checked1, k);
              break;
            }
          }
        }

        for (j = 0; j < vec_num(v_log2); j++) {
          for (k = 0; k < vec_num(v_atoms_not_checked2); k++) {
            if (LMN_SATOM(vec_get(v_log2, j)) == LMN_SATOM(vec_get(v_atoms_not_checked2, k))) {
              vec_pop_n(v_atoms_not_checked2, k);
              break;
            }
          }
        }

        LMN_ASSERT(vec_num(v_atoms_not_checked1) == vec_num(v_atoms_not_checked2));
        break;
      }
    }
  }

  return matched;
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
static BOOL mem_trace_links(LmnSymbolAtomRef a1,
                                LmnSymbolAtomRef a2,
                                Vector *v_log1,
                                Vector *v_log2,
                                int current_depth,
                                BOOL need_to_check_this_membrane_processes)
{
  unsigned int i;
  int next_depth;

  /* 本メソッドを再帰的に呼び出していく過程で，a1(,a2)へのリンクを辿ることが親膜から子膜への遷移を意味する場合，
   * a1, a2の所属膜を対象とした同型性判定を行う必要がある */
  if (need_to_check_this_membrane_processes) {
    LmnMembraneRef mem1, mem2;

    /* ここの処理をする際，a1およびa2は共にプロキシアトムのはず */
    LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a1)) &&
               LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(a2)));

    mem1 = LMN_PROXY_GET_MEM(a1);
    mem2 = LMN_PROXY_GET_MEM(a2);

    if (!mem_equals_rec(mem1, NULL, mem2, NULL, CHECKED_MEM_DEPTH)) {
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
  if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR                &&
      current_depth == CHECKED_MEM_DEPTH                               &&
      LMN_SATOM_GET_ATTR(LMN_SATOM(LMN_SATOM_GET_LINK(a1, 0U)), 1U) !=
          LMN_SATOM_GET_ATTR(LMN_SATOM(LMN_SATOM_GET_LINK(a2, 0U)), 1U)) {
    return FALSE;
  }

  /* a1(= a2) = $in かつa1の所属膜が同型性判定対象膜である場合，a1の第0リンクの接続先である$outは同型性判定対象膜の親膜内の
   * プロセスということになり，トレースの対象に含めてはならない．この基準に基づき，次の変数iの初期値（0 or 1）が決定される． */
  if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR &&
      current_depth == CHECKED_MEM_DEPTH) {
    i = 1;
  } else {
    i = 0;
  }

  for (; i < LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(a1)); i++) {
    LmnLinkAttr attr1, attr2;

    attr1 = LMN_SATOM_GET_ATTR(a1, i);
    attr2 = LMN_SATOM_GET_ATTR(a2, i);

    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      /* アトムa1、a2の第iリンクの接続先が共にデータアトムのケース:
       *   接続先データアトムの値が等しいか検査.
       *   データアトムはa1, a2のみが接続先となるため, a1, a2の次のリンク検査に移行 */
      if (LMN_SATOM_GET_LINK(a1, i) != LMN_SATOM_GET_LINK(a2, i)) {
        return FALSE;
      }
    }
    else if (!LMN_ATTR_IS_DATA(attr1) && !LMN_ATTR_IS_DATA(attr2)) {
      LmnSymbolAtomRef l1, l2;

      /* アトムa1、a2の第iリンクの接続先が共にシンボル(or プロキシ)アトムのケース:
       *  1. 両アトムの第iリンクと接続するリンク番号をチェック
       *  2. 接続先シンボル(or プロキシ)アトムを取得し, ログ上に存在するかどうかをチェック
       *     ログ上にまだ存在しない新規アトムの場合は, 本メソッドを再帰呼び出しして
       *     a1およびa2を起点とする分子全体のマッチングを行う) */

      if (attr1 != attr2) {
        /* {c(X,Y), c(X,Y)} vs. {c(X,Y), c(Y,X)}
         * の例のように、2アトム間のリンクの接続順序が異なっている場合はFALSEを返す */
        return FALSE;
      }

      l1 = LMN_SATOM(LMN_SATOM_GET_LINK(a1, i));
      l2 = LMN_SATOM(LMN_SATOM_GET_LINK(a2, i));

      if ((vec_contains(v_log1, (LmnWord)l1) != vec_contains(v_log2, (LmnWord)l2))) {
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
      next_depth = current_depth;
      if (LMN_SATOM_GET_FUNCTOR(l1) == LMN_IN_PROXY_FUNCTOR   &&
          LMN_SATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR  &&
          LMN_PROXY_GET_MEM(l1)    != LMN_PROXY_GET_MEM(a1))
      {
        next_depth++;
      }
      else if (LMN_SATOM_GET_FUNCTOR(l1) == LMN_OUT_PROXY_FUNCTOR &&
               LMN_SATOM_GET_FUNCTOR(a1) == LMN_IN_PROXY_FUNCTOR  &&
               LMN_PROXY_GET_MEM(l1) != LMN_PROXY_GET_MEM(a1))
      {
        next_depth--;
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
      if (!mem_trace_links(l1, l2, v_log1, v_log2, next_depth, (next_depth > current_depth))) {
        return FALSE;
      }
    }
    else {
       /* アトムa1、a2の第iリンクの接続先アトムの種類(symbol or data)が一致しないケース */
       return FALSE;
    }
  }

  return TRUE;
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
static Vector *mem_mk_matching_vec(LmnMembraneRef mem)
{
  Vector *vec, *v_tmp;
  LmnFunctor f;
  LmnSymbolAtomRef a;
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

    /* 本膜直下のアトムの内、ファンクタがfであるもののアドレスをベクターatom_ptrs内に整理する。
     * 後でソートする関係で、最も多くのアトムのアドレスを管理する構造体(atomvec_data)内のアトム数を求めている。 */
    EACH_ATOM(a, ent, ({
      vec_push(ad->atom_ptrs, (LmnWord)a);
      if (vec_num(ad->atom_ptrs) > anum_max) {
        anum_max = vec_num(ad->atom_ptrs);
      }
    }));
    /* ファンクタfを持つアトムが本膜内に1つも存在しない場合、このファンクタのために割いたメモリーの領域を解放する。
     * これを怠るとメモリリークが起こるので注意!! */
    if (vec_is_empty(ad->atom_ptrs)) {
      vec_free(ad->atom_ptrs);
      LMN_FREE(ad);
    }
    else {
      vec_push(vec, (vec_data_t)ad);
    }
  }));

  /* sort */
  if (anum_max > 0) {
    v_tmp = vec_make(vec_num(vec));

    for (i = 1; i <= anum_max; i++) {
      for (j = 0; j < vec_num(vec); j++) {
        atomvec_data *ad = (atomvec_data *)vec_get(vec, j);
        if (vec_num(ad->atom_ptrs) == i) {
          vec_push(v_tmp, (vec_data_t)ad);
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


/* 未使用コード:
 * mem_mk_matching_vec/1 の戻り値である2つのVectorの比較を行う
 * 2つの膜内に存在するプロセスの種類および個数が完全に一致するならばTRUEを返す */
BOOL mem_is_the_same_matching_vec(Vector *vec1, Vector *vec2)
{
  unsigned int i, j;

  for (i = 0; i < vec_num(vec1); i++) {
    BOOL is_the_same_functor;

    is_the_same_functor = FALSE;
    for (j = 0; j < vec_num(vec2); j++) {
      if (((atomvec_data *)vec_get(vec1, i))->fid ==
           ((atomvec_data *)vec_get(vec2, j))->fid) {
        is_the_same_functor = TRUE;
        break;
      }
    }

    if (!is_the_same_functor ||
        vec_num(((atomvec_data *)vec_get(vec1, i))->atom_ptrs) !=
        vec_num(((atomvec_data *)vec_get(vec2, j))->atom_ptrs)) {
      return FALSE;
    }
  }

  return TRUE;
}


/* 膜parentから膜memを取り除く.
 * memのメモリ管理は呼び出し側で行う. */
void lmn_mem_remove_mem(LmnMembraneRef parent, LmnMembraneRef mem) {
  LMN_ASSERT(parent);
  if (lmn_mem_child_head(parent) == mem) parent->child_head = lmn_mem_next(mem);
  if (lmn_mem_prev(mem)) mem->prev->next = lmn_mem_next(mem);
  if (lmn_mem_next(mem)) mem->next->prev = lmn_mem_prev(mem);
}

/* 膜mem以下全ての階層のメモリを破棄する */
void lmn_mem_free_rec(LmnMembraneRef mem) {
  lmn_mem_drop(mem);
  lmn_mem_free(mem);
}

/* 膜parentから膜memを取り除き, mem以下の階層全てを解放する. */
void lmn_mem_delete_mem(LmnMembraneRef parent, LmnMembraneRef mem) {
  lmn_mem_remove_mem(parent, mem);
  lmn_mem_free_rec(mem);
}

AtomListEntry* lmn_mem_get_atomlist(LmnMembraneRef mem, LmnFunctor f) {
  if ((f < mem->atomset_size) && mem->atomset[f]) {
    return mem->atomset[f];
  } else {
    return NULL;
  }
}

/* 自身を含めた全ての先祖膜を起こす */
void lmn_mem_activate_ancestors(LmnMembraneRef mem) {
  LmnMembraneRef cur;
  for (cur = mem; cur; cur = lmn_mem_parent(cur)) {
    lmn_mem_set_active(mem, TRUE);
  }
}

BOOL lmn_mem_nmems(LmnMembraneRef mem, unsigned int count) {
  unsigned int i;
  LmnMembraneRef mp = lmn_mem_child_head(mem);
  for(i = 0; mp && i <= count; mp = lmn_mem_next(mp), i++);
  return i == count;
}


/* 子膜の数を返す */
int lmn_mem_child_mem_num(LmnMembraneRef mem) {
  unsigned int i;
  LmnMembraneRef mp = lmn_mem_child_head(mem);
  for(i = 0; mp; mp = lmn_mem_next(mp), i++);
  return i;
}

/* add newmem to parent child membranes */
void lmn_mem_add_child_mem(LmnMembraneRef parentmem,
                                         LmnMembraneRef newmem) {
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
LmnSymbolAtomRef lmn_mem_newatom(LmnMembraneRef mem, LmnFunctor f) {
  LmnSymbolAtomRef atom = lmn_new_atom(f);
  mem_push_symbol_atom(mem, atom);
  return atom;
}

/* return # of child membranes */
unsigned int lmn_mem_count_children(LmnMembraneRef mem) {
  LmnMembraneRef c;
  unsigned int n = 0;
  for (c = lmn_mem_child_head(mem); c; c = lmn_mem_next(c)) n++;
  return n;
}

/* return # of descendant membranes */
unsigned int lmn_mem_count_descendants(LmnMembraneRef mem) {
  LmnMembraneRef c;
  unsigned int n = 0;

  for (c = lmn_mem_child_head(mem); c; c = lmn_mem_next(c)) {
    n += 1 + lmn_mem_count_descendants(c);
  }
  return n;
}


/* return TRUE if # of freelinks in mem is equal to count */
BOOL lmn_mem_nfreelinks(LmnMembraneRef mem, unsigned int count) {
  AtomListEntry *ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);
  if (!ent) {
    return count == 0;
  } else {
#ifdef NEW_ATOMLIST
    return count == ent->n;

#else
    LmnSymbolAtomRef atom;
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

void lmn_mem_remove_data_atom(LmnMembraneRef mem, LmnDataAtomRef atom, LmnLinkAttr attr) {
  lmn_mem_data_atom_dec(mem);
}

void mem_remove_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom) {
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
    LMN_PROXY_SET_MEM(atom, NULL);
  } else if (f != LMN_UNIFY_FUNCTOR) {
    lmn_mem_symb_atom_dec(mem);
  }
}

/* 膜memからアトムatomを取り除く.
 * atomの接続先データアトムが存在するならば, そのデータアトムも取り除く.
 * atom自体のメモリ管理は呼び出し側が行う. */
void mem_remove_symbol_atom_with_buddy_data(LmnMembraneRef mem, LmnSymbolAtomRef atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(LMN_SATOM_GET_ATTR(atom, i))) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i), LMN_SATOM_GET_ATTR(atom, i));
    } else if (LMN_ATTR_IS_HL(LMN_SATOM_GET_ATTR(atom, i))) {
      mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, i));
    }
  }
  mem_remove_symbol_atom(mem, atom);
}

void lmn_mem_remove_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)atom, attr);
  } else {
    mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)atom);
  }
}

void move_atom_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem){
  //move_symbol_atom_to_atomlist_tail(LMN_SATOM(a), mem);
  mem_remove_symbol_atom(mem,LMN_SATOM(a));
  mem_push_symbol_atom(mem,LMN_SATOM(a));
}

void move_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem){
  //  move_symbol_atom_to_atomlist_head(LMN_SATOM(a), mem); // ueda
  move_symbol_atom_to_atomlist_head(a, mem);
  }

void move_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem){
  move_symbol_atomlist_to_atomlist_tail(a, mem);
}

void move_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1, LmnMembraneRef mem){
  move_symbol_atom_to_atom_tail(a, a1, mem);
  }

void lmn_mem_delete_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr) {
  lmn_mem_remove_atom(mem, atom, attr);
  lmn_free_atom(atom, attr);
}

void lmn_mem_push_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    lmn_mem_data_atom_inc(mem);
  } else { /* symbol atom */
    mem_push_symbol_atom(mem, LMN_SATOM(atom));
  }
}

void alter_functor(LmnMembraneRef mem, LmnSymbolAtomRef atom, LmnFunctor f) {
  mem_remove_symbol_atom(mem, atom);
  LMN_SATOM_SET_FUNCTOR(atom, f);
  mem_push_symbol_atom(mem, atom);
}

/* ルールセットnewを膜memに追加する */
void lmn_mem_add_ruleset(LmnMembraneRef mem, LmnRuleSetRef ruleset) {
  LMN_ASSERT(ruleset);
  lmn_mem_add_ruleset_sort(&(mem->rulesets), ruleset);
}

void lmn_mem_copy_rules(LmnMembraneRef dest, LmnMembraneRef src) {
  int i;
  for (i = 0; i< lmn_mem_ruleset_num(src); i++) {
    lmn_mem_add_ruleset(dest, lmn_ruleset_copy(lmn_mem_get_ruleset(src, i)));
  }
}

void lmn_mem_clearrules(LmnMembraneRef src) {
  unsigned int i;
  for (i = 0; i < vec_num(&src->rulesets); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(&src->rulesets, i);
    if (lmn_ruleset_is_copy(rs)) {
      lmn_ruleset_copied_free(rs);
    }
  }
  vec_clear(&src->rulesets);
}

/* シンボルアトムatom0と、シンボルorデータアトムatom1の間にリンクを張る。*/
void newlink_symbol_and_something(LmnSymbolAtomRef atom0,
                                                int pos,
                                                LmnAtomRef atom1,
                                                LmnLinkAttr attr) {
  LMN_SATOM_SET_LINK(atom0, pos, atom1);
  LMN_SATOM_SET_ATTR(atom0, pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    LMN_SATOM_SET_LINK(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), atom0);
    LMN_SATOM_SET_ATTR(LMN_SATOM(atom1), LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos));
  }
}
#ifdef USE_FIRSTCLASS_RULE
Vector *lmn_mem_firstclass_rulesets(LmnMembraneRef mem) {
  return mem->firstclass_rulesets;
}
#endif

#ifdef USE_FIRSTCLASS_RULE
void lmn_mem_add_firstclass_ruleset(LmnMembraneRef mem, LmnRuleSetRef fcr) {
  LMN_ASSERT(fcr);
  lmn_mem_add_ruleset_sort(mem->firstclass_rulesets, fcr);
}
#endif

#ifdef USE_FIRSTCLASS_RULE
void lmn_mem_remove_firstclass_ruleset(LmnMembraneRef mem, LmnRuleSetRef fcr) {
  LmnRulesetId del_id = lmn_ruleset_get_id(fcr);
  
  for(int i = 0; i < vec_num(mem->firstclass_rulesets); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(mem->firstclass_rulesets, i);
    if (lmn_ruleset_get_id(rs) != del_id) continue;
    
    /* move successors forward */
    for(int j = i; j < vec_num(mem->firstclass_rulesets) - 1; j++) {
      LmnRuleSetRef next = (LmnRuleSetRef)vec_get(mem->firstclass_rulesets, j + 1);
      vec_set(mem->firstclass_rulesets, j, (vec_data_t)next);
    }

    mem->firstclass_rulesets->num--;
    return;
  }

  LMN_ASSERT(FALSE); // "attempt to delete an absent firstclass ruleset"
}
#endif
