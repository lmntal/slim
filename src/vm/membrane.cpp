/*
 * membrane.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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
#include "dumper.h" /* for debug */
#include "functor.h"
#include "rule.h"
#include "verifier/verifier.h"
#include <ctype.h>
#include <limits.h>

#ifdef PROFILE
#include "verifier/runtime_status.h"
#endif

#ifdef USE_FIRSTCLASS_RULE
#include "firstclass_rule.h"
#endif

#include "membrane.hpp"
#include "rule.hpp"
#include <cassert>

/** ----
 *  AtomListEntry.
 *  同一ファンクタのアトムをリスト単位でまとめておくための機構
 */

/* この構造体をAtomとして扱うことで,この構造体自身が
   HeadとTailの両方の役目を果たしている */

LmnSymbolAtomRef atomlist_head(AtomListEntryRef lst) { return lst->head; }
LmnSymbolAtomRef lmn_atomlist_end(AtomListEntryRef lst) {
  return (LmnSymbolAtomRef)lst;
}

static void lmn_mem_copy_cells_sub(LmnMembraneRef destmem,
                                   LmnMembraneRef srcmem, ProcessTableRef atoms,
                                   BOOL hl_nd);

typedef int AtomListIter;
#define atomlist_iter_initializer(AS) (0)
#define atomlist_iter_condition(Mem, Iter) ((Iter) < Mem->mem_max_functor())
#define atomlist_iter_next(Iter) ((Iter)++)
#define atomlist_iter_get_entry(Mem, Iter) Mem->get_atomlist(Iter)
#define atomlist_iter_get_functor(Iter) (Iter)

/* ルールセットadd_rsをルールセット配列src_vへ追加する.
 * グラフ同型性判定処理などのために整数IDの昇順を維持するよう追加する. */
void lmn_mem_add_ruleset_sort(std::vector<LmnRuleSet *> *src_v, LmnRuleSetRef add_rs) {
  int i, j, n;
  LmnRulesetId add_id;
  add_id = add_rs->id;
  n = src_v->size();
  for (i = 0; i < n; i++) {
    LmnRuleSetRef rs_i;
    LmnRulesetId dst_id;

    rs_i = (*src_v)[i];
    dst_id = rs_i->id;

    if (dst_id == add_id && !add_rs->has_unique()) {
      /* 同じ階層にuniqでない同一のルールセットが既に存在するならば追加する必要はない
       */
      break;
    } else if (dst_id >= add_id) {
      LmnRuleSetRef prev = add_rs;
      src_v->push_back(nullptr);

      for (j = i; j < (n + 1); j++) {
        std::swap((*src_v)[j], prev);
      }
      break;
    }
  }
  if (i == n) {
    src_v->push_back(add_rs);
  }
}

/*----------------------------------------------------------------------
 * Atom Set
 */

static inline AtomListEntry *make_atomlist(void);
static inline void free_atomlist(AtomListEntry *as);

/* 新しいアトムリストを作る */
static inline AtomListEntry *make_atomlist() {
  AtomListEntry *as = LMN_MALLOC(struct AtomListEntry);
  as->record =
      NULL; /* 全てのアトムの種類に対してfindatom2用ハッシュ表が必要なわけではないので動的にmallocさせる
             */
  as->set_empty();

  return as;
}

/* アトムリストの解放処理 */
static inline void free_atomlist(AtomListEntry *as) {
  /* lmn_mem_move_cellsでアトムリストの再利用を行っていて
   * ポインタがNULLになる場合があるので、検査を行う必要がある。*/
  if (as) {
    if (as->record) {
      hashtbl_free(as->record);
    }
    LMN_FREE(as);
  }
}

LmnMembrane::LmnMembrane(){
  this->parent = NULL;
  this->child_head = NULL;
  this->prev = NULL;
  this->next = NULL;
  this->max_functor = 0U;
  this->atomset_size = 32;
  this->is_activated = TRUE;
  this->atom_symb_num = 0U;
  this->atom_data_num = 0U;
  this->name = ANONYMOUS;
  this->id = 0UL;
  this->atomset = LMN_CALLOC(struct AtomListEntry *, this->atomset_size);
  this->set_id(env_gen_next_id());

#ifdef USE_FIRSTCLASS_RULE
  this->firstclass_rulesets = new Vector(4);
#endif

}
const char *LmnMembrane::MEM_NAME() {
  return LMN_SYMBOL_STR(this->NAME_ID());
}
LmnRuleSetRef lmn_mem_get_ruleset(LmnMembraneRef m, int i) {
  return (LmnRuleSetRef)m->get_rulesets()[i];
}


/* 膜memの解放を行う.
 * 膜memに所属する子膜とアトムのメモリ管理は呼び出し側で行う. */
LmnMembrane::~LmnMembrane(){
  /* free all atomlists  */
  for (int i = 0; i < max_functor; i++) {
    if (!this->atomset[i])
      continue;
    auto &as = this->atomset[i];
    if (as->record) {
      hashtbl_free(as->record);
    }
    delete as;
  }

  for (int i = 0; i < this->rulesets.size(); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)this->rulesets[i];
    
    if (rs->is_copy()) {
      delete rs;
    }
  }
  this->rulesets.clear();
  
  env_return_id(this->mem_id());
#ifdef USE_FIRSTCLASS_RULE
  delete this->firstclass_rulesets;
#endif
  delete this->atomset;
}

/* 膜mem内のアトム, 子膜のメモリを解放する.
 * 子膜が存在する場合は, その子膜に再帰する. */
void LmnMembrane::drop() {
  LmnMembraneRef m;

  /* drop and free child mems */
  m = this->child_head;
  while (m) {
    auto n = m;
    m = m->next;
    n->free_rec();
  }
  this->child_head = nullptr;

  
  for (int i = 0; i < max_functor; i++) {
    if (!atomset[i])
      continue;
    /* hyperlinkはbuddy symbol atomと一緒に削除されるため */
    if (LMN_FUNC_IS_HL(i))
      continue;
    
    for (auto a : *atomset[i]) {
      free_symbol_atom_with_buddy_data(a);
    }
    
    atomset[i]->set_empty();
  }
  this->atom_symb_num = 0U;
  this->atom_data_num = 0U;
}

void lmn_mem_rulesets_destroy(const std::vector<LmnRuleSet *> &rulesets) {
  for (auto &ref : rulesets) {
    if (ref->is_copy())
      delete ref;
  }
}

/* ルールセット配列rulesetsを解放する */
void lmn_mem_rulesets_destroy(Vector *rulesets) {
  unsigned int i, n = rulesets->get_num();

  for (i = 0; i < n; i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)rulesets->get(i);

    if (rs->is_copy()) {
      delete rs;
    }
  }
  rulesets->destroy();
}
void move_symbol_atom_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem) {
  LmnFunctor f = a->get_functor();
  AtomListEntry *ent = mem->get_atomlist(f);

  a->get_next()->set_prev(a->get_prev());
  a->get_prev()->set_next(a->get_next());

  a->set_next((LmnSymbolAtomRef)ent);
  a->set_prev(ent->tail);
  ent->tail->set_next(a);
  ent->tail = a;
}

void move_symbol_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem) {
  LmnFunctor f = a->get_functor();
  AtomListEntry *ent = mem->get_atomlist(f);

  a->get_next()->set_prev(a->get_prev());
  a->get_prev()->set_next(a->get_next());

  a->set_next(ent->head);
  a->set_prev((LmnSymbolAtomRef)ent);
  ent->head->set_prev(a);
  ent->head = a;
}
void move_symbol_atomlist_to_atomlist_tail(LmnSymbolAtomRef a,
                                           LmnMembraneRef mem) {
  LmnFunctor f = a->get_functor();
  AtomListEntry *ent = mem->get_atomlist(f);

  if (a != ent->head) {
    ent->tail->set_next(ent->head);
    ent->head->set_prev(ent->tail);
    ent->tail = a->get_prev();
    a->get_prev()->set_next((LmnSymbolAtomRef)ent);
    a->set_prev((LmnSymbolAtomRef)ent);
    ent->head = a;
  }
}
void move_symbol_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                                   LmnMembraneRef mem) {
  LmnFunctor f = a->get_functor();
  AtomListEntry *ent = mem->get_atomlist(f);

  if (ent->tail == a1)
    ent->tail = a1->get_prev();
  else if (ent->head == a1)
    ent->head = a1->get_next();

  a1->get_next()->set_prev(a1->get_prev());
  a1->get_prev()->set_next(a1->get_next());

  if (ent->tail == a->get_next())
    ent->tail = a1;

  a->get_next()->set_prev(a1);
  a1->set_next(a->get_next());
  a1->set_prev(a);
  a->set_next(a1);
}

void mem_push_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom) {
  AtomListEntry *as;
  LmnFunctor f = atom->get_functor();

  // if文も含めてロックが必要。今後要検証
  mut.lock();
  if (atom->get_id() == 0) { /* 膜にpushしたならばidを割り当てる */
    atom->set_id(env_gen_next_id());
  }
  mut.unlock();

  as = mem->get_atomlist(f);
  if (!as) { /* 本膜内に初めてアトムatomがPUSHされた場合 */
    LMN_ASSERT(
        mem->atomset); /* interpreter側で値がオーバーフローすると発生するので,
                          ここで止める */
    if (mem->max_functor < f + 1) {
      mem->max_functor = f + 1;
      while (mem->atomset_size - 1 < mem->max_functor) {
        int org_size = mem->atomset_size;
        mem->atomset_size *= 2;
        LMN_ASSERT(mem->atomset_size > 0);
        mem->atomset = LMN_REALLOC(struct AtomListEntry *, mem->atomset,
                                   mem->atomset_size);
        memset(mem->atomset + org_size, 0,
               (mem->atomset_size - org_size) * sizeof(struct AtomListEntry *));
      }
    }
    as = mem->atomset[f] = make_atomlist();
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, mem);
  } else if (LMN_FUNC_IS_HL(f)) {
    LMN_HL_MEM(lmn_hyperlink_at_to_hl(atom)) = mem;
    mem->symb_atom_inc();
  } else if (f != LMN_UNIFY_FUNCTOR) {
    /* symbol atom except proxy and unify */
    mem->symb_atom_inc();
  }

  as->push(atom);
}
unsigned long LmnMembrane::space() {
  AtomListEntry *ent;
  unsigned long ret;
  unsigned int i;

  ret = 0;
  ret += sizeof(struct LmnMembrane);
  ret += sizeof(struct AtomListEntry *) * this->atomset_size;
  /* atomset */
  EACH_ATOMLIST(this, ent, ({
                  LmnSymbolAtomRef atom;
                  ret += sizeof(struct AtomListEntry);
                  if (ent->record) {
                    ret += internal_hashtbl_space(ent->record);
                  }
                  EACH_ATOM(atom, ent, ({
                              ret += LMN_SATOM_SIZE(LMN_FUNCTOR_ARITY(lmn_functor_table, 
                                  atom->get_functor()));
                            }));
                }));

  /* ruleset */
  ret += this->rulesets.capacity() * sizeof(decltype(this->rulesets)::value_type);
  for (i = 0; i < this->rulesets.size(); i++) {
    LmnRuleSetRef rs = this->rulesets[i];
    if (rs->is_copy()) {
      ret += rs->space();
    }
  }

  return ret;
}
unsigned long LmnMembrane::root_space() {
  unsigned long ret;
  LmnMembraneRef ptr;

  ret = this->space();
  for (ptr = this->child_head; ptr != NULL; ptr = ptr->next) {
    ret += ptr->root_space();
  }

  return ret;
}

void lmn_mem_link_data_atoms(LmnMembraneRef mem, LmnAtomRef d0,
                             LmnLinkAttr attr0, LmnAtomRef d1,
                             LmnLinkAttr attr1) {
  LmnSymbolAtomRef ap = lmn_new_atom(LMN_UNIFY_FUNCTOR);

  ap->set_link(0, d0);
  ap->set_link(1, d1);
  ap->set_attr(0, attr0);
  ap->set_attr(1, attr1);
  mem_push_symbol_atom(mem, ap);
}

/* atom1, atom2をシンボルアトムに限定した unify link */
void lmn_mem_unify_symbol_atom_args(LmnSymbolAtomRef atom1, int pos1,
                                    LmnSymbolAtomRef atom2, int pos2) {
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  ap1 = atom1->get_link(pos1);
  attr1 = atom1->get_attr(pos1);
  ap2 = atom2->get_link(pos2);
  attr2 = atom2->get_attr(pos2);

  ((LmnSymbolAtomRef)ap2)->set_link(attr2, ap1);
  ((LmnSymbolAtomRef)ap2)->set_attr(attr2, attr1);
  ((LmnSymbolAtomRef)ap1)->set_link(attr1, ap2);
  ((LmnSymbolAtomRef)ap1)->set_attr(attr1, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void lmn_mem_unify_atom_args(LmnMembraneRef mem, LmnSymbolAtomRef atom1,
                             int pos1, LmnSymbolAtomRef atom2, int pos2) {
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  ap1 = atom1->get_link(pos1);
  attr1 = atom1->get_attr(pos1);
  ap2 = atom2->get_link(pos2);
  attr2 = atom2->get_attr(pos2);

  if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
    lmn_mem_link_data_atoms(mem, ap1, attr1, ap2, attr2);
  } else if (LMN_ATTR_IS_DATA(attr1)) {
    ((LmnSymbolAtomRef)ap2)->set_link(attr2, ap1);
    ((LmnSymbolAtomRef)ap2)->set_attr(attr2, attr1);
  } else if (LMN_ATTR_IS_DATA(attr2)) {
    ((LmnSymbolAtomRef)ap1)->set_link(attr1, ap2);
    ((LmnSymbolAtomRef)ap1)->set_attr(attr1, attr2);
  } else {
    ((LmnSymbolAtomRef)ap2)->set_link(attr2, ap1);
    ((LmnSymbolAtomRef)ap2)->set_attr(attr2, attr1);
    ((LmnSymbolAtomRef)ap1)->set_link(attr1, ap2);
    ((LmnSymbolAtomRef)ap1)->set_attr(attr1, attr2);
  }
}

/* シンボルアトムに限定したnewlink */
void lmn_newlink_in_symbols(LmnSymbolAtomRef atom0, int pos0,
                            LmnSymbolAtomRef atom1, int pos1) {
  atom0->set_link(pos0, atom1);
  atom1->set_link(pos1, atom0);
  atom0->set_attr(pos0, LMN_ATTR_MAKE_LINK(pos1));
  atom1->set_attr(pos1, LMN_ATTR_MAKE_LINK(pos0));
}

void lmn_newlink_with_ex(LmnMembraneRef mem, LmnSymbolAtomRef atom0,
                         LmnLinkAttr attr0, int pos0, LmnSymbolAtomRef atom1,
                         LmnLinkAttr attr1, int pos1) {
  /* both symbol */
  atom0->set_link(pos0, atom1);
  atom1->set_link(pos1, atom0);

  if (LMN_ATTR_IS_EX(attr0)) {
    if (LMN_ATTR_IS_EX(attr1)) { /* 0, 1 are ex */
                                 //      atom0->set_attr(pos0, attr1);
                                 //      atom1->set_attr(pos1, attr0);
      /* 現状では、hyperlinkアトム同士が接続されると消去される */
      //      lmn_mem_delete_atom(mem, LMN_ATOM(atom0), attr0);
      //      lmn_mem_delete_atom(mem, LMN_ATOM(atom1), attr1);
      lmn_fatal("Two hyperlinks cannot be connected using = .");
    } else { /* 0 is ex */
      atom0->set_attr(pos0, LMN_ATTR_MAKE_LINK(pos1));
      atom1->set_attr(pos1, attr0);
    }
  } else { /* 1 is ex */
    atom0->set_attr(pos0, attr1);
    atom1->set_attr(pos1, LMN_ATTR_MAKE_LINK(pos0));
  }
}

/* シンボルアトムatom0と、シンボルorデータアトムatom1の間にリンクを張る。
 * このコードが重複して現れたので、関数に分割した */
/* static inline void newlink_symbol_and_something(LmnSymbolAtomRef atom0,
                                                int pos,
                                                LmnAtomRef atom1,
                                                LmnLinkAttr attr)
{
  atom0->set_link(pos, atom1);
  atom0->set_attr(pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    LMN_SATOM(atom1)->set_link(LMN_ATTR_GET_VALUE(attr), atom0);
    LMN_SATOM(atom1)->set_attr(LMN_ATTR_GET_VALUE(attr),LMN_ATTR_MAKE_LINK(pos));
  }
} */

static inline void newlink_symbol_and_hlink(LmnSymbolAtomRef atom, int pos,
                                            LmnSymbolAtomRef hlAtom) LMN_UNUSED;
/* シンボルアトムatomと、ハイパーリンクアトムhlAtomの間にリンクを張る。
 * ハイパーリンクは接続の引数が0だったりと特殊なので関数も用意した。*/
static inline void newlink_symbol_and_hlink(LmnSymbolAtomRef atom, int pos,
                                            LmnSymbolAtomRef hlAtom) {
  atom->set_link(pos, hlAtom);
  hlAtom->set_link(0, atom);
}

void lmn_mem_newlink(LmnMembraneRef mem, LmnAtomRef atom0, LmnLinkAttr attr0,
                     int pos0, LmnAtomRef atom1, LmnLinkAttr attr1, int pos1) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr0)) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) { /* both data */
      lmn_mem_link_data_atoms(mem, atom0, attr0, atom1, attr1);
    } else { /* atom0 data, atom1 symbol */
      ((LmnSymbolAtomRef)atom1)->set_link(pos1, atom0);
      ((LmnSymbolAtomRef)atom1)->set_attr(pos1, attr0);
    }
  } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(
                 attr1)) { /* atom0 symbol, atom1 data */
    ((LmnSymbolAtomRef)atom0)->set_link(pos0, atom1);
    ((LmnSymbolAtomRef)atom0)->set_attr(pos0, attr1);
  } else { /* both symbol */
    if (!LMN_ATTR_IS_EX(attr0) && !LMN_ATTR_IS_EX(attr1))
      lmn_newlink_in_symbols((LmnSymbolAtomRef)atom0, pos0,
                             (LmnSymbolAtomRef)atom1, pos1);
    else
      lmn_newlink_with_ex(mem, (LmnSymbolAtomRef)atom0, attr0, pos0,
                          (LmnSymbolAtomRef)atom1, attr1, pos1);
  }
}

void lmn_relink_symbols(LmnSymbolAtomRef atom0, int pos0,
                        LmnSymbolAtomRef atom1, int pos1) {
  newlink_symbol_and_something(
      (LmnSymbolAtomRef)atom0, pos0,
      ((LmnSymbolAtomRef)atom1)->get_link(pos1),
      ((LmnSymbolAtomRef)atom1)->get_attr(pos1));
}

void lmn_mem_relink_atom_args(LmnMembraneRef mem, LmnAtomRef atom0,
                              LmnLinkAttr attr0, int pos0, LmnAtomRef atom1,
                              LmnLinkAttr attr1, int pos1) {
  /* TODO: relinkではatom0,atom1がデータになることはないはず
   *       このことを確認する */
  LMN_ASSERT(!LMN_ATTR_IS_DATA(attr0) && !LMN_ATTR_IS_DATA(attr1));

  newlink_symbol_and_something(
      (LmnSymbolAtomRef)atom0, pos0,
      ((LmnSymbolAtomRef)atom1)->get_link(pos1),
      ((LmnSymbolAtomRef)atom1)->get_attr(pos1));
}
void LmnMembrane::move_cells(LmnMembraneRef srcmem) {
  AtomListEntry *srcent;
  LmnMembraneRef m, next;
  int dst_data_atom_n, src_data_atom_n;

  dst_data_atom_n = this->data_atom_num();
  src_data_atom_n = srcmem->data_atom_num();

  /* move atoms */
  EACH_ATOMLIST(
      srcmem, srcent, ({
        LmnSymbolAtomRef a, next;

        for (a = atomlist_head((srcent)); a != lmn_atomlist_end((srcent));
             a = next) {
          next = a->get_next();

#ifdef USE_FIRSTCLASS_RULE
          if (a->get_functor() == LMN_COLON_MINUS_FUNCTOR) {
            LmnRuleSetRef rs = firstclass_ruleset_lookup(a);
            lmn_mem_remove_firstclass_ruleset(srcmem, rs);
            lmn_mem_add_firstclass_ruleset(this, rs);
          }
#endif

          if (a->get_functor() != LMN_RESUME_FUNCTOR) {
            int i, arity;

            mem_remove_symbol_atom(srcmem, a);
            mem_push_symbol_atom(this, a);
            arity = a->get_link_num();
            for (i = 0; i < arity; i++) {
              if (LMN_ATTR_IS_DATA_WITHOUT_EX(a->get_attr(i))) {
                lmn_mem_push_atom(this, a->get_link(i),
                                  a->get_attr(i));
              }
            }
          }
        }
      }));

  /* move membranes */
  for (m = srcmem->child_head; m; m = next) {
    next = m->next;
    srcmem->remove_mem(m);
    this->add_child_mem(m);
  }

  if (src_data_atom_n > this->data_atom_num() - dst_data_atom_n) {
    this->data_atom_set(dst_data_atom_n + src_data_atom_n);
  }
}

//#define REMOVE            1
//#define STATE(ATOM)        ((ATOM)->get_attr(2))
//#define SET_STATE(ATOM,S)  ((ATOM)->set_attr(2, (S)))

/* cf. Java処理系
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 *
 * 2011/01/23  処理を変更 meguro
 */
void LmnMembrane::remove_proxies() {
  struct Vector remove_list_p, remove_list_m, change_list;
  AtomListEntry *ent;
  unsigned int i;

  ent = this->get_atomlist(LMN_IN_PROXY_FUNCTOR);

  remove_list_p.init(16); /* parent用 */
  remove_list_m.init(16); /* mem用 */
  change_list.init(16);

  if (ent) {
    LmnSymbolAtomRef ipxy;

    EACH_ATOM(ipxy, ent, ({
                LmnSymbolAtomRef a0, a1;
                LmnFunctor f0, f1;

                if (!LMN_ATTR_IS_DATA(ipxy->get_attr(1))) {
                  a0 = (LmnSymbolAtomRef)ipxy->get_link(1);
                  f0 = a0->get_functor();

                  if (f0 == LMN_STAR_PROXY_FUNCTOR) {
                    /* -$*-$in- → ----- */
                    lmn_mem_unify_atom_args(this, a0, 0, ipxy, 0);
                    remove_list_m.push((LmnWord)a0);
                    remove_list_m.push((LmnWord)ipxy);
                  } else {
                    /* -$in- → -$*- */
                    change_list.push((LmnWord)ipxy);
                  }
                } else {
                  /* -$in- → -$*- */
                  change_list.push((LmnWord)ipxy);
                }

                a1 = (LmnSymbolAtomRef)ipxy->get_link(0);
                f1 = a1->get_functor();

                if (f1 == LMN_OUT_PROXY_FUNCTOR) {
                  if (!LMN_ATTR_IS_DATA(a1->get_attr(1))) {
                    LmnSymbolAtomRef a2;
                    LmnFunctor f2;

                    a2 = (LmnSymbolAtomRef)a1->get_link(1);
                    f2 = a2->get_functor();

                    if (f2 == LMN_STAR_PROXY_FUNCTOR) {
                      lmn_mem_unify_atom_args(this->parent, a1, 0, a2, 0);
                      remove_list_p.push((LmnWord)a1);
                      remove_list_p.push((LmnWord)a2);
                    } else {
                      change_list.push((LmnWord)a1);
                    }
                  }
                }
              }));
  }

  for (i = 0; i < remove_list_p.get_num(); i++) {
    mem_remove_symbol_atom(this->parent,
                           (LmnSymbolAtomRef)remove_list_p.get(i));
    lmn_delete_atom((LmnSymbolAtomRef)remove_list_p.get(i));
  }
  remove_list_p.destroy();

  for (i = 0; i < remove_list_m.get_num(); i++) {
    mem_remove_symbol_atom(this, (LmnSymbolAtomRef)remove_list_m.get(i));
    lmn_delete_atom((LmnSymbolAtomRef)remove_list_m.get(i));
  }
  remove_list_m.destroy();

  /* change to star proxy */
  for (i = 0; i < change_list.get_num(); i++) {
    alter_functor(LMN_PROXY_GET_MEM((LmnSymbolAtomRef)change_list.get(i)),
                  (LmnSymbolAtomRef)change_list.get(i),
                  LMN_STAR_PROXY_FUNCTOR);
  }
  change_list.destroy();
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void LmnMembrane::insert_proxies(LmnMembraneRef child_mem) {
  unsigned int i;
  Vector remove_list, change_list;
  LmnSymbolAtomRef star, oldstar;
  AtomListEntry *ent = child_mem->get_atomlist(LMN_STAR_PROXY_FUNCTOR);

  if (!ent)
    return;

  remove_list.init(16);
  change_list.init(16); /* inside proxy にするアトム */

  EACH_ATOM(star, ent, ({
              oldstar = (LmnSymbolAtomRef)star->get_link(0);
              if (LMN_PROXY_GET_MEM(oldstar) == child_mem) { /* (1) */
                if (!remove_list.contains((LmnWord)star)) {
                  lmn_mem_unify_atom_args(child_mem, star, 1, oldstar, 1);
                  remove_list.push((LmnWord)star);
                  remove_list.push((LmnWord)oldstar);
                }
              } else {
                change_list.push((LmnWord)star);

                if (LMN_PROXY_GET_MEM(oldstar) == this) { /* (2) */
                  alter_functor(this, oldstar, LMN_OUT_PROXY_FUNCTOR);
                  lmn_newlink_in_symbols(star, 0, oldstar, 0);
                } else { /* (3) */
                  LmnSymbolAtomRef outside =
                      lmn_mem_newatom(this, LMN_OUT_PROXY_FUNCTOR);
                  LmnSymbolAtomRef newstar =
                      lmn_mem_newatom(this, LMN_STAR_PROXY_FUNCTOR);
                  lmn_newlink_in_symbols(outside, 1, newstar, 1);
                  lmn_mem_relink_atom_args(this, newstar, LMN_ATTR_MAKE_LINK(0),
                                           0, star, LMN_ATTR_MAKE_LINK(0), 0);
                  lmn_newlink_in_symbols(star, 0, outside, 0);
                }
              }
            }));

  for (i = 0; i < change_list.get_num(); i++) {
    alter_functor(child_mem, (LmnSymbolAtomRef)change_list.get(i),
                  LMN_IN_PROXY_FUNCTOR);
  }
  change_list.destroy();

  for (i = 0; i < remove_list.get_num(); i++) {
    mem_remove_symbol_atom(this, (LmnSymbolAtomRef)remove_list.get(i));
    lmn_delete_atom((LmnSymbolAtomRef)remove_list.get(i));
  }
  remove_list.destroy();
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void LmnMembrane::remove_temporary_proxies() {
  unsigned int i;
  Vector remove_list;
  LmnSymbolAtomRef star, outside;
  AtomListEntry *ent = this->get_atomlist(LMN_STAR_PROXY_FUNCTOR);

  if (!ent)
    return;

  remove_list.init(16);

  EACH_ATOM(star, ent, ({
              outside = (LmnSymbolAtomRef)star->get_link(0);
              if (!remove_list.contains((LmnWord)star)) {
                lmn_mem_unify_atom_args(this, star, 1, outside, 1);
                remove_list.push((LmnWord)star);
                remove_list.push((LmnWord)outside);
              }
            }));

  for (i = 0; i < remove_list.get_num(); i++) {
    mem_remove_symbol_atom(this, (LmnSymbolAtomRef)remove_list.get(i));
    lmn_delete_atom((LmnSymbolAtomRef)remove_list.get(i));
  }

  remove_list.destroy();
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void LmnMembrane::remove_toplevel_proxies() {
  Vector remove_list;
  AtomListEntry *ent;
  LmnSymbolAtomRef outside;
  unsigned int i;

  ent = this->get_atomlist(LMN_OUT_PROXY_FUNCTOR);
  if (!ent)
    return;

  remove_list.init(16);

  EACH_ATOM(
      outside, ent, ({
        LmnSymbolAtomRef a0 = (LmnSymbolAtomRef)outside->get_link(0);
        if (LMN_PROXY_GET_MEM(a0) && LMN_PROXY_GET_MEM(a0)->parent != this) {
          if (!LMN_ATTR_IS_DATA(outside->get_attr(1))) {
            LmnSymbolAtomRef a1 =
                (LmnSymbolAtomRef)outside->get_link(1);
            if (a1->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
              LmnSymbolAtomRef a10 =
                  (LmnSymbolAtomRef)a1->get_link(0);
              if (LMN_PROXY_GET_MEM(a10) &&
                  LMN_PROXY_GET_MEM(a10)->parent != this) {
                if (!remove_list.contains((LmnWord)outside)) {
                  lmn_mem_unify_atom_args(this, outside, 0, a1, 0);
                  remove_list.push((LmnWord)outside);
                  remove_list.push((LmnWord)a1);
                }
              }
            }
          }
        }
      }));

  for (i = 0; i < remove_list.get_num(); i++) {
    mem_remove_symbol_atom(this, (LmnSymbolAtomRef)remove_list.get(i));
    lmn_delete_atom((LmnSymbolAtomRef)remove_list.get(i));
  }
  remove_list.destroy();
}
LmnMembraneRef LmnMembrane::copy() {
  ProcessTableRef copymap;
  LmnMembraneRef copied;

  copied = lmn_mem_copy_with_map(this,&copymap);
  delete copymap;
  return copied;
}
LmnMembraneRef LmnMembrane::copy_ex() {
  ProcessTableRef copymap;
  LmnMembraneRef copied;

  copied = lmn_mem_copy_with_map_ex(this, &copymap);
  delete copymap;
  return copied;
}
static inline LmnMembraneRef lmn_mem_copy_with_map_inner(LmnMembraneRef src, ProcessTableRef *ret_copymap, BOOL hl_nd) {
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

  new_mem = new LmnMembrane();

  if (hl_nd) {
    copymap = lmn_mem_copy_cells_ex(new_mem, src, TRUE);
  } else {
    copymap = lmn_mem_copy_cells(new_mem, src);
  }

  for (i = 0; i < src->rulesets.size(); i++) {
    new_mem->rulesets.push_back(new LmnRuleSet(*src->rulesets[i]));
  }
  *ret_copymap = copymap;

  return new_mem;
}
LmnMembraneRef lmn_mem_copy_with_map_ex(LmnMembraneRef src,
                                        ProcessTableRef *ret_copymap) {
  return lmn_mem_copy_with_map_inner(src, ret_copymap, TRUE);
}

LmnMembraneRef lmn_mem_copy_with_map(LmnMembraneRef src,
                                     ProcessTableRef *ret_copymap) {
  return lmn_mem_copy_with_map_inner(src, ret_copymap, FALSE);
}

ProcessTableRef lmn_mem_copy_cells_ex(LmnMembraneRef dst, LmnMembraneRef src,
                                      BOOL hl_nd) {
  ProcessTableRef atoms = new ProcessTbl(64);
  lmn_mem_copy_cells_sub(dst, src, atoms, hl_nd);
  return atoms;
}

ProcessTableRef lmn_mem_copy_cells(LmnMembraneRef destmem,
                                   LmnMembraneRef srcmem) {
  return lmn_mem_copy_cells_ex(destmem, srcmem, FALSE);
}

/* srcmemの実データ構造をdestmemへコピー生成する. atomsは訪問済みの管理に用いる.
 * hl_ndフラグが真の場合, コピー前とコピー後のハイパーリンクのunifyをしない.  */
static void lmn_mem_copy_cells_sub(LmnMembraneRef destmem,
                                   LmnMembraneRef srcmem, ProcessTableRef atoms,
                                   BOOL hl_nd) {
  unsigned int i;
  LmnMembraneRef m;
  AtomListEntry *ent;

  /* copy child mems */
  for (m = srcmem->child_head; m; m = m->next) {
    LmnMembraneRef new_mem = new LmnMembrane();
    lmn_mem_copy_cells_sub(new_mem, m, atoms, hl_nd);
    destmem->add_child_mem(new_mem);

    atoms->proc_tbl_put_mem(m, (LmnWord)new_mem);
    /* copy name */
    new_mem->name = m->name;
    /* copy rulesets */
    for (i = 0; i < m->rulesets.size(); i++) {
      new_mem->rulesets.push_back(new LmnRuleSet(*m->rulesets[i]));
    }
  }

  /* copy atoms */
  EACH_ATOMLIST(
      srcmem, ent, ({
        LmnSymbolAtomRef srcatom;
        LmnWord t = 0;

        EACH_ATOM(
            srcatom, ent, ({
              LmnSymbolAtomRef newatom;
              unsigned int start, end;
              LmnFunctor f;

              LMN_ASSERT(srcatom->get_id() > 0);
              if (proc_tbl_get_by_atom(atoms, srcatom, NULL) ||
                  LMN_IS_HL(srcatom)) {
                continue;
              }

              f = srcatom->get_functor();
              newatom = lmn_mem_newatom(destmem, f);

              atoms->proc_tbl_put_atom(srcatom, (LmnWord)newatom);

              start = 0;
              end = srcatom->get_arity();

              if (LMN_IS_PROXY_FUNCTOR(f)) {
                start = 1;
                end = 2;
                LMN_PROXY_SET_MEM(newatom, destmem);

                if (f == LMN_OUT_PROXY_FUNCTOR) {
                  LmnSymbolAtomRef srcinside;
                  LmnSymbolAtomRef newinside;

                  srcinside = (LmnSymbolAtomRef)srcatom->get_link(0);
                  proc_tbl_get_by_atom(atoms, srcinside, &t);
                  newinside = (LmnSymbolAtomRef)t;

                  /* 必ず子膜につながっているはず */
                  LMN_ASSERT(srcinside->get_functor() ==
                             LMN_IN_PROXY_FUNCTOR);
                  LMN_ASSERT(LMN_PROXY_GET_MEM(srcinside)->parent ==
                             LMN_PROXY_GET_MEM(srcatom));
                  lmn_newlink_in_symbols(newatom, 0, newinside, 0);
                }
              }

              /* リンク先と接続 */
              for (i = start; i < end; i++) {
                LmnLinkAttr attr = srcatom->get_attr(i);
                LmnAtomRef a = srcatom->get_link(i);
                if (LMN_ATTR_IS_DATA(attr)) {
                  LmnAtomRef newargatom;

                  if (LMN_ATTR_IS_HL(attr) && hl_nd) {
                    /* unifyせずにコピーする */
                    HyperLink *newhl, *orihl;
                    LmnAtomRef ori_attr_atom;
                    LmnLinkAttr ori_attr;

                    orihl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)a);
                    ori_attr_atom = LMN_HL_ATTRATOM(orihl);
                    ori_attr = LMN_HL_ATTRATOM_ATTR(orihl);
                    newargatom = (LmnAtomRef)lmn_hyperlink_new_with_attr(
                        ori_attr_atom, ori_attr);
                    newhl =
                        lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)newargatom);

                    /* 子への接続 */
                    if (orihl->children) {
                      HashSetIterator it;
                      for (it = hashset_iterator(orihl->children);
                           !hashsetiter_isend(&it); hashsetiter_next(&it)) {
                        HyperLink *hl = (HyperLink *)hashsetiter_entry(&it);
                        if (proc_tbl_get_by_hlink(atoms, hl, &t)) {
                          newhl->unify((HyperLink *)t, ori_attr_atom,
                                          ori_attr);
                        }
                      }
                    }

                    /* 親への接続 */
                    if (orihl->parent &&
                        proc_tbl_get_by_hlink(atoms, orihl->parent, &t)) {
                      ((HyperLink *)t)->unify(newhl, ori_attr_atom,
                                      ori_attr);
                    }
                    atoms->put_new_hlink(orihl, (LmnWord)newhl);
                  } else {
                    newargatom =
                        (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)a, attr);
                  }

                  if (LMN_ATTR_IS_HL(attr)) {
                    lmn_mem_newlink(destmem, (LmnAtomRef)newatom,
                                    LMN_ATTR_GET_VALUE((LmnWord)newatom), i,
                                    newargatom, LMN_HL_ATTR, 0);
                  } else {
                    newlink_symbol_and_something(newatom, i, newargatom, attr);
                  }

                  lmn_mem_push_atom(destmem, newargatom, attr);
                } else if (proc_tbl_get_by_atom(atoms, (LmnSymbolAtomRef)a,
                                                &t)) {
                  newlink_symbol_and_something(newatom, i, (LmnAtomRef)t, attr);
                }
              }
            }));
      }));

  /* copy activated flag */
  destmem->is_activated = !destmem->rulesets.empty() || srcmem->is_activated;
}

struct LinkObj {
  LmnAtomRef ap;
  LmnLinkAttr pos;
  LinkObj(LmnAtomRef ap,LmnLinkAttr pos);
  LmnAtomRef GetAtom();
  LmnLinkAttr GetPos();
};
LmnAtomRef LinkObj::GetAtom() { return this->ap; }
LmnAtomRef LinkObjGetAtom(LinkObjRef o) { return o->ap; }
LmnLinkAttr LinkObj::GetPos() { return this->pos; }
LmnLinkAttr LinkObjGetPos(LinkObjRef o) { return o->pos; }
LinkObj::LinkObj(LmnAtomRef ap, LmnLinkAttr pos) {
  this->ap = ap;
  this->pos = pos;
}
LinkObjRef LinkObj_make(LmnAtomRef ap, LmnLinkAttr pos) {
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
static inline void
mem_map_hlink(LmnMembraneRef mem, LmnSymbolAtomRef root_hlAtom,
              LmnSymbolAtomRef copied_root_hlAtom, Vector *stack,
              ProcessTableRef atommap, ProcessTableRef hlinkmap,
              ProcessTableRef *attr_functors, Vector *attr_dataAtoms,
              Vector *attr_dataAtom_attrs)

{
  LmnWord t = 0;
  HyperLink *hl = lmn_hyperlink_at_to_hl(root_hlAtom);
  BOOL flg_search_hl = FALSE;
  if (!LMN_HL_HAS_ATTR(hl)) { //属性を持っていない場合は無条件に探索
    flg_search_hl = TRUE;
  } else {
    LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
    LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
    int i;
    if (LMN_ATTR_IS_DATA(attr)) {
      for (i = 0; i < attr_dataAtoms->get_num(); i++) {
        if (lmn_eq_func(attrAtom, attr, (LmnAtomRef)attr_dataAtoms->get(i),
                        attr_dataAtom_attrs->get(i))) {
          flg_search_hl = TRUE;
          break;
        } else {
          continue;
        }
      }
    } else {
      if (proc_tbl_get(*attr_functors,
                       ((LmnSymbolAtomRef)attrAtom)->get_functor(),
                       NULL)) {
        flg_search_hl = TRUE;
      }
    }
  }

  if (flg_search_hl) {
    if (!proc_tbl_get_by_hlink(
            hlinkmap, hl->get_root(),
            &t)) { //同じハイパーリンクが接続されたアトムがスタックに積まれてる場合がある
      int j, element_num;
      hlinkmap->put_new_hlink(
          hl->get_root(),
          (LmnWord)(lmn_hyperlink_at_to_hl(copied_root_hlAtom)));
      Vector *hl_childs = new Vector(16);
      hl->get_elements(hl_childs);
      element_num = hl_childs->get_num() - 1;
      for (j = 0; j < element_num;
           j++) { //ハイパーリンクにつながるすべての接続先を探索
        if (hl->mem !=
            ((HyperLink *)hl_childs->get(j))
                ->mem) { // root_hlAtomの所属膜と異なる膜内はコピーしない
          continue;
        }
        LmnSymbolAtomRef hlAtom = ((HyperLink *)hl_childs->get(j))->atom;
        if (LMN_ATTR_IS_DATA(hlAtom->get_attr(0))) {
          LmnSymbolAtomRef copied_hlAtom =
              lmn_copy_satom_with_data((hlAtom), FALSE);
          lmn_hyperlink_copy(copied_hlAtom, copied_root_hlAtom);
          lmn_mem_push_atom(mem, copied_hlAtom, LMN_HL_ATTR);
        } else {
          LmnSymbolAtomRef linked_hlAtom =
              (LmnSymbolAtomRef)hlAtom->get_link(0);
          if (!proc_tbl_get_by_atom(
                  atommap, linked_hlAtom,
                  &t)) { //ハイパーリンクアトム及びそれにつながるシンボルアトムをコピー
                         // まずシンボルアトムをコピー
            LmnSymbolAtomRef copied_linked_hlAtom =
                lmn_copy_satom_with_data(linked_hlAtom, TRUE);
            LmnSymbolAtomRef copied_hlAtom =
                (LmnSymbolAtomRef)copied_linked_hlAtom->get_link(hlAtom->get_attr(0));
            // ここではハイパーリンク構造体は未作成、lmn_hyperlink_copyで作成
            lmn_hyperlink_copy(copied_hlAtom, copied_root_hlAtom);
            // ここで作られた copied_hlAtom
            // はあとでシンボルアトム側から逆アクセスされるはずなので ここでは
            // push しなくてよい（はず）
            //            lmn_mem_push_atom(mem, copied_hlAtom, LMN_HL_ATTR);
            mem_push_symbol_atom(mem, copied_linked_hlAtom);
            atommap->proc_tbl_put_atom(linked_hlAtom,
                              (LmnWord)copied_linked_hlAtom);
            stack->push((LmnWord)linked_hlAtom);
          } else { //つまり既にスタックに積まれている場合
            //ハイパーリンクへの接続は、スタックに積まれているアトム側からの探索時に行う。
            // そのとき、アトム側からたどったハイパーリンクはまだ atomlist
            // に登録されて いないはず。新たに atomlist に登録する必要がある。
          }
        }
      }
      delete hl_childs;
    } else { //既にハイパーリンクをコピーしていればunify
      (lmn_hyperlink_at_to_hl(copied_root_hlAtom))->lmn_unify((HyperLink *)t, LMN_HL_ATTRATOM((HyperLink *)t),
                          LMN_HL_ATTRATOM_ATTR((HyperLink *)t));
    }
  } else {
    hl->lmn_unify(lmn_hyperlink_at_to_hl(copied_root_hlAtom),
                        LMN_HL_ATTRATOM(hl), LMN_HL_ATTRATOM_ATTR(hl));
  }
  lmn_mem_push_atom(mem, copied_root_hlAtom, LMN_HL_ATTR);
}

/* 膜memのsrcvecを根に持つgroundプロセスをコピーする.
 * srcvecはリンクオブジェクトのベクタ.
 * ret_dstlovecはコピーされた根のリンクオブジェクトのベクタ.
 * ret_atommapはコピー元とコピー先のアトムの対応
 * ret_hlinkmapはハイパーリンクのコピー元と先であり
 * hlgroundのフラグも兼ねている */
static inline void
mem_copy_ground_sub(LmnMembraneRef mem, Vector *srcvec, Vector **ret_dstlovec,
                    ProcessTableRef *ret_atommap, ProcessTableRef *ret_hlinkmap,
                    ProcessTableRef *attr_functors, Vector *attr_dataAtoms,
                    Vector *attr_dataAtom_attrs) {
  ProcessTableRef atommap;
  ProcessTableRef hlinkmap;
  Vector *stack;
  unsigned int i;
  LmnWord t = 0;

  atommap = new ProcessTbl(64);
  hlinkmap = new ProcessTbl(64);
  stack = new Vector(16);
  *ret_dstlovec = new Vector(16);

  /* 根をスタックに積む.
   * スタックにはリンクオブジェクトではなくアトムを積むため,
   * ここで根の先のアトムをコピーしスタックに積む必要がある */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    LmnAtomRef cpatom;

    if (LMN_ATTR_IS_DATA(l->pos)) {
      if (ret_hlinkmap != NULL &&
          l->pos ==
              LMN_HL_ATTR) { // hlgroundならハイパーリンクの先を辿ってコピーする
        cpatom = (LmnAtomRef)lmn_hyperlink_new_with_attr(
            LMN_HL_ATTRATOM(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)l->ap)),
            LMN_HL_ATTRATOM_ATTR(
                lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)l->ap)));
        mem_map_hlink(mem, (LmnSymbolAtomRef)l->ap, (LmnSymbolAtomRef)cpatom,
                      stack, atommap, hlinkmap, attr_functors, attr_dataAtoms,
                      attr_dataAtom_attrs);
      } else {
        cpatom = (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)l->ap, l->pos);
        lmn_mem_push_atom(mem, cpatom, l->pos);
      }
    } else { /* symbol atom */
      /* コピー済みでなければコピーする */
      if (!proc_tbl_get_by_atom(atommap, (LmnSymbolAtomRef)l->ap, &t)) {
        if (ret_hlinkmap != NULL) { // hlgroundなら
          cpatom = lmn_copy_satom_with_data((LmnSymbolAtomRef)l->ap, TRUE);
        } else {
          cpatom = lmn_copy_satom_with_data((LmnSymbolAtomRef)l->ap, FALSE);
        }

        mem_push_symbol_atom(mem, (LmnSymbolAtomRef)cpatom);
        atommap->proc_tbl_put_atom((LmnSymbolAtomRef)l->ap, (LmnWord)cpatom);

        /* 根のリンクのリンクポインタを0に設定する */
        ((LmnSymbolAtomRef)cpatom)->set_link(l->pos, 0);
        stack->push((LmnWord)l->ap);
      } else {
        /* コピー済みの場合はスタックには追加しない */
        cpatom = (LmnAtomRef)t;
        ((LmnSymbolAtomRef)cpatom)->set_link(l->pos, 0);
      }
    }
    (*ret_dstlovec)->push((vec_data_t)new LinkObj(cpatom, l->pos));
  }

  while (!stack->is_empty()) {
    LmnSymbolAtomRef src_atom, copied;

    src_atom = (LmnSymbolAtomRef)stack->pop();
    proc_tbl_get_by_atom(atommap, src_atom, &t);
    copied = (LmnSymbolAtomRef)t;

    for (i = 0; i < src_atom->get_arity(); i++) {
      LmnAtomRef next_src = src_atom->get_link(i);
      LmnLinkAttr next_attr = src_atom->get_attr(i);

      /* copied->get_link(i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        if (ret_hlinkmap != NULL &&
            next_attr ==
                LMN_HL_ATTR) { // hlgroundならハイパーリンクの先を辿ってコピーする
          mem_map_hlink(mem, (LmnSymbolAtomRef)next_src,
                        (LmnSymbolAtomRef)copied->get_link(i), stack,
                        atommap, hlinkmap, attr_functors, attr_dataAtoms,
                        attr_dataAtom_attrs);
        } else {
          if (next_attr == LMN_HL_ATTR) {
            LmnAtomRef next_src_copied = copied->get_link(i);
            lmn_mem_push_atom(mem, next_src_copied, next_attr);
          } else {
            lmn_mem_push_atom(mem, next_src, next_attr);
          }
        }
      } else if (copied->get_link(i) != 0) {
        LmnAtomRef next_copied;
        if (!proc_tbl_get_by_atom(atommap, (LmnSymbolAtomRef)next_src,
                                  &t)) { /* next_srcは未訪問 */
          if (ret_hlinkmap != NULL) {    // hlgroundなら
            next_copied =
                lmn_copy_satom_with_data((LmnSymbolAtomRef)next_src, TRUE);
          } else {
            next_copied =
                lmn_copy_satom_with_data((LmnSymbolAtomRef)next_src, FALSE);
          }
          mem_push_symbol_atom(mem, (LmnSymbolAtomRef)next_copied);
          atommap->proc_tbl_put_atom((LmnSymbolAtomRef)next_src,
                            (LmnWord)next_copied);
          stack->push((LmnWord)next_src);
        } else {
          next_copied = (LmnAtomRef)t;
        }
        copied->set_link(i, next_copied);
      }
    }
  }

  delete stack;
  *ret_atommap = atommap;
  if (ret_hlinkmap != NULL) {
    *ret_hlinkmap = hlinkmap;
  }
}

void lmn_mem_copy_ground(LmnMembraneRef mem, Vector *srcvec,
                         Vector **ret_dstlovec, ProcessTableRef *ret_atommap) {
  mem_copy_ground_sub(mem, srcvec, ret_dstlovec, ret_atommap, NULL, NULL, NULL,
                      NULL);
}

void lmn_mem_copy_hlground(LmnMembraneRef mem, Vector *srcvec,
                           Vector **ret_dstlovec, ProcessTableRef *ret_atommap,
                           ProcessTableRef *ret_hlinkmap,
                           ProcessTableRef *attr_functors,
                           Vector *attr_dataAtoms,
                           Vector *attr_dataAtom_attrs) {
  mem_copy_ground_sub(mem, srcvec, ret_dstlovec, ret_atommap, ret_hlinkmap,
                      attr_functors, attr_dataAtoms, attr_dataAtom_attrs);
}

/* srcvec,dstvecは比較元,比較先groundの明示的自由リンクLinkObj.
 * ground検査はすんでいるものとする. srcとdstが同じ形なら真を返す.
 *
 * TODO: 構造化 */
BOOL lmn_mem_cmp_ground(const Vector *srcvec, const Vector *dstvec) {
  unsigned int i, j;
  BOOL ret_flag = TRUE;
  Vector stack1, stack2;
  SimpleHashtbl map; /* 比較元->比較先 */
  LinkObjRef start1, start2;

  hashtbl_init(&map, 256);

  stack1.init(16);
  stack2.init(16);

  /* startはstackにつまれるので処理中に壊されるためコピー */
  start1 = new LinkObj(((LinkObjRef)srcvec->get(0))->ap,
                        ((LinkObjRef)srcvec->get(0))->pos);
  start2 = new LinkObj(((LinkObjRef)dstvec->get(0))->ap,
                        ((LinkObjRef)dstvec->get(0))->pos);

  if (!LMN_ATTR_IS_DATA(start1->pos) && !LMN_ATTR_IS_DATA(start2->pos)) {
    /* ともにシンボルアトムの場合 */
    stack1.push((LmnWord)start1);
    stack2.push((LmnWord)start2);
  } else { /* data atom は積まない */
    if (!lmn_data_atom_eq((LmnDataAtomRef)start1->ap, start1->pos,
                          (LmnDataAtomRef)start2->ap, start2->pos)) {
      ret_flag = FALSE;
    }
    LMN_FREE(start1);
    LMN_FREE(start2);
  }

  while (!stack1.is_empty()) { /* main loop: start */
    LinkObjRef l1, l2;
    BOOL contains1, contains2;

    l1 = (LinkObjRef)stack1.pop();
    l2 = (LinkObjRef)stack2.pop();
    contains1 = FALSE;
    contains2 = FALSE;

    for (i = 0; i < srcvec->get_num(); i++) {
      LinkObjRef lobj = (LinkObjRef)srcvec->get(i);
      if (l1->ap == ((LmnSymbolAtomRef)lobj->ap)->get_link(lobj->pos) &&
          l1->pos ==
              ((LmnSymbolAtomRef)lobj->ap)->get_attr(lobj->pos)) {
        contains1 = TRUE;
        break;
      }
    }
    for (j = 0; j < dstvec->get_num(); j++) {
      LinkObjRef lobj = (LinkObjRef)dstvec->get(j);
      if (l2->ap == ((LmnSymbolAtomRef)lobj->ap)->get_link(lobj->pos) &&
          l2->pos ==
              ((LmnSymbolAtomRef)lobj->ap)->get_attr(lobj->pos)) {
        contains2 = TRUE;
        break;
      }
    }
    if (i != j) { /* 根の位置が違う */
      LMN_FREE(l1);
      LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }
    if (contains1) { /* 根に到達した場合 */
      LMN_FREE(l1);
      LMN_FREE(l2);
      continue;
    }

    if (l1->pos != l2->pos) { /* 引数検査 */
      LMN_FREE(l1);
      LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if (((LmnSymbolAtomRef)l1->ap)->get_functor() !=
        ((LmnSymbolAtomRef)l2->ap)->get_functor()) {
      /* ファンクタ検査 */
      LMN_FREE(l1);
      LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    }

    if (!hashtbl_contains(&map, (HashKeyType)l1->ap)) {
      /* 未出 */
      hashtbl_put(&map, (HashKeyType)l1->ap, (HashValueType)l2->ap);
    } else if ((LmnAtomRef)hashtbl_get(&map, (HashKeyType)l1->ap) != l2->ap) {
      /* 既出で不一致 */
      LMN_FREE(l1);
      LMN_FREE(l2);
      ret_flag = FALSE;
      break;
    } else {
      /* 既出で一致 */
      continue;
    }

    for (i = 0; i < ((LmnSymbolAtomRef)l1->ap)->get_arity(); i++) {
      LinkObjRef n1, n2;
      if (i == l1->pos)
        continue;
      if (!LMN_ATTR_IS_DATA(((LmnSymbolAtomRef)l1->ap)->get_attr(i)) &&
          !LMN_ATTR_IS_DATA(((LmnSymbolAtomRef)l1->ap)->get_attr(i))) {
        n1 = new LinkObj(((LmnSymbolAtomRef)l1->ap)->get_link(i),
                          LMN_ATTR_GET_VALUE(
                              ((LmnSymbolAtomRef)l1->ap)->get_attr(i)));
        n2 = new LinkObj(((LmnSymbolAtomRef)l2->ap)->get_link(i),
                          LMN_ATTR_GET_VALUE(
                              ((LmnSymbolAtomRef)l2->ap)->get_attr(i)));
        stack1.push((LmnWord)n1);
        stack2.push((LmnWord)n2);
      } else { /* data atom は積まない */
        if (!lmn_data_atom_eq(
                (LmnDataAtomRef)((LmnSymbolAtomRef)l1->ap)->get_link(i),
                ((LmnSymbolAtomRef)l1->ap)->get_attr(i),
                (LmnDataAtomRef)((LmnSymbolAtomRef)l2->ap)->get_link(i),
                ((LmnSymbolAtomRef)l2->ap)->get_attr(i))) {
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
  for (i = 0; i < stack1.get_num(); i++)
    LMN_FREE((LinkObjRef)stack1.get(i));
  for (i = 0; i < stack2.get_num(); i++)
    LMN_FREE((LinkObjRef)stack2.get(i));
  stack1.destroy();
  stack2.destroy();
  hashtbl_destroy(&map);

  return ret_flag;
}

/* srcvecのリンクの列が基底項プロセスに到達(avovecのリンクに到達する場
   合は基底項プロセスではない)している場合、真を返し、natomsに基底項プ
   ロセスないのアトムの数を格納する。*/
BOOL lmn_mem_is_ground(Vector *srcvec, Vector *avovec, unsigned long *natoms) {
  ProcessTableRef atoms;
  BOOL b;

  b = ground_atoms(srcvec, avovec, &atoms, natoms, NULL, NULL, NULL, NULL);

  if (b) {
    delete atoms;
  }

  return b;
}

/* hlground版。*/
BOOL lmn_mem_is_hlground(Vector *srcvec, Vector *avovec, unsigned long *natoms,
                         ProcessTableRef *attr_functors, Vector *attr_dataAtoms,
                         Vector *attr_dataAtom_attrs) {
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  BOOL b;

  b = ground_atoms(srcvec, avovec, &atoms, natoms, &hlinks, attr_functors,
                   attr_dataAtoms, attr_dataAtom_attrs);

  if (b) {
    delete atoms;
    delete hlinks;
  }

  return b;
}

/* xとyが1つのリンクの逆向き表現かどうか */
#define IS_BUDDY(xap, xattr, yap, yattr)                                       \
  (!LMN_ATTR_IS_DATA(xattr) && !LMN_ATTR_IS_DATA(yattr) &&                     \
   ((xap)->get_link(xattr) == yap) &&                                  \
   ((xap)->get_attr(xattr) == yattr))

/* srcvecから出るリンクのリストが基底項プロセスに到達している場合、
 * avovecに基底項プロセスに存在するシンボルアトム、natomsにアトムの数、戻り値に真を返す。
 * リストが基底項プロセスに到達していない場合にはatomsはNULLとなり、偽を返す。
 * ただし、リンクがavovecに到達している場合には、基底項プロセスとはならない。
 * hlinksがNULLでなければhlgroundとして探索
 * attr_functors, attr_dataAtoms, attr_dataAtoms_attrはhlgroundの属性 */
BOOL ground_atoms(
    Vector *srcvec, Vector *avovec,
    ProcessTableRef *atoms, /* ground内の発見済みのシンボルアトム */
    unsigned long *natoms,   /* ground内のアトムの個数 */
    ProcessTableRef *hlinks, /* hlinks!=NULLなら、hlgroundとして探索 */
    ProcessTableRef *attr_functors, /* hlgroundの属性（unary atom）*/
    Vector *attr_dataAtoms,         /* hlgroundの属性（data atom）*/
    Vector *attr_dataAtom_attrs     /* hlgroundの属性(data atomの属性) */
) {
  Vector *unsearched_link_stack; /* 探索待ちリンク */
  // ProcessTbl found_ground_symbol_atoms;  /*
  // ground内の発見済みのシンボルアトム */ unsigned long count_of_ground_atoms;
  // /* ground内のアトムの個数 */
  int i;
  int reached_root_count; /* 到達した根の個数(1つは始点) */
  BOOL result;
  HyperLink *hl;
  LmnMembraneRef mem;
  Vector *hl_childs;
  int element_num;

  *natoms = 0;
  *atoms = new ProcessTbl(64);
  unsearched_link_stack = new Vector(16);
  reached_root_count = 1;
  // found_ground_symbol_atoms = proc_tbl_make_with_size(64);
  result = TRUE;
  // count_of_ground_atoms = 0;

  if (hlinks != NULL) {
    *hlinks = new ProcessTbl(64);
  }

  /* groundはつながったグラフなので1つの根からだけたどればよい */
  {
    LinkObjRef l = (LinkObjRef)srcvec->get(0);
    unsearched_link_stack->push((LmnWord)new LinkObj(l->ap, l->pos));
  }

  while (!unsearched_link_stack->is_empty()) {
    LinkObjRef l;
    LmnAtomRef l_ap;
    LmnLinkAttr l_pos;

    l = (LinkObjRef)unsearched_link_stack->pop();
    l_ap = l->ap;
    l_pos = l->pos;

    LMN_FREE(l);

    if (LMN_ATTR_IS_DATA(
            l_pos)) { /* lがデータなら行き止まり
                         hlgroundの場合はハイパーリンクアトムを探索*/
      if (lmn_data_atom_is_ground((LmnDataAtomRef)l_ap, l_pos, hlinks)) {
        if (l_pos == LMN_HL_ATTR) {
          hl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)l_ap);
          if (hlinks != NULL &&
              !proc_tbl_get_by_hlink(*hlinks, hl->get_root(),
                                     NULL)) {
            BOOL flg_search_hl = FALSE;
            BOOL continue_flag = FALSE;
            /* lがavovecにつながっていれば */
            for (i = 0; avovec != NULL && i < avovec->get_num(); i++) {
              LinkObjRef a = (LinkObjRef)avovec->get(i);
              if (IS_BUDDY((LmnSymbolAtomRef)l_ap, 0, (LmnSymbolAtomRef)a->ap,
                           a->pos)) {
                result = FALSE;
                goto returning;
              }
            }

            /* lがsrcvecにつながっていれば */
            for (i = 0; i < srcvec->get_num(); i++) {
              LinkObjRef a = (LinkObjRef)srcvec->get(i);
              if (IS_BUDDY((LmnSymbolAtomRef)l_ap, 0, (LmnSymbolAtomRef)a->ap,
                           a->pos)) {
                reached_root_count++;
                continue_flag = TRUE;
                break;
              }
            }
            if (!continue_flag) {
              if (!LMN_HL_HAS_ATTR(hl)) { //属性を持っていない場合は無条件に探索
                flg_search_hl = TRUE;
              } else {
                LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl);
                LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl);
                if (LMN_ATTR_IS_DATA(attr)) {
                  for (i = 0; i < attr_dataAtoms->get_num(); i++) {
                    if (lmn_eq_func(attrAtom, attr,
                                    (LmnAtomRef)attr_dataAtoms->get(i),
                                    attr_dataAtom_attrs->get(i))) {
                      flg_search_hl = TRUE;
                      break;
                    } else {
                      continue;
                    }
                  }
                } else {
                  if (proc_tbl_get(
                          *attr_functors,
                          ((LmnSymbolAtomRef)attrAtom)->get_functor(),
                          NULL)) {
                    flg_search_hl = TRUE;
                  }
                }
              }
            }

            if (flg_search_hl) {
              (*hlinks)->put_new_hlink(hl->get_root(),
                                     (LmnWord)hl);
              hl_childs = new Vector(16);

              hl->get_elements(hl_childs);
              element_num = hl_childs->get_num() - 1;
              mem = hl->mem;
              for (i = 0; i < element_num; i++) {
                if (mem != ((HyperLink *)hl_childs->get(i))
                               ->mem) { //別の膜に移動したらFALSEに
                  // 当初していたが，ハイパーリンク接続の場合は同一膜内の構造とマッチするように変更
                  // result = FALSE;
                  // goto returning;
                  continue;
                }
                LmnSymbolAtomRef hlAtom =
                    ((HyperLink *)hl_childs->get(i))->atom;
                LmnAtomRef linked_hlAtom = hlAtom->get_link(0);
                LmnLinkAttr linked_attr = hlAtom->get_attr(0);
                unsearched_link_stack->push(
                         (LmnWord)new LinkObj(hlAtom->get_link(0),
                                               hlAtom->get_attr(0)));
                if (!LMN_ATTR_IS_DATA(linked_attr)) {
                  int j;
                  /* lがavovecにつながっていれば */
                  for (j = 0; avovec != NULL && j < avovec->get_num(); j++) {
                    LinkObjRef a = (LinkObjRef)avovec->get(j);
                    if (IS_BUDDY((LmnSymbolAtomRef)linked_hlAtom, linked_attr,
                                 (LmnSymbolAtomRef)a->ap, a->pos)) {
                      result = FALSE;
                      goto returning;
                    }
                  }

                  /* lがsrcvecにつながっていれば */
                  continue_flag = FALSE;
                  for (j = 0; j < srcvec->get_num(); j++) {
                    LinkObjRef a = (LinkObjRef)srcvec->get(j);
                    if (IS_BUDDY((LmnSymbolAtomRef)linked_hlAtom, linked_attr,
                                 (LmnSymbolAtomRef)a->ap, a->pos)) {
                      reached_root_count++;
                      continue_flag = TRUE;
                      break;
                    }
                  }
                  if (continue_flag)
                    unsearched_link_stack->pop();
                }
              }
              delete hl_childs;
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
      for (i = 0; avovec != NULL && i < avovec->get_num(); i++) {
        LinkObjRef a = (LinkObjRef)avovec->get(i);
        if (IS_BUDDY((LmnSymbolAtomRef)l_ap, l_pos, (LmnSymbolAtomRef)a->ap,
                     a->pos)) {
          result = FALSE;
          goto returning;
        }
      }

      /* lがsrcvecにつながっていれば */
      {
        if (((LmnSymbolAtomRef)l_ap)->get_attr(l_pos) != LMN_HL_ATTR) {
          BOOL continue_flag = FALSE;
          for (i = 0; i < srcvec->get_num(); i++) {
            LinkObjRef a = (LinkObjRef)srcvec->get(i);
            if (IS_BUDDY((LmnSymbolAtomRef)l_ap, l_pos, (LmnSymbolAtomRef)a->ap,
                         a->pos)) {
              reached_root_count++;
              continue_flag = TRUE;
              break;
            }
          }
          if (continue_flag)
            continue;
        }
      }

      /* lがプロキシを指していれば */
      if (((LmnSymbolAtomRef)l_ap)->is_proxy()) {
        result = FALSE;
        goto returning;
      }

      /* lの指すアトムが訪問済みなら */
      if (proc_tbl_get_by_atom(*atoms, (LmnSymbolAtomRef)l_ap, NULL)) {
        continue;
      }

      /* lはシンボルアトムで初出のアトムを指すため,その先を探索する必要がある */
      (*atoms)->proc_tbl_put_atom((LmnSymbolAtomRef)l_ap, (LmnWord)l_ap);
      (*natoms)++;

      for (i = 0; i < ((LmnSymbolAtomRef)l_ap)->get_arity(); i++) {
        if (i == l_pos)
          continue;
        unsearched_link_stack->push(
                 (LmnWord)new LinkObj(
                     ((LmnSymbolAtomRef)l_ap)->get_link(i),
                     ((LmnSymbolAtomRef)l_ap)->get_attr(i)));
      }
    }
  }

  /* もし未到達の根があれば結合グラフになっていないのでgroundでない */
  result = (reached_root_count == srcvec->get_num());

returning:
  for (i = 0; i < unsearched_link_stack->get_num(); i++) {
    LMN_FREE(unsearched_link_stack->get(i));
  }
  delete unsearched_link_stack;

  if (result) {
    //*atoms = found_ground_symbol_atoms;
    //*natoms += count_of_ground_atoms;
  } else {
    if (hlinks != NULL) {
      delete *hlinks;
      *hlinks = NULL;
    }
    delete *atoms;
    *atoms = NULL;
    *natoms = 0;
  }

  return result;
}

/* 前の実装.しばらく残しておく */
BOOL ground_atoms_old(Vector *srcvec, Vector *avovec, HashSet **atoms,
                      unsigned long *natoms) {
  HashSet *visited = new HashSet(16);
  SimpleHashtbl *guard = NULL;
  HashSet *root = new HashSet(16);
  Vector *stack = new Vector(16);
  unsigned int i;
  unsigned int n;            /* 到達したアトムの数 */
  unsigned int n_root_data;  /* 根にあるデータアトムの数 */
  unsigned int n_reach_root; /* 到達した根の数 */
  BOOL ok;

  n = 0;
  n_reach_root = 0;
  n_root_data = 0;
  ok = TRUE;
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
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
    if (l->ap == ((LmnSymbolAtomRef)l->ap)->get_link(l->pos)) {
      n_reach_root++;
      continue;
    }

    root->add((HashKeyType)((LmnSymbolAtomRef)l->ap)->get_link(l->pos));
    if (stack->get_num() == 0) {
      stack->push((LmnWord)l->ap);
    }
  }

  if (ok) {
    if (avovec != NULL && avovec->get_num() > 0) {
      guard = hashtbl_make(16);
      for (i = 0; i < avovec->get_num(); i++) {
        LinkObjRef l = (LinkObjRef)avovec->get(i);
        if (!LMN_ATTR_IS_DATA(l->pos)) {
          hashtbl_put(guard, (HashKeyType)l->ap, (HashValueType)l->pos);
        }
      }
    }

    if (n_root_data == 0) {
      while (stack->get_num() > 0) {
        LmnSymbolAtomRef src_atom = (LmnSymbolAtomRef)stack->pop();
        if (src_atom->is_proxy()) {
          ok = FALSE;
          break;
        }
        if (visited->contains((HashKeyType)src_atom))
          continue;
        if (root->contains((HashKeyType)src_atom)) {
          n_reach_root++;
          continue;
        }
        visited->add((HashKeyType)src_atom);
        n++;
        for (i = 0; i < src_atom->get_arity(); i++) {
          LmnAtomRef next_src = src_atom->get_link(i);
          LmnLinkAttr next_attr = src_atom->get_attr(i);

          if (!LMN_ATTR_IS_DATA(next_attr)) {
            if (guard) {
              int t = hashtbl_get_default(guard, (HashKeyType)next_src, -1);
              if (t >= 0 && t == LMN_ATTR_GET_VALUE(next_attr)) {
                ok = FALSE;
                break;
              }
            }
            stack->push((LmnWord)next_src);
          } else if (lmn_data_atom_is_ground((LmnDataAtomRef)next_src,
                                             next_attr, NULL)) {
            n++;
          } else {
            ok = FALSE;
            break;
          }
        }
      }
    } else if (stack->get_num() >= 2 && n_root_data > 0) {
      ok = FALSE;
    } else if (stack->get_num() >= 3) {
      ok = FALSE;
    }
  }

  delete stack;
  delete root;
  if (guard)
    hashtbl_free(guard);

  if (ok && n_reach_root == srcvec->get_num()) {
    *natoms = n;
    *atoms = visited;
    return TRUE;
  } else {
    delete visited;
    *atoms = NULL;
    return FALSE;
  }
}

int mem_remove_symbol_atom_with_buddy_data_f(LmnWord _k, LmnWord _v,
                                             LmnWord _arg) {
  mem_remove_symbol_atom_with_buddy_data((LmnMembraneRef)_arg,
                                         (LmnSymbolAtomRef)_v);
  return 1;
}
void LmnMembrane::remove_ground(Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);
  atoms->tbl_foreach(mem_remove_symbol_atom_with_buddy_data_f,
                   (LmnWord)this);

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos)) {
      lmn_mem_remove_data_atom(this, (LmnDataAtomRef)l->ap, l->pos);
    } else if (LMN_ATTR_IS_EX(l->pos)) {
      mem_remove_symbol_atom(this, (LmnSymbolAtomRef)l->ap);
    }
  }
  delete atoms;
}

void lmn_mem_remove_hlground(LmnMembraneRef mem, Vector *srcvec,
                             ProcessTableRef *attr_sym, Vector *attr_data,
                             Vector *attr_data_at) {
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t, &hlinks, attr_sym, attr_data,
               attr_data_at);
  atoms->tbl_foreach(mem_remove_symbol_atom_with_buddy_data_f,
                   (LmnWord)mem);

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(l->pos)) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)l->ap, l->pos);
    } else if (LMN_ATTR_IS_EX(l->pos)) {
      mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)l->ap);
    }
  }
  delete atoms;
}

int free_symbol_atom_with_buddy_data_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  free_symbol_atom_with_buddy_data((LmnSymbolAtomRef)_v);
  return 1;
}

void lmn_mem_free_ground(Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long i, t;

  if (ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL)) {
    atoms->tbl_foreach(free_symbol_atom_with_buddy_data_f, (LmnWord)0);
    delete atoms;
  }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA(l->pos))
      lmn_free_atom(l->ap, l->pos);
  }
}

void lmn_mem_free_hlground(Vector *srcvec, ProcessTableRef *attr_sym,
                           Vector *attr_data, Vector *attr_data_at) {
  ProcessTableRef atoms;
  ProcessTableRef hlinks;
  unsigned long i, t;

  hlinks = NULL;
  if (ground_atoms(srcvec, NULL, &atoms, &t, &hlinks, attr_sym, attr_data,
                   attr_data_at)) {
    atoms->tbl_foreach(free_symbol_atom_with_buddy_data_f, (LmnWord)0);
    delete atoms;
  }

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA(l->pos))
      lmn_free_atom(l->ap, l->pos);
  }
}
void LmnMembrane::delete_ground(Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long i, t;

  if (!ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL)) {
    fprintf(stderr, "remove ground false\n");
  }

  atoms->tbl_foreach(mem_remove_symbol_atom_with_buddy_data_f,
                   (LmnWord)this);
  atoms->tbl_foreach(free_symbol_atom_with_buddy_data_f, (LmnWord)0);

  /* atomsはシンボルアトムしか含まないので、srcvecのリンクが直接データ
     アトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA(l->pos)) {
      lmn_mem_remove_data_atom(this, (LmnDataAtomRef)l->ap, l->pos);
      lmn_free_atom(l->ap, l->pos);
    }
  }

  delete atoms;
}

/** ===========================
 *  膜の同型性判定
 *  ---------------------------
 *  created  by Takayuki Sasaki
 *  modified by Masato Gocho
 *  ---------------------------
 */

#define CHECKED_MEM_DEPTH (0)
static BOOL mem_equals_rec(LmnMembraneRef mem1, TraceLogRef log1,
                           LmnMembraneRef mem2, SimplyLog log2,
                           int current_depth);

/* 階層グラフ構造mem1, mem2が同型ならば真を返す. */
BOOL LmnMembrane::equals(LmnMembraneRef mem2) {
  BOOL t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  // 非TIME-OPTではTraceLogのストラクチャでトレースすることができない（実装してない）
  TraceLogRef log1 = new TraceLog();
  SimplyLog log2 = simplylog_make();

  t = mem_equals_rec(this, log1, mem2, log2, CHECKED_MEM_DEPTH);

  simplylog_free(log2);
  delete log1;

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
static BOOL mem_equals_rec(LmnMembraneRef mem1, TraceLogRef log1,
                           LmnMembraneRef mem2, SimplyLog log2,
                           int current_depth) {
#ifndef LMN_MEMEQ_OLD
  /* mem1, mem2が未訪問の場合 */
  if (tracelog_contains_mem(log1, mem1)) {
    if (!simplylog_contains_mem(log2, mem2)) {
      return FALSE;
    } else {
      return mem2->mem_id() == tracelog_get_memMatched(log1, mem1);
    }
  } else if (simplylog_contains_mem(log2, mem2)) {
    return FALSE;
  }

  log1->visit_mem(mem1, mem2->mem_id());
  simplylog_put_mem(log2, mem2);
#endif

  /*  グラフ同形成判定の各STEP:
   *  Step ID | Complexity  | Description
   *  ==========================================================================
   *      1.1 | O(1)        |
   * 文字列に対応した整数ID(LmnMembraneのメンバ)同士の比較 1.2 | O(1)        |
   * シンボルアトム数(LmnMembraneのメンバ)同士の比較 1.3 | O(1)        |
   * データアトム数(LmnMembraneのメンバ)同士の比較 1.4 | O(N)        |
   * N本のAtomListEntryの種類と各アトム数の比較 1.5 | O(N)        |
   * 子膜数の比較(N個の要素を持ったリスト走査) 1.6 | O(N*M)      |
   * 子孫膜数の比較(N:子膜数, M:階層数: 階層数だけ子膜カウント) 2.0 | O(N) |
   * 通常 : N本のRuleSet ID比較(IDは整数かつ整列済み故に線形) | O(N*M)      |
   * uniq: N本のRuleSet IDおよび履歴の比較 |             | 各履歴の比較はO(1),
   * 履歴はハッシュ表管理->M個の履歴比較:O(M) 3.1 | O(?)        |
   * アトム起点のグラフ構造の同形成判定 3.2 | O(?)        |
   * 子膜起点のグラフ構造の同形成判定
   *  --------------------------------------------------------------------------
   *  Step 1.X| O(N + M)    | N:子膜数, M;アトムリスト数.
   * Step 1.6廃止につき,線形の計算量 Step 2.X| O(N)        |
   * 標準仕様(uniq未使用)ならば線形の計算量 Step 3.X| O(?)        | TODO: 調査
   *
   *   子孫膜の比較(Step 1.6)はコストが高いため廃止.
   *     実際にはハッシュ関数mhashとの組合わせで同形成判定を実施しており,
   *     ハッシュ関数mhashは,
   * 階層の深さに関するハッシュコンフリクトを招きやすいものではない.
   */
  if (/* 1.1 */ mem1->NAME_ID() != mem2->NAME_ID() ||
      /* 1.2 */ mem1->symb_atom_num() != mem2->symb_atom_num() ||
      /* 1.3 */ mem1->data_atom_num() != mem2->data_atom_num() ||
      /* 1.4 */ !mem_equals_atomlists(mem1, mem2) ||
      /* 1.5 */ mem1->child_mem_num() != mem2->child_mem_num() ||
      /* 1.6 lmn_mem_count_descendants(mem1) != lmn_mem_count_descendants(mem2))
         ||*/
      /* 2.0 */
      !lmn_rulesets_equals(mem1->get_rulesets(),
                           mem2->get_rulesets()) ||
      /* 3.X */
      !mem_equals_isomorphism(mem1, log1, mem2, log2, current_depth)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

/* 膜mem1,
 * mem2直下のアトムリストを比較(アトムの種類毎にアトム数を比較)した結果の真偽値を返す
 */
static BOOL mem_equals_atomlists(LmnMembraneRef mem1, LmnMembraneRef mem2) {
  AtomListEntry *ent1;
  LmnFunctor f;

  EACH_ATOMLIST_WITH_FUNC(mem1, ent1, f, ({
                            AtomListEntry *ent2 = mem2->get_atomlist(f);
                            size_t s1 = ent1 ? ent1->size() : 0;
                            size_t s2 = ent2 ? ent2->size() : 0;
                            if (s1 != s2) {
                              return FALSE;
                            }
                          }));

  return TRUE;
}

/** ---------------------------
 *  Step 3.X
 */

#define ISOMOR_PHASE_ATOM (0U)
#define ISOMOR_PHASE_CHILD (1U)
#define ISOMOR_PHASE_END (2U)

struct MemIsomorIter {
  BYTE phase;
  AtomListIter pos;
  LmnMembraneRef mem;
  LmnSymbolAtomRef atom;
  LmnMembraneRef child;
  MemIsomorIter(LmnMembraneRef mem);
  ~MemIsomorIter();
  inline LmnSymbolAtomRef atom_traversed();
  inline LmnMembraneRef child_traversed();
  inline BOOL is_root_atom(LmnSymbolAtomRef atom);
};

MemIsomorIter::MemIsomorIter(LmnMembraneRef mem) {
  this->pos = atomlist_iter_initializer(mem->atomset);
  this->phase = ISOMOR_PHASE_ATOM;
  this->mem = mem;
  this->atom = NULL;
  this->child = NULL;
}

MemIsomorIter::~MemIsomorIter() {}
/* イテレータiterに記録した情報を基に,
 * 前回訪問したアトムiter->atomから次に訪問するアトムを求めて返し,
 * ない場合はNULL返す.
 *
 * 出力: iter->pos, iter->atom (iter->memはRead Only) */
inline LmnSymbolAtomRef MemIsomorIter::atom_traversed() {
  BOOL changed_lists;

  if (this->phase != ISOMOR_PHASE_ATOM) {
    return NULL;
  }

  changed_lists = FALSE;
  for (; atomlist_iter_condition(this->mem, this->pos);
       atomlist_iter_next(this->pos), changed_lists = TRUE, this->atom = NULL)
  /* OUTER LOOP */
  {
    AtomListEntry *ent;
    LmnFunctor f;
    f = atomlist_iter_get_functor(this->pos);
    ent = atomlist_iter_get_entry(this->mem, this->pos);

    if (!ent || ent->is_empty() || f == LMN_OUT_PROXY_FUNCTOR) {
      /* アトムリストが空の場合, 次の候補リストを取得する.
       * outside proxyは候補としない */
      continue; /* OUTER LOOP */
    } else {
      BOOL cur_is_tail;

      if (changed_lists || !this->atom) {
        /* 走査先のアトムリストが変更された場合 or
         * イテレータアトムが未設定の場合: リストの先頭アトムをcurとする */
        this->atom = atomlist_head(ent);
      } else if (this->atom == lmn_atomlist_end(ent)) {
        /* 一応, 想定外 */
        continue;
      } else {
        /* 既にイテレータアトムが設定されている場合は,
         * リストから次のアトムを確保する */
        this->atom = this->atom->get_next();
        if (this->atom == lmn_atomlist_end(ent)) {
          /* 結果, curが末尾に到達したならば次のアトムリストを求める. */
          continue; /* OUTER LOOP */
        }
      }

      /* リストを辿り, 根の候補とするアトムを求める */
      cur_is_tail = FALSE;
      while (!is_root_atom(this->atom)) { /* INNER LOOP */
        /* Resumeアトムを読み飛ばす */
        this->atom = this->atom->get_next();

        if (this->atom == lmn_atomlist_end(ent)) {
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

  if (this->atom) {
    return this->atom;
  } else {
    this->phase = ISOMOR_PHASE_CHILD;
    return NULL;
  }
}

inline LmnMembraneRef MemIsomorIter::child_traversed() {
  if (this->phase != ISOMOR_PHASE_CHILD) {
    return NULL;
  } else {
    if (!this->child) {
      this->child = (this->mem)->mem_child_head();
    } else {
      this->child = (this->child)->mem_next();
    }

    if (!this->child) {
      this->phase = ISOMOR_PHASE_END;
      return NULL;
    } else {
      return this->child;
    }
  }
}

static inline BOOL mem_equals_molecules(LmnMembraneRef mem1,
                                        LmnMembraneRef mem2, int current_depth);
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2,
                                       int current_depth);

static BOOL mem_isomor_mols(LmnMembraneRef mem1, TraceLogRef log1,
                            LmnMembraneRef mem2, SimplyLog log2,
                            MemIsomorIter *iter);

/* 未訪問の膜mem1および膜mem2を対象に,
 * アトム起点および子膜起点でグラフ構造をトレースする.
 * 膜mem1とmem2以下の階層グラフが同型と判定されたなれば真を返し,
 * 異形なばら偽を返す. */
static BOOL mem_equals_isomorphism(LmnMembraneRef mem1, TraceLogRef log1,
                                   LmnMembraneRef mem2, SimplyLog log2,
                                   int current_depth) {
  BOOL ret;
#ifdef LMN_MEMEQ_OLD
  /* !!CAUTION!!:
   *   旧同形成判定アルゴリズムは, 特定のケースで正しい結果を返すことができない.
   */
  ret = /* Step 3.1 */ mem_equals_molecules(mem1, mem2, current_depth) &&
        /* Step 3.2 */ mem_equals_children(mem1, mem2, current_depth);
#else
  MemIsomorIter iter = MemIsomorIter(mem1);
  ret = /* Step 3.X */ mem_isomor_mols(mem1, log1, mem2, log2, &iter);
#endif

  return ret;
}

inline BOOL MemIsomorIter::is_root_atom(LmnSymbolAtomRef atom) {
  LmnFunctor f;
  f = atom->get_functor();
  if (f == LMN_RESUME_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR ||
      (f == LMN_IN_PROXY_FUNCTOR &&
       !LMN_ATTR_IS_DATA(atom->get_attr(1)))) {
    /* データアトムと接続するinside proxyだけはルートアトムとする */
    return FALSE;
  }

  return TRUE;
}

#define MEM_ISOMOR_FIRST_TRACE (-1)
#define MEM_ISOMOR_MATCH_WITHOUT_MEM (1U)
#define MEM_ISOMOR_MATCH_WITHIN_MEM (2U)

static inline BOOL mem_isomor_mol_atoms(LmnMembraneRef mem1, TraceLogRef log1,
                                        LmnMembraneRef mem2, SimplyLog log2,
                                        MemIsomorIter *iter,
                                        LmnSymbolAtomRef root1);
static inline BOOL mem_isomor_mol_mems(LmnMembraneRef mem1, TraceLogRef log1,
                                       LmnMembraneRef mem2, SimplyLog log2,
                                       MemIsomorIter *iter,
                                       LmnMembraneRef root1);

/* mem1およびmem2直下から等価な分子構造を再帰DFSで探索する.
 * 全ての分子が過不足なく1対1にした場合, 真を返す. */
static BOOL mem_isomor_mols(LmnMembraneRef mem1, TraceLogRef log1,
                            LmnMembraneRef mem2, SimplyLog log2,
                            MemIsomorIter *iter) {
  LmnSymbolAtomRef root1;

  /* アトム起点の探索のための根を取得 */
  do {
    root1 = iter->atom_traversed();
    /* 未トレースのアトムが現れるか,
     * 末尾(NULL)に到達するまで根の候補探索を繰り返す. */
  } while (root1 && tracelog_contains_atom(log1, root1));

  if (root1) {
    /* アトムの根がある場合: アトム起点のグラフ同形成判定 */
    return mem_isomor_mol_atoms(mem1, log1, mem2, log2, iter, root1);
  } else {
    LmnMembraneRef child;
    do {
      child = iter->child_traversed();
    } while (child && tracelog_contains_mem(log1, child));

    if (child) {
      /* 子膜の根がある場合: 子膜起点のグラフ同形成判定 */
      return mem_isomor_mol_mems(mem1, log1, mem2, log2, iter, child);
    } else {
      /* アトムの根も子膜の根がない場合:
       *   全ての根を候補としたグラフ同形成判定が終了.
       *   比較済プロセス数と所持プロセス数を比較し, 対応漏れがないか検査する.
       */

      return log1->eq_traversed_proc_num(
          mem1, mem1->get_atomlist(LMN_IN_PROXY_FUNCTOR), NULL);
    }
  }
}

static inline int mem_isomor_trace(LmnAtomRef cur1, LmnMembraneRef mem1,
                                   TraceLogRef log1, LmnFunctor f, int from_i,
                                   LmnAtomRef cur2, LmnMembraneRef mem2,
                                   SimplyLog log2);

static inline BOOL mem_isomor_mol_atoms(LmnMembraneRef mem1, TraceLogRef log1,
                                        LmnMembraneRef mem2, SimplyLog log2,
                                        MemIsomorIter *iter,
                                        LmnSymbolAtomRef root1) {
  AtomListEntry *ent2;
  LmnSymbolAtomRef root2;
  LmnFunctor f;

  f = root1->get_functor();
  ent2 = mem2->get_atomlist(f);
  if (!ent2)
    return FALSE;

  /* for keeping current state (snap shot) */
  MemIsomorIter current = (*iter); /* shallow copy */

  EACH_ATOM(root2, ent2, ({
              if (simplylog_contains_atom(log2, root2))
                continue;
              log1->set_btpoint();
              simplylog_set_btpoint(log2);

              /* compare a molecule of root1 with a molecule of root2 */
              switch (mem_isomor_trace((LmnAtomRef)root1, mem1, log1, f,
                                       MEM_ISOMOR_FIRST_TRACE,
                                       (LmnAtomRef)root2, mem2, log2)) {
              case MEM_ISOMOR_MATCH_WITHOUT_MEM: {
                log1->continue_trace();
                simplylog_continue_trace(log2);
                return mem_isomor_mols(mem1, log1, mem2, log2, iter);
              }
              case MEM_ISOMOR_MATCH_WITHIN_MEM:
                /* keep this backtrack point */
                if (mem_isomor_mols(mem1, log1, mem2, log2, iter)) {
                  log1->continue_trace();
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
              log1->backtrack();
              simplylog_backtrack(log2);
            }));

  /* matchingする分子がなかった: FALSE */

  return FALSE;
}

static inline BOOL mem_isomor_mol_mems(LmnMembraneRef mem1, TraceLogRef log1,
                                       LmnMembraneRef mem2, SimplyLog log2,
                                       MemIsomorIter *iter,
                                       LmnMembraneRef root1) {
  LmnMembraneRef root2;
  for (root2 = mem2->mem_child_head(); root2; root2 = root2->mem_next()) {
    if (simplylog_contains_mem(log2, root2))
      continue;
    else {
      log1->set_btpoint();
      simplylog_set_btpoint(log2);
      if (!mem_equals_rec(root1, log1, root2, log2, CHECKED_MEM_DEPTH) ||
          !mem_isomor_mols(mem1, log1, mem2, log2, iter)) {
        log1->backtrack();
        simplylog_backtrack(log2);
      } else {
        log1->continue_trace();
        simplylog_continue_trace(log2);
        return TRUE;
      }
    }
  }

  return FALSE;
}

static inline BOOL
mem_isomor_trace_proxies(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                         LmnFunctor f, LmnAtomRef cur2, LmnMembraneRef mem2,
                         SimplyLog log2);
static inline int mem_isomor_trace_symbols(LmnAtomRef cur1, LmnMembraneRef mem1,
                                           TraceLogRef log1, LmnFunctor f,
                                           int from_i, LmnAtomRef cur2,
                                           LmnMembraneRef mem2, SimplyLog log2);

/* from link番号だけは受け取る.
 * 等価なアトムcur1, cur2から1step先の構造を比較して再帰する. */
static inline int mem_isomor_trace(LmnAtomRef cur1, LmnMembraneRef mem1,
                                   TraceLogRef log1, LmnFunctor f, int from_i,
                                   LmnAtomRef cur2, LmnMembraneRef mem2,
                                   SimplyLog log2) {
  if (LMN_IS_PROXY_FUNCTOR(f)) {
    /* proxy atomの場合 */
    if (!mem_isomor_trace_proxies(cur1, mem1, log1, f, cur2, mem2, log2)) {
      return FALSE;
    } else {
      return MEM_ISOMOR_MATCH_WITHIN_MEM;
    }
  } else {
    /* 非proxy(symbol)アトムの場合 */
    return mem_isomor_trace_symbols(cur1, mem1, log1, f, from_i, cur2, mem2,
                                    log2);
  }
}

static inline BOOL
mem_isomor_trace_proxies(LmnAtomRef cur1, LmnMembraneRef mem1, TraceLogRef log1,
                         LmnFunctor f, LmnAtomRef cur2, LmnMembraneRef mem2,
                         SimplyLog log2) {
  LmnAtomRef pair1, pair2, atom1, atom2;
  LmnLinkAttr attr1, attr2;
  LmnMembraneRef nxtM1, nxtM2;

  /* proxy atomの場合
   * -----------------+
   * ...-0--1-[in]-0--|--0-[out]-1--..
   * -----------------+
   */
  pair1 = ((LmnSymbolAtomRef)cur1)->get_link(0);
  pair2 = ((LmnSymbolAtomRef)cur2)->get_link(0);
  nxtM1 = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)pair1);
  nxtM2 = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)pair2);

  atom1 = ((LmnSymbolAtomRef)pair1)->get_link(1);
  atom2 = ((LmnSymbolAtomRef)pair2)->get_link(1);
  attr1 = ((LmnSymbolAtomRef)pair1)->get_attr(1);
  attr2 = ((LmnSymbolAtomRef)pair2)->get_attr(1);

  /* 1. inside proxy atomに対する訪問関係をチェック */
  {
    LmnAtomRef in1, in2;
    if (f == LMN_OUT_PROXY_FUNCTOR) {
      in1 = pair1;
      in2 = pair2;
    } else if (f == LMN_IN_PROXY_FUNCTOR) {
      in1 = cur1;
      in2 = cur2;
    } else {
      lmn_fatal("sorry, unrecognized proxy functor found: unrecognized proxy");
      return FALSE;
    }

    if (tracelog_contains_atom(log1, (LmnSymbolAtomRef)in1)) {
      if (!simplylog_contains_atom(log2, (LmnSymbolAtomRef)in2) ||
          ((LmnSymbolAtomRef)in2)->get_id() !=
              tracelog_get_atomMatched(log1, (LmnSymbolAtomRef)in1)) {
        return FALSE;
      } else {
        return TRUE;
      }
    } else if (simplylog_contains_atom(log2, (LmnSymbolAtomRef)in2)) {
      return FALSE;
    } else {
      /* in1は互いに未訪問であるため, トレースする必要がある */
      log1->visit_atom((LmnSymbolAtomRef)in1,
                        ((LmnSymbolAtomRef)in2)->get_id(),
                        LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in1));
      simplylog_put_atom(log2, (LmnSymbolAtomRef)in2);
    }
  }

  /* 2. 膜の訪問関係をチェック */
  if (!tracelog_contains_mem(log1, nxtM1)) {
    if (simplylog_contains_mem(log2, nxtM2)) {
      /* nxtM1が未訪問でnxtM2が訪問済ならば異形 */
      return FALSE;
    }
  } else if (!simplylog_contains_mem(log2, nxtM2) ||
             nxtM2->mem_id() != tracelog_get_memMatched(log1, nxtM1)) {
    /* nxtM1が訪問済で, { nxtM2が未訪問 || 対応する膜の関係が合わない }
     * ならば異形 */
    return FALSE;
  } /*
  else {
    膜nxtM1, nxtM2は互いに訪問済 or 未訪問
  }
  */

  /* 3. 比較 */
  if ((LMN_ATTR_IS_DATA(attr1) || LMN_ATTR_IS_DATA(attr2))) {
    /* proxyアトムがデータアトムに接続している場合 */
    if (!lmn_data_atom_eq((LmnDataAtomRef)atom1, attr1, (LmnDataAtomRef)atom2,
                          attr2)) {
      return FALSE;
    } else {
      BOOL ret = TRUE;
      if (f == LMN_OUT_PROXY_FUNCTOR && !tracelog_contains_mem(log1, nxtM1)) {
        /* 初出の子膜に入る(out proxyからトレースする)場合は,
         * 子膜の同形成判定を行う. */
        ret = mem_equals_rec(nxtM1, log1, nxtM2, log2, CHECKED_MEM_DEPTH);
      }
      return ret;
    }
  } else { /* proxyアトムがシンボルアトムに接続している場合 */
    LmnFunctor f_new = ((LmnSymbolAtomRef)atom1)->get_functor();
    if (f_new != ((LmnSymbolAtomRef)atom2)->get_functor() ||
        attr1 != attr2) {
      return FALSE;
    } else {
      BOOL ret =
          mem_isomor_trace(atom1, nxtM1, log1, f_new, LMN_ATTR_GET_VALUE(attr1),
                           atom2, nxtM2, log2);
      if (ret && f == LMN_OUT_PROXY_FUNCTOR &&
          !tracelog_contains_mem(log1, nxtM1)) {
        /* 初出の子膜に入る(out proxyからトレースする)場合は,
         * 子膜の同形成判定を行う. */
        ret = mem_equals_rec(nxtM1, log1, nxtM2, log2, CHECKED_MEM_DEPTH);
      }

      return ret;
    }
  }
}

static inline int mem_isomor_trace_symbols(LmnAtomRef cur1, LmnMembraneRef mem1,
                                           TraceLogRef log1, LmnFunctor f,
                                           int from_i, LmnAtomRef cur2,
                                           LmnMembraneRef mem2,
                                           SimplyLog log2) {
  int to_i, global_ret;

  log1->visit_atom((LmnSymbolAtomRef)cur1,
                    ((LmnSymbolAtomRef)cur2)->get_id(), mem1);
  simplylog_put_atom(log2, (LmnSymbolAtomRef)cur2);

  global_ret = MEM_ISOMOR_MATCH_WITHOUT_MEM;

  /* リンク先を辿る */
  for (to_i = 0; to_i < LMN_FUNCTOR_ARITY(lmn_functor_table, f); to_i++) {
    LmnLinkAttr attr1, attr2;
    LmnAtomRef atom1, atom2;

    /* 辿ってきたリンクは検査済み: skip */
    if (to_i == from_i)
      continue;

    attr1 = ((LmnSymbolAtomRef)cur1)->get_attr(to_i);
    attr2 = ((LmnSymbolAtomRef)cur2)->get_attr(to_i);
    atom1 = ((LmnSymbolAtomRef)cur1)->get_link(to_i);
    atom2 = ((LmnSymbolAtomRef)cur2)->get_link(to_i);

    if (LMN_ATTR_IS_DATA(attr1) ||
        LMN_ATTR_IS_DATA(attr2)) { /* data atomの場合 */
      if (!lmn_data_atom_eq((LmnDataAtomRef)atom1, attr1, (LmnDataAtomRef)atom2,
                            attr2)) {
        return FALSE;
      }      /*
            else continue
            */
    } else { /* symbol atomの場合 */
      LmnFunctor f_new = ((LmnSymbolAtomRef)atom1)->get_functor();

      if (f_new != ((LmnSymbolAtomRef)atom2)->get_functor() ||
          attr1 != attr2) {
        /* 接続先アトムの種類(functor)や接続先リンクの番号が異なる場合は異形 */
        return FALSE;
      }

      if (tracelog_contains_atom(log1, (LmnSymbolAtomRef)atom1)) {
        /* 接続先のアトムが既に訪問済みの場合 */
        if (!simplylog_contains_atom(log2, (LmnSymbolAtomRef)atom2) ||
            ((LmnSymbolAtomRef)atom2)->get_id() !=
                tracelog_get_atomMatched(log1, (LmnSymbolAtomRef)atom1)) {
          /* 互いに訪問済み関係でない場合や訪問済みアトムへの対応関係が一致していない場合は異形
           */
          return FALSE;
        } /*
         else continue
         */
      } else if (simplylog_contains_atom(log2, (LmnSymbolAtomRef)atom2)) {
        /* 接続先のアトムへ初めて訪問する場合:
         * 対応するアトムが訪問済みのため異形 */
        return FALSE;
      } else {
        /* 互いに接続先のアトムへ初めて訪問する場合 */
        switch (mem_isomor_trace(atom1, mem1, log1, f_new,
                                 LMN_ATTR_GET_VALUE(attr1), atom2, mem2,
                                 log2)) {
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

/* この段階で本膜内の「アトムを起点とする走査」はすべて完了し,
 * 両膜直下のグラフ構造は同型である. ここからは,
 * 両膜内に存在するすべての子膜について,
 * その構造が一致するかどうかについて調べていく.
 * 以降、mem1内の子膜を1つ固定し、mem2直下の子膜から対応するものを特定する作業に移っていくが,
 * 結果が偽となるケースにおける処理速度を向上させるため,
 * 子孫膜数が少ない子膜から優先的に固定する */
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2,
                                       int current_depth) LMN_UNUSED;
static inline BOOL mem_equals_children(LmnMembraneRef mem1, LmnMembraneRef mem2,
                                       int current_depth) {
  int child_n;

  child_n = mem1->count_children();
  if (child_n != mem2->count_children()) {
    return FALSE;
  } else if (child_n == 0) {
    return TRUE;
  } else {
    Vector *v_mems_children1,
        *v_mems_children2; /* 本膜直下の子膜を管理するVector
                              (このVectorが空になるまで子膜を起点とする走査が続く)
                            */
    BOOL matched;

    v_mems_children1 = new Vector(child_n);
    v_mems_children2 = new Vector(child_n);
    v_mems_children1->memset_tbl(0,
           sizeof(vec_data_t) * (v_mems_children1->get_cap()));
    v_mems_children2->memset_tbl(0,
           sizeof(vec_data_t) * (v_mems_children2->get_cap()));

    matched = TRUE;

    /* mem1, mem2直下の子膜を取得し、その個数が等しいことを確認 */
    {
      LmnMembraneRef m;
      for (m = mem1->child_head; m; m = m->next)
        v_mems_children1->push((LmnWord)m);
      for (m = mem2->child_head; m; m = m->next)
        v_mems_children2->push((LmnWord)m);

      if (v_mems_children1->get_num() != v_mems_children2->get_num()) {
        /* 本膜直下の子膜数が互いに一致しない場合は偽 */
        matched = FALSE;
      } else {
        /* 子孫膜数の多い順にv_mems_children1, v_mems_children2をソート */
        mem_mk_sorted_children(v_mems_children1);
        mem_mk_sorted_children(v_mems_children2);
      }
    }

    if (matched) {
      matched = mem_equals_children_inner(v_mems_children1, v_mems_children2,
                                          current_depth);
    }

    delete v_mems_children1;
    delete v_mems_children2;

    return matched;
  }
}

static inline BOOL mem_equals_children_inner(Vector *v_mems_children1,
                                             Vector *v_mems_children2,
                                             int current_depth) {
  int i, j;
  BOOL matched;

  /* 子膜を起点とする走査 */
  matched = TRUE;
  while (matched && !v_mems_children1->is_empty()) {
    LmnMembraneRef cm1 = (LmnMembraneRef)v_mems_children1->pop();

    /* fprintf(stderr, "\t-- start to test a descendant membrane --\n\t\t# of
     * descendants of mem(%u): %u\n" , (unsigned int)cm1 ,
     * lmn_mem_count_descendants(cm1)); */

    for (i = v_mems_children2->get_num(); i > 0; i--) {
      LmnMembraneRef cm2 = (LmnMembraneRef)v_mems_children2->get(i - 1);

      matched = mem_equals_rec(cm1, NULL, cm2, NULL, current_depth + 1);
      if (!matched) {
        continue; /* INNER1 LOOP */
      } else {
        /* cm1と同型の膜(=cm2)がv_mems_children2内に見つかった場合にここに入る。
         * v_mems_children2からcm2を取り除く。 */
        for (j = 0; j < v_mems_children2->get_num(); j++) {
          if (cm2 == (LmnMembraneRef)v_mems_children2->get(j)) {
            v_mems_children2->pop_n(j);
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
 * POPされるため、子孫膜数の少ない子膜から順にマッチングの対象となることになる。
 */
static void mem_mk_sorted_children(Vector *vec) {
  unsigned int num_descendants_max;
  unsigned int i, n;
  Vector *v_mems_tmp;

  LMN_ASSERT(vec->get_num());

  num_descendants_max = 0;
  for (i = 0; i < vec->get_num(); i++) {
    n = ((LmnMembraneRef)vec->get(i))->count_descendants();
    if (n > num_descendants_max) {
      num_descendants_max = n;
    }
  }
  v_mems_tmp = new Vector(vec->get_num());
  for (n = 0; n <= num_descendants_max; n++) {
    for (i = 0; i < vec->get_num(); i++) {
      if (n == ((LmnMembraneRef)vec->get(i))->count_descendants()) {
        v_mems_tmp->push(vec->get(i));
      }
    }
  }
  vec->clear();
  while (!v_mems_tmp->is_empty()) {
    vec->push(v_mems_tmp->pop());
  }
  delete v_mems_tmp;
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
static inline BOOL mem_equals_molecules_inner(Vector *v_log1,
                                              Vector *v_atoms_not_checked1,
                                              Vector *v_log2,
                                              Vector *v_atoms_not_checked2,
                                              int current_depth);

/* この段階で両膜は互いに等しい数の子孫膜を持ち、両膜内のアトムのファンクタの種類
 * およびその個数が完全に一致することが確認されている。
 * (i.e. 結果が「同型でない(偽)」になるならば、本膜におけるリンクの接続関係 or
 * 子孫膜が異なっていることを意味する)
 * 以降、少数派のアトムから順に根に定めていき、アトムを起点とする走査の実行に移っていく。
 */
static inline BOOL mem_equals_molecules(LmnMembraneRef mem1,
                                        LmnMembraneRef mem2,
                                        int current_depth) LMN_UNUSED;
static inline BOOL mem_equals_molecules(LmnMembraneRef mem1,
                                        LmnMembraneRef mem2,
                                        int current_depth) {
  unsigned int atom_n = mem1->atom_num();
  LMN_ASSERT(atom_n == mem2->atom_num());

  if (atom_n != mem2->atom_num()) {
    return FALSE;
  } else if (atom_n == 0) {
    return TRUE;
  } else {
    Vector *v_log1, *v_log2; /* 走査中に通過したアトムのログを管理するVector */
    Vector *v_atoms_not_checked1,
        *v_atoms_not_checked2; /* 同型性の判定の判定が済んでいないアトムの集合を管理するVector
                                  (各アトムへのポインタを保存) */
    int i, j;
    BOOL ret;

    /* 以降、未走査／走査済アトムを管理するvectorの初期化 */

    v_atoms_not_checked1 = new Vector(atom_n);
    v_atoms_not_checked2 = new Vector(atom_n);
    v_atoms_not_checked1->memset_tbl(0,
           sizeof(vec_data_t) * (v_atoms_not_checked1->get_cap()));
    v_atoms_not_checked2->memset_tbl(0,
           sizeof(vec_data_t) * (v_atoms_not_checked2->get_cap()));

    {
      Vector *atomvec_mem1, *atomvec_mem2;

      /* atomvec_memX (X = 1,2)は、膜memX 直下のアトムの情報を保持するVector。
       * 膜内のアトムをファンクタ毎に整理し、少数派のアトムからマッチングを開始できるようにする目的で使用する。
       */
      atomvec_mem1 = mem_mk_matching_vec(mem1);
      atomvec_mem2 = mem_mk_matching_vec(mem2);

      LMN_ASSERT(atomvec_mem1->get_num() == atomvec_mem2->get_num());

      /* ベクターatomvec_mem{1,2}には多数派のアトムから順に放りこまれているため、
       * ベクターv_atoms_not_checked{1,2}には多数派のアトムのポインタから順に
       * 放りこまれていくことになる。ゆえに、v_atoms_not_checked{1,2}をPOPしていく
       * ことで少数派のアトムから順に取り出していくことができるようになる。 */
      for (i = 0; i < atomvec_mem1->get_num(); i++) {
        for (j = 0;
             j < ((atomvec_data *)atomvec_mem1->get(i))->atom_ptrs->get_num();
             j++) {
          v_atoms_not_checked1->push(
              ((atomvec_data *)atomvec_mem1->get(i))->atom_ptrs->get(j));
          v_atoms_not_checked2->push(
              ((atomvec_data *)atomvec_mem2->get(i))->atom_ptrs->get(j));
        }
      }

      free_atomvec_data(atomvec_mem1);
      free_atomvec_data(atomvec_mem2);
    }

    LMN_ASSERT(v_atoms_not_checked1->get_num() == v_atoms_not_checked2->get_num());

    v_log1 = new Vector(atom_n);
    v_log2 = new Vector(atom_n);

    ret = mem_equals_molecules_inner(v_log1, v_atoms_not_checked1, v_log2,
                                     v_atoms_not_checked2, current_depth);
    delete v_log1;
    delete v_log2;
    delete v_atoms_not_checked1;
    delete v_atoms_not_checked2;

    return ret;
  }
}

/* 構造体'atomvec_data'に関係するメモリーの領域を解放する */
static void free_atomvec_data(Vector *vec) {
  unsigned int i;

  for (i = 0; i < vec->get_num(); ++i) {
    atomvec_data *ad = (atomvec_data *)vec->get(i);
    if (ad) {
      delete ad->atom_ptrs;
      LMN_FREE(ad);
    }
  }
  delete vec;
}

static BOOL mem_trace_links(LmnSymbolAtomRef a1, LmnSymbolAtomRef a2,
                            Vector *v_log1, Vector *v_log2, int current_depth,
                            BOOL need_to_check_this_membrane_processes);

static inline BOOL mem_equals_molecules_inner(Vector *v_log1,
                                              Vector *v_atoms_not_checked1,
                                              Vector *v_log2,
                                              Vector *v_atoms_not_checked2,
                                              int current_depth) {
  BOOL matched = TRUE;
  while (matched && !v_atoms_not_checked1->is_empty()) {
    LmnSymbolAtomRef a1;
    int i, j, k;

    /* 膜1内から1つアトムを取り出しこれを根とする。
     * 膜1内のアトムの内、(ファンクタが)少数派のものから順に根に定められていくことに注意せよ。
     */
    a1 = (LmnSymbolAtomRef)v_atoms_not_checked1->pop();

    /*fprintf(stdout, "fid(a1):%u\n", (unsigned
     * int)a1->get_functor());*/

    for (i = v_atoms_not_checked2->get_num(); i > 0; i--) {
      LmnSymbolAtomRef a2 = (LmnSymbolAtomRef)v_atoms_not_checked2->get(i - 1);
      /* 膜2内から根a1に対応するアトムの候補を取得 (注:ここの実装にVector.pop()は使用不可!!) */

      v_log1->clear();
      v_log2->clear();
      v_log1->memset_tbl(0, sizeof(vec_data_t) * (v_log1->get_cap()));
      v_log2->memset_tbl(0, sizeof(vec_data_t) * (v_log2->get_cap()));

      /* a2が本当にa1に対応するアトムであるか否かを実際にグラフ構造をトレースして確認する。
       * a2とa1とが1:1に対応する場合に限って matched に真が返り、
       * v_log{1,2}内にはa{1,2}を起点とする分子内の全アトムのアドレスが走査ログとして記録される。
       */
      matched = mem_trace_links(a1, a2, v_log1, v_log2, current_depth, FALSE);
      if (!matched) {
        continue;
      } else {
        /*fprintf(stdout, "fid(a2):%u\n", (unsigned
         * int)a2->get_functor());*/

        /* 両膜内に存在するある分子同士のマッチングに成功した場合にここに入る。
         * 膜2内の未マッチングのアトムを管理していたベクター(v_atoms_not_checked2)
         * から根a1に対応するアトムa2を除去する。 */
        LMN_ASSERT(v_log1->get_num() == v_log2->get_num());
        for (j = 0; j < v_atoms_not_checked2->get_num(); j++) {
          if (LMN_SATOM(v_atoms_not_checked2->get(j)) == a2) {
            v_atoms_not_checked2->pop_n(j);
            break;
          }
        }
        LMN_ASSERT(v_atoms_not_checked1->get_num() ==
                   v_atoms_not_checked2->get_num());

        /* ログ上に存在するすべてのアトムを、未チェックアトムのリストからPOPする
         */
        for (j = 0; j < v_log1->get_num(); j++) {
          for (k = 0; k < v_atoms_not_checked1->get_num(); k++) {
            if (LMN_SATOM(v_log1->get(j)) ==
                LMN_SATOM(v_atoms_not_checked1->get(k))) {
              v_atoms_not_checked1->pop_n(k);
              break;
            }
          }
        }

        for (j = 0; j < v_log2->get_num(); j++) {
          for (k = 0; k < v_atoms_not_checked2->get_num(); k++) {
            if (LMN_SATOM(v_log2->get(j)) ==
                LMN_SATOM(v_atoms_not_checked2->get(k))) {
              v_atoms_not_checked2->pop_n(k);
              break;
            }
          }
        }

        LMN_ASSERT(v_atoms_not_checked1->get_num() ==
                   v_atoms_not_checked2->get_num());
        break;
      }
    }
  }

  return matched;
}

/* アトムa1、a2を起点とする分子(=
 * あるアトムからリンクによって直接辿ることのできるプロセスの集合)
 * の構造が互いに一致するか否かを判定する。同型性判定を行う上での中心的役割を担っている。
 * 両分子構造が完全に一致する場合は、走査中に通過した全アトム(i.e.
 * 分子内の全アトム)のアドレスが ログ用のベクター(v_log1、v_log2)に保存される。
 *
 * なお、第5引数の 'current_depth' は、同型性判定対象となっている膜の親膜にまで
 * 走査が及ぶのを防ぐためのもの。子膜内のプロセスに走査対象が移る際に
 * current_depth は1増加し、
 * 逆に親膜内のプロセスに移る際は1減少する。同型性判定対象の膜の深さは0になっているため、
 * 0未満の深さに存在するプロセスは走査しないようにする。 */
static BOOL mem_trace_links(LmnSymbolAtomRef a1, LmnSymbolAtomRef a2,
                            Vector *v_log1, Vector *v_log2, int current_depth,
                            BOOL need_to_check_this_membrane_processes) {
  unsigned int i;
  int next_depth;

  /* 本メソッドを再帰的に呼び出していく過程で，a1(,a2)へのリンクを辿ることが親膜から子膜への遷移を意味する場合，
   * a1, a2の所属膜を対象とした同型性判定を行う必要がある */
  if (need_to_check_this_membrane_processes) {
    LmnMembraneRef mem1, mem2;

    /* ここの処理をする際，a1およびa2は共にプロキシアトムのはず */
    LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(a1->get_functor()) &&
               LMN_IS_PROXY_FUNCTOR(a2->get_functor()));

    mem1 = LMN_PROXY_GET_MEM(a1);
    mem2 = LMN_PROXY_GET_MEM(a2);

    if (!mem_equals_rec(mem1, NULL, mem2, NULL, CHECKED_MEM_DEPTH)) {
      return FALSE;
    }
  }

  v_log1->push((LmnWord)a1);
  v_log2->push((LmnWord)a2);

  /* a1、a2のファンクタが一致することを確認(不一致の場合は無条件に偽を返す) */
  if (a1->get_functor() != a2->get_functor()) {
    return FALSE;
  }

  /* 090808同型性判定バグ2対応
   * 親膜内のプロセスのうち、$outにつながっているものについては調べる必要がある
   */
  if (a1->get_functor() == LMN_IN_PROXY_FUNCTOR &&
      current_depth == CHECKED_MEM_DEPTH &&
      ((LmnSymbolAtomRef)a1->get_link(0U))->get_attr(1U) !=
          ((LmnSymbolAtomRef)a2->get_link(0U))->get_attr(1U)) {
    return FALSE;
  }

  /* a1(= a2) = $in
   * かつa1の所属膜が同型性判定対象膜である場合，a1の第0リンクの接続先である$outは同型性判定対象膜の親膜内の
   * プロセスということになり，トレースの対象に含めてはならない．この基準に基づき，次の変数iの初期値（0
   * or 1）が決定される． */
  if (a1->get_functor() == LMN_IN_PROXY_FUNCTOR &&
      current_depth == CHECKED_MEM_DEPTH) {
    i = 1;
  } else {
    i = 0;
  }

  for (; i < LMN_FUNCTOR_GET_LINK_NUM(a1->get_functor()); i++) {
    LmnLinkAttr attr1, attr2;

    attr1 = a1->get_attr(i);
    attr2 = a2->get_attr(i);

    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      /* アトムa1、a2の第iリンクの接続先が共にデータアトムのケース:
       *   接続先データアトムの値が等しいか検査.
       *   データアトムはa1, a2のみが接続先となるため, a1,
       * a2の次のリンク検査に移行 */
      if (a1->get_link(i) != a2->get_link(i)) {
        return FALSE;
      }
    } else if (!LMN_ATTR_IS_DATA(attr1) && !LMN_ATTR_IS_DATA(attr2)) {
      LmnSymbolAtomRef l1, l2;

      /* アトムa1、a2の第iリンクの接続先が共にシンボル(or
       * プロキシ)アトムのケース:
       *  1. 両アトムの第iリンクと接続するリンク番号をチェック
       *  2. 接続先シンボル(or プロキシ)アトムを取得し,
       * ログ上に存在するかどうかをチェック
       *     ログ上にまだ存在しない新規アトムの場合は,
       * 本メソッドを再帰呼び出しして
       *     a1およびa2を起点とする分子全体のマッチングを行う) */

      if (attr1 != attr2) {
        /* {c(X,Y), c(X,Y)} vs. {c(X,Y), c(Y,X)}
         * の例のように、2アトム間のリンクの接続順序が異なっている場合はFALSEを返す
         */
        return FALSE;
      }

      l1 = (LmnSymbolAtomRef)a1->get_link(i);
      l2 = (LmnSymbolAtomRef)a2->get_link(i);

      if ((v_log1->contains((LmnWord)l1) !=
           v_log2->contains((LmnWord)l2))) {
        /* 片方の膜においては、これまでのトレースで通過済みのアトムに還ってきた
         * (i.e. 分子内に環状の構造(= 閉路)が存在した)
         * ものの、もう片方の膜では同様の閉路が確認できず、構造の不一致が認められたために偽を返す
         */
        return FALSE;
      } else if (v_log1->contains((LmnWord)l1) &&
                 v_log2->contains((LmnWord)l2)) {
        /* 膜1、2内の対応する分子が共に閉路を形成した場合は、第(i+1)リンクの接続先のチェックに移る
         */
        continue;
      }

      /* a1-->l1 (, a2-->l2) なるリンクのトレースが親膜-->子膜 or
       * 子膜-->親膜なる
       * トレース対象プロセスの所属膜の切り替えを意味する際に，現在トレースしているプロセスの深さを表す変数の値を更新する．
       * (子膜への遷移の際に値が1増加し，逆に親膜内に戻る際は値が1減少する)
       *
       * (注) "a1=$out かつ l1=$in ならば子膜への遷移" としてはいけない．
       *      必ずa1, l1の所属膜が異なっていることも必要条件に含めるようにする．
       *      "{{'+'(L)}},
       * a(L)."のようなケースでは，間に膜の境界を含まないにも関わらず$inと$outが隣接する．
       */
      next_depth = current_depth;
      if (l1->get_functor() == LMN_IN_PROXY_FUNCTOR &&
          a1->get_functor() == LMN_OUT_PROXY_FUNCTOR &&
          LMN_PROXY_GET_MEM(l1) != LMN_PROXY_GET_MEM(a1)) {
        next_depth++;
      } else if (l1->get_functor() == LMN_OUT_PROXY_FUNCTOR &&
                 a1->get_functor() == LMN_IN_PROXY_FUNCTOR &&
                 LMN_PROXY_GET_MEM(l1) != LMN_PROXY_GET_MEM(a1)) {
        next_depth--;
      }

      /* "i = ((a1->get_functor() == LMN_IN_PROXY_FUNCTOR &&
       * current_depth == CHECKED_MEM_DEPTH) ? 1U : 0U)"
       * なる本forループにおけるiの初期化処理により，直後の探索対象アトム(l1,
       * l2)の所属膜が同型性判定対象外の膜になることはないはず */
      LMN_ASSERT(next_depth >= CHECKED_MEM_DEPTH);

      /*
       * a1-->l1 (, a2-->l2)
       * なるリンクが存在し，かつl1の所属膜がa1の所属膜の子膜である場合，
       * 遷移先の階層(i.e.
       * l1およびl2それぞれの所属膜)直下に含まれるプロセスの集合が互いに一致していることを確認する必要が生じる．
       *   例)
       *   mem1 = {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3),
       * {-L3, +R4}, p(L4, R4), {+L4, +R5}, p(L5, R5), {+L5, -R1}.} mem2 =
       * {p(L1, R1), {-L1, +R2}, p(L2, R2), {+L2, +R3}, p(L3, R3), {+L3, +R4},
       * p(L4, R4), {-L4, +R5}, p(L5, R5), {+L5, -R1}.}
       *   のようなmem1とmem2の比較を正しく行う上で必要な措置．
       *
       * (注) "ここをa1の所属膜とl1の所属膜が異なる場合"
       * なる判定を行うと処理が無限ループする可能性があるので注意すること．
       *   無限ループする例)
       *   mem1 = mem2 =
       * {r(lambda(cp(cp(L3,L4,L0),cp(L5,L6,L1),L2),lambda(L7,apply(L3,apply(L5,apply(L4,apply(L6,L7))))))).
       * {top. '+'(L0). '+'(L1). '+'(L2). }.}
       */
      if (!mem_trace_links(l1, l2, v_log1, v_log2, next_depth,
                           (next_depth > current_depth))) {
        return FALSE;
      }
    } else {
      /* アトムa1、a2の第iリンクの接続先アトムの種類(symbol or
       * data)が一致しないケース */
      return FALSE;
    }
  }

  return TRUE;
}

/* 本膜直下のすべてのアトム(子孫膜内のアトムは含まない)について、まずファンクタごとにグループ分けを行う。
 * グループ分けには構造体'atomvec_data'を用いる。構造体'atomvec_data'は整理されるアトムのファンクタのID(=
 * 0,1,2,...)と、
 * そのファンクタを持つ全アトムのアドレスを管理するベクター'atom_ptrs'とを情報として持つ。
 * 本膜直下のすべてのアトムのアドレスを、それぞれ対応する構造体'atomvec_data'内に整理し、
 * この構造体を本メソッドの戻り値となるベクターの要素として格納していく。
 *
 * 本同型性判定のアルゴリズムでは、「少数派のアトム」からマッチングを開始できるようにすることを
 * 目標としているため、先程構造体を整理したベクターについて、構造体内のアトム数が「多い順」にソートしてやる必要がある。
 * ここで「多い順」とした理由は、後のステップで本ベクターを(結果的に)POPしながらアトムのアドレス情報を取り出すことに
 * なるためである。(少数派のアトムから取り出されることになることに注意せよ) */
static Vector *mem_mk_matching_vec(LmnMembraneRef mem) {
  Vector *vec, *v_tmp;
  LmnFunctor f;
  LmnSymbolAtomRef a;
  AtomListEntry *ent;
  unsigned int
      anum_max; /* 膜内に存在するアトムをファンクタ毎にグループ化した際の、集合の大きさの最大値
                 */
  unsigned int i, j;

  vec = new Vector(1);
  vec->memset_tbl(0, sizeof(atomvec_data *) * vec->get_cap());
  anum_max = 0;

  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
                            atomvec_data *ad;

                            ad = LMN_MALLOC(atomvec_data);
                            ad->fid = f;
                            ad->atom_ptrs = new Vector(1);

                            /* 本膜直下のアトムの内、ファンクタがfであるもののアドレスをベクターatom_ptrs内に整理する。
                             * 後でソートする関係で、最も多くのアトムのアドレスを管理する構造体(atomvec_data)内のアトム数を求めている。
                             */
                            EACH_ATOM(a, ent, ({
                                        ad->atom_ptrs->push((LmnWord)a);
                                        if (ad->atom_ptrs->get_num() > anum_max) {
                                          anum_max = ad->atom_ptrs->get_num();
                                        }
                                      }));
                            /* ファンクタfを持つアトムが本膜内に1つも存在しない場合、このファンクタのために割いたメモリーの領域を解放する。
                             * これを怠るとメモリリークが起こるので注意!! */
                            if (ad->atom_ptrs->is_empty()) {
                              delete ad->atom_ptrs;
                              LMN_FREE(ad);
                            } else {
                              vec->push((vec_data_t)ad);
                            }
                          }));

  /* sort */
  if (anum_max > 0) {
    v_tmp = new Vector(vec->get_num());

    for (i = 1; i <= anum_max; i++) {
      for (j = 0; j < vec->get_num(); j++) {
        atomvec_data *ad = (atomvec_data *)vec->get(j);
        if (ad->atom_ptrs->get_num() == i) {
          v_tmp->push((vec_data_t)ad);
        }
      }
    }
    vec->clear();
    /* 構造体内のアトム数が「多い順」にソート */
    while (!v_tmp->is_empty()) {
      vec->push(v_tmp->pop());
    }
    delete v_tmp;
  }

  return vec;
}

/* 未使用コード:
 * mem_mk_matching_vec/1 の戻り値である2つのVectorの比較を行う
 * 2つの膜内に存在するプロセスの種類および個数が完全に一致するならばTRUEを返す
 */
BOOL mem_is_the_same_matching_vec(Vector *vec1, Vector *vec2) {
  unsigned int i, j;

  for (i = 0; i < vec1->get_num(); i++) {
    BOOL is_the_same_functor;

    is_the_same_functor = FALSE;
    for (j = 0; j < vec2->get_num(); j++) {
      if (((atomvec_data *)vec1->get(i))->fid ==
          ((atomvec_data *)vec2->get(j))->fid) {
        is_the_same_functor = TRUE;
        break;
      }
    }

    if (!is_the_same_functor ||
        ((atomvec_data *)vec1->get(i))->atom_ptrs->get_num() !=
            ((atomvec_data *)vec2->get(j))->atom_ptrs->get_num()) {
      return FALSE;
    }
  }

  return TRUE;
}

/* 膜parentから膜memを取り除く.
 * memのメモリ管理は呼び出し側で行う. */
void LmnMembrane::remove_mem(LmnMembraneRef mem) {
  LMN_ASSERT(this);
  if (this->mem_child_head() == mem)
    this->child_head = mem->mem_next();
  if (mem->mem_prev())
    mem->prev->next = mem->mem_next();
  if (mem->mem_next())
    mem->next->prev = mem->mem_prev();
}

/* 膜mem以下全ての階層のメモリを破棄する */
void LmnMembrane::free_rec() {
  this->drop();
  delete this;
}

/* 膜parentから膜memを取り除き, mem以下の階層全てを解放する. */
void LmnMembrane::delete_mem(LmnMembraneRef mem) {
  this->remove_mem(mem);
  mem->free_rec();
}

/* 自身を含めた全ての先祖膜を起こす */
void LmnMembrane::activate_ancestors(){
  LmnMembraneRef cur;
  for (cur = this; cur; cur = cur->mem_parent()) {
    this->set_active(TRUE);
  }
}
BOOL LmnMembrane::nmems(unsigned int count) {
  unsigned int i;
  LmnMembraneRef mp = this->mem_child_head();
  for (i = 0; mp && i <= count; mp = mp->mem_next(), i++)
    ;
  return i == count;
}

/* 子膜の数を返す */
int LmnMembrane::child_mem_num() {
  unsigned int i;
  LmnMembraneRef mp = this->mem_child_head();
  for (i = 0; mp; mp = mp->mem_next(), i++)
    ;
  return i;
}

/* add newmem to parent child membranes */

/* make atom which functor is f, and push atom into mem */
LmnSymbolAtomRef lmn_mem_newatom(LmnMembraneRef mem, LmnFunctor f) {
  LmnSymbolAtomRef atom = lmn_new_atom(f);
  mem_push_symbol_atom(mem, atom);
  return atom;
}

/* return # of child membranes */
unsigned int LmnMembrane::count_children() {
  LmnMembraneRef c;
  unsigned int n = 0;
  for (c = this->mem_child_head(); c; c = c->mem_next())
    n++;
  return n;
}

/* return # of descendant membranes */
unsigned int LmnMembrane::count_descendants() {
  LmnMembraneRef c;
  unsigned int n = 0;

  for (c = this->mem_child_head(); c; c = c->mem_next()) {
    n += 1 + c->count_descendants();
  }
  return n;
}

/* return TRUE if # of freelinks in mem is equal to count */
BOOL LmnMembrane::nfreelinks(unsigned int count) {
  AtomListEntry *ent = this->get_atomlist(LMN_IN_PROXY_FUNCTOR);
  if (!ent) {
    return count == 0;
  } else {
    return count == ent->n;
  }
}
void lmn_mem_remove_data_atom(LmnMembraneRef mem, LmnDataAtomRef atom,
                              LmnLinkAttr attr) {
  mem->data_atom_dec();
}

void mem_remove_symbol_atom(LmnMembraneRef mem, LmnSymbolAtomRef atom) {
  LmnFunctor f = atom->get_functor();
  {
    AtomListEntry *ent = mem->get_atomlist(f);
    ent->remove(atom);
  }

  if (LMN_IS_PROXY_FUNCTOR(f)) {
    LMN_PROXY_SET_MEM(atom, NULL);
  } else if (f != LMN_UNIFY_FUNCTOR) {
    mem->symb_atom_dec();
  }
}

/* 膜memからアトムatomを取り除く.
 * atomの接続先データアトムが存在するならば, そのデータアトムも取り除く.
 * atom自体のメモリ管理は呼び出し側が行う. */
void mem_remove_symbol_atom_with_buddy_data(LmnMembraneRef mem,
                                            LmnSymbolAtomRef atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(atom->get_attr(i))) {
      lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)atom->get_link(i),atom->get_attr(i));
    } else if (LMN_ATTR_IS_HL(atom->get_attr(i))) {
      mem_remove_symbol_atom(mem,
                             (LmnSymbolAtomRef)atom->get_link(i));
    }
  }
  mem_remove_symbol_atom(mem, atom);
}

void lmn_mem_remove_atom(LmnMembraneRef mem, LmnAtomRef atom,
                         LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)atom, attr);
  } else {
    mem_remove_symbol_atom(mem, (LmnSymbolAtomRef)atom);
  }
}

void move_atom_to_atomlist_head(LmnSymbolAtomRef a, LmnMembraneRef mem) {
  //  move_symbol_atom_to_atomlist_head(LMN_SATOM(a), mem); // ueda
  move_symbol_atom_to_atomlist_head(a, mem);
}

void move_atomlist_to_atomlist_tail(LmnSymbolAtomRef a, LmnMembraneRef mem) {
  move_symbol_atomlist_to_atomlist_tail(a, mem);
}

void move_atom_to_atom_tail(LmnSymbolAtomRef a, LmnSymbolAtomRef a1,
                            LmnMembraneRef mem) {
  move_symbol_atom_to_atom_tail(a, a1, mem);
}

void lmn_mem_delete_atom(LmnMembraneRef mem, LmnAtomRef atom,
                         LmnLinkAttr attr) {
  lmn_mem_remove_atom(mem, atom, attr);
  lmn_free_atom(atom, attr);
}

void lmn_mem_push_atom(LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
    mem->data_atom_inc();
  } else { /* symbol atom */
    // mut.lock();
    mem_push_symbol_atom(mem, (LmnSymbolAtomRef)atom);
    // mut.unlock();
  }
}

void alter_functor(LmnMembraneRef mem, LmnSymbolAtomRef atom, LmnFunctor f) {
  mem_remove_symbol_atom(mem, atom);
  atom->set_functor(f);
  mem_push_symbol_atom(mem, atom);
}

/* ルールセットnewを膜memに追加する */
void lmn_mem_add_ruleset(LmnMembraneRef mem, LmnRuleSetRef ruleset) {
  LMN_ASSERT(ruleset);
  lmn_mem_add_ruleset_sort(&(mem->rulesets), ruleset);
}
void LmnMembrane::copy_rules(LmnMembraneRef src) {
  int i;
  for (i = 0; i < src->ruleset_num(); i++) {
    lmn_mem_add_ruleset(this, new LmnRuleSet(*lmn_mem_get_ruleset(src, i)));
  }
}
void LmnMembrane::clearrules() {
  unsigned int i;
  for (i = 0; i < this->rulesets.size(); i++) {
    LmnRuleSetRef rs = this->rulesets[i];
    if (rs->is_copy()) {
      delete rs;
    }
  }
  this->rulesets.clear();
}

/* シンボルアトムatom0と、シンボルorデータアトムatom1の間にリンクを張る。*/
void newlink_symbol_and_something(LmnSymbolAtomRef atom0, int pos,
                                  LmnAtomRef atom1, LmnLinkAttr attr) {
  atom0->set_link(pos, atom1);
  atom0->set_attr(pos, attr);
  if (!LMN_ATTR_IS_DATA(attr)) {
    ((LmnSymbolAtomRef)atom1)->set_link(LMN_ATTR_GET_VALUE(attr),
                       atom0);
    ((LmnSymbolAtomRef)atom1)->set_attr(LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos));
  }
}
#ifdef USE_FIRSTCLASS_RULE
Vector *LmnMembrane::firstclass_rulesets() {
  return this->firstclass_rulesets;
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
  LmnRulesetId del_id = fcr->id;

  for (int i = 0; i < mem->firstclass_rulesets->get_num(); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)mem->firstclass_rulesets->get(i);
    if (rs->id != del_id)
      continue;

    /* move successors forward */
    for (int j = i; j < mem->firstclass_rulesets->get_num() - 1; j++) {
      LmnRuleSetRef next =
          (LmnRuleSetRef)mem->firstclass_rulesets->get(j + 1);
      mem->firstclass_rulesets->set(j, (vec_data_t)next);
    }

    mem->firstclass_rulesets->num--;
    return;
  }

  LMN_ASSERT(FALSE); // "attempt to delete an absent firstclass ruleset"
}

void LmnMembrane::delete_ruleset(LmnRulesetId del_id) {
  for (int i = 0; i < rulesets.size(); i++) {
    if (rulesets[i]->id != del_id)
      continue;
    
    /* move successors forward */
    for (int j = i; j < rulesets.size() - 1; j++) {
      rulesets[j] = rulesets[j + 1];
    }
    
    //    mem_rulesets->num--;
    rulesets->resize(rulesets.size()-1);
    break;
  }
}

#endif
