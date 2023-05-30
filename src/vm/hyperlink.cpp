/*
 * hyperlink.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

#include "hyperlink.h"
#include "atom.h"
#include "atomlist.hpp"
#include "fmt/core.h"

#if SIZEOF_LONG == 4
#define EMPTY_KEY 0xffffffffUL
#define DELETED_KEY 0xfffffffeUL
#elif SIZEOF_LONG == 8
#define EMPTY_KEY 0xffffffffffffffffUL
#define DELETED_KEY 0xfffffffffffffffeUL
#endif

/* ----------------------------------------------------------------------- *
 *  hyperlink system                                                       *
 * ----------------------------------------------------------------------- */

/*
 * 非決定実行への対応 :
 *   非決定実行に対応させる場合は並列化が必要
 *   -
 * commit命令での作業配列のコピー操作に、newhlinkなどでのシンボルアトム生成を対応させる
 *   - グローバル変数hl_sameproccxt は実行中のルールごとに必要
 */

/* prototype */
void               sht_print(SimpleHashtbl *sht);
void               hs_print(HashSet *hs);

static inline unsigned long hyperlink_new_id() { return env_gen_next_id(); /* nd実行のために変更 */ }

// HyperLink *lmn_hyperlink_make(LmnSymbolAtomRef sa)
HyperLink::HyperLink(LmnSymbolAtomRef at) {
  this->atom = at;
  this->rank = 0;
  this->mem  = nullptr;
  this->id   = hyperlink_new_id();
  //  hl->usrid = 0;
  this->parent   = this;
  this->children = nullptr;
  this->attrAtom = nullptr;
  this->attr     = 0;

  at->set_link(1, this);
  at->set_attr(1, 0);

  //  hashtbl_put(at_hl, (HashKeyType)at, (HashValueType)hl);
  //  printf("lmn_hyperlink_make %p -> %p\n", hl, LMN_SATOM(hl->atom));
}
void HyperLink::put_attr(LmnAtomRef attrAtom, LmnLinkAttr attr) {
  this->attrAtom = attrAtom;
  this->attr     = attr;
}
void lmn_hyperlink_make_with_attr(LmnSymbolAtomRef at, LmnAtomRef attrAtom, LmnLinkAttr attr) {
  new HyperLink(at);
  (lmn_hyperlink_at_to_hl(at))->put_attr(attrAtom, attr);
}

/* 新しいhyperlinkの生成 */
LmnSymbolAtomRef lmn_hyperlink_new() {
  LmnSymbolAtomRef atom;

  atom = lmn_new_atom(LMN_HL_FUNC);
  atom->set_id(hyperlink_new_id());
  new HyperLink(atom);

  return atom;
}

LmnSymbolAtomRef lmn_hyperlink_new_with_attr(LmnAtomRef attrAtom, LmnLinkAttr attr) {
  LmnSymbolAtomRef atom;
  atom = lmn_hyperlink_new();
  (lmn_hyperlink_at_to_hl(atom))->put_attr(attrAtom, attr);
  return atom;
}

/* rootまでの全ての親のrankにdの値を加算する */
void HyperLink::rank_calc(int d) {
  HyperLink *parent, *current;

  this->rank += d;
  current    = this;
  parent     = this->parent;
  while (parent != current) {
    //    pro1++;
    parent->rank += d;
    current      = parent;
    parent       = parent->parent;
  }
}

/* HyperLinkのatom, mem, attrAtom, attrのみを交換する */
void HyperLink::swap_atom(HyperLink *hl2) {
  LmnSymbolAtomRef t_atom;
  LmnMembraneRef   t_mem;

  t_atom     = this->atom;
  this->atom = hl2->atom;
  hl2->atom  = t_atom;

  this->atom->set_link(1, this);
  hl2->atom->set_link(1, hl2);

  t_mem     = this->mem;
  this->mem = hl2->mem;
  hl2->mem  = t_mem;
}

/* 子表に格納されている子のうち先頭のものを返す、子表が無ければNULLを返す */
HyperLink *HyperLink::head_child() {
  HashSet *children = this->children;
  if (children) {
    HashSetIterator it;

    for (it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
      auto *child = (HyperLink *)hashsetiter_entry(&it);
      if ((HashKeyType)child < DELETED_KEY) {
        return child;
      }
    }
  }

  return nullptr;
}

/*
 * atom の削除に呼応してHyperLink 構造体を削除する（最適化 ver.）
 *
 * 子をたくさん持つHyperLink を削除してしまうと,
 * 削除後の木構造の再構築に時間がかかる（全ての子に対して親の更新を行なうなど）ため、
 * 削除するHyperLink は木構造の葉であることが望ましい
 *
 * HyperLink 木は変更せずに、HyperLink に対応しているatom
 * を親子間での交換を繰り返して
 * 末端に向かって移動させ、葉に到達した時点でその位置のHyperLink を削除する
 */
void lmn_hyperlink_delete(LmnSymbolAtomRef at) {
  HyperLink *hl = lmn_hyperlink_at_to_hl(at);

  if (hl) {
    HyperLink *parent;

    while (hl->children) {
      HyperLink *child = hl->head_child();
      hl->swap_atom(child);
      hl = child;
    }

    parent = hl->parent;
    if (parent != hl) {
      parent->children->delete_entry((HashKeyType)hl);
      parent->rank_calc(-1);
      if (parent->rank == 0) {
        delete parent->children;
        parent->children = nullptr;
      }
    }

    // LMN_FREE(hl);
    delete hl;
  }
}

/* atom の削除に呼応してHyperLink 構造体を削除する
 *
 * 旧式、使用していないが一応残しておく
 * hyperlinkの削除では何をしているかを把握するためにはいいかも
 */
void lmn_hyperlink_delete_old(LmnSymbolAtomRef at) {
  HyperLink *hl, *parent;
  HashSet   *children;

  hl = lmn_hyperlink_at_to_hl(at);
  if (!hl)
    return;

  parent   = hl->parent;
  children = hl->children;

  if (parent != hl) {
    /* root でない場合：
     * 親の子表から自身を削除し, 親のrankを-1した後,
     *   子がいる   -> 親の子表に自身の子を移す
     *   子がいない -> そのまま削除
     */

    parent->children->delete_entry((HashKeyType)hl);
    //    parent->rank--;
    parent->rank_calc(-1);
    if (parent->rank == 0) {
      delete parent->children;
      parent->children = nullptr;
    }

    if (children) { // 子表があるとき
      HashSetIterator it;
      for (it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
        auto *tmp = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)tmp < DELETED_KEY) {
          parent->children->add((HashKeyType)tmp);
          children->delete_entry((HashKeyType)tmp);
          tmp->parent = parent;
        }
      }
    }
  } else {
    /* root である場合：
     *   子がいる   -> rankが最大の子を新rootにする
     *   子がいない -> そのまま削除
     */
    if (children) { /* 子表があるとき */
      HyperLink      *newroot;
      HashSetIterator it;

      newroot = nullptr;
      for (it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
        newroot = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)newroot < DELETED_KEY) {
          break; /* 現状では先頭の子を新しい親にしている */
        }
      }

      /* 新rootが決定 */
      children->delete_entry((HashKeyType)newroot); /* rootの子表から新rootを除去 */
      newroot->parent = newroot;

      if (!newroot->children) {
        newroot->children = new HashSet(children->num);
      }

      for (it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
        auto *tmp = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)tmp < DELETED_KEY) {
          newroot->children->add((HashKeyType)tmp);
          children->delete_entry((HashKeyType)tmp);
          tmp->parent = newroot;
        }
      }

      newroot->rank = parent->rank - 1;
      if (newroot->rank == 0) {
        delete newroot->children;
        newroot->children = nullptr;
      }
    }
  }

  delete children;

  LMN_FREE(hl);
}

/* hyperlinkのコピー
 * <=> 新しいHyperLink 構造体を生成し、newatomに対応づけた後、oriatomと併合する
 */
void lmn_hyperlink_copy(LmnSymbolAtomRef newatom, LmnSymbolAtomRef oriatom) {
  HyperLink *newhl, *orihl;

  orihl = lmn_hyperlink_at_to_hl(oriatom);
  new HyperLink(newatom);
  newhl = lmn_hyperlink_at_to_hl(newatom);

  (orihl->get_root())->lmn_unify(newhl, LMN_HL_ATTRATOM(orihl), LMN_HL_ATTRATOM_ATTR(orihl));
}

/* Union-Find algorithm の最適化 (Path Compression)
 *   あるHyperLink からlmn_hyperlink_get_root でroot まで辿ったとき、
 *   その経路上にある全てのHyperLink をroot の直接の子として再設定する
 */
void HyperLink::path_compression(Vector *children) {
  int i, n;

  n = children->get_num();
  for (i = 0; i < n; i++) {
    HyperLink *hl, *old_parent;

    hl         = (HyperLink *)children->get(i);
    old_parent = hl->parent;

    if (old_parent != this) {
      HashSet *old_parent_children;
      int      j, sub_rank;

      /* 旧親に対する処理 */
      old_parent_children = old_parent->children;
      old_parent_children->delete_entry((HashKeyType)hl);
      sub_rank = hl->rank + 1;
      for (j = i + 1; j < n; j++) {
        ((HyperLink *)children->get(j))->rank -= sub_rank;
      }

      if (old_parent_children->num == 0) {
        delete old_parent_children;
        old_parent->children = nullptr;
      }

      /* 新親(root)に対する処理 */
      hl->parent = this;
      this->children->add((HashKeyType)hl);
    }
  }
}
void hyperlink_path_compression(HyperLink *root, Vector *children) {
  int i, n;

  n = children->get_num();
  for (i = 0; i < n; i++) {
    HyperLink *hl, *old_parent;

    hl         = (HyperLink *)children->get(i);
    old_parent = hl->parent;

    if (old_parent != root) {
      HashSet *old_parent_children;
      int      j, sub_rank;

      /* 旧親に対する処理 */
      old_parent_children = old_parent->children;
      old_parent_children->delete_entry((HashKeyType)hl);
      sub_rank = hl->rank + 1;
      for (j = i + 1; j < n; j++) {
        ((HyperLink *)children->get(j))->rank -= sub_rank;
      }

      if (old_parent_children->num == 0) {
        delete old_parent_children;
        old_parent->children = nullptr;
      }

      /* 新親(root)に対する処理 */
      hl->parent = root;
      root->children->add((HashKeyType)hl);
    }
  }
}

/* root を返す */
HyperLink *HyperLink::get_root() {
  HyperLink *parent_hl, *current_hl;
  if (this->parent == this)
    return this;
  current_hl = this;
  parent_hl  = this->parent;

  /* hlとrootの間に他のHyperLinkが無ければpath compressionは起こらない
   * ＝ 要素数が2以下であれば、path compressionは起こらない
   */
  if (parent_hl->element_num() <= 2) {
    while (parent_hl != current_hl) {
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }
  } else {
    Vector children;
    children.init(parent_hl->element_num());

    while (parent_hl != current_hl) {
      children.push((LmnWord)current_hl);
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }

    if (!children.is_empty()) {
      parent_hl->path_compression(&children); /* parent_hlはrootになっている */
    }

    children.destroy();
  }

  return parent_hl;
}
HyperLink *lmn_hyperlink_get_root(HyperLink *hl) {
  HyperLink *parent_hl, *current_hl;
  if (hl->parent == hl)
    return hl;
  current_hl = hl;
  parent_hl  = hl->parent;

  /* hlとrootの間に他のHyperLinkが無ければpath compressionは起こらない
   * ＝ 要素数が2以下であれば、path compressionは起こらない
   */
  if (parent_hl->element_num() <= 2) {
    while (parent_hl != current_hl) {
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }
  } else {
    Vector children;
    children.init(parent_hl->element_num());

    while (parent_hl != current_hl) {
      children.push((LmnWord)current_hl);
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }

    if (!children.is_empty()) {
      parent_hl->path_compression(&children); /* parent_hlはrootになっている */
    }

    children.destroy();
  }

  return parent_hl;
}

/* child をparent の子として併合する（parent, childは共にroot）*/
HyperLink *HyperLink::unify(HyperLink *child, LmnAtomRef attrAtom, LmnLinkAttr attr) {
  child->parent = this;
  if (!this->children) {
    this->children = new HashSet(2);
  }
  this->children->add((HashKeyType)child);
  this->rank      = this->rank + child->rank + 1;
  this->attrAtom  = attrAtom;
  this->attr      = attr;
  child->attrAtom = 0;
  child->attr     = 0;

  return this;
}

/* 2 つのhyperlink を併合し、親となった方を返す
 *   rank のより大きい(子を多く持つ)方を親とする
 *   rankが等しい場合はhl1をhl2の親とする
 *   attrで指定された属性を併合後のハイパーリンクの属性とする。
 * */
HyperLink *HyperLink::lmn_unify(HyperLink *hl2, LmnAtomRef attrAtom, LmnLinkAttr attr) {
  HyperLink *root1, *root2, *result;
  int        rank1, rank2;

  root1 = this->get_root();
  root2 = hl2->get_root();

  if (root1 == root2)
    return root1;

  rank1 = this->rank;
  rank2 = hl2->rank;
  //  printf("rank %p %d %p %d\n", hl1, rank1, hl2, rank2);
  if (rank1 >= rank2) {
    result = root1->unify(root2, attrAtom, attr);
  } else {
    result = root2->unify(root1, attrAtom, attr);
  }

  return result;
}

/* '!'アトムのポインタ --> 対応するHyperLink 構造体のポインタ */
HyperLink *lmn_hyperlink_at_to_hl(LmnSymbolAtomRef at) { return (HyperLink *)at->get_link(1); }

/* HyperLink 構造体のポインタ --> 対応する'!'アトムのポインタ */
LmnSymbolAtomRef HyperLink::hl_to_at() {
  //  if (hl->atom) return hl->atom;
  //  else return 0;
  return this->atom;
}

/* rank を返す */
int HyperLink::lmn_rank() { return LMN_HL_RANK(this->get_root()); }

/* hyperlink の要素数(rank + 1)を返す */
int HyperLink::element_num() { return (this->lmn_rank() + 1); }

/* hyperlink 同士の比較 */
BOOL HyperLink::eq_hl(HyperLink *hl2) { return (this->get_root())->hl_to_at() == (hl2->get_root())->hl_to_at(); }

/* hyperlink 同士の比較（'!'アトムポインタから直接） */
BOOL lmn_hyperlink_eq(LmnSymbolAtomRef atom1, LmnLinkAttr attr1, LmnSymbolAtomRef atom2, LmnLinkAttr attr2) {
  return LMN_ATTR_IS_HL(attr1) && LMN_ATTR_IS_HL(attr2) &&
         (lmn_hyperlink_at_to_hl(atom1))->eq_hl(lmn_hyperlink_at_to_hl(atom2));
}

/* hyperlink を1 つ出力
 *   hyperlink が1 つでも出力されるとTRUE を返す */
BOOL hyperlink_print(LmnMembraneRef mem, BOOL *flag, int *group, int *element) {
  AtomListEntryRef atomlist;
  LmnMembraneRef   m;
  LmnSymbolAtomRef atom;
  HyperLink       *hl, *parent;
  HashSet         *children;
  int              WIDTH;
  BOOL             result;
  FILE            *f; /*  出力先は呼び出し側から指定させたい */

  f      = stdout;
  result = FALSE;
  WIDTH  = 22;
  if ((atomlist = mem->get_atomlist(LMN_HL_FUNC))) {
    EACH_ATOM(atom, atomlist, ({
                result = TRUE;

                if (!(*flag)) {
                  fprintf(f, "%9s %9s %13s %5s %5s\n", "[hl_ID]", "[parent]", "[linked with]", "[num]",
                          "[direct children ( inside info )]");
                  (*flag) = TRUE;
                }
                hl = lmn_hyperlink_at_to_hl(atom);

                /* hl_ID */
                //      fprintf(f, "%9lx", LMN_ATOM(atom));
                // fprintf(f, "%9lx", LMN_HL_ID(lmn_hyperlink_at_to_hl(atom)));
                fprintf(f, "%9lu", LMN_HL_ID(lmn_hyperlink_at_to_hl(atom)));
                //      fprintf(f, "%9lx",
                //      LMN_HL_ID(lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl(atom))));

                /* parent */
                if ((parent = hl->parent) != hl) {
                  //        fprintf(f, " %9lx", LMN_ATOM(parent->atom));
                  // fprintf(f, " %9lx", LMN_HL_ID(parent));
                  fprintf(f, " %9lu", LMN_HL_ID(parent));
                } else {
                  (*group)++;
                  fprintf(f, " %9s", "root");
                }

                /* linked with */
                if (!LMN_ATTR_IS_DATA(atom->get_attr(0)) && atom->get_link(0)) {
                  fmt::print(f, " {:13}", ((LmnSymbolAtomRef)atom->get_link(0))->str());
                } else {
                  fprintf(f, " %13s", "---");
                }

                /* element num */
                fprintf(f, " %5d  ", hl->element_num());

                /* (direct children) */
                if ((children = hl->children)) {
                  HashSetIterator hsit;
                  HyperLink      *ch_hl;
                  int             i, n, width;
                  BOOL            comma;

                  width = 0;
                  comma = FALSE;
                  i     = 1;
                  for (hsit = hashset_iterator(children); !hashsetiter_isend(&hsit); hashsetiter_next(&hsit)) {
                    if ((HashKeyType)(ch_hl = (HyperLink *)hashsetiter_entry(&hsit)) < DELETED_KEY) {
                      if (!comma) {
                        comma = TRUE;
                      } else {
                        if (width > WIDTH - 4) {
                          fprintf(f, ",\n%41s", "");
                          width = 0;
                        } else {
                          fprintf(f, ",");
                        }
                      }

                      //            fprintf(f, "%lx", LMN_ATOM(ch_hl->atom));
                      // fprintf(f, "%lx%n", LMN_HL_ID(ch_hl), &n);
                      fprintf(f, "%lu%n", LMN_HL_ID(ch_hl), &n);
                      width += n;
                      i++;
                    }
                  }
                  fprintf(f, ".");
                }

                (*element)++;
                fprintf(f, "\n");
              }));
  }
  //  else result = FALSE;

  for (m = mem->mem_child_head(); m; m = m->mem_next()) {
    result = (hyperlink_print(m, flag, group, element) || result);
  }

  return result;
}

/* num >= 0 */
int hyperlink_print_get_place(int num) {
  int place, tmp;

  place = 1;
  tmp   = num;
  while (tmp >= 10) {
    tmp = tmp / 10;
    place++;
  }

  return place;
}

void lmn_hyperlink_print(LmnMembraneRef gr) { lmn_hyperlink_print(stdout, gr); }

/* グローバルルート膜から順に辿って、存在する全てのhyperlink を出力する */
void lmn_hyperlink_print(FILE *fp, LmnMembraneRef gr) {
  FILE *f;
  int   WIDTH, group, element, place_g, place_e;
  char  tail_g[8], tail_e[14];
  BOOL  flag;

  f       = fp;
  element = 0;
  group   = 0;
  flag    = FALSE;
  fprintf(f,
          "== HyperLink "
          "=============================================================%n\n",
          &WIDTH);
  if (!hyperlink_print(gr, &flag, &group, &element))
    fprintf(f, "There is no hyperlink.\n");

  place_g = hyperlink_print_get_place(group);
  place_e = hyperlink_print_get_place(element);

  if (group < 2)
    sprintf(tail_g, "group, ");
  else
    sprintf(tail_g, "groups,");

  if (element < 2)
    sprintf(tail_e, "element =====");
  else
    sprintf(tail_e, "elements ====");

  place_e = WIDTH - sizeof(tail_g) - sizeof(tail_e) - (place_g + 1) - (place_e + 1);
  while (place_e > 0) {
    fprintf(f, "=");
    place_e--;
  }
  fprintf(f, " %d %s %d %s\n", group, tail_g, element, tail_e);
  //  fprintf(f, "\n");
}

/* 旧式
 * [hl_ID]  [parent] [linked with] [num] [direct children (inside info)]
 */
void hyperlink_print_old() {
  //  BOOL item;
  //
  //  item = FALSE;
  //  printf("== HyperLink
  //  =============================================================\n"); if
  //  (at_hl) {
  //    HashIterator it;
  //    HyperLink *hl, *parent;
  //    LmnSymbolAtomRef atom;
  //    HashSet *children;
  //
  //    for (it = hashtbl_iterator(at_hl); !hashtbliter_isend(&it);
  //    hashtbliter_next(&it)) {
  //      if ((HashValueType)(hl = (HyperLink *)(hashtbliter_entry(&it)->data))
  //      < DELETED_KEY) {
  //        if (!item) {
  //          printf("%9s %9s %13s %5s %5s\n", "[hl_ID]", "[parent]", "[linked
  //          with]", "[num]", "[direct children ( inside info )]"); item =
  //          TRUE;
  //        }
  //
  //        /* hl_ID */
  //        printf("%9lx", (atom = hl->atom));
  //
  //        /* parent */
  //        if ((parent = hl->parent) != hl)
  //          printf(" %9lx", parent->atom);
  //        else
  //          printf(" %9s", "root");
  //
  //        /* linked with */
  //        if (!LMN_ATTR_IS_DATA(atom->get_attr(0)) &&
  //        atom->get_link(0))
  //          printf(" %13s", atom->get_link(0)->str());
  //        else
  //          printf(" %13s", "---");
  //
  //        /* num, rank */
  ////        if (hl->parent == hl)
  ////          printf(" %5d  ", hl->rank+1);
  ////        else
  ////          printf(" %5s  ", "---");
  //        printf(" %5d  ", lmn_hyperlink_element_num(hl));
  ////        printf(" %5d  ", hl->rank);
  //
  //        /* (direct children) */
  //        if ((children = hl->children)) {
  //          HashSetIterator hsit;
  //          HyperLink *ch_hl;
  //          BOOL comma;
  //          int i;
  //
  //          comma = FALSE;
  //          i = 1;
  //          for (hsit = hashset_iterator(children); !hashsetiter_isend(&hsit);
  //          hashsetiter_next(&hsit)) {
  //            if ((HashKeyType)(ch_hl = (HyperLink *)hashsetiter_entry(&hsit))
  //            < DELETED_KEY) {
  //              if (comma) {
  //                if (i % 4 == 1) printf(",\n%41s", "");
  //                else printf(",");
  //              }
  //              else comma = TRUE;
  //              printf("%lx", ch_hl->atom);
  //              i++;
  //            }
  //          }
  //          printf(".");
  //        }
  //
  //        printf("\n");
  //      }
  //
  //    }
  //
  //  }
  //  if (!item) printf("There is no hyperlink.\n");
  //  printf("==========================================================================\n");
}

void lmn_hyperlink_print_old() { hyperlink_print_old(); }

/* for debug @seiji */
void sht_print(SimpleHashtbl *sht) {
  if (!sht) {
    printf(">>>> NULL\n");
  } else {
    int n = hashtbl_num(sht);
    int i;
    printf(">>>> sht %p num %d cap %d\n", sht, n, sht->cap);
    for (i = 0; i < sht->cap; i++) {
      if (sht->tbl[i].key == EMPTY_KEY)
        printf("%3d: key: %p data: %p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data);
      else {
        if (sht->tbl[i].data < DELETED_KEY) {
          printf("%3d: key: %p data: %p->%p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data,
                 (void *)((HyperLink *)sht->tbl[i].data)->atom);
        } else {
          printf("%3d: key: %p data: %p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data);
        }
      }
    }
  }
}

/* for debug @seiji */
void hs_print(HashSet *hs) {
  if (!hs) {
    printf(">>>> NULL\n");
  } else {
    int n = hashtbl_num(hs);
    int i;
    printf(">>>>  hs %p num %d cap %d\n", hs, n, hs->cap);
    for (i = 0; i < hs->cap; i++) {
      //      if (sht->tbl[i].key == EMPTY_KEY) printf("%3d: %s\n", i, "empty");
      if (hs->tbl[i] < DELETED_KEY)
        printf("%3d: key: %p\n", i, (void *)hs->tbl[i]);
      else {
        if (hs->tbl[i] < DELETED_KEY) {
          printf("%3d: key: %p\n", i, (void *)hs->tbl[i]);
        } else {
          printf("%3d: key: %p\n", i, (void *)hs->tbl[i]);
        }
      }
    }
  }
}

/* ----------------------------------------------------------------------- *
 *  same proccess context (同名型付きプロセス文脈)                         *
 *  hyperlink の接続関係を利用したルールマッチング最適化                   *
 * ----------------------------------------------------------------------- */

// FindProcCxt *findproccxt;
//
// void lmn_sameproccxt_init2()
//{
//  findproccxt = LMN_MALLOC<FindProcCxt>();
//  findproccxt = NULL;
//  commit      = FALSE;
//}

/* rootの子を全てtreeに格納する(withoutは除く) */
void HyperLink::get_children_without(std::vector<HyperLink *> &tree, HyperLink *without) const {
  for (auto it = hashset_iterator(this->children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
    auto *hl = (HyperLink *)hashsetiter_entry(&it);
    if (hl->children && hl->rank > 0) { /* hlが子を持つならば */
      hl->get_children_without(tree, without);
    }

    if (hl != without) {
      tree.emplace_back(hl);
    }
  }
}

/* start_hlと同じ集合に属するhyperlinkを全てVectorに格納して返す.
 * fidnproccxtで使用する関係上,
 * start_hlは探索対象外のHyperLinkであるため, treeの最後に追加する  */
void HyperLink::get_elements(std::vector<HyperLink *> &tree) {
  HyperLink *root = this->get_root();
  if (root->rank > 0)
    root->get_children_without(tree, this);
  if (root != this)
    tree.emplace_back(root);
  tree.emplace_back(this);
}

/* ハイパーリンクhlのハッシュ値を返す. */
unsigned long HyperLink::hash() { return this->element_num(); }
