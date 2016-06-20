/*
 * hyperlink.c
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
#include "membrane.h"
#include "react_context.h"

#if SIZEOF_LONG == 4
#  define EMPTY_KEY   0xffffffffUL
#  define DELETED_KEY 0xfffffffeUL
#elif SIZEOF_LONG == 8
#  define EMPTY_KEY   0xffffffffffffffffUL
#  define DELETED_KEY 0xfffffffffffffffeUL
#endif


/* ----------------------------------------------------------------------- *
 *  hyperlink system                                                       *
 * ----------------------------------------------------------------------- */


/*
 * 非決定実行への対応 :
 *   非決定実行に対応させる場合は並列化が必要
 *   - commit命令での作業配列のコピー操作に、newhlinkなどでのシンボルアトム生成を対応させる
 *   - グローバル変数hl_sameproccxt は実行中のルールごとに必要
 */

/* prototype */
void sht_print(SimpleHashtbl *sht);
void hs_print(HashSet *hs);
static inline void sameproccxt_destroy(SimpleHashtbl *sht);


static inline unsigned long hyperlink_new_id()
{
  return env_gen_next_id(); /* nd実行のために変更 */
}


//HyperLink *lmn_hyperlink_make(LmnSAtom sa)
void lmn_hyperlink_make(LmnSAtom at)
{
  HyperLink *hl;

  hl = LMN_MALLOC(HyperLink);
  hl->atom     = at;
  hl->rank     = 0;
  hl->mem      = NULL;
  hl->id       = hyperlink_new_id();
//  hl->usrid = 0;
  hl->parent   = hl;
  hl->children = NULL;
  hl->attrAtom = 0;
  hl->attr     = 0;

  LMN_SATOM_SET_LINK(LMN_SATOM(at), 1, (LmnWord)hl);
  LMN_SATOM_SET_ATTR(LMN_SATOM(at), 1, 0);

//  hashtbl_put(at_hl, (HashKeyType)at, (HashValueType)hl);
//  printf("lmn_hyperlink_make %p -> %p\n", hl, LMN_SATOM(hl->atom));
}

void lmn_hyperlink_put_attr(HyperLink *hl, LmnAtom attrAtom, LmnLinkAttr attr)
{
  hl->attrAtom = attrAtom;
  hl->attr = attr;
}

void lmn_hyperlink_make_with_attr(LmnSAtom at, LmnAtom attrAtom, LmnLinkAttr attr)
{
  lmn_hyperlink_make(at);
  lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(at), attrAtom, attr);
}

/* 新しいhyperlinkの生成 */
LmnSAtom lmn_hyperlink_new()
{
  LmnSAtom atom;

  atom = lmn_new_atom(LMN_HL_FUNC);
  LMN_SATOM_SET_ID(atom, hyperlink_new_id());
  lmn_hyperlink_make(atom);

  return atom;
}

LmnSAtom lmn_hyperlink_new_with_attr(LmnAtom attrAtom, LmnLinkAttr attr)
{
  LmnSAtom atom;
  atom = lmn_hyperlink_new();
  lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(atom), attrAtom, attr);
  return atom;
}

/* rootまでの全ての親のrankにdの値を加算する */
void hyperlink_rank_calc(HyperLink *hl, int d)
{
  HyperLink *parent, *current;

  hl->rank += d;
  current   = hl;
  parent    = hl->parent;
  while (parent != current) {
//    pro1++;
    parent->rank += d;
    current = parent;
    parent = parent->parent;
  }
}


/* HyperLinkのatom, mem, attrAtom, attrのみを交換する */
void hyperlink_swap_atom(HyperLink *hl1, HyperLink *hl2)
{
  LmnSAtom t_atom;
  LmnMembrane *t_mem;

  t_atom    = hl1->atom;
  hl1->atom = hl2->atom;
  hl2->atom = t_atom;

  LMN_SATOM_SET_LINK(hl1->atom, 1, (LmnWord)hl1);
  LMN_SATOM_SET_LINK(hl2->atom, 1, (LmnWord)hl2);

  t_mem     = hl1->mem;
  hl1->mem  = hl2->mem;
  hl2->mem  = t_mem;
}


/* 子表に格納されている子のうち先頭のものを返す、子表が無ければNULLを返す */
HyperLink *hyperlink_head_child(HyperLink *hl)
{
  HashSet *children = hl->children;
  if (children) {
    HashSetIterator it;

    for (it = hashset_iterator(children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
      HyperLink *child = (HyperLink *)hashsetiter_entry(&it);
      if ((HashKeyType)child < DELETED_KEY) {
        return child;
      }
    }
  }

  return NULL;
}


/*
 * atom の削除に呼応してHyperLink 構造体を削除する（最適化 ver.）
 *
 * 子をたくさん持つHyperLink を削除してしまうと, 削除後の木構造の再構築に時間がかかる（全ての子に対して親の更新を行なうなど）ため、
 * 削除するHyperLink は木構造の葉であることが望ましい
 *
 * HyperLink 木は変更せずに、HyperLink に対応しているatom を親子間での交換を繰り返して
 * 末端に向かって移動させ、葉に到達した時点でその位置のHyperLink を削除する
 */
void lmn_hyperlink_delete(LmnSAtom at)
{
  HyperLink *hl = lmn_hyperlink_at_to_hl(at);

  if (hl) {
    HyperLink *parent;

    while (hl->children) {
      HyperLink *child = hyperlink_head_child(hl);
      hyperlink_swap_atom(hl, child);
      hl = child;
    }

    parent = hl->parent;
    if (parent != hl) {
      hashset_delete(parent->children, (HashKeyType)hl);
      hyperlink_rank_calc(parent, -1);
      if (parent->rank == 0) {
        hashset_free(parent->children);
        parent->children = NULL;
      }
    }

    LMN_FREE(hl);
  }
}


/* atom の削除に呼応してHyperLink 構造体を削除する
 *
 * 旧式、使用していないが一応残しておく
 * hyperlinkの削除では何をしているかを把握するためにはいいかも
 */
void lmn_hyperlink_delete_old(LmnSAtom at)
{
  HyperLink *hl, *parent;
  HashSet *children;

  hl = lmn_hyperlink_at_to_hl(at);
  if (!hl) return;

  parent   = hl->parent;
  children = hl->children;

  if (parent != hl) {
    /* root でない場合：
     * 親の子表から自身を削除し, 親のrankを-1した後,
     *   子がいる   -> 親の子表に自身の子を移す
     *   子がいない -> そのまま削除
     */

    hashset_delete(parent->children, (HashKeyType)hl);
//    parent->rank--;
    hyperlink_rank_calc(parent, -1);
    if (parent->rank == 0) {
      hashset_free(parent->children);
      parent->children = NULL;
    }

    if (children) { // 子表があるとき
      HashSetIterator it;
      for (it = hashset_iterator(children);
           !hashsetiter_isend(&it);
           hashsetiter_next(&it)) {
        HyperLink *tmp = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)tmp < DELETED_KEY) {
          hashset_add(parent->children, (HashKeyType)tmp);
          hashset_delete(children, (HashKeyType)tmp);
          tmp->parent = parent;
        }
      }
    }
  }
  else {
    /* root である場合：
     *   子がいる   -> rankが最大の子を新rootにする
     *   子がいない -> そのまま削除
     */
    if (children) { /* 子表があるとき */
      HyperLink *newroot;
      HashSetIterator it;

      newroot = NULL;
      for (it = hashset_iterator(children);
           !hashsetiter_isend(&it);
           hashsetiter_next(&it)) {
        newroot = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)newroot < DELETED_KEY) {
          break; /* 現状では先頭の子を新しい親にしている */
        }
      }

      /* 新rootが決定 */
      hashset_delete(children, (HashKeyType)newroot); /* rootの子表から新rootを除去 */
      newroot->parent = newroot;

      if (!newroot->children) {
        newroot->children = hashset_make(hashset_num(children));
      }

      for (it = hashset_iterator(children);
           !hashsetiter_isend(&it);
           hashsetiter_next(&it)) {
        HyperLink *tmp = (HyperLink *)hashsetiter_entry(&it);
        if ((HashKeyType)tmp < DELETED_KEY) {
          hashset_add(newroot->children, (HashKeyType)tmp);
          hashset_delete(children, (HashKeyType)tmp);
          tmp->parent = newroot;
        }
      }

      newroot->rank = parent->rank - 1;
      if (newroot->rank == 0) {
        hashset_free(newroot->children);
        newroot->children = NULL;
      }
    }
  }

  if (children) {
    hashset_free(children);
  }
  LMN_FREE(hl);
}



/* hyperlinkのコピー
 * <=> 新しいHyperLink 構造体を生成し、newatomに対応づけた後、oriatomと併合する
 */
void lmn_hyperlink_copy(LmnSAtom newatom, LmnSAtom oriatom)
{
  HyperLink *newhl, *orihl;

  orihl = lmn_hyperlink_at_to_hl(oriatom);
  lmn_hyperlink_make(newatom);
  newhl = lmn_hyperlink_at_to_hl(newatom);

  lmn_hyperlink_unify(lmn_hyperlink_get_root(orihl), newhl, LMN_HL_ATTRATOM(orihl), LMN_HL_ATTRATOM_ATTR(orihl));
}

/* Union-Find algorithm の最適化 (Path Compression)
 *   あるHyperLink からlmn_hyperlink_get_root でroot まで辿ったとき、
 *   その経路上にある全てのHyperLink をroot の直接の子として再設定する
 */
void hyperlink_path_compression(HyperLink *root, Vector *children)
{
  int i, n;

  n = vec_num(children);
  for (i = 0; i < n; i++) {
    HyperLink *hl, *old_parent;

    hl = (HyperLink *)vec_get(children, i);
    old_parent = hl->parent;

    if (old_parent != root) {
      HashSet *old_parent_children;
      int j, sub_rank;

      /* 旧親に対する処理 */
      old_parent_children = old_parent->children;
      hashset_delete(old_parent_children, (HashKeyType)hl);
      sub_rank = hl->rank + 1;
      for (j = i + 1; j < n; j++) {
        ((HyperLink *)vec_get(children, j))->rank -= sub_rank;
      }

      if (hashset_num(old_parent_children) == 0) {
        hashset_free(old_parent_children);
        old_parent->children = NULL;
      }

      /* 新親(root)に対する処理 */
      hl->parent = root;
      hashset_add(root->children, (HashKeyType)hl);
    }
  }
}


/* root を返す */
HyperLink *lmn_hyperlink_get_root(HyperLink *hl)
{
  HyperLink *parent_hl, *current_hl;
  if (hl->parent == hl) return hl;
  current_hl = hl;
  parent_hl  = hl->parent;

  /* hlとrootの間に他のHyperLinkが無ければpath compressionは起こらない
   * ＝ 要素数が2以下であれば、path compressionは起こらない
   */
  if (lmn_hyperlink_element_num(parent_hl) <= 2) {
    while (parent_hl != current_hl) {
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }
  }
  else {
    Vector children;
    vec_init(&children, lmn_hyperlink_element_num(parent_hl));

    while (parent_hl != current_hl) {
      vec_push(&children, (LmnWord)current_hl);
      current_hl = parent_hl;
      parent_hl  = current_hl->parent;
    }

    if (!vec_is_empty(&children)) {
      hyperlink_path_compression(parent_hl, &children); /* parent_hlはrootになっている */
    }

    vec_destroy(&children);
  }

  return parent_hl;
}


/* child をparent の子として併合する（parent, childは共にroot）*/
HyperLink *hyperlink_unify(HyperLink *parent, HyperLink *child, LmnAtom attrAtom, LmnLinkAttr attr)
{
  child->parent = parent;
  if (!parent->children) {
    parent->children = hashset_make(2);
  }
  hashset_add(parent->children, (HashKeyType)child);
  parent->rank = parent->rank + child->rank + 1;
  parent->attrAtom = attrAtom;
  parent->attr = attr;
  child->attrAtom = 0;
  child->attr = 0;

  return parent;
}


/* 2 つのhyperlink を併合し、親となった方を返す
 *   rank のより大きい(子を多く持つ)方を親とする
 *   rankが等しい場合はhl1をhl2の親とする
 *   attrで指定された属性を併合後のハイパーリンクの属性とする。
 * */
HyperLink *lmn_hyperlink_unify(HyperLink *hl1, HyperLink *hl2, LmnAtom attrAtom,  LmnLinkAttr attr)
{
  HyperLink *root1, *root2, *result;
  int rank1, rank2;

  root1 = lmn_hyperlink_get_root(hl1);
  root2 = lmn_hyperlink_get_root(hl2);

  if (root1 == root2) return root1;

  rank1 = hl1->rank;
  rank2 = hl2->rank;
//  printf("rank %p %d %p %d\n", hl1, rank1, hl2, rank2);
  if (rank1 >= rank2) {
    result = hyperlink_unify(root1, root2, attrAtom, attr);
  } else {
    result = hyperlink_unify(root2, root1, attrAtom, attr);
  }

  return result;
}


/* '!'アトムのポインタ --> 対応するHyperLink 構造体のポインタ */
HyperLink *lmn_hyperlink_at_to_hl(LmnSAtom at)
{
  return (HyperLink *)LMN_SATOM_GET_LINK(at, 1);
}


/* HyperLink 構造体のポインタ --> 対応する'!'アトムのポインタ */
LmnSAtom lmn_hyperlink_hl_to_at(HyperLink *hl)
{
//  if (hl->atom) return hl->atom;
//  else return 0;
  return hl->atom;
}


/* rank を返す */
int lmn_hyperlink_rank(HyperLink *hl)
{
  return LMN_HL_RANK(lmn_hyperlink_get_root(hl));
}


/* hyperlink の要素数(rank + 1)を返す */
int lmn_hyperlink_element_num(HyperLink *hl)
{
  return (lmn_hyperlink_rank(hl) + 1);
}


/* hyperlink 同士の比較 */
BOOL lmn_hyperlink_eq_hl(HyperLink *hl1, HyperLink *hl2)
{
  return lmn_hyperlink_hl_to_at(lmn_hyperlink_get_root(hl1)) ==
           lmn_hyperlink_hl_to_at(lmn_hyperlink_get_root(hl2));
}


/* hyperlink 同士の比較（'!'アトムポインタから直接） */
BOOL lmn_hyperlink_eq(LmnSAtom atom1, LmnLinkAttr attr1, LmnSAtom atom2, LmnLinkAttr attr2)
{
  return LMN_ATTR_IS_HL(attr1) &&
         LMN_ATTR_IS_HL(attr2) &&
         lmn_hyperlink_eq_hl(lmn_hyperlink_at_to_hl(atom1),
                             lmn_hyperlink_at_to_hl(atom2));
}


/* hyperlink を1 つ出力
 *   hyperlink が1 つでも出力されるとTRUE を返す */
BOOL hyperlink_print(LmnMembrane *mem, BOOL *flag, int *group, int *element)
{
  AtomListEntry *atomlist;
  LmnMembrane *m;
  LmnSAtom atom;
  HyperLink *hl, *parent;
  HashSet *children;
  int WIDTH;
  BOOL result;
  FILE *f; /*  出力先は呼び出し側から指定させたい */

  f = stdout;
  result = FALSE;
  WIDTH  = 22;
  if ((atomlist = lmn_mem_get_atomlist(mem, LMN_HL_FUNC))) {
    EACH_ATOM(atom, atomlist, ({
      result = TRUE;

      if (!(*flag)) {
        fprintf(f, "%9s %9s %13s %5s %5s\n", "[hl_ID]", "[parent]", "[linked with]", "[num]", "[direct children ( inside info )]");
        (*flag) = TRUE;
      }
      hl = lmn_hyperlink_at_to_hl(atom);

      /* hl_ID */
//      fprintf(f, "%9lx", LMN_ATOM(atom));
      fprintf(f, "%9lx", LMN_HL_ID(lmn_hyperlink_at_to_hl(atom)));
//      fprintf(f, "%9lx", LMN_HL_ID(lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl(atom))));

      /* parent */
      if ((parent = hl->parent) != hl) {
//        fprintf(f, " %9lx", LMN_ATOM(parent->atom));
        fprintf(f, " %9lx", LMN_HL_ID(parent));
      } else {
        (*group)++;
        fprintf(f, " %9s", "root");
      }

      /* linked with */
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0)) && LMN_SATOM_GET_LINK(atom, 0)) {
        fprintf(f, " %13s", LMN_SATOM_STR(LMN_SATOM_GET_LINK(atom, 0)));
      } else {
        fprintf(f, " %13s", "---");
      }

      /* element num */
      fprintf(f, " %5d  ", lmn_hyperlink_element_num(hl));

      /* (direct children) */
      if ((children = hl->children)) {
        HashSetIterator hsit;
        HyperLink *ch_hl;
        int i, n, width;
        BOOL comma;

        width = 0;
        comma = FALSE;
        i = 1;
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
            fprintf(f, "%lx%n", LMN_HL_ID(ch_hl), &n);
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

  for (m = mem->child_head; m; m = m->next) {
    result = (hyperlink_print(m, flag, group, element) || result);
  }

  return result;
}



/* num >= 0 */
int hyperlink_print_get_place(int num) {
  int place, tmp;

  place = 1;
  tmp = num;
  while(tmp >= 10) {
    tmp = tmp / 10;
    place++;
  }

  return place;
}


/* グローバルルート膜から順に辿って、存在する全てのhyperlink を出力する */
void lmn_hyperlink_print(LmnMembrane *gr)
{
  FILE *f;
  int WIDTH, group, element, place_g, place_e;
  char tail_g[8], tail_e[14];
  BOOL flag;

  f = stdout;
  element = 0;
  group = 0;
  flag = FALSE;
  fprintf(f, "== HyperLink =============================================================%n\n", &WIDTH);
  if (!hyperlink_print(gr, &flag, &group, &element)) fprintf(f, "There is no hyperlink.\n");

  place_g = hyperlink_print_get_place(group);
  place_e = hyperlink_print_get_place(element);

  if (group < 2)   sprintf(tail_g, "group, ");
  else             sprintf(tail_g, "groups,");

  if (element < 2) sprintf(tail_e, "element =====");
  else             sprintf(tail_e, "elements ====");

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
void hyperlink_print_old()
{
//  BOOL item;
//
//  item = FALSE;
//  printf("== HyperLink =============================================================\n");
//  if (at_hl) {
//    HashIterator it;
//    HyperLink *hl, *parent;
//    LmnSAtom atom;
//    HashSet *children;
//
//    for (it = hashtbl_iterator(at_hl); !hashtbliter_isend(&it); hashtbliter_next(&it)) {
//      if ((HashValueType)(hl = (HyperLink *)(hashtbliter_entry(&it)->data)) < DELETED_KEY) {
//        if (!item) {
//          printf("%9s %9s %13s %5s %5s\n", "[hl_ID]", "[parent]", "[linked with]", "[num]", "[direct children ( inside info )]");
//          item = TRUE;
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
//        if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0)) && LMN_SATOM_GET_LINK(atom, 0))
//          printf(" %13s", LMN_SATOM_STR(LMN_SATOM_GET_LINK(atom, 0)));
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
//          for (hsit = hashset_iterator(children); !hashsetiter_isend(&hsit); hashsetiter_next(&hsit)) {
//            if ((HashKeyType)(ch_hl = (HyperLink *)hashsetiter_entry(&hsit)) < DELETED_KEY) {
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

void lmn_hyperlink_print_old()
{
  hyperlink_print_old();
}


/* for debug @seiji */
void sht_print(SimpleHashtbl *sht)
{
  if (!sht) {
    printf(">>>> NULL\n");
  } else {
    int n = hashtbl_num(sht);
    int i;
    printf(">>>> sht %p num %d cap %d\n", sht, n, sht->cap);
    for (i = 0; i < sht->cap; i++) {
      if (sht->tbl[i].key == EMPTY_KEY) printf("%3d: key: %p data: %p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data);
      else {
        if (sht->tbl[i].data < DELETED_KEY){
          printf("%3d: key: %p data: %p->%p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data, (void*)((HyperLink *)sht->tbl[i].data)->atom);
        } else {
          printf("%3d: key: %p data: %p\n", i, (void *)sht->tbl[i].key, (HyperLink *)sht->tbl[i].data);
        }
      }
    }
  }
}


/* for debug @seiji */
void hs_print(HashSet *hs)
{
  if (!hs) {
    printf(">>>> NULL\n");
  } else {
    int n = hashtbl_num(hs);
    int i;
    printf(">>>>  hs %p num %d cap %d\n", hs, n, hs->cap);
    for (i = 0; i < hs->cap; i++) {
//      if (sht->tbl[i].key == EMPTY_KEY) printf("%3d: %s\n", i, "empty");
      if (hs->tbl[i] < DELETED_KEY) printf("%3d: key: %p\n", i, (void *)hs->tbl[i]);
      else {
        if (hs->tbl[i] < DELETED_KEY){
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

//FindProcCxt *findproccxt;
//
//void lmn_sameproccxt_init2()
//{
//  findproccxt = LMN_MALLOC(FindProcCxt);
//  findproccxt = NULL;
//  commit      = FALSE;
//}

void lmn_sameproccxt_init(LmnReactCxt *rc)
{
  RC_SET_HLINK_SPC(rc, hashtbl_make(2));
}


static inline void sameproccxt_destroy(SimpleHashtbl *hl_sameproccxt)
{
  HashIterator it;

  if (!hl_sameproccxt) return;

  for (it = hashtbl_iterator(hl_sameproccxt);
       !hashtbliter_isend(&it);
       hashtbliter_next(&it)) {
    SameProcCxt *spc = (SameProcCxt *)(hashtbliter_entry(&it)->data);

    if (!spc) {
      continue;
    }
    else if (spc->proccxts) {
      Vector *tree;
      unsigned int i;

      for (i = 0; i < spc->length; i++) {
        ProcCxt *pc = (ProcCxt *)LMN_SPC_PC(spc, i);
        if (pc) LMN_FREE(pc);
      }

      tree = LMN_SPC_TREE(spc);
      if (tree) vec_free(tree);

      LMN_FREE(spc->proccxts);
    }

    LMN_FREE(spc);
  }

  hashtbl_free(hl_sameproccxt);
}



void lmn_sameproccxt_clear(LmnReactCxt *rc)
{
  sameproccxt_destroy(RC_HLINK_SPC(rc));
  RC_SET_HLINK_SPC(rc, NULL);
}


ProcCxt *lmn_sameproccxt_pc_make(int atomi, int arg, ProcCxt *original)
{
  ProcCxt *pc;

  pc = LMN_MALLOC(ProcCxt);
  pc->atomi    = atomi;
  pc->arg      = arg;
  pc->start    = NULL;
  pc->original = original;

  return pc;
}

SameProcCxt *lmn_sameproccxt_spc_make(int atomi, int length)
{
  SameProcCxt *spc;
  int i;
  spc = LMN_MALLOC(SameProcCxt);
  spc->atomi      = atomi;
  spc->length     = length;
  spc->tree       = NULL;
  spc->start_attr = 0;
  spc->proccxts   = LMN_NALLOC(void *, length);
  for (i = 0; i < length; i++)
    spc->proccxts[i] = NULL;

  return spc;
}

BOOL lmn_sameproccxt_from_clone(SameProcCxt *spc, int n)
{
  ProcCxt *pc;
  int i;

  for (i = 0; i < n; i++) {
    pc = (ProcCxt *)LMN_SPC_PC(spc, i);
    if (pc
        && !LMN_PC_IS_ORI(pc)
        && (LMN_PC_ATOMI(LMN_PC_ORI(pc)) != LMN_PC_ATOMI(pc))) { /* clone proccxtを持つ */
      return TRUE; /* hyperlinkからfindatomを行なう */
    }
  }

  return FALSE;
}

/* 探索の始点となる引数を決定する
 * 候補が複数ある場合は、もっとも選択肢の少ない(element_numが小さい)hyperlinkがマッチする引数を探索の始点とする
 */
HyperLink *lmn_sameproccxt_start(SameProcCxt *spc, int atom_arity)
{
  HyperLink *start_hl;
  int i, element_num, start_arity;

  start_hl = NULL;
  element_num = -1;
  start_arity = 0;

  /* バックトラックしてきた場合はspc->treeの中身は初期化されていないため、ここで初期化 */
  if (LMN_SPC_TREE(spc)) {
    vec_free(LMN_SPC_TREE(spc));
    LMN_SPC_TREE(spc) = NULL;
  }

  for (i = 0; i < atom_arity; i++) {
    ProcCxt *pc = (ProcCxt *)LMN_SPC_PC(spc, i);
    if (pc && !LMN_PC_IS_ORI(pc)) {
      HyperLink *hl;
      int tmp_num;

      hl = LMN_PC_START(LMN_PC_ORI(pc));
      LMN_PC_START(pc) = hl;
//      /* オリジナル側で探索始点のハイパーリンクが指定されていない
//       * または探索始点のハイパーリンクの要素数が0（どちらも起こり得ないはず）*/
//      if (!hl) return FALSE;
//      if (!(element_num = lmn_hyperlink_element_num(hl))) return FALSE;

      tmp_num = lmn_hyperlink_element_num(hl);
      if (element_num < 0 || element_num > tmp_num) {
        element_num = tmp_num;
        start_hl = hl;
        start_arity = i;
      }
    }
  }

  LMN_SPC_TREE(spc) = vec_make(element_num <= 0 ? 1 : element_num);
  LMN_SPC_SATTR(spc) = start_arity;
  return start_hl;
}

/* オリジナル側のatom が持つ全ての引数に対して以下の処理を行なう
 * a. 通常の引数
 *   何もしない
 * b. 同名型付きプロセス文脈を持つ引数
 *   clone側での探索の始点となるhyperlinkをspcに保持させる
 */
BOOL lmn_sameproccxt_all_pc_check_original(SameProcCxt *spc, LmnSAtom atom, int atom_arity)
{
  int i;
  BOOL all_pc_check;

  all_pc_check = TRUE;
  for (i = 0; i < atom_arity; i++) {
    ProcCxt *pc;
    LmnSAtom linked_atom;
    LmnLinkAttr linked_attr;

    pc = (ProcCxt *)LMN_SPC_PC(spc, i);
    if (!pc) continue; /* atom(spc)の第i引数が同名プロセス文脈 */

    linked_atom = LMN_SATOM(LMN_SATOM_GET_LINK(atom, i));
    linked_attr = LMN_SATOM_GET_ATTR(atom, i);

    if (!LMN_ATTR_IS_HL(linked_attr)) {
      all_pc_check = FALSE;
    }
    else if (!LMN_PC_IS_ORI(pc) &&
             !lmn_hyperlink_eq_hl(LMN_PC_START(LMN_PC_ORI(pc)),
                                  lmn_hyperlink_at_to_hl(linked_atom))) {
      /* atomの第i引数がハイパーリンク */
      all_pc_check = FALSE;
    }
    else {
      HyperLink *hl = lmn_hyperlink_at_to_hl(linked_atom);
      if (lmn_hyperlink_element_num(hl) <= 1) {
        all_pc_check = FALSE;
      }
      LMN_PC_START(pc) = lmn_hyperlink_at_to_hl(linked_atom);
    }
    if (!all_pc_check) break;
  }


  return all_pc_check;
}


/* clone側のatomが持つ全ての引数に対して以下の処理を行なう
 * a. 通常の引数
 *   何もしない
 * b. 同名型付きプロセス文脈を持つ引数
 *   b-1. original側の引数である場合
 *     clone側での探索の始点となるhyperlinkをspcに保持させる
 *   b-2. clone側の引数である場合
 *     original側で探索の始点として指定されていたhyperlinkに対応していることを確認する
 */
BOOL lmn_sameproccxt_all_pc_check_clone(SameProcCxt *spc, LmnSAtom atom, int atom_arity)
{
  int i;
  BOOL all_pc_check;

  all_pc_check = TRUE;
  for (i = 0; i < atom_arity; i++) {
    ProcCxt *pc;
    LmnSAtom linked_atom;
    LmnLinkAttr linked_attr;

    pc = (ProcCxt *)LMN_SPC_PC(spc, i);
    if (!pc) continue;

    linked_atom = LMN_SATOM(LMN_SATOM_GET_LINK(atom, i));
    linked_attr = LMN_SATOM_GET_ATTR(atom, i);

    if (!LMN_ATTR_IS_HL(linked_attr)) { /* atomの第i引数がハイパーリンク */
      all_pc_check = FALSE;
    }
    else {
      HyperLink *linked_hl = lmn_hyperlink_at_to_hl(linked_atom);

      if (LMN_PC_IS_ORI(pc)) { /* 第i引数がオリジナルであれば、クローン側での探索の始点を保持 */
        LMN_PC_START(pc) = linked_hl;
      } else if (!lmn_hyperlink_eq_hl(linked_hl, LMN_PC_START(pc))) { /* 第i引数がクローンであれば、それの成否を確かめる */
        all_pc_check = FALSE;
      }
    }

    if (!all_pc_check) break;
  }

  return all_pc_check;
}


/* rootの子を全てtreeに格納する(withoutは除く) */
void hyperlink_get_children_without(Vector *tree, HyperLink *root, HyperLink *without)
{
  HashSetIterator it;
  for (it = hashset_iterator(root->children); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
//    printf("%p\n", (void *)((HyperLink *)hashsetiter_entry(&it))->atom);
//    pro1++;
    HyperLink *hl = (HyperLink *)hashsetiter_entry(&it);
    if (hl->children && hl->rank > 0) { /* hlが子を持つならば */
      hyperlink_get_children_without(tree, hl, without);
    }

    if (hl != without) {
      vec_push(tree, (LmnWord)hl);
    }
  }
}


/* start_hlと同じ集合に属するhyperlinkを全てVectorに格納して返す.
 * fidnproccxtで使用する関係上,
 * start_hlは探索対象外のHyperLinkであるため, treeの最後に追加する  */
void lmn_hyperlink_get_elements(Vector *tree, HyperLink *start_hl)
{
  HyperLink *root = lmn_hyperlink_get_root(start_hl);
  if (root->rank > 0)   hyperlink_get_children_without(tree, root, start_hl);
  if (root != start_hl) vec_push(tree, (LmnWord)root);
  vec_push(tree, (LmnWord)start_hl);
}
