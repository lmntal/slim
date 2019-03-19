/*
 * hyperlink.h
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

#ifndef LMN_HYPERLINK_H
#define LMN_HYPERLINK_H

/**
 * @ingroup VM
 * @defgroup Hyperlink
 * @{
 */

#include "lmntal.h"

struct HyperLink;

#include <limits>

#include "atom.h"
#include "element/element.h"
#include "functor.h"
#include "membrane.h"
#include "react_context.hpp"

/* ----------------------------------------------------------------------- *
 *  hyperlink system                                                       *
 * ----------------------------------------------------------------------- */

/* symbol atom'!'の第0引数目は接続元のシンボルアトム
 *                 第1引数目はhlink属性としてHyperLink構造体
 * が埋め込まれている */

/* HyperLink 構造体
 *   '!'をファンクタに持つシンボルアトムごとに生成される
 *   '!'アトムのポインタをIDとして利用しているが、出力（とuniqの履歴生成）の際には
 *   idの値を見かけ上のIDとして利用している
 */
typedef struct HyperLink {
  LmnSymbolAtomRef
      atom; /* 対応する'!'アトムのポインタ、atomが開放されているときはNULL */
  LmnHlinkRank rank;
  LmnMembraneRef mem; /* atom の所属膜（findatomで使用）*/
  unsigned long
      id; /* 集合を一意に識別するID (主に出力とuniqの履歴生成の際に使用) */
          //  long usrid;        /*
  //  ユーザがhyperlinkのidを決められるようにするための変数（未実装）*/
  LmnAtomRef
      attrAtom; /* ハイパーリンクの属性として扱うアトム rootにのみ持たせる */
  LmnLinkAttr attr; /* 属性アトムの属性(0なら属性はなし) rootにのみ持たせる */
  /* 木構造による併合関係の表現 */
  struct HyperLink *parent; /* root の場合は自身のポインタ */
  struct HashSet *children; /* 子表 */

} HyperLink;

#define LMN_HL_EMPTY_ATTR (0)

#define LMN_HL_FUNC LMN_EXCLAMATION_FUNCTOR

#define LMN_HL_RANK(HL) ((HL)->rank)
#define LMN_HL_MEM(HL) ((HL)->mem)
#define LMN_HL_ID(HL) ((HL)->id)
#define LMN_HL_ATTRATOM(HL) ((lmn_hyperlink_get_root(HL))->attrAtom)
#define LMN_HL_ATTRATOM_ATTR(HL) ((lmn_hyperlink_get_root(HL))->attr)
#define LMN_HL_HAS_ATTR(HL)                                                    \
  (LMN_HL_ATTRATOM_ATTR(lmn_hyperlink_get_root(HL)) ||                         \
   LMN_HL_ATTRATOM(lmn_hyperlink_get_root(HL)))

#define LMN_HL_ATOM_ROOT_HL(ATOM)                                              \
  lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl(ATOM))
#define LMN_HL_ATOM_ROOT_ATOM(ATOM)                                            \
  lmn_hyperlink_hl_to_at(lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl(ATOM)))

#define LMN_IS_HL(ATOM) (LMN_FUNC_IS_HL((ATOM)->get_functor()))
#define LMN_FUNC_IS_HL(FUNC) ((FUNC) == LMN_HL_FUNC)
#define LMN_ATTR_IS_HL(ATTR) ((ATTR) == LMN_HL_ATTR)

void lmn_hyperlink_make(LmnSymbolAtomRef at);
void lmn_hyperlink_put_attr(HyperLink *hl, LmnAtomRef attrAtom,
                            LmnLinkAttr attr);
void lmn_hyperlink_make_with_attr(LmnSymbolAtomRef at, LmnAtomRef attrAtom,
                                  LmnLinkAttr attr);
LmnSymbolAtomRef lmn_hyperlink_new();
LmnSymbolAtomRef lmn_hyperlink_new_with_attr(LmnAtomRef attrAtom,
                                             LmnLinkAttr attr);
void lmn_hyperlink_delete(LmnSymbolAtomRef at);
void lmn_hyperlink_copy(LmnSymbolAtomRef newatom, LmnSymbolAtomRef oriatom);
HyperLink *lmn_hyperlink_at_to_hl(LmnSymbolAtomRef at);
LmnSymbolAtomRef lmn_hyperlink_hl_to_at(HyperLink *hl);
HyperLink *lmn_hyperlink_get_root(HyperLink *hl);
HyperLink *hyperlink_unify(HyperLink *parent, HyperLink *child, LmnAtomRef,
                           LmnLinkAttr);
HyperLink *lmn_hyperlink_unify(HyperLink *hl1, HyperLink *hl2, LmnAtomRef,
                               LmnLinkAttr);
int lmn_hyperlink_rank(HyperLink *hl);
int lmn_hyperlink_element_num(HyperLink *hl);
BOOL lmn_hyperlink_eq_hl(HyperLink *hl1, HyperLink *hl2);
BOOL lmn_hyperlink_eq(LmnSymbolAtomRef atom1, LmnLinkAttr attr1,
                      LmnSymbolAtomRef atom2, LmnLinkAttr attr2);
void lmn_hyperlink_print(LmnMembraneRef gr);

/* ----------------------------------------------------------------------- *
 *  same proccess context (同名型付きプロセス文脈)                         *
 *  hyperlink の接続関係を利用したルールマッチング最適化                   *
 * ----------------------------------------------------------------------- */

/* 同名型付きプロセス文脈を持つ引数ごとに生成される */
struct ProcCxt {
  ProcCxt(int atomi, int arg, ProcCxt *original)
      : atomi(atomi), arg(arg), start(nullptr), original_(original) {}
  ProcCxt(int atomi, int arg)
      : atomi(atomi), arg(arg), start(nullptr), original_(nullptr) {}

  bool is_clone() const { return original_ && original_->atomi != atomi; }

  bool is_argument_of(LmnSymbolAtomRef atom, int i) {
    auto linked_attr = atom->get_attr(i);
    if (!LMN_ATTR_IS_HL(linked_attr))
      return false;

    auto linked_atom = (LmnSymbolAtomRef)atom->get_link(i);
    auto linked_hl = lmn_hyperlink_at_to_hl(linked_atom);

    if (original_ && !lmn_hyperlink_eq_hl(linked_hl, original_->start))
      return false;

    return true;
  }

  void match(LmnSymbolAtomRef atom, int i) {
    auto linked_atom = (LmnSymbolAtomRef)atom->get_link(i);
    auto linked_hl = lmn_hyperlink_at_to_hl(linked_atom);
    start = linked_hl;
  }

  ProcCxt *original() { return original_; }

  HyperLink *start;

private:
  int atomi;
  int arg;
  ProcCxt *original_;
};

/* 同名型付きプロセス文脈を持つアトムごとに生成される */
struct SameProcCxt {

  SameProcCxt(int length) : start_attr(0), proccxts(length) {}

  ~SameProcCxt() {
    for (auto pc : proccxts)
      delete pc;
  }

  void add_proccxt_if_absent(int atomi, int arg) {
    if (!proccxts[arg]) {
      proccxts[arg] = new ProcCxt(atomi, arg, nullptr);
    }
  }

  void add_proccxt_if_absent(int atomi, int arg, const SameProcCxt &spc,
                             int arg2) {
    if (!proccxts[arg]) {
      proccxts[arg] = new ProcCxt(atomi, arg, spc.proccxts[arg2]);
    }
  }

  // いい名前が見つかったら変える
  bool is_clone(int n) {
    for (int i = 0; i < n; i++) {
      auto pc = proccxts[i];
      if (pc && pc->is_clone()) { /* clone proccxtを持つ */
        return true;              /* hyperlinkからfindatomを行なう */
      }
    }

    return false;
  }

  /** 探索の始点となる引数を決定する
   * 候補が複数ある場合は、もっとも選択肢の少ない(element_numが小さい)hyperlinkがマッチする引数を探索の始点とする
   */
  HyperLink *start() {
    HyperLink *start_hl = NULL;
    int element_num = std::numeric_limits<int>::max();
    int start_arity = 0;

    /* バックトラックしてきた場合はspc->treeの中身は初期化されていないため、ここで初期化
     */
    this->tree.clear();

    for (int i = 0; i < proccxts.size(); i++) {
      auto pc = this->proccxts[i];

      if (!pc)
        continue;
      if (!pc->original())
        continue;

      auto hl = pc->original()->start;
      pc->start = hl;
      //      /* オリジナル側で探索始点のハイパーリンクが指定されていない
      //       *
      //       または探索始点のハイパーリンクの要素数が0（どちらも起こり得ないはず）*/
      //      if (!hl) return FALSE;
      //      if (!(element_num = lmn_hyperlink_element_num(hl))) return
      //      FALSE;

      int tmp_num = lmn_hyperlink_element_num(hl);
      if (element_num > tmp_num) {
        element_num = tmp_num;
        start_hl = hl;
        start_arity = i;
      }
    }

    this->start_attr = start_arity;
    return start_hl;
  }

  /* オリジナル側のatom が持つ全ての引数に対して以下の処理を行なう
   * a. 通常の引数
   *   何もしない
   * b. 同名型付きプロセス文脈を持つ引数
   *   clone側での探索の始点となるhyperlinkをspcに保持させる
   */

  /* clone側のatomが持つ全ての引数に対して以下の処理を行なう
   * a. 通常の引数
   *   何もしない
   * b. 同名型付きプロセス文脈を持つ引数
   *   b-1. original側の引数である場合
   *     clone側での探索の始点となるhyperlinkをspcに保持させる
   *   b-2. clone側の引数である場合
   *     original側で探索の始点として指定されていたhyperlinkに対応していることを確認する
   */

  bool is_consistent_with(LmnSymbolAtomRef atom) {
    for (int i = 0; i < proccxts.size(); i++) {
      if (proccxts[i] && !proccxts[i]->is_argument_of(atom, i))
        return false;
    }

    return true;
  }

  void match(LmnSymbolAtomRef atom) {
    for (int i = 0; i < proccxts.size(); i++)
      if (proccxts[i])
        proccxts[i]->match(atom, i);
  }

  std::vector<ProcCxt *> proccxts; /* 長さlength のProcCxt 配列 */

  /* findatom 内で使用される一時領域 */
  std::vector<HyperLink *> tree; /* HyperLink tree */
  LmnLinkAttr start_attr;
};

void lmn_sameproccxt_init(LmnReactCxtRef rc);
void lmn_sameproccxt_clear(LmnReactCxtRef rc);
void lmn_hyperlink_get_elements(Vector *tree, HyperLink *start_hl);

/* ハイパーリンクhlのハッシュ値を返す. */
unsigned long lmn_hyperlink_hash(HyperLink *hl);

/* @} */

#endif /* LMN_HYPERLINK_H */
