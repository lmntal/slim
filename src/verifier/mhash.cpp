/*
 * mhash.cpp
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
 * cf. devel/sample/khiroto/mhash.lmn
 *
 * 初期値によってはオーバーフローが生じてしまい、
 * 計算結果が計算順序に因ってしまうことがあるので
 * 注意すること
 */

#include "mhash.h"
#include "element/element.h"
#include "visitlog.h"
#ifdef PROFILE
#include "runtime_status.h"
#endif

/* #define MHASH_C 31 /\* 深さを深くした場合、31は小さすぎるかも *\/ */
#define MHASH_C (101)
#define MHASH_B (13)
#define MHASH_E (3)
#define MHASH_ADD_0 (0)
#define MHASH_MUL_0 (41)
#define MHASH_MEM_ADD_0 (3412)
#define MHASH_MEM_MUL_0 (3412)
#define MHASH_CALCULATING_MEM (1)
#define MHASH_TREE_D (2)

typedef unsigned long mhash_t;

static mhash_t mhash_sub(LmnMembraneRef mem, unsigned long tbl_size);
static inline mhash_t mhash_membrane(LmnMembraneRef mem,
                                     LmnMembraneRef calc_mem,
                                     ProcessTableRef ctx);

static int mhash_depth = MHASH_TREE_D;

void mhash_set_depth(int depth) {
  if (depth > 0)
    mhash_depth = depth;
}

mhash_t mhash(LmnMembraneRef mem) {
  return mhash_sub(mem, round2up(env_next_id()));
  // return mhash_sub(mem, 1024);
  // return 10;
}

static mhash_t mhash_sub(LmnMembraneRef mem, unsigned long tbl_size) {
  ProcessTableRef c = new ProcessTbl(tbl_size);
  mhash_t t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_start_timer(PROFILE_TIME__STATE_HASH_MEM);
#endif

  t = mhash_membrane(mem, NULL, c);
  delete c;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_finish_timer(PROFILE_TIME__STATE_HASH_MEM);
#endif
  return t;
}

static inline mhash_t molecule(LmnSymbolAtomRef atom, LmnMembraneRef calc_mem,
                               ProcessTableRef ctx);
static inline mhash_t memunit(LmnMembraneRef mem,
                              LmnSymbolAtomRef from_in_proxy,
                              LmnMembraneRef calc_mem, ProcessTableRef ctx,
                              int depth);
static mhash_t mhash_rulesets(const std::vector<LmnRuleSet *> &rulesets);

/* 膜memのハッシュ値を返す.
 * 計算の根となる膜をcalc_memとして渡す.  */
static inline mhash_t mhash_membrane(LmnMembraneRef mem,
                                     LmnMembraneRef calc_mem,
                                     ProcessTableRef ctx) {
  mhash_t t;

  if (mem == calc_mem) {
    /* 膜memを起点にしたトレースでmem自身に到達可能な場合の無限ループを防ぐため,
     * 計算中の膜calc_mem( ==mem )のハッシュ値が必要になる場合は定数を返す. */
    return MHASH_CALCULATING_MEM;
  } else if (proc_tbl_get_by_mem(ctx, mem, &t)) {
    /* memのハッシュ値を計算済みなら, それを返す */
    return (mhash_t)t;
  } else {
    mhash_t ret, hash_sum, hash_mul;

    /* initialize */
    hash_sum = MHASH_MEM_ADD_0;
    hash_mul = MHASH_MEM_MUL_0;

    { /** 1. atoms */
      AtomListEntryRef ent;
      LmnFunctor f;

      EACH_ATOMLIST_WITH_FUNC(
          mem, ent, f, ({
            LmnSymbolAtomRef atom;
            /* プロキシは除く */
            if (LMN_IS_PROXY_FUNCTOR(f) || LMN_IS_EX_FUNCTOR(f))
              continue;
            EACH_ATOM(atom, ent, ({
                        /* 各連結分子のハッシュ値の値を求める. */
                        if (!proc_tbl_get_by_atom(ctx, atom, NULL)) {
                          mhash_t tmp = molecule(atom, mem, ctx);
                          hash_sum += tmp;
                          hash_mul *= tmp;
                        }
                      }));
          }));
    }

    { /** 2. membranes */
      LmnMembraneRef child_mem;
      for (child_mem = mem->mem_child_head(); child_mem;
           child_mem = child_mem->mem_next()) {
        mhash_t tmp = mhash_membrane(child_mem, NULL, ctx);
        hash_sum += tmp;
        hash_mul *= tmp;
      }

      /* hiroto論文にない膜名の情報を追加. TODO: こんなんで大丈夫かな. */
      hash_sum += (mem->NAME_ID() + 1);
      hash_mul *= (mem->NAME_ID() + 1);
    }

    { /** 3. rulesets */
      mhash_t tmp;

      /* hiroto論文にないルールセット情報の加算処理.
       * ルールセットの個数/種類が異なるだけの同型な膜に対してハッシュ値が散らばるようになった.
       * TODO:
       *  例えばbenchmarksetの問題ではそんな構造があまりないので少し遅くなったが,
       *  ルールセットの束のハッシュ値は, 状態間で変化がほとんどない.
       * 使い回しできれば速くなる.*/

      tmp = mhash_rulesets(mem->get_rulesets());
      hash_sum += tmp;
      hash_mul *= tmp;
    }

    /* finalzie */
    ret = hash_sum ^ hash_mul;
    ctx->proc_tbl_put_mem(mem, ret);
    return ret;
  }
}

static inline void do_molecule(LmnAtomRef atom, LmnLinkAttr attr,
                               LmnMembraneRef calc_mem, ProcessTableRef ctx,
                               int i_parent, mhash_t *sum, mhash_t *mul);

/* 膜calc_memに所属しているアトムatomをrootにした連結分子のハッシュ値を返す. */
static inline mhash_t molecule(LmnSymbolAtomRef atom, LmnMembraneRef calc_mem,
                               ProcessTableRef ctx) {
  mhash_t sum, mul;

  sum = MHASH_ADD_0;
  mul = MHASH_MUL_0;
  do_molecule(atom, LMN_ATTR_MAKE_LINK(0), calc_mem, ctx, -1, &sum, &mul);

  return sum ^ mul;
}

static inline mhash_t mhash_unit(LmnAtomRef atom, LmnLinkAttr attr,
                                 LmnMembraneRef calc_mem, ProcessTableRef ctx,
                                 int depth);
static inline mhash_t mhash_data(LmnAtomRef atom, LmnLinkAttr attr);

/* アトムatomのハッシュ値を求め, 連結分子のハッシュ値sum, mulに掛け合わせる.
 * atomのリンク先アトムに対して再帰する. */
static inline void do_molecule(LmnAtomRef atom, LmnLinkAttr attr,
                               LmnMembraneRef calc_mem, ProcessTableRef ctx,
                               int i_parent, mhash_t *sum, mhash_t *mul) {
  mhash_t t;

  if (LMN_ATTR_IS_DATA(attr)) {
    /* データアトム(+ ハイパーリンク)の場合 */
    t = mhash_data(atom, attr);
    (*sum) += t;
    (*mul) *= t;
  } else if (((LmnSymbolAtomRef)atom)->get_functor() !=
                 LMN_IN_PROXY_FUNCTOR &&
             ctx->put_new_atom((LmnSymbolAtomRef)atom, 1)) {
    /* シンボルアトムの場合:
     *  (連結分子計算は膜の外部に出て行かないものとしているため,
     * proxyならば打切り)
     * 連結分子中の各アトムをルートにして深さDまでのTreeをハッシュ計算の単位とする.
     */
    t = mhash_unit(atom, attr, calc_mem, ctx, 0);
    (*sum) += t;
    (*mul) *= t;

    if (LMN_IS_SYMBOL_FUNCTOR(((LmnSymbolAtomRef)atom)->get_functor())) {
      const int arity = ((LmnSymbolAtomRef)atom)->get_arity();
      int i_arg;

      for (i_arg = 0; i_arg < arity; i_arg++) {
        LmnLinkAttr to_attr;
        if (i_arg == i_parent)
          continue;

        to_attr = ((LmnSymbolAtomRef)atom)->get_attr(i_arg);
        do_molecule(((LmnSymbolAtomRef)atom)->get_link(i_arg), to_attr,
                    calc_mem, ctx, LMN_ATTR_GET_VALUE(to_attr), sum, mul);
      }
    }
  }
}

static inline mhash_t mhash_symbol(LmnSymbolAtomRef atom);
static inline mhash_t mhash_data(LmnAtomRef atom, LmnLinkAttr attr);
static inline mhash_t memlink(LmnSymbolAtomRef in_proxy,
                              LmnMembraneRef calc_mem, ProcessTableRef ctx);

/* アトムatomを起点にした深さdepthからDまでのTree構造のハッシュ値を返す.
 * Treeの頂点は,
 *   1. Symbol Atom (except proxies)
 *   2. InSide Proxy Atom
 *   3. Data   Atom
 *   4. OutSide Proxy Atomによって到達した子膜
 * とする.
 * 2のように所属膜calc_memの親膜へリンクが抜ける場合や,
 * 3のようにデータアトムを訪問した場合は,
 * 深さDに到達していなくともatomを枝としたトレースを打ち切る.
 * 4のように子膜を頂点とする場合は, 子膜のInSideProxyアトムをトレースする.  */
static mhash_t mhash_unit(LmnAtomRef atom, LmnLinkAttr attr,
                          LmnMembraneRef calc_mem, ProcessTableRef ctx,
                          int depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    /* 3. データアトムの場合:
     *   slim内のデータアトムはatom以外に接続先アトムを持たない仕様にしているため,
     *   深さDに到達していようがいまいがトレースを打ち切る. */
    return mhash_data(atom, attr);
  } else if (((LmnSymbolAtomRef)atom)->get_functor() ==
             LMN_OUT_PROXY_FUNCTOR) {
    /* 4. OutSideProxyアトムの(子膜に入る)場合 */
    LmnSymbolAtomRef in_proxy =
        (LmnSymbolAtomRef)(((LmnSymbolAtomRef)(atom))->get_link(0));
    if (depth == mhash_depth) {
      /* 深さDに到達した場合
       *   子膜のハッシュ値とリンク接続値を掛け合わせた値を計算してトレースを打ち切る.
       */
      return memlink(in_proxy, calc_mem, ctx);
    } else {
      /* 深さDに到達していない場合:
       *   子膜全体のハッシュ値を返す. */
      return memunit(LMN_PROXY_GET_MEM(in_proxy), in_proxy, calc_mem, ctx,
                     depth);
    }
  } else if (((LmnSymbolAtomRef)atom)->get_functor() ==
             LMN_IN_PROXY_FUNCTOR) {
    /* 2. InSideProxyアトムの(親膜に抜ける)場合
     *   プロキシアトムのハッシュ値を返してトレースを打ち切る. */
    return mhash_symbol((LmnSymbolAtomRef)(atom));
  } else if (depth == mhash_depth) {
    /* 1. シンボルアトムの場合 (深さDに到達)
     *   シンボルアトムのハッシュ値に,
     * 接続先リンク番号を掛け合わせた値をハッシュ値として返す. */
    return mhash_symbol((LmnSymbolAtomRef)atom) *
           (LMN_ATTR_GET_VALUE(attr) + 1);
  } else {
    /* 1. シンボルアトムの場合 (深さDに未到達)
     *   atomのハッシュ値を掛け合わせ, 再帰的に深さDまでトレースする. */
    mhash_t hash;
    int i_arg;
    const int arity = ((LmnSymbolAtomRef)atom)->get_arity();
    const int i_from = (depth == 0) ? -1 : (int)LMN_ATTR_GET_VALUE(attr);

    hash = mhash_symbol((LmnSymbolAtomRef)atom);
    for (i_arg = 0; i_arg < arity; i_arg++) {
      if (i_arg == i_from)
        continue;

      /* TODO: ここでtに定数を掛けたほうがいいかも
       *       再帰的にCを掛けているので、係数が重なる危険性がある */
      hash = MHASH_C * hash +
             MHASH_E *
                 mhash_unit(((LmnSymbolAtomRef)atom)->get_link(i_arg),
                            ((LmnSymbolAtomRef)atom)->get_attr(i_arg),
                            calc_mem, ctx, depth + 1);
    }

    return hash;
  }
}

/* mhash_unit計算中に,
 * アトムfrom_in_proxyを経て訪問した子膜child_memのハッシュ値を返す.
 * 子膜全体を1つの頂点として計算するため, 深さDに至るまでのトレースをInSide
 * Proxyアトムから行う.
 * ----------------------------------- $calc_mem --------------+
 * ------ $child_mem -----------+                              |
 * ?--0--1-[$from_in_proxy]-0---|---0-[from_out_proxy]-1--...  |
 * -----------------------------+                              |
 * ------------------------------------------------------------+  */
static inline mhash_t memunit(LmnMembraneRef child_mem,
                              LmnSymbolAtomRef from_in_proxy,
                              LmnMembraneRef calc_mem, ProcessTableRef ctx,
                              int depth) {
  mhash_t hash, child_h;
  AtomListEntryRef insides;
  LmnSymbolAtomRef in_proxy, out_proxy;

  hash = 0;
  insides = child_mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);
  child_h = mhash_membrane(child_mem, calc_mem, ctx);

  LMN_ASSERT(insides);

  EACH_ATOM(in_proxy, insides, ({
              mhash_t tmp;
              if (in_proxy == from_in_proxy)
                continue;

              /* ------------------------------------ $calc_mem
               * -------------------+
               * ------------- $child_mem ----------+ | skip::
               * ?--0--1-[$from_in_proxy]-0--|--0-[from_out_proxy]-1--...   |
               * loop:: ?--0--1-[in_proxy]--0-------|--0-[out_proxy]-1---!trace
               * |
               * -----------------------------------+ |
               * ------------------------------------------------------------------+
               */

              out_proxy = (LmnSymbolAtomRef)(in_proxy->get_link(0));
              tmp = mhash_unit(out_proxy->get_link(1),
                               out_proxy->get_attr(1), calc_mem, ctx,
                               depth + 1);
              tmp *= child_h ^ memlink(in_proxy, calc_mem, ctx);
              hash += tmp;
            }));

  /* hiroto論文にないmhash_membrane(child_mem)の加算処理を追加 @taisuke */
  return hash + child_h;
}

/* アトムin_proxyをトレースした際のハッシュ値を返す.
 * 非プロキシアトムに遭遇するまで, 子膜をトレースしてゆき,
 * トレースした全ての子膜のハッシュ値を掛け合わせる.
 * 最終的に到達した非プロキシアトムのハッシュ値を掛け合わせた値を返す.
 * ------------------------- $calc_mem ------------------------+
 * ----------------child_mem----+                              |
 * atom--?---1--[$in_proxy]-0---|---0-[from_out_proxy]-1--...  |
 * -----------------------------+                              |
 * ------------------------------------------------------------+  */
static inline mhash_t memlink(LmnSymbolAtomRef in_proxy,
                              LmnMembraneRef calc_mem, ProcessTableRef ctx) {
  LmnAtomRef atom;
  mhash_t hash = 0;
  LmnLinkAttr attr;

  while (1) {
    hash = MHASH_B * hash +
           mhash_membrane(LMN_PROXY_GET_MEM(in_proxy), calc_mem, ctx);

    atom = in_proxy->get_link(1);
    attr = in_proxy->get_attr(1);
    if (LMN_ATTR_IS_DATA(attr) ||
        ((LmnSymbolAtomRef)atom)->get_functor() != LMN_OUT_PROXY_FUNCTOR)
      break;
    in_proxy = (LmnSymbolAtomRef)((LmnSymbolAtomRef)atom)->get_link(0);
  }

  if (LMN_ATTR_IS_DATA(attr)) {
    hash *= mhash_data(atom, attr);
  } else {
    hash *=
        mhash_symbol((LmnSymbolAtomRef)atom) * (LMN_ATTR_GET_VALUE(attr) + 1);
  }

  return hash;
}

/* 非データアトムatomのハッシュ値を返す. */
static inline mhash_t mhash_symbol(LmnSymbolAtomRef atom) {
  /* ファンクタの種類を示す整数IDを返す */
  return atom->get_functor();
}

/* データアトムatomのハッシュ値を返す. */
mhash_t mhash_data(LmnAtomRef atom, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    /* こっちの方が本当は好ましいけど遅いから, しょうがない. */
    return (mhash_t)lmn_byte_hash((unsigned char *)(&atom),
                                  sizeof(long) / sizeof(unsigned char));
    /* ここで値0が返るとハッシュ値の衝突数が爆発するため, 1を加算しておく.
     * TODO: 値がオーバーフローするとゼロになってしまう. */
    // return ((mhash_t)atom) + 1;
  case LMN_DBL_ATTR:
    /* double型8バイトをバイト列にキャストしてFNVハッシュ関数にかける. */
    return (mhash_t)lmn_byte_hash((unsigned char *)LMN_GETREF_DOUBLE(atom),
                                  sizeof(double) / sizeof(unsigned char));
  case LMN_SP_ATOM_ATTR:
    /* TODO: スペシャルアトムを定義する際にハッシュ値計算用関数も定義させる */
    if (lmn_is_string(atom, attr)) {
      return (mhash_t)reinterpret_cast<LmnString *>(atom)->hash();
    } else {
      LMN_ASSERT(FALSE); /* under constructions */
      return 1;
    }
  case LMN_HL_ATTR:
    return (mhash_t)(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->hash();
  default:
    LMN_ASSERT(FALSE);
    return 0;
  }
}

/* ルールセットのハッシュ値を返す.
 * 基本は, ルールセットIDを掛け合わせた値.
 * uniqルールセットの場合は, 更にuniqの適用履歴に一意な整数IDを掛け合わせる.
 * 即ち, 文字列とIDの対応テーブル(symbol table)の実装に依存している. */
static mhash_t mhash_rulesets(const std::vector<LmnRuleSet *> &rulesets) {
  mhash_t hash;
  int i, j;

  hash = 1;
  for (i = 0; i < rulesets.size(); i++) {
    LmnRuleSetRef rs = rulesets[i];
    hash *= rs->id;

    if (rs->has_unique()) {
      /* 履歴テーブルを持つ場合は, 履歴の整数IDも掛け合わせる.
       * この計算方法で十分かどうかはちゃんと考えていない. */
      for (auto r : *rs)
        for (auto entry : r->history())
          hash *= entry;
    }
  }

  return hash;
}
