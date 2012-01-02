/*
 * mhash.c
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
 * cf. devel/sample/khiroto/mhash.lmn
 *
 * 初期値によってはオーバーフローが生じてしまい、
 * 計算結果が計算順序に因ってしまうことがあるので
 * 注意すること
 */


#include "mhash.h"
#include "atom.h"
#include "membrane.h"
#include "rule.h"
#include "functor.h"
#include "st.h"
#include "visitlog.h"
#include "slim_header/string.h"
#ifdef PROFILE
# include "runtime_status.h"
#endif


/* #define MHASH_C 31 /\* 深さを深くした場合、31は小さすぎるかも *\/ */
#define MHASH_C                  (101)
#define MHASH_B                   (13)
#define MHASH_E                    (3)
#define MHASH_ADD_0                (0)
#define MHASH_MUL_0               (41)
#define MHASH_MEM_ADD_0         (3412)
#define MHASH_MEM_MUL_0         (3412)
#define MHASH_CALCULATING_MEM      (1)
#define MHASH_TREE_D               (2)

typedef unsigned long mhash_t;

static mhash_t mhash_sub(LmnMembrane *mem, unsigned long tbl_size);
static inline mhash_t mhash_membrane(LmnMembrane *mem,
                                     LmnMembrane *calc_mem,
                                     ProcessTbl  ctx);

mhash_t mhash(LmnMembrane *mem)
{
  return mhash_sub(mem, round2up(env_next_id()));
  //return mhash_sub(mem, 1024);
  //return 10;
}

static mhash_t mhash_sub(LmnMembrane *mem, unsigned long tbl_size)
{
  struct ProcessTbl c;
  mhash_t t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)  profile_start_timer(PROFILE_TIME__STATE_HASH_MEM);
#endif

  proc_tbl_init_with_size((ProcessTbl)&c, tbl_size);
  t = mhash_membrane(mem, NULL, (ProcessTbl)&c);
  proc_tbl_destroy((ProcessTbl)&c);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) profile_finish_timer(PROFILE_TIME__STATE_HASH_MEM);
#endif
  return t;
}



static inline mhash_t molecule(LmnSAtom    atom,
                               LmnMembrane *calc_mem,
                               ProcessTbl  ctx);
static inline mhash_t memunit(LmnMembrane *mem,
                              LmnSAtom    from_in_proxy,
                              LmnMembrane *calc_mem,
                              ProcessTbl  ctx,
                              int         depth);
static mhash_t mhash_rulesets(Vector *rulesets);

/* 膜memのハッシュ値を返す.
 * 計算の根となる膜をcalc_memとして渡す.  */
static inline mhash_t mhash_membrane(LmnMembrane *mem,
                                     LmnMembrane *calc_mem,
                                     ProcessTbl  ctx)
{
  mhash_t t;

  if (mem == calc_mem) {
    /* 膜memを起点にしたトレースでmem自身に到達可能な場合の無限ループを防ぐため,
     * 計算中の膜calc_mem( ==mem )のハッシュ値が必要になる場合は定数を返す. */
    return MHASH_CALCULATING_MEM;
  }
  else if (proc_tbl_get_by_mem(ctx, mem, &t)) {
    return (mhash_t)t;
  }
  else {
    LmnMembrane   *child_mem;
    AtomListEntry *ent;
    mhash_t  hash_sum, hash_mul;
    LmnSAtom atom;
    mhash_t  hash, u;
    LmnFunctor f;

    hash_sum = MHASH_MEM_ADD_0;
    hash_mul = MHASH_MEM_MUL_0;

    /** 1. atoms
     */
    EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
      /* プロキシは除く */
      if (LMN_IS_PROXY_FUNCTOR(f)) continue;
      EACH_ATOM(atom, ent, ({
        /* 各連結分子のハッシュ値の値を求める. */
        if (!proc_tbl_get_by_atom(ctx, (LmnSAtom)atom, NULL) &&
            !LMN_IS_HL(atom)) {
          u = molecule(atom, mem, ctx);
          hash_sum += u;
          hash_mul *= u;
        }
      }));
    }));

    /** 2. membranes
     */
    for (child_mem = mem->child_head; child_mem; child_mem = child_mem->next) {
      mhash_t u = memunit(child_mem, NULL, mem, ctx, 0);
      hash_sum += u;
      hash_mul *= u;
    }

    /* 膜名の情報. TODO: とりあえず. こんなんで大丈夫かな. */
    hash_sum += (mem->name + 1);
    hash_mul *= (mem->name + 1);


    /** 3. rulesets
     * TODO:
     *  ルールセットの個数/種類が異なるだけの同型な膜に対してハッシュ値が散らばるようになったが,
     *  例えばbenchmarksetの問題ではそんな構造があまりないので少し遅くなった
     */
    t = mhash_rulesets(lmn_mem_get_rulesets(mem));
    hash_sum += t;
    hash_mul *= t;

    hash = hash_sum ^ hash_mul;
    proc_tbl_put_mem(ctx, mem, hash);
    return hash;
  }
}



static inline void do_molecule(LmnAtom     atom,
                               LmnLinkAttr attr,
                               LmnMembrane *calc_mem,
                               ProcessTbl  ctx,
                               int         i_parent,
                               mhash_t     *sum,
                               mhash_t     *mul);

/* 膜calc_memに所属しているアトムatomをrootにした連結分子のハッシュ値を返す. */
static inline mhash_t molecule(LmnSAtom    atom,
                               LmnMembrane *calc_mem,
                               ProcessTbl  ctx)
{
  mhash_t sum, mul;

  sum = MHASH_ADD_0;
  mul = MHASH_MUL_0;
  do_molecule(LMN_ATOM(atom),
              LMN_ATTR_MAKE_LINK(0),
              calc_mem,
              ctx,
              -1,
              &sum,
              &mul);

  return sum ^ mul;
}


static inline unsigned long mhash_unit(LmnAtom     atom,
                                       LmnLinkAttr attr,
                                       LmnMembrane *calc_mem,
                                       ProcessTbl  ctx,
                                       int         depth);
static inline unsigned long mhash_data(LmnAtom atom, LmnLinkAttr attr);

/* アトムatomのハッシュ値を求め, 連結分子のハッシュ値sum, mulに掛け合わせる.
 * atomのリンク先アトムに対して再帰する. */
static inline void do_molecule(LmnAtom     atom,
                               LmnLinkAttr attr,
                               LmnMembrane *calc_mem,
                               ProcessTbl  ctx,
                               int         i_parent,
                               mhash_t     *sum,
                               mhash_t     *mul)
{
  mhash_t t;

  if (LMN_ATTR_IS_DATA(attr)) {
    /* データアトムの場合 */
    t = mhash_data(atom, attr);
    (*sum) += t;
    (*mul) *= t;
  }
  else if (LMN_SATOM_GET_FUNCTOR(atom) != LMN_IN_PROXY_FUNCTOR &&
           proc_tbl_put_new_atom(ctx, (LmnSAtom)atom, 1)) {
    /* シンボルアトムの場合:
     *  (連結分子計算は膜の外部に出て行かないものとしているため, proxyならば打切り)
     * 連結分子中の各アトムをルートにして深さDまでのTreeをハッシュ計算の単位とする. */
    t = mhash_unit(atom, attr, calc_mem, ctx, 0);
    (*sum) += t;
    (*mul) *= t;

    if (LMN_IS_SYMBOL_FUNCTOR(LMN_SATOM_GET_FUNCTOR(atom)) &&
        !LMN_IS_HL(atom)) {
      const int arity = LMN_SATOM_GET_ARITY(atom);
      int i_arg;

      for (i_arg = 0; i_arg < arity; i_arg++) {
        if (i_arg != i_parent) {
          LmnLinkAttr to_attr = LMN_SATOM_GET_ATTR(atom, i_arg);
          do_molecule(LMN_SATOM_GET_LINK(atom, i_arg),
                      to_attr,
                      calc_mem,
                      ctx,
                      /* ↓ リンク先がシンボルアトムの場合にのみ意味のある値 */
                      LMN_ATTR_GET_VALUE(to_attr),
                      sum,
                      mul);
        }
      }
    }
  }
}


static inline mhash_t mhash_symbol(LmnSAtom atom);
static inline mhash_t mhash_data(LmnAtom atom, LmnLinkAttr attr);
static inline mhash_t memlink(LmnSAtom    in_proxy,
                              LmnMembrane *calc_mem,
                              ProcessTbl  ctx);

/* アトムatomを起点にした深さdepthからDまでのTree構造のハッシュ値を返す.
 * Treeの頂点は,
 *   1. Symbol Atom (except proxies)
 *   2. InSide Proxy Atom
 *   3. Data   Atom
 *   4. OutSide Proxy Atomによって到達した子膜
 * とする.
 * 2のように所属膜calc_memの親膜へリンクが抜ける場合や,
 * 3のようにデータアトムを訪問した場合は, 深さDに到達していなくともatomを枝としたトレースを打ち切る.
 * 4のように子膜を頂点とする場合は, 子膜のハッシュ値を掛け合わせた後にトレースを打ち切る */
static mhash_t mhash_unit(LmnAtom     atom,
                          LmnLinkAttr attr,
                          LmnMembrane *calc_mem,
                          ProcessTbl  ctx,
                          int         depth)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    /* 3. データアトムの場合:
     *   slim内のデータアトムはatom以外に接続先アトムを持たない仕様にしているため,
     *   深さDに到達していようがいまいがトレースを打ち切る. */
    return mhash_data(atom, attr);
  }
  else if (LMN_SATOM_GET_FUNCTOR(atom) == LMN_OUT_PROXY_FUNCTOR) {
    /* 4. OutSideProxyアトムの(子膜に入る)場合 */
    LmnSAtom in_proxy;
    if (depth == MHASH_TREE_D) {
      /* 深さDに到達した場合
       *  子膜のハッシュ値とリンク接続値を掛け合わせた値を計算してトレースを打ち切る  */
      return memlink(LMN_SATOM(LMN_SATOM_GET_LINK(LMN_SATOM(atom), 0)),
                     calc_mem,
                     ctx);
    } else {
      /* 深さDに到達していない場合:
       *   子膜全体のハッシュ値を返す. */
      in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
      return memunit(LMN_PROXY_GET_MEM(in_proxy),
                     LMN_SATOM(in_proxy),
                     calc_mem,
                     ctx,
                     depth);
    }
  }
  else if (LMN_SATOM_GET_FUNCTOR(atom) == LMN_IN_PROXY_FUNCTOR) {
    /* 2. InSideProxyアトムの(親膜に抜ける)場合
     *   プロキシアトムのハッシュ値を返してトレースを打ち切る. */
    return mhash_symbol(LMN_SATOM(atom));
  }
  else if (depth == MHASH_TREE_D) {
    /* 1. シンボルアトムの場合 (深さDに到達)
     *   シンボルアトムのハッシュ値に, 接続先リンク番号を掛け合わせた値をハッシュ値として返す. */
    return mhash_symbol((LmnSAtom)atom) * (LMN_ATTR_GET_VALUE(attr) + 1);
  }
  else {
    /* 1. シンボルアトムの場合 (深さDに未到達)
     *   atomのハッシュ値を掛け合わせ, 再帰的に深さDまでトレースする. */
    mhash_t hash;
    int i_arg;
    const int arity  = LMN_SATOM_GET_ARITY(atom);
    const int i_from = (depth == 0) ? -1 : (int)LMN_ATTR_GET_VALUE(attr);

    hash = mhash_symbol((LmnSAtom)atom);
    for (i_arg = 0; i_arg < arity; i_arg++) {
      if (i_arg == i_from) continue;

      /* TODO: ここでtに定数を掛けたほうがいいかも
       *       再帰的にCを掛けているので、係数が重なる危険性がある */
      hash = MHASH_C * hash + MHASH_E * mhash_unit(LMN_SATOM_GET_LINK(atom, i_arg),
                                                   LMN_SATOM_GET_ATTR(atom, i_arg),
                                                   calc_mem,
                                                   ctx,
                                                   depth + 1);
    }

    return hash;
  }
}


/* アトムfrom_in_proxyを経て訪問した子膜memのハッシュ値を返す.
 * from_in_proxyはNULLでも構わない */
static inline mhash_t memunit(LmnMembrane *mem,
                             LmnSAtom     from_in_proxy,
                             LmnMembrane  *calc_mem,
                             ProcessTbl   ctx,
                             int          depth)
{
  mhash_t hash;
  AtomListEntry *insides;

  hash    = 0;
  insides = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);

  if (insides) {
    LmnSAtom in, out;
    EACH_ATOM(in, insides, ({
      mhash_t u;
      if (in == from_in_proxy) continue;

      out   = LMN_SATOM(LMN_SATOM_GET_LINK(in, 0));
      u     = mhash_unit(LMN_SATOM_GET_LINK(out, 1),
                         LMN_SATOM_GET_ATTR(out, 1),
                         calc_mem,
                         ctx,
                         depth + 1);
      u    *= mhash_membrane(mem, calc_mem, ctx) ^ memlink(in, calc_mem, ctx);
      hash += u;
    }));
  }

  /* hiroto論文にないmhash_membrane(mem)の加算処理を追加 @taisuke */
  return hash + mhash_membrane(mem, calc_mem, ctx);
}


/* アトムin_proxyをトレースした際のハッシュ値を返す. */
static inline mhash_t memlink(LmnSAtom    in_proxy,
                              LmnMembrane *calc_mem,
                              ProcessTbl  ctx)
{
  LmnAtom atom;
  mhash_t hash = 0;
  LmnLinkAttr attr;

  while (1) {
    hash = MHASH_B * hash + mhash_membrane(LMN_PROXY_GET_MEM(in_proxy), calc_mem, ctx);

    atom = LMN_SATOM_GET_LINK(in_proxy, 1);
    attr = LMN_SATOM_GET_ATTR(in_proxy, 1);
    if (LMN_ATTR_IS_DATA(attr) ||
        LMN_SATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) break;
    in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
  }

  if (LMN_ATTR_IS_DATA(attr)) {
    hash *= mhash_data(atom, attr);
  } else {
    hash *= mhash_symbol((LmnSAtom)atom) * (LMN_ATTR_GET_VALUE(attr) + 1);
  }

  return hash;
}

/* 非データアトムatomのハッシュ値を返す. */
static inline mhash_t mhash_symbol(LmnSAtom atom)
{
  /* ファンクタの種類を示す整数IDを返す */
  return LMN_SATOM_GET_FUNCTOR(atom);
}


/* データアトムatomのハッシュ値を返す. */
static inline mhash_t mhash_data(LmnAtom atom, LmnLinkAttr attr) {
  switch (attr) {
    case LMN_INT_ATTR:
//      return (mhash_t)lmn_byte_hash((unsigned char *)(&atom), sizeof(long) / sizeof(unsigned char)); /* こっちのがいいんだけど〜遅いの */
      return ((mhash_t)atom) + 1;
    case LMN_DBL_ATTR:
      return (mhash_t)lmn_byte_hash((unsigned char *)atom,
                                    sizeof(double) / sizeof(unsigned char));
    case LMN_SP_ATOM_ATTR:
      if (lmn_is_string(atom, attr)) {
        return (mhash_t)lmn_string_hash(LMN_STRING(atom));
      } else {
        lmn_fatal("not implemented");
        return 0;
      }
    case LMN_HL_ATTR:
      return (mhash_t)lmn_hyperlink_hash(lmn_hyperlink_at_to_hl((LmnSAtom)atom));
    default:
      LMN_ASSERT(FALSE);
      return 0;
  }
}



static inline int mhash_multiply_rhistories_f(st_data_t _key,
                                              st_data_t _value,
                                              st_data_t _arg);

/* ルールセットのハッシュ値を返す.
 * 基本は, ルールセットIDを掛け合わせた値.
 * uniqルールセットの場合は, 更にuniqの適用履歴に一意な整数IDを掛け合わせる.
 * 即ち, 文字列とIDの対応テーブル(symbol table)の実装に依存している. */
static mhash_t mhash_rulesets(Vector *rulesets)
{
  mhash_t hash;
  int i, j;

  hash = 1;
  for (i = 0; i < vec_num(rulesets); i++) {
    LmnRuleSet rs = (LmnRuleSet)vec_get(rulesets, i);
    if (!lmn_ruleset_has_uniqrule(rs)) continue;

    for (j = 0; j < lmn_ruleset_rule_num(rs); j++) {
      st_table_t his_tbl = lmn_rule_get_history_tbl(lmn_ruleset_get_rule(rs, j));
      if (!his_tbl || st_num(his_tbl) == 0) continue;
      st_foreach(his_tbl, mhash_multiply_rhistories_f, (st_data_t)&hash);
    }

    hash *= lmn_ruleset_get_id(rs);
  }

  return hash;
}

/* 履歴表は, lmn_interned_idをkeyに, valueを0にしている */
static inline int mhash_multiply_rhistories_f(st_data_t _key,
                                              st_data_t _value,
                                              st_data_t _arg)
{
  unsigned long *u;
  lmn_interned_str id;

  u  = (unsigned long *)_arg;
  id = (lmn_interned_str)_key;
  (*u) *= id;

  return ST_CONTINUE;
}
