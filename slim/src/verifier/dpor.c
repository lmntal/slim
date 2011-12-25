/*
 * dpor.c
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
 * $Id:
 */

#include "dpor.h"
#include "dpor_naive.h"
#include "task.h"
#include "react_context.h"
#include "delta_membrane.h"
#include "state.h"
#include "mc.h"
#include "mc_worker.h"
#include "instruction.h"
#include "lmntal.h"
#include "atom.h"
#include "vector.h"

/**
 * Dynamic Partial Order Reduction
 *
 * 概要:
 *  en(s)を求める際のマッチング(LHS)と書換え(RHS)処理において,
 *  各遷移が依存するLMNtalプロセスのIDをLHSから収集し,
 *  RHSで操作するLMNtalプロセスから, 依存するLHSを発見することで,
 *  遷移間の独立性情報を展開していく.
 *
 * 注意:
 *   Delta-Membraneを使用した状態展開処理に依存している.
 *   通常の状態展開処理では, LHSの対象となるLMNtalプロセスのIDを収集することは可能.
 *   (BUT)
 *   RHSは, コピーした状態を書換えるため, LHSとの依存関係を調べることが難しい.
 *   これを実現するには,
 *     コピー前のプロセスIDをkeyに, 対応するコピー後のプロセスを管理するcopymapに加え,
 *     コピー後のプロセスIDをkeyに, 対応するコピー前のプロセスを管理するcopymapが必要.
 *   Delta-Membraneを使用するデメリットは,
 *     一部未実装の中間語命令があることで動かないプログラムがあること
 *   だけで, 通常より高速化が見込める.
 *   Delta-Membraneの使用が標準になっていくことを前提に, DPORアルゴリズムの実装を進める.
 */


McDporData **dpor_data; /* スレッド毎に独立してデータを持たせる */

Vector *reduced_stack = NULL; /* 削減したグラフを表示するために最後に状態展開を行うためのStack */

struct ContextC1 {
  BOOL         is_ample_cand;
  BOOL         is_on_path;
  MemDeltaRoot *d;              /* 本遷移を実現する階層グラフの差分オブジェクトへのポインタ */
  ProcessTbl LHS_procs;         /* プロセスのIDがkey, LHSフラグがvalue */
  ProcessTbl RHS_procs;         /* プロセスのIDがkey, RHSフラグがvalue */
  unsigned int id;
};

struct ContextC2 {
  LmnWord wt;
  LmnByte at, tt;
  unsigned int wt_size;
};


static ContextC1 contextC1_make(MemDeltaRoot *d, unsigned int id)
{
  ContextC1 c = LMN_MALLOC(struct ContextC1);
  c->d = d;
  c->LHS_procs = proc_tbl_make_with_size(32);
  c->RHS_procs = proc_tbl_make_with_size(32);
  c->is_on_path = FALSE;
  c->is_ample_cand = FALSE;
  c->id = id;
  return c;
}


static inline void contextC1_free(ContextC1 c)
{
  proc_tbl_free(c->LHS_procs);
  proc_tbl_free(c->RHS_procs);
  LMN_FREE(c);
}


static int contextC1_free_f(st_data_t _k, st_data_t _v, st_data_t _arg)
{
  contextC1_free((ContextC1)_v);
  return ST_CONTINUE;
}



static BOOL contextC1s_eq(ContextC1 a, ContextC1 b)
{
  return proc_tbl_eq(a->LHS_procs, b->LHS_procs) &&
         proc_tbl_eq(a->RHS_procs, b->RHS_procs);
}


/* 既出の遷移か否かを判定する.
 * 新規の場合はsrc, そうでない場合は既出のContextC1オブジェクトを返す. */
static ContextC1 contextC1_lookup(st_table_t dst_tbl, ContextC1 src)
{
  ContextC1 ret;
  Vector tmp;
  unsigned int i;

  /* TODO: O(n * m), n:出現した遷移数, m:遷移が持つプロセス数.
   *       遷移の等価性を検査するためのハッシュ関数が必要 */

  vec_init(&tmp, st_num(dst_tbl) + 1);
  st_get_entries_value(dst_tbl, &tmp);

  ret = src;
  for (i = 0; i < vec_num(&tmp); i++) {
    ContextC1 dst = (ContextC1)vec_get(&tmp, i);
    /*
    POR_DEBUG(printf("compare, id%u(Ln%lu,Rn%lu), id%u(Ln%lu,Rn%lu)\n",
        src->id, src->LHS_procs->n, src->RHS_procs->n,
        dst->id, dst->LHS_procs->n, dst->RHS_procs->n));
        */
    if (contextC1s_eq(src, dst)) {
      ret = dst;
      break;
    }
  }

  vec_destroy(&tmp);
  return ret;
}



/** プロセスIDが_kなアトム_vをLHSテーブルに追加する.
 *  ground_atomsで求めたアトムの集合に対して使用する */
static int contextC1_expand_gatoms_LHS_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  ContextC1 c;
//  LmnWord key;

  c = (ContextC1)_arg;
//  key = LMN_SATOM_ID((LmnSAtom)_v);

  if (!proc_tbl_get(c->LHS_procs, _k, NULL)) {
    proc_tbl_put(c->LHS_procs, _k, LHS_DEFAULT);
  }

  return 1;
}



/* 作業配列に含まれるTT_ATOM, TT_MEM属性のプロセスをLHSテーブルに登録する */
static void contextC1_expand_LHS(McDporData      *d,
                                 ContextC1       c,
                                 LmnReactCxt     *rc,
                                 LmnRegister     *v)
{
  unsigned int i;

  for (i = 0; i < warry_use_size(rc); i++) {
    LmnWord key, t;
    BYTE flag;

    if (v[i].tt == TT_ATOM && !LMN_ATTR_IS_DATA(v[i].at)) {
      key = LMN_SATOM_ID((LmnSAtom)v[i].wt);
    } else if (v[i].tt == TT_MEM) {
      key = lmn_mem_id((LmnMembrane *)v[i].wt);
      if (i == 0) {
        dpor_LHS_flag_add(d, key, LHS_MEM_GROOT);
      }
    }
    else {
      key = 0, t = 0, flag = 0;
      continue;
    }

    if (proc_tbl_get(d->wt_flags, key, &t)) {
      flag = (BYTE)t;
    } else {
      flag = LHS_DEFAULT;
    }

    proc_tbl_put(c->LHS_procs, key, (LmnWord)flag);
  }

  for (i = 0; i < vec_num(d->wt_gatoms); i++) {
    ProcessTbl g_atoms = (ProcessTbl)vec_get(d->wt_gatoms, i);
    proc_tbl_foreach(g_atoms, contextC1_expand_gatoms_LHS_f, (LmnWord)c);
  }
}


static inline void contextC1_RHS_tbl_put(ProcessTbl p, LmnWord key, BYTE set)
{
  LmnWord t;
  BYTE op;

  if (proc_tbl_get(p, key, &t)) {
    op = (BYTE)t;
  } else {
    op = OP_NONE;
  }

  RHS_OP_SET(op, set);
  proc_tbl_put(p, key, (LmnWord)op);
}


static inline void contextC1_RHS_tbl_unput(ProcessTbl p, LmnWord key, BYTE unset)
{
  LmnWord t;
  if (proc_tbl_get(p, key, &t)) {
    BYTE op = (BYTE)t;
    RHS_OP_UNSET(op, unset);
    proc_tbl_put(p, key, op);
  }
}


/* プロセスIDをkeyに, オペレーションフラグをvalueにしたテーブルを展開する */
static void contextC1_expand_RHS_inner(ContextC1 c, struct MemDelta *d)
{
  LmnMembrane *mem;
  unsigned int i;
  BOOL need_act_check;
  BOOL need_flink_check;

  mem = d->mem;
  need_act_check   = FALSE;
  need_flink_check = FALSE;

  /* アトムの数が変化した場合, memに対するnatoms命令が左辺に出現した遷移に依存する */
  if (vec_num(&d->new_atoms) > 0 || vec_num(&d->del_atoms) > 0 || d->data_atom_diff != 0) {
    int n = vec_num(&d->new_atoms) + d->data_atom_diff - vec_num(&d->del_atoms);
    if (lmn_mem_atom_num(mem) != n) {
      contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_NATOMS);
    }
    need_act_check = TRUE;
  }

  /* 子膜の数が変化した場合, memに対するnmems命令が左辺に出現した遷移に依存する */
  if (vec_num(&d->new_mems) > 0 || vec_num(&d->del_mems) > 0) {
    int n = vec_num(&d->new_mems) - vec_num(&d->del_mems);
    if (lmn_mem_child_mem_num(mem) != n) {
      contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_NMEMS);
    }
    need_act_check = TRUE;
  }

  if (!vec_is_empty(&d->new_proxies)) {
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_NFLINKS);
    need_act_check = TRUE;
  }

  /* ルールセットが消滅(clearrules)する場合, memが左辺に出現する遷移に依存する */
  if (d->ruleset_removed) {
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_EXISTS);
    need_act_check = TRUE;
  }

  /* 新たなルールセットが追加される場合, memに対するnorules命令が左辺に出現する遷移に依存する */
  if (d->new_rulesets) {
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_NORULES);
    need_act_check = TRUE;
  }

  /* アトムが削除されるなら, そのアトムが左辺に出現した遷移に依存する */
  for (i = 0; i < vec_num(&d->del_atoms); i++) {
    LmnSAtom a = (LmnSAtom)vec_get(&d->del_atoms, i);
    contextC1_RHS_tbl_put(c->RHS_procs, LMN_SATOM_ID(a), OP_DEP_EXISTS);
    if (LMN_SATOM_GET_FUNCTOR(a) == LMN_IN_PROXY_FUNCTOR) {
      need_flink_check = TRUE;
    }
  }

  /* inside proxyアトムが削除されていた場合 */
  if (need_flink_check) {
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_NFLINKS);
    need_act_check = TRUE;
  }


  /* 子膜が削除されるなら, その膜が左辺に出現した遷移に依存する */
  for (i = 0; i < vec_num(&d->del_mems); i++) {
    LmnMembrane *m = (LmnMembrane *)vec_get(&d->del_mems, i);
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(m), OP_DEP_EXISTS);
  }

  /* 膜名が変更された場合, その膜をanymem等で走査した遷移(つまりwt[0]以外のTT_MEM)に依存する */
  if (d->org_name != d->new_name) {
    contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_EXISTS_EX_GROOT);
    need_act_check = TRUE;
  }

  /* 膜になんらかの変化があった場合, 辿れる限りの所属膜に対するSTABLE判定に依存する */
  if (need_act_check) {
    while (mem) {
      contextC1_RHS_tbl_put(c->RHS_procs, lmn_mem_id(mem), OP_DEP_STABLE);
      mem = mem->parent;
    }
  }
}



static void contextC1_expand_RHS(McDporData *mc,
                                 ContextC1  c,
                                 MemDeltaRoot *d)
{
  unsigned int i;
  /* 修正の加えられる膜に対する操作 */
  for (i = 0; i < vec_num(&d->mem_deltas); i++) {
    contextC1_expand_RHS_inner(c, (struct MemDelta *)vec_get(&d->mem_deltas, i));
  }

  /* リンクの繋ぎ替えは考慮しなくてよいはず.
   * -> 繋ぎ替え元のアトムに依存するが,
   *    繋ぎ替え元のアトムは既にstruct MemDeltaのdel_atomsに含まれているので計算済み */
  /*
  for (i = 0; i < vec_num(&d->modified_atoms); i += 2) {
    LmnSAtom src, new;
    src = LMN_SATOM(vec_get(&d->modified_atoms, i));
    new = LMN_SATOM(vec_get(&d->modified_atoms, i+1));
  }
   */
}


static McDporData *dpor_data_make()
{
  McDporData *d = LMN_MALLOC(McDporData);
  d->wt_gatoms = vec_make(4);
  d->wt_flags = proc_tbl_make();
  d->ample_cand = vec_make(8);
  d->nxt_tr_id = 0;
  d->delta_tbl = st_init_ptrtable();
  d->free_deltas = vec_make(32);
  return d;
}


static void dpor_data_free(McDporData *d)
{
  unsigned int i;
  vec_free(d->wt_gatoms);
  proc_tbl_free(d->wt_flags);
  vec_free(d->ample_cand);
  st_foreach(d->delta_tbl, contextC1_free_f, 0);
  st_free_table(d->delta_tbl);
  for (i = 0; i < vec_num(d->free_deltas); i++) {
    MemDeltaRoot *delt = (MemDeltaRoot *)vec_get(d->free_deltas, i);
    dmem_root_free(delt);
  }
  vec_free(d->free_deltas);
  LMN_FREE(d);
}


static void dpor_data_clear(McDporData *d, LmnReactCxt *rc)
{
  vec_clear(d->wt_gatoms);
  proc_tbl_clear(d->wt_flags);
  vec_clear(d->ample_cand);
  st_foreach(d->delta_tbl, contextC1_free_f, 0);
  st_clear(d->delta_tbl);

  while (!vec_is_empty(d->free_deltas)) {
    MemDeltaRoot *delt = (MemDeltaRoot *)vec_pop(d->free_deltas);
    dmem_root_free(delt);
  }

  vec_clear(RC_MEM_DELTAS(rc));

  vec_clear(d->free_deltas);
  d->nxt_tr_id = 0;
}


void dpor_env_init(void)
{
  if (lmn_env.enable_por_old) {
    init_por_vars();
  }
  else {
    unsigned int i, n;
    n = lmn_env.core_num;
    dpor_data = LMN_NALLOC(McDporData *, n);
    for (i = 0; i < n; i++) {
      dpor_data[i] = dpor_data_make();
    }
  }
}


void dpor_env_destroy(void)
{
  if (lmn_env.enable_por_old) {
    free_por_vars();
  }
  else {
    unsigned int i, n;
    n = lmn_env.core_num;
    for (i = 0; i < n; i++) {
      dpor_data_free(dpor_data[i]);
    }

    LMN_FREE(dpor_data);
  }
}


/* LHSのマッチングフラグlhsとRHSの書換えフラグrhsを比較し,
 * 両者が依存関係にあるならばTRUEを返し, 独立関係にあるならばFALSEを返す. */
static inline BOOL dpor_LHS_RHS_are_depend(BYTE lhs, BYTE rhs)
{
  if (RHS_OP(rhs, OP_DEP_EXISTS)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_EXISTS_EX_GROOT) && !LHS_FL(lhs, LHS_MEM_GROOT)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_NATOMS) && LHS_FL(lhs, LHS_MEM_NATOMS)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_NMEMS) && LHS_FL(lhs, LHS_MEM_NMEMS)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_NFLINKS) && LHS_FL(lhs, LHS_MEM_NFLINKS)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_NORULES) && LHS_FL(lhs, LHS_MEM_NORULES)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_STABLE) && LHS_FL(lhs, LHS_MEM_STABLE)) {
    return TRUE;
  } else {
    return FALSE;
  }
}




/* 遷移を管理するオブジェクトsrcとdstが互いに依存関係にある場合に真を返す. */
static BOOL contextC1s_are_depend(ContextC1 src, ContextC1 dst)
{
  ProcessTbl rhs_tbl;
  ProcessTbl lhs_tbl;
  unsigned int i;

  if (src == dst) return FALSE;

  rhs_tbl = src->RHS_procs;
  lhs_tbl = dst->LHS_procs;

#ifdef TIME_OPT
  for (i = 0; i < rhs_tbl->size; i++) {
    if (rhs_tbl->tbl[i] != ULONG_MAX) {
      LmnWord t;
      if (proc_tbl_get(lhs_tbl, (LmnWord)i, &t)) {
        if (dpor_LHS_RHS_are_depend((BYTE)t, (BYTE)rhs_tbl->tbl[i])) {
          return TRUE;
        }
      }
    }
  }

#else
  lmn_fatal("unsupported");
#endif

  return FALSE;
}


/* srcに積まれたdeltamemから取得するContextC1の独立性を検査し依存遷移の集合をretに積む.
 * 依存遷移が存在する場合 or ContextC1がample_candに含まれるContextC1に依存している場合,
 * FALSEを返す */
static BOOL dpor_dependency_check(McDporData *d, Vector *src, Vector *ret)
{
  unsigned int i, j;
  BOOL need_ample_check, ok;

  if (vec_num(d->ample_cand) > 0) {
    need_ample_check = TRUE;
  } else {
    need_ample_check = FALSE;
  }

  ok = TRUE;

  /* i番目のRHSとj番目のLHSの依存性をチェックし, 依存する組合せをretへpush */
  for (i = 0; i < vec_num(src); i++) {
    ContextC1 r;
    MemDeltaRoot *delta_r;
    st_data_t t;

    delta_r = (MemDeltaRoot *)vec_get(src, i);
    if (st_lookup(d->delta_tbl, (st_data_t)delta_r, &t)) {
      r = (ContextC1)t;
    } else {
      lmn_fatal("implementation error");
    }

    for (j = 0; j < vec_num(src); j++) {
      ContextC1 l;
      MemDeltaRoot *delta_l;

      if (j == i) continue;
      delta_l = (MemDeltaRoot *)vec_get(src, j);
      if (st_lookup(d->delta_tbl, (st_data_t)delta_l, &t)) {
        l = (ContextC1)t;
      } else {
        lmn_fatal("implementation error");
      }

      if (contextC1s_are_depend(r, l)) {
        if (ret) {
          if (!vec_contains(ret, (vec_data_t)r)) {
            vec_push(ret, (vec_data_t)r);
          }
          if (!vec_contains(ret, (vec_data_t)l)) {
            vec_push(ret, (vec_data_t)l);
          }
        }

        ok = FALSE;
      }
    }

    if (need_ample_check && !r->is_ample_cand) {
      for (j = 0; j < vec_num(d->ample_cand); j++) {
        if (contextC1s_are_depend(r, (ContextC1)vec_get(d->ample_cand, j))) {
          ok = FALSE;
        }
      }
    }
  }

  return ok;
}


static inline BOOL dpor_explored_cycle(McDporData *mc,
                                       ContextC1  c,
                                       LmnReactCxt *rc)
{
  st_data_t t;
  MemDeltaRoot *d;
  unsigned int i;

  for (i = 0; i < vec_num(RC_MEM_DELTAS(rc)); i++) {
    d = (MemDeltaRoot *)vec_get(RC_MEM_DELTAS(rc), i);
    if (st_lookup(mc->delta_tbl, (st_data_t)d, &t)) {
      ContextC1 succ = (ContextC1)t;
      if (succ->is_on_path) {
        return TRUE;
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  return FALSE;
}


static BOOL dpor_explore_subgraph(McDporData *mc,
                                  ContextC1  c,
                                  Vector     *cur_checked_ids)
{
  LmnMembrane *cur;
  LmnReactCxt rc;
  Vector nxt_checked_ids;
  unsigned int i;
  BOOL ret;

  cur = DMEM_ROOT_MEM(c->d);
  mc_react_cxt_init(&rc);
  vec_init(&nxt_checked_ids, 4);
  RC_SET_GROOT_MEM(&rc, cur);

  POR_DEBUG({
    printf("explore rec%u, delta_reacted_id=%u\n", mc->cur_depth, c->id);
    lmn_dump_mem_stdout(cur);
  });
  mc_expand_inner(&rc, cur);

  ret = TRUE;

  if (dpor_explored_cycle(mc, c, &rc)) {
    POR_DEBUG(printf("detected cycle\n"));
    vec_destroy(&nxt_checked_ids);
    mc_react_cxt_destroy(&rc);
    return ret;
  }

  if (dpor_dependency_check(mc, RC_MEM_DELTAS(&rc), NULL) && mc->cur_depth < 200) {

    for (i = 0; i < vec_num(RC_MEM_DELTAS(&rc)); i++) {
      MemDeltaRoot *succ_d;
      st_data_t t;

      if (ret == FALSE) break;

      /* succ_dを生成した際に, 等価な遷移と置換されている */
      succ_d = (MemDeltaRoot *)vec_get(RC_MEM_DELTAS(&rc), i);
      if (st_lookup(mc->delta_tbl, (st_data_t)succ_d, &t)) {
        ContextC1 succ_c = (ContextC1)t;

        if (vec_contains(cur_checked_ids, (vec_data_t)succ_c->id)) {
          /* 1step前で既に訪問済み
           * つまり、合流しているため、skip */
          POR_DEBUG(printf("Tr%u, contains\n", succ_c->id));
          continue;
        }


        if (succ_c->is_ample_cand) { /* ample候補は辿らなくてok */
          continue;
        } else {
          mc->cur_depth++;
          succ_c->is_on_path = TRUE;
          dmem_root_commit(succ_c->d);
          ret = dpor_explore_subgraph(mc, succ_c, &nxt_checked_ids);
          dmem_root_revert(succ_c->d);
          vec_push(&nxt_checked_ids, succ_c->id); /* next stepに合流性の情報を渡す */
          succ_c->is_on_path = FALSE;
          mc->cur_depth--;
        }
      } else {
        lmn_fatal("implementation error");
      }
    }
  } else {
    POR_DEBUG(printf("depends on ample cand!\n"));
    ret = FALSE;
  }

  vec_destroy(&nxt_checked_ids);
  mc_react_cxt_destroy(&rc);

  return ret;
}


static BOOL dpor_satisfied_C1(McDporData *d, LmnReactCxt *rc, Vector *working_set)
{
  Vector checked_ids;
  unsigned int i;
  BOOL ret;

  vec_init(&checked_ids, 16);

  ret = TRUE;
  POR_DEBUG({
    unsigned int _i;
    for (_i = 0; _i < vec_num(d->ample_cand); _i++) {
      ContextC1 _c_ = (ContextC1)vec_get(d->ample_cand, _i);
      printf("AMP[id=%u, delta=%lu]\n", _c_->id, (LmnWord)_c_->d);
    }
  });
  for (i = 0; i < vec_num(working_set); i++) {
    ContextC1 c = (ContextC1)vec_get(working_set, i);
    if (!c->is_ample_cand) {
      d->cur_depth = 0;
      c->is_on_path = TRUE;
      dmem_root_commit(c->d);
      ret = dpor_explore_subgraph(d, c, &checked_ids);
      vec_push(&checked_ids, c->id);
      dmem_root_revert(c->d);
      c->is_on_path = FALSE;
      if (!ret) break;
    }
  }

  vec_destroy(&checked_ids);
  return ret;
}



void dpor_transition_gen_LHS(McDporData   *mc,
                             MemDeltaRoot *d,
                             LmnReactCxt  *rc,
                             LmnRegister  *v)
{
  ContextC1 c;

  c = contextC1_make(d, mc->nxt_tr_id++);
  contextC1_expand_LHS(mc, c, rc, v);
  mc->tmp = c;
}


BOOL dpor_transition_gen_RHS(McDporData   *mc,
                             MemDeltaRoot *d,
                             LmnReactCxt  *rc,
                             LmnRegister  *v)
{
  ContextC1 c, ret;

  c = mc->tmp;
  mc->tmp = NULL;

  contextC1_expand_RHS(mc, c, d);

  ret = contextC1_lookup(mc->delta_tbl, c);
  POR_DEBUG({
    printf("gen trans id=%u\n", c->id);
    dpor_contextC1_dump_eachL(c);
    dpor_contextC1_dump_eachR(c);
  });

  if (ret != c && !vec_contains(RC_MEM_DELTAS(rc), (vec_data_t)ret->d)) {
    POR_DEBUG({
      printf("detected same trans id=%u\n", ret->id);
      printf("aborted trans_id=%u\n", c->id);
      dpor_contextC1_dump_eachL(ret);
      dpor_contextC1_dump_eachR(ret);
    });

    if (!ret->is_ample_cand) {
      if (!vec_contains(RC_MEM_DELTAS(rc), (vec_data_t)ret->d)) {
        POR_DEBUG(printf("push succ\n\n"));
        mc_react_cxt_add_mem_delta(rc, ret->d, NULL);
      }
    }

    contextC1_free(c);
    return FALSE;
  }
  else {
    st_add_direct(mc->delta_tbl, (st_data_t)d, (st_data_t)c);
    vec_push(mc->free_deltas, (vec_data_t)d);
    return TRUE;
  }
}


/* C3(Cycle Ignoring Problem)の検査を行うする.
 * srcから生成した状態gen_succへの遷移が閉路形成を行うものでなければTRUEを,
 * 閉路形成を行う場合(もしくは行う恐れがある場合)はFALSEを返す.
 * ins_succにはstatespace_insert手続きが返す状態を渡す */
static BOOL dpor_check_cycle_proviso(StateSpace ss,
                                     State      *src,
                                     State      *gen_succ,
                                     State      *ins_succ)
{
  if (gen_succ == ins_succ) {
    /* General Visited Proviso:
     *  既存状態への再訪問でない(新規状態への遷移)なら閉路形成を行う遷移ではない.
     *  Hash-Based分割と併用するとサクセッサの情報を取得するための通信で遅くなる. */
    return TRUE;
  }
  else if (ins_succ == src) {
    /* self-loop detection */
    return FALSE;
  }
  else if (is_on_stack(ins_succ)) {
    /* Stack Proviso:
     *  Stack上の状態に戻るということは閉路であるということ
     *  DFS Stackによる空間構築(逐次)が前提 */
    return FALSE;
  }
  else if (lmn_env.bfs && is_expanded(ins_succ) && lmn_env.core_num == 1) {
    /* Open Set Proviso:
     *  閉路形成を行なう遷移は,
     *  展開済み状態へ再訪問する遷移のサブセットである.(逐次限定) */
    return FALSE;
  }

  return TRUE;
}



/* TODO: 時間なくて雑.. 直す */
static void dpor_ample_set_to_succ_tbl(StateSpace   ss,
                                       Vector       *ample_set,
                                       Vector       *contextC1_set,
                                       LmnReactCxt  *rc,
                                       State        *s,
                                       Vector       *new_ss,
                                       BOOL         f)
{
  st_table_t succ_tbl;         /* サクセッサのポインタの重複検査に使用 */
  unsigned int i, succ_i;
  BOOL satisfied_C3;

  succ_i       = 0;
  succ_tbl     = RC_SUCC_TBL(rc);

  for (i = 0; i < vec_num(RC_EXPANDED(rc)); i++) { /* ごめんなさいort */
    State *succ_s;
    if (has_trans_obj(s)) {
      Transition succ_t = (Transition)vec_get(RC_EXPANDED(rc), i);
      succ_s = transition_next_state(succ_t);
      transition_free(succ_t);
    } else {
      succ_s = (State *)vec_get(RC_EXPANDED(rc), i);
    }
    state_free(succ_s);
  }

  satisfied_C3 = TRUE;
  for (i = 0; i < vec_num(ample_set); i++) {
    Transition src_t;
    st_data_t tmp;
    State *src_succ, *succ;
    MemDeltaRoot *succ_d;
    ContextC1 succ_c;

    succ_c = (ContextC1)vec_get(ample_set, i);
    succ_d = succ_c->d;
    src_succ = state_make_minimal();
    state_set_parent(src_succ, s);
    state_set_property_state(src_succ, DEFAULT_STATE_ID);
    if (has_trans_obj(s)) {
      src_t = transition_make(src_succ, lmn_intern("ample set"));
    } else {
      src_t = NULL;
    }

    succ = statespace_insert_delta(ss, src_succ, succ_d);
    if (succ == src_succ) {
      state_id_issue(succ);
      if (mc_is_dump(f))  dump_state_data(succ, (LmnWord)stdout, ss);
      if (new_ss) {
        vec_push(new_ss, (vec_data_t)succ);
      }
    } else {
      state_free(src_succ);
      if (has_trans_obj(s)) {
        transition_set_state(src_t, succ);
      } else {
        vec_set(RC_EXPANDED(rc), i, (vec_data_t)succ);
      }
    }

    if (mc_has_property(f)) {
      satisfied_C3 = dpor_check_cycle_proviso(ss, s, src_succ, succ);
    }

    tmp = 0;
    if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) {
      st_data_t ins = has_trans_obj(s) ? (st_data_t)src_t : (st_data_t)succ;
      st_add_direct(succ_tbl, (st_data_t)succ, ins);
      vec_set(RC_EXPANDED(rc), succ_i++, ins);
    } else {
      if (has_trans_obj(s)) {
        transition_free(src_t);
      }
    }
  }

  if (!satisfied_C3) {
    /* ample set以外の遷移も展開する */
    for (i = 0; i < vec_num(contextC1_set); i++) {
      Transition src_t;
      st_data_t tmp;
      State *src_succ, *succ;
      MemDeltaRoot *succ_d;
      ContextC1 succ_c;

      succ_c = (ContextC1)vec_get(contextC1_set, i);

      if (succ_c->is_ample_cand) continue; /* さっき登録した */

      succ_d = succ_c->d;
      src_succ = state_make_minimal();
      state_set_parent(src_succ, s);
      state_set_property_state(src_succ, DEFAULT_STATE_ID);
      if (has_trans_obj(s)) {
        src_t = transition_make(src_succ, ANONYMOUS);
      } else {
      src_t = NULL;
      }

      succ = statespace_insert_delta(ss, src_succ, succ_d);
      if (succ == src_succ) {
        state_id_issue(succ);
        if (mc_is_dump(f))  dump_state_data(succ, (LmnWord)stdout, ss);
        if (new_ss) {
          vec_push(new_ss, (vec_data_t)succ);
        }
      } else {
        state_free(src_succ);
        if (has_trans_obj(s)) {
          transition_set_state(src_t, succ);
        } else {
          vec_set(RC_EXPANDED(rc), i, (vec_data_t)succ);
        }
      }

      tmp = 0;
      if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) {
        st_data_t ins = has_trans_obj(s) ? (st_data_t)src_t : (st_data_t)succ;
        st_add_direct(succ_tbl, (st_data_t)succ, ins);
        vec_set(RC_EXPANDED(rc), succ_i++, ins);
      } else {
        if (has_trans_obj(s)) {
          transition_free(src_t);
        }
      }
    }
  }
#ifdef DEBUG
  else if (lmn_env.show_reduced_graph) {
    if (!reduced_stack) {
      reduced_stack = vec_make(512);
    }
    for (i = 0; i < vec_num(contextC1_set); i++) {
      State *src_succ;
      MemDeltaRoot *succ_d;
      ContextC1 succ_c;
      Transition src_t;

      succ_c = (ContextC1)vec_get(contextC1_set, i);

      if (succ_c->is_ample_cand) continue; /* さっき登録した */

      succ_d = succ_c->d;
      src_succ = state_make_minimal();
      state_set_parent(src_succ, s);
      state_set_property_state(src_succ, DEFAULT_STATE_ID);
      src_t = transition_make(src_succ, lmn_intern("reduced"));

      dmem_root_commit(succ_d); /* src_succに対応したグラフ構造へ */
      state_set_mem(src_succ, DMEM_ROOT_MEM(succ_d));
      state_calc_hash(src_succ, state_mem(src_succ), statespace_use_memenc(ss)); /* それを元にハッシュ値やmem_idを計算 */
      if (!is_encoded(src_succ)) {
        state_set_binstr(src_succ, state_calc_mem_dump(src_succ));
      }
      vec_push(reduced_stack, (vec_data_t)src_t);
      dmem_root_revert(succ_d); /* 元に戻す */
    }
  }
#endif

  RC_EXPANDED(rc)->num = succ_i;
  state_succ_set(s, RC_EXPANDED(rc)); /* successorを登録 */
  st_clear(RC_SUCC_TBL(rc));
}


//#define DEP_DEVEL

void dpor_start(StateSpace ss, State *s, LmnReactCxt *rc, Vector *new_s, BOOL flag)
{
  McDporData *d = RC_POR_DATA(rc);

  if (RC_MC_USE_DPOR_NAIVE(rc)) {
    por_calc_ampleset(ss, s, rc, new_s, flag);
    return;
  }
  else if (mc_react_cxt_succ_num_org(rc) <= 1 || !RC_MC_USE_DMEM(rc)) {
    mc_store_successors(ss, s, rc, new_s, flag);
  }
  else {
    Vector v_key, v_val;

    vec_init(&v_key, 32);
    vec_init(&v_val, 32);
    st_get_entries_key(d->delta_tbl, &v_key);
    st_get_entries_value(d->delta_tbl, &v_val);
    dpor_dependency_check(d, &v_key, d->ample_cand);

    POR_DEBUG({
      printf("\n** check ContextC1 table **\n");
      dpor_contextC1_dump(d);
      dpor_dependency_tbl_dump(d);
    });

#ifdef DEBUG
  if (!lmn_env.debug_por_dep) {
#endif
    if (vec_num(d->ample_cand) == 0) {
      ContextC1 c;
      st_data_t t;
      if (st_lookup(d->delta_tbl, (st_data_t)vec_get(RC_MEM_DELTAS(rc), 0), &t)) {
        c = (ContextC1)t;
        c->is_ample_cand = TRUE; /* だいじ */
      } else {
        lmn_fatal("unexpected");
      }
      vec_push(d->ample_cand, (vec_data_t)c);
    }

    if (vec_num(d->ample_cand) == mc_react_cxt_succ_num_org(rc)) {
      POR_DEBUG(printf("@@ ample cand == succ num\n"));
      mc_store_successors(ss, s, rc, new_s, flag);
    }
    else {

      unsigned int i;
      for (i = 0; i < vec_num(d->ample_cand); i++) {
        ((ContextC1)vec_get(d->ample_cand, i))->is_ample_cand = TRUE;
      }

      if (!dpor_satisfied_C1(d, rc, &v_val)) {
        POR_DEBUG(printf("@@ found trans depended on ample set\n"));
        mc_store_successors(ss, s, rc, new_s, flag);
      } else {
        POR_DEBUG(printf("@@ ample set ok\n"));
        dpor_ample_set_to_succ_tbl(ss, d->ample_cand, &v_val, rc, s, new_s, flag);
      }
    }

    vec_destroy(&v_key);
    vec_destroy(&v_val);
#ifdef DEBUG
    /* 独立な遷移に"indep", 依存遷移に"depends"と名前をつける */
  } else {
      unsigned int i, j;
      mc_store_successors(ss, s, rc, new_s, flag);
      for (i = 0; i < state_succ_num(s); i++) {
        State *succ;
        lmn_interned_str name = lmn_intern("ind");

        for (j = 0; j < vec_num(d->ample_cand); j++) {
          ContextC1 c = (ContextC1)vec_get(d->ample_cand, j);
          if (c->id == i) {
            name = lmn_intern("dep");
            break;
          }
        }

        succ = state_succ_state(s, i);
        s->successors[i] = transition_make(succ, name);
      }
      set_trans_obj(s);
    }
#endif
  }

  dpor_data_clear(d, rc);
}


void dpor_explore_redundunt_graph(StateSpace ss)
{
  if (reduced_stack) {
    Vector *new_ss, *search;
    LmnReactCxt rc;
    BYTE f, org_por, org_old, org_del;

    org_por = lmn_env.enable_por;
    org_old = lmn_env.enable_por_old;
    org_del = lmn_env.delta_mem;
    lmn_env.enable_por = FALSE;
    lmn_env.enable_por_old = FALSE;
    lmn_env.delta_mem = FALSE;

    f = 0x00U;
    mc_set_compress(f);
    mc_set_trans(f);
    new_ss = vec_make(32);
    search = vec_make(128);
    mc_react_cxt_init(&rc);

    while (!vec_is_empty(reduced_stack)) {
      State *s, *parent, *ret, tmp_s;
      Transition t;
      LmnMembrane *s_mem;

      t = (Transition)vec_pop(reduced_stack);
      s = transition_next_state(t);
      parent = state_get_parent(s);
      state_succ_add(parent, (succ_data_t)t);

      s_mem = state_mem(s);
      ret = statespace_insert(ss, s);
      if (ret == s) {
        s_set_reduced(s);
        lmn_mem_free_rec(s_mem);
        vec_push(search, (vec_data_t)s);
      } else {
        transition_set_state(t, ret);
        state_free(s);
      }

      state_free_mem(&tmp_s);
    }

    while (!vec_is_empty(search)) {
      State *s;
      AutomataState p_s;
      unsigned int i;

      s = (State *)vec_pop(search);
      p_s = MC_GET_PROPERTY(s, statespace_automata(ss));

      s_set_reduced(s);
      mc_expand(ss, s, p_s, &rc, new_ss, statespace_propsyms(ss), f);

      for (i = 0; i < state_succ_num(s); i++) {
        Transition succ_t = transition(s, i);
        vec_clear(&succ_t->rule_names);
        transition_add_rule(succ_t, lmn_intern("reduced"));
      }

      for (i = 0; i < vec_num(new_ss); i++) {
        vec_push(search, vec_get(new_ss, i));
      }

      vec_clear(new_ss);
    }

    lmn_env.enable_por = org_por;
    lmn_env.enable_por_old = org_old;
    lmn_env.delta_mem = org_del;

    vec_free(reduced_stack);
    mc_react_cxt_destroy(&rc);
    vec_free(new_ss);
    vec_free(search);
  }
}


/** ==================================
 *  ======== for debug only ==========
 *  ==================================
 */
static int dpor_LHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg);
static int dpor_RHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg);
static inline void dpor_LHS_flags_dump(BYTE f);
static inline void dpor_RHS_flags_dump(BYTE f);
static int dpor_LHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg);
static int dpor_RHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg);

void dpor_contextC1_dump(McDporData *d)
{
  st_foreach(d->delta_tbl, dpor_LHS_procs_dump_f, 0);
  st_foreach(d->delta_tbl, dpor_RHS_procs_dump_f, 0);
}

void dpor_contextC1_dump_eachL(ContextC1 c)
{
  dpor_LHS_procs_dump_f((LmnWord)c->d, (LmnWord)c, 0);
}

void dpor_contextC1_dump_eachR(ContextC1 c)
{
  dpor_RHS_procs_dump_f((LmnWord)c->d, (LmnWord)c, 0);
}


static int dpor_LHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  ContextC1 c;
  unsigned int id;

  c = (ContextC1)_v;
  id = (unsigned int)_k;

  printf("LHS[id%u, delta%u]:: ", c->id, id);
  proc_tbl_foreach(c->LHS_procs, dpor_LHS_dump_f, 0);
  printf("\n");

  return ST_CONTINUE;
}

static int dpor_LHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  BYTE flag = (BYTE)_v;
  printf("depID<%2lu: ", _k);
  dpor_LHS_flags_dump(flag);
  printf("> ");
  return 1;
}

static int dpor_RHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  ContextC1 c;
  unsigned int id;

  c = (ContextC1)_v;
  id = (unsigned int)_k;

  printf("RHS[id%u, delta%u]:: ", c->id, id);
  proc_tbl_foreach(c->RHS_procs, dpor_RHS_dump_f, 0);
  printf("\n");

  return ST_CONTINUE;
}

static int dpor_RHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg)
{
  BYTE flag = (BYTE)_v;
  printf("depID<%2lu:", _k);
  dpor_RHS_flags_dump(flag);
  printf("> ");
  return 1;
}


static inline void dpor_LHS_flags_dump(BYTE f)
{
  printf("%s", LHS_FL(f, LHS_MEM_GROOT)   ? "G" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NATOMS)  ? "A" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NMEMS)   ? "M" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NFLINKS) ? "F" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NORULES) ? "R" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_STABLE)  ? "S" : "-");
}


static inline void dpor_RHS_flags_dump(BYTE f)
{
  printf("%s", RHS_OP(f, OP_DEP_EXISTS)          ? "E" : "-");
  printf("%s", RHS_OP(f, OP_DEP_EXISTS_EX_GROOT) ? "G" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NATOMS)          ? "A" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NMEMS)           ? "M" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NFLINKS)         ? "F" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NORULES)         ? "R" : "-");
  printf("%s", RHS_OP(f, OP_DEP_STABLE)          ? "S" : "-");
}



int dpor_dependency_tbl_dump(McDporData *d)
{
  Vector *p;
  unsigned int i, n;

  p = d->ample_cand;

  n = vec_num(d->ample_cand);
  if (n == 0) {
    printf("Tr all independent\n");
  } else {
    for (i = 0; i < n; i++) {
      printf("Tr[%u] = depends\n", i);
    }
  }

  return 1;
}



