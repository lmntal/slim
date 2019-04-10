/*
 * dpor.c
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
 * $Id:
 */

#include "dpor.h"
#include "delta_membrane.h"
#include "dpor_naive.h"
#include "lmntal.h"
#include "mc.h"
#include "mc_worker.h"
#include "state.h"
#include "state.hpp"
#include "vm/vm.h"

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
 *   通常の状態展開処理では,
 * LHSの対象となるLMNtalプロセスのIDを収集することは可能. (BUT) RHSは,
 * コピーした状態を書換えるため, LHSとの依存関係を調べることが難しい.
 *   これを実現するには,
 *     コピー前のプロセスIDをkeyに,
 * 対応するコピー後のプロセスを管理するcopymapに加え,
 *     コピー後のプロセスIDをkeyに,
 * 対応するコピー前のプロセスを管理するcopymapが必要.
 *   Delta-Membraneを使用するデメリットは,
 *     一部未実装の中間語命令があることで動かないプログラムがあること
 *   だけで, 通常より高速化が見込める.
 *   Delta-Membraneの使用が標準になっていくことを前提に,
 * DPORアルゴリズムの実装を進める.
 */

/* スレッド毎に独立してデータを持たせる */
McDporData **dpor_data;

/* for DEBUG (MT-Unsafe):
 * 削減したグラフを表示するために最後に状態展開を行うためのStack */
Vector *reduced_stack = NULL;

struct ContextC1 {
  BOOL is_ample_cand;
  BOOL is_on_path;
  MemDeltaRoot
      *d; /* 本遷移を実現する階層グラフの差分オブジェクトへのポインタ */
  /* TODO: BYTEサイズのflagをサポートするSimpleProcessTableにする */
  ProcessTableRef LHS_procs; /* プロセスのIDがkey, LHSフラグがvalue */
  ProcessTableRef RHS_procs; /* プロセスのIDがkey, RHSフラグがvalue */
  unsigned int id;
};

struct ContextC2 {
  LmnWord wt;
  LmnByte at, tt;
  unsigned int wt_size;
};

static ContextC1Ref contextC1_make(MemDeltaRoot *d, unsigned int id) {
  ContextC1Ref c = LMN_MALLOC(struct ContextC1);
  c->d = d;
  c->LHS_procs = proc_tbl_make_with_size(32);
  c->RHS_procs = proc_tbl_make_with_size(32);
  c->is_on_path = FALSE;
  c->is_ample_cand = FALSE;
  c->id = id;
  return c;
}

static inline void contextC1_free(ContextC1Ref c) {
  proc_tbl_free(c->LHS_procs);
  proc_tbl_free(c->RHS_procs);
  LMN_FREE(c);
}

static int contextC1_free_f(st_data_t _k, st_data_t _v, st_data_t _arg) {
  contextC1_free((ContextC1Ref)_v);
  return ST_CONTINUE;
}

static BOOL contextC1s_eq(ContextC1Ref a, ContextC1Ref b) {
  return proc_tbl_eq(a->LHS_procs, b->LHS_procs) &&
         proc_tbl_eq(a->RHS_procs, b->RHS_procs);
}

/* 既出の遷移か否かを判定する.
 * 新規の場合はsrc, そうでない場合は既出のContextC1オブジェクトを返す. */
static ContextC1Ref contextC1_lookup(st_table_t dst_tbl, ContextC1Ref src) {
  ContextC1Ref ret;
  Vector tmp;
  unsigned int i;

  /* TODO: O(n * m), n:出現した遷移数, m:遷移が持つプロセス数.
   *       遷移の等価性を検査するためのハッシュ関数が必要 */

  tmp.init(st_num(dst_tbl) + 1);
  st_get_entries_value(dst_tbl, &tmp);

  ret = src;
  for (i = 0; i < tmp.get_num(); i++) {
    ContextC1Ref dst = (ContextC1Ref)tmp.get(i);
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

  tmp.destroy();
  return ret;
}

/** プロセスIDが_kなアトム_vをLHSテーブルに追加する.
 *  ground_atomsで求めたアトムの集合に対して使用する */
static int contextC1_expand_gatoms_LHS_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  ContextC1Ref c;
  //  LmnWord key;

  c = (ContextC1Ref)_arg;
  //  key = ((LmnSAtom)_v)->get_id();

  if (!proc_tbl_get(c->LHS_procs, _k, NULL)) {
    proc_tbl_put(c->LHS_procs, _k, LHS_DEFAULT);
  }

  return 1;
}

/* 作業配列に含まれるTT_ATOM, TT_MEM属性のプロセスをLHSテーブルに登録する */
static void contextC1_expand_LHS(McDporData *d, ContextC1Ref c,
                                 LmnReactCxtRef rc) {
  auto &v = rc->work_array;
  unsigned int i;

  for (i = 0; i < rc->capacity(); i++) {
    LmnWord key, t;
    BYTE flag;
    LmnRegisterRef r = &v.at(i);

    if (r->register_tt() == TT_ATOM && !LMN_ATTR_IS_DATA(r->register_at())) {
      key = ((LmnSymbolAtomRef)r->register_wt())->get_id();
    } else if (r->register_tt() == TT_MEM) {
      key = ((LmnMembraneRef)r->register_wt())->mem_id();
      if (i == 0) {
        dpor_LHS_flag_add(d, key, LHS_MEM_GROOT);
      }
    } else {
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

  for (i = 0; i < d->wt_gatoms->get_num(); i++) {
    ProcessTableRef g_atoms = (ProcessTableRef)d->wt_gatoms->get(i);
    proc_tbl_foreach(g_atoms, contextC1_expand_gatoms_LHS_f, (LmnWord)c);
  }
}

static inline void contextC1_RHS_tbl_put(ProcessTableRef p, LmnWord key,
                                         BYTE set) {
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

static inline void contextC1_RHS_tbl_unput(ProcessTableRef p, LmnWord key,
                                           BYTE unset) LMN_UNUSED;
static inline void contextC1_RHS_tbl_unput(ProcessTableRef p, LmnWord key,
                                           BYTE unset) {
  LmnWord t;
  if (proc_tbl_get(p, key, &t)) {
    BYTE op = (BYTE)t;
    RHS_OP_UNSET(op, unset);
    proc_tbl_put(p, key, op);
  }
}

/* プロセスIDをkeyに, オペレーションフラグをvalueにしたテーブルを展開する */
static void contextC1_expand_RHS_inner(ContextC1Ref c, struct MemDelta *d) {
  LmnMembraneRef mem;
  unsigned int i;
  BOOL need_act_check;
  BOOL need_flink_check;

  mem = d->mem;
  need_act_check = FALSE;
  need_flink_check = FALSE;

  /* アトムの数が変化した場合,
   * memに対するnatoms命令が左辺に出現した遷移に依存する */
  if (d->new_atoms.get_num() > 0 || d->del_atoms.get_num() > 0 ||
      d->data_atom_diff != 0) {
    int n = d->new_atoms.get_num() + d->data_atom_diff - d->del_atoms.get_num();
    if (mem->atom_num() != n) {
      contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_NATOMS);
    }
    need_act_check = TRUE;
  }

  /* 子膜の数が変化した場合, memに対するnmems命令が左辺に出現した遷移に依存する
   */
  if (d->new_mems.get_num() > 0 || d->del_mems.get_num() > 0) {
    int n = d->new_mems.get_num() - d->del_mems.get_num();
    if (mem->child_mem_num() != n) {
      contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_NMEMS);
    }
    need_act_check = TRUE;
  }

  if (!d->new_proxies.is_empty()) {
    contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_NFLINKS);
    need_act_check = TRUE;
  }

  /* ルールセットが消滅(clearrules)する場合, memが左辺に出現する遷移に依存する
   */
  if (d->ruleset_removed) {
    contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_EXISTS);
    need_act_check = TRUE;
  }

  /* 新たなルールセットが追加される場合,
   * memに対するnorules命令が左辺に出現する遷移に依存する */
  if (d->new_rulesets) {
    contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_NORULES);
    need_act_check = TRUE;
  }

  /* アトムが削除されるなら, そのアトムが左辺に出現した遷移に依存する */
  for (i = 0; i < d->del_atoms.get_num(); i++) {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)d->del_atoms.get(i);
    contextC1_RHS_tbl_put(c->RHS_procs, a->get_id(), OP_DEP_EXISTS);
    if (a->get_functor() == LMN_IN_PROXY_FUNCTOR) {
      need_flink_check = TRUE;
    }
  }

  /* inside proxyアトムが削除されていた場合 */
  if (need_flink_check) {
    contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_NFLINKS);
    need_act_check = TRUE;
  }

  /* 子膜が削除されるなら, その膜が左辺に出現した遷移に依存する */
  for (i = 0; i < d->del_mems.get_num(); i++) {
    LmnMembraneRef m = (LmnMembraneRef)d->del_mems.get(i);
    contextC1_RHS_tbl_put(c->RHS_procs, m->mem_id(), OP_DEP_EXISTS);
  }

  /* 膜名が変更された場合,
   * その膜をanymem等で走査した遷移(つまりwt[0]以外のTT_MEM)に依存する */
  if (d->org_name != d->new_name) {
    contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(),
                          OP_DEP_EXISTS_EX_GROOT);
    need_act_check = TRUE;
  }

  /* 膜になんらかの変化があった場合,
   * 辿れる限りの所属膜に対するSTABLE判定に依存する */
  if (need_act_check) {
    while (mem) {
      contextC1_RHS_tbl_put(c->RHS_procs, mem->mem_id(), OP_DEP_STABLE);
      mem = mem->mem_parent();
    }
  }
}

static void contextC1_expand_RHS(McDporData *mc, ContextC1Ref c,
                                 MemDeltaRoot *d) {
  unsigned int i;
  /* 修正の加えられる膜に対する操作 */
  for (i = 0; i < d->mem_deltas.get_num(); i++) {
    contextC1_expand_RHS_inner(c,
                               (struct MemDelta *)d->mem_deltas.get(i));
  }

  /* リンクの繋ぎ替えは考慮しなくてよいはず.
   * -> 繋ぎ替え元のアトムに依存するが,
   *    繋ぎ替え元のアトムは既にstruct
   * MemDeltaのdel_atomsに含まれているので計算済み */
  /*
  for (i = 0; i < d->modified_atoms.get_num(); i += 2) {
    LmnSAtom src, new;
    src = LMN_SATOM(d->modified_atoms.get(i));
    new = LMN_SATOM(d->modified_atoms.get(i+1));
  }
   */
}

static McDporData *dpor_data_make() {
  McDporData *d = LMN_MALLOC(McDporData);
  d->wt_gatoms = new Vector(4);
  d->wt_flags = proc_tbl_make();
  d->ample_cand = new Vector(8);
  d->nxt_tr_id = 0;
  d->delta_tbl = st_init_ptrtable();
  d->free_deltas = new Vector(32);
  return d;
}

static void dpor_data_free(McDporData *d) {
  unsigned int i;
  delete d->wt_gatoms;
  proc_tbl_free(d->wt_flags);
  delete d->ample_cand;
  st_foreach(d->delta_tbl, (st_iter_func)contextC1_free_f, (st_data_t)0);
  st_free_table(d->delta_tbl);
  for (i = 0; i < d->free_deltas->get_num(); i++) {
    MemDeltaRoot *delt = (MemDeltaRoot *)d->free_deltas->get(i);
    delete delt;
  }
  delete d->free_deltas;
  LMN_FREE(d);
}

static void dpor_data_clear(McDporData *d, LmnReactCxtRef rc) {
  d->wt_gatoms->clear();
  proc_tbl_clear(d->wt_flags);
  d->ample_cand->clear();
  st_foreach(d->delta_tbl, (st_iter_func)contextC1_free_f, (st_data_t)0);
  st_clear(d->delta_tbl);

  while (!d->free_deltas->is_empty()) {
    MemDeltaRoot *delt = (MemDeltaRoot *)d->free_deltas->pop();
    delete delt;
  }

  RC_MEM_DELTAS(rc)->clear();

  d->free_deltas->clear();
  d->nxt_tr_id = 0;
}

void dpor_env_init(void) {
  if (lmn_env.enable_por_old) {
    init_por_vars();
  } else {
    unsigned int i, n;
    n = lmn_env.core_num;
    dpor_data = LMN_NALLOC(McDporData *, n);
    for (i = 0; i < n; i++) {
      dpor_data[i] = dpor_data_make();
    }
  }
}

void dpor_env_destroy(void) {
  if (lmn_env.enable_por_old) {
    free_por_vars();
  } else {
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
static inline BOOL dpor_LHS_RHS_are_depend(BYTE lhs, BYTE rhs) {
  if (RHS_OP(rhs, OP_DEP_EXISTS)) {
    return TRUE;
  } else if (RHS_OP(rhs, OP_DEP_EXISTS_EX_GROOT) &&
             !LHS_FL(lhs, LHS_MEM_GROOT)) {
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
static BOOL contextC1s_are_depend(ContextC1Ref src, ContextC1Ref dst) {
  ProcessTableRef rhs_tbl;
  ProcessTableRef lhs_tbl;
  unsigned int i;

  if (src == dst)
    return FALSE;

  rhs_tbl = src->RHS_procs;
  lhs_tbl = dst->LHS_procs;

  for (auto rhs : *rhs_tbl) {
    ProcessID lhs;
    if (lhs_tbl->get(rhs.first, &lhs)) {
      if (dpor_LHS_RHS_are_depend((BYTE)lhs, (BYTE)rhs.second)) {
        return TRUE;
      }
    }
  }

  return FALSE;
}

/* srcに積まれたdeltamemから取得するContextC1の独立性を検査し依存遷移の集合をretに積む.
 * 依存遷移が存在する場合 or
 * ContextC1がample_candに含まれるContextC1に依存している場合, FALSEを返す */
static BOOL dpor_dependency_check(McDporData *d, Vector *src, Vector *ret) {
  unsigned int i, j;
  BOOL need_ample_check, ok;

  if (d->ample_cand->get_num() > 0) {
    need_ample_check = TRUE;
  } else {
    need_ample_check = FALSE;
  }

  ok = TRUE;

  /* i番目のRHSとj番目のLHSの依存性をチェックし, 依存する組合せをretへpush */
  for (i = 0; i < src->get_num(); i++) {
    ContextC1Ref r;
    MemDeltaRoot *delta_r;
    st_data_t t;

    delta_r = (MemDeltaRoot *)src->get(i);
    if (st_lookup(d->delta_tbl, (st_data_t)delta_r, &t)) {
      r = (ContextC1Ref)t;
    } else {
      lmn_fatal("implementation error");
    }

    for (j = 0; j < src->get_num(); j++) {
      ContextC1Ref l;
      MemDeltaRoot *delta_l;

      if (j == i)
        continue;
      delta_l = (MemDeltaRoot *)src->get(j);
      if (st_lookup(d->delta_tbl, (st_data_t)delta_l, &t)) {
        l = (ContextC1Ref)t;
      } else {
        lmn_fatal("implementation error");
      }

      if (contextC1s_are_depend(r, l)) {
        if (ret) {
          if (!ret->contains((vec_data_t)r)) {
            ret->push((vec_data_t)r);
          }
          if (!ret->contains((vec_data_t)l)) {
            ret->push((vec_data_t)l);
          }
        }

        ok = FALSE;
      }
    }

    if (need_ample_check && !r->is_ample_cand) {
      for (j = 0; j < d->ample_cand->get_num(); j++) {
        if (contextC1s_are_depend(r, (ContextC1Ref)d->ample_cand->get(j))) {
          ok = FALSE;
        }
      }
    }
  }

  return ok;
}

static inline BOOL dpor_explored_cycle(McDporData *mc, ContextC1Ref c,
                                       LmnReactCxtRef rc) {
  st_data_t t;
  MemDeltaRoot *d;
  unsigned int i;

  for (i = 0; i < RC_MEM_DELTAS(rc)->get_num(); i++) {
    d = (MemDeltaRoot *)RC_MEM_DELTAS(rc)->get(i);
    if (st_lookup(mc->delta_tbl, (st_data_t)d, &t)) {
      ContextC1Ref succ = (ContextC1Ref)t;
      if (succ->is_on_path) {
        return TRUE;
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  return FALSE;
}

static BOOL dpor_explore_subgraph(McDporData *mc, ContextC1Ref c,
                                  Vector *cur_checked_ids) {
  LmnMembraneRef cur;
  Vector nxt_checked_ids;
  unsigned int i;
  BOOL ret;

  MCReactContext rc;
  cur = DMEM_ROOT_MEM(c->d);
  nxt_checked_ids.init(4);
  RC_SET_GROOT_MEM(&rc, cur);

  POR_DEBUG({
    printf("explore rec%u, delta_reacted_id=%u\n", mc->cur_depth, c->id);
    lmn_dump_mem_stdout(cur);
  });
  mc_expand_inner(&rc, cur);

  ret = TRUE;

  if (dpor_explored_cycle(mc, c, &rc)) {
    POR_DEBUG(printf("detected cycle\n"));
    nxt_checked_ids.destroy();
    return ret;
  }

  if (dpor_dependency_check(mc, RC_MEM_DELTAS(&rc), NULL) &&
      mc->cur_depth < 200) {

    for (i = 0; i < RC_MEM_DELTAS(&rc)->get_num(); i++) {
      MemDeltaRoot *succ_d;
      st_data_t t;

      if (ret == FALSE)
        break;

      /* succ_dを生成した際に, 等価な遷移と置換されている */
      succ_d = (MemDeltaRoot *)RC_MEM_DELTAS(&rc)->get(i);
      if (st_lookup(mc->delta_tbl, (st_data_t)succ_d, &t)) {
        ContextC1Ref succ_c = (ContextC1Ref)t;

        if (cur_checked_ids->contains((vec_data_t)succ_c->id)) {
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
          nxt_checked_ids.push(
                   succ_c->id); /* next stepに合流性の情報を渡す */
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

  nxt_checked_ids.destroy();

  return ret;
}

static BOOL dpor_satisfied_C1(McDporData *d, LmnReactCxtRef rc,
                              Vector *working_set) {
  Vector checked_ids;
  unsigned int i;
  BOOL ret;

  checked_ids.init(16);

  ret = TRUE;
  POR_DEBUG({
    unsigned int _i;
    for (_i = 0; _i < d->ample_cand->get_num(); _i++) {
      ContextC1Ref _c_ = (ContextC1Ref)d->ample_cand->get(_i);
      printf("AMP[id=%u, delta=%lu]\n", _c_->id, (LmnWord)_c_->d);
    }
  });
  for (i = 0; i < working_set->get_num(); i++) {
    ContextC1Ref c = (ContextC1Ref)working_set->get(i);
    if (!c->is_ample_cand) {
      d->cur_depth = 0;
      c->is_on_path = TRUE;
      dmem_root_commit(c->d);
      ret = dpor_explore_subgraph(d, c, &checked_ids);
      checked_ids.push(c->id);
      dmem_root_revert(c->d);
      c->is_on_path = FALSE;
      if (!ret)
        break;
    }
  }

  checked_ids.destroy();
  return ret;
}

void dpor_transition_gen_LHS(McDporData *mc, MemDeltaRoot *d,
                             LmnReactCxtRef rc) {
  ContextC1Ref c;

  c = contextC1_make(d, mc->nxt_tr_id++);
  contextC1_expand_LHS(mc, c, rc);
  mc->tmp = c;
}

BOOL dpor_transition_gen_RHS(McDporData *mc, MemDeltaRoot *d, LmnReactCxtRef rc) {
  ContextC1Ref c, ret;

  c = mc->tmp;
  mc->tmp = NULL;

  contextC1_expand_RHS(mc, c, d);

  ret = contextC1_lookup(mc->delta_tbl, c);
  POR_DEBUG({
    printf("gen trans id=%u\n", c->id);
    dpor_contextC1_dump_eachL(c);
    dpor_contextC1_dump_eachR(c);
  });

  if (ret != c && !RC_MEM_DELTAS(rc)->contains((vec_data_t)ret->d)) {
    POR_DEBUG({
      printf("detected same trans id=%u\n", ret->id);
      printf("aborted trans_id=%u\n", c->id);
      dpor_contextC1_dump_eachL(ret);
      dpor_contextC1_dump_eachR(ret);
    });

    if (!ret->is_ample_cand) {
      if (!RC_MEM_DELTAS(rc)->contains((vec_data_t)ret->d)) {
        POR_DEBUG(printf("push succ\n\n"));
        mc_react_cxt_add_mem_delta(rc, ret->d, NULL);
      }
    }

    contextC1_free(c);
    return FALSE;
  } else {
    st_add_direct(mc->delta_tbl, (st_data_t)d, (st_data_t)c);
    mc->free_deltas->push((vec_data_t)d);
    return TRUE;
  }
}

/* C3(Cycle Ignoring Problem)の検査を行うする.
 * srcから生成した状態gen_succへの遷移が閉路形成を行うものでなければTRUEを,
 * 閉路形成を行う場合(もしくは行う恐れがある場合)はFALSEを返す.
 * ins_succにはstatespace_insert手続きが返す状態を渡す */
static BOOL dpor_check_cycle_proviso(StateSpaceRef ss, State *src,
                                     State *gen_succ, State *ins_succ) {
  if (gen_succ == ins_succ) {
    /* General Visited Proviso:
     *  既存状態への再訪問でない(新規状態への遷移)なら閉路形成を行う遷移ではない.
     *  Hash-Based分割と併用するとサクセッサの情報を取得するための通信で遅くなる.
     */
    return TRUE;
  } else if (ins_succ == src) {
    /* self-loop detection */
    return FALSE;
  } else if (ins_succ->is_on_stack()) {
    /* Stack Proviso:
     *  Stack上の状態に戻るということは閉路であるということ
     *  DFS Stackによる空間構築(逐次)が前提 */
    return FALSE;
  } else if (lmn_env.bfs && ins_succ->is_expanded() && lmn_env.core_num == 1) {
    /* Open Set Proviso:
     *  閉路形成を行なう遷移は,
     *  展開済み状態へ再訪問する遷移のサブセットである.(逐次限定) */
    return FALSE;
  }

  return TRUE;
}

/* TODO: 時間なくて雑.. 直す */
static void dpor_ample_set_to_succ_tbl(StateSpaceRef ss, Vector *ample_set,
                                       Vector *contextC1_set, LmnReactCxtRef rc,
                                       State *s, Vector *new_ss, BOOL f) {
  st_table_t succ_tbl; /* サクセッサのポインタの重複検査に使用 */
  unsigned int i, succ_i;
  BOOL satisfied_C3;

  succ_i = 0;
  succ_tbl = RC_SUCC_TBL(rc);

  for (i = 0; i < RC_EXPANDED(rc)->get_num(); i++) { /* ごめんなさいort */
    State *succ_s;
    if (s->has_trans_obj()) {
      TransitionRef succ_t = (TransitionRef)RC_EXPANDED(rc)->get(i);
      succ_s = transition_next_state(succ_t);
      transition_free(succ_t);
    } else {
      succ_s = (State *)RC_EXPANDED(rc)->get(i);
    }
    delete (succ_s);
  }

  satisfied_C3 = TRUE;
  for (i = 0; i < ample_set->get_num(); i++) {
    TransitionRef src_t;
    st_data_t tmp;
    State *src_succ, *succ;
    MemDeltaRoot *succ_d;
    ContextC1Ref succ_c;

    succ_c = (ContextC1Ref)ample_set->get(i);
    succ_d = succ_c->d;
    src_succ = new State();
    state_set_parent(src_succ, s);
    state_set_property_state(src_succ, DEFAULT_STATE_ID);
    if (s->has_trans_obj()) {
      src_t = transition_make(src_succ, lmn_intern("ample set"));
    } else {
      src_t = NULL;
    }

    succ = ss->insert_delta(src_succ, succ_d);
    if (succ == src_succ) {
      state_id_issue(succ);
      if (mc_is_dump(f))
        dump_state_data(succ, (LmnWord)stdout, (LmnWord)NULL);
      if (new_ss) {
        new_ss->push((vec_data_t)succ);
      }
    } else {
      delete (src_succ);
      if (s->has_trans_obj()) {
        transition_set_state(src_t, succ);
      } else {
        RC_EXPANDED(rc)->set(i, (vec_data_t)succ);
      }
    }

    if (mc_has_property(f)) {
      satisfied_C3 = dpor_check_cycle_proviso(ss, s, src_succ, succ);
    }

    tmp = 0;
    if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) {
      st_data_t ins = s->has_trans_obj() ? (st_data_t)src_t : (st_data_t)succ;
      st_add_direct(succ_tbl, (st_data_t)succ, ins);
      RC_EXPANDED(rc)->set(succ_i++, ins);
    } else {
      if (s->has_trans_obj()) {
        transition_free(src_t);
      }
    }
  }

  if (!satisfied_C3) {
    /* ample set以外の遷移も展開する */
    for (i = 0; i < contextC1_set->get_num(); i++) {
      TransitionRef src_t;
      st_data_t tmp;
      State *src_succ, *succ;
      MemDeltaRoot *succ_d;
      ContextC1Ref succ_c;

      succ_c = (ContextC1Ref)contextC1_set->get(i);

      if (succ_c->is_ample_cand)
        continue; /* さっき登録した */

      succ_d = succ_c->d;
      src_succ = new State();
      state_set_parent(src_succ, s);
      state_set_property_state(src_succ, DEFAULT_STATE_ID);
      if (s->has_trans_obj()) {
        src_t = transition_make(src_succ, ANONYMOUS);
      } else {
        src_t = NULL;
      }

      succ = ss->insert_delta(src_succ, succ_d);
      if (succ == src_succ) {
        state_id_issue(succ);
        if (mc_is_dump(f))
          dump_state_data(succ, (LmnWord)stdout, (LmnWord)NULL);
        if (new_ss) {
          new_ss->push((vec_data_t)succ);
        }
      } else {
        delete (src_succ);
        if (s->has_trans_obj()) {
          transition_set_state(src_t, succ);
        } else {
          RC_EXPANDED(rc)->set(i, (vec_data_t)succ);
        }
      }

      tmp = 0;
      if (!st_lookup(succ_tbl, (st_data_t)succ, (st_data_t *)&tmp)) {
        st_data_t ins = s->has_trans_obj() ? (st_data_t)src_t : (st_data_t)succ;
        st_add_direct(succ_tbl, (st_data_t)succ, ins);
        RC_EXPANDED(rc)->set(succ_i++, ins);
      } else {
        if (s->has_trans_obj()) {
          transition_free(src_t);
        }
      }
    }
  }
#ifdef DEBUG
  else if (lmn_env.show_reduced_graph) {
    if (!reduced_stack) {
      reduced_stack = new Vector(512);
    }
    for (i = 0; i < contextC1_set->get_num(); i++) {
      State *src_succ;
      MemDeltaRoot *succ_d;
      ContextC1Ref succ_c;
      TransitionRef src_t;

      succ_c = (ContextC1Ref)contextC1_set->get(i);

      if (succ_c->is_ample_cand)
        continue; /* さっき登録した */

      succ_d = succ_c->d;
      src_succ = new State();
      state_set_parent(src_succ, s);
      state_set_property_state(src_succ, DEFAULT_STATE_ID);
      src_t = transition_make(src_succ, lmn_intern("reduced"));

      dmem_root_commit(succ_d); /* src_succに対応したグラフ構造へ */
      src_succ->state_set_mem(DMEM_ROOT_MEM(succ_d));
      src_succ->state_calc_hash(
          src_succ->state_mem(),
          ss->use_memenc()); /* それを元にハッシュ値やmem_idを計算 */
      if (!src_succ->is_encoded()) {
        src_succ->state_set_binstr(state_calc_mem_dump(src_succ));
      }
      reduced_stack->push((vec_data_t)src_t);
      dmem_root_revert(succ_d); /* 元に戻す */
    }
  }
#endif

  RC_EXPANDED(rc)->num = succ_i;
  s->succ_set(RC_EXPANDED(rc)); /* successorを登録 */
  st_clear(RC_SUCC_TBL(rc));
}

//#define DEP_DEVEL

void dpor_start(StateSpaceRef ss, State *s, LmnReactCxtRef rc, Vector *new_s,
                BOOL flag) {
  McDporData *d = RC_POR_DATA(rc);

  if (RC_MC_USE_DPOR_NAIVE(rc)) {
    por_calc_ampleset(ss, s, rc, new_s, flag);
    return;
  } else if (mc_react_cxt_succ_num_org(rc) <= 1 || !RC_MC_USE_DMEM(rc)) {
    mc_store_successors(ss, s, rc, new_s, flag);
  } else {
    Vector v_key, v_val;

    v_key.init(32);
    v_val.init(32);
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
      if (d->ample_cand->is_empty()) {
        ContextC1Ref c;
        st_data_t t;
        if (st_lookup(d->delta_tbl, (st_data_t)RC_MEM_DELTAS(rc)->get(0),
                      &t)) {
          c = (ContextC1Ref)t;
          c->is_ample_cand = TRUE; /* だいじ */
        } else {
          lmn_fatal("unexpected");
        }
        d->ample_cand->push((vec_data_t)c);
      }

      if (d->ample_cand->get_num() == mc_react_cxt_succ_num_org(rc)) {
        POR_DEBUG(printf("@@ ample cand == succ num\n"));
        mc_store_successors(ss, s, rc, new_s, flag);
      } else {

        unsigned int i;
        for (i = 0; i < d->ample_cand->get_num(); i++) {
          ((ContextC1Ref)d->ample_cand->get(i))->is_ample_cand = TRUE;
        }

        if (!dpor_satisfied_C1(d, rc, &v_val)) {
          POR_DEBUG(printf("@@ found trans depended on ample set\n"));
          mc_store_successors(ss, s, rc, new_s, flag);
        } else {
          POR_DEBUG(printf("@@ ample set ok\n"));
          dpor_ample_set_to_succ_tbl(ss, d->ample_cand, &v_val, rc, s, new_s,
                                     flag);
        }
      }

      v_key.destroy();
      v_val.destroy();
#ifdef DEBUG
      /* 独立な遷移に"indep", 依存遷移に"depends"と名前をつける */
    } else {
      unsigned int i, j;
      mc_store_successors(ss, s, rc, new_s, flag);
      for (i = 0; i < s->successor_num; i++) {
        State *succ;
        lmn_interned_str name = lmn_intern("ind");

        for (j = 0; j < d->ample_cand->get_num(); j++) {
          ContextC1Ref c = (ContextC1Ref)d->ample_cand->get(j);
          if (c->id == i) {
            name = lmn_intern("dep");
            break;
          }
        }

        succ = state_succ_state(s, i);
        s->successors[i] = transition_make(succ, name);
      }
      s->set_trans_obj();
    }
#endif
  }

  dpor_data_clear(d, rc);
}

void dpor_explore_redundunt_graph(StateSpaceRef ss) {
  if (reduced_stack) {
    Vector *new_ss, *search;
    MCReactContext rc;
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
    new_ss = new Vector(32);
    search = new Vector(128);

    while (!reduced_stack->is_empty()) {
      State *s, *parent, *ret, tmp_s;
      TransitionRef t;
      LmnMembraneRef s_mem;

      t = (TransitionRef)reduced_stack->pop();
      s = transition_next_state(t);
      parent = state_get_parent(s);
      parent->succ_add((succ_data_t)t);

      s_mem = s->state_mem();
      ret = ss->insert(s);
      if (ret == s) {
        s->s_set_reduced();
        s_mem->free_rec();
        search->push((vec_data_t)s);
      } else {
        transition_set_state(t, ret);
        delete (s);
      }

      tmp_s.free_mem();
    }

    while (!search->is_empty()) {
      State *s;
      AutomataStateRef p_s;
      unsigned int i;

      s = (State *)search->pop();
      p_s = MC_GET_PROPERTY(s, ss->automata());

      s->s_set_reduced();
      mc_expand(ss, s, p_s, &rc, new_ss, ss->prop_symbols(), f);

      for (i = 0; i < s->successor_num; i++) {
        TransitionRef succ_t = transition(s, i);
        succ_t->rule_names.clear();
        transition_add_rule(succ_t, lmn_intern("reduced"), 0U);
      }

      for (i = 0; i < new_ss->get_num(); i++) {
        search->push(new_ss->get(i));
      }

      new_ss->clear();
    }

    lmn_env.enable_por = org_por;
    lmn_env.enable_por_old = org_old;
    lmn_env.delta_mem = org_del;

    delete reduced_stack;
    delete new_ss;
    delete search;
  }
}

void dpor_LHS_flag_add(McDporData *d, LmnWord proc_id, BYTE set_f) {
  LmnWord t;
  BYTE flags;

  d = DPOR_DATA();
  if (proc_tbl_get(d->wt_flags, proc_id, &t)) { /* CONTAINS */
    flags = (BYTE)t;
  } else { /* NEW */
    flags = LHS_DEFAULT;
  }

  LHS_FL_SET(flags, set_f);
  proc_tbl_put(d->wt_flags, proc_id, (LmnWord)flags);
}

void dpor_LHS_flag_remove(McDporData *d, LmnWord proc_id, BYTE unset_f) {
  LmnWord t;
  BYTE flags;

  d = DPOR_DATA();
  if (proc_tbl_get(d->wt_flags, proc_id, &t)) { /* CONTAINS */
    flags = (BYTE)t;
  } else { /* NEW */
    flags = 0;
    LMN_ASSERT(0);
  }

  LHS_FL_UNSET(flags, unset_f);
  proc_tbl_put(d->wt_flags, proc_id, (LmnWord)flags);
}

void dpor_LHS_add_ground_atoms(McDporData *d, ProcessTableRef atoms) {
  d->wt_gatoms->push((vec_data_t)atoms);
}

void dpor_LHS_remove_ground_atoms(McDporData *d, ProcessTableRef atoms) {
  if (d->wt_gatoms->peek() == (vec_data_t)atoms) {
    d->wt_gatoms->pop();
  } else {
    /* pushした順にpopされるので, ここに来ることはまずないが念のため書いておく
     */
    unsigned int i;
    for (i = 0; i < d->wt_gatoms->get_num(); i++) {
      vec_data_t t = d->wt_gatoms->get(i);
      if (t == (vec_data_t)atoms) {
        d->wt_gatoms->pop_n(i);
      }
    }
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

void dpor_contextC1_dump(McDporData *d) {
  st_foreach(d->delta_tbl, (st_iter_func)dpor_LHS_procs_dump_f, (st_data_t)0);
  st_foreach(d->delta_tbl, (st_iter_func)dpor_RHS_procs_dump_f, (st_data_t)0);
}

void dpor_contextC1_dump_eachL(ContextC1Ref c) {
  dpor_LHS_procs_dump_f((LmnWord)c->d, (LmnWord)c, 0);
}

void dpor_contextC1_dump_eachR(ContextC1Ref c) {
  dpor_RHS_procs_dump_f((LmnWord)c->d, (LmnWord)c, 0);
}

static int dpor_LHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  ContextC1Ref c;
  unsigned int id;

  c = (ContextC1Ref)_v;
  id = (unsigned int)_k;

  printf("LHS[id%u, delta%u]:: ", c->id, id);
  proc_tbl_foreach(c->LHS_procs, dpor_LHS_dump_f, 0);
  printf("\n");

  return ST_CONTINUE;
}

static int dpor_LHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  BYTE flag = (BYTE)_v;
  printf("depID<%2lu: ", _k);
  dpor_LHS_flags_dump(flag);
  printf("> ");
  return 1;
}

static int dpor_RHS_procs_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  ContextC1Ref c;
  unsigned int id;

  c = (ContextC1Ref)_v;
  id = (unsigned int)_k;

  printf("RHS[id%u, delta%u]:: ", c->id, id);
  proc_tbl_foreach(c->RHS_procs, dpor_RHS_dump_f, 0);
  printf("\n");

  return ST_CONTINUE;
}

static int dpor_RHS_dump_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  BYTE flag = (BYTE)_v;
  printf("depID<%2lu:", _k);
  dpor_RHS_flags_dump(flag);
  printf("> ");
  return 1;
}

static inline void dpor_LHS_flags_dump(BYTE f) {
  printf("%s", LHS_FL(f, LHS_MEM_GROOT) ? "G" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NATOMS) ? "A" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NMEMS) ? "M" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NFLINKS) ? "F" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_NORULES) ? "R" : "-");
  printf("%s", LHS_FL(f, LHS_MEM_STABLE) ? "S" : "-");
}

static inline void dpor_RHS_flags_dump(BYTE f) {
  printf("%s", RHS_OP(f, OP_DEP_EXISTS) ? "E" : "-");
  printf("%s", RHS_OP(f, OP_DEP_EXISTS_EX_GROOT) ? "G" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NATOMS) ? "A" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NMEMS) ? "M" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NFLINKS) ? "F" : "-");
  printf("%s", RHS_OP(f, OP_DEP_NORULES) ? "R" : "-");
  printf("%s", RHS_OP(f, OP_DEP_STABLE) ? "S" : "-");
}

int dpor_dependency_tbl_dump(McDporData *d) {
  unsigned int i, n;

  n = d->ample_cand->get_num();
  if (n == 0) {
    printf("Tr all independent\n");
  } else {
    for (i = 0; i < n; i++) {
      printf("Tr[%u] = depends\n", i);
    }
  }

  return 1;
}
