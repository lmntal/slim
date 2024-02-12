/*
 * react_context.cpp
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

#include "react_context.hpp"

#include "hyperlink.h"
#include "task.h"
#include "verifier/verifier.h"

#ifdef USE_FIRSTCLASS_RULE
#include "firstclass_rule.h"
#endif
#include "rule.hpp"

// struct LmnRegister {
//  LmnWord wt;
//  LmnByte at;
//  LmnByte tt;
//};

// struct LmnReactCxt {
//  LmnMembraneRef
//      global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
//  LmnRegisterArray work_arry; /* ルール適用レジスタ */
//  unsigned int warray_cur;     /* work_arryの現在の使用サイズ */
//  unsigned int warray_num; /* work_arryの最大使用サイズ(SPEC命令指定) */
//  unsigned int warray_cap; /* work_arryのキャパシティ */
//  unsigned int trace_num; /* ルール適用回数 (通常実行用トレース実行で使用)  */
//  LmnRulesetId
//      atomic_id; /* atomic step中: atomic set id(signed int), default:-1 */
//  ProcessID proc_org_id; /* atomic step終了時に Process ID をこの値に復帰 */
//  ProcessID proc_next_id; /* atomic step継続時に Process ID をこの値に設定 */
//  LmnMembraneRef cur_mem; /* atomic step継続時に現在膜をこの値に設定 */
//  BYTE mode;
//  BOOL flag;                     /* mode以外に指定するフラグ */
//  void *v;                       /* 各mode毎に固有の持ち物 */
//  SimpleHashtbl *hl_sameproccxt; /* findatom
//                                    時のアトム番号と、同名型付きプロセス文脈を持つアトム引数との対応関係を保持
//                                  */
//#ifdef USE_FIRSTCLASS_RULE
//  Vector *insertion_events;
//#endif
//};

void slim::vm::RuleContext::clear_hl_spc() {
  HashIterator it;

  if (!hl_sameproccxt)
    return;

  for (it = hashtbl_iterator(hl_sameproccxt); !hashtbliter_isend(&it);
       hashtbliter_next(&it)) {
    SameProcCxt *spc = (SameProcCxt *)(hashtbliter_entry(&it)->data);
    delete spc;
  }

  hashtbl_free(hl_sameproccxt);
  hl_sameproccxt = nullptr;
}

BOOL rc_hlink_opt(LmnInstrVar atomi, LmnReactCxtRef rc) {
  /*  return hl_sameproccxtが初期化済み && atomiは同名プロセス文脈を持つアトム
   */
  return rc->get_hl_sameproccxt() &&
         hashtbl_contains(rc->get_hl_sameproccxt(), (HashKeyType)atomi);
}

void react_context_copy(LmnReactCxtRef to, LmnReactCxtRef from) { *to = *from; }

/*----------------------------------------------------------------------
 * ND React Context
 */

MCReactContext::MCReactContext(LmnMembrane *mem) : LmnReactCxt(mem, REACT_ND) {
    if (lmn_env.enable_por_old) {
      this->turnon_optmode(DynamicPartialOrderReduction_Naive);
    } else if (lmn_env.enable_por) {
      this->turnon_optmode(DynamicPartialOrderReduction);
    }

    if (lmn_env.d_compress) {
      this->turnon_optmode(BinaryStringDeltaCompress);
    }

    mem_delta_tmp = NULL;
    opt_mode = 0x00U;
    org_succ_num = 0;
    d_cur = 0;

    if (lmn_env.delta_mem) {
      this->turnon_optmode(DeltaMembrane);
    }

    if (lmn_env.enable_por && !lmn_env.enable_por_old) {
      por = DPOR_DATA();
    }
  }

void mc_react_cxt_add_expanded(MCReactContext *cxt, LmnMembraneRef mem,
                               LmnRuleRef rule) {
  cxt->push_expanded_state(mem);
  cxt->push_expanded_rule(rule);
}

void mc_react_cxt_add_mem_delta(MCReactContext *cxt, struct MemDeltaRoot *d,
                                LmnRuleRef rule) {
  cxt->push_mem_delta_root(d);
  cxt->push_expanded_rule(rule);
}

unsigned int mc_react_cxt_succ_num_org(LmnReactCxtRef cxt) {
  return RC_ND_ORG_SUCC_NUM(cxt);
}

unsigned int mc_react_cxt_expanded_num(MCReactContext *cxt) {
  return cxt->has_optmode(DeltaMembrane) ? cxt->get_mem_delta_roots().size()
                             : cxt->expanded_states().size();
}

///// first-class rulesets

struct LRCInsertEvent {
  LmnSymbolAtomRef satom;
  LmnMembraneRef mem;
};
typedef struct LRCInsertEvent *LRCInsertEventRef;

#ifdef USE_FIRSTCLASS_RULE
BOOL lmn_rc_has_insertion(LmnReactCxtRef rc) {
  return !rc->insertion_events->is_empty();
}

void lmn_rc_push_insertion(LmnReactCxtRef rc, LmnSymbolAtomRef satom,
                           LmnMembraneRef mem) {
  LMN_ASSERT(satom && mem);
  LRCInsertEventRef e = LMN_MALLOC(struct LRCInsertEvent);
  e->satom = satom;
  e->mem = mem;
  rc->insertion_events->push((LmnWord)e);
}
void lmn_rc_pop_insertion(LmnReactCxtRef rc, LmnSymbolAtomRef *satom,
                          LmnMembraneRef *mem) {
  LMN_ASSERT(lmn_rc_has_insertion(rc));
  LRCInsertEventRef e = (LRCInsertEventRef)rc->insertion_events->pop();
  *satom = e->satom;
  *mem = e->mem;
  LMN_FREE(e);
}

void lmn_rc_execute_insertion_events(LmnReactCxtRef rc) {
  while (lmn_rc_has_insertion(rc)) {
    LmnSymbolAtomRef satom;
    LmnMembraneRef mem;

    lmn_rc_pop_insertion(rc, &satom, &mem);

    LmnRuleSetRef rs = firstclass_ruleset_create(satom);
    LmnMembrane::add_firstclass_ruleset(mem, rs);
  }
}
#endif
