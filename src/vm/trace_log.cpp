/*
 * trace_log.cpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 */

#include "trace_log.h"

template <> const TraceData ProcessTable<TraceData>::unused = {0, 0, 0, 0};

/*----------------
 * Tracker
 */

/* 膜ownerを対象として訪問済みにしたプロセス (シンボルアトム + 子膜 + inside
 * proxies) の数が 膜ownerのそれと一致しているか否かを返す */
BOOL tracelog_eq_traversed_proc_num(TraceLogRef l, LmnMembraneRef owner,
                                    AtomListEntryRef in_ent,
                                    AtomListEntryRef avoid) {
  return l->eq_traversed_proc_num(owner, in_ent, avoid);
}

/* ログlに, 所属膜owner1のアトムatom1への訪問を記録する.
 * atom1にマッチしたアトムのプロセスIDもしくは訪問番号atom2_idを併せて記録する.
 */
int tracelog_put_atom(TraceLogRef l, LmnSymbolAtomRef atom1, LmnWord atom2_id,
                      LmnMembraneRef owner1) {
  return l->visit(atom1, atom2_id, owner1);
}

/* ログlに, 膜mem1への訪問を記録する. (所属膜はmem1のメンバから参照するため不要)
 * mem1にマッチした膜のプロセスIDもしくは訪問番号mem2_idを併せて記録する */
int tracelog_put_mem(TraceLogRef l, LmnMembraneRef mem1, LmnWord mem2_id) {
  return l->visit(mem1, mem2_id);
}

/* ログlに, ハイパーグラフのルートオブジェクトhl1への訪問を記録する.
 * (ハイパーグラフ構造には所属膜の概念がなく,
 * 膜オブジェクトからの参照もできないため, 所属膜に対する一切の操作は不要)
 * hl1にマッチしたハイパリンクオブジェクトIDもしくは訪問番号hl2_idを併せて記録する
 */
int tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id) {
  return l->visit(hl1, hl2_id);
}

void tracelog_unput(TraceLogRef l, LmnWord key) { l->leave(key); }

BOOL tracelog_contains(TraceLogRef l, LmnWord key) { return l->contains(key); }

BOOL tracelog_contains_atom(TraceLogRef l, LmnSymbolAtomRef atom) {
  return l->contains(atom->get_id());
}

BOOL tracelog_contains_mem(TraceLogRef l, LmnMembraneRef mem) {
  return l->contains(lmn_mem_id(mem));
}

BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) {
  return l->contains(LMN_HL_ID(hl));
}

LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key) {
  TraceData d;
  l->get(key, &d);
  return d.matched;
}

LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSymbolAtomRef atom) {
  return tracelog_get_matched(l, atom->get_id());
}

LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembraneRef mem) {
  return tracelog_get_matched(l, lmn_mem_id(mem));
}

LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl) {
  return tracelog_get_matched(l, LMN_HL_ID(hl));
}

BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key) {
  TraceData d;
  l->get(key, &d);
  return d.flag;
}

void tracelog_backtrack(TraceLogRef l) { l->backtrack(); }

void tracelog_set_btpoint(TraceLogRef l) { l->set_btpoint(); }

void tracelog_continue_trace(TraceLogRef l) { l->continue_trace(); }
