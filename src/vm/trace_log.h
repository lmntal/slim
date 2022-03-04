/*
 * trace_log.h
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

#ifndef LMN_TRACE_LOG_H
#define LMN_TRACE_LOG_H

#include "element/element.h"
#include "membrane.h"
/*----------------------------------------------------------------------
 * TraceLog
 * --------
 * - struct LogTracker
 * - struct TraceLog
 * - struct SimpleTraceLog
 *
 * VisitLogの改良版.
 * データ構造をグラフ同形成判定に特化させたため,
 * VisitLogと比較すると汎用性は低め.
 */

/* ---
 * Tracker周り
 * 訪問済みテーブルの状態をバックトラックさせるためのデータ構造
 */

#include <stack>
#include <unordered_map>

#include "atomlist.hpp"
#include "membrane.hpp"
#include "process_table.hpp"

class LogTracker {
  std::stack<ProcessID> traced_ids;
  std::stack<std::size_t> btp_idx;

 public:
  void push() {
    btp_idx.push(traced_ids.size());
  }

  void pop() {
    btp_idx.pop();
  }

  void trace(ProcessID id) {
    traced_ids.push(id);
  }

  /* 前回のトレースプロセス数を基に, ログを巻き戻す. */
  template<typename T>
  void revert(T table) {
    /* 前回セットしたトレース数のメモを取り出す */
    const auto bt_idx = btp_idx.top();
    btp_idx.pop();
    /* bt_idxより上のidxに積まれたデータをログ上からunputしていく */
    while (traced_ids.size() > bt_idx) {
      auto key = traced_ids.top();
      traced_ids.pop();
      table->leave(key);
    }
  }
};

/* ----
 * TraceLog
 * グラフ同形成判定用データ構造本体
 */

/* 各{シンボルアトム, inside proxyアトム, 膜}に対して1つずつ対応させるデータ構造
 * outside proxyアトムは含めない */
struct TraceData {             /* 64bit: 24Bytes (32bit: 16Bytes) */
  BYTE flag;                   /* 対応させているデータの種類を示すフラグ */
  unsigned int traversed_proc; /* 膜に対応させている場合は, その膜内で訪問した
                                   {シンボルアトム, inside proxyアトム,
                                  子膜}の総数 を記録している.
                                  他のデータに対応させている場合は0のまま */
  ProcessID owner_id;          /* 対応させているデータ構造の所属膜のID.
                                * プロセスIDは1から始まるため,
                                * 所属膜がない(例えばグローバルルート膜の)場合は, 0 */
  ProcessID matched; /* 対応させているプロセスとマッチさせたプロセスのID.
                      * in-proxyアトムはBS encode時の訪問順序に数えないため,
                      * in-proxyアトムへの対応としては0をセット */

  TraceData() : flag(0), traversed_proc(0), owner_id(0), matched(0) {
  }
  TraceData(BYTE flag, unsigned int traversed_proc, ProcessID owner_id, ProcessID matched)
      : flag(flag),
        traversed_proc(traversed_proc),
        owner_id(owner_id),
        matched(matched) {
  }

  bool operator==(const TraceData &a) const {
    return a.flag == flag && a.traversed_proc == traversed_proc && a.owner_id == owner_id &&
           a.matched == matched;
  }
  bool operator!=(const TraceData &a) const {
    return !(*this == a);
  }

  enum options {
    TRAVERSED_ATOM = 0x1U,
    TRAVERSED_MEM = 0x2U,
    TRAVERSED_HLINK = 0x3U,
    TRAVERSED_OTHERS = 0xfU,
  };
};

struct TraceLog {
  std::unordered_map<ProcessID, TraceData> table;
  LogTracker tracker;

  using key_type = ProcessTable<TraceData>::key_type;

  TraceLog(unsigned long size) {
  }
  TraceLog() {
  }
  unsigned int traversed_proc_count(LmnMembraneRef owner) {
    return table.find(owner->mem_id()) != table.end() ? table[owner->mem_id()].traversed_proc : 0;
  }

  /* 膜ownerを対象として訪問済みにしたプロセス (シンボルアトム + 子膜 + inside
   * proxies) の数が 膜ownerのそれと一致しているか否かを返す */
  bool
  eq_traversed_proc_num(LmnMembraneRef owner, AtomListEntryRef in_ent, AtomListEntryRef avoid) {
    size_t s1 = in_ent ? in_ent->size() : 0;
    size_t s2 = avoid ? avoid->size() : 0;
    return traversed_proc_count(owner) ==
           (owner->symb_atom_num() + owner->child_mem_num() + s1 - s2);
  }

 private:
  /* ログl上のキーkey位置にあるTraceLogに対して, 訪問を記録する.
   * matched_idはマッチしたプロセスのID, ownerは所属膜.
   * 所属膜がNULLでない場合は, 所属膜の情報を記録し,
   * 所属膜側のプロセス訪問カウンタを回す.
   */
  bool visit(LmnWord key, BYTE flag, LmnWord matched_id, LmnMembraneRef owner) {
    if (table.find(key) != table.end()) {
      return false;
    }

    TraceData value = { flag, 0, 0, matched_id };

    if (owner) {
      value.owner_id = owner->mem_id();
      table[owner->mem_id()].traversed_proc++;
    }

    table.insert(std::make_pair(key, value));

    tracker.trace(key);

    return true;
  }

 public:
  /* ログに, 所属膜ownerのアトムatomへの訪問を記録する.
   * atomにマッチしたアトムのプロセスIDもしくは訪問番号atom2_idを併せて記録する.
   */
  bool visit_atom(LmnSymbolAtomRef atom, LmnWord atom2_id, LmnMembraneRef owner) {
    return this->visit(atom->get_id(), TraceData::options::TRAVERSED_ATOM, atom2_id, owner);
  }

  /* ログに, 膜mem1への訪問を記録する.
   * (所属膜はmem1のメンバから参照するため不要)
   * mem1にマッチした膜のプロセスIDもしくは訪問番号mem2_idを併せて記録する */
  bool visit_mem(LmnMembraneRef mem1, LmnWord mem2_id) {
    return this->visit(
        mem1->mem_id(), TraceData::options::TRAVERSED_MEM, mem2_id, mem1->mem_parent());
  }

  /* ログに, ハイパーグラフのルートオブジェクトhl1への訪問を記録する.
   * (ハイパーグラフ構造には所属膜の概念がなく,
   * 膜オブジェクトからの参照もできないため, 所属膜に対する一切の操作は不要)
   * hl1にマッチしたハイパリンクオブジェクトIDもしくは訪問番号hl2_idを併せて記録する
   */
  bool visit_hlink(HyperLink *hl1, LmnWord hl2_id) {
    return this->visit(LMN_HL_ID(hl1), TraceData::options::TRAVERSED_HLINK, hl2_id, NULL);
  }

  void leave(key_type key) {
    const auto owner_id = table[key].owner_id;
    table[owner_id].traversed_proc--;
    if (table[owner_id] == TraceData())
      table.erase(owner_id);
    table.erase(key);
  }

  bool contains(key_type key) const {
    return table.find(key) != table.end();
  }

  void backtrack() {
    tracker.revert(this);
  }

  void set_btpoint() {
    tracker.push();
  }

  void continue_trace() {
    tracker.pop();
  }
};

typedef struct TraceLog *TraceLogRef;

#define TLOG_MATCHED_ID_NONE (0U)

/**
 * Function ProtoTypes
 */

int tracelog_put_atom(
    TraceLogRef l,
    LmnSymbolAtomRef atom1,
    LmnWord atom2_id,
    LmnMembraneRef owner1);
int tracelog_put_mem(TraceLogRef l, LmnMembraneRef mem1, LmnWord mem2_id);
int tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id);
void tracelog_unput(TraceLogRef l, LmnWord key);
BOOL tracelog_contains(TraceLogRef l, LmnWord key);
BOOL tracelog_contains_atom(TraceLogRef l, LmnSymbolAtomRef atom);
BOOL tracelog_contains_mem(TraceLogRef l, LmnMembraneRef mem);
BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl);
LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key);
LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSymbolAtomRef atom);
LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembraneRef mem);
LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl);
void tracelog_backtrack(TraceLogRef l);
void tracelog_set_btpoint(TraceLogRef l);
void tracelog_continue_trace(TraceLogRef l);

#endif /* LMN_TRACE_LOG_H */
