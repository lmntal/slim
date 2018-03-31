/*
 * trace_log.c
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 */

#include "trace_log.h"


#include "process_table.hpp"


/* ----
 * TraceLog
 * グラフ同形成判定用データ構造本体
 */

/* 各{シンボルアトム, inside proxyアトム, 膜}に対して1つずつ対応させるデータ構造
 * outside proxyアトムは含めない */
struct TraceData { /* 64bit: 24Bytes (32bit: 16Bytes) */
  BYTE flag;                   /* 対応させているデータの種類を示すフラグ */
  unsigned int traversed_proc; /* 膜に対応させている場合は, その膜内で訪問した
                                   {シンボルアトム, inside proxyアトム, 子膜}の総数
                                  を記録している.
                                  他のデータに対応させている場合は0のまま */
  ProcessID owner_id;          /* 対応させているデータ構造の所属膜のID.
                                * プロセスIDは1から始まるため,
                                * 所属膜がない(例えばグローバルルート膜の)場合は, 0 */
  ProcessID matched;           /* 対応させているプロセスとマッチさせたプロセスのID.
                                * in-proxyアトムはBS encode時の訪問順序に数えないため,
                                * in-proxyアトムへの対応としては0をセット */

  bool operator==(const TraceData &a) const {
    return a.flag == flag && a.traversed_proc == traversed_proc &&
        a.owner_id == owner_id && a.matched == matched;
  }
  bool operator!=(const TraceData &a) const {
    return !(*this == a);
  }

  enum options {
    TRAVERSED_ATOM   = 0x1U,
    TRAVERSED_MEM    = 0x2U,
    TRAVERSED_HLINK  = 0x3U,
    TRAVERSED_OTHERS = 0xfU,
  };
};


template<>
const TraceData ProcessTable<TraceData>::unused = {0, 0, 0, 0};

struct TraceLog : ProcessTable<TraceData> {
  struct LogTracker tracker;

  TraceLog(unsigned long size) : ProcessTable<TraceData>(size) {
    tracker_init(&this->tracker);
  }

  TraceLog() : ProcessTable<TraceData>() {
    tracker_init(&this->tracker);
  }

  ~TraceLog() {
    tracker_destroy(&this->tracker);
  }

  unsigned int traversed_proc_count(LmnMembraneRef owner) {
    return this->contains(lmn_mem_id(owner)) ? (*this)[lmn_mem_id(owner)].traversed_proc : 0;
  }
  bool eq_traversed_proc_num(LmnMembraneRef owner, AtomListEntryRef in_ent, AtomListEntryRef avoid) {
    return traversed_proc_count(owner) ==
          (lmn_mem_symb_atom_num(owner)
              + lmn_mem_child_mem_num(owner)
              + atomlist_get_entries_num(in_ent)
              - atomlist_get_entries_num(avoid));
  }

private:
  /* ログl上のキーkey位置にあるTraceLogに対して, 訪問を記録する.
   * matched_idはマッチしたプロセスのID, ownerは所属膜.
   * 所属膜がNULLでない場合は, 所属膜の情報を記録し, 所属膜側のプロセス訪問カウンタを回す.
   */
  bool visit(LmnWord key, BYTE flag, LmnWord matched_id, LmnMembraneRef owner) {
    if (this->contains(key)) {
      return false;
    }

    TraceData value = {flag, 0, 0, matched_id};

    if (owner) {
      value.owner_id = lmn_mem_id(owner);

      TraceData dat;
      this->get(lmn_mem_id(owner), dat);
      dat.traversed_proc++;
      this->put(lmn_mem_id(owner), dat);
    }

    this->put(key, value);

    LogTracker_TRACE(&this->tracker, key);

    return true;
  }

public:
  bool visit(LmnSymbolAtomRef atom, LmnWord atom2_id, LmnMembraneRef owner) {
    return this->visit(LMN_SATOM_ID(atom), TraceData::options::TRAVERSED_ATOM, atom2_id, owner);
  }

  bool visit(LmnMembraneRef mem1, LmnWord mem2_id) {
    return this->visit(lmn_mem_id(mem1), TraceData::options::TRAVERSED_MEM, mem2_id, lmn_mem_parent(mem1));
  }

  bool visit(HyperLink *hl1, LmnWord hl2_id) {
    return this->visit(LMN_HL_ID(hl1), TraceData::options::TRAVERSED_HLINK, hl2_id, NULL);
  }

  void leave(key_type key) {
    const auto owner_id = (*this)[key].owner_id;

    TraceData dat;
    this->get(owner_id, dat);
    dat.traversed_proc--;
    this->put(owner_id, dat);

    this->unput(key);
  }

  void backtrack() {
    LogTracker_REVERT(&this->tracker, tracelog_unput, this);
  }

  void set_btpoint() {
    LogTracker_PUSH(&this->tracker);
  }

  void continue_trace() {
    LogTracker_POP(&this->tracker);
  }
};

/*------------
 * TraceLog
 */


TraceLogRef tracelog_make(void)
{
  return new TraceLog();
}

TraceLogRef tracelog_make_with_size(unsigned long size)
{
  return new TraceLog(size);
}

void tracelog_free(TraceLogRef l)
{
  delete l;
}

/*----------------
 * Tracker
 */

void tracker_init(struct LogTracker *track)
{
  vec_init(&track->traced_ids, 128);
  vec_init(&track->btp_idx, 128);
}

void tracker_destroy(struct LogTracker *track)
{
  vec_destroy(&track->traced_ids);
  vec_destroy(&track->btp_idx);
}



/* 膜ownerを対象として訪問済みにしたプロセス (シンボルアトム + 子膜 + inside proxies) の数が
 * 膜ownerのそれと一致しているか否かを返す */
BOOL tracelog_eq_traversed_proc_num(TraceLogRef      l,
                                    LmnMembraneRef   owner,
                                    AtomListEntryRef in_ent,
                                    AtomListEntryRef avoid)
{
  return l->eq_traversed_proc_num(owner, in_ent, avoid);
}


/* ログlに, 所属膜owner1のアトムatom1への訪問を記録する.
 * atom1にマッチしたアトムのプロセスIDもしくは訪問番号atom2_idを併せて記録する. */
int tracelog_put_atom(TraceLogRef l, LmnSymbolAtomRef atom1, LmnWord  atom2_id,
                                    LmnMembraneRef owner1) {
  return l->visit(atom1, atom2_id, owner1);
}

/* ログlに, 膜mem1への訪問を記録する. (所属膜はmem1のメンバから参照するため不要)
 * mem1にマッチした膜のプロセスIDもしくは訪問番号mem2_idを併せて記録する */
int tracelog_put_mem(TraceLogRef l, LmnMembraneRef mem1, LmnWord mem2_id) {
  return l->visit(mem1, mem2_id);
}

/* ログlに, ハイパーグラフのルートオブジェクトhl1への訪問を記録する.
 * (ハイパーグラフ構造には所属膜の概念がなく, 膜オブジェクトからの参照もできないため,
 *  所属膜に対する一切の操作は不要)
 * hl1にマッチしたハイパリンクオブジェクトIDもしくは訪問番号hl2_idを併せて記録する */
int tracelog_put_hlink(TraceLogRef l, HyperLink *hl1, LmnWord hl2_id) {
  return l->visit(hl1, hl2_id);
}

void tracelog_unput(TraceLogRef l, LmnWord key) {
  l->leave(key);
}

BOOL tracelog_contains(TraceLogRef l, LmnWord key) {
  return l->contains(key);
}

BOOL tracelog_contains_atom(TraceLogRef l, LmnSymbolAtomRef atom) {
  return l->contains(LMN_SATOM_ID(atom));
}

BOOL tracelog_contains_mem(TraceLogRef l, LmnMembraneRef mem) {
  return l->contains(lmn_mem_id(mem));
}

BOOL tracelog_contains_hlink(TraceLogRef l, HyperLink *hl) {
  return l->contains(LMN_HL_ID(hl));
}

LmnWord tracelog_get_matched(TraceLogRef l, LmnWord key) {
  TraceData d;
  l->get(key, d);
  return d.matched;
}

LmnWord tracelog_get_atomMatched(TraceLogRef l, LmnSymbolAtomRef atom) {
  return tracelog_get_matched(l, LMN_SATOM_ID(atom));
}

LmnWord tracelog_get_memMatched(TraceLogRef l, LmnMembraneRef mem) {
  return tracelog_get_matched(l, lmn_mem_id(mem));
}

LmnWord tracelog_get_hlinkMatched(TraceLogRef l, HyperLink *hl) {
  return tracelog_get_matched(l, LMN_HL_ID(hl));
}

BYTE tracelog_get_matchedFlag(TraceLogRef l, LmnWord key) {
  TraceData d;
  l->get(key, d);
  return d.flag;
}

void tracelog_backtrack(TraceLogRef l) {
  l->backtrack();
}

void tracelog_set_btpoint(TraceLogRef l) {
  l->set_btpoint();
}

void tracelog_continue_trace(TraceLogRef l) {
  l->continue_trace();
}
