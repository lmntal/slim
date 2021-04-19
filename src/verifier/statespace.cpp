/*
 * statespace.cpp
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

/** @author Masato Gocho
 *  Closed Address Hash Table / Parallel Hash Table for State Management Table
 */
#include "statespace.h"
#include "../lmntal.h"
#include "automata.h"
#include "delta_membrane.h"
#include "mc.h"
#include "mem_encode.h"
#include "runtime_status.h"
#include "state.h"
#include "state.hpp"
#include "state_table.hpp"
#include "vm/vm.h"
#include "state_dumper.h"

namespace c14 = slim::element;

void StateSpace::make_table_pair(TablePair &t) {
  t.tbl = c14::make_unique<StateTable>(this->thread_num);
  if (this->has_property())
    t.acc = c14::make_unique<StateTable>(this->thread_num);
}

void StateSpace::make_table_pair(TablePair &t, TablePair &rehasher) {
  t.tbl = c14::make_unique<StateTable>(this->thread_num, rehasher.tbl.get());
  if (this->has_property())
    t.acc = c14::make_unique<StateTable>(this->thread_num, rehasher.acc.get());
}

/** StateSpace
 */
StateSpace::StateSpace(int thread_num, AutomataRef a, Vector *psyms)
    : using_memenc(false), is_formated(false), thread_num(thread_num),
      out(stdout), init_state(nullptr), property_automata(a), propsyms(psyms),
      end_states(thread_num) {
  if (lmn_env.mem_enc) {
    using_memenc = true;
    make_table_pair(memid_table);
  } else if (lmn_env.optimize_hash) {
    make_table_pair(memid_table);
    make_table_pair(mhash_table, memid_table);
  } else {
    make_table_pair(mhash_table);

    if (slim::config::profile && lmn_env.optimize_hash_old)
      make_table_pair(memid_table);
  }
}

StateSpace::~StateSpace() {}

/* 膜のIDを計算するハッシュ値(mhash)を追加する */
void StateSpace::add_memid_hash(unsigned long hash) {
  add_hash(hash);
  this->mhash_table.tbl->memid_rehash(hash);
}

std::unique_ptr<StateTable> &
StateSpace::insert_destination(State *s, unsigned long hashv) {
  if (s->is_encoded()) /* already calculated canonical binary strings */
    return state_is_accept(automata(), s) ? this->memid_table.acc
                                          : this->memid_table.tbl;

  if (slim::config::profile && lmn_env.optimize_hash_old &&
      !lmn_env.tree_compress && contains_hash(hashv))
    return state_is_accept(automata(), s) ? this->memid_table.acc
                                          : this->memid_table.tbl;

  /* default */
  return state_is_accept(automata(), s) ? this->mhash_table.acc
                                        : this->mhash_table.tbl;
}

std::unique_ptr<StateTable> &
StateSpace::resize_destination(std::unique_ptr<StateTable> &def, State *ret,
                               State *s) {
  /* rehasherが機能した場合, 通常のテーブルを入り口に,
   * memidテーブルにエントリが追加されている
   * なにも考慮せずにテーブル拡張の判定を行ってしまうと,
   * memidテーブルが定数サイズになってしまう. 判定を適切に行うため,
   * テーブルへのポインタを切り替える */
  if (ret->is_encoded())
    return (state_is_accept(this->automata(), s)) ? this->memid_table.acc
                                                  : this->memid_table.tbl;

  return def;
}

/* 状態sが状態空間ssに既出ならばその状態を, 新規ならばs自身を返す.
 * 状態が追加される場合, 状態sに対応する階層グラフ構造s_memを,
 * バイナリストリングへエンコードし, 状態sに追加する. このとき,
 * 状態sのメモリ領域を階層グラフ構造とバイナリストリングとでunionしているため,
 * 本関数の呼び出し側でs_memのメモリ管理を行う必要がある.
 * なお, 既にsのバイナリストリングを計算済みの場合,
 * バイナリストリングへのエンコード処理はskipするため, s_memはNULLで構わない. */
State *StateSpace::insert(State *s) {
  State *ret;
  unsigned long col = 0;
  unsigned long hashv = state_hash(s);

  auto &insert_dst = insert_destination(s, hashv);
  if (slim::config::profile && lmn_env.optimize_hash_old &&
      !lmn_env.tree_compress && !s->is_expanded() && contains_hash(hashv)) {
    s->calc_mem_encode();
  }

  if (slim::config::profile) {
    ret = insert_dst->insert(s, &col);
  } else {
    ret = insert_dst->insert(s);
  }

  if (slim::config::profile && lmn_env.optimize_hash_old &&
      !lmn_env.tree_compress && col >= MEM_EQ_FAIL_THRESHOLD) {
    this->add_memid_hash(hashv);
  }

  auto &resize_tbl = resize_destination(insert_dst, ret, s);
  resize_tbl->resize_if_needed();

  return ret;
}

/* 差分オブジェクトdを用いて状態sの状態データを構築してからstatespace_insertを行う.
 * statespace_insertに必要な条件を揃えるために行うdelta-membrane用wrapper関数.
 *
 * スレッドセーフ条件:
 *
 *   [parent]<--+   [s]
 *     |        |    |
 *     +->mem   +----+
 *
 *   状態sの生成元状態parentのグラフ構造memを,
 * 差分オブジェクトdが刺しており(d->mem),
 *   d->memをTLSとして扱う前提が守られていれば, d->memに対する操作は全てMT-safe
 */
State *StateSpace::insert_delta(State *s, struct MemDeltaRoot *d) {
  State *ret;

  /* d->mem (parentに対応する階層グラフ構造)を,
   * sに対応する階層グラフ構造Xへ書換え */
  dmem_root_commit(d);
  s->state_set_mem(DMEM_ROOT_MEM(d));

  /* Xを基に, ハッシュ値/mem_idなどの状態データを計算する */
  s->state_calc_hash(s->state_mem(), this->use_memenc());

  /* 既にバイナリストリング計算済みとなるcanonical membrane使用時は,
   * この時点でdelta-stringを計算する */
  if (s->is_encoded() && s->s_is_d()) {
    s->calc_binstr_delta();
  }

  ret = this->insert(s);

  /* X(sに対応する階層グラフ構造)をparentに対応する階層グラフ構造に戻す */
  dmem_root_revert(d);

  /* LmnMembraneとLmnBinStrのデータ領域を統合(@rev.458)したため, NULL設定に注意
   */
  if (!s->state_binstr()) {
    s->state_set_mem(NULL);
  }

  return ret;
}

/* 重複検査や排他制御なしに状態sを状態表ssに登録する */
void StateSpace::add_direct(State *s) {
  auto &add_dst =
      (s->is_encoded()) ? this->memid_table.tbl : this->mhash_table.tbl;
  add_dst->add_direct(s);
  add_dst->resize_if_needed();
}

/* 高階関数 */
std::vector<State *> StateSpace::all_states() const {
  std::vector<State *> result;

  // 何故かstd::copyを使うといくつかの状態がコピーされないのでforを使う
  // TODO: おそらくStateTableのイテレータがおかしいので調査する
  if (this->mhash_table.tbl) {
    for (auto s : *mhash_table.tbl)
      result.push_back(s);
    // std::copy(this->mhash_table.tbl->begin(), this->mhash_table.tbl->end(),
    //           result.begin());
  }
  if (this->memid_table.tbl) {
    for (auto s : *memid_table.tbl)
      result.push_back(s);
    // std::copy(this->memid_table.tbl->begin(), this->memid_table.tbl->end(),
    //           std::back_inserter(result));
  }
  if (this->mhash_table.acc) {
    for (auto s : *mhash_table.acc)
      result.push_back(s);
    // std::copy(this->mhash_table.acc->begin(), this->mhash_table.acc->end(),
    //           std::back_inserter(result));
  }
  if (this->memid_table.acc) {
    for (auto s : *memid_table.acc)
      result.push_back(s);
    // std::copy(this->memid_table.acc->begin(), this->memid_table.acc->end(),
    //           std::back_inserter(result));
  }
  return result;
}

/* 初期状態を追加する MT-UNSAFE */
void StateSpace::set_init_state(State *init_state) {
  this->init_state = init_state;
  this->add_direct(init_state);
}

/* 状態数を返す */

unsigned long StateSpace::num() const {
  return (this->num_raw() - this->dummy_num());
}

/* dummyの状態数を含む, 管理している状態数を返す */
unsigned long StateSpace::num_raw() const {
  return (this->mhash_table.tbl ? this->mhash_table.tbl->all_num() : 0) +
         (this->memid_table.tbl ? this->memid_table.tbl->all_num() : 0) +
         (this->mhash_table.acc ? this->mhash_table.acc->all_num() : 0) +
         (this->memid_table.acc ? this->memid_table.acc->all_num() : 0);
}

/* memidテーブルに追加されているdummy状態数を返す */
unsigned long StateSpace::dummy_num() const {
  StateTable *tbl;
  unsigned long ret;
  unsigned int i;

  ret = 0UL;
  if (this->memid_table.tbl) {
    ret += this->memid_table.tbl->all_num_dummy();
  }
  if (this->memid_table.acc) {
    ret += this->memid_table.acc->all_num_dummy();
  }
  return ret;
}

/* 最終状態数を返す */
unsigned long StateSpace::num_of_ends() const {
  unsigned long sum = 0;
  for (const auto &e : end_states)
    sum += e.size();
  return sum;
}

/* 状態空間に**すでに含まれている**状態sを最終状態として登録する */
void StateSpace::mark_as_end(State *s) {
  LMN_ASSERT(env_my_thread_id() < env_threads_num());
  if (this->thread_num > 1)
    this->end_states[env_my_thread_id()].push_back(s);
  else
    this->end_states[0].push_back(s);
}

unsigned long StateSpace::space() const {
  unsigned long ret = sizeof(struct StateSpace);
  if (this->mhash_table.tbl) {
    ret += this->mhash_table.tbl->space();
  }
  if (this->memid_table.tbl) {
    ret += this->memid_table.tbl->space();
  }
  if (this->mhash_table.acc) {
    ret += this->mhash_table.acc->space();
  }
  if (this->memid_table.acc) {
    ret += this->memid_table.acc->space();
  }
  for (const auto &e : end_states)
    ret += e.capacity() * sizeof(e.front());
  return ret;
}

/** Printer et al
 */

void StateSpace::dump_ends() const {
  auto dumper = StateDumper::from_env(this->out);
  for (const auto &end_i : end_states) {
    for (const auto &p : end_i) {
      dumper->state_print_mem(p);
      if (lmn_env.sp_dump_format == LMN_SYNTAX)
        printf(".\n");
    }
  }
}

void StateSpace::dump() {
  StateDumper::from_env(this->out)->dump(this);
}

/* 注: 出力用に, リンクリストの先頭の状態のIDで,
 * ハッシュ表であることを無視した配置へ整列する. ただし,
 * リンクリストを構成する状態の整列はしない. (修正前の処理は,
 * 状態数分の配列をmallocしてから処理するものであったが, これによるlarge
 * mallocがメモリswapを発生させていた. この方式は, メモリswapをさせない, かつ,
 * ある程度の整列結果を得ることを目的としている) */
void StateSpace::format_states() {
#ifndef __CYGWIN__
  /* cygwinテスト時に, ボトルネックになっていた */
  if (!this->is_formated && lmn_env.sp_dump_format != INCREMENTAL) {
    if (this->mhash_table.tbl) this->mhash_table.tbl->format_states();
    if (this->mhash_table.acc) this->mhash_table.acc->format_states();
    if (this->memid_table.tbl) this->memid_table.tbl->format_states();
    if (this->memid_table.acc) this->memid_table.acc->format_states();
    this->is_formated = TRUE;
  }
#endif
}
