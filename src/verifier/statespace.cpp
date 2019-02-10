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


void StateSpace::make_table() {
  if (lmn_env.mem_enc) {
    this->set_memenc();
    this->memid_tbl = new StateTable(this->thread_num);

    if (this->has_property()) {
      this->acc_memid_tbl = new StateTable(this->thread_num);
    }
  } else {
    this->tbl = new StateTable(this->thread_num);

    if (this->has_property()) {
      this->acc_tbl = new StateTable(this->thread_num);
    }

    if (lmn_env.optimize_hash) {
      this->set_rehasher();
      this->memid_tbl = new StateTable(this->thread_num);
      this->tbl->set_rehash_table(this->memid_tbl);
      if (this->acc_tbl) {
        this->acc_memid_tbl = new StateTable(this->thread_num);
        this->acc_tbl->set_rehash_table(this->acc_memid_tbl);
      }
    }
  }

  if (slim::config::profile && lmn_env.optimize_hash_old) {
    if (!this->memid_tbl) {
      this->memid_tbl = new StateTable(this->thread_num);
    }
    if (this->acc_tbl) {
      this->acc_memid_tbl = new StateTable(this->thread_num);
    }
    hashset_init(&this->memid_hashes, 128);
  }
}

#define TABLE_DEFAULT_MAX_DENSITY                                              \
  (5U) /* 1バケットあたりの平均長がこの値を越えた場合にresizeする */
#define MEM_EQ_FAIL_THRESHOLD                                                  \
  (2U) /* 膜の同型性判定にこの回数以上失敗すると膜のエンコードを行う */

/** Macros
 */

#define need_resize(EntryNum, Capacity)                                        \
  (((EntryNum) / (Capacity)) > TABLE_DEFAULT_MAX_DENSITY)

/** StateSpace
 */
StateSpace::StateSpace() {
  tbl_type = 0x00U;
  is_formated = FALSE;
  thread_num = 1;
  out = stdout; /* TOFIX: LmnPortで書き直したいところ */
  init_state = NULL;
  end_states = NULL;
  tbl = NULL;
  memid_tbl = NULL;
  acc_tbl = NULL;
  acc_memid_tbl = NULL;
  property_automata = NULL;
  propsyms = NULL;
}

StateSpace::~StateSpace() {
  /* MEMO: openmpで並列freeすると, tcmallocがsegmentation faultする. */
  // int nPEs = ss->thread_num;
  delete this->tbl;
  delete this->memid_tbl;
  delete this->acc_tbl;
  delete this->acc_memid_tbl;

  if (this->thread_num > 1) {
    for (int i = 0; i < this->thread_num; i++) {
      vec_destroy(&this->end_states[i]);
    }
    LMN_FREE(this->end_states);
  } else {
    vec_free(this->end_states);
  }

#ifdef PROFILE
  if (lmn_env.optimize_hash_old) {
    hashset_destroy(&this->memid_hashes);
  }
#endif
}

#ifdef PROFILE

/* 膜のIDを計算するハッシュ値(mhash)を追加する */
void StateSpace::add_memid_hash(unsigned long hash) {
  State *ptr;
  StateTable *org;
  unsigned long i;

  org = this->tbl;
  hashset_add(&this->memid_hashes, hash);
  org->memid_rehash(hash);
}
#endif

/* 状態sが状態空間ssに既出ならばその状態を, 新規ならばs自身を返す.
 * 状態が追加される場合, 状態sに対応する階層グラフ構造s_memを,
 * バイナリストリングへエンコードし, 状態sに追加する. このとき,
 * 状態sのメモリ領域を階層グラフ構造とバイナリストリングとでunionしているため,
 * 本関数の呼び出し側でs_memのメモリ管理を行う必要がある.
 * なお, 既にsのバイナリストリングを計算済みの場合,
 * バイナリストリングへのエンコード処理はskipするため, s_memはNULLで構わない. */
State *StateSpace::insert(State *s) {
  StateTable *insert_dst;
  State *ret;
  BOOL is_accept;
#ifdef PROFILE
  unsigned long col;
  unsigned long hashv;
  col = 0;
  hashv = state_hash(s);
#endif

  is_accept = this->has_property() && state_is_accept(this->automata(), s);

  if (s->is_encoded()) {
    /* already calculated canonical binary strings */
    if (is_accept) {
      insert_dst = this->acc_memid_tbl;
    } else {
      insert_dst = this->memid_tbl;
    }
  } else {
    /* default */
    if (is_accept) {
      insert_dst = this->acc_tbl;
    } else {
      insert_dst = this->tbl;
    }
#ifdef PROFILE
    if (lmn_env.optimize_hash_old && this->is_memid_hash(hashv) &&
        lmn_env.tree_compress == FALSE) {
      s->calc_mem_encode();
      insert_dst = is_accept ? this->acc_memid_tbl : this->memid_tbl;
    }
#endif
  }

#ifndef PROFILE
  ret = insert_dst->insert(s);
#else
  ret = insert_dst->insert(s, &col);
  if (lmn_env.optimize_hash_old && col >= MEM_EQ_FAIL_THRESHOLD &&
      lmn_env.tree_compress == FALSE) {
    this->add_memid_hash(hashv);
  }
#endif

  if (ret->is_encoded()) {
    /* rehasherが機能した場合, 通常のテーブルを入り口に,
     * memidテーブルにエントリが追加されている
     * なにも考慮せずにテーブル拡張の判定を行ってしまうと,
     * memidテーブルが定数サイズになってしまう. 判定を適切に行うため,
     * テーブルへのポインタを切り替える */
    if (is_accept) {
      insert_dst = this->acc_memid_tbl;
    } else {
      insert_dst = this->memid_tbl;
    }
  }

  if (need_resize(insert_dst->num_by_me(),
                  insert_dst->cap_density())) { /* tableのresize処理 */
    insert_dst->resize(insert_dst->cap());
  }

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
  StateTable *add_dst;

  if (s->is_encoded()) {
    add_dst = this->memid_tbl;
  } else {
    add_dst = this->tbl;
  }

  add_dst->add_direct(s);

  if (need_resize(add_dst->num_by_me(), add_dst->cap_density())) {
    add_dst->resize(add_dst->cap());
  }
}

/* 高階関数 */
std::vector<State *> StateSpace::all_states() const {
  std::vector<State *> result;
  if (this->tbl)
    std::copy(this->tbl->begin(), this->tbl->end(), std::back_inserter(result));
  if (this->memid_tbl)
    std::copy(this->memid_tbl->begin(), this->memid_tbl->end(),
              std::back_inserter(result));
  if (this->acc_tbl)
    std::copy(this->acc_tbl->begin(), this->acc_tbl->end(),
              std::back_inserter(result));
  if (this->acc_memid_tbl)
    std::copy(this->acc_memid_tbl->begin(), this->acc_memid_tbl->end(),
              std::back_inserter(result));
  return result;
}

/* 初期状態を追加する MT-UNSAFE */
void StateSpace::set_init_state(State *init_state, BOOL enable_binstr) {
  this->init_state = init_state;
  this->add_direct(init_state);
  if (enable_binstr) {
    init_state->free_mem();
  }
}

/* 状態数を返す */

unsigned long StateSpace::num() {
  return (this->num_raw() - this->dummy_num());
}

/* dummyの状態数を含む, 管理している状態数を返す */
unsigned long StateSpace::num_raw() {
  return (this->tbl ? this->tbl->all_num() : 0) +
         (this->memid_tbl ? this->memid_tbl->all_num() : 0) +
         (this->acc_tbl ? this->acc_tbl->all_num() : 0) +
         (this->acc_memid_tbl ? this->acc_memid_tbl->all_num() : 0);
}

/* memidテーブルに追加されているdummy状態数を返す */
unsigned long StateSpace::dummy_num() {
  StateTable *tbl;
  unsigned long ret;
  unsigned int i;

  ret = 0UL;
  tbl = this->memid_tbl;
  if (tbl) {
    ret += tbl->all_num_dummy();
  }

  tbl = this->acc_memid_tbl;
  if (tbl) {
    ret += tbl->all_num_dummy();
  }
  return ret;
}

/* 最終状態数を返す */
unsigned long StateSpace::num_of_ends() const {
  if (this->thread_num > 1) {
    unsigned long sum = 0;
    unsigned int i;
    for (i = 0; i < this->thread_num; i++) {
      sum += vec_num(&this->end_states[i]);
    }
    return sum;

  } else {
    return vec_num(this->end_states);
  }
}

/* 状態空間に**すでに含まれている**状態sを最終状態として登録する */
void StateSpace::mark_as_end(State *s) {
  LMN_ASSERT(env_my_thread_id() < env_threads_num());
  if (this->thread_num > 1)
    vec_push(&this->end_states[env_my_thread_id()], (vec_data_t)s);
  else
    vec_push(this->end_states, (vec_data_t)s);
}

unsigned long StateSpace::space() {
  unsigned long ret = sizeof(struct StateSpace);
  if (this->tbl) {
    ret += this->tbl->space();
  }
  if (this->memid_tbl) {
    ret += this->memid_tbl->space();
  }
  if (this->acc_tbl) {
    ret += this->acc_tbl->space();
  }
  if (this->acc_memid_tbl) {
    ret += this->acc_memid_tbl->space();
  }
  if (this->thread_num > 1) {
    unsigned int i;
    for (i = 0; i < this->thread_num; i++)
      ret += vec_space(&this->end_states[i]);
  } else {
    ret += vec_space(this->end_states);
  }
  return ret;
}

/** Printer et al
 */

void StateSpace::dump_ends() {
  const Vector *ends;
  unsigned int i;

  ends = this->end_states;
  if (this->thread_num > 1) {
    Vector *end_i;
    unsigned int j;
    for (i = 0; i < this->thread_num; i++) {
      end_i = (Vector *)(&ends[i]);
      for (j = 0; j < vec_num(end_i); j++) {
        state_print_mem((State *)vec_get(end_i, j), (LmnWord)this->out);
        if (lmn_env.sp_dump_format == LMN_SYNTAX) {
          printf(".\n");
        }
      }
    }
  } else {
    for (i = 0; i < vec_num(ends); i++) {
      state_print_mem((State *)vec_get(ends, i), (LmnWord)this->out);
      if (lmn_env.sp_dump_format == LMN_SYNTAX) {
        printf(".\n");
      }
    }
  }
}

void StateSpace::dump() {
  State *init = this->initial_state();
  switch (lmn_env.mc_dump_format) {
  case Dir_DOT:
    fprintf(this->out, "digraph StateTransition {\n");
    fprintf(this->out, "  node [shape = circle];\n");
    fprintf(this->out,
            "  %lu [style=filled, color = \"#ADD8E6\", shape = Msquare];\n",
            state_format_id(init, this->is_formated));
    this->dump_all_states();
    this->dump_all_transitions();
    this->dump_all_labels();
    fprintf(this->out, "}\n");
    break;
  case FSM:
    /* TODO: under construction..
     *   一般的なLTS状態表現方法であるFSM形式.
     *   変数集合/State Vectorに分けて出力する必要がある.
     *   階層グラフ構造をどのように出力すべきか要検討.
     *   現状ではとりあえず状態データを空にして状態遷移グラフを出力する */
    // statespace_print_state_data
    // statespace_print_state_vector
    fprintf(this->out, "Under_Constructions(2) Binay \"Yes\" \"No\"\n");
    fprintf(this->out, "---\n");
    this->dump_all_states();
    fprintf(this->out, "---\n");
    this->dump_all_transitions();
    break;
  case LaViT: /* FALL THROUGH */
  default:
    if (lmn_env.sp_dump_format != INCREMENTAL) {
      fprintf(this->out, "States\n");
      this->dump_all_states();
    }
    fprintf(this->out, "\n");
    fprintf(this->out, "Transitions\n");
    fprintf(this->out, "init:%lu\n", state_format_id(init, this->is_formated));
    this->dump_all_transitions();
    fprintf(this->out, "\n");

    if (this->has_property() && lmn_env.mc_dump_format == LaViT) {
      fprintf(this->out, "Labels\n");
      this->dump_all_labels();
      fprintf(this->out, "\n");
    }

    break;
  }
}

void StateSpace::dump_all_states() {
  if (this->tbl)
    for (auto &ptr : *this->tbl)
    dump_state_data(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
  if (this->memid_tbl)
    for (auto &ptr : *this->memid_tbl)
                     dump_state_data(ptr , (LmnWord)this->out,
                     (LmnWord)this);
  if (this->acc_tbl)
    for (auto &ptr : *this->acc_tbl)
                     dump_state_data(ptr , (LmnWord)this->out,
                     (LmnWord)this);
  if (this->acc_memid_tbl)
    for (auto &ptr : *this->acc_memid_tbl)
                     dump_state_data(ptr , (LmnWord)this->out,
                     (LmnWord)this);
}

void StateSpace::dump_all_transitions() {
  if (this->tbl)
    for (auto &ptr : *this->tbl)
                     state_print_transition(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
  if (this->memid_tbl)
    for (auto &ptr : *this->memid_tbl)
                     state_print_transition(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
  if (this->acc_tbl)
    for (auto &ptr : *this->acc_tbl)
                     state_print_transition(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
  if (this->acc_memid_tbl)
    for (auto &ptr : *this->acc_memid_tbl)
                     state_print_transition(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
}

void StateSpace::dump_all_labels() {
  if (this->tbl)
    for (auto &ptr : *this->tbl)
    state_print_label(ptr ,
                     (LmnWord)this->out, (LmnWord)this);
  if (this->memid_tbl)
    for (auto &ptr : *this->memid_tbl)
                     state_print_label(ptr , (LmnWord)this->out,
                     (LmnWord)this);
  if (this->acc_tbl)
    for (auto &ptr : *this->acc_tbl)
                     state_print_label(ptr , (LmnWord)this->out,
                     (LmnWord)this);
  if (this->acc_memid_tbl)
    for (auto &ptr : *this->acc_memid_tbl)
                     state_print_label(ptr , (LmnWord)this->out,
                     (LmnWord)this);
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
    if (this->tbl) this->tbl->format_states();
    if (this->memid_tbl) this->memid_tbl->format_states();
    if (this->acc_tbl) this->acc_tbl->format_states();
    if (this->acc_memid_tbl) this->acc_memid_tbl->format_states();
    this->is_formated = TRUE;
  }
#endif
}
