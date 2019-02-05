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
#include "vm/vm.h"

void StateSpace::make_table() {
  if (lmn_env.mem_enc) {
    this->set_memenc();
    this->memid_tbl = new StateTable(this->thread_num);
    if (this->thread_num > 1) {
      this->memid_tbl->set_lock(ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));
    }

    if (this->has_property()) {
      this->acc_memid_tbl = new StateTable(this->thread_num);
      if (this->thread_num > 1) {
        this->acc_memid_tbl->set_lock(
            ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));
      }
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
        this->acc_tbl->set_rehash_table(
            this->acc_memid_tbl);
      }
    }

    if (this->thread_num > 1) {
      this->tbl->set_lock(
          ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));

      if (this->acc_tbl) {
        this->acc_tbl->set_lock(
            ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));
      }
      if (this->memid_tbl) {
        this->memid_tbl->set_lock(
            ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));
      }
      if (this->acc_memid_tbl) {
        this->acc_memid_tbl->set_lock(
            ewlock_make(this->thread_num, DEFAULT_WLOCK_NUM));
      }
    }
  }
#ifdef PROFILE
  if (lmn_env.optimize_hash_old) {
    if (!this->memid_tbl) {
      this->memid_tbl = new StateTable(this->thread_num);
    }
    if (this->acc_tbl) {
      this->acc_memid_tbl = new StateTable(this->thread_num);
    }
    hashset_init(&this->memid_hashes, 128);
  }
#endif
}

AutomataRef statespace_automata(StateSpace *SS) {
  return (SS)->property_automata;
}
Vector *statespace_propsyms(StateSpace *SS) { return ((SS)->propsyms); }

#define TABLE_DEFAULT_MAX_DENSITY                                              \
  (5U) /* 1バケットあたりの平均長がこの値を越えた場合にresizeする */
#define MEM_EQ_FAIL_THRESHOLD                                                  \
  (2U) /* 膜の同型性判定にこの回数以上失敗すると膜のエンコードを行う */

/** Macros
 */

#define need_resize(EntryNum, Capacity)                                        \
  (((EntryNum) / (Capacity)) > TABLE_DEFAULT_MAX_DENSITY)
#define STATE_EQUAL(Tbl, Check, Stored)                                        \
  (state_hash(Check) == state_hash(Stored) &&                                  \
   ((Tbl)->type->compare)(Check, Stored))

/* 状態管理表stに登録されているcompress関数を用いて,
 * 状態sのバイナリストリングbsを計算して返す.
 * (--disable-compressの場合はdummy関数がNULLを返す.)
 * bsが計算済みの場合(NULLでない場合)は, 何もしない */
LmnBinStrRef StateTable::compress_state(State *s, LmnBinStrRef bs) {
  if (!bs) {
    /* canonical idにエンコードを行った場合,
     * bsは状態生成時に計算済みになっているため,
     * このブロックを実行することはない. */
    LMN_ASSERT(!s->is_encoded());
    bs = (*((this)->type->compress))(s);
  }
  return bs;
}

/** Global Vars
 */
struct statespace_type type_state_compress_z = {state_cmp_with_compress,
                                                state_calc_mem_dump_with_z};

struct statespace_type type_state_compress = {state_cmp_with_compress,
                                              state_calc_mem_dump};

struct statespace_type type_state_tree_compress = {
    state_cmp_with_tree, state_calc_mem_dump_with_tree};

struct statespace_type type_state_default = {state_cmp, state_calc_mem_dummy};

/* Table of prime numbers 2^n+a, 2<=n<=30. */
static unsigned long primes[] = {8192 + 27,       131072 + 29,
                                 1048576 + 7,     4194304 + 15,
                                 16777216 + 43,   67108864 + 15,
                                 268435456 + 3,   536870912 + 11,
                                 1073741824 + 85, 0};

static inline unsigned long table_new_size(unsigned long old_size) {
  unsigned long i, n;

  n = (unsigned long)(sizeof(primes) / sizeof(primes[0]));
  for (i = 0; i < n; i++) {
    if (primes[i] > old_size) {
      return primes[i];
    }
  }
  lmn_fatal("Size of StateSpace exceeded the limiting value");
  return 0; /* exception */
}

/* テーブルのサイズを1段階拡張する */
void StateTable::resize(unsigned long old_cap) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_countup(PROFILE_COUNT__HASH_RESIZE_TRIAL);
  }
#endif
  if (this->cap() == old_cap) {
    slim::element::ewmutex mutex(slim::element::ewmutex::exclusive_enter,
                                 this->lock, env_my_thread_id());
    std::lock_guard<slim::element::ewmutex> lk(mutex);
    if (this->cap() == old_cap) {
      unsigned long i, new_cap, bucket;
      State *ptr, *next, **new_tbl;

      new_cap = table_new_size(old_cap);
      new_tbl = LMN_NALLOC(State *, new_cap);

      for (i = 0; i < new_cap; i++) {
        new_tbl[i] = NULL;
      }

      for (i = 0; i < old_cap; i++) {
        ptr = this->tbl[i];
        while (ptr) {
          next = ptr->next;
          bucket = state_hash(ptr) % new_cap;
          ptr->next = new_tbl[bucket];
          if (ptr->is_dummy() && ptr->is_expanded() && !ptr->is_encoded()) {
            /* オリジナルテーブルでdummy_stateが存在する状態にはバイト列は不要
             * (resize中に,
             * 展開済み状態へのデータアクセスは発生しないよう設計している) */
            ptr->free_binstr();
            if (ptr->s_is_d()) {
              ptr->s_unset_d();
            }
          }
          new_tbl[bucket] = ptr;
          ptr = next;
        }
      }

      LMN_FREE(this->tbl);
      this->tbl = new_tbl;
      this->cap_ = new_cap;
      this->cap_density_ = new_cap / this->thread_num;

#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_countup(PROFILE_COUNT__HASH_RESIZE_APPLY);
      }
#endif
    }
  }
}

void StateTable::set_lock(EWLock *lock) { this->lock = lock; }

bool StateTable::use_rehasher() const { return use_rehasher_; }

unsigned long StateTable::num_by_me() const {
  return this->num[env_my_thread_id()];
}

unsigned long StateTable::all_num() const {
  unsigned long ret = 0;
  unsigned int i;
  for (i = 0; i < this->thread_num; i++) {
    ret += this->num[i];
  }
  return ret;
}

void StateTable::num_increment() { num[env_my_thread_id()]++; }

void StateTable::num_dummy_increment() { num_dummy_[env_my_thread_id()]++; }

void StateTable::set_rehash_table(StateTable *rehash_tbl) {
  this->rehash_tbl_ = rehash_tbl;
  this->set_rehasher();
}

StateTable *StateTable::rehash_table() { return this->rehash_tbl_; }

unsigned long StateTable::space() const {
  return sizeof(struct StateTable) +
         (num ? thread_num * sizeof(unsigned long) : 0) +
         (num_dummy_ ? thread_num * sizeof(unsigned long) : 0) +
         (cap_ * sizeof(State *)) + lmn_ewlock_space(lock);
}

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

/* for parallel model checker mode */
StateSpaceRef statespace_make_for_parallel(int thread_num, AutomataRef a,
                                           Vector *psyms) {
  return new StateSpace(thread_num, a, psyms);
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

/* **既に状態管理表stに追加済みの状態s**(およびsに対応する階層グラフ構造)と
 * 等価な状態を新たに生成し,
 * 生成した状態をテーブルstのrehash先テーブルへ追加する.
 * ここで新たに生成する状態は,
 * sに対応する階層グラフ構造対して一意なバイナリストリングを持つ. */
void StateTable::memid_rehash(State *s) {
  State *new_s;
  LmnMembraneRef m;

  new_s = new State();
  m = lmn_binstr_decode(s->state_binstr());
  new_s->state_name = s->state_name;
  new_s->state_set_binstr(lmn_mem_encode(m));
  new_s->hash = binstr_hash(new_s->state_binstr());

  new_s->set_encoded();
  new_s->set_expanded();
  new_s->set_dummy();

  this->rehash_table()->add_direct(
      new_s); /* dummy stateのparentをoriginalとして扱う */
  state_set_parent(new_s, s);
  this->rehash_table()->num_dummy_increment();

  lmn_mem_free_rec(m);
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

  is_accept = this->has_property() &&
              state_is_accept(this->automata(), s);

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
      insert_dst = is_accept ? this->acc_memid_tbl
                             : this->memid_tbl;
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
    std::copy(this->tbl->begin(), this->tbl->end(),
              std::back_inserter(result));
  if (this->memid_tbl)
    std::copy(this->memid_tbl->begin(),
              this->memid_tbl->end(), std::back_inserter(result));
  if (this->acc_tbl)
    std::copy(this->acc_tbl->begin(),
              this->acc_tbl->end(), std::back_inserter(result));
  if (this->acc_memid_tbl)
    std::copy(this->acc_memid_tbl->begin(),
              this->acc_memid_tbl->end(),
              std::back_inserter(result));
  return result;
}

/*----------------------------------------------------------------------
 * StateTable
 */

StateTable::StateTable(int thread_num, unsigned long size) {
  unsigned long i;
  this->thread_num = thread_num;

  if (lmn_env.enable_compress_mem) {
    if (lmn_env.z_compress) {
      this->type = &type_state_compress_z;
    } else if (lmn_env.tree_compress) {
      this->type = &type_state_tree_compress;
    } else {
      this->type = &type_state_compress;
    }
  } else {
    this->type = &type_state_default;
  }

  this->use_rehasher_ = FALSE;
  size = table_new_size(size);
  this->tbl = LMN_NALLOC(State *, size);
  this->cap_ = size;
  this->cap_density_ = size / thread_num;
  this->num = LMN_NALLOC(unsigned long, thread_num);
  this->num_dummy_ = LMN_NALLOC(unsigned long, thread_num);
  this->lock = NULL;
  this->rehash_tbl_ = NULL;

  memset(this->tbl, 0x00, size * (sizeof(State *)));

  for (i = 0; i < thread_num; i++) {
    this->num[i] = 0UL;
    this->num_dummy_[i] = 0UL;
  }
}

StateTable::~StateTable() {
  for (auto &ptr : *this)
    delete ptr;

  if (this->lock) {
    ewlock_free(this->lock);
  }
  delete (this->num_dummy_);
  delete (this->num);
  delete (this->tbl);
}

/* statetable_insert: 状態sが状態空間stに既出ならばその状態を,
 * 新規ならばs自身を返す.
 *
 * 以下, ハッシュ値が等しい状態を検出した場合の振舞いに関するメモ
 * - 膜のハッシュ関数(mhash)から膜のIDのハッシュ関数(memid_hash)へのrehash:
 * -- (用語) 膜のハッシュ関数によるテーブル   : オリジナルテーブル
 * -- (用語) 膜IDのハッシュ関数によるテーブル : memidテーブル
 *
 * - 状態をRehashするケース
 * -- その1
 *  サクセッサ状態の展開時, 展開元と展開先とでハッシュ値が衝突する場合
 *  展開先状態のメモリはまだハッシュ表に登録していないため,
 * 呼出しスレッドからの操作は全てMT-safe
 * -- その2
 *  既にハッシュ表(オリジナルテーブル)に登録してある状態をrehashできる条件は,
 *  >> 状態空間に対して全サクセッサの登録が完了した状態 << に対する場合(即ち,
 * 展開済み状態)のみ.
 *
 * - 前提/制約条件
 * -- テーブル単位で各状態は必ずユニークにする(即ち,
 * 状態展開時に選択する状態の重複は避ける)
 * --- 等価な状態が複数個, テーブル内に存在することは避ける.
 *      (理由1. 状態数不一致原因を特定するのが困難になる
 *       理由2. モデル検査器としての信頼性が曖昧になる
 *       理由3. 冗長な計算が発生する.)
 * -- 即ち,
 * オリジナルテーブルとmemidテーブルとの間で等価な状態が重複して存在することも避ける
 * -- バイト列のrehashにより, 許容できないメモリ使用量の増加を招いてはならない
 *   (Rehashによる単なる高速化ではなく, 時間と空間のトレードオフを考慮する)
 *
 * - 制約を満たすために
 * -- 1. rehash前後のstruct Stateオブジェクトにdummyフラグを立て,
 * ---
 * memidテーブル側のdummy状態(のメンバparent)からオリジナルテーブル側のdummy状態を刺す.
 * --- memidテーブル側のdummy状態は,
 * rehashしたバイト列データ(とそのハッシュ値)のみを保持する.
 *     (遷移先状態へのポインタや, 状態ラベルなどの情報を保持することはしない.)
 *
 *         [ original A ] <-----ptr----- [memid A]
 *         |- is_dummy:TRUE              |- is_dummy:TRUE
 *         |- is_memid:FALSE             |- is_memid:TRUE
 *         |- binstr                     |- mid_binstr
 *         |- mhash                      |- mid_hash (FNV)
 *         |- label                      |- ptr
 *         |- succ ptrs
 *         |- LTS parent
 *         |- et al..
 *
 *
 * -- 2. オリジナルテーブル側のdummy状態から,
 * MT-Safeなタイミングでrehash前のバイト列を破棄.
 * --- (Rehash前の状態は,
 * 任意のタイミングで異なるスレッドから探索によるメモリアクセスを受ける)
 * --- リスト走査中にdummy状態を発見した場合, 新たな処理を追加する.
 *
 * - statespace_insertに対する追加処理発生のタイミング
 * -- 状態(hash値A)を探索した際, dummy状態(hash値A)とハッシュ値が衝突した場合.
 *
 * - 追加処理の概要
 * -- 1. Search元の状態(ハッシュ値A)の一意なバイト列を計算
 * -- 2. 一意なバイト列への計算が完了したならば,
 *       そのバイト列とバイト列のハッシュ値(FNV)から,
 * memidテーブルをSearchを行う.
 *
 * - スレッドセーフなバイト列のRehashとRehash元バイト列の破棄
 * --
 * テーブルに登録済みの状態に対して任意のタイミングでメモリアクセスが発生する.
 *    任意のタイミングのメモリアクセスとは,
 *       A. 遷移先計算 (未展開状態のみ)
 *       B. 状態の等価性判定 (statespace_insert)
 * -- Rehashした状態(予めdummyフラグを立てておく)を,
 * まずmemidテーブルに登録してしまう.
 * -- memidテーブルへの登録が完了したならば,
 * Rehash元の状態にもdummyフラグを立てておく.
 * --
 * dummyフラグの立っているオリジナルテーブルの状態(バイト列)に対するメモリアクセスがなくなる.
 *    ただし, dummyフラグを立てている最中も,
 * バイト列へのメモリアクセスが発生し得る.
 * -- そこで, 全スレッドが同期を取るStateSpaceサイズ拡張処理の際に,
 * 冗長なバイト列を破棄する.
 */
State *StateTable::insert(State *ins, unsigned long *col) {
  State *ret;
  LmnBinStrRef compress;

  if (ins->is_binstr_user()) {
    /* 既に状態insがバイナリストリングを保持している場合 */
    compress = ins->state_binstr();
  } else {
    compress = NULL;
  }

  ret = NULL;
  slim::element::ewmutex outer_mutex(slim::element::ewmutex::enter, this->lock,
                                     env_my_thread_id());
  std::lock_guard<slim::element::ewmutex> lk(outer_mutex);
  State *str;
  unsigned long bucket, hash;

  hash = state_hash(ins);
  bucket = hash % this->cap();
  str = this->tbl[bucket];

  /* case: empty bucket */
  if (!str) {
    /* strがNULL --> 即ち未使用バケットの場合 */
    compress = this->compress_state(ins, compress);
    if (!this->tbl[bucket]) {
      slim::element::ewmutex mutex(slim::element::ewmutex::write, this->lock,
                                   bucket);
      std::lock_guard<slim::element::ewmutex> lk(mutex);
      if (!this->tbl[bucket]) {
        state_set_compress_for_table(ins, compress);
        this->num_increment();
        if (tcd_get_byte_length(&ins->tcd) == 0 && lmn_env.tree_compress) {
          TreeNodeID ref;
          tcd_set_byte_length(&ins->tcd, ins->state_binstr()->len);
          ref = lmn_bscomp_tree_encode(ins->state_binstr());
          tcd_set_root_ref(&ins->tcd, ref);
        }
        this->tbl[bucket] = ins;
        ret = ins;
      }
    }

    if (!ret) {
      /* 他のスレッドが先にバケットを埋めた場合, retがNULL
       * insert先strを再設定し, case: non-empty backetへ飛ぶ */
      str = this->tbl[bucket];
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_countup(PROFILE_COUNT__HASH_FAIL_TO_INSERT);
      }
#endif
    }
  }

  /* case: non-empty bucket */
  while (!ret) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_countup(PROFILE_COUNT__HASH_CONFLICT_ENTRY);
    }
#endif

    if (hash == state_hash(str)) {
      /* >>>>>>> ハッシュ値が等しい状態に対する処理ここから <<<<<<<<　*/
      if (lmn_env.hash_compaction) {
        if (str->is_dummy() && str->is_encoded()) {
          /* rehashテーブル側に登録されたデータ(オリジナル側のデータ:parentを返す)
           */
          ret = state_get_parent(str);
        } else {
          ret = str;
        }
        break;
      }

      if (this->use_rehasher() && str->is_dummy() && !str->is_encoded() &&
          lmn_env.tree_compress == FALSE) {
        /* A. オリジナルテーブルにおいて, dummy状態が比較対象
         * 　 --> memidテーブル側の探索へ切り替える.
         *    (オリジナルテーブルのdummy状態のバイト列は任意のタイミングで破棄されるため,
         *     直接dummy状態上のメモリを比較対象とするとスレッドセーフでなくなる)
         */

        if (ins->is_binstr_user()) {
          ins->free_binstr();
        } else if (compress) {
          lmn_binstr_free(compress);
        }
        ins->s_unset_d();
        ins->calc_mem_encode();
        /*compress = NULL;*/

#ifndef PROFILE
        ret = this->rehash_table()->insert(ins);
#else
        ret = this->rehash_table()->insert(ins, col);
#endif
        break;
      } else if (!STATE_EQUAL(this, ins, str)) {
        /** B. memidテーブルへのlookupの場合,
         *     もしくはオリジナルテーブルへのlookupで非dummy状態と等価な場合
         */
        if (str->is_dummy() && str->is_encoded()) {
          /* rehashテーブル側に登録されたデータ(オリジナル側のデータ:parentを返す)
           */
          ret = state_get_parent(str);
        } else {
          ret = str;
        }
        LMN_ASSERT(ret);
        break;
      } else if (str->is_encoded()) {
        /** C. memidテーブルへのlookupでハッシュ値が衝突した場合.
         * (同形成判定結果が偽) */
#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_countup(PROFILE_COUNT__HASH_CONFLICT_HASHV);
        }
#endif
      } else {
        /** D.
         * オリジナルテーブルへのlookupで非dummy状態とハッシュ値が衝突した場合.
         */
        LMN_ASSERT(!str->is_encoded());
#ifdef PROFILE
        (*col)++;
        if (lmn_env.profile_level >= 3) {
          profile_countup(PROFILE_COUNT__HASH_CONFLICT_HASHV);
        }
#endif
        if (this->use_rehasher() && lmn_env.tree_compress == FALSE) {
          if (state_get_parent(ins) == str) {
            /* 1step遷移した状態insの親状態とでハッシュ値が衝突している場合:
             *  + 状態insだけでなくその親状態もrehashする.
             *  + ただし, encodeした親状態をmemidテーブルへ突っ込んだ後,
             *    親状態の従来のバイト列の破棄は実施しない.
             *    これは,
             * 他のスレッドによる状態比較処理が本スレッドの処理と競合した際に,
             *    本スレッドが立てるdummyフラグを他のスレッドが見逃す恐れがあるため
             */
            this->memid_rehash(str);
            str->set_dummy();
          }

          /* 比較元をencode */
          if (ins->is_binstr_user()) {
            ins->free_binstr();
          } else if (compress) {
            lmn_binstr_free(compress);
          }
          ins->s_unset_d();
          ins->calc_mem_encode();

#ifndef PROFILE
          ret = this->rehash_table()->insert(ins);
#else
          ret = this->rehash_table()->insert(ins, col);
#endif

          LMN_ASSERT(ret);
          break;
        }
      }
      /* >>>>>>> ハッシュ値が等しい状態に対する処理ここまで <<<<<<<< */
    }

    /** エントリリストへの状態追加操作:
     * 逐次
     *   1. 各エントリ(状態)に対して, ハッシュ値と状態の比較を行う.
     *   2. 等価な状態を発見した場合, その状態へのアドレスを返す.
     *   3. リスト末尾へ到達した場合, バイト列が未計算ならば計算する.
     *   4. 計算したバイト列を状態に設定した後にリスト末尾に状態を追加する,
     *      (追加した状態のアドレスを返す.)
     * 並列処理への拡張
     *   4. (逐次版の手順4は実施しない)
     *      ロックを確保後,
     * 末尾エントリのnextアドレス(追加予定位置)がNULLであるか否かを検査
     *   5. NULLでない(他のスレッドが追加した)ならば, ロックを開放し,
     *      処理1(リスト走査)へcontinue.
     *   6. NULLならば,
     * 計算したバイト列を状態に設定してからリスト末尾に状態を追加する.
     *      (追加した状態のアドレスを返す.)
     */
    if (!str->next) { /* リスト末尾の場合 */
      compress = this->compress_state(ins, compress);
      slim::element::ewmutex mutex(slim::element::ewmutex::write, this->lock,
                                   bucket);
      if (!str->next) {
        std::lock_guard<slim::element::ewmutex> lk(mutex);
        if (!str->next) {
          this->num_increment();
          state_set_compress_for_table(ins, compress);
          if (tcd_get_byte_length(&ins->tcd) == 0 && lmn_env.tree_compress) {
            TreeNodeID ref;
            tcd_set_byte_length(&ins->tcd, ins->state_binstr()->len);
            ref = lmn_bscomp_tree_encode(ins->state_binstr());
            tcd_set_root_ref(&ins->tcd, ref);
          }
          str->next = ins;
          ret = ins;
        }
      }

#ifdef PROFILE
      if (!ret && lmn_env.profile_level >= 3) {
        profile_countup(PROFILE_COUNT__HASH_FAIL_TO_INSERT);
      }
#endif
    }

    str = str->next; /* リストを辿る */
  }

  if (ret != ins) {
    /* 別のスレッドの割込みで追加に失敗した場合, 計算したバイト列を破棄する.*/
    if (ins->is_binstr_user()) {
      ins->free_binstr();
    } else if (compress) {
      // lmn_binstr_free(compress);
    }
  }

  return ret;
}

/* 重複検査なしに状態sを状態表stに登録する */
void StateTable::add_direct(State *s) {
  slim::element::ewmutex outer_mutex(slim::element::ewmutex::enter, this->lock,
                                     env_my_thread_id());
  std::lock_guard<slim::element::ewmutex> lk(outer_mutex);

  LmnBinStrRef compress;
  State *ptr;
  unsigned long bucket;
  BOOL inserted;

  if (s->is_binstr_user()) {
    compress = s->state_binstr();
  } else {
    compress = NULL;
  }

  bucket = state_hash(s) % this->cap();
  ptr = this->tbl[bucket];
  inserted = FALSE;

  if (!ptr) { /* empty */
    compress = this->compress_state(s, compress);
    if (!this->tbl[bucket]) {
      slim::element::ewmutex mutex(slim::element::ewmutex::write, this->lock,
                                   bucket);
      std::lock_guard<slim::element::ewmutex> lk(mutex);
      if (!this->tbl[bucket]) {
        state_set_compress_for_table(s, compress);
        this->num_increment();
        if (tcd_get_byte_length(&s->tcd) == 0 && lmn_env.tree_compress) {
          TreeNodeID ref;
          tcd_set_byte_length(&s->tcd, s->state_binstr()->len);
          ref = lmn_bscomp_tree_encode(s->state_binstr());
          tcd_set_root_ref(&s->tcd, ref);
        }
        this->tbl[bucket] = s;
        inserted = TRUE;
      }
    }
    if (!inserted)
      ptr = this->tbl[bucket];
  }

  while (!inserted) {
    if (!ptr->next) { /* リスト末尾の場合 */
      compress = this->compress_state(s, compress);
      if (!ptr->next) {
        slim::element::ewmutex mutex(slim::element::ewmutex::write, this->lock,
                                     bucket);
        std::lock_guard<slim::element::ewmutex> lk(mutex);
        if (!ptr->next) {
          inserted = TRUE;
          this->num_increment();
          state_set_compress_for_table(s, compress);
          ptr->next = s;
        }
      }
    }
    ptr = ptr->next;
  }
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
         (this->memid_tbl ? this->memid_tbl->all_num()
                                     : 0) +
         (this->acc_tbl ? this->acc_tbl->all_num()
                                      : 0) +
         (this->acc_memid_tbl
              ? this->acc_memid_tbl->all_num()
              : 0);
}

/* memidテーブルに追加されているdummy状態数を返す */
unsigned long StateSpace::dummy_num() {
  StateTable *tbl;
  unsigned long ret;
  unsigned int i;

  ret = 0UL;
  tbl = this->memid_tbl;
  if (tbl) {
    for (i = 0; i < this->thread_num; i++) {
      ret += tbl->num_dummy(i);
    }
  }

  tbl = this->acc_memid_tbl;
  if (tbl) {
    for (i = 0; i < this->thread_num; i++) {
      ret += tbl->num_dummy(i);
    }
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

static inline int statetable_cmp_state_id_gr_f(const void *a_, const void *b_) {
  State *p1, *p2;

  p1 = *(State **)a_;
  p2 = *(State **)b_;

  if (p1 && p2) {
    return state_id(p1) - state_id(p2);
  } else if (p1 || p2) {
    return p1 ? state_id(p1) : (-1 * state_id(p2));
  } else {
    return 1;
  }
}

static inline void statetable_issue_state_id_f(State *s) {
  static unsigned long id = 1;
  state_set_format_id(s, id++);
}

/* CAUTION: MT-Unsafe */
void StateTable::format_states() {
  qsort(this->tbl, this->cap(), sizeof(struct State *),
        statetable_cmp_state_id_gr_f);
  for (auto &ptr : *this)
    statetable_issue_state_id_f(ptr);
}
