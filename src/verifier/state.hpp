/*
 * state.hpp
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

#ifndef LMN_STATE_HPP
#define LMN_STATE_HPP
#include "trie.hpp"
#include "lmntal.h"
#include "mhash.h"
#include "state_defs.h"
#include "runtime_status.h"
#include "vm/vm.h"

/** Flags (8bit)
 *  0000 0001  stack上に存在する頂点であることを示すフラグ (for nested dfs)
 *  0000 0010  受理サイクル探索において探索済みの頂点であることを示すフラグ
 *  0000 0100  遷移先を計算済みであること(closed node)を示すフラグ.
 *  0000 1000  受理サイクル上の状態であることを示すフラグ
 *  0001 0000  ハッシュ表上でDummyオブジェクトであるか否かを示すフラグ
 *             (Rehash処理で使用)
 *  0010 0000  successorとしてstruct Stateを直接参照するか,
 *             struct Transitionを介して参照するかを示すフラグ.
 *  0100 0000  状態データ(union)がmemを直接的に保持する場合に立てるフラグ
 *  1000 0000  保持するバイナリストリングが階層グラフ構造に対して一意なID
 *             であるかを示すフラグ
 */

enum StateFlags {
  ON_STACK_MASK = 0x01U,
  FOR_MC_MASK = 0x01U << 1,
  ON_CYCLE_MASK = 0x01U << 2,
  EXPANDED_MASK = 0x01U << 3,
  DUMMY_SYMBOL_MASK = 0x01U << 4,
  TRANS_OBJ_MASK = 0x01U << 5,
  MEM_ENCODED_MASK = 0x01U << 6,
  MEM_DIRECT_MASK = 0x01U << 7
};

/** Flags2 (8bit)
 *  0000 0001  Partial Order
 * ReductionによるReductionマーキング(debug/demo用機能) 0000 0010  D compression
 * stateか否かを示すフラグ 0000 0100  (MAPNDFS)explorer visit flag 0000 1000
 * (MAPNDFS)generator visit flag 0001 0000  (MCNDFS)blue flag 0010 0000
 * (MCNDFS)red flag 0100 0000  (Visualize)visited 1000 0000
 */

enum StateFlags2 {
  STATE_REDUCED_MASK = 0x01U,
  STATE_DELTA_MASK = 0x01U << 1,
  STATE_UPDATE_MASK = 0x01U << 2,
  EXPLORER_VISIT_MASK = 0x01U << 3,
  GENERATOR_VISIT_MASK = 0x01U << 4,
  STATE_BLUE_MASK = 0x01U << 5,
  STATE_RED_MASK = 0x01U << 6,
  STATE_VIS_VISITED_MASK = 0x01U << 7
};

/* Descriptor */
struct State {                /* Total:72(36)byte */
  unsigned int successor_num; /*  4(4)byte: サクセッサの数 */
  BYTE state_name; /*  1(1)byte: 同期積オートマトンの性質ラベル */
  BYTE flags;      /*  1(1)byte: フラグ管理用ビットフィールド */
  BYTE flags2;     /*  1(1)byte: フラグ管理用ビットフィールド2 */
  BYTE flags3; /*  1(1)byte: アラインメントの隙間(一時的にdpor_naiveで使用中) */
  unsigned long hash; /*  8(4)byte: 通常時: 膜memのハッシュ値, --mem-enc時:
                         膜の一意なバイト列のハッシュ値  */
  state_data_t data; /*  8(4)byte: 膜, バイナリストリングのどちらか */
  TreeCompressData
      tcd; /*  8(8)byte: Tree Compression 用のデータ無理やり8 Byteにしている */
  succ_data_t *successors; /*  8(4)byte: サクセッサポインタの配列 */
  State *next; /*  8(4)byte: 状態管理表に登録する際に必要なポインタ */
  State *parent; /*  8(4)byte: 自身を生成した状態へのポインタを持たせておく */
  unsigned long state_id; /*  8(4)byte: 生成順に割り当てる状態の整数ID */
  State *map; /*  8(4)byte: MAP値 or 最適化実行時の前状態 */
  Graphinfo *graphinfo;
  Trie *trie;
#ifndef MINIMAL_STATE
  BYTE *
      local_flags; /*  8(4)byte:
                      並列実行時、スレッド事に保持しておきたいフラグ(mcndfsのcyanフラグ等)
                    */
  pthread_mutex_t expand_lock;
  unsigned long expander_id;
  void state_set_expander_id(unsigned long id) { expander_id = id; }
  unsigned long state_expander_id() { return expander_id; }
  void state_expand_lock_init() { lmn_mutex_init(&(expand_lock)); }
  void state_expand_lock_destroy() { lmn_mutex_destroy(&(expand_lock)); }
  void state_expand_lock() { lmn_mutex_lock(&(expand_lock)); }
  void state_expand_unlock() { lmn_mutex_unlock(&(expand_lock)); }

  /* manipulation for local flags */
  void s_set_cyan(int i) { local_flags[i] |= STATE_CYAN_MASK; }
  void s_unset_cyan(int i) { local_flags[i] &= (~STATE_CYAN_MASK); }
  BOOL s_is_cyan(int i) {
    return (local_flags && (local_flags[i] & STATE_CYAN_MASK));
  }
#else
  void state_set_expander_id(unsigned long id) {}
  unsigned long state_expander_id() { return 0; }
  void state_expand_lock_init() {}
  void state_expand_lock_destroy() {}
  void state_expand_lock() {}
  void state_expand_unlock() {}
#endif

  BOOL has_trans_obj() { return flags & TRANS_OBJ_MASK; }
  BOOL is_binstr_user() { return flags & MEM_DIRECT_MASK; }
  BOOL is_dummy() { return flags & DUMMY_SYMBOL_MASK; }
  BOOL is_encoded() { return flags & MEM_ENCODED_MASK; }
  BOOL is_expanded() { return flags & EXPANDED_MASK; }
  BOOL is_on_cycle() { return flags & ON_CYCLE_MASK; }
  BOOL is_on_stack() { return flags & ON_STACK_MASK; }
  BOOL is_snd() { return flags & FOR_MC_MASK; }
  void set_binstr_user() { flags |= MEM_DIRECT_MASK; }
  void set_dummy() { flags |= DUMMY_SYMBOL_MASK; }
  void set_encoded() { flags |= MEM_ENCODED_MASK; }
  void set_expanded() { flags |= EXPANDED_MASK; }
  void set_on_cycle() { flags |= ON_CYCLE_MASK; }
  void set_on_stack() { flags |= ON_STACK_MASK; }
  void set_snd() { flags |= FOR_MC_MASK; }
  void set_trans_obj() { flags |= TRANS_OBJ_MASK; }
  void unset_binstr_user() { flags &= (~MEM_DIRECT_MASK); }
  void unset_on_stack() { flags &= (~ON_STACK_MASK); }
  void unset_trans_obj() { flags &= (~TRANS_OBJ_MASK); }
  BOOL s_is_d() { return flags2 & STATE_DELTA_MASK; }
  BOOL s_is_reduced() { return flags2 & STATE_REDUCED_MASK; }
  BOOL s_is_update() { return flags2 & STATE_UPDATE_MASK; }
  void s_set_d() { flags2 |= STATE_DELTA_MASK; }
  void s_set_reduced() { flags2 |= STATE_REDUCED_MASK; }
  void s_set_update() { flags2 |= STATE_UPDATE_MASK; }
  void s_unset_d() { flags2 &= (~STATE_DELTA_MASK); }
  void s_unset_update() { flags2 &= (~STATE_UPDATE_MASK); }
  BOOL s_is_visited_by_explorer() { return flags2 & EXPLORER_VISIT_MASK; }
  BOOL s_is_visited_by_generator() { return flags2 & GENERATOR_VISIT_MASK; }
  void s_set_visited_by_explorer() { flags2 |= EXPLORER_VISIT_MASK; }
  void s_set_visited_by_generator() { flags2 |= GENERATOR_VISIT_MASK; }
  BOOL s_is_blue() { return flags2 & STATE_BLUE_MASK; }
  BOOL s_is_red() { return flags2 & STATE_RED_MASK; }
  BOOL s_is_visited_by_visualizer() { return flags2 & STATE_VIS_VISITED_MASK; }
  void s_set_blue() { flags2 |= STATE_BLUE_MASK; }
  void s_set_red() { flags2 |= STATE_RED_MASK; }
  void s_set_visited_by_visualizer() { flags2 |= STATE_VIS_VISITED_MASK; }
  void s_set_fresh() { flags3 |= STATE_FRESH_MASK; }
  void s_unset_fresh() { flags3 &= (~STATE_FRESH_MASK); }
  BOOL s_is_fresh() { return flags3 & STATE_FRESH_MASK; }
#ifdef KWBT_OPT
  LmnCost cost; /*  8(4)byte: cost */
#endif
  LmnBinStrRef state_binstr() {
    if (is_binstr_user()) {
      return (LmnBinStrRef)data;
    } else {
      return NULL;
    }
  }

  void state_set_binstr(LmnBinStrRef bs) {
    data = (state_data_t)bs;
    set_binstr_user();
  }

  void state_set_mem(LmnMembraneRef mem) {
    unset_binstr_user();
    data = (state_data_t)mem;
  }

  void state_calc_hash(LmnMembraneRef mem, BOOL canonical) {
    if (canonical) {
      state_set_binstr(lmn_mem_encode(mem));
      hash = binstr_hash(this->state_binstr());
      set_encoded();
    } else {
      hash = mhash(mem);
    }
  }

  /* 状態sに対応する階層グラフ構造を返す.
   * 既にバイナリストリングへエンコードしている場合の呼び出しは想定外. */
  LmnMembraneRef state_mem() {
    if (is_binstr_user()) {
      return NULL;
    } else {
      return (LmnMembraneRef)data;
    }
  }

  /* 状態srcと等価な状態を新たに構築して返す.
   * srcに階層グラフ構造が割り当てられている場合, その階層グラフ構造までを,
   * else: srcにバイナリストリングが割り当てられている場合,
   * そのバイナリストリングを, 複製(即ち, deep copy)する. また,
   * これに伴うencode関連の状態フラグもコピーするが,
   * 状態空間構築のためのフラグはコピーしない.
   * なお, 引数memがNULLではない場合は,
   * これをsrcに対応する階層グラフ構造として扱う.　*/
  State *duplicate(LmnMembraneRef mem) {
    State *dst = new State();

    if (!is_binstr_user() && !mem) {
      mem = state_mem();
    }

    if (mem) {
      dst->state_set_mem(lmn_mem_copy_ex(mem));
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
      }
#endif
    } else if (state_binstr()) {
      dst->state_set_binstr(lmn_binstr_copy(state_binstr()));
      if (is_encoded()) {
        dst->set_encoded();
      }
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
      }
#endif
    }

    dst->hash = hash;

#ifdef PROFILE
    if (lmn_env.profile_level >= 3 && mem) {
      profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
      profile_finish_timer(PROFILE_TIME__STATE_COPY);
    }
#endif
    return dst;
  }

  void succ_set(Vector *v) {
    if (!vec_is_empty(v) && !successors) {
      unsigned int i;
      successor_num = vec_num(v);
      successors = LMN_NALLOC(succ_data_t, successor_num);
      for (i = 0; i < successor_num; i++) {
        successors[i] = (succ_data_t)vec_get(v, i);
      }
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_add_space(PROFILE_SPACE__TRANS_OBJECT,
                          sizeof(succ_data_t) * vec_num(v));
        profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, 0);
      }
#endif
    }
  }

  void succ_add(succ_data_t succ) {
    if (!successors) {
      successors = LMN_NALLOC(succ_data_t, 1);
    } else {
      successors = LMN_REALLOC(succ_data_t, successors, successor_num + 1);
    }
    successors[successor_num] = succ;
    successor_num++;
  }

  void succ_clear() {
    if (this->has_trans_obj()) {
      unsigned int i;
      for (i = 0; i < this->successor_num; i++) {
        TransitionRef t = transition(this, i);
        transition_free(t);
      }
    }

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT,
                           sizeof(succ_data_t) * this->successor_num);
      profile_add_space(PROFILE_SPACE__TRANS_OBJECT, 0);
    }
#endif

    LMN_FREE(this->successors);
    this->successors = NULL;
    this->successor_num = 0;
    this->unset_trans_obj();
  }

  void free_mem() {
    if (this->state_mem()) {
#ifdef PROFILE
      if (lmn_env.profile_level >= 3) {
        profile_remove_space(PROFILE_SPACE__STATE_MEMBRANE,
                             lmn_mem_space(this->state_mem()));
      }
#endif
      lmn_mem_free_rec(this->state_mem());
      this->state_set_mem(NULL);
    }
  }

  void free_binstr() {
    if (this->state_binstr()) {
      lmn_binstr_free(this->state_binstr());
      this->data = NULL;
    }
    this->unset_binstr_user();
  }

  /* 状態sに対応する階層グラフ構造をバイナリストリングにエンコードして返す.
   * sのフラグを操作する. */
  LmnBinStrRef mem_dump() {
    LmnBinStrRef ret;

    if (this->state_binstr()) {
      /* 既にエンコード済みの場合は何もしない. */
      ret = this->state_binstr();
    } else if (this->state_mem()) {
      ret = lmn_mem_to_binstr(this->state_mem());
      if (this->s_is_d() && this->state_D_ref()) {
        LmnBinStrRef dif;
        dif = state_binstr_D_compress(ret, this->state_D_ref());
        /* 元のバイト列は直ちに破棄せず, 一時的にキャッシュしておく. */
        state_D_cache(this, ret);
        ret = dif;
      } else {
        this->s_unset_d();
      }
    } else {
      lmn_fatal("unexpected.");
      ret = NULL;
    }

    return ret;
  }

  /* 状態sに対応する階層グラフ構造をバイナリストリングにエンコードして返す.
   * sのフラグを操作する. */
  LmnBinStrRef mem_dump_with_tree() {
    if (this->state_binstr()) /* 既にエンコード済みの場合は何もしない. */
      return this->state_binstr();

    if (this->state_mem())
      return lmn_mem_to_binstr(this->state_mem());

    lmn_fatal("unexpected.");
    return NULL;
  }

  /* 状態sに対応した階層グラフ構造のバイナリストリングをzlibで圧縮して返す.
   * 状態sはread only */
  LmnBinStrRef mem_dump_with_z() {
    LmnBinStrRef ret;
    if (this->is_binstr_user()) {
      /* 既にバイナリストリングを保持している場合は, なにもしない. */
      ret = this->state_binstr();
    } else if (this->state_mem()) {
      LmnBinStrRef bs = lmn_mem_to_binstr(this->state_mem());
      /* TODO: --d-compressとの組合わせ */
      ret = lmn_bscomp_z_encode(bs);
      if (ret != bs) { /* 圧縮成功 */
        lmn_binstr_free(bs);
      }
    } else {
      lmn_fatal("unexpected");
    }

    return ret;
  }

  /* 状態sに対応する階層グラフ構造と等価な階層グラフ構造を新たに構築して返す.
   * 構築できなかった場合はNULLを返す. */
  LmnMembraneRef duplicate_membrane() {
    LmnMembraneRef ret = NULL;
    if (!this->is_binstr_user() && this->state_mem()) {
      ret = lmn_mem_copy(this->state_mem());
    } else if (this->is_binstr_user() && this->state_binstr()) {
      ret = lmn_binstr_decode(this->state_binstr());
    }

#ifdef PROFILE
    if (lmn_env.profile_level >= 3 && ret) {
      profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(ret));
    }
#endif

    return ret;
  }

  /* 状態sに対応する階層グラフ構造Xを,
   * Xに対して一意なIDとなるバイナリストリングへエンコードする.
   * エンコードしたバイナリストリングをsに割り当てる.
   * sに対応する階層グラフ構造Xへのメモリ参照が消えるため,
   * 呼び出し側でメモリ管理を行う.
   * sが既にバイナリストリングを保持している場合は,
   * そのバイナリストリングは破棄する. (ただし,
   * sが既に一意なIDとなるバイナリストリングへエンコード済みの場合は何もしない.)
   * 既に割当済みのバイナリストリングを破棄するため,
   * sをハッシュ表に登録した後の操作はMT-unsafeとなる. 要注意. */
  void calc_mem_encode() {
    if (!this->is_encoded()) {
      LmnBinStrRef mid;

      if (this->state_mem()) {
        mid = lmn_mem_encode(this->state_mem());
      } else if (this->state_binstr()) {
        LmnMembraneRef m;

        m = lmn_binstr_decode(this->state_binstr());
        mid = lmn_mem_encode(m);
        this->free_binstr();
        lmn_mem_free_rec(m);
      } else {
        lmn_fatal("unexpected.");
      }

      this->state_set_binstr(mid);
      this->hash = binstr_hash(this->state_binstr());
      this->set_encoded();
    }
  }

  void calc_binstr_delta() {
    LmnBinStrRef org = this->state_binstr();
    if (org && this->state_D_ref()) {
      LmnBinStrRef dif;
      dif = state_binstr_D_compress(org, this->state_D_ref());
      state_D_cache(this, org);
      this->state_set_binstr(dif);
    } else {
      this->s_unset_d();
    }
  }

  /* バイナリストリングorgと状態ref_sのバイナリストリングとの差分バイナリストリングを返す.
   * orgのメモリ管理は呼出し側で行う. */
  static inline LmnBinStrRef state_binstr_D_compress(LmnBinStrRef org,
                                                     State *ref_s) {
    LmnBinStrRef ref, dif;

    ref = state_D_fetch(ref_s);
    if (ref) {
      dif = lmn_bscomp_d_encode(org, ref);
    } else {
      ref = ref_s->reconstruct_binstr();
      dif = lmn_bscomp_d_encode(org, ref);
      if (ref_s->s_is_d()) {
        lmn_binstr_free(ref);
      }
    }

    return dif;
  }

  /* 状態sとの差分計算の対象とする状態に対する参照を返す. */
  State *state_D_ref() {
    /* とりあえず親ノードにした */
    return this->parent;
  }

  /* 状態sに対応するバイナリストリングを, sがrefする状態を基に再構築して返す. */
  LmnBinStrRef reconstruct_binstr() {
    if (!this->s_is_d())
      return this->state_binstr();

    LmnBinStrRef ref = this->state_D_ref()->reconstruct_binstr();
    LmnBinStrRef ret = lmn_bscomp_d_decode(ref, this->state_binstr());
    LMN_ASSERT(this->state_D_ref());

    if (this->state_D_ref()->s_is_d())
      lmn_binstr_free(ref);

    return ret;
  }

  /* 状態sに対応する階層グラフ構造memへのアドレスを返す.
   * memがエンコードされている場合は, デコードしたオブジェクトのアドレスを返す.
   * デコードが発生した場合のメモリ管理は呼び出し側で行う. */
  LmnMembraneRef restore_membrane() {
    return this->restore_membrane_inner(TRUE);
  }

  /* delta-compression用のinner関数.
   * flagが真の場合, デコード済みのバイナリストリングをキャッシュから取得する.
   * キャッシュにバイナリストリングを置かないケースで使用する場合は,
   * inner関数を直接呼び出し, flagに偽を渡しておけばよい. */
  LmnMembraneRef restore_membrane_inner(BOOL flag) {
    if (this->state_mem())
      return this->state_mem();

    if (this->s_is_d()) {
      LmnBinStrRef b =
          (flag) ? state_D_fetch(this) : this->reconstruct_binstr();
      return lmn_binstr_decode(b);
    }

    LmnBinStrRef b = this->state_binstr();
    if (lmn_env.tree_compress && !b) {
      TreeNodeID ref;
      tcd_get_root_ref(&this->tcd, &ref);
      LMN_ASSERT(ref);
      LMN_ASSERT(tcd_get_byte_length(&this->tcd) != 0);
      b = lmn_bscomp_tree_decode((TreeNodeID)ref,
                                 tcd_get_byte_length(&this->tcd));
    }

    LMN_ASSERT(b);
    return lmn_binstr_decode(b);
  }

public:
  State()
      : data(NULL), state_name(0x00U), flags(0x00U), flags2(0x00U),
        flags3(0x00U), hash(0), next(NULL), successors(NULL), successor_num(0),
        parent(NULL), state_id(0), map(NULL) {
    memset(&tcd, 0x00, sizeof(TreeCompressData));
#ifndef MINIMAL_STATE
    state_set_expander_id(LONG_MAX);
    local_flags = 0x00U;
    state_expand_lock_init();
#endif
    s_set_fresh();

#ifdef KWBT_OPT
    if (lmn_env.opt_mode != OPT_NONE) {
      cost = lmn_env.opt_mode == OPT_MINIMIZE ? ULONG_MAX : 0;
    }
#endif

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
    }
#endif
  }

  State(LmnMembraneRef mem, BYTE property_label, BOOL do_encode) : State() {
    state_set_mem(mem);
    state_name = property_label;
    state_calc_hash(mem, do_encode);
    // convertedgraph = new ConvertedGraph(mem);
    if (is_encoded()) {
      lmn_mem_free_rec(mem);
    }
#ifdef PROFILE
    else if (lmn_env.profile_level >= 3) {
      profile_add_space(PROFILE_SPACE__STATE_MEMBRANE, lmn_mem_space(mem));
    }
#endif
  }

  ~State() {
    if (successors) {
#ifdef PROFILE
      if (lmn_env.profile_level >= 3)
        profile_remove_space(PROFILE_SPACE__TRANS_OBJECT,

                            sizeof(succ_data_t) * this->successor_num);

                             // sizeof(succ_data_t) * state_succ_num(s));

#endif
      if (has_trans_obj()) {
        unsigned int i;
        for (i = 0; i < successor_num; i++) {
          transition_free(transition(this, i));
        }
      }
      LMN_FREE(successors);
    }
#ifndef MINIMAL_STATE
    if (local_flags) {
      LMN_FREE(local_flags);
    }
#endif
    state_expand_lock_destroy();
    this->free_mem();
    this->free_binstr();
    delete graphinfo;
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_remove_space(PROFILE_SPACE__STATE_OBJECT, sizeof(struct State));
    }
#endif
  }
};

#endif
