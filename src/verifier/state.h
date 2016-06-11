/*
 * state.h
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

#ifndef LMN_STATE_H
#define LMN_STATE_H

#include "../lmntal.h"
#include "lmntal_thread.h"
#include "mem_encode.h"
#include "../membrane.h"
#include "../rule.h"
#include <vector.h>
#include "binstr_compress.h"
#include "tree_compress.h"
#include "automata.h"
#include "../react_context.h"

/** ------------
 *  State
 */

typedef void*  succ_data_t;
typedef void*  state_data_t;

/* unionで書く方が良い? どうしよっかな */
//typedef union succ_data_t  succ_data_t;
//union succ_data_t {
//  State      *succ_s;
//  Transition succ_t;
//};
//typedef union state_data_t state_data_t;
//union state_data_t { /* 8(4)Byte */
//  LmnMembrane       *mem;             /*  8(4)byte: LMNtalプログラムの状態であるグローバルルート膜へのポインタ */
//  LmnBinStr          compress_mem;    /*  8(4)byte: 膜memをエンコードしたバイナリストリング */
//}

typedef BYTE TreeRootRef[6];

typedef struct TreeCompressData {
  TreeRootRef root_ref;
  unsigned short byte_length;
} TreeCompressData;

static inline void tcd_set_root_ref(TreeCompressData *tcd, uint64_t ref)
{
  memcpy(&tcd->root_ref, &ref, sizeof(TreeRootRef));
}

static inline void tcd_get_root_ref(TreeCompressData *tcd, uint64_t *ref)
{
  memcpy(ref, &tcd->root_ref, sizeof(TreeRootRef));
}

static inline unsigned short tcd_get_byte_length(TreeCompressData *data)
{
  return data->byte_length;
}

static inline void tcd_set_byte_length(TreeCompressData *data, unsigned short byte_length)
{
  data->byte_length = byte_length;
}

/* Descriptor */
struct State {                 /* Total:72(36)byte */
  unsigned int       successor_num;   /*  4(4)byte: サクセッサの数 */
  BYTE               state_name;      /*  1(1)byte: 同期積オートマトンの性質ラベル */
  BYTE               flags;           /*  1(1)byte: フラグ管理用ビットフィールド */
  BYTE               flags2;          /*  1(1)byte: フラグ管理用ビットフィールド2 */
  BYTE               flags3;          /*  1(1)byte: アラインメントの隙間(一時的にdpor_naiveで使用中) */
  unsigned long      hash;            /*  8(4)byte: 通常時: 膜memのハッシュ値, --mem-enc時: 膜の一意なバイト列のハッシュ値  */
  state_data_t       data;            /*  8(4)byte: 膜, バイナリストリングのどちらか */
  TreeCompressData   tcd;             /*  8(8)byte: Tree Compression 用のデータ無理やり8 Byteにしている */
  succ_data_t       *successors;      /*  8(4)byte: サクセッサポインタの配列 */
  State             *next;            /*  8(4)byte: 状態管理表に登録する際に必要なポインタ */
  State             *parent;          /*  8(4)byte: 自身を生成した状態へのポインタを持たせておく */
  unsigned long      state_id;        /*  8(4)byte: 生成順に割り当てる状態の整数ID */
  State             *map;             /*  8(4)byte: MAP値 or 最適化実行時の前状態 */
#ifndef MINIMAL_STATE 
  BYTE              *local_flags;     /*  8(4)byte: 並列実行時、スレッド事に保持しておきたいフラグ(mcndfsのcyanフラグ等) */
  pthread_mutex_t    expand_lock;
  unsigned long      expander_id;
#endif
#ifdef KWBT_OPT
  LmnCost            cost;            /*  8(4)byte: cost */
#endif
};

#define state_flags(S)                 ((S)->flags)
#define state_flags2(S)                ((S)->flags2)
#define state_flags3(S)                ((S)->flags3)
#ifndef MINIMAL_STATE
#define state_loflags(S)               ((S)->local_flags)
#endif

#define HASH_COMPACTION_MASK           (0x01U << 5)
#define set_on_hash_compaction(S)      (state_flags3(S) |= HASH_COMPACTION_MASK)
#define unset_on_hash_compaction(S)    (state_flags3(S) &= HASH_COMPACTION_MASK)
#define is_on_hash_compaction(S)       (state_flags3(S) &  HASH_COMPACTION_MASK)

#ifndef MINIMAL_STATE
#define state_set_expander_id(S, ID)          ((S)->expander_id = (ID))
#define state_expander_id(S)                  ((S)->expander_id)
#define state_expand_lock_init(S)             (lmn_mutex_init(&((S)->expand_lock)))
#define state_expand_lock_destroy(S)          (lmn_mutex_destroy(&((S)->expand_lock)))
#define state_expand_lock(S)                  (lmn_mutex_lock(&((S)->expand_lock)))
#define state_expand_unlock(S)                (lmn_mutex_unlock(&((S)->expand_lock)))
#else
#define state_set_expander_id(S, ID)          (NULL)
#define state_expander_id(S)                  (0)
#define state_expand_lock_init(S)             (NULL)
#define state_expand_lock_destroy(S)          (NULL)
#define state_expand_lock(S)                  (NULL)
#define state_expand_unlock(S)                (NULL)
#endif

/** Flags (8bit)
 *  0000 0001  stack上に存在する頂点であることを示すフラグ (for nested dfs)
 *  0000 0010  受理サイクル探索において探索済みの頂点であることを示すフラグ
 *  0000 0100  遷移先を計算済みであること(closed node)を示すフラグ.
 *  0000 1000  受理サイクル上の状態であることを示すフラグ
 *  0001 0000  ハッシュ表上でDummyオブジェクトであるか否かを示すフラグ (Rehash処理で使用)
 *  0010 0000  successorとしてstruct Stateを直接参照するか, struct Transitionを介して参照するかを示すフラグ.
 *  0100 0000  状態データ(union)がmemを直接的に保持する場合に立てるフラグ
 *  1000 0000  保持するバイナリストリングが階層グラフ構造に対して一意なIDであるかを示すフラグ
 */

#define ON_STACK_MASK                  (0x01U)
#define FOR_MC_MASK                    (0x01U << 1)
#define ON_CYCLE_MASK                  (0x01U << 2)
#define EXPANDED_MASK                  (0x01U << 3)
#define DUMMY_SYMBOL_MASK              (0x01U << 4)
#define TRANS_OBJ_MASK                 (0x01U << 5)
#define MEM_ENCODED_MASK               (0x01U << 6)
#define MEM_DIRECT_MASK                (0x01U << 7)

/* manipulation for flags */
#define set_on_stack(S)                ((S)->flags |=   ON_STACK_MASK)
#define unset_on_stack(S)              ((S)->flags &= (~ON_STACK_MASK))
#define is_on_stack(S)                 ((S)->flags &    ON_STACK_MASK)
#define set_snd(S)                     ((S)->flags |=   FOR_MC_MASK)
#define unset_snd(S)                   ((S)->flags &= (~FOR_MC_MASK))
#define is_snd(S)                      ((S)->flags &    FOR_MC_MASK)
#define set_expanded(S)                ((S)->flags |=   EXPANDED_MASK)
#define unset_expanded(S)              ((S)->flags &= (~EXPANDED_MASK))
#define is_expanded(S)                 ((S)->flags &    EXPANDED_MASK)
#define set_on_cycle(S)                ((S)->flags |=   ON_CYCLE_MASK)
#define unset_on_cycle(S)              ((S)->flags &= (~ON_CYCLE_MASK))
#define is_on_cycle(S)                 ((S)->flags &    ON_CYCLE_MASK)
#define set_dummy(S)                   ((S)->flags |=   DUMMY_SYMBOL_MASK)
#define unset_dummy(S)                 ((S)->flags &= (~DUMMY_SYMBOL_MASK))
#define is_dummy(S)                    ((S)->flags &    DUMMY_SYMBOL_MASK)
#define set_trans_obj(S)               ((S)->flags |=   TRANS_OBJ_MASK)
#define unset_trans_obj(S)             ((S)->flags &= (~TRANS_OBJ_MASK))
#define has_trans_obj(S)               ((S)->flags &    TRANS_OBJ_MASK)
#define set_encoded(S)                 ((S)->flags |=   MEM_ENCODED_MASK)
#define unset_encoded(S)               ((S)->flags &= (~MEM_ENCODED_MASK))
#define is_encoded(S)                  ((S)->flags &    MEM_ENCODED_MASK)
#define set_binstr_user(S)             ((S)->flags |=   MEM_DIRECT_MASK)
#define unset_binstr_user(S)           ((S)->flags &= (~MEM_DIRECT_MASK))
#define is_binstr_user(S)              ((S)->flags &    MEM_DIRECT_MASK)



/** Flags2 (8bit)
 *  0000 0001  Partial Order ReductionによるReductionマーキング(debug/demo用機能)
 *  0000 0010  D compression stateか否かを示すフラグ
 *  0000 0100  (MAPNDFS)explorer visit flag
 *  0000 1000  (MAPNDFS)generator visit flag
 *  0001 0000  (MCNDFS)blue flag
 *  0010 0000  (MCNDFS)red flag
 *  0100 0000  (Visualize)visited
 *  1000 0000
 */

#define STATE_REDUCED_MASK             (0x01U)
#define STATE_DELTA_MASK               (0x01U << 1)
#define STATE_UPDATE_MASK              (0x01U << 2)
#define EXPLORER_VISIT_MASK            (0x01U << 3)
#define GENERATOR_VISIT_MASK           (0x01U << 4)
#define STATE_BLUE_MASK                (0x01U << 5)
#define STATE_RED_MASK                 (0x01U << 6)
#define STATE_VIS_VISITED_MASK         (0x01U << 7)

/* manipulation for flags2 */
#define s_set_d(S)                     ((S)->flags2 |=   STATE_DELTA_MASK)
#define s_unset_d(S)                   ((S)->flags2 &= (~STATE_DELTA_MASK))
#define s_is_d(S)                      ((S)->flags2 &    STATE_DELTA_MASK)
#define s_set_reduced(S)               ((S)->flags2 |=   STATE_REDUCED_MASK)
#define s_unset_reduced(S)             ((S)->flags2 &= (~STATE_REDUCED_MASK))
#define s_is_reduced(S)                ((S)->flags2 &    STATE_REDUCED_MASK)
#define s_set_update(S)                ((S)->flags2 |=   STATE_UPDATE_MASK)
#define s_unset_update(S)              ((S)->flags2 &= (~STATE_UPDATE_MASK))
#define s_is_update(S)                 ((S)->flags2 &    STATE_UPDATE_MASK)

#define s_set_visited_by_explorer(S)                  ((S)->flags2 |=   EXPLORER_VISIT_MASK)
#define s_unset_visited_by_explorer(S)                ((S)->flags2 &= (~EXPLORER_VISIT_MASK))
#define s_is_visited_by_explorer(S)                   ((S)->flags2 &    EXPLORER_VISIT_MASK)
#define s_set_visited_by_generator(S)                 ((S)->flags2 |=   GENERATOR_VISIT_MASK)
#define s_unset_visited_by_generator(S)               ((S)->flags2 &= (~GENERATOR_VISIT_MASK))
#define s_is_visited_by_generator(S)                  ((S)->flags2 &    GENERATOR_VISIT_MASK)
#define s_set_unvisited(S)                 (s_unset_visited_by_explorer(S); s_unset_visited_by_generator(S))
#define s_is_unvisited(S)                  (!s_is_visited_by_explorer(S) && !s_is_visited_by_generator(S))

#define s_set_blue(S)                ((S)->flags2 |=   STATE_BLUE_MASK)
#define s_unset_blue(S)              ((S)->flags2 &= (~STATE_BLUE_MASK))
#define s_is_blue(S)                 ((S)->flags2 &    STATE_BLUE_MASK)

#define s_set_red(S)                ((S)->flags2 |=   STATE_RED_MASK)
#define s_unset_red(S)              ((S)->flags2 &= (~STATE_RED_MASK))
#define s_is_red(S)                 ((S)->flags2 &    STATE_RED_MASK)

#define s_set_visited_by_visualizer(S)                ((S)->flags2 |=   STATE_VIS_VISITED_MASK)
#define s_unset_visited_by_visualizer(S)              ((S)->flags2 &= (~STATE_VIS_VISITED_MASK))
#define s_is_visited_by_visualizer(S)                 ((S)->flags2 &    STATE_VIS_VISITED_MASK)

/** Flags3 (8bit)
 *  0000 0001 freshな状態(展開されておらず、または展開用スタックにも積まれていない状態)。fresh successor heuristcs用。
 *  0000 0010  
 *  0000 0100  
 *  0000 1000  
 *  0001 0000  
 *  0010 0000  
 *  0100 0000  
 *  1000 0000
 */


#define STATE_FRESH_MASK             (0x01U)

/* manipulation for flags2 */
#define s_set_fresh(S)                     ((S)->flags3 |=   STATE_FRESH_MASK)
#define s_unset_fresh(S)                   ((S)->flags3 &= (~STATE_FRESH_MASK))
#define s_is_fresh(S)                      ((S)->flags3 &    STATE_FRESH_MASK)


/** local flags (8bit)
 *  0000 0001  (MCNDFS)cyan flag
 *  0000 0010  
 *  0000 0100  
 *  0000 1000  
 *  0001 0000  
 *  0010 0000  
 *  0100 0000
 *  1000 0000
 */
#define STATE_CYAN_MASK             (0x01U)

/* manipulation for local flags */
#define s_set_cyan(S, i)                     ((((S)->local_flags)[i]) |=   STATE_CYAN_MASK)
#define s_unset_cyan(S, i)                   ((((S)->local_flags)[i]) &= (~STATE_CYAN_MASK))
#define s_is_cyan(S, i)                      (((S)->local_flags) && ((((S)->local_flags)[i]) &    STATE_CYAN_MASK))


/*　不必要な場合に使用する状態ID/遷移ID/性質オートマトン */
#define DEFAULT_STATE_ID       0
#define DEFAULT_TRANSITION_ID  0
#define DEFAULT_PROP_AUTOMATA  NULL

State       *state_make(LmnMembrane *mem, BYTE state_name, BOOL encode);
State       *state_make_minimal(void);
State       *state_copy(State *src, LmnMembrane *src_mem);
void         state_free(State *s);
void         state_succ_set(State *s, Vector *v);
void         state_succ_add(State *s, succ_data_t succ);
void         state_succ_clear(State *s);
void         state_free_mem(State *s);
void         state_free_binstr(State *s);
void         state_calc_mem_encode(State *s);
LmnBinStrRef    state_calc_mem_dump(State *s);
LmnBinStrRef    state_calc_mem_dump_with_z(State *s);
LmnBinStrRef    state_calc_mem_dump_with_tree(State *s);
LmnBinStrRef    state_calc_mem_dummy(State *s);
void         state_calc_hash(State *s, LmnMembrane *mem, BOOL encode);
void         state_free_compress_mem(State *s);
LmnMembrane *state_mem_copy(State *state);
int          state_cmp(State *s1, State *s2);
int          state_cmp_with_compress(State *s1, State *s2);
int          state_cmp_with_tree(State *s1, State *s2);
void         state_binstr_d_compress(State *s);
LmnBinStrRef    state_binstr_reconstructor(State *s);
void         state_calc_binstr_delta(State *s);

static inline LmnMembrane   *state_restore_mem(State *s);
static inline LmnMembrane   *state_restore_mem_inner(State *s, BOOL flag);
static inline unsigned long  state_id(State *s);
static inline void           state_id_issue(State *s);
static inline void           state_set_format_id(State *s, unsigned long v);
static inline unsigned long  state_format_id(State *s, BOOL is_formated);
static inline BYTE           state_property_state(State *s);
static inline void           state_set_property_state(State *s, BYTE label);
static inline unsigned long  state_hash(State *s);
static inline LmnMembrane   *state_mem(State *s);
static inline void           state_set_mem(State *s, LmnMembrane *mem);
static inline void           state_unset_mem(State *s);
static inline LmnBinStrRef      state_binstr(State *s);
static inline void           state_set_binstr(State *s, LmnBinStrRef bs);
static inline void           state_unset_binstr(State *s);
static inline State         *state_get_parent(State *s);
static inline void           state_set_parent(State *s, State *parent);
static inline unsigned int   state_succ_num(State *s);
static inline State         *state_succ_state(State *s, int idx);
static inline BOOL           state_succ_contains(State *s, State *t);
static inline BOOL           state_is_accept(AutomataRef a, State *s);
static inline BOOL           state_is_end(AutomataRef a, State *s);
static inline BYTE           state_scc_id(AutomataRef a, State *s);
static inline State         *state_D_ref(State *s);
static inline void           state_D_cache(State *s, LmnBinStrRef dec);
static inline LmnBinStrRef      state_D_fetch(State *s);
static inline void           state_D_flush(State *s);
static inline void           state_D_progress(State *s, LmnReactCxt *rc);
static inline void           state_update_cost(State *s,
                                               TransitionRef t,
                                               State *pre,
                                               Vector *new_ss,
                                               BOOL f,
                                               EWLock *ewlock);
static inline void           state_set_cost(State *s, LmnCost cost, State * pre);

#define state_map(S)         ((S)->map)

#ifdef KWBT_OPT
# define state_cost(S)        ((S)->cost)
#else
# define state_cost(S)        0U
#endif
#define state_cost_lock(EWLOCK, ID)   (ewlock_acquire_write(EWLOCK, ID))
#define state_cost_unlock(EWLOCK, ID) (ewlock_release_write(EWLOCK, ID))




/** ------------
 *  Transition
 */

struct Transition {
  State *s;          /*  8byte: 遷移先状態 */
  unsigned long id;  /*  8byte: State graph(=\= Automata)上の各遷移に付与されるグローバルなID．
                                ある2本の遷移が同一のものと判断される場合はこのIDの値も等しくなる． */
  Vector rule_names; /* 24byte: ルール名 複数あるのは多重辺(porなしの場合)*/
#ifdef KWBT_OPT
  LmnCost cost;            /*  8(4)byte: cost */
#endif
};

TransitionRef    transition_make(State *s, lmn_interned_str rule_name);
unsigned long transition_space(TransitionRef t);
void          transition_free(TransitionRef t);
void          transition_add_rule(TransitionRef t,
                                  lmn_interned_str rule_name,
                                  LmnCost cost);

static inline unsigned long    transition_id(TransitionRef t);
static inline void             transition_set_id(TransitionRef t, unsigned long x);
static inline int              transition_rule_num(TransitionRef t);
static inline lmn_interned_str transition_rule(TransitionRef t, int idx);
static inline State           *transition_next_state(TransitionRef t);
static inline void             transition_set_state(TransitionRef t, State *s);
static inline TransitionRef       transition(State *s, unsigned int i);
static inline LmnCost          transition_cost(TransitionRef t);
static inline void             transition_set_cost(TransitionRef t, LmnCost cost);

/** ------------
 *  Printer
 */

void dump_state_data(State *s, LmnWord _fp, LmnWord _owner);
void state_print_mem(State *s, LmnWord _fp);
void state_print_transition(State *s, LmnWord _fp, LmnWord _owner);
void state_print_label(State *s, LmnWord _fp, LmnWord _owner);
void state_print_error_path(State *s, LmnWord _fp);



/** -------
 *  inline functions
 */

/* 状態sに対応する階層グラフ構造memへのアドレスを返す.
 * memがエンコードされている場合は, デコードしたオブジェクトのアドレスを返す.
 * デコードが発生した場合のメモリ管理は呼び出し側で行う. */
static inline LmnMembrane *state_restore_mem(State *s) {
  return state_restore_mem_inner(s, TRUE);
}

/* delta-compression用のinner関数.
 * flagが真の場合, デコード済みのバイナリストリングをキャッシュから取得する.
 * キャッシュにバイナリストリングを置かないケースで使用する場合は,
 * inner関数を直接呼び出し, flagに偽を渡しておけばよい. */
static inline LmnMembrane *state_restore_mem_inner(State *s, BOOL flag) {
  if (state_mem(s)) {
    return state_mem(s);
  }
  else if (s_is_d(s)) {
    LmnBinStrRef b;
    if (flag) {
      b = state_D_fetch(s);
    } else {
      b = state_binstr_reconstructor(s);
    }
    return lmn_binstr_decode(b);
  }
  else {
    LmnBinStrRef b = state_binstr(s);
    if (lmn_env.tree_compress && b == NULL) {
      TreeNodeID ref;
      tcd_get_root_ref(&s->tcd, &ref);
      LMN_ASSERT(ref);
      LMN_ASSERT(tcd_get_byte_length(&s->tcd) != 0);
      b = lmn_bscomp_tree_decode((TreeNodeID)ref, tcd_get_byte_length(&s->tcd));
    }
    LMN_ASSERT(b);
    return lmn_binstr_decode(b);
  }
}

/* 状態sに割り当てた整数IDを返す. */
static inline unsigned long state_id(State *s) {
  return s->state_id;
}

/* 状態sに対して, 状態固有となる整数IDを割り当てる.
 * 即ち, 実行スレッド間で, 同じ整数IDは使用されない.
 * 既にIDが割り当てられている場合はなにもしない. */
static inline void state_id_issue(State *s) {
  if (s->state_id == 0) {
    /* 状態ID用に設置したTLS機構から整数IDを生成して割り当てる. */
    s->state_id = env_gen_state_id();
  }
}

/* 特殊な整数ID(format id) vを状態sに割り当てる.
 * 状態遷移グラフをCUIにプリントする際, 可視性を向上させるために呼び出す.
 * 状態遷移グラフの構築および探索中に使用してはならない */
static inline void state_set_format_id(State *s, unsigned long v) {
  /* ハッシュ値が不要となる段階で呼び出すため, hash値の領域を再利用する */
  s->hash = v;
}

/* 状態sに割り当てた特殊な整数ID(format id)を返す.
 * format_idを割り当てていない場合(is_formated==FALSE)や
 * 状態遷移グラフを構築しながら状態データを出力する場合は,
 * 状態生成時に割り当てた整数IDをそのまま返す. */
static inline unsigned long state_format_id(State *s, BOOL is_formated) {
  if (is_formated && lmn_env.sp_dump_format != INCREMENTAL) {
    return s->hash;
  } else {
    return state_id(s);
  }
}

/* 状態sに割り当てた性質オートマトン上の状態名を返す.
 * 性質オートマトンを使用していない場合は, 戻り値は0 */
static inline BYTE state_property_state(State *s) {
  return s->state_name;
}

/* 状態sに性質オートマトン上の状態名labelを割り当てる. */
static inline void state_set_property_state(State *s, BYTE label) {
  s->state_name = label;
}

/* 状態sに対応するハッシュ値を返す. */
static inline unsigned long state_hash(State *s) {
  /* state_property_stateは0の場合があるので+1する */
  return s->hash * (state_property_state(s) + 1);
}

/* 状態sに対応する階層グラフ構造を返す.
 * 既にバイナリストリングへエンコードしている場合の呼び出しは想定外. */
static inline LmnMembrane *state_mem(State *s) {
  if (is_binstr_user(s)) {
    return NULL;
  } else {
    return (LmnMembrane *)s->data;
  }
}

/* 状態sに階層グラフ構造memを割り当てる.
 * sに対してバイナリストリングBを割り当てている場合は, Bのメモリ管理は呼出し側で行う*/
static inline void state_set_mem(State *s, LmnMembrane *mem) {
  unset_binstr_user(s);
  s->data = (state_data_t)mem;
}

/* 状態sが参照する階層グラフ構造用の領域をクリアする.
 * 階層グラフ構造の参照を持たない場合は, なにもしない. */
static inline void state_unset_mem(State *s) {
  if (!is_binstr_user(s)) {
    state_set_mem(s, NULL);
  }
}

/* 状態sに割り当てたバイナリストリングを返す. */
static inline LmnBinStrRef state_binstr(State *s) {
  if (is_binstr_user(s)) {
    return (LmnBinStrRef)s->data;
  } else {
    return NULL;
  }
}

/* 状態sに対応する階層グラフ構造からエンコードしたバイナリストリングbsを, sに割り当てる　*/
static inline void state_set_binstr(State *s, LmnBinStrRef bs) {
  s->data = (state_data_t)bs;
  set_binstr_user(s);
}

/* 状態sが参照するバイナリストリング用の領域をクリアする.
 * バイナリストリングに対する参照を持たない場合は, なにもしない. */
static inline void state_unset_binstr(State *s) {
  if (is_binstr_user(s)) {
    s->data = (state_data_t)NULL;
    unset_binstr_user(s);
  }
}

/* 状態sを生成した状態(親ノード)へのアドレスを返す. */
static inline State *state_get_parent(State *s) {
  return s->parent;
}

/* 状態sに, sを生成した状態(親ノード)へのアドレスを割り当てる. */
static inline void state_set_parent(State *s, State *parent) {
  s->parent = parent;
}

/* 状態sから遷移可能な状態数を返す. */
static inline unsigned int state_succ_num(State *s) {
  return s->successor_num;
}

/* 状態sから遷移可能な状態の集合から, idx番目の状態を返す. */
static inline State *state_succ_state(State *s, int idx) {
  /* successorデータはTransitionがある場合とそうでない場合とで処理が異なる */
  if (has_trans_obj(s)) {
    return transition_next_state((TransitionRef)s->successors[idx]);
  } else {
    return (State *)s->successors[idx];
  }
}

/* 状態sから遷移可能な状態集合に, 状態tが含まれている場合に真を返す.
 * O(state_succ_num(s))と効率的ではないため, 可能な限り利用しない. */
static inline BOOL state_succ_contains(State *s, State *t) {
  unsigned int i;
  for (i = 0; i < state_succ_num(s); i++) {
    State *succ = state_succ_state(s, i);
    if (succ == t) return TRUE;
  }
  return FALSE;
}

/* 状態sが, 性質オートマトンa上のaccept状態に対応している(受理状態)ならば真を返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す. */
static inline BOOL state_is_accept(AutomataRef a, State *s) {
  if (a) {
    return atmstate_is_accept(automata_get_state(a, state_property_state(s)));
  } else {
    return FALSE;
  }
}

/* 状態sが, 性質オートマトンa上のend状態に対応している(invalid end state)ならば真を返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * TOFIX: rename (end --> invalid end) */
static inline BOOL state_is_end(AutomataRef a, State *s) {
  if (a) {
    return atmstate_is_end(automata_get_state(a, state_property_state(s)));
  } else {
    return FALSE;
  }
}

/* 状態sに対応する性質オートマトンa上の状態が, 属している強連結成分(scc)のグループIDを返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * 性質オートマトンaに対して強連結成分分解を行っていない場合の戻り値は, 未定義. */
static inline BYTE state_scc_id(AutomataRef a, State *s) {
  if (a) {
    return atmstate_scc_type(automata_get_state(a, state_property_state(s)));
  } else {
    return FALSE;
  }
}

/* 状態sとの差分計算の対象とする状態に対する参照を返す. */
static inline State *state_D_ref(State *s) {
  /* とりあえず親ノードにした */
  return state_get_parent(s);
}

/* 状態sに対応する非圧縮バイナリストリングdをキャッシングする. */
static inline void state_D_cache(State *s, LmnBinStrRef d) {
  LMN_ASSERT(!state_D_fetch(s));  
  /* メモリ節約の結果, 保守性ないコード. 注意 */
  s->successors = (succ_data_t *)d;
}

/* キャッシングしておいた状態sに対応する非圧縮バイナリストリングに対する参照を返す. */
static inline LmnBinStrRef state_D_fetch(State *s) {
  if (s_is_d(s)) {
    return (LmnBinStrRef)s->successors;
  } else {
    return NULL;
  }
}

/* 状態sに対応する非圧縮バイナリストリングのキャッシュをクリアする. */
void state_D_flush(State *s) {
  LmnBinStrRef cached = state_D_fetch(s);
  if (cached) {
    lmn_binstr_free(cached);
  }
  s->successors = NULL;
}

/* 差分圧縮バイト列に基づく状態生成処理のfinalizeを行う. */
static inline void state_D_progress(State *s, LmnReactCxt *rc) {
  RC_D_PROGRESS(rc);
  state_D_flush(s);
}

/* MT-unsafe */
static inline void state_set_cost(State *s, LmnCost cost, State * pre) {
#ifdef KWBT_OPT
  s->cost = cost;
#endif
  s->map  = pre;
}

/* 状態sのcostが最適ならば更新し、状態sを遷移先更新状態にする
 * f==true: minimize
 * f==false: maximize */
static inline void state_update_cost(State *s,
                                     TransitionRef t,
                                     State *pre,
                                     Vector *new_ss,
                                     BOOL f,
                                     EWLock *ewlock)
{
  LmnCost cost;
  cost = transition_cost(t) + state_cost(pre);
  if (env_threads_num() >= 2) state_cost_lock(ewlock, state_hash(s));
  if ((f && state_cost(s) > cost) || (!f && state_cost(s) < cost)) {
    state_set_cost(s, cost, pre);
    if (is_expanded(s) && new_ss) vec_push(new_ss, (vec_data_t)s);
    s_set_update(s);
  }
  if (env_threads_num() >= 2) state_cost_unlock(ewlock, state_hash(s));
}

/* 遷移tに割り当てたidを返す. */
static inline unsigned long transition_id(TransitionRef t) {
  return t->id;
}

/* 遷移tに整数ID idを割り当てる. */
static inline void transition_set_id(TransitionRef t, unsigned long id) {
  t->id = id;
}

static inline int transition_rule_num(TransitionRef t) {
  return vec_num(&t->rule_names);
}

static inline lmn_interned_str transition_rule(TransitionRef t, int idx) {
  return vec_get(&t->rule_names, idx);
}

static inline State *transition_next_state(TransitionRef t) {
  return t->s;
}

static inline void transition_set_state(TransitionRef t, State *s) {
  t->s = s;
}

static inline TransitionRef transition(State *s, unsigned int i) {
  return (TransitionRef)(s->successors[i]);
}

static inline LmnCost transition_cost(TransitionRef t) {
#ifdef KWBT_OPT
  return t->cost;
#else
  return 0;
#endif
}

static inline void transition_set_cost(TransitionRef t, LmnCost cost) {
#ifdef KWBT_OPT
  t->cost = cost;
#endif
}

#endif
