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

#include "lmntal.h"
#include "lmntal_thread.h"
#include "mem_encode.h"
#include "membrane.h"
#include "rule.h"
#include "vector.h"


typedef struct State       State;
typedef struct Transition *Transition;

typedef void*    succ_data_t;

struct State { /* 62(38)byte : 64(32)bit環境 */
  BOOL               flags;           /*  1(1)byte: 各状態に対する操作を記録するビットフィールド */
  BYTE               state_name;      /*  1(1)byte: 同期積オートマトンの性質ラベル */
#if SIZEOF_VOIDP < 8
  uint16_t           predecessor_num; /*  0(2)byte: プレデセッサの数. とりあえずアラインメントの隙間を利用しておくにとどめる */
#endif
  unsigned int       successor_num;   /*  4(4)byte: サクセッサの数 */
#if SIZEOF_VOIDP == 8
  uint16_t           predecessor_num; /*  0(2)byte: プレデセッサの数. とりあえずアラインメントの隙間を利用しておくにとどめる */
#endif
  succ_data_t       *successors;      /*  8(4)byte: サクセッサ列の先頭ポインタ */
  unsigned long      hash;            /*  8(4)byte: 通常時: 膜memのハッシュ値, --mem-enc時: 膜の一意なバイト列のハッシュ値  */
  LmnMembrane       *mem;             /*  8(4)byte: グローバルルート膜へのポインタ */
  LmnBinStr          compress_mem;    /*  8(4)byte: 通常時: 膜のバイト列, --mem-enc時: 膜の一意なバイト列 */
  State             *next;            /*  8(4)byte: 状態管理表に登録する際に必要なポインタ */
  State             *predecessor;     /*  8(4)byte: 自身を生成した状態へのポインタを持たせておく */
  unsigned long      state_id;        /*  8(4)byte: 状態の整数ID */
};

struct Transition {
  State *s;          /*  8byte: 遷移先状態 */
  unsigned long id;  /*  8byte: State graph(=\= Automata)上の各遷移に付与されるグローバルなID．
                                ある2本の遷移が同一のものと判断される場合はこのIDの値も等しくなる． */
  Vector rule_names; /* 24byte: ルール名 */
};


/** Flags (8bit)
 *  0000 0001  stack上に存在する頂点であることを示すフラグ (for nested dfs)
 *  0000 0010  受理サイクル探索を行った頂点であることを示すフラグ(for nested DFS)
 *  0000 0100  遷移先を計算済みであること(closed node)を示すフラグ.
 *  0000 1000  受理サイクル上の状態であることを示すフラグ
 *  0001 0000  flag to show that it was not reduced by the partial order reduction
 *  0010 0000  flag to show that it was calculated mem-id (not mem-dump)
 *  0100 0000  flag to show that the hash value might collide in this state with predecessor
 *
 */

#define ON_STACK_MASK                  (0x01U)
#define SND_MASK                       (0x01U << 1)
#define EXPANDED_MASK                  (0x01U << 3)
#define ON_CYCLE_MASK                  (0x01U << 4)
#define REPRESENTATIVE_MASK            (0x01U << 5)
#define MEM_ENCODED_MASK               (0x01U << 6)
#define DUMMY_SYMBOL_MASK              (0x01U << 7)

#define set_on_stack(S)                ((S)->flags |= ON_STACK_MASK)
#define unset_on_stack(S)              ((S)->flags &= (~ON_STACK_MASK))
#define is_on_stack(S)                 ((S)->flags & ON_STACK_MASK)
#define set_snd(S)                     ((S)->flags |= SND_MASK)
#define unset_snd(S)                   ((S)->flags &= (~SND_MASK))
#define is_snd(S)                      ((S)->flags & SND_MASK)
#define set_expanded(S)                ((S)->flags |= EXPANDED_MASK)
#define unset_expanded(S)              ((S)->flags &= (~EXPANDED_MASK))
#define is_expanded(S)                 ((S)->flags & EXPANDED_MASK)
#define set_on_cycle(S)                ((S)->flags |= ON_CYCLE_MASK)
#define unset_on_cycle(S)              ((S)->flags &= (~ON_CYCLE_MASK))
#define is_on_cycle(S)                 ((S)->flags & ON_CYCLE_MASK)
#define set_ample(S)                   ((S)->flags |= REPRESENTATIVE_MASK)
#define is_ample(S)                    ((S)->flags & REPRESENTATIVE_MASK)
#define set_encoded(S)                 ((S)->flags |= MEM_ENCODED_MASK)
#define unset_encoded(S)               ((S)->flags &= (~MEM_ENCODED_MASK))
#define is_encoded(S)                  ((S)->flags & MEM_ENCODED_MASK)
#define set_dummy(S)                   ((S)->flags |= DUMMY_SYMBOL_MASK)
#define unset_dummy(S)                 ((S)->flags &= (~DUMMY_SYMBOL_MASK))
#define is_dummy(S)                    ((S)->flags & DUMMY_SYMBOL_MASK)

/*　不必要な場合に使用する状態ID/遷移ID/性質オートマトン */
#define DEFAULT_STATE_ID       0
#define DEFAULT_TRANSITION_ID  0
#define DEFAULT_PROP_AUTOMATA  NULL

extern struct st_hash_type type_statehash;

#define state_id(S)                    ((S)->state_id)
#define state_id_issue(S)              ((S)->state_id = ((env_gen_state_id() << 8) | lmn_thread_id))
#define state_id_pp_id(S)              ((S)->state_id >> 8)
#define state_id_pp_proc(S)            ((S)->state_id & 0xffU)
#define state_set_format_id(S, V)      ((S)->hash = (V))
#define state_format_id(S)             (mc_data.is_format_states ? (S)->hash \
                                                                 : state_id(S))
#define state_property_state(S)        ((S)->state_name)
#define state_hash(S)                  ((S)->hash * (state_property_state(S) + 1)) /* +1 は state_nameが0の場合があるため */
#define state_set_property_state(S, L) ((S)->state_name = (L))
#define state_mem(S)                   ((S)->mem)
#define state_set_mem(S, M)            ((S)->mem = (M))
#define state_mem_binstr(S)            ((S)->compress_mem)
#define state_set_compress_mem(S, BS)  ((S)->compress_mem = (BS))
#define state_get_predecessor(S)       ((S)->predecessor)
#define state_set_predecessor(S, P)    ((S)->predecessor = (P))
#define state_inc_predecessor_num(S)   (ADD_AND_FETCH(state_get_predecessor(S), 1))
#define state_dec_predecessor_num(S)   (SUB_AND_FETCH(state_get_predecessor(S), 1))
#define state_succ_num(S)              ((S)->successor_num)
#define state_succ_get(S, I)           (lmn_env.show_transition ? transition_next_state((Transition)((S)->successors[I])) \
                                                                 : (State *)((S)->successors[I]))
#define state_restore_mem(S)           (state_mem(S) ? state_mem(S) \
                                                     : lmn_binstr_decode(state_mem_binstr(S)))
#define state_succ_set_sub(S, SUCCS)                                \
  do {                                                              \
    state_succ_num(S) = vec_num(SUCCS);                             \
    if (state_succ_num(S) > 0) {                                    \
      unsigned int __i;                                             \
      (S)->successors = LMN_NALLOC(succ_data_t, state_succ_num(S)); \
      for (__i = 0; __i < state_succ_num(S); __i++) {               \
        (S)->successors[__i] = (succ_data_t)vec_get(SUCCS, __i);    \
      }                                                             \
    }                                                               \
  } while (0)
#ifdef PROFILE
#  define  state_succ_set(S, SUCCS)                                 \
  do {                                                              \
    if (lmn_env.profile_level >= 3) {                               \
      profile_add_space(PROFILE_SPACE__TRANS_OBJECT,                \
                        sizeof(succ_data_t) * vec_num(SUCCS));      \
      profile_remove_space(PROFILE_SPACE__TRANS_OBJECT, 0);         \
    }                                                               \
    state_succ_set_sub(S, SUCCS);                                   \
  } while (0)
#else
#  define  state_succ_set(S, SUCCS) state_succ_set_sub(S, SUCCS)
#endif


State *state_make(LmnMembrane *mem, BYTE state_name, BOOL encode);
inline State *state_make_minimal(void);
State *state_copy(State *src);
inline State *state_copy_with_mem(State *src, LmnMembrane *mem);
void state_free(State *s);
inline void state_free_mem(State *s);
inline void state_calc_mem_encode(State *s);
inline LmnBinStr state_calc_mem_dump(State *s);
inline LmnBinStr state_calc_mem_dump_with_z(State *s);
inline LmnBinStr state_calc_mem_dummy(State *s);
inline void state_calc_hash(State *s, LmnMembrane *mem, BOOL encode);
inline void state_free_compress_mem(State *s);
inline LmnMembrane *state_copied_mem(State *state);
int state_cmp(State *s1, State *s2);
int state_cmp_with_compress(State *s1, State *s2);


#define transition_id(T)             ((T)->id)
#define transition_set_id(T, I)      ((T)->id = (I))
#define transition_rule_num(T)       (vec_num(&((T)->rule_names)))
#define transition_rule(T, Idx)      (vec_get(&((T)->rule_names), (Idx)))
#define transition_next_state(T)     ((T)->s)
#define transition_set_state(T, S)   ((T)->s = (S))

Transition transition(State *s, unsigned int i);
Transition transition_make(State *s, lmn_interned_str rule_name);
Transition transition_make_with_id(State *s, unsigned long id, lmn_interned_str rule);
inline unsigned long transition_space(Transition t);
void transition_free(Transition t);
void transition_add_rule(Transition t, lmn_interned_str rule_name);

void state_set_id_for_dump(State *s, LmnWord _d);
void dump_state_data(State *s, LmnWord _fp);
void state_print_mem(State *s, LmnWord _fp);
void state_print_transition(State *s, LmnWord _fp);
void state_print_label(State *s, LmnWord _fp);
void state_print_error_path(State *s, LmnWord _fp);

#endif
