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

/**
 * @ingroup  Verifier
 * @defgroup State
 * @{
 */

#include "../lmntal.h"
#include "automata.h"
#include "binstr_compress.h"
#include "element/element.h"
#include "mem_encode.h"
#include "state_defs.h"
#include "tree_compress.h"
#include "vm/vm.h"

/** ------------
 *  State
 */

void tcd_set_root_ref(TreeCompressData *tcd, uint64_t ref);
void tcd_get_root_ref(TreeCompressData *tcd, uint64_t *ref);
unsigned short tcd_get_byte_length(TreeCompressData *data);
void tcd_set_byte_length(TreeCompressData *data, unsigned short byte_length);

struct State;

/** Flags3 (8bit)
 *  0000 0001
 * freshな状態(展開されておらず、または展開用スタックにも積まれていない状態)。fresh
 * successor heuristcs用。 0000 0010 0000 0100 0000 1000 0001 0000 0010 0000
 *  0100 0000
 *  1000 0000
 */

#define STATE_FRESH_MASK (0x01U)

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
#define STATE_CYAN_MASK (0x01U)

/*　不必要な場合に使用する状態ID/遷移ID/性質オートマトン */
#define DEFAULT_STATE_ID 0
#define DEFAULT_TRANSITION_ID 0
#define DEFAULT_PROP_AUTOMATA NULL

LmnBinStrRef state_calc_mem_dump(State *s);
LmnBinStrRef state_calc_mem_dump_with_z(State *s);
LmnBinStrRef state_calc_mem_dump_with_tree(State *s);
LmnBinStrRef state_calc_mem_dummy(State *s);
int state_cmp(State *s1, State *s2);
int state_cmp_with_compress(State *s1, State *s2);
int state_cmp_with_tree(State *s1, State *s2);
LmnMembraneRef state_restore_mem(State *s);
unsigned long state_id(State *s);
void state_id_issue(State *s);
void state_set_format_id(State *s, unsigned long v);
unsigned long state_format_id(State *s, BOOL is_formated);
BYTE state_property_state(State *s);
void state_set_property_state(State *s, BYTE label);
unsigned long state_hash(State *s);
void state_unset_mem(State *s);
void state_unset_binstr(State *s);
State *state_get_parent(State *s);
void state_set_parent(State *s, State *parent);
State *state_succ_state(State *s, int idx);
BOOL state_succ_contains(State *s, State *t);
BOOL state_is_accept(AutomataRef a, State *s);
BOOL state_is_end(AutomataRef a, State *s);
BYTE state_scc_id(AutomataRef a, State *s);
State *state_D_ref(State *s);
void state_D_cache(State *s, LmnBinStrRef dec);
LmnBinStrRef state_D_fetch(State *s);
void state_D_flush(State *s);
void state_D_progress(State *s, LmnReactCxtRef rc);
void state_update_cost(State *s, TransitionRef t, State *pre, Vector *new_ss,
                       BOOL f, EWLock *ewlock);
void state_set_cost(State *s, LmnCost cost, State *pre);

LmnCost state_cost(State *S);
void state_cost_lock(EWLock *EWLOCK, mtx_data_t ID);
void state_cost_unlock(EWLock *EWLOCK, mtx_data_t ID);

/** ------------
 *  Transition
 */

struct Transition {
  State *s; /*  8byte: 遷移先状態 */
  unsigned long
      id; /*  8byte: State graph(=\=
             Automata)上の各遷移に付与されるグローバルなID．
                     ある2本の遷移が同一のものと判断される場合はこのIDの値も等しくなる．
           */
  Vector rule_names; /* 24byte: ルール名 複数あるのは多重辺(porなしの場合)*/
#ifdef KWBT_OPT
  LmnCost cost; /*  8(4)byte: cost */
#endif
};

TransitionRef transition_make(State *s, lmn_interned_str rule_name);
unsigned long transition_space(TransitionRef t);
void transition_free(TransitionRef t);
void transition_add_rule(TransitionRef t, lmn_interned_str rule_name,
                         LmnCost cost);

unsigned long transition_id(TransitionRef t);
void transition_set_id(TransitionRef t, unsigned long x);
int transition_rule_num(TransitionRef t);
lmn_interned_str transition_rule(TransitionRef t, int idx);
State *transition_next_state(TransitionRef t);
void transition_set_state(TransitionRef t, State *s);
TransitionRef transition(State *s, unsigned int i);
LmnCost transition_cost(TransitionRef t);
void transition_set_cost(TransitionRef t, LmnCost cost);

/** ------------
 *  Printer
 */

class StateDumper {
public:
  void dump_state_data(State *s, FILE *_fp, const StateSpace *_owner);
  void state_print_mem(State *s, FILE *_fp);
  void state_print_transition(State *s, FILE *_fp, const StateSpace *_owner);
  void state_print_label(State *s, FILE *_fp, const StateSpace *_owner);
  void state_print_error_path(State *s, FILE *_fp);
};

/** -------
 *  inline functions
 */

/* 状態sに対応する階層グラフ構造memへのアドレスを返す.
 * memがエンコードされている場合は, デコードしたオブジェクトのアドレスを返す.
 * デコードが発生した場合のメモリ管理は呼び出し側で行う. */
LmnMembraneRef state_restore_mem(State *s);

/* 状態sに割り当てた整数IDを返す. */
unsigned long state_id(State *s);

/* 状態sに対して, 状態固有となる整数IDを割り当てる.
 * 即ち, 実行スレッド間で, 同じ整数IDは使用されない.
 * 既にIDが割り当てられている場合はなにもしない. */
void state_id_issue(State *s);

/* 特殊な整数ID(format id) vを状態sに割り当てる.
 * 状態遷移グラフをCUIにプリントする際, 可視性を向上させるために呼び出す.
 * 状態遷移グラフの構築および探索中に使用してはならない */
void state_set_format_id(State *s, unsigned long v);

/* 状態sに割り当てた特殊な整数ID(format id)を返す.
 * format_idを割り当てていない場合(is_formated==FALSE)や
 * 状態遷移グラフを構築しながら状態データを出力する場合は,
 * 状態生成時に割り当てた整数IDをそのまま返す. */
unsigned long state_format_id(State *s, BOOL is_formated);

/* 状態sに割り当てた性質オートマトン上の状態名を返す.
 * 性質オートマトンを使用していない場合は, 戻り値は0 */
BYTE state_property_state(State *s);
/* 状態sに性質オートマトン上の状態名labelを割り当てる. */
void state_set_property_state(State *s, BYTE label);

/* 状態sに対応するハッシュ値を返す. */
unsigned long state_hash(State *s);

/* 状態sに対応する階層グラフ構造を返す.
 * 既にバイナリストリングへエンコードしている場合の呼び出しは想定外. */
LmnMembraneRef state_mem(State *s);

/* 状態sに階層グラフ構造memを割り当てる.
 * sに対してバイナリストリングBを割り当てている場合は,
 * Bのメモリ管理は呼出し側で行う*/
void state_set_mem(State *s, LmnMembraneRef mem);

/* 状態sが参照する階層グラフ構造用の領域をクリアする.
 * 階層グラフ構造の参照を持たない場合は, なにもしない. */
void state_unset_mem(State *s);

/* 状態sに割り当てたバイナリストリングを返す. */
LmnBinStrRef state_binstr(State *s);

/* 状態sに対応する階層グラフ構造からエンコードしたバイナリストリングbsを,
 * sに割り当てる　*/
void state_set_binstr(State *s, LmnBinStrRef bs);

/* 状態sが参照するバイナリストリング用の領域をクリアする.
 * バイナリストリングに対する参照を持たない場合は, なにもしない. */
void state_unset_binstr(State *s);

/* 状態sを生成した状態(親ノード)へのアドレスを返す. */
State *state_get_parent(State *s);

/* 状態sに, sを生成した状態(親ノード)へのアドレスを割り当てる. */
void state_set_parent(State *s, State *parent);

/* 状態sから遷移可能な状態の集合から, idx番目の状態を返す. */
State *state_succ_state(State *s, int idx);

/* 状態sから遷移可能な状態集合に, 状態tが含まれている場合に真を返す.
 * O(state_succ_num(s))と効率的ではないため, 可能な限り利用しない. */
BOOL state_succ_contains(State *s, State *t);

/* 状態sが,
 * 性質オートマトンa上のaccept状態に対応している(受理状態)ならば真を返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す. */
BOOL state_is_accept(AutomataRef a, State *s);

/* 状態sが, 性質オートマトンa上のend状態に対応している(invalid end
 * state)ならば真を返す. 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * TOFIX: rename (end --> invalid end) */
BOOL state_is_end(AutomataRef a, State *s);

/* 状態sに対応する性質オートマトンa上の状態が,
 * 属している強連結成分(scc)のグループIDを返す.
 * 性質オートマトンaが存在しない場合は直ちに偽を返す.
 * 性質オートマトンaに対して強連結成分分解を行っていない場合の戻り値は, 未定義.
 */
BYTE state_scc_id(AutomataRef a, State *s);

/* 状態sとの差分計算の対象とする状態に対する参照を返す. */
State *state_D_ref(State *s);

/* 状態sに対応する非圧縮バイナリストリングdをキャッシングする. */
void state_D_cache(State *s, LmnBinStrRef d);

/* キャッシングしておいた状態sに対応する非圧縮バイナリストリングに対する参照を返す.
 */
LmnBinStrRef state_D_fetch(State *s);

/* 状態sに対応する非圧縮バイナリストリングのキャッシュをクリアする. */
void state_D_flush(State *s);

/* 差分圧縮バイト列に基づく状態生成処理のfinalizeを行う. */
void state_D_progress(State *s, LmnReactCxtRef rc);

/* MT-unsafe */
void state_set_cost(State *s, LmnCost cost, State *pre);

/* 状態sのcostが最適ならば更新し、状態sを遷移先更新状態にする
 * f==true: minimize
 * f==false: maximize */
void state_update_cost(State *s, TransitionRef t, State *pre, Vector *new_ss,
                       BOOL f, EWLock *ewlock);

/* 遷移tに割り当てたidを返す. */
unsigned long transition_id(TransitionRef t);

/* 遷移tに整数ID idを割り当てる. */
void transition_set_id(TransitionRef t, unsigned long id);

int transition_rule_num(TransitionRef t);
lmn_interned_str transition_rule(TransitionRef t, int idx);

State *transition_next_state(TransitionRef t);

void transition_set_state(TransitionRef t, State *s);

TransitionRef transition(State *s, unsigned int i);

LmnCost transition_cost(TransitionRef t);

void transition_set_cost(TransitionRef t, LmnCost cost);
/* @} */

#endif
