/*
 * mc.h
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

#ifndef MC_H
#define MC_H

#include "st.h"
#include "internal_hash.h"
#include "membrane.h"
#include "vector.h"
#include "automata.h"
#include "rule.h"

typedef struct State State;
typedef struct StateTransition StateTransition;

struct State {
  LmnMembrane *mem;     /* グローバルルート膜 */
  unsigned long hash; /* mhash(mem) */
  BOOL flags;           /* nested DFSの際にDFS{1,2}いずれの走査を受けたかや，successorが展開済であるか否かを管理するフラグ */
  Vector successor;     /* 通常時: Vector of States，ample(s)計算中: Vector of StateTransitions */
  lmn_interned_str rule_name;
  BYTE state_name;
};

/* 訪問済みノードが格納される */
st_table *States;

/* 展開されたシステムの状態が格納される */
st_table *expanded;

struct StateTransition {
  State *succ_state;     /* 遷移先状態 */
  unsigned long id;    /* State graph(=\= Automata)上の各遷移に付与されるグローバルなID．ある2本の遷移が同一のものと判断される場合はこのIDの値も等しくなる． */
  LmnRule rule;          /* 本遷移の際に適用されるシステムルール */
};

/**
 * task.cのinterpret()で用いられるフラグなど
 *
 * nd_exec: 非決定的実行フラグ
 *   初期化ルール適用以前：FALSE
 *   初期化ルール適用以降：TRUE
 * system_rule_committed: ボディ実行中フラグ
 *   ボディ実行中のみTRUE (i.e. ルールのボディ部冒頭のCOMMIT命令の処理開始時にTRUEとなり、PROCEED命令の処理終了時にFALSEになる)
 *   左辺に出現するPROCEED（GROUP終了を表す）と右辺に出現するPROCEEDを区別するために使用
 * system_rule_proceeded: システムルール適用成功フラグ
 *   システムルール適用成功時：TRUE
 *   システムルール実行時はinterpret()が常にFALSEを返す仕様となっているため、システムルール適用成功を表すフラグとして代わりにこれを用いる
 * property_rule: 性質ルール実行中フラグ
 *   性質ルール適用成功時にTRUEを返す目的で使用
 * initial_state: 非決定的実行時の初期状態
 * calculating_ample: ample(s)計算中フラグ
 *   PORが有効かつample/1(c.f. por.c)実行中のみTRUE
 */
typedef struct McFlags {
  BOOL nd_exec;
  BOOL system_rule_committed;
  BOOL system_rule_proceeded;
  BOOL property_rule;
  BYTE property_state;
  State *initial_state;
  BOOL calculating_ample;
} McFlags;

typedef struct MCData { /* TODO: 構造体の名前 */
  Automata property_automata;
  Vector *propsyms;
  unsigned long next_strans_id;
  StateTransition *prev_strans;
} MCData;

McFlags mc_flags;
MCData mc_data;

LMN_EXTERN State *state_make(LmnMembrane *mem, BYTE state_name, lmn_interned_str rule);
State *state_make_for_nd(LmnMembrane *mem, lmn_interned_str rule);
LMN_EXTERN State *state_copy(State *s);
LMN_EXTERN inline void state_succ_init(State *s, int init_size);
LMN_EXTERN void state_free(State *s);
LMN_EXTERN void strans_free(StateTransition *strans);
BYTE state_property_state(State *state);
LMN_EXTERN StateTransition *strans_make(State *s, unsigned long id, LmnRule rule);

/* flag of the first DFS (nested DFS, on-stack state) */
#define FST_MASK                   (0x01U)
/* flag of the second DFS (nested DFS, visited state) */
#define SND_MASK                   (0x01U << 1)
/* flag to show that it is on the search stack */
#define ON_STACK_MASK              (0x01U << 2)
/* flag to show that all its successor states(and transitions) are expanded */
#define EXPANDED_MASK              (0x01U << 3)
/* flag to show that all the independency relations between transitions enabled on the state are checked */
#define INDEPENDENCY_CHECKED_MASK  (0x01U << 4)
/* flag to show that it was not reduced by the partial order reduction */
#define REPRESENTATIVE_MASK        (0x01U << 5)
/* macros for nested DFS */
#define set_fst(S)                     ((S)->flags |= FST_MASK)
#define unset_fst(S)                   ((S)->flags &= (~FST_MASK))
#define is_fst(S)                      ((S)->flags & FST_MASK)
#define set_snd(S)                     ((S)->flags |= SND_MASK)
#define unset_snd(S)                   ((S)->flags &= (~SND_MASK))
#define is_snd(S)                      ((S)->flags & SND_MASK)
#define set_open(S)                    ((S)->flags |= ON_STACK_MASK)
#define unset_open(S)                  ((S)->flags &= (~ON_STACK_MASK))
#define is_open(S)                     ((S)->flags & ON_STACK_MASK)
#define set_expanded(S)                ((S)->flags |= EXPANDED_MASK)
#define unset_expanded(S)              ((S)->flags &= (~EXPANDED_MASK))
#define is_expanded(S)                 ((S)->flags & EXPANDED_MASK)
#define set_independency_checked(S)    ((S)->flags |= INDEPENDENCY_CHECKED_MASK)
#define unset_independency_checked(S)  ((S)->flags &= (~INDEPENDENCY_CHECKED_MASK))
#define is_independency_checked(S)     ((S)->flags & INDEPENDENCY_CHECKED_MASK)
#define set_ample(S)                   ((S)->flags |= REPRESENTATIVE_MASK)
#define is_ample(S)                    ((S)->flags & REPRESENTATIVE_MASK)

LMN_EXTERN long state_hash(State *s);
LMN_EXTERN int state_cmp(HashKeyType s1, HashKeyType s2);

LMN_EXTERN inline void activate_ancestors(LmnMembrane *mem);

int mc_load_property(Automata *a, PVector *prop_defs);
LMN_EXTERN void mc_explain_error(int error_id);
LMN_EXTERN char *mc_error_msg(int error_id);
#endif
