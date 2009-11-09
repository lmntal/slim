/*
 * task.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: task.c,v 1.37 2008/10/21 11:31:58 riki Exp $
 */


#include "atom.h"
#include "dumper.h"
#include "instruction.h"
#include "lmntal.h"
#include "membrane.h"
#include "rule.h"
#include "task.h"
#include "vector.h"
#include "symbol.h"
#include "functor.h"
#include <stdio.h>
#include "st.h"
#include "mc.h"
#include "mhash.h"
#include "por.h"
#include "automata.h"
#include "propositional_symbol.h"
#include "functor.h"
#include "error.h"
#include "ccallback.h"
#include "special_atom.h"
#include "slim_header/string.h"
#include "slim_header/memstack.h"
#include "slim_header/port.h"
#include "react_context.h"
#include "visitlog.h"
#include <string.h>

#ifdef PROFILE
#include "runtime_status.h"
#endif

LmnWord *wt, *wt_t; /* variable vector used in interpret */
LmnByte *at, *at_t; /* attribute vector */
unsigned int wt_size;

unsigned int trace_num = 0; /* for tracer */

typedef void (* callback_0)(ReactCxt,
                            LmnMembrane *);
typedef void (* callback_1)(ReactCxt,
                            LmnMembrane *,
                            LmnAtom, LmnLinkAttr);
typedef void (* callback_2)(ReactCxt,
                            LmnMembrane *,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr);
typedef void (* callback_3)(ReactCxt,
                            LmnMembrane *,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr);
typedef void (* callback_4)(ReactCxt,
                            LmnMembrane *,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr,
                            LmnAtom, LmnLinkAttr);


inline Vector *links_from_idxs(const Vector *link_idxs, LmnWord *wt, LmnByte *at);
inline void free_links(Vector *links);

#define SWAP(T,X,Y)       do { T t=(X); (X)=(Y); (Y)=t;} while(0)
#define READ_VAL(T,I,X)      ((X)=*(T*)(I), I+=sizeof(T))

/*
  Javaによる処理系ではリンクはリンクオブジェクトで表現するが、SLIMでは
  リンクオブジェクトはなく直接リンク先のアトムと引数番号でリンクを表す。
  しかし、このSLIMの実装方法では，a(L,L).  a(X,Y) :- b(X), b(Y). の場合
  に2.のようになりうまくいかない。これは、GETLINKで得たリンクの値が
  INHERIT_LINK によるリンクのつなぎ替えの過程で更新されないからだ。そこ
  で、3.のようにしてGETLINK でのリンク先の取得を実際には遅延させるよう
  にして正常に動作させる案を考えた。

  現在は3.の方法を取っている。具体的には wtとatにリンク先のアトムと引数
  番号ではなくリンク元のアトムと引数番号を保持する。さらに、リンク元を
  保持している場合にはwt内の値の下位1ビットを1にし、リンク先を保持して
  いる場合と区別をしている。どちらの場合も同じマクロを使い同様のコード
  でリンク先の取得が出きるようにしている。

  1. リンクオブジェクトがある場合(Javaによる処理系)

    0--+       0----b       0 +--b       +--b
    a  |  =>   a       =>   a |      =>  |
    1--+       1            1 +--b       +--b

  2. リンクオブジェクトがない場合(SLIM)
     A,BはGETLINKで得た、リンク先を表している

                  (Bの先を指す)    (Aの先を指す)    (不正な状態)
      A                A                  A               A
      |                |             b--> |          b--> |
  +---0<---+       +---0              <---0
  |   a    |  =>   |   a      =>          a      =>
  +-->1----+       +-->1--->              1--->
      |                |<-- b             |<-- b          |<-- b
      B                B                  B               B


  3. リンクオブジェクトがない場合、リンクはリンク元を保持(SLIM)
      A,BはGETLINKを行った際のリンク元の情報を持つ。実際のリンク先は
      リンクのつなぎ替えを行う際に得る。

             (Bのリンク先を得る)                (Aのリンク先を得る)  aが消滅
                                 +---------+      +---------+       +---------+-+      +-----------+
                                 |         |      |         |       |         | |      |           |
    B                B           v     B   |      v     B   |       v     B   | |      v           |
    |            b-->|           b     |   |      b     |   |       b     |   | |      b           |
  +---0<--+       +---0<--+      | +---0   |      | +---0   |       | +---0   | |      |           |
  |   a   |  =>   |   a   |  =>  | |   a   |  =>  | |   a   |  =>   | |   a   | | =>   |           |
  +-->1---+       +-->1---+      +-+-->1---+      +-+-->1---+       | +-->1---+ |      |           |
        |              |                |                |<--b      +------|--->b      +---------->b
        A              A                A                A                 A
*/

/* リンク先のアトムを得る */
#define LINKED_ATOM(LINKI) wt[LINKI]
/* リンク先のアトムの引数のattributeを得る */
#define LINKED_ATTR(LINKI) at[LINKI]

static BOOL interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr);
static BOOL react_ruleset_in_all_mem(struct ReactCxt *rc, LmnRuleSet rs, LmnMembrane *mem);

void task_init()
{
  wt_size = 1024;
  wt = LMN_NALLOC(LmnWord, wt_size);
  wt_t = LMN_NALLOC(LmnWord, wt_size);
  at = LMN_NALLOC(LmnByte, wt_size);
  at_t = LMN_NALLOC(LmnByte, wt_size);
  memset(wt, 0, sizeof(LmnWord) * wt_size);
  memset(wt_t, 0, sizeof(LmnWord) * wt_size);
  memset(at, 0, sizeof(LmnByte) * wt_size);
  memset(at_t, 0, sizeof(LmnByte) * wt_size);
}

void task_finalize()
{
  LMN_FREE(wt);
  LMN_FREE(wt_t);
  LMN_FREE(at);
  LMN_FREE(at_t);
}

inline BOOL react_rule(struct ReactCxt *rc, LmnMembrane *mem, LmnRule rule)
{
  LmnTranslated translated;
  BYTE *inst_seq;
  BOOL result;

  translated = lmn_rule_get_translated(rule);
  inst_seq = lmn_rule_get_inst_seq(rule);

  /* まず、トランスレート済みの関数を実行する
     それがない場合命令列をinterpretで実行する */
  wt[0] = (LmnWord)mem;
#ifdef PROFILE
  if(lmn_env.profile_level >= 2) {
    status_start_rule();
  }
#endif
  result =
    (translated &&  translated(rc, mem)) ||
    (inst_seq && interpret(rc, rule, inst_seq));

#ifdef PROFILE
  if(lmn_env.profile_level >= 2) {
    status_finish_rule(rule, result);
    if(lmn_env.trace) {
      status_rule_output(rule);
    }
  }
#endif
  if (lmn_env.trace && result) {
    if(trace_num != 0) {
      fprintf(stdout, "---->%s\n", lmn_id_to_name(lmn_rule_get_name(rule)));
    }
    fprintf(stdout, "%d: ", trace_num++);
    lmn_dump_cell_stdout(RC_GROOT_MEM(rc));
  }

  return result;
}

/* ルールセットのルールを可能な限り適用する。非決定実行時にはルールセッ
   トのルールが適用できなくなってから状態を生成し、途中の状態は生成され
   ない。ルールセットのルールが非決定性を含んでいたとしても、決定的にルー
   ル適用を行うので、生成される状態は高々一つである */
static BOOL react_ruleset_atomic(ReactCxt rc, LmnMembrane *mem, LmnRuleSet ruleset)
{
  int i, n;
  BOOL ok = FALSE;
  n = lmn_ruleset_rule_num(ruleset);

  /* MC */
  if (RC_GET_MODE(rc, REACT_ND)) {
    struct ReactCxt stand_alone_rc;

    LmnMembrane *new_global_root, *new_mem;
    ProcessTbl copymap;
    BOOL reacted_once = FALSE;
    LmnWord t;

    stand_alone_react_cxt_init(&stand_alone_rc);
    new_global_root = lmn_mem_copy_with_map(RC_GROOT_MEM(rc), &copymap);
    RC_SET_GROOT_MEM(&stand_alone_rc, new_global_root);

    /* コピーされた膜を取得 */
    proc_tbl_get_by_mem(copymap, mem, &t);
    new_mem = (LmnMembrane *)t;
    if (!new_mem) {
      /* memは子膜ではなく、グローバルルート膜 */
      new_mem = new_global_root;
    }
    proc_tbl_free(copymap);

    while (TRUE) {
      BOOL reacted = FALSE;
      for (i = 0; i < n; i++) {
        reacted = reacted || react_rule(&stand_alone_rc,
                                        new_mem,
                                        lmn_ruleset_get_rule(ruleset, i));
      }
      if (reacted) {
        lmn_react_systemruleset(&stand_alone_rc, new_mem);
      }
      reacted_once = reacted_once || reacted;
      if (!reacted) break;
    }

    if (reacted_once) {
      nd_react_cxt_add_expanded(rc, new_mem, dummy_rule());
    } else {
      lmn_mem_drop(new_global_root);
      lmn_mem_free(new_global_root);
    }
    ok = reacted_once;
    stand_alone_react_cxt_destroy(&stand_alone_rc);
  } else { /* 通常実行時 */
    BOOL reacted_once = FALSE;

    while (TRUE) {
      BOOL reacted = FALSE;
      for (i = 0; i < n; i++) {
        reacted = reacted || react_rule(rc, mem, lmn_ruleset_get_rule(ruleset, i));
      }
      if (reacted) {
        lmn_react_systemruleset(rc, mem);
      }
      reacted_once = reacted_once || reacted;
      if (!reacted) break;
    }
    ok = reacted_once;
  }
  return ok;
}

void set_all_ruleset_validation(LmnMembrane *mem, BOOL b)
{

  if (mem == NULL) return;

  for (; mem; mem = mem->next) {
    unsigned int i, n = lmn_mem_ruleset_num(mem);
    for (i = 0; i < n; i++) {
      lmn_ruleset_set_valid(lmn_mem_get_ruleset(mem, i), b);
    }
    set_all_ruleset_validation(mem->child_head, b);
  }

}

/* ルールセットのルールを可能な限り適用する。非決定実行時にはルールセッ
   トのルールが適用できななる適用してできるすべての状態を状態空間に追加
   する。*/
static BOOL react_ruleset_atomic_nd(ReactCxt rc, LmnMembrane *mem, LmnRuleSet ruleset)
{
  int i, n;
  BOOL ok = FALSE;
  n = lmn_ruleset_rule_num(ruleset);

  /* MC */
  if (RC_GET_MODE(rc, REACT_ND)) {
    StateSpace states;
    const Vector *end_states;

    set_all_ruleset_validation(RC_GROOT_MEM(rc), FALSE);
    lmn_ruleset_set_valid(ruleset, TRUE);
    lmn_ruleset_set_atomic(ruleset, FALSE);

    states = do_nd(RC_GROOT_MEM(rc));
    end_states = state_space_end_states(states);

    set_all_ruleset_validation(RC_GROOT_MEM(rc), TRUE);
    lmn_ruleset_set_atomic(ruleset, TRUE);

    for (i = 0; i < vec_num(end_states); i++) {
      LmnMembrane *m = state_copied_mem((State *)vec_get(end_states, i));
      /* 生成された状態はすべての膜がstableになっているので、activateする */
      activate_ancestors(m);
      nd_react_cxt_add_expanded(rc,
                                m,
                                dummy_rule());
      state_space_remove(states, (State *)vec_get(end_states, i));
      state_free((State *)vec_get(end_states, i));
    }

    ok = end_states > 0;
    state_space_free(states);
  } else { /* 通常実行時 */
    BOOL reacted_once = FALSE;

    while (TRUE) {
      BOOL reacted = FALSE;
      for (i = 0; i < n; i++) {
        reacted = reacted || react_rule(rc, mem, lmn_ruleset_get_rule(ruleset, i));
      }
      if (reacted) {
        lmn_react_systemruleset(rc, mem);
      }
      reacted_once = reacted_once || reacted;
      if (!reacted) break;
    }
    ok = reacted_once;
  }
  return ok;
}

static void react_initial_rulesets(struct ReactCxt *rc, LmnMembrane *mem)
{
  int i;
  BOOL reacted;

  do {

    reacted = FALSE;
    if (react_ruleset_in_all_mem(rc, initial_system_ruleset, mem)) {
      reacted = TRUE;
      continue;
    }
    for(i=0; i < lmn_ruleset_rule_num(initial_ruleset); i++){
      if (react_rule(rc, mem, lmn_ruleset_get_rule(initial_ruleset, i))) {
        reacted = TRUE;
        break;
      }
    }
  } while (reacted);
}

/* 膜memでrulesetsのルールの適用を行う。初期ルールにしか使わないつもりなので
   適用の成否を無視する */
void react_start_rulesets(LmnMembrane *mem, Vector *rulesets)
{
  struct ReactCxt rc;
  int i;

  stand_alone_react_cxt_init(&rc);
  RC_SET_GROOT_MEM(&rc, mem);

  for(i=0; i<vec_num(rulesets); ++i){
    lmn_react_ruleset(&rc, mem, (LmnRuleSet)vec_get(rulesets,i));
  }
  react_initial_rulesets(&rc, mem);
  lmn_react_systemruleset(&rc, mem);

}

/* 膜memでrulesetのルールの適用を試みる。適用が起こった場合TRUEを返し、
   起こらなかった場合にはFALSEを返す。 */
BOOL lmn_react_ruleset(struct ReactCxt *rc, LmnMembrane *mem, LmnRuleSet ruleset)
{
  int i, n;
  BOOL result = FALSE;

  if (!lmn_ruleset_is_valid(ruleset)) return FALSE;

  if (lmn_ruleset_atomic_type(ruleset) == ATOMIC_NONE) {
    n = lmn_ruleset_rule_num(ruleset);

    for (i = 0; i < n; i++) {
      if (react_rule(rc, mem, lmn_ruleset_get_rule(ruleset, i))) {
        result = TRUE;
        break;
      }
    }
  } else if (lmn_ruleset_atomic_type(ruleset) == ATOMIC_ND) {
    result = react_ruleset_atomic_nd(rc, mem, ruleset);
  } else if (lmn_ruleset_atomic_type(ruleset) == ATOMIC_DET) {
    result = react_ruleset_atomic(rc, mem, ruleset);
  }

  return result;
}

/* system rulesetをmem以下の全膜へ適用 */
static void react_systemruleset(struct ReactCxt *rc, LmnMembrane *mem)
{
  for(; mem; mem = mem->next) {
    int i;
    int n = lmn_ruleset_rule_num(system_ruleset);
    react_systemruleset(rc, mem->child_head);
    while (TRUE) {
      BOOL reacted = FALSE;
      for (i = 0; i < n; i++) {
        reacted = reacted || react_rule(rc, mem, lmn_ruleset_get_rule(system_ruleset, i));
      }
      if (!reacted) break;
    }
  }
}

void lmn_react_systemruleset(struct ReactCxt *rc, LmnMembrane *mem)
{
  LmnWord tmp_rc_mode = rc->mode;
  int temp_prof = lmn_env.profile_level;
  lmn_env.profile_level = 0;
  RC_SET_MODE(rc, REACT_STAND_ALONE);

  react_systemruleset(rc, mem);

  lmn_env.profile_level = temp_prof;
  RC_SET_MODE(rc, tmp_rc_mode);
}

static int react_rulesets(struct ReactCxt *rc, LmnMembrane *mem)
{
  unsigned int i;
  int flag = FALSE;

  for (i = 0; i < mem->rulesets.num; i++) {
    if (lmn_react_ruleset(rc, mem, (LmnRuleSet)vec_get(&mem->rulesets, i))) {
      flag = TRUE;
      break;
    }
  }
  return flag;
}

/* cur_memと、その子孫膜に存在するルールすべてに対して適用を試みる */
BOOL react_all_rulesets(struct ReactCxt *rc,
                        LmnMembrane *cur_mem)
{
  unsigned int i;
  struct Vector rulesets = cur_mem->rulesets; /* 本膜のルールセットの集合 */
  BOOL ok = FALSE;

  for (i = 0; i < vec_num(&rulesets); i++) {
    ok = ok || lmn_react_ruleset(rc, cur_mem,
                                 (LmnRuleSet)vec_get(&rulesets, i));
  }
#ifdef OLD
  if (!ok) { /* 通常のルールセットが適用できなかった場合 */
    /* システムルールセットの適用 */
    if (lmn_react_ruleset(rc, cur_mem, system_ruleset)) {
      ok = TRUE;
    }
  }
#endif
  return ok;
}

/* ルールセットrsをmem以下のすべての膜内で適用する */
static BOOL react_ruleset_in_all_mem(struct ReactCxt *rc, LmnRuleSet rs, LmnMembrane *mem)
{
  LmnMembrane *m;

  for (m = mem->child_head; m; m = m->next) {
    if (react_ruleset_in_all_mem(rc, rs, m)) return TRUE;
  }

  return lmn_react_ruleset(rc, mem, rs);
}

static void mem_oriented_loop(struct ReactCxt *rc, LmnMembrane *mem)
{
  LmnMemStack memstack = RC_MEMSTACK(rc);


  while(!lmn_memstack_isempty(memstack)){
    LmnMembrane *mem = lmn_memstack_peek(memstack);
    if (!react_rulesets(rc, mem)) {
      BOOL temp_env_p = lmn_env.profile_level;
      lmn_env.profile_level = 0;
      if (!lmn_react_ruleset(rc, mem, system_ruleset)) {
        /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
        lmn_memstack_pop(memstack);
      }
      lmn_env.profile_level = temp_env_p;
    }
  }
}

/* 通常実行を行う */
void lmn_run(Vector *start_rulesets)
{
  LmnMembrane *mem;
  struct ReactCxt mrc;

  mem_react_cxt_init(&mrc);
  mem = lmn_mem_make();
  RC_SET_GROOT_MEM(&mrc, mem);
  lmn_memstack_push(RC_MEMSTACK(&mrc), mem);
  {
    int temp_env_p = lmn_env.profile_level;
    if(lmn_env.trace && lmn_env.profile_level >= 2) {
        fprintf(stdout, "  %6s|%6s|%6s|%6s\n", " Name", " Apply", " Trial", " BackTrack");
    }
    lmn_env.profile_level = 0;
    react_start_rulesets(mem, start_rulesets);
    lmn_env.profile_level = temp_env_p;
  }

  lmn_memstack_reconstruct(RC_MEMSTACK(&mrc), mem);
  /* for tracer */
  mem_oriented_loop(&mrc, mem);

  lmn_dump_cell_stdout(mem);

  /* 後始末 */
  lmn_mem_drop(mem);
  lmn_mem_free(mem);
  mem_react_cxt_destroy(&mrc);
}

/* Utility for reading data */

#define READ_DATA_ATOM(dest, attr)              \
  do {                                          \
    switch (attr) {                             \
    case LMN_INT_ATTR:                          \
       READ_VAL(long, instr, (dest));           \
       break;                                   \
     case LMN_DBL_ATTR:                         \
     {                                          \
        double *x;                              \
        x = LMN_MALLOC(double);                 \
        READ_VAL(double, instr, *x);            \
        (dest) = (LmnWord)x;                    \
        break;                                  \
     }                                          \
     case LMN_STRING_ATTR:                      \
     {                                          \
        lmn_interned_str s;                     \
        READ_VAL(lmn_interned_str, instr, s);   \
        (dest) = (LmnWord)lmn_string_make(lmn_id_to_name(s));   \
        break;                                  \
     }                                          \
     default:                                   \
        lmn_fatal("Implementation error");      \
     }                                          \
   } while (0)

/* attrに応じて、ファンクタを読み込み、destに定数アトムを生成する。
   attrは適切な値に変更する場合がある */
#define READ_CONST_DATA_ATOM(dest, attr)        \
  do {                                          \
    switch (attr) {                             \
    case LMN_INT_ATTR:                          \
       READ_VAL(long, instr, (dest));           \
       break;                                   \
     case LMN_DBL_ATTR:                         \
       (dest) = (LmnWord)instr;                 \
       instr += sizeof(double);                 \
        (attr) = LMN_CONST_DBL_ATTR;            \
       break;                                   \
     case LMN_STRING_ATTR:                      \
     {                                          \
        lmn_interned_str s;                     \
        READ_VAL(lmn_interned_str, instr, s);   \
        (dest) = s;                             \
        (attr) = LMN_CONST_STR_ATTR;            \
        break;                                  \
     }                                          \
     default:                                   \
        lmn_fatal("Implementation error");      \
     }                                          \
   } while (0)

#define READ_CMP_DATA_ATOM(attr, x, result)            \
       do {                                            \
          switch(attr) {                               \
          case LMN_INT_ATTR:                           \
            {                                          \
              long t;                                  \
              READ_VAL(long, instr, t);                \
              (result) = ((long)(x) == t);             \
              break;                                   \
            }                                          \
          case LMN_DBL_ATTR:                           \
            {                                          \
              double t;                                \
              READ_VAL(double, instr, t);              \
              (result) = (*(double*)(x) == t);         \
              break;                                   \
            }                                          \
          case LMN_STRING_ATTR:                        \
            {                                          \
              lmn_interned_str s;                      \
              LmnString str1;                          \
              READ_VAL(lmn_interned_str, instr, s);    \
              str1 = lmn_string_make(lmn_id_to_name(s)); \
              (result) = lmn_string_eq(str1, (LmnString)(x));   \
              lmn_string_free(str1);                   \
              break;                                   \
            }                                          \
          default:                                     \
            lmn_fatal("Implementation error");         \
          }                                            \
          } while (0)

#define SKIP_DATA_ATOM(attr) \
       do {                                            \
          switch(attr) {                               \
          case LMN_INT_ATTR:                           \
            {                                          \
              long t;                                  \
              READ_VAL(long, instr, t);                \
              break;                                   \
            }                                          \
          case LMN_DBL_ATTR:                           \
            {                                          \
              double t;                                \
              READ_VAL(double, instr, t);              \
              break;                                   \
            }                                          \
          case LMN_STRING_ATTR:                        \
            {                                          \
              lmn_interned_str s;                      \
              READ_VAL(lmn_interned_str, instr, s);    \
              break;                                   \
            }                                          \
          default:                                     \
            lmn_fatal("Implementation error");         \
          }                                            \
          } while (0)

/* DEBUG: */
/* static void print_wt(void); */

/* mem != NULL ならば memにUNIFYを追加、そうでなければ
   UNIFYは膜に所属しない */
HashSet *insertconnectors(LmnMembrane *mem, const Vector *links)
{
  unsigned int i, j;
  HashSet *retset = hashset_make(8);
  /* EFFICIENCY: retsetがHash Setである意味は?　ベクタでいいのでは？
     中間命令でセットを使うように書かれている */

  for(i = 0; i < links->num; i++) {
    LmnWord linkid1 = vec_get(links, i);
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(linkid1))) continue;
    for(j = i+1; j < links->num; j++) {
      LmnWord linkid2 = vec_get(links, j);
      if (LMN_ATTR_IS_DATA(LINKED_ATTR(linkid2))) continue;
      /* is buddy? */
      if (LINKED_ATOM(linkid2) == LMN_SATOM_GET_LINK(LINKED_ATOM(linkid1), LINKED_ATTR(linkid1)) &&
          LINKED_ATTR(linkid2) == LMN_SATOM_GET_ATTR(LINKED_ATOM(linkid1), LINKED_ATTR(linkid1))) {
        /* '='アトムをはさむ */
        LmnSAtom eq;
        LmnSAtom a1, a2;
        LmnLinkAttr t1, t2;

        if (mem) eq = lmn_mem_newatom(mem, LMN_UNIFY_FUNCTOR);
        else {
          eq = lmn_new_atom(LMN_UNIFY_FUNCTOR);
        }

        /* リンクがリンクの元を持つ場合、あらかじめリンク先の取得をしていなければ
           ならない。リンク元はnew_link時に書き換えられてしまう。*/

        a1 = LMN_SATOM(LINKED_ATOM(linkid1));
        a2 = LMN_SATOM(LINKED_ATOM(linkid2));
        t1 = LINKED_ATTR(linkid1);
        t2 = LINKED_ATTR(linkid2);

        lmn_newlink_in_symbols(a1, t1, eq, 0);
        lmn_newlink_in_symbols(a2, t2, eq, 1);
        hashset_add(retset, (HashKeyType)eq);
      }
    }
  }

  return retset;
}

static BOOL interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr)
{
/*   LmnRuleInstr start = instr; */
  LmnInstrOp op;

  while (TRUE) {
  LOOP:;
    READ_VAL(LmnInstrOp, instr, op);
/*     fprintf(stdout, "op: %d %d\n", op, (instr - start)); */
/*     lmn_dump_mem((LmnMembrane*)wt[0]); */
    switch (op) {
    case INSTR_SPEC:
    {
      LmnInstrVar s0, s1;

      READ_VAL(LmnInstrVar, instr, s0);
      READ_VAL(LmnInstrVar, instr, s1);

      /* extend vector if need */
      if (s1 > wt_size) {
        wt_size = s1;
        wt = LMN_REALLOC(LmnWord, wt, wt_size);
        at = LMN_REALLOC(LmnLinkAttr, at, wt_size);
      }
      break;
    }
    case INSTR_INSERTCONNECTORSINNULL:
    {
      LmnInstrVar seti, list_num;
      Vector links;
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num); /* TODO: vector_initの仕様変更に伴い変更する */
      for (i = 0; i < list_num; i++) {
        LmnInstrVar t;
        READ_VAL(LmnInstrVar, instr, t);
        vec_push(&links, (LmnWord)t); /* TODO: vector_initの仕様変更に伴い変更する(インデックスアクセスに) */
      }

      wt[seti] = (LmnWord)insertconnectors(NULL, &links);
      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if(interpret(rc, rule, instr)) {
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      }
      else assert(0);
      break;
    }
    case INSTR_INSERTCONNECTORS:
    {
      LmnInstrVar seti, list_num, memi, enti;
      Vector links; /* src list */
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num);

      for (i = 0; i < list_num; i++) {
        READ_VAL(LmnInstrVar, instr, enti);
        vec_push(&links, (LmnWord)enti); /* TODO: vector_initの仕様変更に伴い変更する */
      }

      READ_VAL(LmnInstrVar, instr, memi);
      wt[seti] = (LmnWord)insertconnectors((LmnMembrane *)wt[memi], &links);
      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if(interpret(rc, rule, instr)){
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      }else assert(0);
      break;
    }
    case INSTR_JUMP:
    {
      /* EFFICIENCY: 新たに作業配列をmallocしているので非常に遅い
                     -O3 で生成される中間命令にJUMPが含まれないため
                     これでもよい */
      LmnInstrVar num, i, n;
      LmnJumpOffset offset;
      BOOL ret;
      LmnWord *wt_org = wt;
      LmnByte *at_org = at;
      LmnWord *wt2 = LMN_NALLOC(LmnWord, wt_size);
      LmnByte *at2 = LMN_NALLOC(LmnByte, wt_size);
      /**
       * MC -->
       */
      if (RC_GET_MODE(rc, REACT_ND)) {
        for(i = 0; i < wt_size; i++) {
          wt2[i] = at2[i] = 0;
        }
      }
      /**
       * <-- MC
       */
      unsigned int wt_size_org = wt_size;
      LmnRuleInstr next;

      READ_VAL(LmnJumpOffset, instr, offset);
      next = instr + offset;

      i = 0;
      /* atom */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
      }
      /* mem */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
      }
      /* vars */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
      }

      instr = next;

      wt = wt2;
      at = at2;
      wt_size = wt_size_org;

      ret = interpret(rc, rule, instr);
      LMN_FREE(wt);
      LMN_FREE(at);
      wt = wt_org;
      at = at_org;
      wt_size = wt_size_org;
      return ret;
    }
    case INSTR_RESETVARS:
    {
      LmnInstrVar num, i, n, t;

      i = 0;
      /* atom */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt_t[i] = wt[n];
        at_t[i] = at[n];
      }

      /* mem */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt_t[i] = wt[n];
        at_t[i] = at[n];
      }

      /* vars */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt_t[i] = wt[n];
        at_t[i] = at[n];
      }

      for (t=0; t<=i; t++) {
        wt[t] = wt_t[t];
        at[t] = at_t[t];
      }

      break;
    }
    case INSTR_COMMIT:
    {
      lmn_interned_str rule_name;
      LmnLineNum line_num;

      READ_VAL(lmn_interned_str, instr, rule_name);
      READ_VAL(LmnLineNum, instr, line_num);

      lmn_rule_set_name(rule, rule_name);

      /*
       * MC mode
       *
       * グローバル変数global_rootに格納されているグローバルルート膜をコピーして，
       * そのコピーに対してボディ命令を適用する．
       * その際に変数配列の情報もコピー前のものからコピー後のものへと書き換える．
       *
       * ・性質ルールの場合(廃止)
       *   性質ルール適用結果がglobal_rootに格納される
       * ・システムルールの場合
       *   システムルール適用結果を新たな状態として生成する
       *
       * CONTRACT: COMMIT命令に到達したルールはマッチング検査に成功している
       */
      if (RC_GET_MODE(rc, REACT_ND)) {
        unsigned int i;
        /* グローバルルート膜のコピー */
        ProcessTbl copymap;
        LmnMembrane *tmp_global_root;
        LmnWord t;
#ifdef PROFILE
        if (lmn_env.profile_level >= 2) {
          status_start_commit();
        }
#endif
        tmp_global_root = lmn_mem_copy_with_map(RC_GROOT_MEM(rc), &copymap);

        /* 変数配列および属性配列のコピー */
        LmnWord *wtcp = LMN_NALLOC(LmnWord, wt_size);
        LmnByte *atcp = LMN_NALLOC(LmnByte, wt_size);
        for(i = 0; i < wt_size; i++) {
          wtcp[i] = atcp[i] = 0;
        }

        /* copymapの情報を基に変数配列を書換える */
        /* TODO: wt_sizeまでループを回さずにすませられないか */
        for (i = 0; i < wt_size; i++) {
          atcp[i] = at[i];
          if(LMN_INT_ATTR == at[i]) { /* intのみポインタでないため */
            wtcp[i] = wt[i];
          }
          else if(proc_tbl_get(copymap, wt[i], &t)) {
            wtcp[i] = t;
          }
          else if(wt[i] == (LmnWord)RC_GROOT_MEM(rc)) { /* グローバルルート膜 */
            wtcp[i] = (LmnWord)tmp_global_root;
          }
          else if (at[i] == LMN_DBL_ATTR) {
            double *d = (double *)wtcp[i];
            LMN_COPY_DBL_ATOM(d, wt[i]);
            wtcp[i] = (LmnWord)d;
          }
        }
        proc_tbl_free(copymap);
        /* 変数配列および属性配列をコピーと入れ換える */
        SWAP(LmnWord *, wtcp, wt);
        SWAP(LmnByte *, atcp, at);
#ifdef PROFILE
        if (lmn_env.profile_level >= 2) {
          status_finish_commit();
        }
#endif
        interpret(rc, rule, instr);
        lmn_react_systemruleset(rc, (LmnMembrane *)wt[0]);
        nd_react_cxt_add_expanded(rc, tmp_global_root, rule);

        if (!RC_GET_MODE(rc, REACT_MEM_ORIENTED) &&
            lmn_rule_get_history(rule) != NULL   &&
            lmn_rule_get_pre_id(rule) != 0)
              st_delete(lmn_rule_get_history(rule), lmn_rule_get_pre_id(rule), 0);

        /* 変数配列および属性配列を元に戻す（いらないかも？） */
        SWAP(LmnWord *, wtcp, wt);
        SWAP(LmnByte *, atcp, at);

        LMN_FREE(wtcp);
        LMN_FREE(atcp);

        return FALSE;
      } else if (RC_GET_MODE(rc, REACT_PROPERTY)) {
        return TRUE;
      }
      break;
    }
    case INSTR_FINDATOM:
    {
      LmnInstrVar atomi, memi;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnLinkAttr, instr, attr);

      if (LMN_ATTR_IS_DATA(attr)) {
        fprintf(stderr, "I can not find data atoms.\n");
        assert(FALSE);
      } else { /* symbol atom */
        LmnFunctor f;
        AtomListEntry *atomlist_ent;
        LmnSAtom atom;

        READ_VAL(LmnFunctor, instr, f);
        atomlist_ent = lmn_mem_get_atomlist((LmnMembrane*)wt[memi], f);
        if (atomlist_ent) {
          at[atomi] = LMN_ATTR_MAKE_LINK(0);
          EACH_ATOM(atom, atomlist_ent, {
            wt[atomi] = (LmnWord)atom;
            if (interpret(rc, rule, instr)) {
              return TRUE;
            }
#ifdef PROFILE
            if (lmn_env.profile_level >= 2) {
              status_backtrack_counter();
            }
#endif
          });
        }
       return FALSE;
      }
      break;
    }
    case INSTR_FINDATOM2:
    {
      LmnInstrVar atomi, memi, findatomid;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, findatomid);
      READ_VAL(LmnLinkAttr, instr, attr);
      if (LMN_ATTR_IS_DATA(attr)) {
        fprintf(stderr, "I can not find data atoms.\n");
        assert(FALSE);
      } else { /* symbol atom */
        LmnFunctor f;
        AtomListEntry *atomlist_ent;
        LmnSAtom start_atom, atom, record;

        READ_VAL(LmnFunctor, instr, f);
        atomlist_ent = lmn_mem_get_atomlist((LmnMembrane*)wt[memi], f);
        if (atomlist_ent) {
          at[atomi] = LMN_ATTR_MAKE_LINK(0);
          /*
           * TODO: (TOFIX)64bit環境でwarningが出る
           */
          record = LMN_SATOM(atomlist_get_record(atomlist_ent, findatomid));
          if(!record) {
            start_atom = atomlist_head(atomlist_ent);
            record = lmn_new_atom(LMN_RESUME_FUNCTOR);
            hashtbl_put(&atomlist_ent->record, findatomid, (HashKeyType)record);
            LMN_SATOM_SET_NEXT(atomlist_ent, record);
            LMN_SATOM_SET_PREV(record, atomlist_ent);
            LMN_SATOM_SET_NEXT(record, start_atom);
            LMN_SATOM_SET_PREV(start_atom, record);
          } else {
            start_atom = LMN_SATOM_GET_NEXT_RAW(record);
          }
#define DBG 0
#if DBG
          int count=0;
#endif
          for (atom = start_atom;
               atom != lmn_atomlist_end(atomlist_ent);
               atom = LMN_SATOM_GET_NEXT_RAW(atom)) {
#if DBG
            count++;
#endif
            if(LMN_SATOM_GET_FUNCTOR(atom)==LMN_RESUME_FUNCTOR)
              continue;
            wt[atomi] = (LmnWord)atom;
            LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(record), LMN_SATOM_GET_PREV(record));
            LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(record), LMN_SATOM_GET_NEXT_RAW(record));
            LMN_SATOM_SET_NEXT(record, atom);
            LMN_SATOM_SET_PREV(record, LMN_SATOM_GET_PREV(atom));
            LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(atom), record);
            LMN_SATOM_SET_PREV(atom, record);

            if (interpret(rc, rule, instr)) {
#if DBG
              printf("count=%d\n", count);
#endif
              return TRUE;
            }
#ifdef PROFILE
            if (lmn_env.profile_level >= 2) {
              status_backtrack_counter();
            }
#endif
          }

          /* 現在のfindatom2の実装にはバグがある（cf. 言語班Wiki
             findatom2議論）。バグを回避するために、履歴アトムの後ろの
             アトムすべてからのマッチングに失敗した場合、履歴アトムの前
             のアトムに対してマッチングを試みる */
          EACH_ATOM(atom, atomlist_ent, {
              if (atom == start_atom) break;
              wt[atomi] = (LmnWord)atom;
              if (interpret(rc, rule, instr)) {
                return TRUE;
              }
#ifdef PROFILE
              if (lmn_env.profile_level >= 2) {
                status_backtrack_counter();
              }
#endif
          });

        }
        return FALSE;
      }
      break;
    }
    case INSTR_LOCKMEM:
    {
      LmnInstrVar mem, atom, memn;
      READ_VAL(LmnInstrVar, instr, mem);
      READ_VAL(LmnInstrVar, instr, atom);
      READ_VAL(lmn_interned_str, instr, memn);

      LMN_ASSERT(!LMN_ATTR_IS_DATA(at[atom]));
      LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt[atom]))));
      wt[mem] = (LmnWord)LMN_PROXY_GET_MEM(wt[atom]);
      if (RC_GET_MODE(rc, REACT_ND)) { at[mem] = 0; /* MC */ }
      if(((LmnMembrane*)wt[mem])->name != memn) return FALSE;
      LMN_ASSERT(((LmnMembrane *)wt[mem])->parent);
      break;
    }
    case INSTR_ANYMEM:
    {
      LmnInstrVar mem1, mem2, memt, memn; /* dst, parent, type, name */
      LmnMembrane* mp;

      READ_VAL(LmnInstrVar, instr, mem1);
      READ_VAL(LmnInstrVar, instr, mem2);
      READ_VAL(LmnInstrVar, instr, memt);
      READ_VAL(lmn_interned_str, instr, memn);

      mp = ((LmnMembrane*)wt[mem2])->child_head;
      while (mp) {
        wt[mem1] = (LmnWord)mp;
        if (RC_GET_MODE(rc, REACT_ND)) { at[mem1] = 0; /* MC */ }
        if (mp->name == memn && interpret(rc, rule, instr)) {
          return TRUE;
        }
        mp = mp->next;
#ifdef PROFILE
        if (lmn_env.profile_level >= 2) {
          status_backtrack_counter();
        }
#endif
      }
      return FALSE;
      break;
    }
    case INSTR_NMEMS:
    {
      LmnInstrVar memi, nmems;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, nmems);

      if(!lmn_mem_nmems((LmnMembrane*)wt[memi], nmems)) {
        return FALSE;
      }
      break;
    }
    case INSTR_NORULES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      if(((LmnMembrane *)wt[memi])->rulesets.num) return FALSE;
      break;
    }
    case INSTR_NEWATOM:
    {
      LmnInstrVar atomi, memi;
      LmnAtom ap;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnLinkAttr, instr, attr);
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_DATA_ATOM(ap, attr);
      } else { /* symbol atom */
        LmnFunctor f;

        READ_VAL(LmnFunctor, instr, f);
        ap = LMN_ATOM(lmn_new_atom(f));
      }
      lmn_mem_push_atom((LmnMembrane *)wt[memi], ap, attr);
      wt[atomi] = (LmnWord)ap;
      at[atomi] = attr;
      break;
    }
    case INSTR_NATOMS:
    {
      LmnInstrVar memi, natoms;
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, natoms);
      if(!lmn_mem_natoms((LmnMembrane*)wt[memi], natoms)) {
        return FALSE;
      }
      break;
    }
    case INSTR_NATOMSINDIRECT:
    {
      LmnInstrVar memi, natomsi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, natomsi);

      if(!lmn_mem_natoms((LmnMembrane*)wt[memi], wt[natomsi])) {
        return FALSE;
      }
      break;
    }
    case INSTR_ALLOCLINK:
    {
      LmnInstrVar link, atom, n;

      READ_VAL(LmnInstrVar, instr, link);
      READ_VAL(LmnInstrVar, instr, atom);
      READ_VAL(LmnInstrVar, instr, n);

      if (LMN_ATTR_IS_DATA(at[atom])) {
        wt[link] = wt[atom];
        at[link] = at[atom];
      } else { /* link to atom */
        wt[link] = (LmnWord)LMN_SATOM(wt[atom]);
        at[link] = LMN_ATTR_MAKE_LINK(n);
      }
      break;
    }
    case INSTR_UNIFYLINKS:
    {
      LmnInstrVar link1, link2, mem;

      READ_VAL(LmnInstrVar, instr, link1);
      READ_VAL(LmnInstrVar, instr, link2);
      READ_VAL(LmnInstrVar, instr, mem);

      if (LMN_ATTR_IS_DATA(LINKED_ATTR(link1))) {
        if (LMN_ATTR_IS_DATA(LINKED_ATTR(link2))) { /* 1, 2 are data */
          lmn_mem_link_data_atoms((LmnMembrane *)wt[mem], wt[link1], at[link1], LINKED_ATOM(link2), LINKED_ATTR(link2));
        }
        else { /* 1 is data */
          LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATOM(link1));
          LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATTR(link1));
        }
      }
      else if (LMN_ATTR_IS_DATA(LINKED_ATTR(link2))) { /* 2 is data */
        LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATOM(link2));
        LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATTR(link2));
      }
      else { /* 1, 2 are symbol atom */
        LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATOM(link2));
        LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATOM(link1));
        LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATTR(link2));
        LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATTR(link1));
      }
      break;
    }
    case INSTR_NEWLINK:
    {
      LmnInstrVar atom1, atom2, pos1, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      lmn_mem_newlink((LmnMembrane *)wt[memi],
                      wt[atom1],
                      at[atom1],
                      pos1,
                      wt[atom2],
                      at[atom2],
                      pos2);
      break;
    }
    case INSTR_RELINK:
    {
      LmnInstrVar atom1, atom2, pos1, pos2, memi;
      LmnSAtom ap;
      LmnByte attr;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      ap = LMN_SATOM(LMN_SATOM_GET_LINK(wt[atom2], pos2));
      attr = LMN_SATOM_GET_ATTR(wt[atom2], pos2);

      if(LMN_ATTR_IS_DATA(at[atom1]) && LMN_ATTR_IS_DATA(attr)) {
        #ifdef DEBUG
        fprintf(stderr, "Two data atoms are connected each other.\n");
        #endif
      }
      else if (LMN_ATTR_IS_DATA(at[atom1])) {
        LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);
        LMN_SATOM_SET_ATTR(ap, attr, at[atom1]);
      }
      else if (LMN_ATTR_IS_DATA(attr)) {
        LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);
        LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);
      }
      else {
        LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);
        LMN_SATOM_SET_ATTR(ap, attr, pos1);
        LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);
        LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);
      }
      break;
    }
    case INSTR_INHERITLINK:
    {
      LmnInstrVar atomi, posi, linki, memi;
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, posi);
      READ_VAL(LmnInstrVar, instr, linki);
      READ_VAL(LmnInstrVar, instr, memi);

      if(LMN_ATTR_IS_DATA(at[atomi]) && LMN_ATTR_IS_DATA(LINKED_ATTR(linki))) {
        #ifdef DEBUG
        fprintf(stderr, "Two data atoms are connected each other.\n");
        #endif
      }
      else if(LMN_ATTR_IS_DATA(at[atomi])) {
        LMN_SATOM_SET_LINK(LINKED_ATOM(linki), LINKED_ATTR(linki), wt[atomi]);
        LMN_SATOM_SET_ATTR(LINKED_ATOM(linki), LINKED_ATTR(linki), at[atomi]);
      }
      else if(LMN_ATTR_IS_DATA(LINKED_ATTR(linki))) {
        LMN_SATOM_SET_LINK(LMN_SATOM(wt[atomi]), posi, LINKED_ATOM(linki));
        LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atomi]), posi, LINKED_ATTR(linki));
      }
      else {
        LMN_SATOM_SET_LINK(LMN_SATOM(wt[atomi]), posi, LINKED_ATOM(linki));
        LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atomi]), posi, LINKED_ATTR(linki));
        LMN_SATOM_SET_LINK(LINKED_ATOM(linki), LINKED_ATTR(linki), wt[atomi]);
        LMN_SATOM_SET_ATTR(LINKED_ATOM(linki), LINKED_ATTR(linki), posi);
      }

      break;
    }
    case INSTR_GETLINK:
    {
      LmnInstrVar linki, atomi, posi;
      READ_VAL(LmnInstrVar, instr, linki);
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, posi);

      /* リンク先の取得をせずにリンク元の情報を格納しておく。
         リンク元が格納されていることを示すため最下位のビットを立てる */
      wt[linki] = LMN_SATOM_GET_LINK(wt[atomi], posi);
      at[linki] = LMN_SATOM_GET_ATTR(wt[atomi], posi);
      break;
    }
    case INSTR_UNIFY:
    {
      LmnInstrVar atom1, pos1, atom2, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      lmn_mem_unify_atom_args((LmnMembrane *)wt[memi],
                              LMN_SATOM(wt[atom1]),
                              pos1,
                              LMN_SATOM(wt[atom2]),
                              pos2);
      break;
    }
    case INSTR_PROCEED:
      return TRUE;
    case INSTR_STOP:
      return FALSE;
    case INSTR_NOT:
    {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      if (interpret(rc, rule, instr)) {
        return FALSE;
      }
      instr += subinstr_size;
      break;
    }
    case INSTR_ENQUEUEATOM:
    {
      LmnInstrVar atom;

      READ_VAL(LmnInstrVar, instr, atom);
      /* do nothing */
      break;
    }
    case INSTR_DEQUEUEATOM:
    {
      LmnInstrVar atom;

      READ_VAL(LmnInstrVar, instr, atom);
      break;
    }
    case INSTR_NEWMEM:
    {
      LmnInstrVar newmemi, parentmemi, memf;
      LmnMembrane *mp;

      READ_VAL(LmnInstrVar, instr, newmemi);
      READ_VAL(LmnInstrVar, instr, parentmemi);
      READ_VAL(LmnInstrVar, instr, memf);

      mp = lmn_mem_make(); /*lmn_new_mem(memf);*/
      lmn_mem_add_child_mem((LmnMembrane*)wt[parentmemi], mp);
      wt[newmemi] = (LmnWord)mp;
      lmn_mem_set_active(mp, TRUE);
      if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
        lmn_memstack_push(RC_MEMSTACK(rc), mp);
      }
      break;
    }
    case INSTR_ALLOCMEM:
    {
      LmnInstrVar dstmemi;

      READ_VAL(LmnInstrVar, instr, dstmemi);

      wt[dstmemi] = (LmnWord)lmn_mem_make();
      break;
    }
    case INSTR_REMOVEATOM:
    {
      LmnInstrVar atomi, memi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);

      lmn_mem_remove_atom((LmnMembrane*)wt[memi], wt[atomi], at[atomi]);
      break;
    }
    case INSTR_FREEATOM:
    {
      LmnInstrVar atomi;

      READ_VAL(LmnInstrVar, instr, atomi);

      lmn_free_atom(wt[atomi], at[atomi]);
      break;
    }
    case INSTR_REMOVEMEM:
    {
      LmnInstrVar memi, parenti;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, parenti);

      lmn_mem_remove_mem((LmnMembrane *)wt[parenti], (LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_FREEMEM:
    {
      LmnInstrVar memi;
      LmnMembrane *mp;

      READ_VAL(LmnInstrVar, instr, memi);

      mp = (LmnMembrane*)wt[memi];
      lmn_mem_free(mp);
      break;
    }
    case INSTR_ADDMEM:
    {
      LmnInstrVar dstmem, srcmem;

      READ_VAL(LmnInstrVar, instr, dstmem);
      READ_VAL(LmnInstrVar, instr, srcmem);

      LMN_ASSERT(!((LmnMembrane *)wt[srcmem])->parent);

      lmn_mem_add_child_mem((LmnMembrane *)wt[dstmem], (LmnMembrane *)wt[srcmem]);
      break;
    }
    case INSTR_ENQUEUEMEM:
    {
      LmnInstrVar memi;
      READ_VAL(LmnInstrVar, instr, memi);
      if (RC_GET_MODE(rc, REACT_ND)) {
        activate_ancestors((LmnMembrane *)wt[memi]); /* MC */
      } else if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
        lmn_memstack_push(RC_MEMSTACK(rc), (LmnMembrane *)wt[memi]); /* 通常実行時 */
      }
      break;
    }
    case INSTR_UNLOCKMEM:
    { /* 何もしない */
      LmnInstrVar memi;
      READ_VAL(LmnInstrVar, instr, memi);
      break;
    }
    case INSTR_LOADRULESET:
    {
      LmnInstrVar memi;
      LmnRulesetId id;
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnRulesetId, instr, id);

      lmn_mem_add_ruleset((LmnMembrane*)wt[memi], lmn_ruleset_from_id(id));
      break;
    }
    case INSTR_LOADMODULE:
    {
      LmnInstrVar memi;
      lmn_interned_str module_name_id;
      LmnRuleSet ruleset;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(lmn_interned_str, instr, module_name_id);

      if ((ruleset = lmn_get_module_ruleset(module_name_id))) {
        /* テーブル内にルールセットがある場合 */
        lmn_mem_add_ruleset((LmnMembrane*)wt[memi], ruleset);
      }
      else {
        /* テーブル内にルールセットがない場合 */
        fprintf(stderr, "Undefined module %s\n", lmn_id_to_name(module_name_id));
      }
      break;
    }
    case INSTR_RECURSIVELOCK:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      /* do notiong */
      break;
    }
    case INSTR_RECURSIVEUNLOCK:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      /* do notiong */
      break;
    }
    case INSTR_DEREFATOM:
    {
      LmnInstrVar atom1, atom2, posi;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, posi);

      wt[atom1] = (LmnWord)LMN_SATOM(LMN_SATOM_GET_LINK(wt[atom2], posi));
      at[atom1] = LMN_SATOM_GET_ATTR(wt[atom2], posi);
      break;
    }
    case INSTR_DEREF:
    {
      LmnInstrVar atom1, atom2, pos1, pos2;
      LmnByte attr;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, pos2);

      attr = LMN_SATOM_GET_ATTR(wt[atom2], pos1);
      LMN_ASSERT(!LMN_ATTR_IS_DATA(at[atom2]));
      if (LMN_ATTR_IS_DATA(attr)) {
        if (pos2 != 0) return FALSE;
      }
      else {
        if (attr != pos2) return FALSE;
      }
      wt[atom1] = LMN_SATOM_GET_LINK(wt[atom2], pos1);
      at[atom1] = attr;
      break;
    }
    case INSTR_FUNC:
    {
      LmnInstrVar atomi;
      LmnFunctor f;
      LmnLinkAttr attr;
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);

      if (LMN_ATTR_IS_DATA(at[atomi]) == LMN_ATTR_IS_DATA(attr)) {
        if(LMN_ATTR_IS_DATA(at[atomi])) {
          BOOL eq;
          if(at[atomi] != attr) return FALSE; /* comp attr */
          READ_CMP_DATA_ATOM(attr, wt[atomi], eq);
          if (!eq) return FALSE;
        }
        else /* symbol atom */
          {
            READ_VAL(LmnFunctor, instr, f);
            if (LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt[atomi])) != f) {
              return FALSE;
            }
          }
      }
      else { /* LMN_ATTR_IS_DATA(at[atomi]) != LMN_ATTR_IS_DATA(attr) */
        return FALSE;
      }
      break;
    }
    case INSTR_NOTFUNC:
    {
      LmnInstrVar atomi;
      LmnFunctor f;
      LmnLinkAttr attr;
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);

      if (LMN_ATTR_IS_DATA(at[atomi]) == LMN_ATTR_IS_DATA(attr)) {
        if(LMN_ATTR_IS_DATA(at[atomi])) {
          if(at[atomi] == attr) {
            BOOL eq;
            READ_CMP_DATA_ATOM(attr, wt[atomi], eq);
            if (eq) return FALSE;
          }else{
            goto label_skip_data_atom;
          }
        }
        else { /* symbol atom */
          READ_VAL(LmnFunctor, instr, f);
          if (LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt[atomi])) == f) return FALSE;
        }
      }else if(LMN_ATTR_IS_DATA(attr)){
        goto label_skip_data_atom;
      }
      break;
    label_skip_data_atom:
      SKIP_DATA_ATOM(attr);
      break;
    }
    case INSTR_ISGROUND:
    {
      LmnInstrVar funci, srclisti, avolisti;
      Vector *srcvec, *avovec;
      unsigned long natoms;
      BOOL b;

      READ_VAL(LmnInstrVar, instr, funci);
      READ_VAL(LmnInstrVar, instr, srclisti);
      READ_VAL(LmnInstrVar, instr, avolisti);

      /* リンクオブジェクトのベクタを構築 */
      srcvec = links_from_idxs((Vector *)wt[srclisti], wt, at);
      avovec = links_from_idxs((Vector *)wt[avolisti], wt, at);

      b = lmn_mem_is_ground(srcvec, avovec, &natoms);

      free_links(srcvec);
      free_links(avovec);

      if (!b) return FALSE;
      wt[funci] = (LmnWord)natoms;
      at[funci] = LMN_INT_ATTR;
      break;
    }
    case INSTR_UNIQ:
    {

      LmnInstrVar llist, n;
      Vector *srcvec;
      LmnPort port;
      lmn_interned_str id;

      port = (LmnPort)lmn_make_output_string_port();
      READ_VAL(LmnInstrVar, instr, llist);

      unsigned int i = 0;
      for (; i < llist; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        srcvec = (Vector*) wt[n];

        /*グラフをstr_idに取得*/
        lmn_dump_atom(
            port, (LmnWord)wt[vec_get(srcvec, 0)], (LmnLinkAttr)at[vec_get(srcvec, 0)]);
        port_put_raw_s(port, ":");
      }

      id = lmn_intern((char *)lmn_string_c_str(port->data));

      lmn_port_free(port);

      /* 履歴表と照合 */
      if (st_is_member(lmn_rule_get_history(rule), (st_data_t)id)) return FALSE;

      /* 履歴に挿入 */
      st_insert(lmn_rule_get_history(rule), (st_data_t)id, 0);
      lmn_rule_set_pre_id(rule, id);

      break;
    }
    case INSTR_EQGROUND:
    case INSTR_NEQGROUND:
    {
      LmnInstrVar srci, dsti;
      Vector *srcvec, *dstvec;
      BOOL ret_flag;

      READ_VAL(LmnInstrVar, instr, srci);
      READ_VAL(LmnInstrVar, instr, dsti);

      srcvec = links_from_idxs((Vector *)wt[srci], wt, at);
      dstvec = links_from_idxs((Vector *)wt[dsti], wt, at);

      ret_flag = lmn_mem_cmp_ground(srcvec, dstvec);

      free_links(srcvec);
      free_links(dstvec);

      if((!ret_flag && INSTR_EQGROUND == op) || (ret_flag && INSTR_NEQGROUND == op)) {
        return FALSE;
      }
      break;
    }
    case INSTR_COPYGROUND:
    {
      LmnInstrVar dstlist, srclist, memi;
      Vector *srcvec, *dstlovec, *retvec; /* 変数番号のリスト */
      ProcessTbl atommap;

      READ_VAL(LmnInstrVar, instr, dstlist);
      READ_VAL(LmnInstrVar, instr, srclist);
      READ_VAL(LmnInstrVar, instr, memi);

      /* リンクオブジェクトのベクタを構築 */
      srcvec = links_from_idxs((Vector *)wt[srclist], wt, at);

      lmn_mem_copy_ground((LmnMembrane *)wt[memi],
                          srcvec,
                          &dstlovec,
                          &atommap);
      free_links(srcvec);

      /* 返り値の作成 */
      retvec = vec_make(2);
      vec_push(retvec, (LmnWord)dstlovec);
      vec_push(retvec, (LmnWord)atommap);
      wt[dstlist] = (LmnWord)retvec;
      at[dstlist] = (LmnByte)LIST_AND_MAP;

      /* 解放のための再帰。ベクタを解放するための中間ご命令がない */
      interpret(rc, rule, instr);

      free_links(dstlovec);
      vec_free(retvec);

      return TRUE; /* COPYGROUNDはボディに出現する */
    }
    case INSTR_REMOVEGROUND:
    case INSTR_FREEGROUND:
    {
      LmnInstrVar listi, memi;
      Vector *srcvec; /* 変数番号のリスト */

      READ_VAL(LmnInstrVar, instr, listi);
      if (INSTR_REMOVEGROUND == op) {
        READ_VAL(LmnInstrVar, instr, memi);
      }

      srcvec = links_from_idxs((Vector *)wt[listi], wt, at);

      switch (op) {
      case INSTR_REMOVEGROUND:
        lmn_mem_remove_ground((LmnMembrane *)wt[memi], srcvec);
        break;
      case INSTR_FREEGROUND:
        lmn_mem_free_ground(srcvec);
        break;
      }

      free_links(srcvec);

      break;
    }
    case INSTR_ISUNARY:
    {
      LmnInstrVar atomi;
      READ_VAL(LmnInstrVar, instr, atomi);

      if (LMN_ATTR_IS_DATA(at[atomi])) {
        switch (at[atomi]) {
        case LMN_SP_ATOM_ATTR:
          /* スペシャルアトムはgroundの結果をunaryの結果とする */
          if (!SP_ATOM_IS_GROUND(wt[atomi])) {
            return FALSE;
          }
          break;
        default:
          break;
        }
      }
      else if (LMN_SATOM_GET_ARITY(wt[atomi]) != 1)
        return FALSE;
      break;
    }
    case INSTR_ISINT:
    {
      LmnInstrVar atomi;
      READ_VAL(LmnInstrVar, instr, atomi);

      if (at[atomi] != LMN_INT_ATTR)
        return FALSE;
      break;
    }
    case INSTR_ISFLOAT:
    {
      LmnInstrVar atomi;
      READ_VAL(LmnInstrVar, instr, atomi);

      if(at[atomi] != LMN_DBL_ATTR)
        return FALSE;
      break;
    }
    case INSTR_ISSTRING:
    {
      LmnInstrVar atomi;

      READ_VAL(LmnInstrVar, instr, atomi);

      if(!lmn_is_string(wt[atomi], at[atomi]))
        return FALSE;
      break;
    }
    case INSTR_ISINTFUNC:
    {
      LmnInstrVar funci;
      READ_VAL(LmnInstrVar, instr, funci);

      if(at[funci] != LMN_INT_ATTR)
        return FALSE;
      break;
    }
    case INSTR_ISFLOATFUNC:
    {
      LmnInstrVar funci;
      READ_VAL(LmnInstrVar, instr, funci);

      if(at[funci] != LMN_DBL_ATTR)
        return FALSE;
      break;
    }
    case INSTR_COPYATOM:
    {
      LmnInstrVar atom1, memi, atom2;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atom2);

      at[atom1] = at[atom2];
      wt[atom1] = lmn_copy_atom(wt[atom2], at[atom2]);
      lmn_mem_push_atom((LmnMembrane *)wt[memi], wt[atom1], at[atom1]);
      break;
    }
    case INSTR_EQATOM:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      /* データアトムは１引数なので,この命令が出る状況では
         では常にFALSEのはず */
      if (LMN_ATTR_IS_DATA(at[atom1]) ||
          LMN_ATTR_IS_DATA(at[atom2]) ||
          LMN_SATOM(wt[atom1]) != LMN_SATOM(wt[atom2]))
        return FALSE;
      break;
    }
    case INSTR_NEQATOM:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if (!(LMN_ATTR_IS_DATA(at[atom1]) ||
            LMN_ATTR_IS_DATA(at[atom2]) ||
            LMN_SATOM(wt[atom1]) != LMN_SATOM(wt[atom2])))
        return FALSE;
      break;
    }
    case INSTR_EQMEM:
    {
      LmnInstrVar mem1, mem2;

      READ_VAL(LmnInstrVar, instr, mem1);
      READ_VAL(LmnInstrVar, instr, mem2);
      if (wt[mem1] != wt[mem2]) return FALSE;
      break;
    }
    case INSTR_NEQMEM:
    {
      LmnInstrVar mem1, mem2;
      READ_VAL(LmnInstrVar, instr, mem1);
      READ_VAL(LmnInstrVar, instr, mem2);

      if(wt[mem1] == wt[mem2]) return FALSE;
      break;
    }
    case INSTR_STABLE:
    {
      LmnInstrVar memi;
      READ_VAL(LmnInstrVar, instr, memi);

      if (lmn_mem_is_active((LmnMembrane *)wt[memi])) {
        return FALSE;
      }

      break;
    }
    case INSTR_NEWLIST:
    {
      LmnInstrVar listi;
      Vector *listvec = vec_make(16);
      READ_VAL(LmnInstrVar, instr, listi);
      wt[listi] = (LmnWord)listvec;
      if (RC_GET_MODE(rc, REACT_ND)) { at[listi] = 0; /* MC */ }
      /* 解放のための再帰 */
      if (interpret(rc, rule, instr)) {
        vec_free(listvec);
        return TRUE;
      }
      else {
        vec_free(listvec);
        return FALSE;
      }
      break;
    }
    case INSTR_ADDTOLIST:
    {
      LmnInstrVar listi, linki;
      READ_VAL(LmnInstrVar, instr, listi);
      READ_VAL(LmnInstrVar, instr, linki);
      vec_push((Vector *)wt[listi], linki);
      break;
    }
    case INSTR_GETFROMLIST:
    {
      LmnInstrVar dsti, listi, posi;
      READ_VAL(LmnInstrVar, instr, dsti);
      READ_VAL(LmnInstrVar, instr, listi);
      READ_VAL(LmnInstrVar, instr, posi);

      switch (at[listi]) {
        case LIST_AND_MAP:
          wt[dsti] = vec_get((Vector *)wt[listi], (unsigned int)posi);
          if (posi == 0)
            at[dsti] = LINK_LIST;
          else if (posi == 1)
            at[dsti] = MAP;
          else
            assert(0);
          break;
        case LINK_LIST: /* LinkObjをfreeするのはここ？ */
        {
          LinkObj lo = (LinkObj)vec_get((Vector *)wt[listi], (unsigned int)posi);
          wt[dsti] = (LmnWord)lo->ap;
          at[dsti] = lo->pos;
          break;
        }
      }
      break;
    }
    case INSTR_IADD:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] + (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_ISUB:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] - (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IMUL:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] * (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IDIV:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] / (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_INEG:
    {
      LmnInstrVar dstatom, atomi;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atomi);
      wt[dstatom] = (LmnWord)(-(long)wt[atomi]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IMOD:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] % (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_INOT:
    {
      LmnInstrVar dstatom, atomi;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atomi);
      wt[dstatom] = (LmnWord)~(int)atomi;
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IAND:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] & (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IOR:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] | (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IXOR:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)((long)wt[atom1] ^ (long)wt[atom2]);
      at[dstatom] = LMN_INT_ATTR;
      break;
    }
    case INSTR_ILT:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] < (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_ILE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] <= (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_IGT:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] > (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_IGE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] >= (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_IEQ:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] == (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_INE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!((long)wt[atom1] != (long)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_ILTFUNC:
    {
      LmnInstrVar func1, func2;
      READ_VAL(LmnInstrVar, instr, func1);
      READ_VAL(LmnInstrVar, instr, func2);

      if(!((long)wt[func1] < (long)wt[func2])) return FALSE;
      break;
    }
    case INSTR_ILEFUNC:
    {
      LmnInstrVar func1, func2;
      READ_VAL(LmnInstrVar, instr, func1);
      READ_VAL(LmnInstrVar, instr, func2);

      if(!((long)wt[func1] <= (long)wt[func2])) return FALSE;
      break;
    }
    case INSTR_IGTFUNC:
    {
      LmnInstrVar func1, func2;
      READ_VAL(LmnInstrVar, instr, func1);
      READ_VAL(LmnInstrVar, instr, func2);

      if(!((long)wt[func1] > (long)wt[func2])) return FALSE;
      break;
    }
    case INSTR_IGEFUNC:
    {
      LmnInstrVar func1, func2;
      READ_VAL(LmnInstrVar, instr, func1);
      READ_VAL(LmnInstrVar, instr, func2);

      if(!((long)wt[func1] >= (long)wt[func2])) return FALSE;
      break;
    }
    case  INSTR_FADD:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)LMN_MALLOC(double);
      *(double *)wt[dstatom] = *(double *)wt[atom1] + *(double *)wt[atom2];
      at[dstatom] = LMN_DBL_ATTR;
      break;
    }
    case  INSTR_FSUB:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)LMN_MALLOC(double);
      *(double *)wt[dstatom] = *(double *)wt[atom1] - *(double *)wt[atom2];
      at[dstatom] = LMN_DBL_ATTR;
      break;
    }
    case  INSTR_FMUL:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)LMN_MALLOC(double);
      *(double *)wt[dstatom] = *(double *)wt[atom1] * *(double *)wt[atom2];
      at[dstatom] = LMN_DBL_ATTR;
      break;
    }
    case  INSTR_FDIV:
    {
      LmnInstrVar dstatom, atom1, atom2;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      wt[dstatom] = (LmnWord)LMN_MALLOC(double);
      *(double *)wt[dstatom] = *(double *)wt[atom1] / *(double *)wt[atom2];
      at[dstatom] = LMN_DBL_ATTR;
      break;
    }
    case  INSTR_FNEG:
    {
      LmnInstrVar dstatom, atomi;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atomi);

      wt[dstatom] = (LmnWord)LMN_MALLOC(double);
      *(double *)wt[dstatom] = -*(double *)wt[atomi];
      at[dstatom] = LMN_DBL_ATTR;
      break;
    }
    case INSTR_FLT:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] < *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_FLE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] <= *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_FGT:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] > *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_FGE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] >= *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_FEQ:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] == *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_FNE:
    {
      LmnInstrVar atom1, atom2;
      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if(!(*(double*)wt[atom1] != *(double*)wt[atom2])) return FALSE;
      break;
    }
    case INSTR_ALLOCATOM:
    {
      LmnInstrVar atomi;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);
      at[atomi] = attr;
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_CONST_DATA_ATOM(wt[atomi], at[atomi]);
      } else { /* symbol atom */
        LmnFunctor f;
/*         fprintf(stderr, "symbol atom can't be created in GUARD\n"); */
/*         exit(EXIT_FAILURE); */
        READ_VAL(LmnFunctor, instr, f);

        /* 本来のallocatomは格納するのは定数アトムだが、簡単のためにファンクタを格納する */
        wt[atomi] = f;
      }
      break;
    }
    case INSTR_ALLOCATOMINDIRECT:
    {
      LmnInstrVar atomi;
      LmnFunctor funci;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, funci);

      if (LMN_ATTR_IS_DATA(at[funci])) {
        wt[atomi] = lmn_copy_data_atom(wt[funci], at[funci]);
        at[atomi] = at[funci];
      } else { /* symbol atom */
        fprintf(stderr, "symbol atom can't be created in GUARD\n");
        exit(EXIT_FAILURE);
      }
      break;
    }
    case INSTR_SAMEFUNC:
    {
      LmnInstrVar atom1, atom2;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);

      if (!lmn_eq_func(wt[atom1], at[atom1], wt[atom2], at[atom2]))
        return FALSE;
      break;
    }
    case INSTR_GETFUNC:
    {
      LmnInstrVar funci, atomi;

      READ_VAL(LmnFunctor, instr, funci);
      READ_VAL(LmnInstrVar, instr, atomi);

      if(LMN_ATTR_IS_DATA(at[atomi])) {
        /* ここで得るファンクタはガード命令中で一時的に使われるだけなので
           double はポインタのコピーで十分なはず */
        wt[funci]=wt[atomi];
      }
      else {
        wt[funci] = (LmnWord)LMN_SATOM_GET_FUNCTOR(wt[atomi]);
      }
      at[funci] = at[atomi];

      break;
    }
    case INSTR_PRINTINSTR:
    {
      char c;

      while (TRUE) {
        READ_VAL(char, instr, c);
        if (!c) break;
        fprintf(stderr, "%c", c);
      }
      goto LOOP;
    }
    case INSTR_SETMEMNAME:
    {
      LmnInstrVar memi;
      lmn_interned_str name;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(lmn_interned_str, instr, name);
      ((LmnMembrane *)wt[memi])->name = name;
      break;
    }
    case INSTR_COPYRULES:
    {
      LmnInstrVar destmemi, srcmemi;
      unsigned int i;
      struct Vector *v;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      v = &((LmnMembrane *)wt[srcmemi])->rulesets;
      for (i = 0; i< v->num; i++) {
        lmn_mem_add_ruleset((LmnMembrane *)wt[destmemi], lmn_ruleset_copy((LmnRuleSet)vec_get(v, i)));
      }
      break;
    }
    case INSTR_REMOVEPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      lmn_mem_remove_proxies((LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_INSERTPROXIES:
    {
      LmnInstrVar parentmemi, childmemi;

      READ_VAL(LmnInstrVar, instr, parentmemi);
      READ_VAL(LmnInstrVar, instr, childmemi);
      lmn_mem_insert_proxies((LmnMembrane *)wt[parentmemi], (LmnMembrane *)wt[childmemi]);
      break;
    }
    case INSTR_DELETECONNECTORS:
    {
      LmnInstrVar srcset, srcmap;
      HashSet *delset;
      ProcessTbl delmap;
      HashSetIterator it;
      READ_VAL(LmnInstrVar, instr, srcset);
      READ_VAL(LmnInstrVar, instr, srcmap);

      delset = (HashSet *)wt[srcset];
      delmap = (ProcessTbl)wt[srcmap];

      for(it = hashset_iterator(delset); !hashsetiter_isend(&it); hashsetiter_next(&it)) {
        LmnSAtom orig = LMN_SATOM(hashsetiter_entry(&it));
        LmnSAtom copy;
        LmnWord t;

        proc_tbl_get_by_atom(delmap, orig, &t);
        copy = LMN_SATOM(t);
        lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);
        /* mem がないので仕方なく直接アトムリストをつなぎ変える
           UNIFYアトムはnatomに含まれないので大丈夫 */
        LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(copy), LMN_SATOM_GET_PREV(copy));
        LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(copy), LMN_SATOM_GET_NEXT_RAW(copy));

        lmn_delete_atom(orig);
      }

      proc_tbl_free(delmap);
      break;
    }
    case INSTR_REMOVETOPLEVELPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      lmn_mem_remove_toplevel_proxies((LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_DEREFFUNC:
    {
      LmnInstrVar funci, atomi, pos;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, funci);
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, pos);

      attr = LMN_SATOM_GET_ATTR(LMN_SATOM(wt[atomi]), pos);
      if (LMN_ATTR_IS_DATA(attr)) {
        wt[funci] = LMN_SATOM_GET_LINK(LMN_SATOM(wt[atomi]), pos);
      }
      else { /* symbol atom */
        wt[funci] = LMN_SATOM_GET_FUNCTOR(LMN_SATOM_GET_LINK(LMN_SATOM(wt[atomi]), pos));
      }
      at[funci] = attr;
      break;
    }
    case INSTR_LOADFUNC:
    {
      LmnInstrVar funci;
      LmnLinkAttr attr;

      READ_VAL(LmnFunctor, instr, funci);
      READ_VAL(LmnLinkAttr, instr, attr);
      at[funci] = attr;
      if(LMN_ATTR_IS_DATA(attr)) {
        READ_CONST_DATA_ATOM(wt[funci], at[funci]);
      }
      else {
        LmnFunctor f;

        READ_VAL(LmnFunctor, instr, f);
        wt[funci] = (LmnWord)f;
      }
      break;
    }
    case INSTR_EQFUNC:
    {
      LmnInstrVar func0;
      LmnInstrVar func1;

      READ_VAL(LmnFunctor, instr, func0);
      READ_VAL(LmnFunctor, instr, func1);

      if (at[func0] != at[func1]) return FALSE;
      switch (at[func0]) {
      case LMN_INT_ATTR:
        if ((long)wt[func0] != (long)wt[func1]) return FALSE;
        break;
      case LMN_DBL_ATTR:
        if (*(double*)(&wt[func0]) !=
            *(double*)(&wt[func1])) return FALSE;
        break;
      default:
        if (wt[func0] != wt[func1]) return FALSE;
        break;
      }
      break;
    }
    case INSTR_NEQFUNC:
    {
      LmnInstrVar func0;
      LmnInstrVar func1;

      READ_VAL(LmnFunctor, instr, func0);
      READ_VAL(LmnFunctor, instr, func1);

      if (at[func0] == at[func1]) {
        switch (at[func0]) {
        case LMN_INT_ATTR:
          if ((long)wt[func0] == (long)wt[func1]) return FALSE;
          break;
        case LMN_DBL_ATTR:
          if (*(double*)(&wt[func0]) ==
              *(double*)(&wt[func1])) return FALSE;
          break;
        default:
          if (wt[func0] == wt[func1]) return FALSE;
          break;
        }
      }
      break;
    }
    case INSTR_ADDATOM:
    {
      LmnInstrVar memi, atomi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);
      lmn_mem_push_atom((LmnMembrane *)wt[memi], wt[atomi], at[atomi]);
      break;
    }
    case INSTR_MOVECELLS:
    {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      LMN_ASSERT(wt[destmemi] != wt[srcmemi]);
      lmn_mem_move_cells((LmnMembrane *)wt[destmemi], (LmnMembrane *)wt[srcmemi]);
      break;
    }
    case INSTR_REMOVETEMPORARYPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      lmn_mem_remove_temporary_proxies((LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_NFREELINKS:
    {
      LmnInstrVar memi, count;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, count);

      if (!lmn_mem_nfreelinks((LmnMembrane *)wt[memi], count)) return FALSE;
      break;
    }
    case INSTR_COPYCELLS:
    {
      LmnInstrVar mapi, destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, mapi);
      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      wt[mapi] = (LmnWord)lmn_mem_copy_cells((LmnMembrane *)wt[destmemi],
                                             (LmnMembrane *)wt[srcmemi]);
      break;
    }
    case INSTR_LOOKUPLINK:
    {
      LmnInstrVar destlinki, tbli, srclinki;

      READ_VAL(LmnInstrVar, instr, destlinki);
      READ_VAL(LmnInstrVar, instr, tbli);
      READ_VAL(LmnInstrVar, instr, srclinki);

      at[destlinki] = LINKED_ATTR(srclinki);
      if (LMN_ATTR_IS_DATA(LINKED_ATTR(srclinki))) {
        wt[destlinki] = LINKED_ATOM(srclinki);
      }
      else { /* symbol atom */
        ProcessTbl ht = (ProcessTbl)wt[tbli];
        proc_tbl_get_by_atom(ht, LMN_SATOM(LINKED_ATOM(srclinki)), &wt[destlinki]);
      }
      break;
    }
    case INSTR_CLEARRULES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      vec_clear(&((LmnMembrane *)wt[memi])->rulesets);
      break;
    }
    case INSTR_DROPMEM:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      lmn_mem_drop((LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_TESTMEM:
    {
      LmnInstrVar memi, atomi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);
      LMN_ASSERT(!LMN_ATTR_IS_DATA(at[atomi]));
      LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(wt[atomi])));

      if (LMN_PROXY_GET_MEM(wt[atomi]) != (LmnMembrane *)wt[memi]) return FALSE;
      break;
    }
    case INSTR_IADDFUNC:
    {
      LmnInstrVar desti, i0, i1;

      READ_VAL(LmnInstrVar, instr, desti);
      READ_VAL(LmnInstrVar, instr, i0);
      READ_VAL(LmnInstrVar, instr, i1);
      LMN_ASSERT(at[i0] == LMN_INT_ATTR);
      LMN_ASSERT(at[i1] == LMN_INT_ATTR);
      wt[desti] = wt[i0] + wt[i1];
      at[desti] = LMN_INT_ATTR;
      break;
    }
    case INSTR_ISUBFUNC:
    {
      LmnInstrVar desti, i0, i1;

      READ_VAL(LmnInstrVar, instr, desti);
      READ_VAL(LmnInstrVar, instr, i0);
      READ_VAL(LmnInstrVar, instr, i1);
      LMN_ASSERT(at[i0] == LMN_INT_ATTR);
      LMN_ASSERT(at[i1] == LMN_INT_ATTR);
      wt[desti] = wt[i0] - wt[i1];
      at[desti] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IMULFUNC:
    {
      LmnInstrVar desti, i0, i1;

      READ_VAL(LmnInstrVar, instr, desti);
      READ_VAL(LmnInstrVar, instr, i0);
      READ_VAL(LmnInstrVar, instr, i1);
      LMN_ASSERT(at[i0] == LMN_INT_ATTR);
      LMN_ASSERT(at[i1] == LMN_INT_ATTR);
      wt[desti] = wt[i0] * wt[i1];
      at[desti] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IDIVFUNC:
    {
      LmnInstrVar desti, i0, i1;

      READ_VAL(LmnInstrVar, instr, desti);
      READ_VAL(LmnInstrVar, instr, i0);
      READ_VAL(LmnInstrVar, instr, i1);
      LMN_ASSERT(at[i0] == LMN_INT_ATTR);
      LMN_ASSERT(at[i1] == LMN_INT_ATTR);
      wt[desti] = wt[i0] / wt[i1];
      at[desti] = LMN_INT_ATTR;
      break;
    }
    case INSTR_IMODFUNC:
    {
      LmnInstrVar desti, i0, i1;

      READ_VAL(LmnInstrVar, instr, desti);
      READ_VAL(LmnInstrVar, instr, i0);
      READ_VAL(LmnInstrVar, instr, i1);
      LMN_ASSERT(at[i0] == LMN_INT_ATTR);
      LMN_ASSERT(at[i1] == LMN_INT_ATTR);
      wt[desti] = wt[i0] % wt[i1];
      at[desti] = LMN_INT_ATTR;
      break;
    }
    case INSTR_GROUP:
    {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      if (!interpret(rc, rule, instr)) return FALSE;
      instr += subinstr_size;
      break;
    }
    case INSTR_BRANCH:
    {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      if (interpret(rc, rule, instr)) return TRUE;
      instr += subinstr_size;
      break;
    }
    case INSTR_LOOP:
    {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      while (interpret(rc, rule, instr)) ;
      instr += subinstr_size;
      break;
    }
    case INSTR_CALLBACK:
    {
      LmnInstrVar memi, atomi;
      LmnSAtom atom;
      const struct CCallback *c;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);

      atom = LMN_SATOM(wt[atomi]);

      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0))) {
        LmnSAtom f_name = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
        lmn_interned_str name = LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(f_name));
        int arity = LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom));

        c = get_ccallback(name);
        if (!c) break;

        if (arity-1 != c->arity) {
          fprintf(stderr, "EXTERNAL FUNC: invalid arity - %s\n", LMN_SYMBOL_STR(name));
          break;
        }

        lmn_mem_delete_atom((LmnMembrane *)wt[memi], wt[atomi], at[atomi]);
        lmn_mem_delete_atom((LmnMembrane *)wt[memi],
                            LMN_SATOM_GET_LINK(atom, 0),
                            LMN_SATOM_GET_ATTR(atom, 0));

        switch (arity) {
        case 1:
          ((callback_0)c->f)(rc, (LmnMembrane *)wt[memi]);
          break;
        case 2:
          ((callback_1)c->f)(rc,
                             (LmnMembrane *)wt[memi],
                          LMN_SATOM_GET_LINK(atom, 1), LMN_SATOM_GET_ATTR(atom, 1));
          break;
        case 3:
          ((callback_2)c->f)(rc,
                             (LmnMembrane *)wt[memi],
                          LMN_SATOM_GET_LINK(atom, 1), LMN_SATOM_GET_ATTR(atom, 1),
                          LMN_SATOM_GET_LINK(atom, 2), LMN_SATOM_GET_ATTR(atom, 2));
          break;
        case 4:
          ((callback_3)c->f)(rc,
                             (LmnMembrane *)wt[memi],
                          LMN_SATOM_GET_LINK(atom, 1), LMN_SATOM_GET_ATTR(atom, 1),
                          LMN_SATOM_GET_LINK(atom, 2), LMN_SATOM_GET_ATTR(atom, 2),
                          LMN_SATOM_GET_LINK(atom, 3), LMN_SATOM_GET_ATTR(atom, 3));
          break;
        case 5:
          ((callback_4)c->f)(rc,
                             (LmnMembrane *)wt[memi],
                          LMN_SATOM_GET_LINK(atom, 1), LMN_SATOM_GET_ATTR(atom, 1),
                          LMN_SATOM_GET_LINK(atom, 2), LMN_SATOM_GET_ATTR(atom, 2),
                          LMN_SATOM_GET_LINK(atom, 3), LMN_SATOM_GET_ATTR(atom, 3),
                          LMN_SATOM_GET_LINK(atom, 4), LMN_SATOM_GET_ATTR(atom, 4));
          break;
        default:
          printf("EXTERNAL FUNCTION: too many arguments\n");
          break;
        }
      }

      break;
    }
    case INSTR_GETCLASS:
    {
      LmnInstrVar reti, atomi;

      READ_VAL(LmnInstrVar, instr, reti);
      READ_VAL(LmnInstrVar, instr, atomi);

      if (LMN_ATTR_IS_DATA(at[atomi])) {
        switch (at[atomi]) {
        case LMN_INT_ATTR:
          wt[reti] = lmn_intern("int");
          break;
        case LMN_DBL_ATTR:
          wt[reti] = lmn_intern("float");
          break;
        case LMN_SP_ATOM_ATTR:
          wt[reti] = SP_ATOM_NAME(wt[atomi]);
          break;
        }
      } else { /* symbol atom */
        wt[reti] = lmn_intern("symbol");
      }
      break;
    }
    case INSTR_SUBCLASS:
    {
      LmnInstrVar subi, superi;

      READ_VAL(LmnInstrVar, instr, subi);
      READ_VAL(LmnInstrVar, instr, superi);

      /* サブやスーパークラスなどの階層の概念がないので単純比較を行う */
      if (wt[subi] != wt[superi]) return FALSE;
      break;
    }
    default:
      fprintf(stderr, "interpret: Unknown operation %d\n", op);
      exit(1);
    }
/*     lmn_dump_mem((LmnMembrane *)wt[0]); */
/*     print_wt(); */

    #ifdef DEBUG
/*     print_wt(); */
    #endif
  }
}

/* DEBUG: */
/* static void print_wt(void) */
/* { */
/*   unsigned int i; */
/*   unsigned int end = 16; */

/*   fprintf(stderr, " wt: ["); */
/*   for (i = 0; i < end; i++) { */
/*     if (i>0) fprintf(stderr, ", "); */
/*     fprintf(stderr, "%lu", wt[i]); */
/*   } */
/*   fprintf(stderr, "]"); */
/*   fprintf(stderr, "\n"); */
/*   fprintf(stderr, " at: ["); */
/*   for (i = 0; i < end; i++) { */
/*     if (i>0) fprintf(stderr, ", "); */
/*     fprintf(stderr, "%u", at[i]); */
/*   } */
/*   fprintf(stderr, "]"); */
/*   fprintf(stderr, "\n"); */
/* } */


inline Vector *links_from_idxs(const Vector *link_idxs, LmnWord *wt, LmnByte *at)
{
  unsigned long i;
  Vector *v = vec_make(16);

  /* リンクオブジェクトのベクタを構築 */
  for (i = 0; i < vec_num(link_idxs); i++) {
    LinkObj l = LinkObj_make(wt[vec_get(link_idxs, i)],
                             at[vec_get(link_idxs, i)]);
    vec_push(v, (LmnWord)l);
  }
  return v;
}

inline void free_links(Vector *links)
{
  unsigned long i;

  for (i = 0; i < vec_num(links); i++) {
    LMN_FREE(vec_get(links, i));
  }
  vec_free(links);
}
