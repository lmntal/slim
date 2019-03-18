/*
 * task.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

#include "task.h"
#include "ccallback.h"
#include "dumper.h"
#include "interpret/false_driven_enumerator.hpp"
#include "interpret/interpreter.hpp"
#include "memstack.h"
#include "normal_thread.h"
#include "special_atom.h"
#include "symbol.h"
#include "verifier/runtime_status.h"
#include "verifier/verifier.h"

#ifdef USE_FIRSTCLASS_RULE
#include "firstclass_rule.h"
#endif

#include <algorithm>

typedef void (*callback_0)(LmnReactCxtRef, LmnMembraneRef);
typedef void (*callback_1)(LmnReactCxtRef, LmnMembraneRef, LmnAtomRef,
                           LmnLinkAttr);
typedef void (*callback_2)(LmnReactCxtRef, LmnMembraneRef, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr);
typedef void (*callback_3)(LmnReactCxtRef, LmnMembraneRef, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr, LmnAtomRef,
                           LmnLinkAttr);
typedef void (*callback_4)(LmnReactCxtRef, LmnMembraneRef, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr);

typedef void (*callback_5)(LmnReactCxtRef, LmnMembraneRef, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr, LmnAtomRef,
                           LmnLinkAttr, LmnAtomRef, LmnLinkAttr, LmnAtomRef,
                           LmnLinkAttr);

struct Vector user_system_rulesets; /* system ruleset defined by user */

/**
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
  でリンク先の取得ができるようにしている。

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

         (Bのリンク先を得る)        (Aのリンク先を得る)    aが消滅
                          +---------+    +---------+    +---------+-+    +----+
                          |         |    |         |    |         | |    |    |
  B             B         v     B   |    v     B   |    v     B   | |    v    |
  |         b-->|         b     |   |    b     |   |    b     |   | |    b    |
+---0<--+    +---0<--+    | +---0   |    | +---0   |    | +---0   | |    |    |
|   a   | => |   a   | => | |   a   | => | |   a   | => | |   a   | | => |    |
+-->1---+    +-->1---+    +-+-->1---+    +-+-->1---+    | +-->1---+ |    |    |
      |            |              |              |<--b  +------|--->b    +--->b
      A            A              A              A             A
*/

/* リンク先のアトムを得る */
#define LINKED_ATOM(LINKI) ((LmnAtomRef)rc->wt(LINKI))
/* リンク先のアトムの引数のattributeを得る */
#define LINKED_ATTR(LINKI) rc->at(LINKI)

static inline BOOL react_ruleset(LmnReactCxtRef rc, LmnMembraneRef mem,
                                 LmnRuleSetRef ruleset);
static inline void react_initial_rulesets(LmnReactCxtRef rc,
                                          LmnMembraneRef mem);
static inline BOOL react_ruleset_in_all_mem(LmnReactCxtRef rc, LmnRuleSetRef rs,
                                            LmnMembraneRef mem);
static BOOL dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule,
                           LmnRuleInstr instr);

static void mem_oriented_loop(LmnReactCxtRef rc, LmnMembraneRef mem);

void lmn_dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule,
                        LmnRuleInstr instr) {
  dmem_interpret(rc, rule, instr);
}

namespace c14 = slim::element;
namespace c17 = slim::element;

/** 通常実行時の入口.
 *  インタタラクティブ実行時の処理フローは以下の通り[yueno]
 *    1. 後始末     normal_cleaningフラグがたっている時
 *    2. 初期化     !(normal_remainモード時 && normal_remaining=ON) 時
 *    3. 処理       常に行う
 *    4. 後始末     通常モード時
 *    5. 継続フラグ 通常モードならON、normal_remainモードならOFFにセットする
 */
void lmn_run(Vector *start_rulesets) {
  static LmnMembraneRef mem;
  static std::unique_ptr<MemReactContext> mrc = nullptr;

  if (!mrc)
    mrc = c14::make_unique<MemReactContext>();

#ifdef USE_FIRSTCLASS_RULE
  first_class_rule_tbl_init();
#endif

  /* 通常実行では非決定実行とは異なりProcess IDを
   * 1から再割り当てする機会(状態圧縮と復元)が存在しない.
   * 破棄したProcessのIDを使い回す必要がある.
   * もし通常実行の並列化を考えるならばIDを再利用するためのMT-Safeな機構が必要
   */
  if (!env_proc_id_pool()) {
    env_set_proc_id_pool(vec_make(64));
  }

  /* interactive: normal_cleaningフラグがONの場合は後始末 */
  if (lmn_env.normal_cleaning) {
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
    mrc = nullptr;
    lmn_env.normal_cleaning = FALSE;
  }

  /* interactive : (normal_remain時でnormal_remaining=ON)以外の場合は初期化 */
  if (!lmn_env.normal_remain && !lmn_env.normal_remaining) {
    mrc = c14::make_unique<MemReactContext>();
    mem = lmn_mem_make();
    RC_SET_GROOT_MEM(mrc.get(), mem);
  }
  lmn_memstack_push(RC_MEMSTACK((MemReactContext *)mrc.get()), mem);

  // normal parallel mode init
  if (lmn_env.enable_parallel && !lmn_env.nd) {
    normal_parallel_init();
  }

  /** PROFILE START */
  if (lmn_env.profile_level >= 1) {
    profile_start_exec();
    profile_start_exec_thread();
  }

  react_start_rulesets(mem, start_rulesets);
  lmn_memstack_reconstruct(RC_MEMSTACK((MemReactContext *)mrc.get()), mem);

  if (lmn_env.trace) {
    if (lmn_env.output_format != JSON)
      fprintf(stdout, "%d: ", RC_TRACE_NUM_INC(mrc.get()));
    lmn_dump_cell_stdout(mem);
    if (lmn_env.show_hyperlink)
      lmn_hyperlink_print(mem);
  }

  mem_oriented_loop(mrc.get(), mem);

  /** PROFILE FINISH */
  if (lmn_env.profile_level >= 1) {
    profile_finish_exec_thread();
    profile_finish_exec();
  }

  if (lmn_env
          .dump) { /* lmntalではioモジュールがあるけど必ず実行結果を出力するプログラミング言語,
                      で良い?? */
    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      fprintf(stdout, "finish.\n");
    } else {
      lmn_dump_cell_stdout(mem);
    }
  }
  if (lmn_env.show_hyperlink)
    lmn_hyperlink_print(mem);

  // normal parallel mode free
  if (lmn_env.enable_parallel && !lmn_env.nd) {
    if (lmn_env.profile_level == 3)
      normal_parallel_prof_dump(stderr);
    normal_parallel_free();
  }

  /* 後始末 */
  if (lmn_env.normal_remain) {
    lmn_env.normal_remaining = TRUE;
  } else {
    lmn_env.normal_remaining = FALSE;
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
    mrc = nullptr;
  }
  if (env_proc_id_pool()) {
    vec_free(env_proc_id_pool());
  }
}

/** 膜スタックに基づいた通常実行 */
static void mem_oriented_loop(LmnReactCxtRef rc, LmnMembraneRef mem) {
  LmnMemStack memstack = RC_MEMSTACK((MemReactContext *)rc);

  while (!lmn_memstack_isempty(memstack)) {
    LmnMembraneRef mem = lmn_memstack_peek(memstack);
    if (!react_all_rulesets(rc, mem)) {
      /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
      lmn_memstack_pop(memstack);
    }
  }
}

/**
 * @brief 膜内の0stepルールセットを適用できるだけ適用する
 */
void react_zerostep_rulesets(LmnReactCxtRef rc, LmnMembraneRef cur_mem) {
  struct Vector *rulesets = lmn_mem_get_rulesets(cur_mem);
  BOOL reacted = FALSE;

  rc->is_zerostep = true;
  do {
    reacted = FALSE;
    for (int i = 0; i < vec_num(rulesets); i++) {
      LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(rulesets, i);
      if (!rs->is_zerostep())
        continue;
      reacted |= react_ruleset(rc, cur_mem, rs);
    }
  } while (reacted);
  rc->is_zerostep = false;
}

/**
 * @brief 膜内の子膜に再帰的に0stepルールセットを適用する
 * @sa react_zerostep_rulesetsm
 */
void react_zerostep_recursive(LmnReactCxtRef rc, LmnMembraneRef cur_mem) {
  for (; cur_mem; cur_mem = lmn_mem_next(cur_mem)) {
    react_zerostep_recursive(rc, lmn_mem_child_head(cur_mem));
    react_zerostep_rulesets(rc, cur_mem);
  }
}

/** cur_memに存在するルールすべてに対して適用を試みる
 * @see mem_oriented_loop (task.c)
 * @see expand_inner      (nd.c) */
BOOL react_all_rulesets(LmnReactCxtRef rc, LmnMembraneRef cur_mem) {
  unsigned int i;
  struct Vector *rulesets =
      lmn_mem_get_rulesets(cur_mem); /* 本膜のルールセットの集合 */
  BOOL ok = FALSE;

  /* ルールセットの適用 */
  for (i = 0; i < vec_num(rulesets); i++) {
    if (react_ruleset(rc, cur_mem, (LmnRuleSetRef)vec_get(rulesets, i))) {
      /* ndでは失敗するまでマッチングバックトラックしているので必ずFALSEが返ってくる
       */
      ok = TRUE;
      break;
    }
  }

#ifdef USE_FIRSTCLASS_RULE
  for (i = 0; i < vec_num(lmn_mem_firstclass_rulesets(cur_mem)); i++) {
    if (react_ruleset(
            rc, cur_mem,
            (LmnRuleSetRef)vec_get(lmn_mem_firstclass_rulesets(cur_mem), i))) {
      ok = TRUE;
      break;
    }
  }
#endif

  /* 通常実行では, 適用が発生しなかった場合にシステムルールの適用を行う
   * ndではokはFALSEなので, system_rulesetが適用される. */
  ok = ok || react_ruleset(rc, cur_mem, system_ruleset);

#ifdef USE_FIRSTCLASS_RULE
  lmn_rc_execute_insertion_events(rc);
#endif

  return ok;
}

/** 膜memに対してルールセットrsの各ルールの適用を試みる.
 *  戻り値:
 *   通常実行では, 書換えに成功した場合にTRUE,
 * マッチングしなかった場合にFALSEを返す.
 *   非決定実行では常にFALSEを返す(マッチングに失敗するまでバックトラックする仕様).
 */
static inline BOOL react_ruleset(LmnReactCxtRef rc, LmnMembraneRef mem,
                                 LmnRuleSetRef rs) {
  for (auto r : *rs) {
#ifdef PROFILE
    if (!lmn_env.nd && lmn_env.profile_level >= 2)
      profile_rule_obj_set(rs, r);
#endif
    if (react_rule(rc, mem, r))
      return true;
  }
  return false;
}

/** 膜memに対してルールruleの適用を試みる.
 *  戻り値:
 *   通常実行では, 書換えに成功した場合にTRUE,
 * マッチングしなかった場合にFALSEを返す. 非決定実行では,
 * マッチングに失敗するまでバックトラックを繰り返すため常にFALSEが返る. */
BOOL react_rule(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule) {
  LmnTranslated translated;
  BYTE *inst_seq;
  BOOL result;

  translated = rule->translated;
  inst_seq = rule->inst_seq;

  rc->resize(1);
  rc->wt(0) = (LmnWord)mem;
  rc->tt(0) = TT_MEM;

  profile_start_trial();

  if (lmn_env.enable_parallel && !lmn_env.nd)
    rule_wall_time_start();

  /* まず、トランスレート済みの関数を実行する
   * それがない場合、命令列をinterpretで実行する */
  slim::vm::interpreter in(rc, rule, inst_seq);
  result = (translated && translated(rc, mem, rule)) || (inst_seq && in.run());

  if (lmn_env.enable_parallel && !lmn_env.nd && normal_parallel_flag)
    rule_wall_time_finish();

  /* 適用に成功したら0step実行に入る。既に入っていれば何もしない */
  if (result && !rc->is_zerostep)
    react_zerostep_rulesets(rc, mem);

  profile_finish_trial();

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED) && !rc->is_zerostep) {
    if (lmn_env.trace && result) {
      if (lmn_env.sp_dump_format == LMN_SYNTAX) {
        lmn_dump_mem_stdout(RC_GROOT_MEM(rc));
        fprintf(stdout, ".\n");
        lmn_dump_mem_stdout(RC_GROOT_MEM(rc));
        fprintf(stdout, ":- ");
        RC_TRACE_NUM_INC(rc);
      } else if (lmn_env.output_format == JSON) {
        lmn_dump_cell_stdout(RC_GROOT_MEM(rc));
      } else {
        fprintf(stdout, "---->%s\n", lmn_id_to_name(rule->name));
        fprintf(stdout, "%d: ", RC_TRACE_NUM_INC(rc));
        lmn_dump_cell_stdout(RC_GROOT_MEM(rc));
        if (lmn_env.show_hyperlink)
          lmn_hyperlink_print(RC_GROOT_MEM(rc));
      }
    }
  }

  if (RC_HLINK_SPC(rc)) {
    lmn_sameproccxt_clear(rc); /* とりあえずここに配置 */
    // normal parallel destroy
    if (lmn_env.enable_parallel && !lmn_env.nd) {
      int i;
      for (i = 0; i < lmn_env.core_num; i++) {
        lmn_sameproccxt_clear(thread_info[i]->rc);
      }
    }
  }

  return result;
}

/* 膜memでrulesetsのルールの適用を行う.
 * 適用結果は無視する */
void react_start_rulesets(LmnMembraneRef mem, Vector *rulesets) {
  LmnReactCxt rc(REACT_STAND_ALONE);
  int i;

  RC_SET_GROOT_MEM(&rc, mem);

  for (i = 0; i < vec_num(rulesets); i++) {
    react_ruleset(&rc, mem, (LmnRuleSetRef)vec_get(rulesets, i));
  }
  react_initial_rulesets(&rc, mem);
  react_zerostep_recursive(&rc, mem);

#ifdef USE_FIRSTCLASS_RULE
  // register first-class rulesets produced by the initial process.
  lmn_rc_execute_insertion_events(&rc);
#endif
}

inline static void react_initial_rulesets(LmnReactCxtRef rc,
                                          LmnMembraneRef mem) {
  BOOL reacted;

  do {
    reacted = FALSE;
    if (react_ruleset_in_all_mem(rc, initial_system_ruleset, mem)) {
      reacted = TRUE;
      continue;
    }
    for (auto r : *initial_ruleset) {
      if (react_rule(rc, mem, r)) {
        reacted = TRUE;
        break;
      }
    }
  } while (reacted);
}

/* ルールセットrsをmem以下のすべての膜内で適用する */
static BOOL react_ruleset_in_all_mem(LmnReactCxtRef rc, LmnRuleSetRef rs,
                                     LmnMembraneRef mem) {
  LmnMembraneRef m;

  for (m = lmn_mem_child_head(mem); m; m = lmn_mem_next(m)) {
    if (react_ruleset_in_all_mem(rc, rs, m))
      return TRUE;
  }

  return react_ruleset(rc, mem, rs);
}

/* Utility for reading data */

#define READ_DATA_ATOM(dest, attr)                                             \
  do {                                                                         \
    switch (attr) {                                                            \
    case LMN_INT_ATTR: {                                                       \
      long n;                                                                  \
      READ_VAL(long, instr, n);                                                \
      (dest) = (LmnAtomRef)n;                                                  \
      break;                                                                   \
    }                                                                          \
    case LMN_DBL_ATTR: {                                                       \
      double x;                                                                \
      READ_VAL(double, instr, x);                                              \
      (dest) = (LmnAtomRef)lmn_create_double_atom(x);                          \
      break;                                                                   \
    }                                                                          \
    case LMN_STRING_ATTR: {                                                    \
      lmn_interned_str s;                                                      \
      READ_VAL(lmn_interned_str, instr, s);                                    \
      (dest) = (LmnAtomRef)lmn_string_make(lmn_id_to_name(s));                 \
      break;                                                                   \
    }                                                                          \
    default:                                                                   \
      lmn_fatal("Implementation error 1");                                     \
    }                                                                          \
  } while (0)

/* attrに応じて、ファンクタを読み込み、destに定数アトムを生成する。
 * attrは適切な値に変更する場合がある */
#define READ_CONST_DATA_ATOM(dest, attr, type)                                 \
  do {                                                                         \
    switch (attr) {                                                            \
    case LMN_INT_ATTR:                                                         \
      READ_VAL(long, instr, (dest));                                           \
      break;                                                                   \
    case LMN_DBL_ATTR: {                                                       \
      double x;                                                                \
      READ_VAL(double, instr, x);                                              \
      (dest) = (LmnWord)lmn_create_double_atom(x);                             \
      break;                                                                   \
    }                                                                          \
    case LMN_STRING_ATTR: {                                                    \
      lmn_interned_str s;                                                      \
      READ_VAL(lmn_interned_str, instr, s);                                    \
      (dest) = s;                                                              \
      (attr) = LMN_CONST_STR_ATTR;                                             \
      break;                                                                   \
    }                                                                          \
    default:                                                                   \
      lmn_fatal("Implementation error 2");                                     \
    }                                                                          \
    (type) = TT_OTHER;                                                         \
  } while (0)

#define READ_CMP_DATA_ATOM(attr, x, result, type)                              \
  do {                                                                         \
    switch (attr) {                                                            \
    case LMN_INT_ATTR: {                                                       \
      long t;                                                                  \
      READ_VAL(long, instr, t);                                                \
      (result) = ((long)(x) == t);                                             \
      break;                                                                   \
    }                                                                          \
    case LMN_DBL_ATTR: {                                                       \
      double t;                                                                \
      READ_VAL(double, instr, t);                                              \
      (result) = (lmn_get_double(x) == t);                                     \
      break;                                                                   \
    }                                                                          \
    case LMN_STRING_ATTR: {                                                    \
      lmn_interned_str s;                                                      \
      LmnStringRef str1;                                                       \
      READ_VAL(lmn_interned_str, instr, s);                                    \
      str1 = lmn_string_make(lmn_id_to_name(s));                               \
      (result) = lmn_string_eq(str1, (LmnStringRef)(x));                       \
      lmn_string_free(str1);                                                   \
      break;                                                                   \
    }                                                                          \
    default:                                                                   \
      lmn_fatal("Implementation error 3");                                     \
    }                                                                          \
    (type) = TT_ATOM;                                                          \
  } while (0)

#define SKIP_DATA_ATOM(attr)                                                   \
  do {                                                                         \
    switch (attr) {                                                            \
    case LMN_INT_ATTR: {                                                       \
      SKIP_VAL(long, instr);                                                   \
      break;                                                                   \
    }                                                                          \
    case LMN_DBL_ATTR: {                                                       \
      SKIP_VAL(double, instr);                                                 \
      break;                                                                   \
    }                                                                          \
    case LMN_STRING_ATTR: {                                                    \
      SKIP_VAL(lmn_interned_str, instr);                                       \
      break;                                                                   \
    }                                                                          \
    default:                                                                   \
      lmn_fatal("Implementation error 4");                                     \
    }                                                                          \
  } while (0)

/* DEBUG: */
/* static void print_wt(void); */

/* mem != NULL ならば memにUNIFYを追加、そうでなければUNIFYは膜に所属しない */
HashSet *insertconnectors(slim::vm::RuleContext *rc, LmnMembraneRef mem,
                          const Vector *links) {
  unsigned int i, j;
  HashSet *retset;
  /* EFFICIENCY: retsetがHash Setである意味は?　ベクタでいいのでは？
   * 中間命令でセットを使うように書かれている */

  retset = new HashSet(8);
  for (i = 0; i < links->num; i++) {
    LmnWord linkid1 = vec_get(links, i);
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(linkid1)))
      continue;
    for (j = i + 1; j < links->num; j++) {
      LmnWord linkid2 = vec_get(links, j);
      if (LMN_ATTR_IS_DATA(LINKED_ATTR(linkid2)))
        continue;
      /* is buddy? */
      if (LINKED_ATOM(linkid2) ==
              LMN_SATOM_GET_LINK((LmnSymbolAtomRef)LINKED_ATOM(linkid1),
                                 LINKED_ATTR(linkid1)) &&
          LINKED_ATTR(linkid2) ==
              LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(linkid1),
                                 LINKED_ATTR(linkid1))) {
        /* '='アトムをはさむ */
        LmnSymbolAtomRef eq;
        LmnSymbolAtomRef a1, a2;
        LmnLinkAttr t1, t2;

        if (mem)
          eq = lmn_mem_newatom(mem, LMN_UNIFY_FUNCTOR);
        else {
          eq = lmn_new_atom(LMN_UNIFY_FUNCTOR);
        }

        if (LMN_SATOM_ID(eq) == 0) {
          LMN_SATOM_SET_ID(eq, env_gen_next_id());
        }

        /* リンクがリンクの元を持つ場合、あらかじめリンク先の取得をしていなければならない。
         * リンク元はnew_link時に書き換えられてしまう。*/

        a1 = (LmnSymbolAtomRef)LINKED_ATOM(linkid1);
        a2 = (LmnSymbolAtomRef)LINKED_ATOM(linkid2);
        t1 = LINKED_ATTR(linkid1);
        t2 = LINKED_ATTR(linkid2);

        lmn_newlink_in_symbols(a1, t1, eq, 0);
        lmn_newlink_in_symbols(a2, t2, eq, 1);
        retset->add((HashKeyType)eq);
      }
    }
  }

  return retset;
}

void slim::vm::interpreter::findatom(LmnReactCxtRef rc, LmnRuleRef rule,
                                     LmnRuleInstr instr, LmnMembrane *mem,
                                     LmnFunctor f, size_t reg) {
  auto atomlist_ent = lmn_mem_get_atomlist(mem, f);

  if (!atomlist_ent)
    return;

  auto iter = std::begin(*atomlist_ent);
  auto end = std::end(*atomlist_ent);
  if (iter == end)
    return;

  auto v = std::vector<LmnRegister>();
  std::transform(iter, end, std::back_inserter(v), [](LmnSymbolAtomRef atom) {
    return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
  });

  this->push_stackframe(
      make_false_driven_enumerator(*this, instr, reg, std::move(v)));
}

/** find atom with a hyperlink occurred in the current rule for the first time.
 */
void slim::vm::interpreter::findatom_original_hyperlink(
    LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr, SameProcCxt *spc,
    LmnMembrane *mem, LmnFunctor f, size_t reg) {
  auto atomlist_ent = lmn_mem_get_atomlist(mem, f);
  if (!atomlist_ent)
    return;

  auto filtered = slim::element::make_range_remove_if(
      std::begin(*atomlist_ent), std::end(*atomlist_ent),
      [=](LmnSymbolAtomRef atom) {
        return !spc->is_consistent_with((LmnSymbolAtomRef)atom);
      });
  auto v = std::vector<std::function<LmnRegister(void)>>();
  std::transform(
      filtered.begin(), filtered.end(), std::back_inserter(v),
      [=](LmnSymbolAtomRef atom) {
        return [=]() {
          spc->match(atom);
          return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
        };
      });

  this->push_stackframe(
      make_false_driven_enumerator(*this, instr, reg, std::move(v)));
}

void lmn_hyperlink_get_elements(std::vector<HyperLink *> &tree,
                                HyperLink *start_hl) {
  Vector vec;
  vec_init(&vec, 1);
  lmn_hyperlink_get_elements(&vec, start_hl);
  for (int i = 0; i < vec_num(&vec); i++)
    tree.push_back((HyperLink *)vec_get(&vec, i));
  vec_destroy(&vec);
}

/** find atom with a hyperlink occurred again in the current rule. */
void slim::vm::interpreter::findatom_clone_hyperlink(
    LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr, SameProcCxt *spc,
    LmnMembrane *mem, LmnFunctor f, size_t reg) {
  /* 探索の始点となるhyperlinkと同じ集合に含まれるhyperlink群を
   * (Vector*)LMN_SPC_TREE(spc)に取得.
   * (Vector*)LMN_SPC_TREE(spc)の最後に格納されているHyperLinkは
   * 探索の対象外なので要素数を-1する */
  HyperLink *h = spc->start();
  lmn_hyperlink_get_elements(spc->tree, h);
  auto element_num = spc->tree.size() - 1;
  if (element_num <= 0)
    return;

  std::vector<HyperLink *> children;
  for (int i = 0; i < element_num; i++)
    children.push_back(spc->tree[i]);

  /* ----------------------------------------------------------
   * この時点で探索の始点とすべきハイパーリンクの情報がspc内に格納されている
   * ---------------------------------------------------------- */

  if (!lmn_mem_get_atomlist(mem, LMN_HL_FUNC))
    return;

  auto filtered = slim::element::make_range_remove_if(
      children.begin(), children.end(), [=](HyperLink *h) {
        auto linked_pc = h->atom;
        auto hl_atom = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(linked_pc, 0);
        /*    本来findatomするはずだったファンクタと一致しているか
         * || hyperlinkアトムの接続先の引数が正しいか
         * || 本来findatomするはずだったアトムと所属膜が一致しているか
         */
        return LMN_SATOM_GET_FUNCTOR(hl_atom) != f ||
               spc->start_attr != LMN_SATOM_GET_ATTR(linked_pc, 0) ||
               LMN_HL_MEM(lmn_hyperlink_at_to_hl(linked_pc)) != mem ||
               !spc->is_consistent_with(hl_atom);
      });
  std::vector<std::function<LmnRegister(void)>> v;
  std::transform(
      filtered.begin(), filtered.end(), std::back_inserter(v),
      [=](HyperLink *h) {
        return [=]() {
          auto atom = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(h->atom, 0);
          spc->match(atom);
          return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
        };
      });

  this->push_stackframe(
      make_false_driven_enumerator(*this, instr, reg, std::move(v)));
}

/** hyperlinkの接続関係からfindatom */
void slim::vm::interpreter::findatom_through_hyperlink(
    LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr, SameProcCxt *spc,
    LmnMembrane *mem, LmnFunctor f, size_t reg) {
  auto atom_arity = LMN_FUNCTOR_ARITY(f);

  /* 型付きプロセス文脈atomiがoriginal/cloneのどちらであるか判別 */
  if (spc->is_clone(atom_arity)) {
    findatom_clone_hyperlink(rc, rule, instr, spc, mem, f, reg);
  } else {
    findatom_original_hyperlink(rc, rule, instr, spc, mem, f, reg);
  }
}

BOOL ground_atoms(Vector *srcvec, Vector *avovec,
                  std::unique_ptr<ProcessTbl> &atoms, unsigned long *natoms,
                  std::unique_ptr<ProcessTbl> &hlinks,
                  const std::vector<LmnFunctor> &attr_functors,
                  const std::vector<LmnWord> &attr_dataAtoms,
                  const std::vector<LmnLinkAttr> &attr_dataAtom_attrs) {
  auto v1 = vec_make(attr_dataAtoms.size());
  auto v2 = vec_make(attr_dataAtom_attrs.size());
  auto p1 = proc_tbl_make_with_size(attr_functors.size());

  for (auto &v : attr_dataAtoms)
    vec_push(v1, v);
  for (auto &v : attr_dataAtom_attrs)
    vec_push(v2, v);
  for (auto &p : attr_functors)
    proc_tbl_put(p1, p, p);

  auto a = atoms.get();
  auto h = hlinks.get();
  auto result = ground_atoms(srcvec, avovec, &a, natoms, &h, &p1, v1, v2);
  atoms = std::unique_ptr<ProcessTbl>(a);
  hlinks = std::unique_ptr<ProcessTbl>(h);
  vec_free(v1);
  vec_free(v2);
  proc_tbl_free(p1);
  return result;
}

BOOL ground_atoms(Vector *srcvec, Vector *avovec,
                  std::unique_ptr<ProcessTbl> &atoms, unsigned long *natoms) {
  auto a = atoms.get();
  auto result = ground_atoms(srcvec, avovec, &a, natoms, nullptr, nullptr,
                             nullptr, nullptr);
  atoms = std::unique_ptr<ProcessTbl>(a);
  return result;
}

std::vector<c17::variant<std::pair<LmnLinkAttr, LmnAtomRef>, LmnFunctor>>
read_unary_atoms(LmnReactCxt *rc, LmnRuleInstr &instr) {
  std::vector<c17::variant<std::pair<LmnLinkAttr, LmnAtomRef>, LmnFunctor>>
      args;
  LmnInstrVar n;
  READ_VAL(LmnInstrVar, instr, n);

  for (int i = 0; i < n; i++) {
    LmnLinkAttr attr;
    READ_VAL(LmnLinkAttr, instr, attr);
    if (LMN_ATTR_IS_DATA(attr)) {
      LmnAtomRef at;
      READ_DATA_ATOM(at, attr);
      args.push_back(std::make_pair(attr, at));
    } else {
      LmnFunctor f;
      READ_VAL(LmnFunctor, instr, f);
      args.push_back(f);
    }
  }
  return args;
}

std::vector<c17::variant<std::pair<LmnLinkAttr, LmnAtomRef>, LmnFunctor>>
read_unary_atoms_indirect(LmnReactCxt *rc, LmnRuleInstr &instr) {
  std::vector<c17::variant<std::pair<LmnLinkAttr, LmnAtomRef>, LmnFunctor>>
      args;
  LmnInstrVar n;
  READ_VAL(LmnInstrVar, instr, n);

  for (int i = 0; i < n; i++) {
    LmnInstrVar ai;
    READ_VAL(LmnInstrVar, instr, ai);
    if (LMN_ATTR_IS_DATA(rc->at(ai))) {
      args.push_back(std::make_pair(rc->at(ai), (LmnAtomRef)rc->wt(ai)));
    } else {
      args.push_back(LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(ai)));
    }
  }
  return args;
}

std::vector<HyperLink *> lmn_hyperlink_get_elements(HyperLink *start_hl) {
  std::vector<HyperLink *> vec;
  Vector tree;
  vec_init(&tree, 32);
  lmn_hyperlink_get_elements(&tree, start_hl);
  for (int i = 0; i < vec_num(&tree) - 1; i++)
    vec.push_back((HyperLink *)vec_get(&tree, i));
  vec_destroy(&tree);
  return vec;
}

struct exec_subinstructions_while {
  slim::vm::interpreter &interpreter;
  LmnRuleInstr body;
  LmnRuleInstr next;

  exec_subinstructions_while(slim::vm::interpreter &interpreter,
                             LmnRuleInstr body, LmnRuleInstr next)
      : interpreter(interpreter), body(body), next(next) {}

  bool operator()(bool result) {
    if (result) {
      interpreter.push_stackframe(
          exec_subinstructions_while(interpreter, body, next));
      interpreter.instr = body;
    } else {
      interpreter.instr = next;
    }
    return interpreter.run();
  }
};

/**
 *  execute a command at instr.
 *  instr is incremented by the size of operation.
 *  returns true if execution finished sucessfully.
 *  stop becomes true only if executien should be aborted.
 */
bool slim::vm::interpreter::exec_command(LmnReactCxt *rc, LmnRuleRef rule,
                                         LmnRuleInstr &instr, bool &stop) {
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr, op);
  stop = true;

  if (lmn_env.find_atom_parallel)
    return FALSE;

  switch (op) {
  case INSTR_SPEC: {
    LmnInstrVar s0;

    SKIP_VAL(LmnInstrVar, instr);
    READ_VAL(LmnInstrVar, instr, s0);

    rc->resize(s0);
    break;
  }
  case INSTR_INSERTCONNECTORSINNULL: {
    LmnInstrVar seti, list_num;
    Vector links;
    unsigned int i;

    READ_VAL(LmnInstrVar, instr, seti);
    READ_VAL(LmnInstrVar, instr, list_num);

    vec_init(&links, list_num + 1);
    for (i = 0; i < list_num; i++) {
      LmnInstrVar t;
      READ_VAL(LmnInstrVar, instr, t);
      vec_push(&links, (LmnWord)t);
    }

    auto hashset = insertconnectors(rc, NULL, &links);
    rc->reg(seti) = {(LmnWord)hashset, 0, TT_OTHER};

    vec_destroy(&links);

    /* EFFICIENCY: 解放のための再帰 */
    this->push_stackframe([=](bool result) {
      hashset_free(hashset);
      return result;
    });

    break;
  }
  case INSTR_INSERTCONNECTORS: {
    LmnInstrVar seti, list_num, memi, enti;
    Vector links; /* src list */
    unsigned int i;

    READ_VAL(LmnInstrVar, instr, seti);
    READ_VAL(LmnInstrVar, instr, list_num);

    vec_init(&links, list_num + 1);

    for (i = 0; i < list_num; i++) {
      READ_VAL(LmnInstrVar, instr, enti);
      vec_push(&links, (LmnWord)enti);
    }

    READ_VAL(LmnInstrVar, instr, memi);
    auto hashset = insertconnectors(rc, (LmnMembraneRef)rc->wt(memi), &links);
    rc->reg(seti) = {(LmnWord)hashset, 0, TT_OTHER};

    vec_destroy(&links);

    /* EFFICIENCY: 解放のための再帰 */
    this->push_stackframe([=](bool result) {
      hashset_free(hashset);
      return result;
    });

    break;
  }
  case INSTR_JUMP: {
    /* EFFICIENCY: 新たに作業配列をmallocしているので非常に遅い
                   -O3 で生成される中間命令にJUMPが含まれないため
                   これでもよい */
    LmnRuleInstr next;
    LmnInstrVar num, i, n;
    LmnJumpOffset offset;

    auto v = LmnRegisterArray(rc->capacity());

    READ_VAL(LmnJumpOffset, instr, offset);
    next = instr + offset;

    i = 0;
    /* atom */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }
    /* mem */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }
    /* vars */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }

    instr = next;

    // use pointer to avoid copy capture.
    auto tmp = new std::vector<LmnRegister>(std::move(rc->work_array));
    rc->warray_set(std::move(v));

    this->push_stackframe([=](bool result) {
      rc->warray_set(std::move(*tmp));
      delete tmp;
      return result;
    });
    break;
  }
  case INSTR_RESETVARS: {
    LmnInstrVar num, i, n, t;

    auto v = LmnRegisterArray(rc->capacity());

    i = 0;
    /* atom */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }

    /* mem */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }

    /* vars */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      v.at(i) = rc->reg(n);
    }

    for (t = 0; t <= i; t++) {
      rc->reg(t) = v.at(t);
    }

    break;
  }
  case INSTR_COMMIT: {
    lmn_interned_str rule_name;

    READ_VAL(lmn_interned_str, instr, rule_name);
    SKIP_VAL(LmnLineNum, instr);

    if (lmn_env.findatom_parallel_mode) {
      lmn_fatal("Couldn't find sync instruction!!");
    }

#ifdef KWBT_OPT
    {
      LmnInstrVar cost;
      READ_VAL(LmnInstrVar, instr, cost);
      rule->cost = cost;
    }
#endif

    rule->name = rule_name;

    profile_apply();

    /*
     * MC mode
     *
     * グローバル変数global_rootに格納されているグローバルルート膜をコピーして
     * そのコピーに対してボディ命令を適用する．
     * その際に変数配列の情報もコピー前のものからコピー後のものへと書き換える．
     *
     * CONTRACT: COMMIT命令に到達したルールはマッチング検査に成功している
     */
    if (RC_GET_MODE(rc, REACT_ND) && !rc->is_zerostep) {
      ProcessID org_next_id = env_next_id();

      if (RC_MC_USE_DMEM(rc)) {
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        /** >>>>>>>> enable delta-membrane <<<<<<< **/
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        struct MemDeltaRoot *d =
            dmem_root_make(RC_GROOT_MEM(rc), rule, env_next_id());
        RC_ND_SET_MEM_DELTA_ROOT(rc, d);

        /* dmem_commit/revertとの整合性を保つため,
         * uniq処理の特殊性を吸収しておく */
        rule->undo_history();

        if (RC_MC_USE_DPOR(rc)) {
          dpor_transition_gen_LHS(RC_POR_DATA(rc), d, rc);
        }

        dmem_interpret(rc, rule, instr);
        dmem_root_finish(d);

        if (RC_MC_USE_DPOR(rc)) {
          if (!dpor_transition_gen_RHS(RC_POR_DATA(rc), d, rc)) {
            dmem_root_free(d);
          } else {
            mc_react_cxt_add_mem_delta(rc, d, rule);
          }

          /* サクセッサへの差分オブジェクトが複数できあがることになるが,
           * 差分オブジェクト間では生成したプロセスのIDに重複があってはならない.
           */
          RC_ND_SET_MEM_DELTA_ROOT(rc, NULL);
          return FALSE;
        }

        mc_react_cxt_add_mem_delta(rc, d, rule);
        RC_ND_SET_MEM_DELTA_ROOT(rc, NULL);
      } else {
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        /** >>>>>>> disable delta-membrane <<<<<<< **/
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        ProcessTableRef copymap;
        LmnMembraneRef tmp_global_root;
        unsigned int i, n;

#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_start_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
        }
#endif

        tmp_global_root = lmn_mem_copy_with_map_ex(RC_GROOT_MEM(rc), &copymap);

        /** 変数配列および属性配列のコピー */
        auto v = LmnRegisterArray(rc->capacity());

        /** copymapの情報を基に変数配列を書換える */
        for (i = 0; i < rc->capacity(); i++) {
          LmnWord t;
          LmnRegisterRef r = &v.at(i);
          r->register_set_at(rc->at(i));
          r->register_set_tt(rc->tt(i));

          if (r->register_tt() == TT_ATOM) {
            if (LMN_ATTR_IS_DATA(r->register_at())) {
              /* data-atom */
              if (r->register_at() == LMN_HL_ATTR) {
                if (proc_tbl_get_by_hlink(
                        copymap,
                        lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(i)),
                        &t)) {
                  r->register_set_wt(
                      (LmnWord)lmn_hyperlink_hl_to_at((HyperLink *)t));
                } else {
                  r->register_set_wt(
                      (LmnWord)rc->wt(i)); /* new_hlink命令等の場合 */
                }
              } else {
                r->register_set_wt((LmnWord)lmn_copy_data_atom(
                    (LmnAtom)rc->wt(i), r->register_at()));
              }
            } else if (proc_tbl_get_by_atom(copymap,
                                            (LmnSymbolAtomRef)rc->wt(i), &t)) {
              /* symbol-atom */
              r->register_set_wt((LmnWord)t);
            } else {
              t = 0;
            }
          } else if (r->register_tt() == TT_MEM) {
            if (rc->wt(i) ==
                (LmnWord)RC_GROOT_MEM(rc)) { /* グローバルルート膜 */
              r->register_set_wt((LmnWord)tmp_global_root);
            } else if (proc_tbl_get_by_mem(copymap, (LmnMembraneRef)rc->wt(i),
                                           &t)) {
              r->register_set_wt((LmnWord)t);
            } else {
              t = 0;
              //              v[i].wt = wt(rc, i); //
              //              allocmem命令の場合はTT_OTHERになっている(2014-05-08
              //              ueda)
            }
          } else { /* TT_OTHER */
            r->register_set_wt(rc->wt(i));
          }
        }
        proc_tbl_free(copymap);

        /** 変数配列および属性配列をコピーと入れ換え, コピー側を書き換える */
        auto tmp = new std::vector<LmnRegister>(std::move(rc->work_array));
        rc->warray_set(std::move(v));

#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_finish_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
        }
#endif

        this->push_stackframe([=](bool result) {
          react_zerostep_recursive(
              rc, tmp_global_root); /**< 0stepルールを適用する */
          mc_react_cxt_add_expanded(rc, tmp_global_root, rule);

          rule->undo_history();

          rc->warray_set(std::move(*tmp));
          if (!rc->keep_process_id_in_nd_mode)
            env_set_next_id(org_next_id);
          delete tmp;
          return false;
        });
        break;
      }

      return FALSE; /* matching backtrack! */
    } else if (RC_GET_MODE(rc, REACT_PROPERTY)) {
      return TRUE; /* propertyはmatchingのみ */
    }

    break;
  }
  case INSTR_FINDATOM: {
    LmnInstrVar atomi, memi;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnLinkAttr, instr, attr);

    if (LMN_ATTR_IS_DATA(attr))
      throw std::runtime_error("cannot find data atoms.");

    if (lmn_env.find_atom_parallel)
      return false;

    LmnFunctor f;
    READ_VAL(LmnFunctor, instr, f);
    auto mem = (LmnMembraneRef)rc->wt(memi);

    if (rc_hlink_opt(atomi, rc)) {
      /* hyperlink の接続関係を利用したルールマッチング最適化 */
      if (!RC_HLINK_SPC(rc))
        lmn_sameproccxt_init(rc);
      auto spc =
          (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc), (HashKeyType)atomi);
      findatom_through_hyperlink(rc, rule, instr, spc, mem, f, atomi);
    } else {
      findatom(rc, rule, instr, mem, f, atomi);
    }

    return false; // false driven loop
  }
  case INSTR_FINDATOM2: {
    LmnInstrVar atomi, memi, findatomid;
    LmnLinkAttr attr;

    if (RC_GET_MODE(rc, REACT_ND) && !rc->is_zerostep) {
      lmn_fatal("This mode:exhaustive search can't use instruction:FindAtom2");
    }

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, findatomid);
    READ_VAL(LmnLinkAttr, instr, attr);
    if (LMN_ATTR_IS_DATA(attr)) {
      lmn_fatal("I can not find data atoms.\n");
    } else { /* symbol atom */
      LmnFunctor f;
      AtomListEntryRef atomlist_ent;
      AtomListEntry::const_iterator start_atom(nullptr, nullptr);

      READ_VAL(LmnFunctor, instr, f);
      atomlist_ent = lmn_mem_get_atomlist((LmnMembraneRef)rc->wt(memi), f);
      if (!atomlist_ent)
        return false;

      auto record = atomlist_ent->find_record(findatomid);
      if (record == atomlist_ent->end()) {
        start_atom = std::begin(*atomlist_ent);
        record =
            atomlist_ent->insert(findatomid, lmn_new_atom(LMN_RESUME_FUNCTOR));
      } else {
        start_atom = std::next(record, 1);
      }

      std::vector<AtomListEntry::const_iterator> its;
      for (auto it = start_atom; it != std::end(*atomlist_ent); ++it)
        its.push_back(it);
      auto filtered = slim::element::make_range_remove_if(
          its.begin(), its.end(),
          [=](const AtomListEntry::const_iterator &atom) {
            return LMN_SATOM_GET_FUNCTOR(*atom) == LMN_RESUME_FUNCTOR;
          });
      std::vector<std::function<LmnRegister(void)>> candidates;
      std::transform(
          filtered.begin(), filtered.end(), std::back_inserter(candidates),
          [=](const AtomListEntry::const_iterator &it) {
            return [=]() {
              atomlist_ent->splice(std::prev(it, 1), *atomlist_ent, record);
              return LmnRegister(
                  {(LmnWord)*it, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
            };
          });

      this->push_stackframe(make_false_driven_enumerator(
          *this, instr, atomi, std::move(candidates)));

      /* 現在のfindatom2の実装にはバグがある（cf.
       * 言語班Wikifindatom2議論）。
       * バグを回避するために、履歴アトムの後ろのアトムすべてからのマッチングに失敗した場合、
       * 履歴アトムの前のアトムに対してマッチングを試みる */
      auto v = std::vector<LmnRegister>();
      std::transform(
          atomlist_ent->begin(), start_atom, std::back_inserter(v),
          [=](LmnSymbolAtomRef atom) {
            return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
          });

      this->push_stackframe(
          make_false_driven_enumerator(*this, instr, atomi, std::move(v)));

      return false;
    }
    break;
  }
  case INSTR_FINDATOMP: { // TODO: refactoring
    if (!lmn_env.enable_parallel || lmn_env.nd) {
      REWRITE_VAL(LmnInstrOp, instr, INSTR_FINDATOM);
      break;
    }
    LmnInstrVar atomi, memi;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnLinkAttr, instr, attr);

    if (LMN_ATTR_IS_DATA(attr)) {
      lmn_fatal("I can not find data atoms.\n");
    } else { /* symbol atom */
      LmnFunctor f;

      READ_VAL(LmnFunctor, instr, f);

      auto atom_arity = LMN_FUNCTOR_ARITY(f);

      if (rc_hlink_opt(atomi, rc)) {
        SameProcCxt *spc;

        if (!RC_HLINK_SPC(rc)) {
          lmn_sameproccxt_init(rc);
        }

        /* 型付きプロセス文脈atomiがoriginal/cloneのどちらであるか判別 */
        spc = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc), (HashKeyType)atomi);
        if (spc->is_clone(atom_arity)) {
          lmn_fatal(
              "Can't use hyperlink searching in parallel-runtime mode.\n");
        }
      }
      auto atomlist_ent = lmn_mem_get_atomlist((LmnMembraneRef)rc->wt(memi), f);
      if (atomlist_ent)
        return false;
      ///
      int ip;
      LmnInstrVar i;
      BOOL judge;
      LmnSymbolAtomRef atom;

      normal_parallel_flag = TRUE;

      while (!deq_is_empty(temp)) {
        ip = (int)deq_pop_head(temp);
        atom = (LmnSymbolAtomRef)thread_info[ip]->rc->wt(atomi);
        if (check_exist(atom, f)) {
          rc->reg(atomi) = {(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM};
          if (rc_hlink_opt(atomi, rc)) {
            auto spc = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc),
                                                  (HashKeyType)atomi);

            if (spc->is_consistent_with((LmnSymbolAtomRef)rc->wt(atomi))) {
              spc->match((LmnSymbolAtomRef)rc->wt(atomi));
              if (interpret(rc, rule, instr)) {
                success_temp_check++;
                return TRUE;
              }
            }
          } else {
            if (interpret(rc, rule, instr)) {
              success_temp_check++;
              return TRUE;
            }
          }
        }
        fail_temp_check++;
      }

      active_thread = std::min((unsigned int)atomlist_ent->n, lmn_env.core_num);

      lmn_env.findatom_parallel_mode = TRUE;
      for (ip = 0, atom = atomlist_head(atomlist_ent); ip < active_thread;
           atom = LMN_SATOM_GET_NEXT_RAW(atom), ip++) {
        // pthread create
        if (lmn_env.find_atom_parallel)
          break;
        if (!check_exist((LmnSymbolAtomRef)thread_info[ip]->next_atom, f) ||
            atom == thread_info[ip]->next_atom ||
            lmn_env.findatom_parallel_inde)
          thread_info[ip]->next_atom = NULL;
        threadinfo_init(ip, atomi, rule, rc, instr, atomlist_ent, atom_arity);
        //
        pthread_mutex_unlock(thread_info[ip]->exec);
      }
      for (auto ip2 = 0; ip2 < ip; ip2++) {
        // lmn_thread_join(findthread[ip2]);
        op_lock(ip2, 0);
        profile_backtrack_add(thread_info[ip2]->backtrack);
        thread_info[ip2]->profile->backtrack_num += thread_info[ip2]->backtrack;
      }
      lmn_env.findatom_parallel_mode = FALSE;

      // copy register
      judge = TRUE;
      for (auto ip2 = 0; ip2 < ip; ip2++) {
        if (thread_info[ip2]->judge && judge) {
          rc->work_array = thread_info[ip2]->rc->work_array;
          if (lmn_env.trace)
            fprintf(stdout, "( Thread id : %d )", thread_info[ip2]->id);
          instr = instr_parallel;
          judge = FALSE;
          continue;
        }
        if (thread_info[ip2]->judge) {
          deq_push_head(temp, ip2);
        }
      }

      if (!lmn_env.find_atom_parallel)
        return FALSE; // Can't find atom
      lmn_env.find_atom_parallel = FALSE;
      break; // Find atom!!
    }
    break;
  }
  case INSTR_SYNC: {
    if (lmn_env.findatom_parallel_mode) {
      lmn_env.find_atom_parallel = TRUE;
      instr_parallel = instr;
      return TRUE;
    }
    break;
  }

  case INSTR_LOCKMEM: {
    LmnInstrVar memi, atomi, memn;
    LmnMembraneRef m;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(lmn_interned_str, instr, memn);

    LMN_ASSERT(!LMN_ATTR_IS_DATA(rc->at(atomi)));
    LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(
        LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)(wt(rc, atomi)))));
    //      LMN_ASSERT(((LmnMembraneRef)wt(rc, memi))->parent);

    m = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)rc->wt(atomi));
    if (LMN_MEM_NAME_ID(m) != memn)
      return FALSE;
    rc->reg(memi) = {(LmnWord)m, 0, TT_MEM};
    break;
  }
  case INSTR_ANYMEM: {
    LmnInstrVar mem1, mem2, memn; /* dst, parent, type, name */

    READ_VAL(LmnInstrVar, instr, mem1);
    READ_VAL(LmnInstrVar, instr, mem2);
    SKIP_VAL(LmnInstrVar, instr);
    READ_VAL(lmn_interned_str, instr, memn);

    rc->at(mem1) = 0;
    rc->tt(mem1) = TT_MEM;
    auto children = slim::vm::membrane_children((LmnMembraneRef)rc->wt(mem2));
    auto filtered = slim::element::make_range_remove_if(
        children.begin(), children.end(),
        [=](LmnMembrane &m) { return LMN_MEM_NAME_ID(&m) != memn; });
    std::vector<LmnRegister> v;
    for (auto &m : filtered)
      v.push_back(LmnRegister({(LmnWord)&m, 0, TT_MEM}));

    this->push_stackframe(
        make_false_driven_enumerator(*this, instr, mem1, std::move(v)));
    return false;
  }
  case INSTR_NMEMS: {
    LmnInstrVar memi, nmems;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, nmems);

    if (!lmn_mem_nmems((LmnMembraneRef)rc->wt(memi), nmems)) {
      return FALSE;
    }

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NMEMS);
      this->push_stackframe([=](bool result) {
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NMEMS);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_NORULES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    if (vec_num(lmn_mem_get_rulesets((LmnMembraneRef)rc->wt(memi))))
      return FALSE;

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NORULES);
      this->push_stackframe([=](bool result) {
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NORULES);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_NEWATOM: {
    LmnInstrVar atomi, memi;
    LmnAtomRef ap;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnLinkAttr, instr, attr);
    if (LMN_ATTR_IS_DATA(attr)) {
      READ_DATA_ATOM(ap, attr);
    } else { /* symbol atom */
      LmnFunctor f;

      READ_VAL(LmnFunctor, instr, f);
      ap = lmn_new_atom(f);

#ifdef USE_FIRSTCLASS_RULE
      if (f == LMN_COLON_MINUS_FUNCTOR) {
        lmn_rc_push_insertion(rc, (LmnSymbolAtomRef)ap,
                              (LmnMembraneRef)wt(rc, memi));
      }
#endif
    }
    lmn_mem_push_atom((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)ap, attr);
    rc->reg(atomi) = {(LmnWord)ap, attr, TT_ATOM};
    break;
  }
  case INSTR_NATOMS: {
    LmnInstrVar memi, natoms;
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, natoms);

    if (!lmn_mem_natoms((LmnMembraneRef)rc->wt(memi), natoms)) {
      return FALSE;
    }

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NATOMS);
      this->push_stackframe([=](bool result) {
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NATOMS);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_NATOMSINDIRECT: {
    LmnInstrVar memi, natomsi;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, natomsi);

    if (!lmn_mem_natoms((LmnMembraneRef)rc->wt(memi), rc->wt(natomsi))) {
      return FALSE;
    }

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NATOMS);
      this->push_stackframe([=](bool result) {
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NATOMS);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_ALLOCLINK: {
    LmnInstrVar link, atom, n;

    READ_VAL(LmnInstrVar, instr, link);
    READ_VAL(LmnInstrVar, instr, atom);
    READ_VAL(LmnInstrVar, instr, n);

    if (LMN_ATTR_IS_DATA(rc->at(atom))) {
      rc->reg(link) = {rc->wt(atom), rc->at(atom), TT_ATOM};
    } else { /* link to atom */
      rc->reg(link) = {(LmnWord)LMN_SATOM(rc->wt(atom)), LMN_ATTR_MAKE_LINK(n),
                       TT_ATOM};
    }
    break;
  }
  case INSTR_UNIFYLINKS: {
    LmnInstrVar link1, link2, mem;
    LmnLinkAttr attr1, attr2;

    READ_VAL(LmnInstrVar, instr, link1);
    READ_VAL(LmnInstrVar, instr, link2);
    READ_VAL(LmnInstrVar, instr, mem);

    attr1 = LINKED_ATTR(link1);
    attr2 = LINKED_ATTR(link2);

    if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {
      if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 1, 2 are data */
        lmn_mem_link_data_atoms((LmnMembraneRef)rc->wt(mem),
                                (LmnAtomRef)rc->wt(link1), rc->at(link1),
                                LINKED_ATOM(link2), attr2);
      } else { /* 1 is data */
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(link2),
                           LMN_ATTR_GET_VALUE(attr2), LINKED_ATOM(link1));
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(link2),
                           LMN_ATTR_GET_VALUE(attr2), attr1);
      }
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 2 is data */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(link1),
                         LMN_ATTR_GET_VALUE(attr1), LINKED_ATOM(link2));
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(link1),
                         LMN_ATTR_GET_VALUE(attr1), attr2);
    } else { /* 1, 2 are symbol atom */

      if (LMN_ATTR_IS_EX(attr1)) {
        if (LMN_ATTR_IS_EX(attr2)) { /* 1, 2 are ex */
          lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),
                              (LmnSymbolAtomRef)LINKED_ATOM(link1), attr1,
                              0, // ex atom ⊂ unary atom
                              (LmnSymbolAtomRef)LINKED_ATOM(link2), attr2, 0);
        } else { /* 1 is ex */
          lmn_newlink_with_ex(
              (LmnMembraneRef)rc->wt(mem), (LmnSymbolAtomRef)LINKED_ATOM(link1),
              attr1, 0, (LmnSymbolAtomRef)LINKED_ATOM(link2), attr2, attr2);
        }
      } else if (LMN_ATTR_IS_EX(attr2)) { /* 2 is ex */
        lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),
                            (LmnSymbolAtomRef)LINKED_ATOM(link1), attr1, attr1,
                            (LmnSymbolAtomRef)LINKED_ATOM(link2), attr2, 0);
      } else {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(link1),
                           LMN_ATTR_GET_VALUE(attr1), LINKED_ATOM(link2));
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(link2),
                           LMN_ATTR_GET_VALUE(attr2), LINKED_ATOM(link1));
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(link1),
                           LMN_ATTR_GET_VALUE(attr1), attr2);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(link2),
                           LMN_ATTR_GET_VALUE(attr2), attr1);
      }
    }
    break;
  }
  case INSTR_NEWLINK: {
    LmnInstrVar atom1, atom2, pos1, pos2, memi;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos2);
    READ_VAL(LmnInstrVar, instr, memi);

    lmn_mem_newlink((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atom1),
                    rc->at(atom1), pos1, (LmnAtomRef)rc->wt(atom2),
                    rc->at(atom2), pos2);
    break;
  }
  case INSTR_RELINK: {
    LmnInstrVar atom1, atom2, pos1, pos2, memi;
    LmnSymbolAtomRef ap;
    LmnByte attr;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos2);
    READ_VAL(LmnInstrVar, instr, memi);

    ap = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atom2),
                                              pos2);
    attr = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2);

    if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1)) &&
        LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
      /* hlink属性ではない通常のデータアトム同士の接続 */
#ifdef DEBUG
      fprintf(stderr, "Two data atoms are connected each other.\n");
#endif
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))) {
      /* hlink属性ではない通常のデータアトムatom1とシンボルアトムatom2の接続.
       */
      LMN_SATOM_SET_LINK(ap, attr, (LmnAtomRef)rc->wt(atom1));
      LMN_SATOM_SET_ATTR(ap, attr, rc->at(atom1));
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
      /* hlink属性ではない通常のデータアトムatom2とシンボルアトムatom1の接続
       */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, ap);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr);
    } else if (!LMN_ATTR_IS_EX(rc->at(atom1)) && !LMN_ATTR_IS_EX(attr)) {
      /* シンボルアトム同士の接続 */
      LMN_SATOM_SET_LINK(ap, attr, (LmnAtomRef)rc->wt(atom1));
      LMN_SATOM_SET_ATTR(ap, attr, pos1);
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, ap);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr);
    } else if (LMN_ATTR_IS_EX(rc->at(atom1))) {
      lmn_newlink_with_ex((LmnMembraneRef)rc->wt(memi),
                          (LmnSymbolAtomRef)rc->wt(atom1), rc->at(atom1), pos1,
                          ap,
                          // 0,
                          attr, /* this arg should be attr because
                                   atom2 may be a hyperlink. */
                          attr);
    } else {
      lmn_newlink_with_ex((LmnMembraneRef)rc->wt(memi),
                          (LmnSymbolAtomRef)rc->wt(atom1), rc->at(atom1), pos1,
                          ap, attr, 0);
    }
    break;
  }
  case INSTR_SWAPLINK: {
    LmnInstrVar atom1, atom2, pos1, pos2;
    LmnSymbolAtomRef ap1, ap2;
    LmnByte attr1, attr2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos2);
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1)) &&
        LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom2))) {
      //(D,D)
#ifdef DEBUG
      fprintf(stderr, "Two data atoms are specified in the arg of the "
                      "swaplink instruction.\n");
#endif
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))) {
      //(D,S)
      ap2 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(
          (LmnSymbolAtomRef)rc->wt(atom2), pos2);
      attr2 = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2);
      LMN_SATOM_SET_LINK(ap2, attr2, (LmnAtomRef)rc->wt(atom1));
      LMN_SATOM_SET_ATTR(ap2, attr2, rc->at(atom1));
      break;
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom2))) {
      //(S,D)
      ap1 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(
          (LmnSymbolAtomRef)rc->wt(atom1), pos1);
      attr1 = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1);
      LMN_SATOM_SET_LINK(ap1, attr1, (LmnAtomRef)rc->wt(atom2));
      LMN_SATOM_SET_ATTR(ap1, attr1, rc->at(atom2));
      break;
    }
    //(S,S)
    ap1 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atom1),
                                               pos1);
    ap2 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atom2),
                                               pos2);
    attr1 = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1);
    attr2 = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2);

    if ((LmnSymbolAtomRef)rc->wt(atom1) == ap2 &&
        (LmnSymbolAtomRef)rc->wt(atom2) == ap1 && attr1 == pos2 &&
        attr2 == pos1) {
      // use same link

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1) &&
               LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) {
      //(-D,-D)

      /* データアトムap2とシンボルアトムatom1 */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, ap2);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr2);

      /* データアトムap1とシンボルアトムatom2 */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, ap1);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2, attr1);

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {
      //(-D,-S)

      /* データアトムap1とシンボルアトムatom2 */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, ap1);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2, attr1);

      /* シンボルアトムatom1とシンボルアトムap2 */
      if (ap2 != NULL) {
        LMN_SATOM_SET_LINK(ap2, attr2, (LmnAtomRef)rc->wt(atom1));
        LMN_SATOM_SET_ATTR(ap2, attr2, pos1);
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, ap2);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr2);
      } else {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, 0);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, 0);
      }

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) {
      //(-S,-D)

      /* データアトムap2とシンボルアトムatom1 */
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, ap2);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr2);

      /* シンボルアトムatom2とシンボルアトムap1 */
      if (ap1 != NULL) {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, ap1);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2,
                           LMN_ATTR_GET_VALUE(attr1));
        LMN_SATOM_SET_LINK(ap1, attr1, (LmnAtomRef)rc->wt(atom2));
        LMN_SATOM_SET_ATTR(ap1, attr1, pos2);
      } else {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, 0);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2, 0);
      }

    } else {
      //(-S,-S)

      /* シンボルアトムatom2とシンボルアトムap1 */
      if (ap1 != NULL) {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, ap1);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2,
                           LMN_ATTR_GET_VALUE(attr1));
        LMN_SATOM_SET_LINK(ap1, attr1, (LmnAtomRef)rc->wt(atom2));
        LMN_SATOM_SET_ATTR(ap1, attr1, pos2);
      } else {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos2, 0);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos2, 0);
      }

      /* シンボルアトムatom1とシンボルアトムap2 */
      if (ap2 != NULL) {
        LMN_SATOM_SET_LINK(ap2, attr2, (LmnAtomRef)rc->wt(atom1));
        LMN_SATOM_SET_ATTR(ap2, attr2, pos1);
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)(LmnSymbolAtomRef)rc->wt(atom1),
                           pos1, ap2);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, attr2);
      } else {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atom1), pos1, 0);
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atom1), pos1, 0);
      }
    }

    break;
  }

    /*     case INSTR_SWAPLINK: */
    /*     { */
    /*       LmnInstrVar atom1, atom2, pos1, pos2; */
    /*       LmnSAtom ap1,ap2; */
    /*       LmnLinkAttr attr1, attr2; */
    /*       READ_VAL(LmnInstrVar, instr, atom1); */
    /*       READ_VAL(LmnInstrVar, instr, pos1); */
    /*       READ_VAL(LmnInstrVar, instr, atom2); */
    /*       READ_VAL(LmnInstrVar, instr, pos2); */
    /*       ap1 = LMN_SATOM(LMN_SATOM_GET_LINK(wt(rc, atom1), pos1)); */
    /*       ap2 = LMN_SATOM(LMN_SATOM_GET_LINK(wt(rc, atom2), pos2)); */
    /*       attr1 = LMN_SATOM_GET_ATTR(wt(rc, atom1), pos1); */
    /*       attr2 = LMN_SATOM_GET_ATTR(wt(rc, atom2), pos2); */
    /*       if ((LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1)) &&
     * LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) */
    /*           || (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom2)) &&
     * LMN_ATTR_IS_DATA_WITHOUT_EX(attr1))) { */
    /*         /\* atom1とap2が共にデータアトム or
     * atom2とap1が共にデータアトム *\/ */
    /* #ifdef DEBUG */
    /*         fprintf(stderr, "Two data atoms are connected each other.\n");
     */
    /* #endif */
    /*       }else if(LMN_SATOM(wt(rc,atom1)) == ap2 &&
     * LMN_SATOM(wt(rc,atom2)) == ap1 && attr1 == pos2 && attr2 ==pos1){ */
    /*       }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom2))){ */
    /*         /\* データアトムatom2とシンボルアトムap1 *\/ */
    /*      if(ap1 != NULL){ */
    /*        LMN_SATOM_SET_LINK(ap1, attr1, wt(rc, atom2)); */
    /*        LMN_SATOM_SET_ATTR(ap1, attr1, pos2); */
    /*      } */
    /*         if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*        if(ap2 != NULL){ */
    /*          LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*          LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*        } */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, attr2); */
    /*         }else { */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*                      LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*                      LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1,
     * ap2); */
    /*                      LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * attr2); */
    /*              }else{ */
    /*                LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*                LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*           /\*LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*           LMN_SATOM_SET_ATTR(ap2, attr2, rc->at(atom1)); */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, attr2);*\/
     */
    /*         } */
    /*       } */
    /*       else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)){ */
    /*         /\* データアトムap1とシンボルアトムatom2 *\/ */
    /*         LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom2)), pos2, ap1); */
    /*         LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom2)), pos2, attr1); */
    /*         if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*           LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*           LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, attr2); */
    /*         }else if (!LMN_ATTR_IS_EX(rc->at(atom1)) &&
     * !LMN_ATTR_IS_EX(attr2)){ */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*                      LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*                      LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1,
     * ap2); */
    /*                      LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * attr2); */
    /*              }else{ */
    /*                LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*                LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*           /\*LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*           LMN_SATOM_SET_ATTR(ap2, attr2, rc->at(atom1)); */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, attr2);*\/
     */
    /*         } */
    /*       } */
    /*       else if (!LMN_ATTR_IS_EX(rc->at(atom1)) && !LMN_ATTR_IS_EX(at(rc,
     * atom2)) */
    /*                && !LMN_ATTR_IS_EX(attr1) && !LMN_ATTR_IS_EX(attr2)){ */
    /*         /\* シンボルアトムatom2とシンボルアトムap1 *\/ */

    /*         if(ap1 != NULL){ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom2)), pos2, ap1); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom2)), pos2,
     * LMN_ATTR_GET_VALUE(attr1)); */
    /*           LMN_SATOM_SET_LINK(ap1, attr1, wt(rc, atom2)); */
    /*           LMN_SATOM_SET_ATTR(ap1, attr1, pos2); */
    /*         }else{ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom2)), pos2, 0); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom2)), pos2, 0); */
    /*         } */
    /*      if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*           LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*           LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * LMN_ATTR_GET_VALUE(attr2)); */
    /*         }else { */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      LMN_SATOM_SET_LINK(ap2, LMN_ATTR_GET_VALUE(attr2),
     * wt(rc, atom1)); */
    /*                      LMN_SATOM_SET_ATTR(ap2, LMN_ATTR_GET_VALUE(attr2),
     * pos1); */
    /*                      LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1,
     * ap2); */
    /*                      LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * LMN_ATTR_GET_VALUE(attr2)); */
    /*              }else{ */
    /*                LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*                LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*      /\* */
    /*         if(ap2){ */
    /*              if(LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*                // データアトムatom1とシンボルアトムap2  */
    /*                LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*                LMN_SATOM_SET_ATTR(ap2, attr2, rc->at(atom1)); */
    /*              }else if(LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*                // データアトムap2とシンボルアトムatom1  */
    /*                LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2);
     */
    /*                LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * attr2); */
    /*              }else{ */
    /*                // シンボルアトムatom1とシンボルアトムap2  */
    /*                LMN_SATOM_SET_LINK(ap2, attr2, wt(rc, atom1)); */
    /*                LMN_SATOM_SET_ATTR(ap2, attr2, pos1); */
    /*                LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, ap2);
     */
    /*                LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,
     * attr2); */
    /*              } */
    /*         }else{ */
    /*           LMN_SATOM_SET_LINK(LMN_SATOM(wt(rc, atom1)), pos1, 0); */
    /*           LMN_SATOM_SET_ATTR(LMN_SATOM(wt(rc, atom1)), pos1,0); */
    /*         }*\/ */
    /*       } */
    /*       } */
    /*       break; */
    /*     } */
  case INSTR_INHERITLINK: {
    LmnInstrVar atomi, posi, linki;
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, posi);
    READ_VAL(LmnInstrVar, instr, linki);
    SKIP_VAL(LmnInstrVar, instr);

    if (LMN_ATTR_IS_DATA(rc->at(atomi)) &&
        LMN_ATTR_IS_DATA(LINKED_ATTR(linki))) {
#ifdef DEBUG
      fprintf(stderr, "Two data atoms are connected each other.\n");
#endif
    } else if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(linki),
                         LINKED_ATTR(linki), (LmnAtomRef)rc->wt(atomi));
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(linki),
                         LINKED_ATTR(linki), rc->at(atomi));
    } else if (LMN_ATTR_IS_DATA(LINKED_ATTR(linki))) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atomi), posi,
                         LINKED_ATOM(linki));
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), posi,
                         LINKED_ATTR(linki));
    } else {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atomi), posi,
                         LINKED_ATOM(linki));
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), posi,
                         LINKED_ATTR(linki));
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LINKED_ATOM(linki),
                         LINKED_ATTR(linki), (LmnAtomRef)rc->wt(atomi));
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)LINKED_ATOM(linki),
                         LINKED_ATTR(linki), posi);
    }

    break;
  }
  case INSTR_GETLINK: {
    LmnInstrVar linki, atomi, posi;
    READ_VAL(LmnInstrVar, instr, linki);
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, posi);

    /* リンク先の取得をせずにリンク元の情報を格納しておく。
     * リンク元が格納されていることを示すため最下位のビットを立てる */

    rc->reg(linki) = {
        (LmnWord)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atomi), posi),
        LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), posi), TT_ATOM};

    break;
  }
  case INSTR_HYPERGETLINK:
    // head部用命令
    // hyperlinkにつながるリンク先だけレジスタに格納かつ以降の命令の実行を行う
    {
      LmnInstrVar linki, atomi, posi;
      READ_VAL(LmnInstrVar, instr, linki);
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, posi);

      LmnAtomRef hlAtom =
          LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atomi), posi);
      LmnLinkAttr attr =
          LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), posi);
      if (attr != LMN_HL_ATTR) {
        return FALSE;
      } else {
        HyperLink *hl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)hlAtom);
        auto v = lmn_hyperlink_get_elements(hl);
        auto regs = std::vector<LmnRegister>();
        std::transform(v.begin(), v.end(), std::back_inserter(regs),
                       [](HyperLink *h) -> LmnRegister {
                         auto child_hlAtom = h->atom;
                         auto linked_atom = LMN_SATOM_GET_LINK(child_hlAtom, 0);
                         return {(LmnWord)linked_atom,
                                 LMN_SATOM_GET_ATTR(child_hlAtom, 0), TT_ATOM};
                       });

        this->push_stackframe(
            make_false_driven_enumerator(*this, instr, linki, std::move(regs)));
        return false;
      }
      break;
    }
  case INSTR_UNIFY: {
    LmnInstrVar atom1, pos1, atom2, pos2, memi;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos2);
    READ_VAL(LmnInstrVar, instr, memi);

    lmn_mem_unify_atom_args((LmnMembraneRef)rc->wt(memi),
                            (LmnSymbolAtomRef)rc->wt(atom1), pos1,
                            (LmnSymbolAtomRef)rc->wt(atom2), pos2);
    break;
  }
  case INSTR_PROCEED:
    return TRUE;
  case INSTR_STOP:
    return FALSE;
  case INSTR_NOT: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    auto next = instr + subinstr_size;
    this->push_stackframe([=](bool result) {
      if (result)
        return false;
      this->instr = next;
      return this->run();
    });
    break;
  }
  case INSTR_ENQUEUEATOM: {
    SKIP_VAL(LmnInstrVar, instr);
    /* do nothing */
    break;
  }
  case INSTR_DEQUEUEATOM: {
    SKIP_VAL(LmnInstrVar, instr);
    break;
  }
  case INSTR_TAILATOM: {
    LmnInstrVar atomi, memi;
    LmnMembraneRef mem = (LmnMembraneRef)rc->wt(memi);
    LmnSymbolAtomRef sa = (LmnSymbolAtomRef)rc->wt(atomi);
    LmnFunctor f = LMN_SATOM_GET_FUNCTOR(sa);
    AtomListEntry *ent = lmn_mem_get_atomlist(mem, f);
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    ent->move_atom_to_atomlist_tail(sa);
    break;
  }

  case INSTR_HEADATOM: {
    LmnInstrVar atomi, memi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    move_atom_to_atomlist_head((LmnSymbolAtomRef)rc->wt(atomi),
                               (LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_TAILATOMLIST: {
    LmnInstrVar atomi, memi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);
    move_atomlist_to_atomlist_tail((LmnSymbolAtomRef)rc->wt(atomi),
                                   (LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_ATOMTAILATOM: {
    LmnInstrVar atomi, atomi2, memi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, atomi2);
    READ_VAL(LmnInstrVar, instr, memi);
    move_atom_to_atom_tail((LmnSymbolAtomRef)rc->wt(atomi),
                           (LmnSymbolAtomRef)rc->wt(atomi2),
                           (LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_CLEARLINK: {
    LmnInstrVar atomi, link;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, link);

    if (!LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atomi))) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)rc->wt(atomi), link, NULL);
    }

    break;
  }
  case INSTR_NEWMEM: {
    LmnInstrVar newmemi, parentmemi;
    LmnMembraneRef mp;

    READ_VAL(LmnInstrVar, instr, newmemi);
    READ_VAL(LmnInstrVar, instr, parentmemi);
    SKIP_VAL(LmnInstrVar, instr);

    mp = lmn_mem_make(); /*lmn_new_mem(memf);*/
    lmn_mem_add_child_mem((LmnMembraneRef)rc->wt(parentmemi), mp);
    rc->wt(newmemi) = (LmnWord)mp;
    rc->tt(newmemi) = TT_MEM;
    lmn_mem_set_active(mp, TRUE);
    if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
      lmn_memstack_push(RC_MEMSTACK((MemReactContext *)rc), mp);
    }
    break;
  }
  case INSTR_ALLOCMEM: {
    LmnInstrVar dstmemi;
    READ_VAL(LmnInstrVar, instr, dstmemi);
    rc->wt(dstmemi) = (LmnWord)lmn_mem_make();
    rc->tt(dstmemi) = TT_OTHER; /* 2014-05-08, ueda */
    break;
  }
  case INSTR_REMOVEATOM: {
    LmnInstrVar atomi, memi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, memi);

#ifdef USE_FIRSTCLASS_RULE
    LmnSymbolAtomRef atom = (LmnSymbolAtomRef)rc->wt(atomi);
    LmnLinkAttr attr = rc->at(atomi);
    if (LMN_HAS_FUNCTOR(atom, attr, LMN_COLON_MINUS_FUNCTOR)) {
      LmnMembraneRef mem = (LmnMembraneRef)rc->wt(memi);
      lmn_mem_remove_firstclass_ruleset(mem, firstclass_ruleset_lookup(atom));
      firstclass_ruleset_release(atom);
    }
#endif

    lmn_mem_remove_atom((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atomi),
                        rc->at(atomi));

    break;
  }
  case INSTR_FREEATOM: {
    LmnInstrVar atomi;

    READ_VAL(LmnInstrVar, instr, atomi);
    lmn_free_atom((LmnAtomRef)rc->wt(atomi), rc->at(atomi));
    break;
  }
  case INSTR_REMOVEMEM: {
    LmnInstrVar memi, parenti;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, parenti);

    lmn_mem_remove_mem((LmnMembraneRef)rc->wt(parenti),
                       (LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_FREEMEM: {
    LmnInstrVar memi;
    LmnMembraneRef mp;

    READ_VAL(LmnInstrVar, instr, memi);

    mp = (LmnMembraneRef)rc->wt(memi);
    lmn_mem_free(mp);
    if (rc->is_zerostep) {
      lmn_memstack_delete(RC_MEMSTACK((MemReactContext *)rc), mp);
    }
    break;
  }
  case INSTR_ADDMEM: {
    LmnInstrVar dstmem, srcmem;

    READ_VAL(LmnInstrVar, instr, dstmem);
    READ_VAL(LmnInstrVar, instr, srcmem);

    //      LMN_ASSERT(!((LmnMembraneRef)rc->wt( srcmem))->parent);

    lmn_mem_add_child_mem((LmnMembraneRef)rc->wt(dstmem),
                          (LmnMembraneRef)rc->wt(srcmem));
    break;
  }
  case INSTR_ENQUEUEMEM: {
    LmnInstrVar memi;
    READ_VAL(LmnInstrVar, instr, memi);

    if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
      lmn_memstack_push(RC_MEMSTACK((MemReactContext *)rc),
                        (LmnMembraneRef)rc->wt(memi));
    }
    break;
  }
  case INSTR_UNLOCKMEM: { /* do nothing */
    SKIP_VAL(LmnInstrVar, instr);
    break;
  }
  case INSTR_LOADRULESET: {
    LmnInstrVar memi;
    LmnRulesetId id;
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnRulesetId, instr, id);

    lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(memi), LmnRuleSetTable::at(id));
    break;
  }
  case INSTR_LOADMODULE: {
    LmnInstrVar memi;
    lmn_interned_str module_name_id;
    LmnRuleSetRef ruleset;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(lmn_interned_str, instr, module_name_id);

    if ((ruleset = lmn_get_module_ruleset(module_name_id))) {
      /* テーブル内にルールセットがある場合 */
      lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(memi), ruleset);
    } else {
      /* テーブル内にルールセットがない場合 */
      fprintf(stderr, "Undefined module %s\n", lmn_id_to_name(module_name_id));
    }
    break;
  }
  case INSTR_RECURSIVELOCK: {
    SKIP_VAL(LmnInstrVar, instr);
    /* do notiong */
    break;
  }
  case INSTR_RECURSIVEUNLOCK: {
    SKIP_VAL(LmnInstrVar, instr);
    /* do notiong */
    break;
  }
  case INSTR_DEREFATOM: {
    LmnInstrVar atom1, atom2, posi;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, posi);

    rc->reg(atom1) = {(LmnWord)LMN_SATOM(LMN_SATOM_GET_LINK(
                          (LmnSymbolAtomRef)rc->wt(atom2), posi)),
                      LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), posi),
                      TT_ATOM};
    break;
  }
  case INSTR_DEREF: {
    LmnInstrVar atom1, atom2, pos1, pos2;
    LmnByte attr;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, pos2);

    attr = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atom2), pos1);
    LMN_ASSERT(!LMN_ATTR_IS_DATA(rc->at(atom2)));
    if (LMN_ATTR_IS_DATA(attr)) {
      if (pos2 != 0)
        return FALSE;
    } else {
      if (attr != pos2)
        return FALSE;
    }
    rc->reg(atom1) = {
        (LmnWord)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atom2), pos1),
        attr, TT_ATOM};
    break;
  }
  case INSTR_FUNC: {
    LmnInstrVar atomi;
    LmnFunctor f;
    LmnLinkAttr attr;
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnLinkAttr, instr, attr);

    if (LMN_ATTR_IS_DATA(rc->at(atomi)) == LMN_ATTR_IS_DATA(attr)) {
      if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
        BOOL eq;
        if (rc->at(atomi) != attr)
          return FALSE; /* comp attr */
        LmnByte type;
        READ_CMP_DATA_ATOM(attr, rc->wt(atomi), eq, type);
        rc->tt(atomi) = type;
        if (!eq)
          return FALSE;
      } else { /* symbol atom */
        READ_VAL(LmnFunctor, instr, f);
        if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(atomi)) != f) {
          return FALSE;
        }
        if (rc_hlink_opt(atomi, rc)) {
          auto spc = ((SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc),
                                                 (HashKeyType)atomi));
          if (!spc->is_consistent_with((LmnSymbolAtomRef)rc->wt(atomi)))
            return FALSE;
        }
      }
    } else { /* LMN_ATTR_IS_DATA(rc->at(atomi)) != LMN_ATTR_IS_DATA(attr) */
      return FALSE;
    }
    break;
  }
  case INSTR_NOTFUNC: {
    LmnInstrVar atomi;
    LmnFunctor f;
    LmnLinkAttr attr;
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnLinkAttr, instr, attr);

    if (LMN_ATTR_IS_DATA(rc->at(atomi)) == LMN_ATTR_IS_DATA(attr)) {
      if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
        if (rc->at(atomi) == attr) {
          BOOL eq;
          LmnByte type;
          READ_CMP_DATA_ATOM(attr, rc->wt(atomi), eq, type);
          rc->tt(atomi) = type;
          if (eq)
            return FALSE;
        } else {
          goto label_skip_data_atom;
        }
      } else { /* symbol atom */
        READ_VAL(LmnFunctor, instr, f);
        if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(atomi)) == f)
          return FALSE;
      }
    } else if (LMN_ATTR_IS_DATA(attr)) {
      goto label_skip_data_atom;
    }
    break;
  label_skip_data_atom:
    SKIP_DATA_ATOM(attr);
    break;
  }
  case INSTR_ISGROUND:
  case INSTR_ISHLGROUND:
  case INSTR_ISHLGROUNDINDIRECT: {
    LmnInstrVar funci, srclisti, avolisti;
    Vector *srcvec, *avovec;
    unsigned long natoms;
    BOOL b;

    READ_VAL(LmnInstrVar, instr, funci);
    READ_VAL(LmnInstrVar, instr, srclisti);
    READ_VAL(LmnInstrVar, instr, avolisti);

    /* リンクオブジェクトのベクタを構築 */
    srcvec = links_from_idxs((Vector *)rc->wt(srclisti), rc);
    avovec = links_from_idxs((Vector *)rc->wt(avolisti), rc);

    std::unique_ptr<ProcessTbl> atoms = nullptr;
    std::unique_ptr<ProcessTbl> hlinks = nullptr;

    switch (op) {
    case INSTR_ISHLGROUND:
    case INSTR_ISHLGROUNDINDIRECT: {
      std::vector<LmnFunctor> attr_functors(16);
      std::vector<LmnWord> attr_dataAtoms(16);
      std::vector<LmnLinkAttr> attr_dataAtom_attrs(16);

      auto args = (op == INSTR_ISHLGROUNDINDIRECT)
                      ? read_unary_atoms_indirect(rc, instr)
                      : read_unary_atoms(rc, instr);

      for (auto &v : args) {
        if (c17::holds_alternative<LmnFunctor>(v)) {
          attr_functors.push_back(c17::get<LmnFunctor>(v));
        } else {
          auto &p = c17::get<std::pair<LmnLinkAttr, LmnDataAtomRef>>(v);
          attr_dataAtom_attrs.push_back(p.first);
          attr_dataAtoms.push_back(p.second);
        }
      }
      std::sort(std::begin(attr_functors), std::end(attr_functors));

      b = ground_atoms(srcvec, avovec, atoms, &natoms, hlinks, attr_functors,
                       attr_dataAtoms, attr_dataAtom_attrs);
      break;
    }
    case INSTR_ISGROUND: {
      b = ground_atoms(srcvec, avovec, atoms, &natoms);
      break;
    }
    }
    free_links(srcvec);
    free_links(avovec);

    if (!b)
      return false;

    rc->reg(funci) = {natoms, LMN_INT_ATTR, TT_OTHER};

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      auto addr = atoms.get();
      atoms.release();
      dpor_LHS_add_ground_atoms(RC_POR_DATA(rc), addr);

      this->push_stackframe([=](bool result) {
        dpor_LHS_remove_ground_atoms(RC_POR_DATA(rc), addr);
        proc_tbl_free(addr);
        return false;
      });
    }

    break;
  }
  case INSTR_UNIQ: {
    /*
     * uniq 命令は、
     * "全ての失敗しうるガード命令列の最後尾" かつ
     * "シンボルアトムを生成するガード命令列より前" に
     * 挿入されるように、コンパイラで配置変更を行なっている
     */

    LmnInstrVar llist, n;
    LmnPortRef port;
    lmn_interned_str id;
    unsigned int i;
    BOOL sh;
    LmnLinkAttr attr;

    port = (LmnPortRef)lmn_make_output_string_port();
    READ_VAL(LmnInstrVar, instr, llist);

    if (lmn_env.show_hyperlink) {
      sh = TRUE;
      /* MT-UNSAFE!!
       *  --show_hlオプションの有無でlmn_dump_atomから取得できる
       *  バイト列が変わってしまうため、とりあえずの回避策
       *
       *  TODO:
       *    実行時オプション用のフラグデータの書換えは,
       *    フラグをReactCxtオブジェクトに記録させることで,
       *    スレッドセーフにできる */
      lmn_env.show_hyperlink = FALSE;
    } else {
      sh = FALSE;
    }

    for (i = 0; i < (int)llist; i++) {
      Vector *srcvec;

      READ_VAL(LmnInstrVar, instr, n);
      srcvec = (Vector *)rc->wt(n);
      attr = (LmnLinkAttr)rc->at(vec_get(srcvec, 0));

      /** 識別子の生成 **/
      /* 引数に直接データアトムが接続されている場合 */
      if (LMN_ATTR_IS_DATA(attr)) {
        switch (attr) {
        case LMN_INT_ATTR: {
          char *s = int_to_str(rc->wt(vec_get(srcvec, 0)));
          port_put_raw_s(port, s);
          LMN_FREE(s);
          break;
        }
        case LMN_DBL_ATTR: {
          char buf[64];
          sprintf(buf, "%f", lmn_get_double(rc->wt(vec_get(srcvec, 0))));
          port_put_raw_s(port, buf);
          break;
        }
        case LMN_HL_ATTR: {
          char buf[16];
          port_put_raw_s(port, EXCLAMATION_NAME);
          sprintf(buf, "%lx",
                  LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(
                      (LmnSymbolAtomRef)rc->wt(vec_get(srcvec, 0)))));
          port_put_raw_s(port, buf);
          break;
        }
        default: /* int, double, hlink 以外はとりあえず今まで通り */
          lmn_dump_atom(port, (LmnAtomRef)rc->wt(vec_get(srcvec, 0)),
                        (LmnLinkAttr)rc->at(vec_get(srcvec, 0)));
        }
      } else { /* symbol atom */
        lmn_dump_atom(port, (LmnAtomRef)rc->wt(vec_get(srcvec, 0)),
                      (LmnLinkAttr)rc->at(vec_get(srcvec, 0)));
      }
      port_put_raw_s(port, ":");
    }

    id = lmn_intern((char *)lmn_string_c_str((LmnStringRef)port->data));
    lmn_port_free(port);

    if (sh)
      lmn_env.show_hyperlink = TRUE;

    /* 履歴表と照合 */
    if (rule->has_history(id))
      return FALSE;

    /* 履歴に挿入 */
    rule->add_history(id);

    break;
  }
  case INSTR_NEWHLINKWITHATTR:
  case INSTR_NEWHLINKWITHATTRINDIRECT:
  case INSTR_NEWHLINK: {
    /* 全ての失敗しうるガード制約よりも後で実行されるように、
     * コンパイラで配置変更を行なっている
     */

    LmnInstrVar atomi;
    READ_VAL(LmnInstrVar, instr, atomi);

    switch (op) {
    case INSTR_NEWHLINKWITHATTR: {
      LmnAtomRef ap;
      LmnLinkAttr attr;
      READ_VAL(LmnLinkAttr, instr, attr);
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_DATA_ATOM(ap, attr);
      } else {
        LmnFunctor f;
        READ_VAL(LmnFunctor, instr, f);
        ap = lmn_new_atom(f);
        attr = 0; //シンボルアトムということを表す。値に意味はない。
        if (LMN_SATOM_GET_ARITY((LmnSymbolAtomRef)ap) > 1) {
          lmn_fatal("hyperlink's attribute takes only an unary atom");
        }
      }

      rc->reg(atomi) = {
          (LmnWord)lmn_hyperlink_new_with_attr((LmnSymbolAtomRef)ap, attr),
          LMN_HL_ATTR, TT_ATOM};
      break;
    }
    case INSTR_NEWHLINKWITHATTRINDIRECT: {
      LmnAtomRef ap;
      LmnLinkAttr attr;
      LmnInstrVar atomi2; //変数名どうにかしたい。
      READ_VAL(LmnInstrVar, instr, atomi2);
      ap = lmn_copy_atom((LmnAtomRef)rc->wt(atomi2), rc->at(atomi2));
      attr = rc->at(atomi2);
      if (!LMN_ATTR_IS_DATA(rc->at(atomi2)) &&
          LMN_SATOM_GET_ARITY((LmnSymbolAtomRef)ap) > 1) {
        lmn_fatal("hyperlink's attribute takes only an unary atom");
      }
      rc->reg(atomi) = {(LmnWord)lmn_hyperlink_new_with_attr(ap, attr),
                        LMN_HL_ATTR, TT_ATOM};
      break;
    }
    case INSTR_NEWHLINK:
      rc->reg(atomi) = {(LmnWord)lmn_hyperlink_new(), LMN_HL_ATTR, TT_ATOM};
      break;
    }
    break;
  }
  case INSTR_MAKEHLINK: {
    /* // 未実装
     *
     * i(N) :- make(N, $x), N1 = N-1 | i(N1), hoge($x).
     * のようにして、int(N)の値をIDとするhyperlinkを生成できるような機能があると
     * 性能測定にとても便利かも
     * (構文(記法？)はこれじゃないとしても、hyperlinkへの値の束縛に若干関係しそうなにほひ)
     */
    break;
  }
  case INSTR_ISHLINK: {
    LmnInstrVar atomi;
    READ_VAL(LmnInstrVar, instr, atomi);

    if (!LMN_ATTR_IS_HL(rc->at(atomi)))
      return FALSE;

    break;
  }
  case INSTR_GETATTRATOM: {
    LmnInstrVar dstatomi, atomi;
    READ_VAL(LmnInstrVar, instr, dstatomi);
    READ_VAL(LmnInstrVar, instr, atomi);

    rc->reg(dstatomi) = {(LmnWord)LMN_HL_ATTRATOM(lmn_hyperlink_at_to_hl(
                             (LmnSymbolAtomRef)rc->wt(atomi))),
                         LMN_HL_ATTRATOM_ATTR(lmn_hyperlink_at_to_hl(
                             (LmnSymbolAtomRef)rc->wt(atomi))),
                         TT_OTHER};
    break;
  }
  case INSTR_GETNUM: {
    LmnInstrVar dstatomi, atomi;

    /* ISHLINKチェック済み */
    READ_VAL(LmnInstrVar, instr, dstatomi);
    READ_VAL(LmnInstrVar, instr, atomi);

    rc->reg(dstatomi) = {
        (LmnWord)lmn_hyperlink_element_num(
            lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(atomi))),
        LMN_INT_ATTR, TT_OTHER};
    break;
  }
  case INSTR_UNIFYHLINKS: {
    LmnSymbolAtomRef atom;
    LmnInstrVar memi, atomi;
    LmnLinkAttr attr1, attr2;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);

    atom = (LmnSymbolAtomRef)rc->wt(atomi);

    attr1 = LMN_SATOM_GET_ATTR(atom, 0);
    attr2 = LMN_SATOM_GET_ATTR(atom, 1);

    /* >< の両辺のアトムがハイパーリンクであれば併合 */
    if (LMN_ATTR_IS_HL(attr1) && LMN_ATTR_IS_HL(attr2)) {
      LmnMembraneRef m;
      LmnSymbolAtomRef atom1, atom2;
      HyperLink *hl1, *hl2;

      m = (LmnMembraneRef)rc->wt(memi);
      atom1 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, 0);
      atom2 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, 1);

      hl1 = lmn_hyperlink_at_to_hl(atom1);
      hl2 = lmn_hyperlink_at_to_hl(atom2);

      if (LMN_SATOM_GET_ARITY(atom) ==
          2) { //二引数の場合は一つ目のハイパーリンクの属性を継承する
        lmn_hyperlink_unify(hl1, hl2, LMN_HL_ATTRATOM(hl1),
                            LMN_HL_ATTRATOM_ATTR(hl1));
      } else if (LMN_SATOM_GET_ARITY(atom) ==
                 3) { //三引数の場合は三引数目を併合後の属性とする
        LmnAtom attrAtom;
        attrAtom = LMN_ATOM(LMN_SATOM_GET_LINK(atom, 2));
        lmn_hyperlink_unify(hl1, hl2, (LmnAtomRef)attrAtom,
                            LMN_SATOM_GET_ATTR(atom, 2));
      } else {
        lmn_fatal("too many arguments to >< atom");
      }

      lmn_mem_delete_atom(m, (LmnAtomRef)rc->wt(atomi), rc->at(atomi));
      lmn_mem_delete_atom(m, atom1, (LmnWord)attr1);
      lmn_mem_delete_atom(m, atom2, (LmnWord)attr2);
    }
    break;
  }
  case INSTR_FINDPROCCXT: {
    /**
     * 同名の型付きプロセス文脈名を持つルールを最適化モードで実行するための命令
     * hyperlink専用(2010/10/10時点)
     *
     * Java版コンパイラ側で--hl-optを付けてコンパイルすることで、findatomの前に挿入される
     * SLIM側で--hlオプション指定で実行可能（オプション無しで実行されるとwarning）
     *
     * cf. 同名の型付きプロセス文脈名の分離
     *     a($x), b($x) :- ...
     *   → a($x), a($x0) :- hlink($x), $x = $x0 | ...
     *   同じ名前の型付きプロセス文脈名を記述すると、片方の名前をユニークなものに変更し、
     *   ガードで型チェック、構造比較を行うルールに自動的に変換してくれる
     *   便宜的に、元の名前（ここでの$x）をオリジナル、新たに生成された名前（ここでの$x0）をクローンと呼んでいる
     */
    LmnInstrVar atom1, length1, arg1, atom2, length2, arg2;
    SameProcCxt *spc1, *spc2;
    int i;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, length1);
    READ_VAL(LmnInstrVar, instr, arg1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, length2);
    READ_VAL(LmnInstrVar, instr, arg2);

    if (!RC_HLINK_SPC(rc)) {
      lmn_sameproccxt_init(rc);
    }

    if (!hashtbl_contains(RC_HLINK_SPC(rc), (HashKeyType)atom1)) {
      spc1 = new SameProcCxt(length1);
      hashtbl_put(RC_HLINK_SPC(rc), (HashKeyType)atom1, (HashValueType)spc1);
    } else {
      spc1 = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc), (HashKeyType)atom1);
    }

    if (!hashtbl_contains(RC_HLINK_SPC(rc), (HashKeyType)atom2)) {
      spc2 = new SameProcCxt(length2);
      hashtbl_put(RC_HLINK_SPC(rc), (HashKeyType)atom2, (HashValueType)spc2);
    } else {
      spc2 = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(rc), (HashKeyType)atom2);
    }

    spc1->add_proccxt_if_absent(atom1, arg1);
    spc2->add_proccxt_if_absent(atom2, arg2, *spc1, arg1);

    ////normal parallel init
    if (lmn_env.enable_parallel && !lmn_env.nd) {
      for (i = 0; i < lmn_env.core_num; i++) {
        if (!RC_HLINK_SPC(thread_info[i]->rc)) {
          lmn_sameproccxt_init(thread_info[i]->rc);
        }

        if (!hashtbl_contains(RC_HLINK_SPC(thread_info[i]->rc),
                              (HashKeyType)atom1)) {
          spc1 = new SameProcCxt(length1);
          hashtbl_put(RC_HLINK_SPC(thread_info[i]->rc), (HashKeyType)atom1,
                      (HashValueType)spc1);
        } else {
          spc1 = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(thread_info[i]->rc),
                                            (HashKeyType)atom1);
        }

        if (!hashtbl_contains(RC_HLINK_SPC(thread_info[i]->rc),
                              (HashKeyType)atom2)) {
          spc2 = new SameProcCxt(length2);
          hashtbl_put(RC_HLINK_SPC(thread_info[i]->rc), (HashKeyType)atom2,
                      (HashValueType)spc2);
        } else {
          spc2 = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(thread_info[i]->rc),
                                            (HashKeyType)atom2);
        }

        spc1->add_proccxt_if_absent(atom1, arg1);
        spc2->add_proccxt_if_absent(atom2, arg2, *spc1, arg1);
      }
    }
    break;
  }
  case INSTR_EQGROUND:
  case INSTR_NEQGROUND: {
    LmnInstrVar srci, dsti;
    Vector *srcvec, *dstvec;
    BOOL ret_flag;

    READ_VAL(LmnInstrVar, instr, srci);
    READ_VAL(LmnInstrVar, instr, dsti);

    srcvec = links_from_idxs((Vector *)rc->wt(srci), rc);
    dstvec = links_from_idxs((Vector *)rc->wt(dsti), rc);

    ret_flag = lmn_mem_cmp_ground(srcvec, dstvec);

    free_links(srcvec);
    free_links(dstvec);

    if ((!ret_flag && INSTR_EQGROUND == op) ||
        (ret_flag && INSTR_NEQGROUND == op)) {
      return FALSE;
    }
    break;
  }
  case INSTR_COPYHLGROUND:
  case INSTR_COPYHLGROUNDINDIRECT:
  case INSTR_COPYGROUND: {
    LmnInstrVar dstlist, srclist, memi;
    Vector *srcvec, *dstlovec, *retvec; /* 変数番号のリスト */
    ProcessTableRef atommap;
    ProcessTableRef hlinkmap;

    READ_VAL(LmnInstrVar, instr, dstlist);
    READ_VAL(LmnInstrVar, instr, srclist);
    READ_VAL(LmnInstrVar, instr, memi);

    /* リンクオブジェクトのベクタを構築 */
    srcvec = links_from_idxs((Vector *)rc->wt(srclist), rc);

    switch (op) {
    case INSTR_COPYHLGROUND:
    case INSTR_COPYHLGROUNDINDIRECT: {
      ProcessTableRef attr_functors;
      Vector attr_dataAtoms;
      Vector attr_dataAtom_attrs;
      vec_init(&attr_dataAtoms, 16);
      vec_init(&attr_dataAtom_attrs, 16);
      attr_functors = proc_tbl_make_with_size(16);
      LmnInstrVar i = 0, n;

      READ_VAL(LmnInstrVar, instr, n);

      switch (op) {
      case INSTR_COPYHLGROUNDINDIRECT: {
        LmnInstrVar ai;
        for (; n--; i++) {
          READ_VAL(LmnInstrVar, instr, ai);
          if (LMN_ATTR_IS_DATA(rc->at(ai))) {
            vec_push(&attr_dataAtom_attrs, rc->at(ai));
            vec_push(&attr_dataAtoms, rc->wt(ai));
          } else {
            LmnFunctor f;
            f = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(ai));
            proc_tbl_put(attr_functors, f, f);
          }
        }
        break;
      }
      case INSTR_COPYHLGROUND: {
        for (; n--; i++) {
          LmnLinkAttr attr;
          READ_VAL(LmnLinkAttr, instr, attr);
          if (LMN_ATTR_IS_DATA(attr)) {
            LmnAtomRef at;
            vec_push(&attr_dataAtom_attrs, attr);
            READ_DATA_ATOM(at, attr);
            vec_push(&attr_dataAtoms, (LmnWord)at);
          } else {
            LmnFunctor f;
            READ_VAL(LmnFunctor, instr, f);
            proc_tbl_put(attr_functors, f, f);
          }
        }
        break;
      }
      }
      lmn_mem_copy_hlground((LmnMembraneRef)rc->wt(memi), srcvec, &dstlovec,
                            &atommap, &hlinkmap, &attr_functors,
                            &attr_dataAtoms, &attr_dataAtom_attrs);

      break;
    }
    case INSTR_COPYGROUND:
      lmn_mem_copy_ground((LmnMembraneRef)rc->wt(memi), srcvec, &dstlovec,
                          &atommap);
      break;
    }
    free_links(srcvec);

    /* 返り値の作成 */
    retvec = vec_make(2);
    vec_push(retvec, (LmnWord)dstlovec);
    vec_push(retvec, (LmnWord)atommap);
    rc->reg(dstlist) = {(LmnWord)retvec, LIST_AND_MAP, TT_OTHER};

    this->push_stackframe([=](bool result) {
      free_links(dstlovec);
      vec_free(retvec);
      LMN_ASSERT(result);
      return result;
    });

    break;
  }
  case INSTR_REMOVEHLGROUND:
  case INSTR_REMOVEHLGROUNDINDIRECT:
  case INSTR_FREEHLGROUND:
  case INSTR_FREEHLGROUNDINDIRECT:
  case INSTR_REMOVEGROUND:
  case INSTR_FREEGROUND: {
    LmnInstrVar listi, memi;
    Vector *srcvec; /* 変数番号のリスト */

    READ_VAL(LmnInstrVar, instr, listi);
    if (INSTR_REMOVEGROUND == op || INSTR_REMOVEHLGROUND == op ||
        INSTR_REMOVEHLGROUNDINDIRECT == op) {
      READ_VAL(LmnInstrVar, instr, memi);
    } else {
      memi = 0;
    }
    srcvec = links_from_idxs((Vector *)rc->wt(listi), rc);

    switch (op) {
    case INSTR_REMOVEHLGROUND:
    case INSTR_REMOVEHLGROUNDINDIRECT:
    case INSTR_FREEHLGROUND:
    case INSTR_FREEHLGROUNDINDIRECT: {
      ProcessTableRef attr_functors;
      Vector attr_dataAtoms;
      Vector attr_dataAtom_attrs;
      vec_init(&attr_dataAtoms, 16);
      vec_init(&attr_dataAtom_attrs, 16);
      attr_functors = proc_tbl_make_with_size(16);
      LmnInstrVar i = 0, n;

      READ_VAL(LmnInstrVar, instr, n);

      switch (op) {
      case INSTR_REMOVEHLGROUNDINDIRECT:
      case INSTR_FREEHLGROUNDINDIRECT: {
        LmnInstrVar ai;
        for (; n--; i++) {
          READ_VAL(LmnInstrVar, instr, ai);
          if (LMN_ATTR_IS_DATA(rc->at(ai))) {
            vec_push(&attr_dataAtom_attrs, rc->at(ai));
            vec_push(&attr_dataAtoms, rc->wt(ai));
          } else {
            LmnFunctor f;
            f = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(ai));
            proc_tbl_put(attr_functors, f, f);
          }
        }
        break;
      }
      case INSTR_REMOVEHLGROUND:
      case INSTR_FREEHLGROUND: {
        for (; n--; i++) {
          LmnLinkAttr attr;
          READ_VAL(LmnLinkAttr, instr, attr);
          if (LMN_ATTR_IS_DATA(attr)) {
            LmnAtomRef at;
            vec_push(&attr_dataAtom_attrs, attr);
            READ_DATA_ATOM(at, attr);
            vec_push(&attr_dataAtoms, (LmnWord)at);
          } else {
            LmnFunctor f;
            READ_VAL(LmnFunctor, instr, f);
            proc_tbl_put(attr_functors, f, f);
          }
        }
        break;
      }
      }
      switch (op) {
      case INSTR_REMOVEHLGROUND:
      case INSTR_REMOVEHLGROUNDINDIRECT:
        lmn_mem_remove_hlground((LmnMembraneRef)rc->wt(memi), srcvec,
                                &attr_functors, &attr_dataAtoms,
                                &attr_dataAtom_attrs);
        break;
      case INSTR_FREEHLGROUND:
      case INSTR_FREEHLGROUNDINDIRECT:
        lmn_mem_free_hlground(
            srcvec, // this may also cause a bug, see 15 lines below
            &attr_functors, &attr_dataAtoms, &attr_dataAtom_attrs);
        break;
      }
      proc_tbl_free(attr_functors);
      vec_destroy(&attr_dataAtoms);
      vec_destroy(&attr_dataAtom_attrs);
      break;
    }
    case INSTR_REMOVEGROUND:
      lmn_mem_remove_ground((LmnMembraneRef)rc->wt(memi), srcvec);
      break;
    case INSTR_FREEGROUND:
      lmn_mem_free_ground(srcvec);
      break;
    }

    free_links(srcvec);

    break;
  }
  case INSTR_ISUNARY: {
    LmnInstrVar atomi;
    READ_VAL(LmnInstrVar, instr, atomi);

    if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
      switch (rc->at(atomi)) {
      case LMN_SP_ATOM_ATTR:
        /* スペシャルアトムはgroundの結果をunaryの結果とする */
        if (!SP_ATOM_IS_GROUND(rc->wt(atomi))) {
          return FALSE;
        }
        break;
      default:
        break;
      }
    } else if (LMN_SATOM_GET_ARITY((LmnSymbolAtomRef)rc->wt(atomi)) != 1)
      return FALSE;
    break;
  }
  case INSTR_ISINT: {
    LmnInstrVar atomi;
    READ_VAL(LmnInstrVar, instr, atomi);

    if (rc->at(atomi) != LMN_INT_ATTR)
      return FALSE;
    break;
  }
  case INSTR_ISFLOAT: {
    LmnInstrVar atomi;
    READ_VAL(LmnInstrVar, instr, atomi);

    if (rc->at(atomi) != LMN_DBL_ATTR)
      return FALSE;
    break;
  }
  case INSTR_ISSTRING: {
    LmnInstrVar atomi;

    READ_VAL(LmnInstrVar, instr, atomi);

    if (!lmn_is_string((LmnAtomRef)rc->wt(atomi), rc->at(atomi)))
      return FALSE;
    break;
  }
  case INSTR_ISINTFUNC: {
    LmnInstrVar funci;
    READ_VAL(LmnInstrVar, instr, funci);

    if (rc->at(funci) != LMN_INT_ATTR)
      return FALSE;
    break;
  }
  case INSTR_ISFLOATFUNC: {
    LmnInstrVar funci;
    READ_VAL(LmnInstrVar, instr, funci);

    if (rc->at(funci) != LMN_DBL_ATTR)
      return FALSE;
    break;
  }
  case INSTR_COPYATOM: {
    LmnInstrVar atom1, memi, atom2;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(atom1) = {
        (LmnWord)lmn_copy_atom((LmnAtomRef)rc->wt(atom2), rc->at(atom2)),
        rc->at(atom2), TT_OTHER};
    lmn_mem_push_atom((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atom1),
                      rc->at(atom1));
    break;
  }
  case INSTR_EQATOM: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    /* データアトムは１引数なので,この命令が出る状況では
       では常にFALSEのはず */
    if (LMN_ATTR_IS_DATA(rc->at(atom1)) || LMN_ATTR_IS_DATA(rc->at(atom2)) ||
        LMN_SATOM(rc->wt(atom1)) != LMN_SATOM(rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_NEQATOM: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(LMN_ATTR_IS_DATA(rc->at(atom1)) || LMN_ATTR_IS_DATA(rc->at(atom2)) ||
          LMN_SATOM(rc->wt(atom1)) != LMN_SATOM(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_EQMEM: {
    LmnInstrVar mem1, mem2;

    READ_VAL(LmnInstrVar, instr, mem1);
    READ_VAL(LmnInstrVar, instr, mem2);
    if (rc->wt(mem1) != rc->wt(mem2))
      return FALSE;
    break;
  }
  case INSTR_NEQMEM: {
    LmnInstrVar mem1, mem2;
    READ_VAL(LmnInstrVar, instr, mem1);
    READ_VAL(LmnInstrVar, instr, mem2);

    if (rc->wt(mem1) == rc->wt(mem2))
      return FALSE;
    break;
  }
  case INSTR_STABLE: {
    LmnInstrVar memi;
    READ_VAL(LmnInstrVar, instr, memi);

    if (lmn_mem_is_active((LmnMembraneRef)rc->wt(memi))) {
      return FALSE;
    }

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_STABLE);
      this->push_stackframe([=](bool result) {
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_STABLE);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_NEWLIST: {
    LmnInstrVar listi;
    Vector *listvec = vec_make(16);
    READ_VAL(LmnInstrVar, instr, listi);
    rc->reg(listi) = {(LmnWord)listvec, 0, TT_OTHER};

    /* 解放のための再帰 */
    this->push_stackframe([=](bool result) {
      vec_free(listvec);
      return result;
    });
    break;
  }
  case INSTR_ADDTOLIST: {
    LmnInstrVar listi, linki;
    READ_VAL(LmnInstrVar, instr, listi);
    READ_VAL(LmnInstrVar, instr, linki);
    vec_push((Vector *)rc->wt(listi), linki);

    break;
  }
  case INSTR_GETFROMLIST: {
    LmnInstrVar dsti, listi, posi;
    READ_VAL(LmnInstrVar, instr, dsti);
    READ_VAL(LmnInstrVar, instr, listi);
    READ_VAL(LmnInstrVar, instr, posi);

    switch (rc->at(listi)) {
    case LIST_AND_MAP:
      if (posi == 0) {
        rc->reg(dsti) = {vec_get((Vector *)rc->wt(listi), (unsigned int)posi),
                         LINK_LIST, TT_OTHER};
      } else if (posi == 1) {
        rc->reg(dsti) = {vec_get((Vector *)rc->wt(listi), (unsigned int)posi),
                         MAP, TT_OTHER};
      } else {
        lmn_fatal("unexpected attribute @instr_getfromlist");
      }
      break;
    case LINK_LIST: /* LinkObjをfreeするのはここ？ */
    {
      LinkObjRef lo =
          (LinkObjRef)vec_get((Vector *)rc->wt(listi), (unsigned int)posi);
      rc->reg(dsti) = {(LmnWord)LinkObjGetAtom(lo), LinkObjGetPos(lo), TT_ATOM};
      break;
    }
    }
    break;
  }
  case INSTR_IADD: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);
    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) + (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_ISUB: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) - (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IMUL: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) * (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IDIV: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) / (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};

    break;
  }
  case INSTR_INEG: {
    LmnInstrVar dstatom, atomi;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atomi);
    rc->reg(dstatom) = {static_cast<LmnWord>((-(long)rc->wt(atomi))),
                        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IMOD: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) % (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_INOT: {
    LmnInstrVar dstatom, atomi;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atomi);
    rc->reg(dstatom) = {static_cast<LmnWord>((~(int)rc->wt(atomi))),
                        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IAND: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) & (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IOR: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) | (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};

    break;
  }
  case INSTR_IXOR: {
    LmnInstrVar dstatom, atom1, atom2;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) ^ (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_ILT: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) < (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_ILE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) <= (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_IGT: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) > (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_IGE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) >= (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_IEQ: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) == (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_INE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!((long)rc->wt(atom1) != (long)rc->wt(atom2)))
      return FALSE;
    break;
  }
  case INSTR_ILTFUNC: {
    LmnInstrVar func1, func2;
    READ_VAL(LmnInstrVar, instr, func1);
    READ_VAL(LmnInstrVar, instr, func2);

    if (!((long)rc->wt(func1) < (long)rc->wt(func2)))
      return FALSE;
    break;
  }
  case INSTR_ILEFUNC: {
    LmnInstrVar func1, func2;
    READ_VAL(LmnInstrVar, instr, func1);
    READ_VAL(LmnInstrVar, instr, func2);

    if (!((long)rc->wt(func1) <= (long)rc->wt(func2)))
      return FALSE;
    break;
  }
  case INSTR_IGTFUNC: {
    LmnInstrVar func1, func2;
    READ_VAL(LmnInstrVar, instr, func1);
    READ_VAL(LmnInstrVar, instr, func2);

    if (!((long)rc->wt(func1) > (long)rc->wt(func2)))
      return FALSE;
    break;
  }
  case INSTR_IGEFUNC: {
    LmnInstrVar func1, func2;
    READ_VAL(LmnInstrVar, instr, func1);
    READ_VAL(LmnInstrVar, instr, func2);

    if (!((long)rc->wt(func1) >= (long)rc->wt(func2)))
      return FALSE;
    break;
  }
  case INSTR_FADD: {
    LmnInstrVar dstatom, atom1, atom2;
    LmnAtom d;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    d = lmn_create_double_atom(lmn_get_double(rc->wt(atom1)) +
                               lmn_get_double(rc->wt(atom2)));
    rc->reg(dstatom) = {d, LMN_DBL_ATTR, TT_ATOM};
    break;
  }
  case INSTR_FSUB: {
    LmnInstrVar dstatom, atom1, atom2;
    LmnAtom d;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    d = lmn_create_double_atom(lmn_get_double(rc->wt(atom1)) -
                               lmn_get_double(rc->wt(atom2)));
    rc->reg(dstatom) = {d, LMN_DBL_ATTR, TT_ATOM};
    break;
  }
  case INSTR_FMUL: {
    LmnInstrVar dstatom, atom1, atom2;
    LmnAtom d;

    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    d = lmn_create_double_atom(lmn_get_double(rc->wt(atom1)) *
                               lmn_get_double(rc->wt(atom2)));
    rc->reg(dstatom) = {d, LMN_DBL_ATTR, TT_ATOM};
    break;
  }
  case INSTR_FDIV: {
    LmnInstrVar dstatom, atom1, atom2;
    LmnAtom d;

    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    d = lmn_create_double_atom(lmn_get_double(rc->wt(atom1)) /
                               lmn_get_double(rc->wt(atom2)));
    rc->reg(dstatom) = {d, LMN_DBL_ATTR, TT_ATOM};
    break;
  }
  case INSTR_FNEG: {
    LmnInstrVar dstatom, atomi;
    LmnAtom d;
    READ_VAL(LmnInstrVar, instr, dstatom);
    READ_VAL(LmnInstrVar, instr, atomi);

    d = lmn_create_double_atom(-lmn_get_double(rc->wt(atomi)));
    rc->reg(dstatom) = {d, LMN_DBL_ATTR, TT_ATOM};
    break;
  }
  case INSTR_FLT: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) < lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_FLE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) <= lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_FGT: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) > lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_FGE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) >= lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_FEQ: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) == lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_FNE: {
    LmnInstrVar atom1, atom2;
    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!(lmn_get_double(rc->wt(atom1)) != lmn_get_double(rc->wt(atom2))))
      return FALSE;
    break;
  }
  case INSTR_ALLOCATOM: {
    LmnInstrVar atomi;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnLinkAttr, instr, attr);
    rc->at(atomi) = attr;
    if (LMN_ATTR_IS_DATA(attr)) {
      LmnWord w;
      LmnByte a = rc->at(atomi), t;
      READ_CONST_DATA_ATOM(w, a, t);
      rc->reg(atomi) = {w, a, t};
    } else { /* symbol atom */
      LmnFunctor f;
      /*         fprintf(stderr, "symbol atom can't be created in GUARD\n");
       */
      /*         exit(EXIT_FAILURE); */
      READ_VAL(LmnFunctor, instr, f);

      /* 本来のallocatomは格納するのは定数アトムだが、簡単のためにファンクタを格納する
       */
      rc->wt(atomi) = f;
    }
    rc->tt(atomi) = TT_OTHER; /* ヘッドに存在しないのでコピー対象外 */
    break;
  }
  case INSTR_ALLOCATOMINDIRECT: {
    LmnInstrVar atomi;
    LmnInstrVar srcatomi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, srcatomi);

    if (LMN_ATTR_IS_DATA(rc->at(srcatomi))) {
      if (LMN_ATTR_IS_EX(rc->at(srcatomi))) {
        rc->wt(atomi) = rc->wt(srcatomi);
      } else {
        rc->wt(atomi) = lmn_copy_data_atom(rc->wt(srcatomi), rc->at(srcatomi));
      }
      rc->at(atomi) = rc->at(srcatomi);
      rc->tt(atomi) = TT_OTHER;
    } else { /* symbol atom */
      fprintf(stderr, "symbol atom can't be created in GUARD\n");
      exit(EXIT_FAILURE);
    }
    break;
  }
  case INSTR_SAMEFUNC: {
    LmnInstrVar atom1, atom2;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);

    if (!lmn_eq_func((LmnAtomRef)rc->wt(atom1), rc->at(atom1),
                     (LmnAtomRef)rc->wt(atom2), rc->at(atom2)))
      return FALSE;
    break;
  }
  case INSTR_GETFUNC: {
    LmnInstrVar funci, atomi;

    READ_VAL(LmnInstrVar, instr, funci);
    READ_VAL(LmnInstrVar, instr, atomi);

    if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
      /* ここで得るファンクタはガード命令中で一時的に使われるだけなので
         double はポインタのコピーで十分なはず */
      rc->reg(funci) = {rc->wt(atomi), rc->at(atomi), TT_OTHER};
    } else {

      rc->reg(funci) = {
          (LmnWord)LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(atomi)),
          rc->at(atomi), TT_OTHER};
    }
    break;
  }
  case INSTR_PRINTINSTR: {
    char c;

    while (TRUE) {
      READ_VAL(char, instr, c);
      if (!c)
        break;
      fprintf(stderr, "%c", c);
    }
    break;
  }
  case INSTR_SETMEMNAME: {
    LmnInstrVar memi;
    lmn_interned_str name;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(lmn_interned_str, instr, name);
    lmn_mem_set_name((LmnMembraneRef)rc->wt(memi), name);
    break;
  }
  case INSTR_COPYRULES: {
    LmnInstrVar destmemi, srcmemi;
    unsigned int i;
    struct Vector *v;

    READ_VAL(LmnInstrVar, instr, destmemi);
    READ_VAL(LmnInstrVar, instr, srcmemi);
    v = lmn_mem_get_rulesets((LmnMembraneRef)rc->wt(srcmemi));
    for (i = 0; i < v->num; i++) {
      auto cp = new LmnRuleSet(*(LmnRuleSetRef)vec_get(v, i));
      lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(destmemi), cp);
    }
    break;
  }
  case INSTR_REMOVEPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    lmn_mem_remove_proxies((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_INSERTPROXIES: {
    LmnInstrVar parentmemi, childmemi;

    READ_VAL(LmnInstrVar, instr, parentmemi);
    READ_VAL(LmnInstrVar, instr, childmemi);
    lmn_mem_insert_proxies((LmnMembraneRef)rc->wt(parentmemi),
                           (LmnMembraneRef)rc->wt(childmemi));
    break;
  }
  case INSTR_DELETECONNECTORS: {
    LmnInstrVar srcset, srcmap;
    HashSet *delset;
    ProcessTableRef delmap;
    HashSetIterator it;
    READ_VAL(LmnInstrVar, instr, srcset);
    READ_VAL(LmnInstrVar, instr, srcmap);

    delset = (HashSet *)rc->wt(srcset);
    delmap = (ProcessTableRef)rc->wt(srcmap);

    for (it = hashset_iterator(delset); !hashsetiter_isend(&it);
         hashsetiter_next(&it)) {
      LmnSymbolAtomRef orig, copy;
      LmnWord t;

      orig = (LmnSymbolAtomRef)hashsetiter_entry(&it);
      t = 0;
      proc_tbl_get_by_atom(delmap, orig, &t);
      copy = (LmnSymbolAtomRef)t;

      lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);
      /* mem がないので仕方なく直接アトムリストをつなぎ変える.
       * UNIFYアトムはnatomに含まれないので大丈夫 */
      LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(copy),
                         LMN_SATOM_GET_PREV(copy));
      LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(copy),
                         LMN_SATOM_GET_NEXT_RAW(copy));

      lmn_delete_atom(copy);
    }

    proc_tbl_free(delmap);
    break;
  }
  case INSTR_REMOVETOPLEVELPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    lmn_mem_remove_toplevel_proxies((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_DEREFFUNC: {
    LmnInstrVar funci, atomi, pos;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, funci);
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnLinkAttr, instr, pos);

    attr = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), pos);
    if (LMN_ATTR_IS_DATA(attr)) {
      rc->reg(funci) = {
          (LmnWord)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)rc->wt(atomi), pos),
          attr, TT_OTHER};
    } else { /* symbol atom */
      rc->reg(funci) = {
          LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)LMN_SATOM_GET_LINK(
              (LmnSymbolAtomRef)rc->wt(atomi), pos)),
          attr, TT_OTHER};
    }
    break;
  }
  case INSTR_LOADFUNC: {
    LmnInstrVar funci;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, funci);
    READ_VAL(LmnLinkAttr, instr, attr);
    rc->at(funci) = attr;
    rc->tt(funci) = TT_OTHER;
    if (LMN_ATTR_IS_DATA(attr)) {
      LmnWord w;
      LmnByte a = rc->at(funci), t;
      READ_CONST_DATA_ATOM(w, a, t);
      rc->reg(funci) = {w, a, t};
    } else {
      LmnFunctor f;

      READ_VAL(LmnFunctor, instr, f);
      rc->wt(funci) = f;
      rc->tt(funci) = TT_OTHER;
    }
    break;
  }
  case INSTR_EQFUNC: {
    LmnInstrVar func0;
    LmnInstrVar func1;

    READ_VAL(LmnFunctor, instr, func0);
    READ_VAL(LmnFunctor, instr, func1);

    if (rc->at(func0) != rc->at(func1))
      return FALSE;
    switch (rc->at(func0)) {
    case LMN_INT_ATTR:
      if ((long)rc->wt(func0) != (long)rc->wt(func1))
        return FALSE;
      break;
    case LMN_DBL_ATTR:
      if (lmn_get_double(rc->wt(func0)) != lmn_get_double(rc->wt(func1)))
        return FALSE;
      break;
    case LMN_HL_ATTR:
      if (!lmn_hyperlink_eq_hl(
              lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func0)),
              lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func1))))
        return FALSE;
      break;
    default:
      if (rc->wt(func0) != rc->wt(func1))
        return FALSE;
      break;
    }
    break;
  }
  case INSTR_NEQFUNC: {
    LmnInstrVar func0;
    LmnInstrVar func1;

    READ_VAL(LmnFunctor, instr, func0);
    READ_VAL(LmnFunctor, instr, func1);

    if (rc->at(func0) == rc->at(func1)) {
      switch (rc->at(func0)) {
      case LMN_INT_ATTR:
        if ((long)rc->wt(func0) == (long)rc->wt(func1))
          return FALSE;
        break;
      case LMN_DBL_ATTR:
        if (lmn_get_double(rc->wt(func0)) == lmn_get_double(rc->wt(func1)))
          return FALSE;
        break;
      case LMN_HL_ATTR:
        if (lmn_hyperlink_eq_hl(
                lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func0)),
                lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func1))))
          return FALSE;
        break;
      default:
        if (rc->wt(func0) == rc->wt(func1))
          return FALSE;
        break;
      }
    }
    break;
  }
  case INSTR_ADDATOM: {
    LmnInstrVar memi, atomi;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);
    lmn_mem_push_atom((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atomi),
                      rc->at(atomi));
    break;
  }
  case INSTR_MOVECELLS: {
    LmnInstrVar destmemi, srcmemi;

    READ_VAL(LmnInstrVar, instr, destmemi);
    READ_VAL(LmnInstrVar, instr, srcmemi);
    LMN_ASSERT(rc->wt(destmemi) != rc->wt(srcmemi));
    lmn_mem_move_cells((LmnMembraneRef)rc->wt(destmemi),
                       (LmnMembraneRef)rc->wt(srcmemi));
    break;
  }
  case INSTR_REMOVETEMPORARYPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    lmn_mem_remove_temporary_proxies((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_NFREELINKS: {
    LmnInstrVar memi, count;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, count);

    if (!lmn_mem_nfreelinks((LmnMembraneRef)rc->wt(memi), count))
      return FALSE;

    if (RC_GET_MODE(rc, REACT_ND) && RC_MC_USE_DPOR(rc) && !rc->is_zerostep) {
      LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
      dpor_LHS_flag_add(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NFLINKS);
      this->push_stackframe([=](bool result) {
        LMN_ASSERT(!result);
        dpor_LHS_flag_remove(RC_POR_DATA(rc), lmn_mem_id(m), LHS_MEM_NFLINKS);
        return false; /* 全ての候補取得のためにNDは常にFALSEを返す仕様 */
      });
    }

    break;
  }
  case INSTR_COPYCELLS: {
    LmnInstrVar mapi, destmemi, srcmemi;

    READ_VAL(LmnInstrVar, instr, mapi);
    READ_VAL(LmnInstrVar, instr, destmemi);
    READ_VAL(LmnInstrVar, instr, srcmemi);
    rc->wt(mapi) = (LmnWord)lmn_mem_copy_cells((LmnMembraneRef)rc->wt(destmemi),
                                               (LmnMembraneRef)rc->wt(srcmemi));
    rc->tt(mapi) = TT_OTHER;
    break;
  }
  case INSTR_LOOKUPLINK: {
    LmnInstrVar destlinki, tbli, srclinki;

    READ_VAL(LmnInstrVar, instr, destlinki);
    READ_VAL(LmnInstrVar, instr, tbli);
    READ_VAL(LmnInstrVar, instr, srclinki);

    rc->at(destlinki) = LINKED_ATTR(srclinki);
    rc->tt(destlinki) = TT_ATOM;
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(srclinki))) {
      rc->wt(destlinki) = (LmnWord)LINKED_ATOM(srclinki);
    } else { /* symbol atom */
      ProcessTableRef ht = (ProcessTableRef)rc->wt(tbli);
      LmnWord w = rc->wt(destlinki);
      proc_tbl_get_by_atom(ht, (LmnSymbolAtomRef)LINKED_ATOM(srclinki), &w);
      rc->wt(destlinki) = w;
    }
    break;
  }
  case INSTR_CLEARRULES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    lmn_mem_clearrules((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_DROPMEM: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    lmn_mem_drop((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_TESTMEM: {
    LmnInstrVar memi, atomi;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);
    LMN_ASSERT(!LMN_ATTR_IS_DATA(rc->at(atomi)));
    LMN_ASSERT(LMN_IS_PROXY_FUNCTOR(
        LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(atomi))));

    if (LMN_PROXY_GET_MEM((LmnSymbolAtomRef)rc->wt(atomi)) !=
        (LmnMembraneRef)rc->wt(memi))
      return FALSE;
    break;
  }
  case INSTR_IADDFUNC: {
    LmnInstrVar desti, i0, i1;

    READ_VAL(LmnInstrVar, instr, desti);
    READ_VAL(LmnInstrVar, instr, i0);
    READ_VAL(LmnInstrVar, instr, i1);
    LMN_ASSERT(rc->at(i0) == LMN_INT_ATTR);
    LMN_ASSERT(rc->at(i1) == LMN_INT_ATTR);
    rc->reg(desti) = {rc->wt(i0) + rc->wt(i1), LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_ISUBFUNC: {
    LmnInstrVar desti, i0, i1;

    READ_VAL(LmnInstrVar, instr, desti);
    READ_VAL(LmnInstrVar, instr, i0);
    READ_VAL(LmnInstrVar, instr, i1);
    LMN_ASSERT(rc->at(i0) == LMN_INT_ATTR);
    LMN_ASSERT(rc->at(i1) == LMN_INT_ATTR);
    rc->reg(desti) = {rc->wt(i0) - rc->wt(i1), LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IMULFUNC: {
    LmnInstrVar desti, i0, i1;

    READ_VAL(LmnInstrVar, instr, desti);
    READ_VAL(LmnInstrVar, instr, i0);
    READ_VAL(LmnInstrVar, instr, i1);
    LMN_ASSERT(rc->at(i0) == LMN_INT_ATTR);
    LMN_ASSERT(rc->at(i1) == LMN_INT_ATTR);
    rc->reg(desti) = {rc->wt(i0) * rc->wt(i1), LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IDIVFUNC: {
    LmnInstrVar desti, i0, i1;

    READ_VAL(LmnInstrVar, instr, desti);
    READ_VAL(LmnInstrVar, instr, i0);
    READ_VAL(LmnInstrVar, instr, i1);
    LMN_ASSERT(rc->at(i0) == LMN_INT_ATTR);
    LMN_ASSERT(rc->at(i1) == LMN_INT_ATTR);
    rc->reg(desti) = {rc->wt(i0) / rc->wt(i1), LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_IMODFUNC: {
    LmnInstrVar desti, i0, i1;

    READ_VAL(LmnInstrVar, instr, desti);
    READ_VAL(LmnInstrVar, instr, i0);
    READ_VAL(LmnInstrVar, instr, i1);
    LMN_ASSERT(rc->at(i0) == LMN_INT_ATTR);
    LMN_ASSERT(rc->at(i1) == LMN_INT_ATTR);
    rc->reg(desti) = {rc->wt(i0) % rc->wt(i1), LMN_INT_ATTR, TT_ATOM};
    break;
  }
  case INSTR_GROUP: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    auto next = instr + subinstr_size;
    this->push_stackframe([=](bool result) {
      if (!result)
        return false;
      this->instr = next;
      return this->run();
    });
    break;
  }
  case INSTR_BRANCH: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    if (RC_HLINK_SPC(rc)) {
      lmn_sameproccxt_clear(
          rc); /*branchとhyperlinkを同時起動するための急場しのぎ */
    }

    auto next = instr + subinstr_size;
    this->push_stackframe([=](bool result) {
      if (result)
        return true;
      this->instr = next;
      return this->run();
    });
    break;
  }
  case INSTR_LOOP: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    this->push_stackframe(
        exec_subinstructions_while(*this, instr, instr + subinstr_size));
    break;
  }
  case INSTR_CALLBACK: {
    LmnInstrVar memi, atomi;
    LmnSymbolAtomRef atom;
    const struct CCallback *c;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);

    atom = (LmnSymbolAtomRef)rc->wt(atomi);

    if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0))) {
      LmnSymbolAtomRef f_name = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, 0);
      lmn_interned_str name =
          LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(f_name));
      int arity = LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom));

      c = get_ccallback(name);
      if (!c)
        break;

      if (arity - 1 != c->arity) {
        fprintf(stderr, "EXTERNAL FUNC: invalid arity - %s\n",
                LMN_SYMBOL_STR(name));
        break;
      }

      /* (2015-07-30) moved to the end so that lmn_dump_mem can safely
         be called in callback functions
      lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi), rc->wt( atomi),
      rc->at(atomi)); lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi),
                          LMN_SATOM_GET_LINK(atom, 0),
                          LMN_SATOM_GET_ATTR(atom, 0));
      */

      switch (arity) {
      case 1:
        ((callback_0)c->f)(rc, (LmnMembraneRef)rc->wt(memi));
        break;
      case 2:
        ((callback_1)c->f)(rc, (LmnMembraneRef)rc->wt(memi),
                           LMN_SATOM_GET_LINK(atom, 1),
                           LMN_SATOM_GET_ATTR(atom, 1));
        break;
      case 3:
        ((callback_2)c->f)(
            rc, (LmnMembraneRef)rc->wt(memi), LMN_SATOM_GET_LINK(atom, 1),
            LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
            LMN_SATOM_GET_ATTR(atom, 2));
        break;
      case 4:
        ((callback_3)c->f)(
            rc, (LmnMembraneRef)rc->wt(memi), LMN_SATOM_GET_LINK(atom, 1),
            LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
            LMN_SATOM_GET_ATTR(atom, 2), LMN_SATOM_GET_LINK(atom, 3),
            LMN_SATOM_GET_ATTR(atom, 3));
        break;
      case 5:
        ((callback_4)c->f)(
            rc, (LmnMembraneRef)rc->wt(memi), LMN_SATOM_GET_LINK(atom, 1),
            LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
            LMN_SATOM_GET_ATTR(atom, 2), LMN_SATOM_GET_LINK(atom, 3),
            LMN_SATOM_GET_ATTR(atom, 3), LMN_SATOM_GET_LINK(atom, 4),
            LMN_SATOM_GET_ATTR(atom, 4));
        break;
      case 6:
        ((callback_5)c->f)(
            rc, (LmnMembraneRef)rc->wt(memi), LMN_SATOM_GET_LINK(atom, 1),
            LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
            LMN_SATOM_GET_ATTR(atom, 2), LMN_SATOM_GET_LINK(atom, 3),
            LMN_SATOM_GET_ATTR(atom, 3), LMN_SATOM_GET_LINK(atom, 4),
            LMN_SATOM_GET_ATTR(atom, 4), LMN_SATOM_GET_LINK(atom, 5),
            LMN_SATOM_GET_ATTR(atom, 5));
        break;
      default:
        printf("EXTERNAL FUNCTION: too many arguments\n");
        break;
      }

      lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi),
                          (LmnAtomRef)rc->wt(atomi), rc->at(atomi));
      lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi),
                          LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0),
                          LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)atom, 0));
    }

    break;
  }
  case INSTR_GETCLASS: {
    LmnInstrVar reti, atomi;

    READ_VAL(LmnInstrVar, instr, reti);
    READ_VAL(LmnInstrVar, instr, atomi);

    rc->tt(reti) = TT_OTHER;
    if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
      switch (rc->at(atomi)) {
      case LMN_INT_ATTR:
        rc->wt(reti) = lmn_intern("int");
        break;
      case LMN_DBL_ATTR:
        rc->wt(reti) = lmn_intern("float");
        break;
      case LMN_SP_ATOM_ATTR:
        rc->wt(reti) = SP_ATOM_NAME(rc->wt(atomi));
        break;
      default:
        rc->wt(reti) = lmn_intern("unknown");
        break;
      }
    } else { /* symbol atom */
      rc->wt(reti) = lmn_intern("symbol");
    }
    break;
  }
  case INSTR_SUBCLASS: {
    LmnInstrVar subi, superi;

    READ_VAL(LmnInstrVar, instr, subi);
    READ_VAL(LmnInstrVar, instr, superi);

    /* サブやスーパークラスなどの階層の概念がないので単純比較を行う */
    if (rc->wt(subi) != rc->wt(superi))
      return FALSE;
    break;
  }
  case INSTR_CELLDUMP: {
    printf("CELL DUMP:\n");
    lmn_dump_cell_stdout(RC_GROOT_MEM(rc));
    lmn_hyperlink_print(RC_GROOT_MEM(rc));
    break;
  }
  default:
    fprintf(stderr, "interpret: Unknown operation %d\n", op);
    exit(1);
  }

  stop = false;
  return false;
}

bool slim::vm::interpreter::run() {
  bool result;
  while (true) {
    bool stop;
    result = exec_command(this->rc, this->rule, this->instr, stop);
    if (stop)
      break;
  }

  while (!this->callstack.empty()) {
    auto s = this->callstack.back();
    this->callstack.pop_back();
    result = s.callback(result);
  }

  return result;
}

bool slim::vm::interpreter::interpret(LmnReactCxt *rc, LmnRuleRef rule,
                                      LmnRuleInstr instr) {
  auto trc = this->rc;
  auto trule = this->rule;
  auto tinstr = this->instr;
  std::vector<stack_frame> tstack;
  this->rc = rc;
  this->rule = rule;
  this->instr = instr;
  tstack.swap(this->callstack);
  bool result = run();
  this->rc = trc;
  this->rule = trule;
  this->instr = tinstr;
  this->callstack = tstack;
  return result;
}

static BOOL dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule,
                           LmnRuleInstr instr) {
  /*   LmnRuleInstr start = instr; */
  LmnInstrOp op;

  while (TRUE) {
    READ_VAL(LmnInstrOp, instr, op);
    /*     fprintf(stdout, "op: %d %d\n", op, (instr - start)); */
    /*     lmn_dump_mem(LmnMembraneRef)wt(rc, 0)); */
    switch (op) {
    case INSTR_SPEC: {
      LmnInstrVar s0;

      SKIP_VAL(LmnInstrVar, instr);
      READ_VAL(LmnInstrVar, instr, s0);

      rc->resize(s0);
      break;
    }
    case INSTR_INSERTCONNECTORSINNULL: {
      LmnInstrVar seti, list_num;
      Vector links;
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num + 1);
      for (i = 0; i < list_num; i++) {
        LmnInstrVar t;
        READ_VAL(LmnInstrVar, instr, t);
        vec_push(&links, (LmnWord)t);
      }

      rc->reg(seti) = {(LmnWord)insertconnectors(rc, NULL, &links), 0,
                       TT_OTHER};
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if (dmem_interpret(rc, rule, instr)) {
        hashset_free((HashSet *)rc->wt(seti));
        return TRUE;
      } else {
        LMN_ASSERT(0);
      }
      break;
    }
    case INSTR_INSERTCONNECTORS: {
      LmnInstrVar seti, list_num, memi, enti;
      Vector links; /* src list */
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num + 1);

      for (i = 0; i < list_num; i++) {
        READ_VAL(LmnInstrVar, instr, enti);
        vec_push(&links, (LmnWord)enti);
      }

      READ_VAL(LmnInstrVar, instr, memi);

      rc->reg(seti) = {
          (LmnWord)insertconnectors(rc, (LmnMembraneRef)rc->wt(memi), &links),
          0, TT_OTHER};
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if (dmem_interpret(rc, rule, instr)) {
        hashset_free((HashSet *)rc->wt(seti));
        return TRUE;
      } else {
        LMN_ASSERT(0);
      }
      break;
    }
    case INSTR_NEWATOM: {
      LmnInstrVar atomi, memi;
      LmnAtomRef ap;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnLinkAttr, instr, attr);
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_DATA_ATOM(ap, attr);
      } else { /* symbol atom */
        LmnFunctor f;

        READ_VAL(LmnFunctor, instr, f);
        ap = (LmnAtomRef)dmem_root_new_atom(RC_ND_MEM_DELTA_ROOT(rc), f);
      }

      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembraneRef)rc->wt(memi), (LmnAtomRef)ap, attr);

      rc->reg(atomi) = {
          (LmnWord)ap, attr,
          TT_OTHER}; /* BODY命令のアトムなのでコピー対象にしない->TT_OTHER */
      break;
    }
    case INSTR_COPYATOM: {
      LmnInstrVar atom1, memi, atom2;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atom2);

      rc->reg(atom1) = {(LmnWord)dmem_root_copy_atom(RC_ND_MEM_DELTA_ROOT(rc),
                                                     (LmnAtomRef)rc->wt(atom2),
                                                     rc->at(atom2)),
                        rc->at(atom2), TT_OTHER};
      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembraneRef)rc->wt(memi),
                          (LmnAtomRef)rc->wt(atom1), rc->at(atom1));
      break;
    }
    case INSTR_ALLOCLINK: {
      LmnInstrVar link, atom, n;

      READ_VAL(LmnInstrVar, instr, link);
      READ_VAL(LmnInstrVar, instr, atom);
      READ_VAL(LmnInstrVar, instr, n);

      if (LMN_ATTR_IS_DATA(rc->at(atom))) {
        rc->wt(link) = rc->wt(atom);
        rc->at(link) = rc->at(atom);
      } else { /* link to atom */
        rc->wt(link) = (LmnWord)LMN_SATOM(rc->wt(atom));
        rc->at(link) = LMN_ATTR_MAKE_LINK(n);
      }
      rc->tt(link) = TT_OTHER;
      break;
    }
    case INSTR_UNIFYLINKS: {
      LmnInstrVar link1, link2, mem;

      READ_VAL(LmnInstrVar, instr, link1);
      READ_VAL(LmnInstrVar, instr, link2);
      READ_VAL(LmnInstrVar, instr, mem);

      if (LMN_ATTR_IS_DATA(LINKED_ATTR(link1))) {
        if (LMN_ATTR_IS_DATA(LINKED_ATTR(link2))) { /* 1, 2 are data */
          dmem_root_link_data_atoms(
              RC_ND_MEM_DELTA_ROOT(rc), (LmnMembraneRef)rc->wt(mem),
              (LmnDataAtomRef)LINKED_ATOM(link1), LINKED_ATTR(link1),
              (LmnDataAtomRef)LINKED_ATOM(link2), LINKED_ATTR(link2));
        } else { /* 1 is data */
          dmem_root_unify_links(RC_ND_MEM_DELTA_ROOT(rc),
                                (LmnMembraneRef)rc->wt(mem), LINKED_ATOM(link2),
                                LINKED_ATTR(link2), LINKED_ATOM(link1),
                                LINKED_ATTR(link1));
        }
      } else { /* 2 is data or 1, 2 are symbol atom */
        dmem_root_unify_links(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembraneRef)rc->wt(mem), LINKED_ATOM(link1),
                              LINKED_ATTR(link1), LINKED_ATOM(link2),
                              LINKED_ATTR(link2));
      }
      break;
    }
    case INSTR_NEWLINK: {
      LmnInstrVar atom1, atom2, pos1, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_newlink(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembraneRef)rc->wt(memi),
                        (LmnAtomRef)rc->wt(atom1), rc->at(atom1), pos1,
                        (LmnAtomRef)rc->wt(atom2), rc->at(atom2), pos2);
      break;
    }
    case INSTR_RELINK: {
      LmnInstrVar atom1, atom2, pos1, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_relink(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembraneRef)rc->wt(memi),
                       (LmnAtomRef)rc->wt(atom1), rc->at(atom1), pos1,
                       (LmnAtomRef)rc->wt(atom2), rc->at(atom2), pos2);
      break;
    }
    case INSTR_GETLINK: {
      LmnInstrVar linki, atomi, posi;
      READ_VAL(LmnInstrVar, instr, linki);
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, posi);

      rc->wt(linki) = (LmnWord)dmem_root_get_link(
          RC_ND_MEM_DELTA_ROOT(rc), (LmnSymbolAtomRef)rc->wt(atomi), posi);
      rc->at(linki) =
          (LmnWord)LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)rc->wt(atomi), posi);
      rc->tt(linki) = TT_OTHER;
      break;
    }
    case INSTR_UNIFY: {
      LmnInstrVar atom1, pos1, atom2, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_unify_atom_args(RC_ND_MEM_DELTA_ROOT(rc),
                                (LmnMembraneRef)rc->wt(memi),
                                (LmnSymbolAtomRef)rc->wt(atom1), pos1,
                                (LmnSymbolAtomRef)rc->wt(atom2), pos2);
      break;
    }
    case INSTR_PROCEED:
      return TRUE;
    case INSTR_STOP:
      return FALSE;
    case INSTR_ENQUEUEATOM: {
      SKIP_VAL(LmnInstrVar, instr);
      /* do nothing */
      break;
    }
    case INSTR_DEQUEUEATOM: {
      SKIP_VAL(LmnInstrVar, instr);
      /* do nothing */
      break;
    }
    case INSTR_NEWMEM: {
      LmnInstrVar newmemi, parentmemi;
      LmnMembraneRef mp;

      READ_VAL(LmnInstrVar, instr, newmemi);
      READ_VAL(LmnInstrVar, instr, parentmemi);
      SKIP_VAL(LmnInstrVar, instr);

      mp = dmem_root_new_mem(RC_ND_MEM_DELTA_ROOT(rc)); /*lmn_new_mem(memf);*/
      dmem_root_add_child_mem(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembraneRef)rc->wt(parentmemi), mp);
      rc->wt(newmemi) = (LmnWord)mp;
      rc->tt(newmemi) = TT_OTHER;
      lmn_mem_set_active(mp, TRUE);
      if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
        lmn_memstack_push(RC_MEMSTACK((MemReactContext *)rc), mp);
      }
      break;
    }
    case INSTR_ALLOCMEM: {
      LmnInstrVar dstmemi;

      READ_VAL(LmnInstrVar, instr, dstmemi);

      rc->wt(dstmemi) = (LmnWord)dmem_root_new_mem(RC_ND_MEM_DELTA_ROOT(rc));
      rc->tt(dstmemi) = TT_OTHER;
      break;
    }
    case INSTR_REMOVEATOM: {
      LmnInstrVar atomi, memi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_remove_atom(RC_ND_MEM_DELTA_ROOT(rc),
                            (LmnMembraneRef)rc->wt(memi),
                            (LmnAtomRef)rc->wt(atomi), rc->at(atomi));
      break;
    }
    case INSTR_FREEATOM: {
      LmnInstrVar atomi;

      READ_VAL(LmnInstrVar, instr, atomi);

      dmem_root_free_atom(RC_ND_MEM_DELTA_ROOT(rc), (LmnAtomRef)rc->wt(atomi),
                          rc->at(atomi));
      break;
    }
    case INSTR_REMOVEMEM: {
      LmnInstrVar memi, parenti;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, parenti);

      dmem_root_remove_mem(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembraneRef)rc->wt(parenti),
                           (LmnMembraneRef)rc->wt(memi));
      break;
    }
    case INSTR_FREEMEM: {
      LmnInstrVar memi;
      LmnMembraneRef mp;

      READ_VAL(LmnInstrVar, instr, memi);

      mp = (LmnMembraneRef)rc->wt(memi);
      /*       lmn_mem_free(mp); */
      break;
    }
    case INSTR_ADDMEM: {
      LmnInstrVar dstmem, srcmem;

      READ_VAL(LmnInstrVar, instr, dstmem);
      READ_VAL(LmnInstrVar, instr, srcmem);

      //      LMN_ASSERT(!((LmnMembraneRef)rc->wt( srcmem))->parent);

      //      lmn_mem_add_child_mem((LmnMembraneRef)rc->wt( dstmem),
      //      (LmnMembraneRef)rc->wt( srcmem));
      dmem_root_add_child_mem(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembraneRef)rc->wt(dstmem),
                              (LmnMembraneRef)rc->wt(srcmem));
      break;

      break;
    }
    case INSTR_ENQUEUEMEM: {
      SKIP_VAL(LmnInstrVar, instr);
      //      if (RC_GET_MODE(rc, REACT_ND) && !rc->is_zerostep)
      //      {
      //        lmn_mem_activate_ancestors((LmnMembraneRef)rc->wt( memi)); /* MC
      //        */
      //      }
      /* 通常実行ではdmem_interpretを使用しないため以下のコードは不要.
       * ただ,
       * 通常実行用dmemはテスト用やinteractive実行用として作っておいてもよさそう
       */
      //      if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
      //        lmn_memstack_push(RC_MEMSTACK((MemReactContext *)rc),
      //        (LmnMembraneRef)rc->wt( memi)); /* 通常実行時 */
      //      }
      break;
    }
    case INSTR_UNLOCKMEM: {
      SKIP_VAL(LmnInstrVar, instr);
      /* do nothing */
      break;
    }
    case INSTR_LOADRULESET: {
      LmnInstrVar memi;
      LmnRulesetId id;
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnRulesetId, instr, id);

      lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(memi),
                          LmnRuleSetTable::at(id));
      break;
    }
    case INSTR_LOADMODULE: {
      LmnInstrVar memi;
      lmn_interned_str module_name_id;
      LmnRuleSetRef ruleset;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(lmn_interned_str, instr, module_name_id);

      if ((ruleset = lmn_get_module_ruleset(module_name_id))) {
        /* テーブル内にルールセットがある場合 */
        lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(memi), ruleset);
      } else {
        /* テーブル内にルールセットがない場合 */
        fprintf(stderr, "Undefined module %s\n",
                lmn_id_to_name(module_name_id));
      }
      break;
    }
    case INSTR_RECURSIVELOCK: {
      SKIP_VAL(LmnInstrVar, instr);
      /* do nothing */
      break;
    }
    case INSTR_RECURSIVEUNLOCK: {
      SKIP_VAL(LmnInstrVar, instr);
      /* do nothing */
      break;
    }
    case INSTR_COPYGROUND: {
      LmnInstrVar dstlist, srclist, memi;
      Vector *srcvec, *dstlovec, *retvec; /* 変数番号のリスト */
      ProcessTableRef atommap;

      READ_VAL(LmnInstrVar, instr, dstlist);
      READ_VAL(LmnInstrVar, instr, srclist);
      READ_VAL(LmnInstrVar, instr, memi);

      /* リンクオブジェクトのベクタを構築 */
      srcvec = links_from_idxs((Vector *)rc->wt(srclist), rc);

      dmem_root_copy_ground(RC_ND_MEM_DELTA_ROOT(rc),
                            (LmnMembraneRef)rc->wt(memi), srcvec, &dstlovec,
                            &atommap);
      free_links(srcvec);

      /* 返り値の作成 */
      retvec = vec_make(2);
      vec_push(retvec, (LmnWord)dstlovec);
      vec_push(retvec, (LmnWord)atommap);
      rc->reg(dstlist) = {(LmnWord)retvec, LIST_AND_MAP, TT_OTHER};

      /* 解放のための再帰。ベクタを解放するための中間語命令がない */
      dmem_interpret(rc, rule, instr);

      free_links(dstlovec);
      vec_free(retvec);

      return TRUE; /* COPYGROUNDはボディに出現する */
    }
    case INSTR_REMOVEGROUND:
    case INSTR_FREEGROUND: {
      LmnInstrVar listi, memi;
      Vector *srcvec; /* 変数番号のリスト */

      memi = 0; /* warningを黙らす */
      READ_VAL(LmnInstrVar, instr, listi);
      if (INSTR_REMOVEGROUND == op) {
        READ_VAL(LmnInstrVar, instr, memi);
      }

      srcvec = links_from_idxs((Vector *)rc->wt(listi), rc);

      switch (op) {
      case INSTR_REMOVEGROUND:
        dmem_root_remove_ground(RC_ND_MEM_DELTA_ROOT(rc),
                                (LmnMembraneRef)rc->wt(memi), srcvec);
        break;
      case INSTR_FREEGROUND:
        /* memを使い回す関係上freeするとまずい */
        //         dmem_root_free_ground(RC_ND_MEM_DELTA_ROOT(rc), srcvec);
        break;
      }

      free_links(srcvec);

      break;
    }
    case INSTR_NEWLIST: {
      LmnInstrVar listi;
      Vector *listvec = vec_make(16);
      READ_VAL(LmnInstrVar, instr, listi);
      rc->reg(listi) = {(LmnWord)listvec, 0, TT_OTHER};

      if (dmem_interpret(rc, rule, instr)) {
        vec_free(listvec);
        return TRUE;
      } else {
        vec_free(listvec);
        return FALSE;
      }
      break;
    }
    case INSTR_ADDTOLIST: {
      LmnInstrVar listi, linki;
      READ_VAL(LmnInstrVar, instr, listi);
      READ_VAL(LmnInstrVar, instr, linki);
      vec_push((Vector *)rc->wt(listi), linki);
      break;
    }
    case INSTR_GETFROMLIST: {
      LmnInstrVar dsti, listi, posi;
      READ_VAL(LmnInstrVar, instr, dsti);
      READ_VAL(LmnInstrVar, instr, listi);
      READ_VAL(LmnInstrVar, instr, posi);

      switch (rc->at(listi)) {
      case LIST_AND_MAP:

        if (posi == 0) {
          rc->reg(dsti) = {vec_get((Vector *)rc->wt(listi), (unsigned int)posi),
                           LINK_LIST, TT_OTHER};
        } else if (posi == 1) {
          rc->reg(dsti) = {vec_get((Vector *)rc->wt(listi), (unsigned int)posi),
                           MAP, TT_OTHER};
        } else {
          LMN_ASSERT(0);
        }
        break;
      case LINK_LIST: /* LinkObjをfreeするのはここ？ */
      {
        LinkObjRef lo =
            (LinkObjRef)vec_get((Vector *)rc->wt(listi), (unsigned int)posi);
        rc->wt(dsti) = (LmnWord)LinkObjGetAtom(lo);
        rc->at(dsti) = LinkObjGetPos(lo);
        break;
      }
      default:
        lmn_fatal("unexpected.");
        break;
      }
      break;
    }
    case INSTR_ALLOCATOM: {
      LmnInstrVar atomi;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);

      rc->at(atomi) = attr;
      if (LMN_ATTR_IS_DATA(attr)) {
        LmnWord w;
        LmnByte a = rc->at(atomi), t;
        READ_CONST_DATA_ATOM(w, a, t);
        rc->reg(atomi) = {w, a, t};
      } else { /* symbol atom */
        LmnFunctor f;
        /*         fprintf(stderr, "symbol atom can't be created in GUARD\n");
         */
        /*         exit(EXIT_FAILURE); */
        READ_VAL(LmnFunctor, instr, f);

        /* 本来のallocatomは格納するのは定数アトムだが、簡単のためにファンクタを格納する
         */
        rc->wt(atomi) = f;
      }
      rc->tt(atomi) = TT_OTHER;
      break;
    }
    case INSTR_ALLOCATOMINDIRECT: {
      LmnInstrVar atomi;
      LmnInstrVar srcatomi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, srcatomi);

      if (LMN_ATTR_IS_DATA(rc->at(srcatomi))) {
        rc->reg(atomi) = {
            lmn_copy_data_atom(rc->wt(srcatomi), rc->at(srcatomi)),
            rc->at(srcatomi), TT_OTHER};
      } else { /* symbol atom */
        fprintf(stderr, "symbol atom can't be created in GUARD\n");
        exit(EXIT_FAILURE);
      }
      break;
    }
    case INSTR_GETFUNC: {
      LmnInstrVar funci, atomi;

      READ_VAL(LmnInstrVar, instr, funci);
      READ_VAL(LmnInstrVar, instr, atomi);

      if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
        /* ここで得るファンクタはガード命令中で一時的に使われるだけなので
           double はポインタのコピーで十分なはず */
        rc->wt(funci) = rc->wt(atomi);
      } else {
        rc->wt(funci) = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)rc->wt(atomi));
      }
      rc->at(funci) = rc->at(atomi);
      rc->tt(funci) = TT_OTHER;
      break;
    }
    case INSTR_SETMEMNAME: {
      LmnInstrVar memi;
      lmn_interned_str name;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(lmn_interned_str, instr, name);
      dmem_root_set_mem_name(RC_ND_MEM_DELTA_ROOT(rc),
                             (LmnMembraneRef)rc->wt(memi), name);
      break;
    }
    case INSTR_COPYRULES: {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);

      dmem_root_copy_rules(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembraneRef)rc->wt(destmemi),
                           (LmnMembraneRef)rc->wt(srcmemi));
      break;
    }
    case INSTR_REMOVEPROXIES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                               (LmnMembraneRef)rc->wt( memi));
      break;
    }
    case INSTR_INSERTPROXIES: {
      LmnInstrVar parentmemi, childmemi;

      READ_VAL(LmnInstrVar, instr, parentmemi);
      READ_VAL(LmnInstrVar, instr, childmemi);
      dmem_root_insert_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                               (LmnMembraneRef)rc->wt( parentmemi),
                               (LmnMembraneRef)rc->wt( childmemi));
      break;
    }
    case INSTR_DELETECONNECTORS: {
      LmnInstrVar srcset, srcmap;
      HashSet *delset;
      ProcessTableRef delmap;
      HashSetIterator it;
      READ_VAL(LmnInstrVar, instr, srcset);
      READ_VAL(LmnInstrVar, instr, srcmap);

      delset = (HashSet *)rc->wt( srcset);
      delmap = (ProcessTableRef)rc->wt( srcmap);

      for (it = hashset_iterator(delset); !hashsetiter_isend(&it);
           hashsetiter_next(&it)) {
        LmnSymbolAtomRef orig, copy;
        LmnWord t;

        orig = (LmnSymbolAtomRef)hashsetiter_entry(&it);
        t = 0; /* warningを黙らす */
        proc_tbl_get_by_atom(delmap, orig, &t);
        copy = (LmnSymbolAtomRef)t;
        lmn_mem_unify_symbol_atom_args(orig, 0, orig, 1);
        lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);

        lmn_delete_atom(orig);
        lmn_delete_atom(copy);
      }

      if (delmap)
        proc_tbl_free(delmap);
      break;
    }
    case INSTR_REMOVETOPLEVELPROXIES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_toplevel_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                                        (LmnMembraneRef)rc->wt( memi));
      break;
    }
    case INSTR_ADDATOM: {
      LmnInstrVar memi, atomi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);
      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembraneRef)rc->wt( memi),
                          (LmnAtomRef)rc->wt( atomi), rc->at(atomi));
      break;
    }
    case INSTR_MOVECELLS: {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      LMN_ASSERT(rc->wt( destmemi) != rc->wt( srcmemi));
      dmem_root_move_cells(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembraneRef)rc->wt( destmemi),
                           (LmnMembraneRef)rc->wt( srcmemi));
      break;
    }
    case INSTR_REMOVETEMPORARYPROXIES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_temporary_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                                         (LmnMembraneRef)rc->wt( memi));
      break;
    }
    case INSTR_COPYCELLS: {
      LmnInstrVar mapi, destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, mapi);
      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      dmem_root_copy_cells(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembraneRef)rc->wt( destmemi),
                           (LmnMembraneRef)rc->wt( srcmemi));
      rc->tt(mapi) = TT_OTHER;
      break;
    }
    case INSTR_LOOKUPLINK: {
      LmnInstrVar destlinki, tbli, srclinki;

      READ_VAL(LmnInstrVar, instr, destlinki);
      READ_VAL(LmnInstrVar, instr, tbli);
      READ_VAL(LmnInstrVar, instr, srclinki);

      rc->at(destlinki) = LINKED_ATTR(srclinki);
      if (LMN_ATTR_IS_DATA(LINKED_ATTR(srclinki))) {
        rc->wt( destlinki) = (LmnWord)LINKED_ATOM(srclinki);
      } else { /* symbol atom */
        ProcessTableRef ht = (ProcessTableRef)rc->wt( tbli);
        LmnWord w = rc->wt( destlinki);
        proc_tbl_get_by_atom(ht, (LmnSymbolAtomRef)LINKED_ATOM(srclinki), &w);
        rc->wt( destlinki) = w;
      }
      break;
    }
    case INSTR_CLEARRULES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_clear_ruleset(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembraneRef)rc->wt( memi));
      vec_clear(lmn_mem_get_rulesets((LmnMembraneRef)rc->wt( memi)));

      break;
    }
    case INSTR_DROPMEM: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_drop(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembraneRef)rc->wt( memi));
      break;
    }
    case INSTR_LOOP: {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      while (dmem_interpret(rc, rule, instr))
        ;
      instr += subinstr_size;
      break;
    }
    case INSTR_CALLBACK: {
      LmnInstrVar memi, atomi;
      LmnSymbolAtomRef atom;
      const struct CCallback *c;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);

      atom = (LmnSymbolAtomRef)rc->wt( atomi);

      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0))) {
        LmnSymbolAtomRef f_name = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, 0);
        lmn_interned_str name =
            LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(f_name));
        int arity = LMN_FUNCTOR_ARITY(LMN_SATOM_GET_FUNCTOR(atom));

        c = get_ccallback(name);
        if (!c)
          break;

        if (arity - 1 != c->arity) {
          fprintf(stderr, "EXTERNAL FUNC: invalid arity - %s\n",
                  LMN_SYMBOL_STR(name));
          break;
        }

        lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi),
                            (LmnAtomRef)rc->wt( atomi), rc->at(atomi));
        lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi),
                            LMN_SATOM_GET_LINK(atom, 0),
                            LMN_SATOM_GET_ATTR(atom, 0));

        switch (arity) {
        case 1:
          ((callback_0)c->f)(rc, (LmnMembraneRef)rc->wt( memi));
          break;
        case 2:
          ((callback_1)c->f)(rc, (LmnMembraneRef)rc->wt( memi),
                             LMN_SATOM_GET_LINK(atom, 1),
                             LMN_SATOM_GET_ATTR(atom, 1));
          break;
        case 3:
          ((callback_2)c->f)(
              rc, (LmnMembraneRef)rc->wt( memi), LMN_SATOM_GET_LINK(atom, 1),
              LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
              LMN_SATOM_GET_ATTR(atom, 2));
          break;
        case 4:
          ((callback_3)c->f)(
              rc, (LmnMembraneRef)rc->wt( memi), LMN_SATOM_GET_LINK(atom, 1),
              LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
              LMN_SATOM_GET_ATTR(atom, 2), LMN_SATOM_GET_LINK(atom, 3),
              LMN_SATOM_GET_ATTR(atom, 3));
          break;
        case 5:
          ((callback_4)c->f)(
              rc, (LmnMembraneRef)rc->wt( memi), LMN_SATOM_GET_LINK(atom, 1),
              LMN_SATOM_GET_ATTR(atom, 1), LMN_SATOM_GET_LINK(atom, 2),
              LMN_SATOM_GET_ATTR(atom, 2), LMN_SATOM_GET_LINK(atom, 3),
              LMN_SATOM_GET_ATTR(atom, 3), LMN_SATOM_GET_LINK(atom, 4),
              LMN_SATOM_GET_ATTR(atom, 4));
          break;
        default:
          printf("EXTERNAL FUNCTION: too many arguments\n");
          break;
        }
      }

      break;
    }
    default:
      fprintf(stderr, "interpret: Unknown operation %d\n", op);
      exit(1);
    }
    /*     lmn_dump_mem((LmnMembraneRef)rc->wt( 0)); */
    /*     print_wt(); */

#ifdef DEBUG
    /*     print_wt(); */
#endif
  }
}

Vector *links_from_idxs(const Vector *link_idxs, LmnReactCxtRef rc) {
  unsigned long i;
  Vector *vec = vec_make(16);

  /* リンクオブジェクトのベクタを構築 */
  for (i = 0; i < vec_num(link_idxs); i++) {
    vec_data_t t = vec_get(link_idxs, i);
    LinkObjRef l =
        LinkObj_make((LmnAtomRef)rc->wt(t), rc->at(t));
    vec_push(vec, (LmnWord)l);
  }
  return vec;
}

void free_links(Vector *links) {
  unsigned long i;

  for (i = 0; i < vec_num(links); i++) {
    LMN_FREE(vec_get(links, i));
  }
  vec_free(links);
}
