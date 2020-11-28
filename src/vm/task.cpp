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
#include "normal_thread.h"
#include "special_atom.h"
#include "symbol.h"
#include "verifier/runtime_status.h"
#include "verifier/verifier.h"

#ifdef USE_FIRSTCLASS_RULE
#include "firstclass_rule.h"
#endif

#include <algorithm>
// #include <execution>
#include <thread>
#include <mutex>
#include <iostream>
#include <sstream>
#include <stdio.h>

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

static inline BOOL react_ruleset(LmnReactCxtRef rc, LmnMembraneRef mem,
                                 LmnRuleSetRef ruleset, int ti=0);
static inline void react_initial_rulesets(LmnReactCxtRef rc,
                                          LmnMembraneRef mem);
static inline BOOL react_ruleset_in_all_mem(LmnReactCxtRef rc, LmnRuleSetRef rs,
                                            LmnMembraneRef mem);
static BOOL dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule,
                           LmnRuleInstr instr);

static void mem_oriented_loop(MemReactContext *ctx, LmnMembraneRef mem);

void lmn_dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule,
                        LmnRuleInstr instr) {
  dmem_interpret(rc, rule, instr);
}

namespace c14 = slim::element;
namespace c17 = slim::element;

// int tnum = 10;

std::mutex mut;
// std::vector<std::unique_ptr<std::mutex>> tmut;




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
    mrc = c14::make_unique<MemReactContext>(nullptr);

#ifdef USE_FIRSTCLASS_RULE
  first_class_rule_tbl_init();
#endif

  /* 通常実行では非決定実行とは異なりProcess IDを
   * 1から再割り当てする機会(状態圧縮と復元)が存在しない.
   * 破棄したProcessのIDを使い回す必要がある.
   * もし通常実行の並列化を考えるならばIDを再利用するためのMT-Safeな機構が必要
   */
  if (!env_proc_id_pool()) {
    env_set_proc_id_pool(new Vector(64));
  }

  /* interactive: normal_cleaningフラグがONの場合は後始末 */
  if (lmn_env.normal_cleaning) {
    mem->drop();
    delete mem;
    mrc = nullptr;
    lmn_env.normal_cleaning = FALSE;
  }

  /* interactive : (normal_remain時でnormal_remaining=ON)以外の場合は初期化 */
  if (!lmn_env.normal_remain && !lmn_env.normal_remaining) {
    mem = new LmnMembrane();
    mrc = c14::make_unique<MemReactContext>(mem);
  }
  mrc->memstack_push(mem);

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
  mrc->memstack_reconstruct(mem);

  if (lmn_env.trace) {
    if (lmn_env.output_format != JSON) {
      mrc->increment_reaction_count();
      fprintf(stdout, "%d: ", mrc->get_reaction_count());
    }
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
    mem->drop();
    delete mem;
    mrc = nullptr;
  }
  if (env_proc_id_pool()) {
    delete env_proc_id_pool();
  }
}

/** 膜スタックに基づいた通常実行 */
// ここを並列化できるかも
static void mem_oriented_loop(MemReactContext *ctx, LmnMembraneRef mem) {
  // std::for_each(std::execution::par, ctx->memstack_first(), ctx->memstack_peek(), [&](MemReactContext *ctx, LmnMembraneRef &mem_cur){
  //   if (!react_all_rulesets(ctx, mem_cur)) {
  //     /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //     ctx->memstack_pop();
  //   }
  // });

  // std::for_each(ctx->memstack_begin(), ctx->memstack_end(), [&](MemReactContext *ctx, LmnMembraneRef mem_cur){
  //   if (!react_all_rulesets(ctx, mem_cur)) {
  //     /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //     ctx->memstack_pop();
  //   }
  // });

  // int tnum = 4;
  // std::vector<std::thread> ts(tnum);

  // int sz = ctx->get_size();

  // std::cout << "size: " << sz << std::endl;
  // int compsz = sz/tnum;

  // auto react = [tnum, compsz, sz](MemReactContext *ctx, int ti){
    
  //   int beg = compsz * ti;
  //   int en  = compsz * (ti+1);
  //   if(ti == 0) beg=1;
  //   if(ti == tnum - 1) en = sz-1;

  //   for (int i=beg;i<en;i++){

  //     std::cout << i << std::endl;
      
  //     LmnMembraneRef mem = ctx->get_ith_mem(i);
  //     if (!react_all_rulesets(ctx, mem)) {
  //       /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //       // ctx->memstack_pop();
  //     }
  //   }
  // };

  // for(int i=0;i<tnum;i++){
  //   ts[i] = std::thread(react,ctx,i);
  // }
  // for(int i=0;i<tnum;i++){
  //   ts[i].join();
  // }

  // ctx->memstack_clear();

  /* スレッド使用2 */
  
  // auto react = [&](){
  //   if(ctx->memstack_isempty())
  //     return;

  //   LmnMembraneRef mem = ctx->memstack_peek();

  //   if (!react_all_rulesets(ctx, mem)) {
  //     /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //     ctx->memstack_pop();
  //   }
  // };

  // while (!ctx->memstack_isempty()) {
  //   int tnum = 1;
  //   std::vector<std::thread> ts(tnum);

  //   for(int i=0;i<tnum;i++){
  //     ts[i] = std::thread(react);
  //   }
  //   for(int i=0;i<tnum;i++){
  //     ts[i].join();
  //   }
  // }

  /* ルールセット調査 */
  // while (!ctx->memstack_isempty()) {
  //   LmnMembraneRef mem = ctx->memstack_peek();

  //   BOOL reacted = react_all_rulesets(ctx,mem);

  //   if(reacted)
  //     std::cout << "reacted" << std::endl;
  //   else{
  //     std::cout << "not reacted" << std::endl;
  //   }

  //   if (!reacted) {
  //     /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //     ctx->memstack_pop();
  //   }
  // }

  /* react調査 */

  int tnum = 10; 

  auto react = [&](MemReactContext *ctx, LmnMembraneRef m, int ti){
    BOOL reacted = false;
      do{
        reacted = react_all_rulesets(ctx,m,ti);
        // if(reacted)
        //   std::cout << "reacted" << std::endl;
        // else{
        //   std::cout << "not reactfed" << std::endl;1
        // }
      }while(reacted);
  };

  // グローバルルート膜対策
  LmnMembraneRef gmem = ctx->memstack_pop();
  // MemReactContext *ctx_copied = new MemReactContext(gmem, REACT_MEM_ORIENTED);
  // LmnReactCxt *rc_copied = new LmnReactCxt(*rc);
  // MemReactContext ctx_copied = MemReactContext(mem);
  react(ctx, gmem, 0);

  while (!ctx->memstack_isempty()) {

    // 後でdelete出来るように保存しておく
    std::vector<MemReactContext *> ctx_copied_vec;

    int cnt = 0;
    std::vector<std::thread> ts(tnum);
    for(int i=0;i<tnum;i++){
      if(ctx->memstack_isempty())
        break;
      cnt++;
      LmnMembraneRef mem = ctx->memstack_pop();

      // ctxをコピー
      MemReactContext *ctx_copied = new MemReactContext(*ctx);
      ctx_copied_vec.push_back(ctx_copied);
      // LmnReactCxt *rc_copied = new LmnReactCxt(*rc);
      // MemReactContext ctx_copied = MemReactContext(mem);
      ts[i] = std::thread(react, ctx_copied, mem, i);
    }
    // std::cout << "cnt :" << cnt << std::endl; 
    // for(int j=0;j<tnum;j++){
    for(int j=0;j<cnt;j++){
      ts[j].join();
      delete ctx_copied_vec[j];
    }
    // std::cout << "ok" << std::endl;
  }


  /* 元コード */
  // while (!ctx->memstack_isempty()) {
  //   LmnMembraneRef mem = ctx->memstack_peek();

  //   if (!react_all_rulesets(ctx, mem)) {
  //     /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
  //     ctx->memstack_pop();
  //   }
  // }


}

/**
 * @brief 膜内の0stepルールセットを適用できるだけ適用する
 */
bool react_zerostep_rulesets(LmnReactCxtRef rc, LmnMembraneRef cur_mem) {
  auto &rulesets = cur_mem->get_rulesets();
  BOOL reacted = FALSE;
  bool reacted_any = false;

  rc->is_zerostep = true;
  do {
    reacted = FALSE;
    for (int i = 0; i < rulesets.size(); i++) {
      LmnRuleSetRef rs = rulesets[i];
      if (!rs->is_zerostep())
        continue;
      reacted |= react_ruleset(rc, cur_mem, rs);
    }
    reacted_any |= reacted;
  } while (reacted);
  rc->is_zerostep = false;

  return reacted_any;
}

/**
 * @brief 膜内の子膜に再帰的に0stepルールセットを適用する
 * @sa react_zerostep_rulesetsm
 */
void react_zerostep_recursive(LmnReactCxtRef rc, LmnMembraneRef cur_mem) {
  for (; cur_mem; cur_mem = cur_mem->mem_next()) {
    react_zerostep_recursive(rc, cur_mem->mem_child_head());
    react_zerostep_rulesets(rc, cur_mem);
  }
}

/** cur_memに存在するルールすべてに対して適用を試みる
 * @see mem_oriented_loop (task.c)
 * @see expand_inner      (nd.c) */
BOOL react_all_rulesets(LmnReactCxtRef rc, LmnMembraneRef cur_mem, int ti) {
  unsigned int i;
  auto &rulesets = cur_mem->get_rulesets(); /* 本膜のルールセットの集合 */
  BOOL ok = FALSE;

  /* ルールセットの適用 */
  for (i = 0; i < rulesets.size(); i++) {
    if (react_ruleset(rc, cur_mem, rulesets[i], ti)) {
      /* ndでは失敗するまでマッチングバックトラックしているので必ずFALSEが返ってくる
       */
      ok = TRUE;
      break;
    }
  }

#ifdef USE_FIRSTCLASS_RULE
  for (i = 0; i < (cur_mem->firstclass_rulesets())->get_num(); i++) {
    if (react_ruleset(
            rc, cur_mem,
            (LmnRuleSetRef)(cur_mem->firstclass_rulesets())->get(i))) {
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
                                 LmnRuleSetRef rs, int ti) {
  for (auto r : *rs) {
    
#ifdef PROFILE
    if (!lmn_env.nd && lmn_env.profile_level >= 2)
      profile_rule_obj_set(rs, r);
#endif
    // std::cout << "rule start [" << ti << "] " << std::endl;
    // mut.lock();
    BOOL reacted = react_rule(rc, mem, r, ti);
    // mut.unlock();
    // std::cout << "rule end [" << ti << "] " << std::endl;
    if (reacted)
      return true;
  }
  return false;
}

/** 膜memに対してルールruleの適用を試みる.
 *  戻り値:
 *   通常実行では, 書換えに成功した場合にTRUE,
 * マッチングしなかった場合にFALSEを返す. 非決定実行では,
 * マッチングに失敗するまでバックトラックを繰り返すため常にFALSEが返る. */
BOOL react_rule(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule, int ti) {
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

  // std::cout << "start translated [" << ti << "] " << std::endl;
  // mut.lock();
  result = (translated && translated(rc, mem, rule)) || (inst_seq && in.run(ti));
  // mut.unlock();
  // std::cout << "end translated [" << ti << "] " << std::endl;


  if (lmn_env.enable_parallel && !lmn_env.nd && normal_parallel_flag)
    rule_wall_time_finish();

  /* 適用に成功したら0step実行に入る。既に入っていれば何もしない */
  if (result && !rc->is_zerostep) {
    // ここも関係ない
    bool reacted = react_zerostep_rulesets(rc, mem);
    if (reacted && rc->has_mode(REACT_MEM_ORIENTED)) {
      // zerostep中に生成した膜が消えたりすると膜スタック中の膜が解放されメモリアクセス違反になるので膜スタックを作り直す
      // 反応開始時点ではmemが膜スタックの先頭にあるはずなのでmem以下だけ作り直せばいいかも？
      ((MemReactContext *)rc)->memstack_reconstruct(rc->get_global_root());
    }
  }

  profile_finish_trial();

  if (rc->has_mode(REACT_MEM_ORIENTED) && !rc->is_zerostep) {
    // std::cout << "reacting!" << std::endl;
    if (lmn_env.trace && result) {
      if (lmn_env.sp_dump_format == LMN_SYNTAX) {
        lmn_dump_mem_stdout(rc->get_global_root());
        fprintf(stdout, ".\n");
        lmn_dump_mem_stdout(rc->get_global_root());
        fprintf(stdout, ":- ");
        rc->increment_reaction_count();
      } else if (lmn_env.output_format == JSON) {
        lmn_dump_cell_stdout(rc->get_global_root());
      } else {
        fprintf(stdout, "---->%s\n", lmn_id_to_name(rule->name));
        rc->increment_reaction_count();
        fprintf(stdout, "%d: ", rc->get_reaction_count());
        lmn_dump_cell_stdout(rc->get_global_root());
        if (lmn_env.show_hyperlink)
          lmn_hyperlink_print(rc->get_global_root());
      }
    }
  }

  // ここは実行されない
  if (rc->get_hl_sameproccxt()) {
    rc->clear_hl_spc(); /* とりあえずここに配置 */
    // normal parallel destroy
    if (lmn_env.enable_parallel && !lmn_env.nd) {
      int i;
      for (i = 0; i < lmn_env.core_num; i++) {
        thread_info[i]->rc->clear_hl_spc();
      }
    }
  }

  return result;
}

/* 膜memでrulesetsのルールの適用を行う.
 * 適用結果は無視する */
void react_start_rulesets(LmnMembraneRef mem, Vector *rulesets) {
  LmnReactCxt rc(mem, REACT_STAND_ALONE);
  int i;

  for (i = 0; i < rulesets->get_num(); i++) {
    react_ruleset(&rc, mem, (LmnRuleSetRef)rulesets->get(i));
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

  for (m = mem->mem_child_head(); m; m = m->mem_next()) {
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
      (dest) = (LmnAtomRef) new LmnString(lmn_id_to_name(s));                  \
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
      str1 = new LmnString(lmn_id_to_name(s));                                 \
      (result) = *str1 == *(LmnStringRef)(x);                                  \
      delete (str1);                                                           \
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
  for (i = 0; i < links->get_num(); i++) {
    LmnWord linkid1 = links->get(i);
    if (LMN_ATTR_IS_DATA(rc->at(linkid1)))
      continue;
    for (j = i + 1; j < links->get_num(); j++) {
      LmnWord linkid2 = links->get(j);
      if (LMN_ATTR_IS_DATA(rc->at(linkid2)))
        continue;
      /* is buddy? */
      if ((LmnAtomRef)rc->wt(linkid2) == ((LmnSymbolAtomRef)rc->wt(linkid1))
                                      ->get_link(rc->at(linkid1)) &&
          rc->at(linkid2) == ((LmnSymbolAtomRef)rc->wt(linkid1))
                                      ->get_attr(rc->at(linkid1))) {
        /* '='アトムをはさむ */
        LmnSymbolAtomRef eq;
        LmnSymbolAtomRef a1, a2;
        LmnLinkAttr t1, t2;

        if (mem)
          eq = lmn_mem_newatom(mem, LMN_UNIFY_FUNCTOR);
        else {
          eq = lmn_new_atom(LMN_UNIFY_FUNCTOR);
        }

        if (eq->get_id() == 0) {
          eq->set_id(env_gen_next_id());
        }

        /* リンクがリンクの元を持つ場合、あらかじめリンク先の取得をしていなければならない。
         * リンク元はnew_link時に書き換えられてしまう。*/

        a1 = (LmnSymbolAtomRef)rc->wt(linkid1);
        a2 = (LmnSymbolAtomRef)rc->wt(linkid2);
        t1 = rc->at(linkid1);
        t2 = rc->at(linkid2);

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
  

  // std::stringstream ss1;
  // ss1 << "1: " << mem->get_atomlist(f)->size() << " " << mem;
  // std::cout << ss1.str() << std::endl;

  // printf("1: %d %d\n",mem->get_atomlist(f)->size(), mem);

  auto atomlist_ent = mem->get_atomlist(f);

  if (!atomlist_ent)
    return;

  mut.lock();
  auto iter = std::begin(*atomlist_ent);
  auto end = std::end(*atomlist_ent);
  if (iter == end){
    mut.unlock();
    return;
  }

  auto v = std::vector<LmnRegister>(atomlist_ent->size());
  
  std::transform(iter, end, v.begin(), [](LmnSymbolAtomRef atom) {
    return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
  });

  // mut.lock();
  this->false_driven_enumerate(reg, std::move(v));
  mut.unlock();

  // std::stringstream ss2;
  // ss2 << "2: " << mem->get_atomlist(f)->size() << " " << mem;
  // std::cout << ss2.str() << std::endl;
  // mut.unlock();

  // printf("2: %d %d\n",mem->get_atomlist(f)->size(), mem);
}

/** find atom with a hyperlink occurred in the current rule for the first time.
 */
void slim::vm::interpreter::findatom_original_hyperlink(
    LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr, SameProcCxt *spc,
    LmnMembrane *mem, LmnFunctor f, size_t reg) {
  auto atomlist_ent = mem->get_atomlist(f);
  if (!atomlist_ent)
    return;

  auto filtered = slim::element::make_range_remove_if(
      std::begin(*atomlist_ent), std::end(*atomlist_ent),
      [=](LmnSymbolAtomRef atom) { return !spc->is_consistent_with(atom); });
  auto v = std::vector<std::function<LmnRegister(void)>>();
  std::transform(
      filtered.begin(), filtered.end(), std::back_inserter(v),
      [=](LmnSymbolAtomRef atom) {
        return [=]() {
          spc->match(atom);
          return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
        };
      });

  this->false_driven_enumerate(reg, std::move(v));
}

void lmn_hyperlink_get_elements(std::vector<HyperLink *> &tree,
                                HyperLink *start_hl) {
  Vector vec;
  vec.init(1);
  start_hl->get_elements(&vec);
  for (int i = 0; i < vec.get_num(); i++)
    tree.push_back((HyperLink *)vec.get(i));
  vec.destroy();
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

  if (!mem->get_atomlist(LMN_HL_FUNC))
    return;

  auto filtered = slim::element::make_range_remove_if(
      children.begin(), children.end(), [=](HyperLink *h) {
        auto linked_pc = h->atom;
        auto hl_atom = (LmnSymbolAtomRef)linked_pc->get_link(0);
        /*    本来findatomするはずだったファンクタと一致しているか
         * || hyperlinkアトムの接続先の引数が正しいか
         * || 本来findatomするはずだったアトムと所属膜が一致しているか
         */
        return hl_atom->get_functor() != f ||
               spc->start_attr != linked_pc->get_attr(0) ||
               LMN_HL_MEM(lmn_hyperlink_at_to_hl(linked_pc)) != mem ||
               !spc->is_consistent_with(hl_atom);
      });
  std::vector<std::function<LmnRegister(void)>> v;
  std::transform(
      filtered.begin(), filtered.end(), std::back_inserter(v),
      [=](HyperLink *h) {
        return [=]() {
          auto atom = (LmnSymbolAtomRef)h->atom->get_link(0);
          spc->match(atom);
          return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
        };
      });

  this->false_driven_enumerate(reg, std::move(v));
}

/** hyperlinkの接続関係からfindatom */
void slim::vm::interpreter::findatom_through_hyperlink(
    LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr, SameProcCxt *spc,
    LmnMembrane *mem, LmnFunctor f, size_t reg) {
  auto atom_arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);

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
  auto v1 = new Vector(attr_dataAtoms.size());
  auto v2 = new Vector(attr_dataAtom_attrs.size());
  auto p1 = new ProcessTbl(attr_functors.size());

  for (auto &v : attr_dataAtoms)
    v1->push(v);
  for (auto &v : attr_dataAtom_attrs)
    v2->push(v);
  for (auto &p : attr_functors)
    p1->proc_tbl_put(p, p);

  auto a = atoms.get();
  auto h = hlinks.get();
  auto result = ground_atoms(srcvec, avovec, &a, natoms, &h, &p1, v1, v2);
  atoms = std::unique_ptr<ProcessTbl>(a);
  hlinks = std::unique_ptr<ProcessTbl>(h);
  delete v1;
  delete v2;
  delete p1;
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
      args.push_back(((LmnSymbolAtomRef)rc->wt(ai))->get_functor());
    }
  }
  return args;
}

std::vector<HyperLink *> lmn_hyperlink_get_elements(HyperLink *start_hl) {
  std::vector<HyperLink *> vec;
  Vector tree;
  tree.init(32);
  start_hl->get_elements(&tree);
  for (int i = 0; i < tree.get_num() - 1; i++)
    vec.push_back((HyperLink *)tree.get(i));
  tree.destroy();
  return vec;
}

struct exec_subinstructions_while {
  bool executed; // 一度whileループを抜けたらtrue
  LmnRuleInstr body;
  LmnRuleInstr next;

  exec_subinstructions_while(LmnRuleInstr body, LmnRuleInstr next)
      : executed(false), body(body), next(next) {}

  slim::vm::interpreter::command_result
  operator()(slim::vm::interpreter &interpreter, bool result) {
    if (executed) // すでに実行が終わっていて戻ってきた
      return result ? slim::vm::interpreter::command_result::Success
                    : slim::vm::interpreter::command_result::Failure;

    if (result) {
      interpreter.instr = body;
    } else {
      interpreter.instr = next;
      executed = true;
    }
    return slim::vm::interpreter::command_result::Trial;
  }
};

struct exec_subinstructions_not {
  bool executed;
  LmnRuleInstr next_instr;

  exec_subinstructions_not(LmnRuleInstr next_instr)
      : executed(false), next_instr(next_instr) {}

  slim::vm::interpreter::command_result
  operator()(slim::vm::interpreter &interpreter, bool result) {
    if (!executed) {
      // サブ命令列で成功が返ったら失敗
      if (result)
        return slim::vm::interpreter::command_result::Failure;

      // うまくいったので次の命令へ飛ぶ
      interpreter.instr = next_instr;
      executed = true;
      return slim::vm::interpreter::command_result::Trial;
    } else {
      return result ? slim::vm::interpreter::command_result::Success
                    : slim::vm::interpreter::command_result::Failure;
    }
  }
};

struct exec_subinstructions_group {
  bool executed;
  LmnRuleInstr next_instr;

  exec_subinstructions_group(LmnRuleInstr next_instr)
      : executed(false), next_instr(next_instr) {}

  slim::vm::interpreter::command_result
  operator()(slim::vm::interpreter &interpreter, bool result) {
    if (!executed) {
      // サブ命令列で失敗が返ったら失敗
      if (!result)
        return slim::vm::interpreter::command_result::Failure;

      // うまくいったので次の命令へ飛ぶ
      interpreter.instr = next_instr;
      executed = true;
      return slim::vm::interpreter::command_result::Trial;
    } else {
      return result ? slim::vm::interpreter::command_result::Success
                    : slim::vm::interpreter::command_result::Failure;
    }
  }
};

struct exec_subinstructions_branch {
  bool executed;
  LmnRuleInstr next_instr;

  exec_subinstructions_branch(LmnRuleInstr next_instr)
      : executed(false), next_instr(next_instr) {}

  slim::vm::interpreter::command_result
  operator()(slim::vm::interpreter &interpreter, bool result) {
    if (!executed) {
      // サブ命令列で成功が返ったら成功
      if (result)
        return slim::vm::interpreter::command_result::Success;

      // うまくいったので次の命令へ飛ぶ
      interpreter.instr = next_instr;
      executed = true;
      return slim::vm::interpreter::command_result::Trial;
    } else {
      return result ? slim::vm::interpreter::command_result::Success
                    : slim::vm::interpreter::command_result::Failure;
    }
  }
};

/**
 *  execute a command at instr.
 *  instr is incremented by the size of operation.
 *  returns true if execution finished sucessfully.
 *  stop becomes true only if executien should be aborted.
 */
bool slim::vm::interpreter::exec_command(LmnReactCxt *rc, LmnRuleRef rule,
                                         bool &stop) {
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr, op);
  stop = true;

  if (lmn_env.find_atom_parallel)
    return FALSE;

  // std::cout << op << std::endl;

  switch (op) {
  case INSTR_SPEC: {
    LmnInstrVar s0;

    SKIP_VAL(LmnInstrVar, instr);
    READ_VAL(LmnInstrVar, instr, s0);

    // mut.lock();
    rc->resize(s0);
    // mut.unlock();
    break;
  }
  case INSTR_INSERTCONNECTORSINNULL: {
    LmnInstrVar seti, list_num;
    Vector links;
    unsigned int i;

    READ_VAL(LmnInstrVar, instr, seti);
    READ_VAL(LmnInstrVar, instr, list_num);

    links.init(list_num + 1);
    for (i = 0; i < list_num; i++) {
      LmnInstrVar t;
      READ_VAL(LmnInstrVar, instr, t);
      links.push((LmnWord)t);
    }

    auto hashset = insertconnectors(rc, NULL, &links);
    rc->reg(seti) = {(LmnWord)hashset, 0, TT_OTHER};

    links.destroy();

    /* EFFICIENCY: 解放のための再帰 */
    this->push_stackframe([=](interpreter &itr, bool result) {
      delete hashset;
      return result ? command_result::Success : command_result::Failure;
    });

    break;
  }
  case INSTR_INSERTCONNECTORS: {
    LmnInstrVar seti, list_num, memi, enti;
    Vector links; /* src list */
    unsigned int i;

    READ_VAL(LmnInstrVar, instr, seti);
    READ_VAL(LmnInstrVar, instr, list_num);

    links.init(list_num + 1);

    for (i = 0; i < list_num; i++) {
      READ_VAL(LmnInstrVar, instr, enti);
      links.push((LmnWord)enti);
    }

    READ_VAL(LmnInstrVar, instr, memi);
    auto hashset = insertconnectors(rc, (LmnMembraneRef)rc->wt(memi), &links);
    rc->reg(seti) = {(LmnWord)hashset, 0, TT_OTHER};

    links.destroy();

    /* EFFICIENCY: 解放のための再帰 */
    this->push_stackframe([=](interpreter &itr, bool result) {
      delete hashset;
      return result ? command_result::Success : command_result::Failure;
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

    mut.lock();
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

    this->push_stackframe([=](interpreter &itr, bool result) {
      rc->warray_set(std::move(*tmp));
      delete tmp;
      return result ? command_result::Success : command_result::Failure;
    });
    mut.unlock();
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

    // mut.lock();
    rule->name = rule_name;
    // mut.unlock();

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
    if (rc->has_mode(REACT_ND) && !rc->is_zerostep) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      ProcessID org_next_id = env_next_id();

      if (mcrc->has_optmode(DeltaMembrane)) {
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        /** >>>>>>>> enable delta-membrane <<<<<<< **/
        /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
        struct MemDeltaRoot *d =
            new MemDeltaRoot(rc->get_global_root(), rule, env_next_id());
        RC_ND_SET_MEM_DELTA_ROOT(rc, d);

        /* dmem_commit/revertとの整合性を保つため,
         * uniq処理の特殊性を吸収しておく */
        rule->undo_history();

        if (mcrc->has_optmode(DynamicPartialOrderReduction)) {
          dpor_transition_gen_LHS(RC_POR_DATA(rc), d, rc);
        }

        dmem_interpret(rc, rule, instr);
        dmem_root_finish(d);

        if (mcrc->has_optmode(DynamicPartialOrderReduction)) {
          if (!dpor_transition_gen_RHS(RC_POR_DATA(rc), d, mcrc)) {
            delete d;
          } else {
            mc_react_cxt_add_mem_delta(mcrc, d, rule);
          }

          /* サクセッサへの差分オブジェクトが複数できあがることになるが,
           * 差分オブジェクト間では生成したプロセスのIDに重複があってはならない.
           */
          RC_ND_SET_MEM_DELTA_ROOT(rc, NULL);
          return FALSE;
        }

        mc_react_cxt_add_mem_delta(mcrc, d, rule);
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

        tmp_global_root = lmn_mem_copy_with_map_ex(rc->get_global_root(), &copymap);

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
                  r->register_set_wt((LmnWord)((HyperLink *)t)->hl_to_at());
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
                (LmnWord)rc->get_global_root()) { /* グローバルルート膜 */
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
        delete copymap;

        /** 変数配列および属性配列をコピーと入れ換え, コピー側を書き換える */
        auto tmp = new std::vector<LmnRegister>(std::move(rc->work_array));
        rc->warray_set(std::move(v));

#ifdef PROFILE
        if (lmn_env.profile_level >= 3) {
          profile_finish_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
        }
#endif

        this->push_stackframe([=](interpreter &itr, bool result) {
          react_zerostep_recursive(
              rc, tmp_global_root); /**< 0stepルールを適用する */
          mc_react_cxt_add_expanded(mcrc, tmp_global_root, rule);

          rule->undo_history();

          rc->warray_set(std::move(*tmp));
          if (!rc->keep_process_id_in_nd_mode)
            env_set_next_id(org_next_id);
          delete tmp;
          return command_result::Failure;
        });
        break;
      }

      return FALSE; /* matching backtrack! */
    } else if (rc->has_mode(REACT_PROPERTY)) {
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
      if (!rc->get_hl_sameproccxt())
        rc->prepare_hl_spc();
      auto spc =
          (SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(), (HashKeyType)atomi);
      findatom_through_hyperlink(rc, rule, instr, spc, mem, f, atomi);
    } else {
      // mut.lock();
      findatom(rc, rule, instr, mem, f, atomi);
      // mut.unlock();
    }

    return false; // false driven loop
  }
  case INSTR_FINDATOM2: {
    LmnInstrVar atomi, memi, findatomid;
    LmnLinkAttr attr;

    if (rc->has_mode(REACT_ND) && !rc->is_zerostep) {
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
      atomlist_ent = ((LmnMembraneRef)rc->wt(memi))->get_atomlist(f);
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
            return (*atom)->get_functor() == LMN_RESUME_FUNCTOR;
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

      this->false_driven_enumerate(atomi, std::move(candidates));

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

      this->false_driven_enumerate(atomi, std::move(v));

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

      auto atom_arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);

      if (rc_hlink_opt(atomi, rc)) {
        SameProcCxt *spc;

        if (!rc->get_hl_sameproccxt()) {
          rc->prepare_hl_spc();
        }

        /* 型付きプロセス文脈atomiがoriginal/cloneのどちらであるか判別 */
        spc = (SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(), (HashKeyType)atomi);
        if (spc->is_clone(atom_arity)) {
          lmn_fatal(
              "Can't use hyperlink searching in parallel-runtime mode.\n");
        }
      }
      auto atomlist_ent = ((LmnMembraneRef)rc->wt(memi))->get_atomlist(f);
      if (atomlist_ent)
        return false;
      ///
      int ip;
      LmnInstrVar i;
      BOOL judge;
      LmnSymbolAtomRef atom;

      normal_parallel_flag = TRUE;

      while (!temp->is_empty()) {
        ip = (int)(temp->pop_head());
        atom = (LmnSymbolAtomRef)thread_info[ip]->rc->wt(atomi);
        if (check_exist(atom, f)) {
          rc->reg(atomi) = {(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM};
          if (rc_hlink_opt(atomi, rc)) {
            auto spc = (SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(),
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
           atom = atom->get_next(), ip++) {
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
          temp->push_head(ip2);
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
        ((LmnSymbolAtomRef)(rc->wt(atomi)))->get_functor()));
    //      LMN_ASSERT(((LmnMembraneRef)wt(rc, memi))->parent);

    m = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)rc->wt(atomi));
    if (m->NAME_ID() != memn)
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
        [=](LmnMembrane &m) { return (&m)->NAME_ID() != memn; });
    std::vector<LmnRegister> v;
    for (auto &m : filtered)
      v.push_back(LmnRegister({(LmnWord)&m, 0, TT_MEM}));

    this->false_driven_enumerate(mem1, std::move(v));
    return false;
  }
  case INSTR_NMEMS: {
    LmnInstrVar memi, nmems;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, nmems);

    if (!((LmnMembraneRef)rc->wt(memi))->nmems(nmems)) {
      return FALSE;
    }

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NMEMS);
        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NMEMS);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
    }

    break;
  }
  case INSTR_NORULES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    if (!((LmnMembraneRef)rc->wt(memi))->get_rulesets().empty())
      return FALSE;

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NORULES);
        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NORULES);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
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
      mut.lock();
      ap = lmn_new_atom(f);
      mut.unlock();

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

    if (!((LmnMembraneRef)rc->wt(memi))->natoms(natoms)) {
      return FALSE;
    }

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NATOMS);
        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NATOMS);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
    }

    break;
  }
  case INSTR_NATOMSINDIRECT: {
    LmnInstrVar memi, natomsi;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, natomsi);

    if (!((LmnMembraneRef)rc->wt(memi))->natoms(rc->wt(natomsi))) {
      return FALSE;
    }

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NATOMS);
        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NATOMS);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
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

    mut.lock();

    attr1 = rc->at(link1);
    attr2 = rc->at(link2);

    if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {
      if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 1, 2 are data */
        lmn_mem_link_data_atoms((LmnMembraneRef)rc->wt(mem),
                                (LmnAtomRef)rc->wt(link1), rc->at(link1),
                                (LmnAtomRef)rc->wt(link2), attr2);
      } else { /* 1 is data */
        ((LmnSymbolAtomRef)rc->wt(link2))
            ->set_link(LMN_ATTR_GET_VALUE(attr2), (LmnAtomRef)rc->wt(link1));
        ((LmnSymbolAtomRef)rc->wt(link2))
            ->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);
      }
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 2 is data */
      ((LmnSymbolAtomRef)rc->wt(link1))
          ->set_link(LMN_ATTR_GET_VALUE(attr1), (LmnAtomRef)rc->wt(link2));
      ((LmnSymbolAtomRef)rc->wt(link1))
          ->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);
    } else { /* 1, 2 are symbol atom */

      if (LMN_ATTR_IS_EX(attr1)) {
        if (LMN_ATTR_IS_EX(attr2)) { /* 1, 2 are ex */
          lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),
                              (LmnSymbolAtomRef)rc->wt(link1), attr1,
                              0, // ex atom ⊂ unary atom
                              (LmnSymbolAtomRef)rc->wt(link2), attr2, 0);
        } else { /* 1 is ex */
          lmn_newlink_with_ex(
              (LmnMembraneRef)rc->wt(mem), (LmnSymbolAtomRef)rc->wt(link1),
              attr1, 0, (LmnSymbolAtomRef)rc->wt(link2), attr2, attr2);
        }
      } else if (LMN_ATTR_IS_EX(attr2)) { /* 2 is ex */
        lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),
                            (LmnSymbolAtomRef)rc->wt(link1), attr1, attr1,
                            (LmnSymbolAtomRef)rc->wt(link2), attr2, 0);
      } else {
        ((LmnSymbolAtomRef)rc->wt(link1))
            ->set_link(LMN_ATTR_GET_VALUE(attr1), (LmnAtomRef)rc->wt(link2));
        ((LmnSymbolAtomRef)rc->wt(link2))
            ->set_link(LMN_ATTR_GET_VALUE(attr2), (LmnAtomRef)rc->wt(link1));
        ((LmnSymbolAtomRef)rc->wt(link1))
            ->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);
        ((LmnSymbolAtomRef)rc->wt(link2))
            ->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);
      }
    }
    mut.unlock();
    break;
  }
  case INSTR_NEWLINK: {
    LmnInstrVar atom1, atom2, pos1, pos2, memi;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos2);
    READ_VAL(LmnInstrVar, instr, memi);

    // TODO(Fukui): 今後atom1と2をそれぞれロックする
    mut.lock();
    lmn_mem_newlink((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atom1),
                    rc->at(atom1), pos1, (LmnAtomRef)rc->wt(atom2),
                    rc->at(atom2), pos2);
    mut.unlock();
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

    ap = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom2))->get_link(pos2);
    attr = ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(pos2);

    if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1)) &&
        LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
      /* hlink属性ではない通常のデータアトム同士の接続 */
#ifdef DEBUG
      fprintf(stderr, "Two data atoms are connected each other.\n");
#endif
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))) {
      /* hlink属性ではない通常のデータアトムatom1とシンボルアトムatom2の接続.
       */
      ap->set_link(attr, (LmnAtomRef)rc->wt(atom1));
      ap->set_attr(attr, rc->at(atom1));
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {
      /* hlink属性ではない通常のデータアトムatom2とシンボルアトムatom1の接続
       */
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, ap);
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr);
    } else if (!LMN_ATTR_IS_EX(rc->at(atom1)) && !LMN_ATTR_IS_EX(attr)) {
      /* シンボルアトム同士の接続 */
      ap->set_link(attr, (LmnAtomRef)rc->wt(atom1));
      ap->set_attr(attr, pos1);
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, ap);
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr);
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
      ap2 = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom2))->get_link(pos2);
      attr2 = ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(pos2);
      ap2->set_link(attr2, (LmnAtomRef)rc->wt(atom1));
      ap2->set_attr(attr2, rc->at(atom1));
      break;
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom2))) {
      //(S,D)
      ap1 = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom1))->get_link(pos1);
      attr1 = ((LmnSymbolAtomRef)rc->wt(atom1))->get_attr(pos1);
      ap1->set_link(attr1, (LmnAtomRef)rc->wt(atom2));
      ap1->set_attr(attr1, rc->at(atom2));
      break;
    }
    //(S,S)
    ap1 = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom1))->get_link(pos1);
    ap2 = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom2))->get_link(pos2);
    attr1 = ((LmnSymbolAtomRef)rc->wt(atom1))->get_attr(pos1);
    attr2 = ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(pos2);

    if ((LmnSymbolAtomRef)rc->wt(atom1) == ap2 &&
        (LmnSymbolAtomRef)rc->wt(atom2) == ap1 && attr1 == pos2 &&
        attr2 == pos1) {
      // use same link

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1) &&
               LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) {
      //(-D,-D)

      /* データアトムap2とシンボルアトムatom1 */
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, ap2);
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr2);

      /* データアトムap1とシンボルアトムatom2 */
      ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, ap1);
      ((LmnSymbolAtomRef)rc->wt(atom2))->set_attr(pos2, attr1);

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {
      //(-D,-S)

      /* データアトムap1とシンボルアトムatom2 */
      ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, ap1);
      ((LmnSymbolAtomRef)rc->wt(atom2))->set_attr(pos2, attr1);

      /* シンボルアトムatom1とシンボルアトムap2 */
      if (ap2 != NULL) {
        ap2->set_link(attr2, (LmnAtomRef)rc->wt(atom1));
        ap2->set_attr(attr2, pos1);
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, ap2);
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr2);
      } else {
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, 0);
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, 0);
      }

    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) {
      //(-S,-D)

      /* データアトムap2とシンボルアトムatom1 */
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, ap2);
      ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr2);

      /* シンボルアトムatom2とシンボルアトムap1 */
      if (ap1 != NULL) {
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, ap1);
        ((LmnSymbolAtomRef)rc->wt(atom2))
            ->set_attr(pos2, LMN_ATTR_GET_VALUE(attr1));
        ap1->set_link(attr1, (LmnAtomRef)rc->wt(atom2));
        ap1->set_attr(attr1, pos2);
      } else {
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, 0);
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_attr(pos2, 0);
      }

    } else {
      //(-S,-S)

      /* シンボルアトムatom2とシンボルアトムap1 */
      if (ap1 != NULL) {
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, ap1);
        ((LmnSymbolAtomRef)rc->wt(atom2))
            ->set_attr(pos2, LMN_ATTR_GET_VALUE(attr1));
        ap1->set_link(attr1, (LmnAtomRef)rc->wt(atom2));
        ap1->set_attr(attr1, pos2);
      } else {
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_link(pos2, 0);
        ((LmnSymbolAtomRef)rc->wt(atom2))->set_attr(pos2, 0);
      }

      /* シンボルアトムatom1とシンボルアトムap2 */
      if (ap2 != NULL) {
        ap2->set_link(attr2, (LmnAtomRef)rc->wt(atom1));
        ap2->set_attr(attr2, pos1);
        ((LmnSymbolAtomRef)(LmnSymbolAtomRef)rc->wt(atom1))
            ->set_link(pos1, ap2);
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, attr2);
      } else {
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_link(pos1, 0);
        ((LmnSymbolAtomRef)rc->wt(atom1))->set_attr(pos1, 0);
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
    /*       ap1 = LMN_SATOM(wt(rc, atom1)->get_link(pos1)); */
    /*       ap2 = LMN_SATOM(wt(rc, atom2)->get_link(pos2)); */
    /*       attr1 = (wt(rc, atom1))->get_attr(pos1); */
    /*       attr2 = (wt(rc, atom2))->get_attr(pos2); */
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
    /*        ap1->set_link(attr1, wt(rc, atom2)); */
    /*        ap1->set_attr(attr1, pos2); */
    /*      } */
    /*         if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*        if(ap2 != NULL){ */
    /*          ap2->set_link(attr2, wt(rc, atom1)); */
    /*          ap2->set_attr(attr2, pos1); */
    /*        } */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, attr2); */
    /*         }else { */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      ap2->set_link(attr2, wt(rc, atom1)); */
    /*                      ap2->set_attr(attr2, pos1); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_link(pos1,
     * ap2); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * attr2); */
    /*              }else{ */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, 0);
     */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*           /\*ap2->set_link(attr2, wt(rc, atom1)); */
    /*           ap2->set_attr(attr2, rc->at(atom1)); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, attr2);*\/
     */
    /*         } */
    /*       } */
    /*       else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)){ */
    /*         /\* データアトムap1とシンボルアトムatom2 *\/ */
    /*         (LMN_SATOM(wt(rc, atom2)))->set_link(pos2, ap1); */
    /*         (LMN_SATOM(wt(rc, atom2)))->set_attr(pos2, attr1); */
    /*         if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*           ap2->set_link(attr2, wt(rc, atom1)); */
    /*           ap2->set_attr(attr2, pos1); */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, attr2); */
    /*         }else if (!LMN_ATTR_IS_EX(rc->at(atom1)) &&
     * !LMN_ATTR_IS_EX(attr2)){ */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      ap2->set_link(attr2, wt(rc, atom1)); */
    /*                      ap2->set_attr(attr2, pos1); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_link(pos1,
     * ap2); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * attr2); */
    /*              }else{ */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, 0);
     */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*           /\*ap2->set_link(attr2, wt(rc, atom1)); */
    /*           ap2->set_attr(attr2, rc->at(atom1)); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, attr2);*\/
     */
    /*         } */
    /*       } */
    /*       else if (!LMN_ATTR_IS_EX(rc->at(atom1)) && !LMN_ATTR_IS_EX(at(rc,
     * atom2)) */
    /*                && !LMN_ATTR_IS_EX(attr1) && !LMN_ATTR_IS_EX(attr2)){ */
    /*         /\* シンボルアトムatom2とシンボルアトムap1 *\/ */

    /*         if(ap1 != NULL){ */
    /*           (LMN_SATOM(wt(rc, atom2)))->set_link(pos2, ap1); */
    /*           (LMN_SATOM(wt(rc, atom2)))->set_attr(pos2,
     * LMN_ATTR_GET_VALUE(attr1)); */
    /*           ap1->set_link(attr1, wt(rc, atom2)); */
    /*           ap1->set_attr(attr1, pos2); */
    /*         }else{ */
    /*           (LMN_SATOM(wt(rc, atom2)))->set_link(pos2, 0); */
    /*           (LMN_SATOM(wt(rc, atom2)))->set_attr(pos2, 0); */
    /*         } */
    /*      if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*           /\* データアトムatom1とシンボルアトムap2 *\/ */
    /*           ap2->set_link(attr2, wt(rc, atom1)); */
    /*           ap2->set_attr(attr2, pos1); */
    /*         }else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*           /\* データアトムap2とシンボルアトムatom1 *\/ */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * LMN_ATTR_GET_VALUE(attr2)); */
    /*         }else { */
    /*           /\* シンボルアトムatom1とシンボルアトムap2 *\/ */
    /*      ////// */
    /*              if(ap2 != NULL){ */
    /*                      ap2->set_link(LMN_ATTR_GET_VALUE(attr2),
     * wt(rc, atom1)); */
    /*                      ap2->set_attr(LMN_ATTR_GET_VALUE(attr2),
     * pos1); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * ap2); */
    /*                      (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * LMN_ATTR_GET_VALUE(attr2)); */
    /*              }else{ */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, 0);
     */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, 0);
     */
    /*              } */
    /*      ////// */
    /*      /\* */
    /*         if(ap2){ */
    /*              if(LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))){ */
    /*                // データアトムatom1とシンボルアトムap2  */
    /*                ap2->set_link(attr2, wt(rc, atom1)); */
    /*                ap2->set_attr(attr2, rc->at(atom1)); */
    /*              }else if(LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)){ */
    /*                // データアトムap2とシンボルアトムatom1  */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2);
     */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * attr2); */
    /*              }else{ */
    /*                // シンボルアトムatom1とシンボルアトムap2  */
    /*                ap2->set_link(attr2, wt(rc, atom1)); */
    /*                ap2->set_attr(attr2, pos1); */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, ap2);
     */
    /*                (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1,
     * attr2); */
    /*              } */
    /*         }else{ */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_link(pos1, 0); */
    /*           (LMN_SATOM(wt(rc, atom1)))->set_attr(pos1, 0); */
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
        LMN_ATTR_IS_DATA(rc->at(linki))) {
#ifdef DEBUG
      fprintf(stderr, "Two data atoms are connected each other.\n");
#endif
    } else if (LMN_ATTR_IS_DATA(rc->at(atomi))) {
      ((LmnSymbolAtomRef)rc->wt(linki))
          ->set_link(rc->at(linki), (LmnAtomRef)rc->wt(atomi));
      ((LmnSymbolAtomRef)rc->wt(linki))
          ->set_attr(rc->at(linki), rc->at(atomi));
    } else if (LMN_ATTR_IS_DATA(rc->at(linki))) {
      ((LmnSymbolAtomRef)rc->wt(atomi))->set_link(posi, (LmnAtomRef)rc->wt(linki));
      ((LmnSymbolAtomRef)rc->wt(atomi))->set_attr(posi, rc->at(linki));
    } else {
      ((LmnSymbolAtomRef)rc->wt(atomi))->set_link(posi, (LmnAtomRef)rc->wt(linki));
      ((LmnSymbolAtomRef)rc->wt(atomi))->set_attr(posi, rc->at(linki));
      ((LmnSymbolAtomRef)rc->wt(linki))
          ->set_link(rc->at(linki), (LmnAtomRef)rc->wt(atomi));
      ((LmnSymbolAtomRef)rc->wt(linki))
          ->set_attr(rc->at(linki), posi);
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
        (LmnWord)((LmnSymbolAtomRef)rc->wt(atomi))->get_link(posi),
        ((LmnSymbolAtomRef)rc->wt(atomi))->get_attr(posi), TT_ATOM};

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

      LmnAtomRef hlAtom = ((LmnSymbolAtomRef)rc->wt(atomi))->get_link(posi);
      LmnLinkAttr attr = ((LmnSymbolAtomRef)rc->wt(atomi))->get_attr(posi);
      if (attr != LMN_HL_ATTR) {
        return FALSE;
      } else {
        HyperLink *hl = lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)hlAtom);
        auto v = lmn_hyperlink_get_elements(hl);
        auto regs = std::vector<LmnRegister>();
        std::transform(
            v.begin(), v.end(), std::back_inserter(regs),
            [](HyperLink *h) -> LmnRegister {
              auto child_hlAtom = h->atom;
              auto linked_atom = child_hlAtom->get_link(0);
              return {(LmnWord)linked_atom, child_hlAtom->get_attr(0), TT_ATOM};
            });

        this->false_driven_enumerate(linki, std::move(regs));
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
    this->push_stackframe(exec_subinstructions_not(instr + subinstr_size));
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
    LmnFunctor f = sa->get_functor();
    AtomListEntry *ent = mem->get_atomlist(f);
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
      ((LmnSymbolAtomRef)rc->wt(atomi))->set_link(link, NULL);
    }

    break;
  }
  case INSTR_NEWMEM: {
    // std::cout << "new!" << std::endl;
    LmnInstrVar newmemi, parentmemi;
    LmnMembraneRef mp;

    READ_VAL(LmnInstrVar, instr, newmemi);
    READ_VAL(LmnInstrVar, instr, parentmemi);
    SKIP_VAL(LmnInstrVar, instr);

    mp = new LmnMembrane(); /*lmn_new_mem(memf);*/
    ((LmnMembraneRef)rc->wt(parentmemi))->add_child_mem(mp);
    rc->wt(newmemi) = (LmnWord)mp;
    rc->tt(newmemi) = TT_MEM;
    mp->set_active(TRUE);
    if (rc->has_mode(REACT_MEM_ORIENTED)) {
      ((MemReactContext *)rc)->memstack_push(mp);
    }
    break;
  }
  case INSTR_ALLOCMEM: {
    LmnInstrVar dstmemi;
    READ_VAL(LmnInstrVar, instr, dstmemi);
    rc->wt(dstmemi) = (LmnWord) new LmnMembrane();
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

    // mut.lock();
    lmn_mem_remove_atom((LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atomi),
                        rc->at(atomi));
    // mut.unlock();

    break;
  }
  case INSTR_FREEATOM: {
    LmnInstrVar atomi;

    READ_VAL(LmnInstrVar, instr, atomi);

    // 仮のロック。atomiやatomlistentryにロックを掛けた方がいいかも。
    mut.lock();
    lmn_free_atom((LmnAtomRef)rc->wt(atomi), rc->at(atomi));
    mut.unlock();
    break;
  }
  case INSTR_REMOVEMEM: {
    LmnInstrVar memi, parenti;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, parenti);

    ((LmnMembraneRef)rc->wt(parenti))->remove_mem((LmnMembraneRef)rc->wt(memi));
    break;
  }
  case INSTR_FREEMEM: {
    LmnInstrVar memi;
    LmnMembraneRef mp;

    READ_VAL(LmnInstrVar, instr, memi);

    mp = (LmnMembraneRef)rc->wt(memi);
    delete mp;
    break;
  }
  case INSTR_ADDMEM: {
    LmnInstrVar dstmem, srcmem;

    READ_VAL(LmnInstrVar, instr, dstmem);
    READ_VAL(LmnInstrVar, instr, srcmem);

    //      LMN_ASSERT(!((LmnMembraneRef)rc->wt( srcmem))->parent);

    ((LmnMembraneRef)rc->wt(dstmem))
        ->add_child_mem((LmnMembraneRef)rc->wt(srcmem));
    break;
  }
  case INSTR_ENQUEUEMEM: {
    LmnInstrVar memi;
    READ_VAL(LmnInstrVar, instr, memi);

    if (rc->has_mode(REACT_MEM_ORIENTED)) {
      ((MemReactContext *)rc)->memstack_push((LmnMembraneRef)rc->wt(memi));
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

    mut.lock();
    rc->reg(atom1) = {
        (LmnWord)LMN_SATOM(((LmnSymbolAtomRef)rc->wt(atom2))->get_link(posi)),
        ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(posi), TT_ATOM};
    mut.unlock();
    break;
  }
  case INSTR_DEREF: {
    LmnInstrVar atom1, atom2, pos1, pos2;
    LmnByte attr;

    READ_VAL(LmnInstrVar, instr, atom1);
    READ_VAL(LmnInstrVar, instr, atom2);
    READ_VAL(LmnInstrVar, instr, pos1);
    READ_VAL(LmnInstrVar, instr, pos2);

    // TODO(Fukui): atom2にロックをかける
    mut.lock();

    attr = ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(pos1);
    LMN_ASSERT(!LMN_ATTR_IS_DATA(rc->at(atom2)));
    if (LMN_ATTR_IS_DATA(attr)) {
      if (pos2 != 0){
        return FALSE;
      }
    } else {
      if (attr != pos2){
        return FALSE;
      }
    }

    rc->reg(atom1) = {
        (LmnWord)((LmnSymbolAtomRef)rc->wt(atom2))->get_link(pos1), attr,
        TT_ATOM};
    mut.unlock();
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
        if (((LmnSymbolAtomRef)rc->wt(atomi))->get_functor() != f) {
          return FALSE;
        }
        if (rc_hlink_opt(atomi, rc)) {
          auto spc = ((SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(),
                                                 (HashKeyType)atomi));

          auto atom = (LmnSymbolAtomRef)rc->wt(atomi);
          for (int i = 0; i < spc->proccxts.size(); i++) {
            if (spc->proccxts[i]) {
              if (spc->proccxts[i]->is_argument_of(atom, i)) {
                auto linked_hl =
                    lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom->get_link(i));
                spc->proccxts[i]->start = linked_hl;
              } else {
                return false;
              }
            }
          }
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
        if (((LmnSymbolAtomRef)rc->wt(atomi))->get_functor() == f)
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

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        auto addr = atoms.get();
        atoms.release();
        dpor_LHS_add_ground_atoms(RC_POR_DATA(rc), addr);

        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_remove_ground_atoms(RC_POR_DATA(rc), addr);
          delete addr;
          return command_result::Failure;
        });
      }
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
      attr = (LmnLinkAttr)rc->at(srcvec->get(0));

      /** 識別子の生成 **/
      /* 引数に直接データアトムが接続されている場合 */
      if (LMN_ATTR_IS_DATA(attr)) {
        switch (attr) {
        case LMN_INT_ATTR: {
          char *s = int_to_str(rc->wt(srcvec->get(0)));
          port_put_raw_s(port, s);
          LMN_FREE(s);
          break;
        }
        case LMN_DBL_ATTR: {
          char buf[64];
          sprintf(buf, "%f", lmn_get_double(rc->wt(srcvec->get(0))));
          port_put_raw_s(port, buf);
          break;
        }
        case LMN_HL_ATTR: {
          char buf[16];
          port_put_raw_s(port, EXCLAMATION_NAME);
          sprintf(buf, "%lx",
                  LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(
                      (LmnSymbolAtomRef)rc->wt(srcvec->get(0)))));
          port_put_raw_s(port, buf);
          break;
        }
        default: /* int, double, hlink 以外はとりあえず今まで通り */
          lmn_dump_atom(port, (LmnAtomRef)rc->wt(srcvec->get(0)),
                        (LmnLinkAttr)rc->at(srcvec->get(0)));
        }
      } else { /* symbol atom */
        lmn_dump_atom(port, (LmnAtomRef)rc->wt(srcvec->get(0)),
                      (LmnLinkAttr)rc->at(srcvec->get(0)));
      }
      port_put_raw_s(port, ":");
    }

    id = lmn_intern(((LmnStringRef)port->data)->c_str());
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
        if (((LmnSymbolAtomRef)ap)->get_arity() > 1) {
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
          ((LmnSymbolAtomRef)ap)->get_arity() > 1) {
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
        (LmnWord)(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(atomi)))
            ->element_num(),
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

    attr1 = atom->get_attr(0);
    attr2 = atom->get_attr(1);

    /* >< の両辺のアトムがハイパーリンクであれば併合 */
    if (LMN_ATTR_IS_HL(attr1) && LMN_ATTR_IS_HL(attr2)) {
      LmnMembraneRef m;
      LmnSymbolAtomRef atom1, atom2;
      HyperLink *hl1, *hl2;

      m = (LmnMembraneRef)rc->wt(memi);
      atom1 = (LmnSymbolAtomRef)atom->get_link(0);
      atom2 = (LmnSymbolAtomRef)atom->get_link(1);

      hl1 = lmn_hyperlink_at_to_hl(atom1);
      hl2 = lmn_hyperlink_at_to_hl(atom2);

      if (atom->get_arity() ==
          2) { //二引数の場合は一つ目のハイパーリンクの属性を継承する
        hl1->lmn_unify(hl2, LMN_HL_ATTRATOM(hl1), LMN_HL_ATTRATOM_ATTR(hl1));
      } else if (atom->get_arity() ==
                 3) { //三引数の場合は三引数目を併合後の属性とする
        LmnAtom attrAtom;
        attrAtom = LMN_ATOM(atom->get_link(2));
        hl1->lmn_unify(hl2, (LmnAtomRef)attrAtom, atom->get_attr(2));
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

    if (!rc->get_hl_sameproccxt()) {
      rc->prepare_hl_spc();
    }

    if (!hashtbl_contains(rc->get_hl_sameproccxt(), (HashKeyType)atom1)) {
      spc1 = new SameProcCxt(length1);
      hashtbl_put(rc->get_hl_sameproccxt(), (HashKeyType)atom1, (HashValueType)spc1);
    } else {
      spc1 = (SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(), (HashKeyType)atom1);
    }

    if (!hashtbl_contains(rc->get_hl_sameproccxt(), (HashKeyType)atom2)) {
      spc2 = new SameProcCxt(length2);
      hashtbl_put(rc->get_hl_sameproccxt(), (HashKeyType)atom2, (HashValueType)spc2);
    } else {
      spc2 = (SameProcCxt *)hashtbl_get(rc->get_hl_sameproccxt(), (HashKeyType)atom2);
    }

    spc1->add_proccxt_if_absent(atom1, arg1);
    spc2->add_proccxt_if_absent(atom2, arg2, *spc1, arg1);

    ////normal parallel init
    if (lmn_env.enable_parallel && !lmn_env.nd) {
      for (i = 0; i < lmn_env.core_num; i++) {
        if (!thread_info[i]->rc->get_hl_sameproccxt()) {
          thread_info[i]->rc->prepare_hl_spc();
        }

        if (!hashtbl_contains(thread_info[i]->rc->get_hl_sameproccxt(),
                              (HashKeyType)atom1)) {
          spc1 = new SameProcCxt(length1);
          hashtbl_put(thread_info[i]->rc->get_hl_sameproccxt(), (HashKeyType)atom1,
                      (HashValueType)spc1);
        } else {
          spc1 = (SameProcCxt *)hashtbl_get(thread_info[i]->rc->get_hl_sameproccxt(),
                                            (HashKeyType)atom1);
        }

        if (!hashtbl_contains(thread_info[i]->rc->get_hl_sameproccxt(),
                              (HashKeyType)atom2)) {
          spc2 = new SameProcCxt(length2);
          hashtbl_put(thread_info[i]->rc->get_hl_sameproccxt(), (HashKeyType)atom2,
                      (HashValueType)spc2);
        } else {
          spc2 = (SameProcCxt *)hashtbl_get(thread_info[i]->rc->get_hl_sameproccxt(),
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
      attr_dataAtoms.init(16);
      attr_dataAtom_attrs.init(16);
      attr_functors = new ProcessTbl(16);
      LmnInstrVar i = 0, n;

      READ_VAL(LmnInstrVar, instr, n);

      switch (op) {
      case INSTR_COPYHLGROUNDINDIRECT: {
        LmnInstrVar ai;
        for (; n--; i++) {
          READ_VAL(LmnInstrVar, instr, ai);
          if (LMN_ATTR_IS_DATA(rc->at(ai))) {
            attr_dataAtom_attrs.push(rc->at(ai));
            attr_dataAtoms.push(rc->wt(ai));
          } else {
            LmnFunctor f;
            f = ((LmnSymbolAtomRef)rc->wt(ai))->get_functor();
            attr_functors->proc_tbl_put(f, f);
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
            attr_dataAtom_attrs.push(attr);
            READ_DATA_ATOM(at, attr);
            attr_dataAtoms.push((LmnWord)at);
          } else {
            LmnFunctor f;
            READ_VAL(LmnFunctor, instr, f);
            attr_functors->proc_tbl_put(f, f);
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
    retvec = new Vector(2);
    retvec->push((LmnWord)dstlovec);
    retvec->push((LmnWord)atommap);
    rc->reg(dstlist) = {(LmnWord)retvec, LIST_AND_MAP, TT_OTHER};

    this->push_stackframe([=](interpreter &itr, bool result) {
      free_links(dstlovec);
      delete retvec;
      LMN_ASSERT(result);
      return result ? command_result::Success : command_result::Failure;
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
      attr_dataAtoms.init(16);
      attr_dataAtom_attrs.init(16);
      attr_functors = new ProcessTbl(16);
      LmnInstrVar i = 0, n;

      READ_VAL(LmnInstrVar, instr, n);

      switch (op) {
      case INSTR_REMOVEHLGROUNDINDIRECT:
      case INSTR_FREEHLGROUNDINDIRECT: {
        LmnInstrVar ai;
        for (; n--; i++) {
          READ_VAL(LmnInstrVar, instr, ai);
          if (LMN_ATTR_IS_DATA(rc->at(ai))) {
            attr_dataAtom_attrs.push(rc->at(ai));
            attr_dataAtoms.push(rc->wt(ai));
          } else {
            LmnFunctor f;
            f = ((LmnSymbolAtomRef)rc->wt(ai))->get_functor();
            attr_functors->proc_tbl_put(f, f);
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
            attr_dataAtom_attrs.push(attr);
            READ_DATA_ATOM(at, attr);
            attr_dataAtoms.push((LmnWord)at);
          } else {
            LmnFunctor f;
            READ_VAL(LmnFunctor, instr, f);
            attr_functors->proc_tbl_put(f, f);
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
      delete attr_functors;
      attr_dataAtoms.destroy();
      attr_dataAtom_attrs.destroy();
      break;
    }
    case INSTR_REMOVEGROUND:
      ((LmnMembraneRef)rc->wt(memi))->remove_ground(srcvec);
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
    } else if (((LmnSymbolAtomRef)rc->wt(atomi))->get_arity() != 1)
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

    if (((LmnMembraneRef)rc->wt(memi))->is_active()) {
      return FALSE;
    }

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_STABLE);
        this->push_stackframe([=](interpreter &itr, bool result) {
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_STABLE);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
    }

    break;
  }
  case INSTR_NEWLIST: {
    LmnInstrVar listi;
    Vector *listvec = new Vector(16);
    READ_VAL(LmnInstrVar, instr, listi);
    rc->reg(listi) = {(LmnWord)listvec, 0, TT_OTHER};

    /* 解放のための再帰 */
    this->push_stackframe([=](interpreter &itr, bool result) {
      delete listvec;
      return result ? command_result::Success : command_result::Failure;
    });
    break;
  }
  case INSTR_ADDTOLIST: {
    LmnInstrVar listi, linki;
    READ_VAL(LmnInstrVar, instr, listi);
    READ_VAL(LmnInstrVar, instr, linki);
    ((Vector *)rc->wt(listi))->push(linki);

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
        rc->reg(dsti) = {((Vector *)rc->wt(listi))->get((unsigned int)posi),
                         LINK_LIST, TT_OTHER};
      } else if (posi == 1) {
        rc->reg(dsti) = {((Vector *)rc->wt(listi))->get((unsigned int)posi),
                         MAP, TT_OTHER};
      } else {
        lmn_fatal("unexpected attribute @instr_getfromlist");
      }
      break;
    case LINK_LIST: /* LinkObjをfreeするのはここ？ */
    {
      LinkObjRef lo =
          (LinkObjRef)((Vector *)rc->wt(listi))->get((unsigned int)posi);
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
    // mut.lock();
    rc->reg(dstatom) = {
        static_cast<LmnWord>(((long)rc->wt(atom1) + (long)rc->wt(atom2))),
        LMN_INT_ATTR, TT_ATOM};
    // mut.unlock();
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
    // mut.lock();
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
    // mut.unlock();
    break;
  }
  case INSTR_ALLOCATOMINDIRECT: {
    LmnInstrVar atomi;
    LmnInstrVar srcatomi;

    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnInstrVar, instr, srcatomi);

    if (LMN_ATTR_IS_DATA(rc->at(srcatomi))) {
      mut.lock();
      if (LMN_ATTR_IS_EX(rc->at(srcatomi))) {
        rc->wt(atomi) = rc->wt(srcatomi);
      } else {
        rc->wt(atomi) = lmn_copy_data_atom(rc->wt(srcatomi), rc->at(srcatomi));
      }
      rc->at(atomi) = rc->at(srcatomi);
      rc->tt(atomi) = TT_OTHER;
      mut.unlock();
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
          (LmnWord)((LmnSymbolAtomRef)rc->wt(atomi))->get_functor(),
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
    ((LmnMembraneRef)rc->wt(memi))->set_name(name);
    break;
  }
  case INSTR_COPYRULES: {
    LmnInstrVar destmemi, srcmemi;
    unsigned int i;

    READ_VAL(LmnInstrVar, instr, destmemi);
    READ_VAL(LmnInstrVar, instr, srcmemi);
    auto &v = ((LmnMembraneRef)rc->wt(srcmemi))->get_rulesets();
    for (auto &rs : v) {
      auto cp = new LmnRuleSet(*rs);
      lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(destmemi), cp);
    }
    break;
  }
  case INSTR_REMOVEPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    ((LmnMembraneRef)rc->wt(memi))->remove_proxies();
    break;
  }
  case INSTR_INSERTPROXIES: {
    LmnInstrVar parentmemi, childmemi;

    READ_VAL(LmnInstrVar, instr, parentmemi);
    READ_VAL(LmnInstrVar, instr, childmemi);
    ((LmnMembraneRef)rc->wt(parentmemi))
        ->insert_proxies((LmnMembraneRef)rc->wt(childmemi));
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
      copy->get_next()->set_prev(copy->get_prev());
      copy->get_prev()->set_next(copy->get_next());

      lmn_delete_atom(copy);
    }

    delete delmap;
    break;
  }
  case INSTR_REMOVETOPLEVELPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    ((LmnMembraneRef)rc->wt(memi))->remove_toplevel_proxies();
    break;
  }
  case INSTR_DEREFFUNC: {
    LmnInstrVar funci, atomi, pos;
    LmnLinkAttr attr;

    READ_VAL(LmnInstrVar, instr, funci);
    READ_VAL(LmnInstrVar, instr, atomi);
    READ_VAL(LmnLinkAttr, instr, pos);

    attr = ((LmnSymbolAtomRef)rc->wt(atomi))->get_attr(pos);
    if (LMN_ATTR_IS_DATA(attr)) {
      rc->reg(funci) = {
          (LmnWord)((LmnSymbolAtomRef)rc->wt(atomi))->get_link(pos), attr,
          TT_OTHER};
    } else { /* symbol atom */
      rc->reg(funci) = {
          ((LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atomi))->get_link(pos))
              ->get_functor(),
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
      if (!(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func0)))
               ->eq_hl(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func1))))
        return FALSE;
      break;
    case LMN_SP_ATOM_ATTR:
      if (!SP_ATOM_EQ(rc->wt(func0), rc->wt(func1)))
        return FALSE;
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
        if ((lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func0)))
                ->eq_hl(
                    lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)rc->wt(func1))))
          return FALSE;
        break;
      case LMN_SP_ATOM_ATTR:
        if (SP_ATOM_EQ(rc->wt(func0), rc->wt(func1)))
          return FALSE;
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
    ((LmnMembraneRef)rc->wt(destmemi))
        ->move_cells((LmnMembraneRef)rc->wt(srcmemi));
    break;
  }
  case INSTR_REMOVETEMPORARYPROXIES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    ((LmnMembraneRef)rc->wt(memi))->remove_temporary_proxies();
    break;
  }
  case INSTR_NFREELINKS: {
    LmnInstrVar memi, count;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, count);

    if (!((LmnMembraneRef)rc->wt(memi))->nfreelinks(count))
      return FALSE;

    if (rc->has_mode(REACT_ND)) {
      auto mcrc = dynamic_cast<MCReactContext *>(rc);
      if (mcrc->has_optmode(DynamicPartialOrderReduction) && !rc->is_zerostep) {
        LmnMembraneRef m = (LmnMembraneRef)rc->wt(memi);
        dpor_LHS_flag_add(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NFLINKS);
        this->push_stackframe([=](interpreter &itr, bool result) {
          LMN_ASSERT(!result);
          dpor_LHS_flag_remove(RC_POR_DATA(rc), m->mem_id(), LHS_MEM_NFLINKS);
          return command_result::
              Failure; /* 全ての候補取得のためにNDは常にFALSEを返す仕様
                        */
        });
      }
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

    rc->at(destlinki) = rc->at(srclinki);
    rc->tt(destlinki) = TT_ATOM;
    if (LMN_ATTR_IS_DATA(rc->at(srclinki))) {
      rc->wt(destlinki) = (LmnWord)rc->wt(srclinki);
    } else { /* symbol atom */
      ProcessTableRef ht = (ProcessTableRef)rc->wt(tbli);
      LmnWord w = rc->wt(destlinki);
      proc_tbl_get_by_atom(ht, (LmnSymbolAtomRef)rc->wt(srclinki), &w);
      rc->wt(destlinki) = w;
    }
    break;
  }
  case INSTR_CLEARRULES: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    ((LmnMembraneRef)rc->wt(memi))->clearrules();
    break;
  }
  case INSTR_DROPMEM: {
    LmnInstrVar memi;

    READ_VAL(LmnInstrVar, instr, memi);
    ((LmnMembraneRef)rc->wt(memi))->drop();
    break;
  }
  case INSTR_TESTMEM: {
    LmnInstrVar memi, atomi;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);
    LMN_ASSERT(!LMN_ATTR_IS_DATA(rc->at(atomi)));
    LMN_ASSERT(
        LMN_IS_PROXY_FUNCTOR(((LmnSymbolAtomRef)rc->wt(atomi))->get_functor()));

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
    this->push_stackframe(exec_subinstructions_group(next));
    break;
  }
  case INSTR_BRANCH: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    if (rc->get_hl_sameproccxt()) {
      /*branchとhyperlinkを同時起動するための急場しのぎ */
      rc->clear_hl_spc();
    }

    auto next = instr + subinstr_size;
    this->push_stackframe(exec_subinstructions_branch(next));
    break;
  }
  case INSTR_LOOP: {
    LmnSubInstrSize subinstr_size;
    READ_VAL(LmnSubInstrSize, instr, subinstr_size);

    this->push_stackframe(
        exec_subinstructions_while(instr, instr + subinstr_size));
    break;
  }
  case INSTR_CALLBACK: {
    LmnInstrVar memi, atomi;
    LmnSymbolAtomRef atom;
    const struct CCallback *c;

    READ_VAL(LmnInstrVar, instr, memi);
    READ_VAL(LmnInstrVar, instr, atomi);
    mut.lock();

    atom = (LmnSymbolAtomRef)rc->wt(atomi);

    if (!LMN_ATTR_IS_DATA(atom->get_attr(0))) {
      LmnSymbolAtomRef f_name = (LmnSymbolAtomRef)atom->get_link(0);
      lmn_interned_str name =
          LMN_FUNCTOR_NAME_ID(lmn_functor_table, f_name->get_functor());
      int arity = LMN_FUNCTOR_ARITY(lmn_functor_table, atom->get_functor());

      c = CCallback::get_ccallback(name);
      if (!c)
        break;

      if (arity - 1 != c->get_arity()) {
        fprintf(stderr, "EXTERNAL FUNC: invalid arity - %s\n",
                LMN_SYMBOL_STR(name));
        break;
      }

      /* (2015-07-30) moved to the end so that lmn_dump_mem can safely
         be called in callback functions
      lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi), rc->wt( atomi),
      rc->at(atomi)); lmn_mem_delete_atom((LmnMembraneRef)rc->wt( memi),
                          atom->get_link(0),
                          atom->get_attr(0));
      */

      switch (arity) {
      case 1:
        ((callback_0)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi));
        break;
      case 2:
        ((callback_1)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                 atom->get_link(1), atom->get_attr(1));
        break;
      case 3:
        ((callback_2)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                 atom->get_link(1), atom->get_attr(1),
                                 atom->get_link(2), atom->get_attr(2));
        break;
      case 4:
        ((callback_3)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                 atom->get_link(1), atom->get_attr(1),
                                 atom->get_link(2), atom->get_attr(2),
                                 atom->get_link(3), atom->get_attr(3));
        break;
      case 5:
        ((callback_4)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                 atom->get_link(1), atom->get_attr(1),
                                 atom->get_link(2), atom->get_attr(2),
                                 atom->get_link(3), atom->get_attr(3),
                                 atom->get_link(4), atom->get_attr(4));
        break;
      case 6:
        ((callback_5)c->get_f())(
            rc, (LmnMembraneRef)rc->wt(memi), atom->get_link(1),
            atom->get_attr(1), atom->get_link(2), atom->get_attr(2),
            atom->get_link(3), atom->get_attr(3), atom->get_link(4),
            atom->get_attr(4), atom->get_link(5), atom->get_attr(5));
        break;
      default:
        printf("EXTERNAL FUNCTION: too many arguments\n");
        break;
      }



      lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi),
                          (LmnAtomRef)rc->wt(atomi), rc->at(atomi));
      lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi),
                          ((LmnSymbolAtomRef)atom)->get_link(0),
                          ((LmnSymbolAtomRef)atom)->get_attr(0));
      mut.unlock();
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
    lmn_dump_cell_stdout(rc->get_global_root());
    lmn_hyperlink_print(rc->get_global_root());
    break;
  }
  default:
    fprintf(stderr, "interpret: Unknown operation %d\n", op);
    exit(1);
  }

  stop = false;
  return false;
}

bool slim::vm::interpreter::run(int ti=0) {
  //  << "running!" << std::endl;
  bool result;
  do {
    // キリのいいところまで命令列を実行する
    bool stop = false;
    do {
      // std::cout << "command start [" << ti << "] " << std::endl;
      // mut.lock();
      result = exec_command(this->rc, this->rule, stop);
      // mut.unlock();
      // std::cout << "command end [" << ti << "] " << std::endl;
    } while (!stop);

    // 成否がわかったのでstack frameに積まれているcallbackを消費する
    while (!this->callstack.empty()) {
      auto &s = this->callstack.back();
      auto r = s.callback(*this, result);

      if (r == command_result::Success) {
        result = true;
        this->callstack.pop_back();
      } else if (r == command_result::Failure) {
        result = false;
        this->callstack.pop_back();
      } else if (r == command_result::Trial) {
        // ここにきた場合は違うパターンでリトライするためにcallbackをpopしない
        // (c.f. false_driven_enumerator)
        break;
      }
    }
  } while (!this->callstack.empty());

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

      links.init(list_num + 1);
      for (i = 0; i < list_num; i++) {
        LmnInstrVar t;
        READ_VAL(LmnInstrVar, instr, t);
        links.push((LmnWord)t);
      }

      rc->reg(seti) = {(LmnWord)insertconnectors(rc, NULL, &links), 0,
                       TT_OTHER};
      links.destroy();

      /* EFFICIENCY: 解放のための再帰 */
      if (dmem_interpret(rc, rule, instr)) {
        delete (HashSet *)rc->wt(seti);
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

      links.init(list_num + 1);

      for (i = 0; i < list_num; i++) {
        READ_VAL(LmnInstrVar, instr, enti);
        links.push((LmnWord)enti);
      }

      READ_VAL(LmnInstrVar, instr, memi);

      rc->reg(seti) = {
          (LmnWord)insertconnectors(rc, (LmnMembraneRef)rc->wt(memi), &links),
          0, TT_OTHER};
      links.destroy();

      /* EFFICIENCY: 解放のための再帰 */
      if (dmem_interpret(rc, rule, instr)) {
        delete (HashSet *)rc->wt(seti);
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

      RC_ND_MEM_DELTA_ROOT(rc)->push_atom((LmnMembraneRef)rc->wt(memi),
                                          (LmnAtomRef)ap, attr);

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
      RC_ND_MEM_DELTA_ROOT(rc)->push_atom((LmnMembraneRef)rc->wt(memi),
                                          (LmnAtomRef)rc->wt(atom1),
                                          rc->at(atom1));
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

      if (LMN_ATTR_IS_DATA(rc->at(link1))) {
        if (LMN_ATTR_IS_DATA(rc->at(link2))) { /* 1, 2 are data */
          RC_ND_MEM_DELTA_ROOT(rc)->link_data_atoms(
              (LmnMembraneRef)rc->wt(mem), (LmnDataAtomRef)rc->wt(link1),
              rc->at(link1), (LmnDataAtomRef)rc->wt(link2),
              rc->at(link2));
        } else { /* 1 is data */
          RC_ND_MEM_DELTA_ROOT(rc)->unify_links(
              (LmnMembraneRef)rc->wt(mem), (LmnAtomRef)rc->wt(link2),
              rc->at(link2), (LmnAtomRef)rc->wt(link1), rc->at(link1));
        }
      } else { /* 2 is data or 1, 2 are symbol atom */
        RC_ND_MEM_DELTA_ROOT(rc)->unify_links(
            (LmnMembraneRef)rc->wt(mem), (LmnAtomRef)rc->wt(link1), rc->at(link1),
            (LmnAtomRef)rc->wt(link2), rc->at(link2));
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

      RC_ND_MEM_DELTA_ROOT(rc)->newlink(
          (LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atom1),
          rc->at(atom1), pos1, (LmnAtomRef)rc->wt(atom2), rc->at(atom2), pos2);
      break;
    }
    case INSTR_RELINK: {
      LmnInstrVar atom1, atom2, pos1, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      RC_ND_MEM_DELTA_ROOT(rc)->relink(
          (LmnMembraneRef)rc->wt(memi), (LmnAtomRef)rc->wt(atom1),
          rc->at(atom1), pos1, (LmnAtomRef)rc->wt(atom2), rc->at(atom2), pos2);
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
          (LmnWord)((LmnSymbolAtomRef)rc->wt(atomi))->get_attr(posi);
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

      RC_ND_MEM_DELTA_ROOT(rc)->unify_atom_args(
          (LmnMembraneRef)rc->wt(memi), (LmnSymbolAtomRef)rc->wt(atom1), pos1,
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
      mp->set_active(TRUE);
      if (rc->has_mode(REACT_MEM_ORIENTED)) {
        ((MemReactContext *)rc)->memstack_push(mp);
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

      RC_ND_MEM_DELTA_ROOT(rc)->remove_atom((LmnMembraneRef)rc->wt(memi),
                                            (LmnAtomRef)rc->wt(atomi),
                                            rc->at(atomi));
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
      //      if (rc->has_mode(REACT_ND) && !rc->is_zerostep)
      //      {
      //        lmn_mem_activate_ancestors((LmnMembraneRef)rc->wt( memi)); /* MC
      //        */
      //      }
      /* 通常実行ではdmem_interpretを使用しないため以下のコードは不要.
       * ただ,
       * 通常実行用dmemはテスト用やinteractive実行用として作っておいてもよさそう
       */
      //      if (rc->has_mode(REACT_MEM_ORIENTED)) {
      //        ((MemReactContext *)rc)->memstack_push(
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
      retvec = new Vector(2);
      retvec->push((LmnWord)dstlovec);
      retvec->push((LmnWord)atommap);
      rc->reg(dstlist) = {(LmnWord)retvec, LIST_AND_MAP, TT_OTHER};

      /* 解放のための再帰。ベクタを解放するための中間語命令がない */
      dmem_interpret(rc, rule, instr);

      free_links(dstlovec);
      delete retvec;

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
      Vector *listvec = new Vector(16);
      READ_VAL(LmnInstrVar, instr, listi);
      rc->reg(listi) = {(LmnWord)listvec, 0, TT_OTHER};

      if (dmem_interpret(rc, rule, instr)) {
        delete listvec;
        return TRUE;
      } else {
        delete listvec;
        return FALSE;
      }
      break;
    }
    case INSTR_ADDTOLIST: {
      LmnInstrVar listi, linki;
      READ_VAL(LmnInstrVar, instr, listi);
      READ_VAL(LmnInstrVar, instr, linki);
      ((Vector *)rc->wt(listi))->push(linki);
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
          rc->reg(dsti) = {((Vector *)rc->wt(listi))->get((unsigned int)posi),
                           LINK_LIST, TT_OTHER};
        } else if (posi == 1) {
          rc->reg(dsti) = {((Vector *)rc->wt(listi))->get((unsigned int)posi),
                           MAP, TT_OTHER};
        } else {
          LMN_ASSERT(0);
        }
        break;
      case LINK_LIST: /* LinkObjをfreeするのはここ？ */
      {
        LinkObjRef lo =
            (LinkObjRef)((Vector *)rc->wt(listi))->get((unsigned int)posi);
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
        rc->wt(funci) = ((LmnSymbolAtomRef)rc->wt(atomi))->get_functor();
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
                               (LmnMembraneRef)rc->wt(memi));
      break;
    }
    case INSTR_INSERTPROXIES: {
      LmnInstrVar parentmemi, childmemi;

      READ_VAL(LmnInstrVar, instr, parentmemi);
      READ_VAL(LmnInstrVar, instr, childmemi);
      dmem_root_insert_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                               (LmnMembraneRef)rc->wt(parentmemi),
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
        t = 0; /* warningを黙らす */
        proc_tbl_get_by_atom(delmap, orig, &t);
        copy = (LmnSymbolAtomRef)t;
        lmn_mem_unify_symbol_atom_args(orig, 0, orig, 1);
        lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);

        lmn_delete_atom(orig);
        lmn_delete_atom(copy);
      }

      if (delmap)
        delete delmap;
      break;
    }
    case INSTR_REMOVETOPLEVELPROXIES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_toplevel_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                                        (LmnMembraneRef)rc->wt(memi));
      break;
    }
    case INSTR_ADDATOM: {
      LmnInstrVar memi, atomi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);
      RC_ND_MEM_DELTA_ROOT(rc)->push_atom((LmnMembraneRef)rc->wt(memi),
                                          (LmnAtomRef)rc->wt(atomi),
                                          rc->at(atomi));
      break;
    }
    case INSTR_MOVECELLS: {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      LMN_ASSERT(rc->wt(destmemi) != rc->wt(srcmemi));
      RC_ND_MEM_DELTA_ROOT(rc)->move_cells((LmnMembraneRef)rc->wt(destmemi),
                                           (LmnMembraneRef)rc->wt(srcmemi));
      break;
    }
    case INSTR_REMOVETEMPORARYPROXIES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_temporary_proxies(RC_ND_MEM_DELTA_ROOT(rc),
                                         (LmnMembraneRef)rc->wt(memi));
      break;
    }
    case INSTR_COPYCELLS: {
      LmnInstrVar mapi, destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, mapi);
      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      RC_ND_MEM_DELTA_ROOT(rc)->copy_cells((LmnMembraneRef)rc->wt(destmemi),
                                           (LmnMembraneRef)rc->wt(srcmemi));
      rc->tt(mapi) = TT_OTHER;
      break;
    }
    case INSTR_LOOKUPLINK: {
      LmnInstrVar destlinki, tbli, srclinki;

      READ_VAL(LmnInstrVar, instr, destlinki);
      READ_VAL(LmnInstrVar, instr, tbli);
      READ_VAL(LmnInstrVar, instr, srclinki);

      rc->at(destlinki) = rc->at(srclinki);
      if (LMN_ATTR_IS_DATA(rc->at(srclinki))) {
        rc->wt(destlinki) = (LmnWord)rc->wt(srclinki);
      } else { /* symbol atom */
        ProcessTableRef ht = (ProcessTableRef)rc->wt(tbli);
        LmnWord w = rc->wt(destlinki);
        proc_tbl_get_by_atom(ht, (LmnSymbolAtomRef)rc->wt(srclinki), &w);
        rc->wt(destlinki) = w;
      }
      break;
    }
    case INSTR_CLEARRULES: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_clear_ruleset(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembraneRef)rc->wt(memi));
      ((LmnMembraneRef)rc->wt(memi))->clearrules();

      break;
    }
    case INSTR_DROPMEM: {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_drop(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembraneRef)rc->wt(memi));
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

      atom = (LmnSymbolAtomRef)rc->wt(atomi);

      if (!LMN_ATTR_IS_DATA(atom->get_attr(0))) {
        LmnSymbolAtomRef f_name = (LmnSymbolAtomRef)atom->get_link(0);
        lmn_interned_str name = LMN_FUNCTOR_NAME_ID(lmn_functor_table, f_name->get_functor());
        int arity = LMN_FUNCTOR_ARITY(lmn_functor_table, atom->get_functor());

        c = CCallback::get_ccallback(name);
        if (!c)
          break;

        if (arity - 1 != c->get_arity()) {
          fprintf(stderr, "EXTERNAL FUNC: invalid arity - %s\n",
                  LMN_SYMBOL_STR(name));
          break;
        }

        lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi),
                            (LmnAtomRef)rc->wt(atomi), rc->at(atomi));
        lmn_mem_delete_atom((LmnMembraneRef)rc->wt(memi), atom->get_link(0),
                            atom->get_attr(0));

        switch (arity) {
        case 1:
          ((callback_0)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi));
          break;
        case 2:
          ((callback_1)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                   atom->get_link(1), atom->get_attr(1));
          break;
        case 3:
          ((callback_2)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                   atom->get_link(1), atom->get_attr(1),
                                   atom->get_link(2), atom->get_attr(2));
          break;
        case 4:
          ((callback_3)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                   atom->get_link(1), atom->get_attr(1),
                                   atom->get_link(2), atom->get_attr(2),
                                   atom->get_link(3), atom->get_attr(3));
          break;
        case 5:
          ((callback_4)c->get_f())(rc, (LmnMembraneRef)rc->wt(memi),
                                   atom->get_link(1), atom->get_attr(1),
                                   atom->get_link(2), atom->get_attr(2),
                                   atom->get_link(3), atom->get_attr(3),
                                   atom->get_link(4), atom->get_attr(4));
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
  Vector *vec = new Vector(16);

  /* リンクオブジェクトのベクタを構築 */
  for (i = 0; i < link_idxs->get_num(); i++) {
    vec_data_t t = link_idxs->get(i);
    LinkObjRef l = LinkObj_make((LmnAtomRef)rc->wt(t), rc->at(t));
    vec->push((LmnWord)l);
  }
  return vec;
}

void free_links(Vector *links) {
  unsigned long i;

  for (i = 0; i < links->get_num(); i++) {
    LMN_FREE(links->get(i));
  }
  delete links;
}
