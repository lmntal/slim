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


#include "task.h"
#include "atom.h"
#include "dumper.h"
#include "instruction.h"
#include "vector.h"
#include "symbol.h"
#include "functor.h"
#include "st.h"
#include "functor.h"
#include "error.h"
#include "ccallback.h"
#include "special_atom.h"
#include "hyperlink.h"//seiji
#include "slim_header/string.h"
#include "slim_header/memstack.h"
#include "slim_header/port.h"
#include "visitlog.h"
#include "lmntal_thread.h"
#include "delta_membrane.h"
#include "mc_worker.h"
#include "mc_generator.h"

#include "runtime_status.h"

LmnWord **wts, **wts_t;
LmnByte **ats, **ats_t;
LmnByte **tts, **tts_t;
LMN_TLS LmnWord *wt, *wt_t; /* variable vector used in interpret */
LMN_TLS LmnByte *at, *at_t; /* attribute vector */
LMN_TLS LmnByte *tt, *tt_t; /* attribute vector */
LMN_TLS unsigned int wt_size;
unsigned int trace_num = 0; /* for tracer (MT-Unsafe) */


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


inline static BOOL react_ruleset(struct ReactCxt *rc, LmnMembrane *mem, LmnRuleSet ruleset);
inline static BOOL react_ruleset_inner(struct ReactCxt *rc, LmnMembrane *mem, LmnRuleSet rs);
inline static void react_initial_rulesets(struct ReactCxt *rc, LmnMembrane *mem);
inline static BOOL react_ruleset_in_all_mem(struct ReactCxt *rc, LmnRuleSet rs, LmnMembrane *mem);
static BOOL react_ruleset_atomic_all(struct ReactCxt *main_rc,
                                     LmnMembrane     *main_mem,
                                     LmnRuleSet      main_at_set);
static BOOL react_ruleset_atomic_sync(struct ReactCxt *main_rc,
                                      LmnMembrane     *main_mem,
                                      LmnRuleSet      main_at_set);

static BOOL interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr);
static BOOL dmem_interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr);


static void mem_oriented_loop(struct ReactCxt *rc, LmnMembrane *mem);

#define WT_SIZE_DEF 1024

void lmn_dmem_interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr)
{
  dmem_interpret(rc, rule, instr);
}

void task_init()
{
  unsigned int i, n;
  n = lmn_env.core_num;
  wt_size = WT_SIZE_DEF;

  wts   = LMN_NALLOC(LmnWord *, n);
  ats   = LMN_NALLOC(LmnByte *, n);
  tts   = LMN_NALLOC(LmnByte *, n);
  wts_t = LMN_NALLOC(LmnWord *, n);
  ats_t = LMN_NALLOC(LmnByte *, n);
  tts_t = LMN_NALLOC(LmnByte *, n);

  for (i = 0; i < n; i++) {
    wts[i]   = LMN_NALLOC(LmnWord, wt_size);
    ats[i]   = LMN_NALLOC(LmnByte, wt_size);
    tts[i]   = LMN_NALLOC(LmnByte, wt_size);
    wts_t[i] = LMN_NALLOC(LmnWord, wt_size);
    ats_t[i] = LMN_NALLOC(LmnByte, wt_size);
    tts_t[i] = LMN_NALLOC(LmnByte, wt_size);
    memset(wts[i], 0U, sizeof(LmnWord) * wt_size);
    memset(ats[i], 0U, sizeof(LmnByte) * wt_size);
    memset(tts[i], 0U, sizeof(LmnByte) * wt_size);
    memset(wts_t[i], 0U, sizeof(LmnWord) * wt_size);
    memset(ats_t[i], 0U, sizeof(LmnByte) * wt_size);
    memset(tts_t[i], 0U, sizeof(LmnByte) * wt_size);
  }

  wt = NULL;
  at = NULL;
  tt = NULL;
  wt_t = NULL;
  at_t = NULL;
  tt_t = NULL;
}

/* React Contextへ使用する作業配列を割り当てる */
inline
void task_allocate_workspace(struct ReactCxt *rc)
{
  unsigned int cid = lmn_thread_id;
  wt_size = WT_SIZE_DEF;
  wt      = wts[cid];
  at      = ats[cid];
  tt      = tts[cid];
  wt_t    = wts_t[cid];
  at_t    = ats_t[cid];
  tt_t    = tts_t[cid];
}

void task_finalize()
{
  unsigned int i, n;
  n = lmn_env.core_num;
  for (i = 0; i < n; i++) {
    LMN_FREE(wts[i]);
    LMN_FREE(ats[i]);
    LMN_FREE(tts[i]);
    LMN_FREE(wts_t[i]);
    LMN_FREE(ats_t[i]);
    LMN_FREE(tts_t[i]);
  }

  LMN_FREE(wts);
  LMN_FREE(ats);
  LMN_FREE(tts);
  LMN_FREE(wts_t);
  LMN_FREE(ats_t);
  LMN_FREE(tts_t);
}

/** 通常実行時の入口.
 *  インタタラクティブ実行時の処理フローは以下の通り[yueno]
 *    1. 後始末     normal_cleaningフラグがたっている時
 *    2. 初期化     !(normal_remainモード時 && normal_remaining=ON) 時
 *    3. 処理       常に行う
 *    4. 後始末     通常モード時
 *    5. 継続フラグ 通常モードならON、normal_remainモードならOFFにセットする
 */
void lmn_run(Vector *start_rulesets)
{
  static LmnMembrane *mem;
  static struct ReactCxt mrc;

#ifdef TIME_OPT
  /* 通常実行では非決定実行とは異なりProcess IDを
   * 1から再割り当てする機会(状態圧縮と復元)が存在しない.
   * 破棄したProcessのIDを使い回す必要がある.
   * もし通常実行の並列化を考えるならばIDを再利用するためのMT-Safeな機構が必要 */
  if (!lmn_id_pool) {
    lmn_id_pool = vec_make(64);
  }
#endif

  /* interactive: normal_cleaningフラグがONの場合は後始末 */
  if (lmn_env.normal_cleaning) {
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
    mem_react_cxt_destroy(&mrc);
    lmn_env.normal_cleaning = FALSE;
  }

  /* interactive : (normal_remain時でnormal_remaining=ON)以外の場合は初期化 */
  if (!lmn_env.normal_remain && !lmn_env.normal_remaining) {
    mem_react_cxt_init(&mrc);
    mem = lmn_mem_make();
    RC_SET_GROOT_MEM(&mrc, mem);
  }
  lmn_memstack_push(RC_MEMSTACK(&mrc), mem);

  /** PROFILE START */
  if (lmn_env.profile_level >= 1) {
    profile_start_exec();
    profile_start_exec_thread();
  }

  react_start_rulesets(mem, start_rulesets);
  lmn_memstack_reconstruct(RC_MEMSTACK(&mrc), mem);
  mem_oriented_loop(&mrc, mem);

  /** PROFILE FINISH */
  if (lmn_env.profile_level >= 1) {
    profile_finish_exec_thread();
    profile_finish_exec();
  }
  if (lmn_env.dump) { /* lmntalではioモジュールがあるけど必ず実行結果を出力するプログラミング言語, で良い?? */
    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      fprintf(stdout, "finish.\n");
    } else {
      lmn_dump_cell_stdout(mem);
    }
  }
  if (lmn_env.show_hyperlink) lmn_hyperlink_print(mem);//seiji

  /* 後始末 */
  if (lmn_env.normal_remain) {
    lmn_env.normal_remaining = TRUE;
  } else {
    lmn_env.normal_remaining = FALSE;
    lmn_mem_drop(mem);
    lmn_mem_free(mem);
    mem_react_cxt_destroy(&mrc);
  }
#ifdef TIME_OPT
  if (lmn_id_pool) {
    vec_free(lmn_id_pool);
  }
#endif

}


/** 膜スタックに基づいた通常実行 */
static void mem_oriented_loop(struct ReactCxt *rc, LmnMembrane *mem)
{
  LmnMemStack memstack = RC_MEMSTACK(rc);

  while(!lmn_memstack_isempty(memstack)){
    LmnMembrane *mem = lmn_memstack_peek(memstack);
    if (!react_all_rulesets(rc, mem)) {
      /* ルールが何も適用されなければ膜スタックから先頭を取り除く */
      lmn_memstack_pop(memstack);
    }
  }
}


/** cur_memに存在するルールすべてに対して適用を試みる
 * @see mem_oriented_loop (task.c)
 * @see expand_inner      (nd.c) */
BOOL react_all_rulesets(struct ReactCxt *rc, LmnMembrane *cur_mem)
{
  unsigned int i;
  struct Vector rulesets = cur_mem->rulesets; /* 本膜のルールセットの集合 */
  BOOL ok = FALSE;

  /* ルールセットの適用 */
  for (i = 0; i < vec_num(&rulesets); i++) {
    if (react_ruleset(rc, cur_mem, (LmnRuleSet)vec_get(&rulesets, i))) {
      /* ndでは失敗するまでマッチングバックトラックしているので必ずFALSEが返ってくる */
      ok = TRUE;
      break;
    }
  }

  /* 通常実行では, 適用が発生しなかった場合にシステムルールの適用を行う
   * ndではokはFALSEなので, system_rulesetが適用される. */
  ok = ok || react_ruleset_inner(rc, cur_mem, system_ruleset);

  return ok;
}


/** 膜memに対してルールセットrsの各ルールの適用を試みる.
 *  戻り値:
 *   通常実行では, 書換えに成功した場合にTRUE, マッチングしなかった場合にFALSEを返す.
 *   非決定実行では常にFALSEを返す(マッチングに失敗するまでバックトラックする仕様). */
inline static BOOL react_ruleset(struct ReactCxt *rc,
                                 LmnMembrane *mem,
                                 LmnRuleSet rs)
{
  BOOL result;

  if (RC_IS_ATOMIC_STEP(rc)) {
    /* atomic_step時 */
    if (lmn_ruleset_is_valid_atomic(rs)) {
      /* 目的のidのatomic setが有効である場合 */
      result = react_ruleset_inner(rc, mem, rs);
      result = react_ruleset_inner(rc, mem, system_ruleset);
    } else {
      /* 子膜のstable検査を誤判定する恐れがあるかも..
       * TRUEを返すと以降のルールセット適用がskipされてしまう */
      return FALSE;
    }
  }
  else {
    /* 通常時 */
    switch (lmn_ruleset_atomic_type(rs)) {
    case ATOMIC_NONE:
      result = react_ruleset_inner(rc, mem, rs);
      break;
    case ATOMIC_ALL_EXHAUSTIVE:
      result = react_ruleset_atomic_all(rc, mem, rs);
      break;
    case ATOMIC_EACH_SYNC:
      result = react_ruleset_atomic_sync(rc, mem, rs);
      break;
    default:
      lmn_fatal("unexpected react testing");
    }
  }

  return result;
}


/**  @see react_ruleset (task.c)  */
inline static BOOL react_ruleset_inner(struct ReactCxt *rc,
                                       LmnMembrane *mem,
                                       LmnRuleSet rs)
{
  unsigned int i;
  BOOL ret = FALSE;
  for (i = 0; i < lmn_ruleset_rule_num(rs); i++) {
    LmnRule r = lmn_ruleset_get_rule(rs, i);
#ifdef PROFILE
    if (!lmn_env.nd && lmn_env.profile_level >= 2) {
      profile_rule_obj_set(rs, r);
    }
#endif
    if (react_rule(rc, mem, r)) {
      ret = TRUE;
      break;
    }
  }
  return ret;
}


/** 膜memに対してルールruleの適用を試みる.
 *  戻り値:
 *   通常実行では, 書換えに成功した場合にTRUE, マッチングしなかった場合にFALSEを返す.
 *   非決定実行では, マッチングに失敗するまでバックトラックを繰り返すため常にFALSEが返る. */
BOOL react_rule(struct ReactCxt *rc, LmnMembrane *mem, LmnRule rule)
{
  LmnTranslated translated;
  BYTE *inst_seq;
  BOOL result;

  translated = lmn_rule_get_translated(rule);
  inst_seq = lmn_rule_get_inst_seq(rule);

  wt[0] = (LmnWord)mem;
  tt[0] = TT_MEM;
  RC_SET_WORK_VEC_SIZE(rc, 1);

  profile_start_trial();

  /* まず、トランスレート済みの関数を実行する
     それがない場合命令列をinterpretで実行する */
  result = (translated && translated(rc, mem, rule)) ||
           (inst_seq   && interpret(rc, rule, inst_seq));

  profile_finish_trial();

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    if (lmn_env.trace && result) {
      if (lmn_env.sp_dump_format == LMN_SYNTAX) {
        lmn_dump_mem_stdout(RC_GROOT_MEM(rc));
        fprintf(stdout, ".\n");
        lmn_dump_mem_stdout(RC_GROOT_MEM(rc));
        fprintf(stdout, ":- ");
        trace_num++;
      }
      else {
        if (trace_num != 0) {
          fprintf(stdout, "---->%s\n", lmn_id_to_name(lmn_rule_get_name(rule)));
        }
        fprintf(stdout, "%d: ", trace_num++);
        lmn_dump_cell_stdout(RC_GROOT_MEM(rc));
        if (lmn_env.show_hyperlink) lmn_hyperlink_print(mem);//seiji
      }
    }
  }
  if (sameproccxt) lmn_sameproccxt_clear();//seiji //とりあえずここに配置

  return result;
}


/* ルールセットat_setの各ルールを可能な限り適用する.
 * 非決定実行時には状態遷移のサブグラフを構築し,
 * その停止状態をmemの遷移先として生成し, rcに登録する. */
static BOOL react_ruleset_atomic_all(struct ReactCxt *rc,
                                     LmnMembrane     *mem,
                                     LmnRuleSet      at_set)
{
  BOOL ret = FALSE;
  lmn_ruleset_validate_atomic(at_set);

  if (RC_GET_MODE(rc, REACT_ND)) {
    /* 概要:
     *  状態空間構築処理をサブルーチンとして実行する.
     *  マッチングのたびに初期化処理などを行うと性能に影響するため
     *  1段階だけ通常通りのルール適用で状態を展開する.
     *  展開した各状態を基点にsubgraphを構築する.　*/
    unsigned int start_succ_num, atomic_init_num;

    if (RC_ND_DELTA_ENABLE(rc)) {
      lmn_fatal("unsupported delta-membrane, atomic step");
    }

    /* 準備:
     *   遷移元となる状態のatomic_setにatomicフラグを有効ににする
     *   1step展開した際のサクセッサはルールコピー時にフラグを引き継ぐ  */

    start_succ_num  = mc_react_cxt_expanded_num(rc);
    react_ruleset_inner(rc, mem, at_set);
    atomic_init_num = mc_react_cxt_expanded_num(rc) - start_succ_num;

    /* 遷移先状態が1つ以上存在したならばsubgraphの構築を行う */
    if (atomic_init_num > 0) {
      LmnWorker       *w;
      StateSpace       sub_states;
      const Vector    *sub_ends;
      unsigned int     i;
      BYTE             sub_flags;

      /* 1st: subgraph構築に必要なオプションを設定 */
      sub_flags = 0x00U;
      mc_set_compress(sub_flags);
      mc_set_compact(sub_flags);

      /* 2nd: subgraphの状態管理表と, ルール適用コンテキストを設定 */
      sub_states = state_space_make();
      w = lmn_worker_make(sub_states, lmn_thread_id, sub_flags);
      dfs_env_set(w);
      WORKER_INIT(w);
      mc_react_cxt_init(&WORKER_RC(w), DEFAULT_STATE_ID);

      RC_START_ATOMIC_STEP(&WORKER_RC(w), lmn_ruleset_get_id(at_set));

      /* 3rd: LmnWorkerの構築 */
      for (i = 0; i < atomic_init_num; i++) {
        State *sub_s, *sub_t;
        LmnMembrane *sub_m;

        /* TODO: mainルーチンがdelta-memの場合の処理をまだ書いていない */
        sub_m = (LmnMembrane *)mc_react_cxt_expanded_pop(rc);
        sub_s = state_make(sub_m, DEFAULT_STATE_ID,
                           state_space_use_memenc(sub_states));
        sub_t = state_space_insert(sub_states, sub_s);

        if (sub_s != sub_t) { /* duplicate detected! */
          state_free(sub_s);
          continue;
        } else {              /* new */
          sub_states->init_state = sub_s; /* たぶん不要 */
          dfs_start(w);
        }
      }
      RC_FINISH_ATOMIC_STEP(rc);

      sub_ends = state_space_end_states(sub_states);
      for (i = 0; i < vec_num(sub_ends); i++) {
        /* 最終状態をrestoreしてactivateしてrcに追加 */
        State       *end_s;
        LmnMembrane *end_m;

        end_s = (State *)vec_get(sub_ends, i);
        end_m = lmn_binstr_decode(state_mem_binstr(end_s));
        /* decode処理ではactivateフラグが立つ @see mem_encode.c */
        mc_react_cxt_add_expanded(rc, end_m, dummy_rule());
      }

      /* sub_graphたちを破棄 */
      state_space_free(sub_states);
      mc_react_cxt_destroy(&WORKER_RC(w));
      lmn_worker_free(w);
    }
  }
  else if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    unsigned int i;
    BOOL reacted_once = FALSE;

    while (TRUE) {
      BOOL reacted = FALSE;
      for (i = 0; i < lmn_ruleset_rule_num(at_set); i++) {
        reacted = reacted || react_rule(rc, mem, lmn_ruleset_get_rule(at_set, i));
      }
      if (reacted) {
        react_ruleset_inner(rc, mem, system_ruleset);
      }
      reacted_once = reacted_once || reacted;
      if (!reacted) break;
    }
    ret = reacted_once;
  }
  else {
    lmn_fatal("unexpected.");
  }

  lmn_ruleset_invalidate_atomic(at_set);
  return ret;
}


/* ルールセットat_setの各ルールを手続き型言語のstatement処理のように1stepずつ適用する.
 * 適用に失敗したルールはskipし, 次のルール適用を行う.
 * 非決定実行時には, ルール間のインタリーブを無視し, ルール内の非決定性のみを取得する. */
static BOOL react_ruleset_atomic_sync(struct ReactCxt *rc,
                                      LmnMembrane     *mem,
                                      LmnRuleSet      at_set)
{
  BOOL ret = FALSE;

  lmn_ruleset_validate_atomic(at_set);

  if (RC_GET_MODE(rc, REACT_ND)) {
    unsigned int r_i, r_n, start_succ_num, atomic_init_num;

    if (RC_ND_DELTA_ENABLE(rc)) {
      lmn_fatal("unsupported delta-membrane, atomic step");
    }

    start_succ_num  = mc_react_cxt_expanded_num(rc);
    r_n = lmn_ruleset_rule_num(at_set);
    atomic_init_num = 0;
    for (r_i = 0; (r_i < r_n) && (atomic_init_num == 0); r_i++) {
      react_rule(rc, mem, lmn_ruleset_get_rule(at_set, r_i));
      atomic_init_num = mc_react_cxt_expanded_num(rc) - start_succ_num;
    }

    if (atomic_init_num > 0) {
      struct ReactCxt sub_rc;
      Vector *primary, *secondary, *swap;
      unsigned int i;

      mc_react_cxt_init(&sub_rc, DEFAULT_STATE_ID);
      primary   = vec_make(32);
      secondary = vec_make(32);

      /* at_setで生成したmembraneを取り出す */
      for (i = 0; i < atomic_init_num; i++) {
        LmnMembrane *sub_m = (LmnMembrane *)mc_react_cxt_expanded_pop(rc);
        vec_push(primary, (vec_data_t)sub_m);
      }

      for (i = r_i; i < r_n; i++) {
        while (vec_num(primary) > 0) {
          LmnMembrane *m = (LmnMembrane *)vec_pop(primary);
          RC_SET_GROOT_MEM(&sub_rc, m);
          react_rule(&sub_rc, m, lmn_ruleset_get_rule(at_set, i));

          if (mc_react_cxt_expanded_num(&sub_rc) == 0) {
            vec_push(secondary, (vec_data_t)m);
          } else {
            while (mc_react_cxt_expanded_num(&sub_rc) > 0) {
              vec_push(secondary, (vec_data_t)mc_react_cxt_expanded_pop(&sub_rc));
            }
            lmn_mem_drop(m);
            lmn_mem_free(m);
          }
        }

        swap      = primary;
        primary   = secondary;
        secondary = swap;
      }

      while (vec_num(primary) > 0) {
        mc_react_cxt_add_expanded(rc, (LmnMembrane *)vec_pop(primary), dummy_rule());
      }

      vec_free(primary);
      vec_free(secondary);
      mc_react_cxt_destroy(&sub_rc);
    }
  }
  else if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    unsigned int i;

    for (i = 0; i < lmn_ruleset_rule_num(at_set); i++) {
      if (react_rule(rc, mem, lmn_ruleset_get_rule(at_set, i))) {
        ret = TRUE;
        react_ruleset_inner(rc, mem, system_ruleset);
      }
    }
  }
  else {
    lmn_fatal("unexpected.");
  }

  lmn_ruleset_invalidate_atomic(at_set);
  return ret;
}


/* 膜memでrulesetsのルールの適用を行う.
 * 適用結果は無視する */
void react_start_rulesets(LmnMembrane *mem, Vector *rulesets)
{
  struct ReactCxt rc;
  int i;

  stand_alone_react_cxt_init(&rc);
  RC_SET_GROOT_MEM(&rc, mem);

  for (i = 0; i < vec_num(rulesets); i++) {
    react_ruleset(&rc, mem, (LmnRuleSet)vec_get(rulesets, i));
  }
  react_initial_rulesets(&rc, mem);
}


inline static void react_initial_rulesets(struct ReactCxt *rc, LmnMembrane *mem)
{
  int i;
  BOOL reacted;

  do {
    reacted = FALSE;
    if (react_ruleset_in_all_mem(rc, initial_system_ruleset, mem)) {
      reacted = TRUE;
      continue;
    }
    for(i = 0; i < lmn_ruleset_rule_num(initial_ruleset); i++){
      if (react_rule(rc, mem, lmn_ruleset_get_rule(initial_ruleset, i))) {
        reacted = TRUE;
        break;
      }
    }
  } while (reacted);
}

/* ルールセットrsをmem以下のすべての膜内で適用する */
static BOOL react_ruleset_in_all_mem(struct ReactCxt *rc, LmnRuleSet rs, LmnMembrane *mem)
{
  LmnMembrane *m;

  for (m = mem->child_head; m; m = m->next) {
    if (react_ruleset_in_all_mem(rc, rs, m)) return TRUE;
  }

  return react_ruleset(rc, mem, rs);
}

/* Utility for reading data */

#define READ_DATA_ATOM(dest, attr)                            \
  do {                                                        \
    switch (attr) {                                           \
    case LMN_INT_ATTR:                                        \
      READ_VAL(long, instr, (dest));                          \
      break;                                                  \
    case LMN_DBL_ATTR:                                        \
    {                                                         \
      double *x;                                              \
      x = LMN_MALLOC(double);                                 \
      READ_VAL(double, instr, *x);                            \
      (dest) = (LmnWord)x;                                    \
      break;                                                  \
    }                                                         \
    case LMN_STRING_ATTR:                                     \
    {                                                         \
      lmn_interned_str s;                                     \
      READ_VAL(lmn_interned_str, instr, s);                   \
      (dest) = (LmnWord)lmn_string_make(lmn_id_to_name(s));   \
      break;                                                  \
    }                                                         \
    default:                                                  \
      lmn_fatal("Implementation error");                      \
    }                                                         \
  } while (0)

/* attrに応じて、ファンクタを読み込み、destに定数アトムを生成する。
   attrは適切な値に変更する場合がある */
#define READ_CONST_DATA_ATOM(dest, attr, type)                \
  do {                                                        \
    switch (attr) {                                           \
    case LMN_INT_ATTR:                                        \
       READ_VAL(long, instr, (dest));                         \
       break;                                                 \
     case LMN_DBL_ATTR:                                       \
       (dest) = (LmnWord)instr;                               \
       instr += sizeof(double);                               \
        (attr) = LMN_CONST_DBL_ATTR;                          \
       break;                                                 \
     case LMN_STRING_ATTR:                                    \
     {                                                        \
        lmn_interned_str s;                                   \
        READ_VAL(lmn_interned_str, instr, s);                 \
        (dest) = s;                                           \
        (attr) = LMN_CONST_STR_ATTR;                          \
        break;                                                \
     }                                                        \
     default:                                                 \
        lmn_fatal("Implementation error");                    \
     }                                                        \
     (type) = TT_OTHER;                                       \
   } while (0)

#define READ_CMP_DATA_ATOM(attr, x, result, type)             \
  do {                                                        \
    switch(attr) {                                            \
    case LMN_INT_ATTR:                                        \
    {                                                         \
      long t;                                                 \
      READ_VAL(long, instr, t);                               \
      (result) = ((long)(x) == t);                            \
      break;                                                  \
    }                                                         \
    case LMN_DBL_ATTR:                                        \
    {                                                         \
      double t;                                               \
      READ_VAL(double, instr, t);                             \
      (result) = (*(double*)(x) == t);                        \
      break;                                                  \
    }                                                         \
    case LMN_STRING_ATTR:                                     \
    {                                                         \
      lmn_interned_str s;                                     \
      LmnString str1;                                         \
      READ_VAL(lmn_interned_str, instr, s);                   \
      str1 = lmn_string_make(lmn_id_to_name(s));              \
      (result) = lmn_string_eq(str1, (LmnString)(x));         \
      lmn_string_free(str1);                                  \
      break;                                                  \
    }                                                         \
    default:                                                  \
      lmn_fatal("Implementation error");                      \
    }                                                         \
    (type) = TT_ATOM;                                         \
  } while (0)

#define SKIP_DATA_ATOM(attr)                                  \
  do {                                                        \
    switch(attr) {                                            \
    case LMN_INT_ATTR:                                        \
    {                                                         \
      long t;                                                 \
      READ_VAL(long, instr, t);                               \
      break;                                                  \
    }                                                         \
    case LMN_DBL_ATTR:                                        \
    {                                                         \
      double t;                                               \
      READ_VAL(double, instr, t);                             \
      break;                                                  \
    }                                                         \
    case LMN_STRING_ATTR:                                     \
    {                                                         \
      lmn_interned_str s;                                     \
      READ_VAL(lmn_interned_str, instr, s);                   \
      break;                                                  \
    }                                                         \
    default:                                                  \
      lmn_fatal("Implementation error");                      \
    }                                                         \
  } while (0)

/* DEBUG: */
/* static void print_wt(void); */

/* mem != NULL ならば memにUNIFYを追加、そうでなければ
   UNIFYは膜に所属しない */
inline HashSet *insertconnectors(LmnMembrane *mem, const Vector *links)
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

	if (LMN_SATOM_ID(eq) == 0) {
	  LMN_SATOM_SET_ID(eq, env_gen_next_id());
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
  LmnInstrOp op;

  while (TRUE) {
  LOOP:;
    READ_VAL(LmnInstrOp, instr, op);

    switch (op) {
    case INSTR_SPEC:
    {
      LmnInstrVar s0, s1;

      READ_VAL(LmnInstrVar, instr, s0);
      READ_VAL(LmnInstrVar, instr, s1);

      /* extend vector if need */
#ifdef DEBUG
      if (s1 > (0x01U << (sizeof(LmnInstrOp) * 8))) {
        lmn_fatal("The number of instructions exceeded the limit");
      }
#endif
      if (s1 > wt_size) {
      //  s1 = wt_size = round2up(s1)
        wt_size = s1;
        wt = LMN_REALLOC(LmnWord, wt, wt_size);
        at = LMN_REALLOC(LmnByte, at, wt_size);
        tt = LMN_REALLOC(LmnByte, tt, wt_size);
      }
      if (s1 > RC_WORK_VEC_SIZE(rc)) {
        memset(tt + RC_WORK_VEC_SIZE(rc), 0U, s1 - RC_WORK_VEC_SIZE(rc));
      }
      RC_SET_WORK_VEC_SIZE(rc, s1);
      break;
    }
    case INSTR_INSERTCONNECTORSINNULL:
    {
      LmnInstrVar seti, list_num;
      Vector links;
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num);
      for (i = 0; i < list_num; i++) {
        LmnInstrVar t;
        READ_VAL(LmnInstrVar, instr, t);
        vec_push(&links, (LmnWord)t);
      }

      wt[seti] = (LmnWord)insertconnectors(NULL, &links);
      tt[seti] = TT_OTHER;

      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if(interpret(rc, rule, instr)) {
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      } else LMN_ASSERT(0);

      break;
    }
    case INSTR_INSERTCONNECTORS:
    {
      LmnInstrVar seti, list_num, memi, enti;
      Vector links; /* src list */
      unsigned int i;

      READ_VAL(LmnInstrVar, instr, seti);
      READ_VAL(LmnInstrVar, instr, list_num);

      vec_init(&links, list_num + 1); /* avoid 0 byte malloc */

      for (i = 0; i < list_num; i++) {
        READ_VAL(LmnInstrVar, instr, enti);
        vec_push(&links, (LmnWord)enti);
      }

      READ_VAL(LmnInstrVar, instr, memi);
      wt[seti] = (LmnWord)insertconnectors((LmnMembrane *)wt[memi], &links);
      tt[seti] = TT_OTHER;
      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if (interpret(rc, rule, instr)) {
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      } else LMN_ASSERT(0);
      break;
    }
    case INSTR_JUMP:
    {
      /* EFFICIENCY: 新たに作業配列をmallocしているので非常に遅い
                     -O3 で生成される中間命令にJUMPが含まれないため
                     これでもよい */
      LmnWord *wt_org, *wt2;
      LmnByte *at_org, *at2, *tt_org, *tt2;
      unsigned int rc_work_size_org;
      unsigned int wt_size_org;
      LmnRuleInstr next;
      LmnInstrVar num, i, n;
      LmnJumpOffset offset;
      BOOL ret;

      rc_work_size_org = RC_WORK_VEC_SIZE(rc);
      wt_size_org = wt_size;
      wt_org = wt;
      at_org = at;
      tt_org = tt;
      wt2 = LMN_NALLOC(LmnWord, wt_size_org);
      at2 = LMN_NALLOC(LmnByte, wt_size_org);
      tt2 = LMN_NALLOC(LmnByte, wt_size_org);

      /**
       * MC -->
       */
      if (RC_GET_MODE(rc, REACT_ND)) {
        for(i = 0; i < rc_work_size_org; i++) {
          wt2[i] = at2[i] = tt2[i] = 0;
        }
      }
      /**
       * <-- MC
       */

      READ_VAL(LmnJumpOffset, instr, offset);
      next = instr + offset;

      i = 0;
      /* atom */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
        tt2[i] = tt[n];
      }
      /* mem */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
        tt2[i] = tt[n];
      }
      /* vars */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt2[i] = wt[n];
        at2[i] = at[n];
        tt2[i] = tt[n];
      }

      instr = next;

      wt = wt2;
      at = at2;
      tt = tt2;

      ret = interpret(rc, rule, instr);
      LMN_FREE(wt);
      LMN_FREE(at);
      LMN_FREE(tt);
      wt = wt_org;
      at = at_org;
      tt = tt_org;
      wt_size = wt_size_org;
      RC_SET_WORK_VEC_SIZE(rc, rc_work_size_org);

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
        tt_t[i] = tt[n];
      }

      /* mem */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt_t[i] = wt[n];
        at_t[i] = at[n];
        tt_t[i] = tt[n];
      }

      /* vars */
      READ_VAL(LmnInstrVar, instr, num);
      for (; num--; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        wt_t[i] = wt[n];
        at_t[i] = at[n];
        tt_t[i] = tt[n];
      }

      for (t=0; t<=i; t++) {
        wt[t] = wt_t[t];
        at[t] = at_t[t];
        tt[t] = tt_t[t];
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
      if (RC_GET_MODE(rc, REACT_ND)) {
        unsigned int org_next_id = env_next_id();
        if (RC_ND_DELTA_ENABLE(rc)) {
          /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
          /** >>>>>>>> enable delta-membrane <<<<<<< **/
          /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
          struct MemDeltaRoot *d = dmem_root_make(RC_GROOT_MEM(rc), rule, env_next_id());
          RC_ND_SET_MEM_DELTA_ROOT(rc, d);
          dmem_interpret(rc, rule, instr);
          dmem_root_finish(d);
          mc_react_cxt_add_mem_delta(rc, d, rule);
          RC_ND_SET_MEM_DELTA_ROOT(rc, NULL);
        }
        else {
          /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
          /** >>>>>>> disable delta-membrane <<<<<<< **/
          /** >>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<< **/
          unsigned int i, n;
          LmnWord *wtcp;
          LmnByte *atcp, *ttcp;
          ProcessTbl copymap;
          LmnMembrane *tmp_global_root;
          LmnWord t;
          unsigned int wt_size_org, work_size_org;

#ifdef PROFILE
          if (lmn_env.profile_level >= 3) {
            profile_start_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
          }
#endif

          t = 0;
          wt_size_org = wt_size;
          work_size_org = RC_WORK_VEC_SIZE(rc);
          tmp_global_root = lmn_mem_copy_with_map(RC_GROOT_MEM(rc), &copymap);

          /** 変数配列および属性配列のコピー */
          wtcp = LMN_NALLOC(LmnWord, wt_size);
          atcp = LMN_NALLOC(LmnByte, wt_size);
          ttcp = LMN_NALLOC(LmnByte, wt_size);
#ifdef TIME_OPT
          n = RC_WORK_VEC_SIZE(rc);
#else
          n = wt_size;
#endif
          for(i = 0; i < n; i++) {
            wtcp[i] = atcp[i] = ttcp[i] = 0;
          }

          /** copymapの情報を基に変数配列を書換える */
#ifdef TIME_OPT
          for (i = 0; i < n; i++) {
            atcp[i] = at[i];
            ttcp[i] = tt[i];
            if (tt[i] == TT_ATOM) {
              if(LMN_INT_ATTR == at[i]) { /* intのみポインタでないため */
                wtcp[i] = wt[i];
              } else if (at[i] == LMN_DBL_ATTR) {
                double *d = (double *)wtcp[i];
                LMN_COPY_DBL_ATOM(d, wt[i]);
                wtcp[i] = (LmnWord)d;
              } else { /* symbol atom */
                if (proc_tbl_get_by_atom(copymap, LMN_SATOM(wt[i]), &t)) {
                  wtcp[i] = (LmnWord)t;
                } else {
                  lmn_fatal("implementation error");
                }
              }
            }
            else if (tt[i] == TT_MEM) {
              if(wt[i] == (LmnWord)RC_GROOT_MEM(rc)) { /* グローバルルート膜 */
                wtcp[i] = (LmnWord)tmp_global_root;
              } else {
                if (proc_tbl_get_by_mem(copymap, (LmnMembrane *)wt[i], &t)) {
                  wtcp[i] = (LmnWord)t;
                }
                else {
                  lmn_fatal("implementation error");
                }
              }
            } else {
              wtcp[i] = wt[i];
            }
          }
#else
          for (i = 0; i < n; i++) {
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
#endif
          proc_tbl_free(copymap);

          /** 変数配列および属性配列をコピーと入れ換え, コピー側を書き換える */
          SWAP(LmnWord *, wtcp, wt);
          SWAP(LmnByte *, atcp, at);
          SWAP(LmnByte *, ttcp, tt);
#ifdef PROFILE
          if (lmn_env.profile_level >= 3) {
            profile_finish_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
          }
#endif

          /** コピーしたグローバルルート膜と作業配列を用いてBODY命令を適用  */
          interpret(rc, rule, instr);
          mc_react_cxt_add_expanded(rc, tmp_global_root, rule);

          if (lmn_rule_get_history_tbl(rule) && lmn_rule_get_pre_id(rule) != 0) {
            st_delete(lmn_rule_get_history_tbl(rule), lmn_rule_get_pre_id(rule), 0);
          }

          /* 変数配列および属性配列を元に戻す（いらないかも？） */
          SWAP(LmnWord *, wtcp, wt);
          SWAP(LmnByte *, atcp, at);
          SWAP(LmnByte *, ttcp, tt);

          LMN_FREE(wtcp);
          LMN_FREE(atcp);
          LMN_FREE(ttcp);

          RC_SET_WORK_VEC_SIZE(rc, work_size_org);
          wt_size = wt_size_org;
        }

        env_set_next_id(org_next_id);
        return FALSE; /* matching backtrack! */
      }
      else if (RC_GET_MODE(rc, REACT_PROPERTY)) {
        return TRUE;  /* propertyはmatchingのみ */
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
        lmn_fatal("I can not find data atoms.\n");
      } else { /* symbol atom */
        LmnFunctor f;
        AtomListEntry *atomlist_ent;
        LmnSAtom atom;

        READ_VAL(LmnFunctor, instr, f);

        if (!lmn_hyperlink_opt(atomi)) {//seiji
          atomlist_ent = lmn_mem_get_atomlist((LmnMembrane*)wt[memi], f);
          if (atomlist_ent) {
            EACH_ATOM(atom, atomlist_ent, ({
              wt[atomi] = (LmnWord)atom;
              at[atomi] = LMN_ATTR_MAKE_LINK(0);
              tt[atomi] = TT_ATOM;

              if (interpret(rc, rule, instr)) {
                return TRUE;
              }
              profile_backtrack();
            }));
          }
        } else {
          /* hyperlink の接続関係を利用したルールマッチング最適化 */
          SameProcCxt *spc;
          LmnSAtom linked_pc;
          int i, atom_arity, element_num;

          atom_arity = LMN_FUNCTOR_ARITY(f);

          /* 型付きプロセス文脈atomiがoriginal/cloneのどちらであるか判別 */
          spc = (SameProcCxt *)hashtbl_get(sameproccxt, (HashKeyType)atomi);

          if (lmn_sameproccxt_from_clone(spc, atom_arity)) { /** hyperlinkの接続関係からfindatom */

            /* 探索の始点となるhyperlinkと同じ集合に含まれるhyperlink群を(Vector*)LMN_SPC_TREE(spc)に取得 */
            lmn_hyperlink_get_elements(LMN_SPC_TREE(spc), lmn_sameproccxt_start(spc, atom_arity));
            /* (Vector*)LMN_SPC_TREE(spc)の最後に格納されているHyperLinkは探索の対象外なので要素数は-1される */
            if ((element_num = vec_num(LMN_SPC_TREE(spc)) - 1) <= 0) return FALSE;
            /* ----------------------------------------------------------------------- *
             * この時点で探索の始点とすべきハイパーリンクの情報がspc内に格納されている *
             * ----------------------------------------------------------------------- */

            if (lmn_mem_get_atomlist((LmnMembrane *)wt[memi], LMN_HL_FUNC)) { // ハイパーリンクアトムが膜内にあるか
              at[atomi] = LMN_ATTR_MAKE_LINK(0);
              tt[atomi] = TT_ATOM;
              for (i = 0; i < element_num; i++) {
                linked_pc = ((HyperLink *)vec_get(LMN_SPC_TREE(spc), i))->atom;
                wt[atomi] = LMN_SATOM_GET_LINK(linked_pc, 0);
                /*    本来findatomするはずだったファンクタと一致しているか
                 * || hyperlinkアトムの接続先の引数が正しいか
                 * || 本来findatomするはずだったアトムと所属膜が一致しているか */
                if (LMN_SATOM_GET_FUNCTOR(wt[atomi]) != f
                    || LMN_SPC_SATTR(spc) != LMN_SATOM_GET_ATTR(linked_pc, 0)
                    || LMN_HL_MEM(lmn_hyperlink_at_to_hl(linked_pc)) != (LmnMembrane *)wt[memi])
                  continue;

                if (lmn_sameproccxt_all_pc_check_clone(spc, LMN_SATOM(wt[atomi]), atom_arity)
                    && interpret(rc, rule, instr)) {
                  return TRUE;
                }
                profile_backtrack();
              }
            }
          } else { /* 通常のfindatom */
            atomlist_ent = lmn_mem_get_atomlist((LmnMembrane*)wt[memi], f);
            if (atomlist_ent) {
              EACH_ATOM(atom, atomlist_ent, ({
                wt[atomi] = (LmnWord)atom;
                at[atomi] = LMN_ATTR_MAKE_LINK(0);
                tt[atomi] = TT_ATOM;

                if (lmn_sameproccxt_all_pc_check_original(spc, atom, atom_arity) /* 次の */
                    && interpret(rc, rule, instr)) {
                  return TRUE;
                }
                profile_backtrack();
              }));
            }
          }
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
        lmn_fatal("I can not find data atoms.\n");
      } else { /* symbol atom */
        LmnFunctor f;
        AtomListEntry *atomlist_ent;
        LmnSAtom start_atom, atom, record;

        READ_VAL(LmnFunctor, instr, f);
        atomlist_ent = lmn_mem_get_atomlist((LmnMembrane*)wt[memi], f);
        if (atomlist_ent) {
          at[atomi] = LMN_ATTR_MAKE_LINK(0);
          tt[atomi] = TT_ATOM;
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
            if(LMN_SATOM_GET_FUNCTOR(atom) == LMN_RESUME_FUNCTOR)
              continue;
            wt[atomi] = (LmnWord)atom;
            tt[atomi] = TT_ATOM;
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
            profile_backtrack();
          }

          /* 現在のfindatom2の実装にはバグがある（cf. 言語班Wiki
             findatom2議論）。バグを回避するために、履歴アトムの後ろの
             アトムすべてからのマッチングに失敗した場合、履歴アトムの前
             のアトムに対してマッチングを試みる */
          EACH_ATOM(atom, atomlist_ent, ({
            if (atom == start_atom) break;
            wt[atomi] = (LmnWord)atom;
	    tt[atomi] = TT_ATOM;
            if (interpret(rc, rule, instr)) {
              return TRUE;
            }
            profile_backtrack();
          }));

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
      tt[mem] = TT_MEM;
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

      tt[mem1] = TT_MEM;
      mp = ((LmnMembrane*)wt[mem2])->child_head;
      while (mp) {
        wt[mem1] = (LmnWord)mp;
        if (RC_GET_MODE(rc, REACT_ND)) { at[mem1] = 0; /* MC */ }
        if (mp->name == memn && interpret(rc, rule, instr)) {
          return TRUE;
        }
        mp = mp->next;
        profile_backtrack();
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
      tt[atomi] = TT_ATOM;
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
      tt[link] = TT_ATOM;
      break;
    }
    case INSTR_UNIFYLINKS:
    {
      LmnInstrVar link1, link2, mem;
      LmnLinkAttr attr1, attr2;//seiji

      READ_VAL(LmnInstrVar, instr, link1);
      READ_VAL(LmnInstrVar, instr, link2);
      READ_VAL(LmnInstrVar, instr, mem);

      attr1 = LINKED_ATTR(link1);//seiji
      attr2 = LINKED_ATTR(link2);//seiji

      if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {
        if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 1, 2 are data */
          lmn_mem_link_data_atoms((LmnMembrane *)wt[mem], wt[link1], at[link1], LINKED_ATOM(link2), attr2);
        }
        else { /* 1 is data */
          LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(attr2), LINKED_ATOM(link1));
          LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(attr2), attr1);
        }
      }
      else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 2 is data */
        LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(attr1), LINKED_ATOM(link2));
        LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(attr1), attr2);
      }
      else { /* 1, 2 are symbol atom */

        if (LMN_ATTR_IS_EX(attr1)) {
          if (LMN_ATTR_IS_EX(attr2)) { /* 1, 2 are ex */
            lmn_newlink_with_ex(LMN_SATOM(LINKED_ATOM(link1)), attr1, 0, // ex atom ⊂ unary atom
                                LMN_SATOM(LINKED_ATOM(link2)), attr2, 0);
          } else { /* 1 is ex */
            lmn_newlink_with_ex(LMN_SATOM(LINKED_ATOM(link1)), attr1, 0,
                                LMN_SATOM(LINKED_ATOM(link2)), attr2, attr2);
          }
        } else if (LMN_ATTR_IS_EX(attr2)) { /* 2 is ex */
          lmn_newlink_with_ex(LMN_SATOM(LINKED_ATOM(link1)), attr1, attr1,
                              LMN_SATOM(LINKED_ATOM(link2)), attr2, 0);
        } else {
          LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(attr1), LINKED_ATOM(link2));
          LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(attr2), LINKED_ATOM(link1));
          LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(attr1), attr2);
          LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(attr2), attr1);
        }

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

      if(LMN_ATTR_IS_DATA_WITHOUT_EX(at[atom1]) && LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {//seiji
#ifdef DEBUG
        fprintf(stderr, "Two data atoms are connected each other.\n");
#endif
      }
      else if (LMN_ATTR_IS_DATA_WITHOUT_EX(at[atom1])) {//seiji
        LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);
        LMN_SATOM_SET_ATTR(ap, attr, at[atom1]);
      }
      else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {//seiji
        LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);
        LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);
      }
      else {
        if (!LMN_ATTR_IS_EX(at[atom1]) && !LMN_ATTR_IS_EX(attr)) {//seiji
          LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);
          LMN_SATOM_SET_ATTR(ap, attr, pos1);
          LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);
          LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);
        } else {//seiji
          lmn_newlink_with_ex(LMN_SATOM(wt[atom1]), at[atom1], pos1, ap, attr, 0);
        }
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

      if (LMN_ATTR_IS_DATA(at[atomi]) && LMN_ATTR_IS_DATA(LINKED_ATTR(linki))) {
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
      tt[linki] = TT_ATOM;

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
      tt[newmemi] = TT_MEM;
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
 //     tt[dstmemi] = TT_MEM;
      break;
    }
    case INSTR_REMOVEATOM:
    {
      LmnInstrVar atomi, memi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      lmn_mem_remove_atom((LmnMembrane *)wt[memi], wt[atomi], at[atomi]);

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
        lmn_mem_activate_ancestors((LmnMembrane *)wt[memi]); /* MC */
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

      lmn_mem_add_ruleset((LmnMembrane *)wt[memi], lmn_ruleset_from_id(id));
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
      tt[atom1] = TT_ATOM;
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
      tt[atom1] = TT_ATOM;
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
          READ_CMP_DATA_ATOM(attr, wt[atomi], eq, tt[atomi]);
          if (!eq) return FALSE;
        }
        else /* symbol atom */
          {
            READ_VAL(LmnFunctor, instr, f);
            if (LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt[atomi])) != f) {
              return FALSE;
            }
            if (lmn_hyperlink_opt(atomi) && //seiji
                !lmn_sameproccxt_all_pc_check_original((SameProcCxt *)hashtbl_get(sameproccxt, (HashKeyType)atomi),
                                                       LMN_SATOM(wt[atomi]),
                                                       LMN_FUNCTOR_ARITY(f)))
              return FALSE;
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
        if (LMN_ATTR_IS_DATA(at[atomi])) {
          if (at[atomi] == attr) {
            BOOL eq;
            READ_CMP_DATA_ATOM(attr, wt[atomi], eq, tt[atomi]);
            if (eq) return FALSE;
          } else {
            goto label_skip_data_atom;
          }
        }
        else { /* symbol atom */
          READ_VAL(LmnFunctor, instr, f);
          if (LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt[atomi])) == f) return FALSE;
        }
      } else if(LMN_ATTR_IS_DATA(attr)) {
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
      tt[funci] = TT_OTHER;
      break;
    }
    case INSTR_UNIQ:
    {
      /*
       * uniq 命令は、
       * "全ての失敗しうるガード命令列の最後尾" かつ
       * "シンボルアトムを生成するガード命令列より前" に
       * 挿入されるように、コンパイラで配置変更を行なっている
       */

      LmnInstrVar llist, n;
      Vector *srcvec;
      LmnPort port;
      lmn_interned_str id;
      unsigned int i;
      BOOL sh;

      port = (LmnPort)lmn_make_output_string_port();
      READ_VAL(LmnInstrVar, instr, llist);

      if (lmn_env.show_hyperlink) {
        sh = TRUE;
        lmn_env.show_hyperlink = FALSE;
      }
			else sh = FALSE;

      for (i = 0; i < (int)llist; i++) {
        READ_VAL(LmnInstrVar, instr, n);
        srcvec = (Vector*) wt[n];

        /* 識別子の生成 */
        lmn_dump_atom(port, (LmnWord)wt[vec_get(srcvec, 0)], (LmnLinkAttr)at[vec_get(srcvec, 0)]);
        port_put_raw_s(port, ":");
      }
      
      id = lmn_intern((char *)lmn_string_c_str(port->data));
      lmn_port_free(port);

      if (sh) lmn_env.show_hyperlink = TRUE;

      /* 履歴表と照合 */
      if (st_is_member(lmn_rule_get_history_tbl(rule), (st_data_t)id)) return FALSE;

      /* 履歴に挿入 */
      st_insert(lmn_rule_get_history_tbl(rule), (st_data_t)id, 0);
      lmn_rule_set_pre_id(rule, id);

      break;
    }
    case INSTR_NEWHLINK:
    {
      /* 全ての失敗しうるガード制約よりも後で実行されるように、
       * コンパイラで配置変更を行なっている
       */

      if (!lmn_env.hyperlink) {
        fprintf(stdout, "Can't use hyperlink without option --hl.\n");
        exit(1);
      }

      LmnInstrVar atomi;
      READ_VAL(LmnInstrVar, instr, atomi);

      wt[atomi] = (LmnWord)lmn_hyperlink_new();
      at[atomi] = LMN_HL_ATTR;
      tt[atomi] = TT_ATOM;

      break;
    }
    case INSTR_MAKEHLINK:
    {
      if (!lmn_env.hyperlink) {
        fprintf(stdout, "Can't use hyperlink without option --hl.\n");
        exit(1);
      }
      // 未実装
      break;
    }
    case INSTR_ISHLINK:
    {
      if (!lmn_env.hyperlink) {
        fprintf(stdout, "Can't use hyperlink without option --hl.\n");
        exit(1);
      }

      LmnInstrVar atomi;
      READ_VAL(LmnInstrVar, instr, atomi);

      if (!LMN_ATTR_IS_HL(at[atomi])) return FALSE;

      break;
    }
    case INSTR_GETNUM:
    {
      if (!lmn_env.hyperlink) {
        fprintf(stdout, "Can't use hyperlink without option --hl.\n");
        exit(1);
      }

      /* ISHLINKチェック済み */
      LmnInstrVar dstatomi, atomi;
      READ_VAL(LmnInstrVar, instr, dstatomi);
      READ_VAL(LmnInstrVar, instr, atomi);

      wt[dstatomi] = (LmnWord)lmn_hyperlink_element_num(lmn_hyperlink_at_to_hl(LMN_SATOM(wt[atomi])));
      at[dstatomi] = LMN_INT_ATTR;
      tt[dstatomi] = TT_ATOM;

      break;
    }
    case INSTR_UNIFYHLINKS:
    {
      if (!lmn_env.hyperlink) {
        fprintf(stdout, "Can't use hyperlink without option --hl.\n");
        exit(1);
      }

      LmnInstrVar memi, atomi;
      LmnSAtom atom, atom1, atom2;
      LmnLinkAttr attr1, attr2;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);

      atom = LMN_SATOM(wt[atomi]);

      attr1 = LMN_SATOM_GET_ATTR(atom, 0);
      attr2 = LMN_SATOM_GET_ATTR(atom, 1);

      /* >< の両辺のアトムがハイパーリンクであれば併合 */
      if (LMN_ATTR_IS_HL(attr1) && LMN_ATTR_IS_HL(attr2)) {
        atom1 = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
        atom2 = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 1));

        lmn_hyperlink_unify(lmn_hyperlink_at_to_hl(atom1), lmn_hyperlink_at_to_hl(atom2));

        lmn_mem_delete_atom((LmnMembrane *)wt[memi], wt[atomi], at[atomi]);
        lmn_mem_delete_atom((LmnMembrane *)wt[memi], (LmnWord)atom1, (LmnWord)attr1);
        lmn_mem_delete_atom((LmnMembrane *)wt[memi], (LmnWord)atom2, (LmnWord)attr2);

      }
      break;
    }
    case INSTR_FINDPROCCXT:
    {
      /** 同名の型付きプロセス文脈名を持つルールを最適化モードで実行するための命令
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

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, length1);
      READ_VAL(LmnInstrVar, instr, arg1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, length2);
      READ_VAL(LmnInstrVar, instr, arg2);

      if (lmn_env.hyperlink) {
        if (!sameproccxt) lmn_sameproccxt_init();

        if (!hashtbl_contains(sameproccxt, (HashKeyType)atom1)) {
          spc1 = lmn_sameproccxt_spc_make(atom1, length1);
          hashtbl_put(sameproccxt, (HashKeyType)atom1, (HashValueType)spc1);
        } else {
          spc1 = (SameProcCxt *)hashtbl_get(sameproccxt, (HashKeyType)atom1);
        }
        if (!LMN_SPC_PC(spc1, arg1))
          LMN_SPC_PC(spc1, arg1) = lmn_sameproccxt_pc_make(atom1, arg1, NULL);

        if (!hashtbl_contains(sameproccxt, (HashKeyType)atom2)) {
          spc2 = lmn_sameproccxt_spc_make(atom2, length2);
          hashtbl_put(sameproccxt, (HashKeyType)atom2, (HashValueType)spc2);
        } else {
          spc2 = (SameProcCxt *)hashtbl_get(sameproccxt, (HashKeyType)atom2);
        }
        if (!LMN_SPC_PC(spc2, arg2))
          LMN_SPC_PC(spc2, arg2) = lmn_sameproccxt_pc_make(atom2, arg2, LMN_SPC_PC(spc1, arg1));

      }

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

      /* MEMO: time-optで消えていたので, なんか理由があるのかも. ないのかも.
       * ないとメモリリークするから, ありにしたい. */
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
      tt[dstlist] = TT_OTHER;

      /* 解放のための再帰。ベクタを解放するための中間語命令がない */
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
      } else {
        memi = 0;
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
      tt[atom1] = TT_ATOM;
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
      tt[listi] = TT_OTHER;
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
          tt[dsti] = TT_OTHER;
          if (posi == 0)
            at[dsti] = LINK_LIST;
          else if (posi == 1)
            at[dsti] = MAP;
          else
            lmn_fatal("unexpected attribute @instr_getfromlist");
          break;
        case LINK_LIST: /* LinkObjをfreeするのはここ？ */
        {
          LinkObj lo = (LinkObj)vec_get((Vector *)wt[listi], (unsigned int)posi);
          wt[dsti] = (LmnWord)lo->ap;
          at[dsti] = lo->pos;
          tt[dsti] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
      break;
    }
    case INSTR_INEG:
    {
      LmnInstrVar dstatom, atomi;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atomi);
      wt[dstatom] = (LmnWord)(-(long)wt[atomi]);
      at[dstatom] = LMN_INT_ATTR;
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
      break;
    }
    case INSTR_INOT:
    {
      LmnInstrVar dstatom, atomi;
      READ_VAL(LmnInstrVar, instr, dstatom);
      READ_VAL(LmnInstrVar, instr, atomi);
      wt[dstatom] = (LmnWord)~(int)atomi;
      at[dstatom] = LMN_INT_ATTR;
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[atomi] = TT_ATOM;
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_CONST_DATA_ATOM(wt[atomi], at[atomi], tt[atomi]);
      } else { /* symbol atom */
        LmnFunctor f;
/*         fprintf(stderr, "symbol atom can't be created in GUARD\n"); */
/*         exit(EXIT_FAILURE); */
        READ_VAL(LmnFunctor, instr, f);

        /* 本来のallocatomは格納するのは定数アトムだが、簡単のためにファンクタを格納する */
        wt[atomi] = f;
        tt[atomi] = TT_OTHER;
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
        if (LMN_ATTR_IS_EX(at[funci])) wt[atomi] = wt[funci];//seiji
        else wt[atomi] = lmn_copy_data_atom(wt[funci], at[funci]);
        at[atomi] = at[funci];
        tt[atomi] = TT_ATOM;
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
        wt[funci] = wt[atomi];
      }
      else {
        wt[funci] = (LmnWord)LMN_SATOM_GET_FUNCTOR(wt[atomi]);
      }
      at[funci] = at[atomi];
      tt[funci] = TT_OTHER;
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
      lmn_mem_set_name((LmnMembrane *)wt[memi], name);
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
        LmnRuleSet cp = lmn_ruleset_copy((LmnRuleSet)vec_get(v, i));
        lmn_mem_add_ruleset((LmnMembrane *)wt[destmemi], cp);
        if (RC_GET_MODE(rc, REACT_ATOMIC)) {
          /* atomic step中にatomic setをコピーした場合のため */
          lmn_ruleset_invalidate_atomic(cp);
        }
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
        LmnWord t = 0;

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
      tt[funci] = TT_OTHER;
      break;
    }
    case INSTR_LOADFUNC:
    {
      LmnInstrVar funci;
      LmnLinkAttr attr;

      READ_VAL(LmnFunctor, instr, funci);
      READ_VAL(LmnLinkAttr, instr, attr);
      at[funci] = attr;
      tt[funci] = TT_OTHER;
      if(LMN_ATTR_IS_DATA(attr)) {
        READ_CONST_DATA_ATOM(wt[funci], at[funci], tt[funci]);
      }
      else {
        LmnFunctor f;

        READ_VAL(LmnFunctor, instr, f);
        wt[funci] = (LmnWord)f;
        tt[funci] = TT_OTHER;
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
      case LMN_HL_ATTR://seiji
        if (!lmn_hyperlink_eq_hl(lmn_hyperlink_at_to_hl(LMN_SATOM(wt[func0])),
                                 lmn_hyperlink_at_to_hl(LMN_SATOM(wt[func1]))))
          return FALSE;
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
        case LMN_HL_ATTR://seiji
          if (lmn_hyperlink_eq_hl(lmn_hyperlink_at_to_hl(LMN_SATOM(wt[func0])),
                                  lmn_hyperlink_at_to_hl(LMN_SATOM(wt[func1]))))
            return FALSE;
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
      tt[mapi] = TT_OTHER;
      break;
    }
    case INSTR_LOOKUPLINK:
    {
      LmnInstrVar destlinki, tbli, srclinki;

      READ_VAL(LmnInstrVar, instr, destlinki);
      READ_VAL(LmnInstrVar, instr, tbli);
      READ_VAL(LmnInstrVar, instr, srclinki);

      at[destlinki] = LINKED_ATTR(srclinki);
      tt[destlinki] = TT_ATOM;
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
//      vec_clear(&((LmnMembrane *)wt[memi])->rulesets);
      lmn_mem_clearrules((LmnMembrane *)wt[memi]);
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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

      if (lmn_env.hyperlink && sameproccxt) lmn_sameproccxt_clear();//seiji /*branchとhyperlinkを同時起動するための急場しのぎ*/
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

      tt[reti] = TT_OTHER;
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

static BOOL dmem_interpret(struct ReactCxt *rc, LmnRule rule, LmnRuleInstr instr)
{
/*   LmnRuleInstr start = instr; */
  LmnInstrOp op;

  while (TRUE) {
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
        wt = LMN_REALLOC(LmnWord,     wt, wt_size);
        at = LMN_REALLOC(LmnLinkAttr, at, wt_size);
        tt = LMN_REALLOC(LmnLinkAttr, tt, wt_size);
      }
      if (s1 > RC_WORK_VEC_SIZE(rc)) {
        memset(tt + RC_WORK_VEC_SIZE(rc), 0, s1 - RC_WORK_VEC_SIZE(rc));
      }
      RC_SET_WORK_VEC_SIZE(rc, s1);
      break;
    }
    case INSTR_INSERTCONNECTORSINNULL:
    {
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

      wt[seti] = (LmnWord)insertconnectors(NULL, &links);
      tt[seti] = TT_OTHER;
      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if(dmem_interpret(rc, rule, instr)) {
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      }
      else LMN_ASSERT(0);
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
      tt[seti] = TT_OTHER;
      if (RC_GET_MODE(rc, REACT_ND)) { at[seti] = 0; /* MC */ }
      vec_destroy(&links);

      /* EFFICIENCY: 解放のための再帰 */
      if(dmem_interpret(rc, rule, instr)){
        hashset_free((HashSet *)wt[seti]);
        return TRUE;
      }else LMN_ASSERT(0);
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
        ap = LMN_ATOM(dmem_root_new_atom(RC_ND_MEM_DELTA_ROOT(rc), f));
      }
      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembrane *)wt[memi], ap, attr);
      wt[atomi] = (LmnWord)ap;
      at[atomi] = attr;
      tt[atomi] = TT_ATOM;
      break;
    }
    case INSTR_COPYATOM:
    {
      LmnInstrVar atom1, memi, atom2;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atom2);

      at[atom1] = at[atom2];
      wt[atom1] = dmem_root_copy_atom(RC_ND_MEM_DELTA_ROOT(rc), wt[atom2], at[atom2]);
      tt[atom1] = TT_ATOM;
      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembrane *)wt[memi], wt[atom1], at[atom1]);
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
      tt[link] = TT_ATOM;
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

      dmem_root_newlink(RC_ND_MEM_DELTA_ROOT(rc),
                        (LmnMembrane *)wt[memi],
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

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_relink(RC_ND_MEM_DELTA_ROOT(rc),
                       (LmnMembrane *)wt[memi],
                       wt[atom1],
                       at[atom1],
                       pos1,
                       wt[atom2],
                       at[atom2],
                       pos2);
      break;
    }
    case INSTR_GETLINK:
    {
      LmnInstrVar linki, atomi, posi;
      READ_VAL(LmnInstrVar, instr, linki);
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, posi);

      wt[linki] = dmem_root_get_link(RC_ND_MEM_DELTA_ROOT(rc), LMN_SATOM(wt[atomi]), posi);
      at[linki] = LMN_SATOM_GET_ATTR(wt[atomi], posi);
      tt[linki] = TT_ATOM;
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

      dmem_root_unify_atom_args(RC_ND_MEM_DELTA_ROOT(rc),
                                (LmnMembrane *)wt[memi],
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

      mp = dmem_root_new_mem(RC_ND_MEM_DELTA_ROOT(rc)); /*lmn_new_mem(memf);*/
      dmem_root_add_child_mem(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembrane*)wt[parentmemi], mp);
      wt[newmemi] = (LmnWord)mp;
      tt[newmemi] = TT_MEM;
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

      wt[dstmemi] = (LmnWord)dmem_root_new_mem(RC_ND_MEM_DELTA_ROOT(rc));
//      tt[dstmemi] = TT_MEM;
      break;
    }
    case INSTR_REMOVEATOM:
    {
      LmnInstrVar atomi, memi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);

      dmem_root_remove_atom(RC_ND_MEM_DELTA_ROOT(rc),
                            (LmnMembrane*)wt[memi], wt[atomi], at[atomi]);
      break;
    }
    case INSTR_FREEATOM:
    {
      LmnInstrVar atomi;

      READ_VAL(LmnInstrVar, instr, atomi);

      dmem_root_free_atom(RC_ND_MEM_DELTA_ROOT(rc), wt[atomi], at[atomi]);
      break;
    }
    case INSTR_REMOVEMEM:
    {
      LmnInstrVar memi, parenti;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, parenti);

      dmem_root_remove_mem(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembrane *)wt[parenti], (LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_FREEMEM:
    {
      LmnInstrVar memi;
      LmnMembrane *mp;

      READ_VAL(LmnInstrVar, instr, memi);

      mp = (LmnMembrane*)wt[memi];
/*       lmn_mem_free(mp); */
      break;
    }
    case INSTR_ADDMEM:
    {
      LmnInstrVar dstmem, srcmem;

      READ_VAL(LmnInstrVar, instr, dstmem);
      READ_VAL(LmnInstrVar, instr, srcmem);

//      LMN_ASSERT(!((LmnMembrane *)wt[srcmem])->parent);

//      lmn_mem_add_child_mem((LmnMembrane *)wt[dstmem], (LmnMembrane *)wt[srcmem]);
      dmem_root_add_child_mem(RC_ND_MEM_DELTA_ROOT(rc),
                              (LmnMembrane*)wt[dstmem], (LmnMembrane *)wt[srcmem]);
      //lmn_fatal("INSTR_ADDMEM, not implemented");
      break;

      break;
    }
    case INSTR_ENQUEUEMEM:
    {
      LmnInstrVar memi;
      READ_VAL(LmnInstrVar, instr, memi);
      if (RC_GET_MODE(rc, REACT_ND)) {
        lmn_mem_activate_ancestors((LmnMembrane *)wt[memi]); /* MC */
      } else if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
        lmn_memstack_push(RC_MEMSTACK(rc), (LmnMembrane *)wt[memi]); /* 通常実行時 */
      }
      break;
    }
    case INSTR_UNLOCKMEM:
    {
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

      dmem_root_copy_ground(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembrane *)wt[memi],
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
      tt[dstlist] = TT_OTHER;

      /* 解放のための再帰。ベクタを解放するための中間ご命令がない */
      dmem_interpret(rc, rule, instr);

      free_links(dstlovec);
      vec_free(retvec);

      return TRUE; /* COPYGROUNDはボディに出現する */
    }
    case INSTR_REMOVEGROUND:
    case INSTR_FREEGROUND:
    {
      LmnInstrVar listi, memi;
      Vector *srcvec; /* 変数番号のリスト */

      memi = 0; /* warningを黙らす */
      READ_VAL(LmnInstrVar, instr, listi);
      if (INSTR_REMOVEGROUND == op) {
         READ_VAL(LmnInstrVar, instr, memi);
      }

      srcvec = links_from_idxs((Vector *)wt[listi], wt, at);
      
      switch (op) {
       case INSTR_REMOVEGROUND:
         dmem_root_remove_ground(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi], srcvec);
         break;
       case INSTR_FREEGROUND:
         dmem_root_free_ground(RC_ND_MEM_DELTA_ROOT(rc), srcvec);
         break;
      }

      free_links(srcvec);

      break;
    }
    case INSTR_NEWLIST:
    {
      LmnInstrVar listi;
      Vector *listvec = vec_make(16);
      READ_VAL(LmnInstrVar, instr, listi);
      wt[listi] = (LmnWord)listvec;
      tt[listi] = TT_OTHER;
      if (RC_GET_MODE(rc, REACT_ND)) { at[listi] = 0; /* MC */ }
      /* 解放のための再帰 */
      if (dmem_interpret(rc, rule, instr)) {
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
          tt[dsti] = TT_OTHER;
          if (posi == 0)
            at[dsti] = LINK_LIST;
          else if (posi == 1)
            at[dsti] = MAP;
          else
            LMN_ASSERT(0);
          break;
        case LINK_LIST: /* LinkObjをfreeするのはここ？ */
        {
          LinkObj lo = (LinkObj)vec_get((Vector *)wt[listi], (unsigned int)posi);
          wt[dsti] = (LmnWord)lo->ap;
          at[dsti] = lo->pos;
          tt[dsti] = TT_ATOM;
          break;
        }
      }
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
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
      tt[dstatom] = TT_ATOM;
      break;
    }
    case INSTR_ALLOCATOM:
    {
      LmnInstrVar atomi;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);
      at[atomi] = attr;
      tt[atomi] = TT_ATOM;
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_CONST_DATA_ATOM(wt[atomi], at[atomi], tt[atomi]);
      } else { /* symbol atom */
        LmnFunctor f;
/*         fprintf(stderr, "symbol atom can't be created in GUARD\n"); */
/*         exit(EXIT_FAILURE); */
        READ_VAL(LmnFunctor, instr, f);

        /* 本来のallocatomは格納するのは定数アトムだが、簡単のためにファンクタを格納する */
        wt[atomi] = f;
        tt[atomi] = TT_OTHER;
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
        tt[atomi] = TT_ATOM;
      } else { /* symbol atom */
        fprintf(stderr, "symbol atom can't be created in GUARD\n");
        exit(EXIT_FAILURE);
      }
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
      tt[funci] = TT_OTHER;
      break;
    }
    case INSTR_SETMEMNAME:
    {
      LmnInstrVar memi;
      lmn_interned_str name;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(lmn_interned_str, instr, name);
      dmem_root_set_mem_name(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi], name);
      break;
    }
    case INSTR_COPYRULES:
    {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);

      dmem_root_copy_rules(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[destmemi], (LmnMembrane *)wt[srcmemi]);
      break;
    }
    case INSTR_REMOVEPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_proxies(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_INSERTPROXIES:
    {
      LmnInstrVar parentmemi, childmemi;

      READ_VAL(LmnInstrVar, instr, parentmemi);
      READ_VAL(LmnInstrVar, instr, childmemi);
      dmem_root_insert_proxies(RC_ND_MEM_DELTA_ROOT(rc),(LmnMembrane *)wt[parentmemi], (LmnMembrane *)wt[childmemi]);
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

        t = 0; /* warningを黙らす */
        proc_tbl_get_by_atom(delmap, orig, &t);
        copy = LMN_SATOM(t);
        lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);
        /* mem がないので仕方なく直接アトムリストをつなぎ変える
           UNIFYアトムはnatomに含まれないので大丈夫 */
        LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(copy), LMN_SATOM_GET_PREV(copy));
        LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(copy), LMN_SATOM_GET_NEXT_RAW(copy));

        lmn_delete_atom(orig);
      }

      if (delmap) proc_tbl_free(delmap);
      break;
    }
    case INSTR_REMOVETOPLEVELPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_toplevel_proxies(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_ADDATOM:
    {
      LmnInstrVar memi, atomi;

      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnInstrVar, instr, atomi);
      dmem_root_push_atom(RC_ND_MEM_DELTA_ROOT(rc),
                          (LmnMembrane *)wt[memi],
                          wt[atomi],
                          at[atomi]);
      break;
    }
    case INSTR_MOVECELLS:
    {
      LmnInstrVar destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      LMN_ASSERT(wt[destmemi] != wt[srcmemi]);
      dmem_root_move_cells(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembrane *)wt[destmemi], (LmnMembrane *)wt[srcmemi]);
      break;
    }
    case INSTR_REMOVETEMPORARYPROXIES:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_remove_temporary_proxies(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi]);
      break;
    }
    case INSTR_COPYCELLS:
    {
      LmnInstrVar mapi, destmemi, srcmemi;

      READ_VAL(LmnInstrVar, instr, mapi);
      READ_VAL(LmnInstrVar, instr, destmemi);
      READ_VAL(LmnInstrVar, instr, srcmemi);
      dmem_root_copy_cells(RC_ND_MEM_DELTA_ROOT(rc),
                           (LmnMembrane *)wt[destmemi],
                           (LmnMembrane *)wt[srcmemi]);
      tt[mapi] = TT_OTHER;
      break;
    }
    case INSTR_LOOKUPLINK:
    {
      LmnInstrVar destlinki, tbli, srclinki;

      READ_VAL(LmnInstrVar, instr, destlinki);
      READ_VAL(LmnInstrVar, instr, tbli);
      READ_VAL(LmnInstrVar, instr, srclinki);

      at[destlinki] = LINKED_ATTR(srclinki);
      tt[destlinki] = TT_ATOM;
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
      dmem_root_clear_ruleset(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi]);
      vec_clear(&((LmnMembrane *)wt[memi])->rulesets);
      break;
    }
    case INSTR_DROPMEM:
    {
      LmnInstrVar memi;

      READ_VAL(LmnInstrVar, instr, memi);
      dmem_root_drop(RC_ND_MEM_DELTA_ROOT(rc), (LmnMembrane *)wt[memi]);
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
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
      tt[desti] = TT_ATOM;
      break;
    }
    case INSTR_LOOP:
    {
      LmnSubInstrSize subinstr_size;
      READ_VAL(LmnSubInstrSize, instr, subinstr_size);

      while (dmem_interpret(rc, rule, instr)) ;
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

