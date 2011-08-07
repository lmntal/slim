/*
 * load.c - Load Intermediate Language
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
 * $Id: load.c,v 1.13 2008/10/17 08:40:50 sasaki Exp $
 */

#include "load.h"
#include "lmntal.h"
#include "symbol.h"
#include "syntax.h"
#include "arch.h"
#include "rule.h"
#include "lmntal_system_adapter.h"
#include "il_parser.h"
#include "il_lexer.h"
#include "load.h"
#include "file_util.h"
#include "so.h"
#include <dirent.h>
#include <limits.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

/* prototypes */

void dump_module(Module m);
void dump_il(IL il);
void build_cmd(char *buf, char *file_name);
FILE *compile(char *filename);

/*----------------------------------------------------------------------
 * Dump(デバッグ用)。最初使用しただけなので dump_*は消してOK
 */

static void dump_functor(Functor f)
{
  switch (functor_get_type(f)) {
  case STX_SYMBOL:
    printf("%s.%s_%d[%d]",
           lmn_id_to_name(LMN_FUNCTOR_MODULE_ID(functor_get_id(f))),
           lmn_id_to_name(LMN_FUNCTOR_NAME_ID(functor_get_id(f))),
           LMN_FUNCTOR_ARITY(functor_get_id(f)),
           functor_get_id(f)
           );
    break;
  case INT_FUNC:
    printf("%ld", functor_get_int_value(f));
    break;
  case FLOAT_FUNC:
    printf("%f", functor_get_float_value(f));
    break;
  case STX_IN_PROXY:
    printf("$in");
    break;
  case STX_OUT_PROXY:
    printf("$out");
    break;
  case STX_UNIFY:
    printf("=");
    break;
  default:
    fprintf(stderr, "unexpected functor type %d", functor_get_type(f));
    exit(EXIT_FAILURE);
  }
}

static void dump_instr(Instruction inst);

static void dump_arg(InstrArg arg)
{
  switch (inst_arg_get_type(arg)) {
  case InstrVar:
    printf("%d", inst_arg_get_var(arg));
    break;
  case Label:
    printf("L%d", inst_arg_get_label(arg));
    break;
  case String:
    printf("\"%s\"", lmn_id_to_name(inst_arg_get_str_id(arg)));
    break;
  case LineNum:
    printf("Line:%d", inst_arg_get_linenum(arg));
    break;
  case ArgRuleset:
    printf("@%d", inst_arg_get_ruleset_id(arg));
    break;
  case ArgFunctor:
    dump_functor(inst_arg_get_functor(arg));
    break;
  case InstrVarList:
  {
    unsigned int i;
    ArgList l;

    l = inst_arg_get_var_list(arg);
    printf("[");
    for (i = 0; i < var_list_num(l); i++) {
      if (i > 0) printf(", ");
      printf("%d", var_list_get(l, i));
    }
    printf("]");
    break;
  }
  case InstrList:
  {
    unsigned int i;
    InstList l;

    l = inst_arg_get_inst_list(arg);
    printf("[\n");
    for (i = 0; i < inst_list_num(l); i++) {
      printf("   ");
      dump_instr(inst_list_get(l, i));
    }
    printf("     ]");
    break;
  }
  default:
    LMN_ASSERT(FALSE);
    break;
  }
}

static void dump_instr(Instruction inst)
{
  unsigned int i;
  ArgList l;

  printf("     %d arg= [", inst_get_id(inst));
  l = inst_get_args(inst);
 for (i = 0; i < arg_list_num(l); i++) {
    if (i > 0) printf(", ");
    dump_arg(arg_list_get(l, i));
  }
  printf("]\n");
}

static void dump_instblock(InstBlock ib)
{
  unsigned int i;
  InstList l;

  printf("    Label=%d \n", inst_block_get_label(ib));
  l = inst_block_get_instructions(ib);
  for (i = 0; i < inst_list_num(l); i++) {
    dump_instr(inst_list_get(l, i));
  }
}

static void dump_rule(Rule rule)
{
  printf("Rule NAME=%s \n", lmn_id_to_name(rule_get_name(rule)));
  printf("  amatch \n");
  dump_instblock(rule_get_amatch(rule));
  printf("  mmatch \n");
  dump_instblock(rule_get_mmatch(rule));
  printf("  guard \n");
  dump_instblock(rule_get_guard(rule));
  printf("  body \n");
  dump_instblock(rule_get_body(rule));
}

static void dump_ruleset(RuleSet rs)
{
  unsigned int i;
  RuleList rl;

  if (ruleset_is_system_ruleset(rs)) printf("SystemRuleset:\n");
  printf("Ruleset ID=%d \n", ruleset_get_id(rs));

  rl = ruleset_get_rulelist(rs);
  for (i = 0; i < rulelist_num(rl); i++) {
    dump_rule(rulelist_get(rl, i));
  }
}

void dump_module(Module m)
{
  printf("module %s, %d\n",
         lmn_id_to_name(module_get_name(m)),
         module_get_ruleset(m));
}

void dump_il(IL il)
{
  int i;
  RuleSets rss;
  ModuleList ml;

  rss = il_get_rulesets(il);
  ml = il_get_module_list(il);

  for (i = 0; i < rulesets_num(rss); i++) {
    dump_ruleset(rulesets_get(rss, i));
  }
  for (i = 0; i < module_list_num(ml); i++) {
    dump_module(module_list_get(ml, i));
  }

}

/*
 *  Instruction Format
 *
 *  * instructions
 *     sequence of instruction
 *
 *  * instruction
 *     LmnInstrOp              : instruction ID
 *     sequence of argument    : arguments of the instruction
 *
 *  * argument
 *    * integer functor
 *        BYTE(LMN_INT_ATTR)
 *        long                  : integer value
 *    * float functor
 *        BYTE(LMN_FLOAT_ATTR)
 *        double                : double nvalue
 *    * string functor
 *        BYTE(LMN_STRING_ATTR)
 *        lmn_interned_str      : string id
 *    * symbol functor
 *        BYTE(0)
 *        LmnFunctor(functor id)
 *    * in/out proxy functor
 *        BYTE(0)
 *        LmnFunctor(LMN_IN(OUT)_PROXY_FUNCTOR)
 *    * unify functor
 *        BYTE(0)
 *        LmnFunctor(LMN_UNIFY_FUNCTOR)
 *    * ruleset
 *        LmnRulesetId           : RuleSet ID
 *    * instruction list
 *        LmnSubInstrSize         : size of instruction list
 *        sequence of instruction
 *    * InstrVar
 *        LmnInstrVar      : integer, variable number
 *    * InstrVarList
 *        int16_t          : # of elements (N)
 *        LmnInstrVar * N
 *    * Label
 *        LmnJumpOffset      : difference between the next location to destination
 *
 */

#include "syntax.h"
#include "atom.h"
#include "rule.h"

/* 構文木の読み込み時に使うデータ。各ルールの解析じに作成し，解析後に破
   棄する。ラベルは各ルールにローカルなものとして処理している */
typedef struct Context {
  /* ラベルのからラベルのある位置の対応*/
  st_table_t label_to_loc;
  /* ラベルを参照している位置と参照しているラベルの対応 */
  st_table_t loc_to_label_ref;

  /* 書き込み位置とbyte_seqのキャパシティ */
  unsigned int loc, cap;
  /* ルールの命令列を書き込む領域 */
  BYTE *byte_seq;
} *Context;


void expand_byte_sec(Context c);
static void load_instruction(Instruction inst, Context c);

/* Contextを作成する */
static Context context_make()
{
  Context c = LMN_MALLOC(struct Context);

  c->loc = 0;
  c->cap = 256;
  c->byte_seq = LMN_NALLOC(BYTE, c->cap);

  return c;
}

/* Contextの解放 */
static void context_free(Context c)
{
  LMN_FREE(c);
}

/* 命令列を書き込む領域を広げる */
void expand_byte_sec(Context c)
{
  c->cap *= 2;
  c->byte_seq = LMN_REALLOC(BYTE, c->byte_seq, c->cap);
}

/* 現在の一に書き込TYPE型のデータを書き込む */
#define WRITE(TYPE, VALUE, CONTEXT)                              \
  do {                                                           \
    while ((CONTEXT)->loc + sizeof(TYPE) >= (CONTEXT)->cap) {    \
      expand_byte_sec(CONTEXT);                                  \
    }                                                            \
    *(TYPE*)((CONTEXT)->byte_seq + (CONTEXT)->loc) = (VALUE);    \
 } while (0)



/* 現在の書き込み位置を移動する */
#define MOVE(TYPE, CONTEXT)  (CONTEXT)->loc += sizeof(TYPE)

/* WRITE & MOVE */
#define WRITE_MOVE(TYPE, VALUE, CONTEXT)                         \
  do {                                                           \
    do {                                                         \
      while ((CONTEXT)->loc + sizeof(TYPE) >= (CONTEXT)->cap) {  \
        expand_byte_sec(CONTEXT);                                \
      }                                                          \
      *(TYPE*)((CONTEXT)->byte_seq + (CONTEXT)->loc) = (VALUE);  \
    } while (0);                                                 \
    (CONTEXT)->loc += sizeof(TYPE);                              \
  } while (0)

/* LCOの位置に書き込む */
#define WRITE_HERE(TYPE, VALUE, CONTEXT, LOC)                    \
  do {                                                           \
    do {                                                         \
      while ((LOC) + sizeof(TYPE) >= (CONTEXT)->cap) {  \
        expand_byte_sec(CONTEXT);                                \
      }                                                          \
      *(TYPE*)((CONTEXT)->byte_seq + (LOC)) = (VALUE);           \
    } while (0);                                                 \
  } while (0)


static void load_arg(InstrArg arg, Context c)
{
  unsigned int i;

  switch (inst_arg_get_type(arg)) {
  case InstrVar:
    WRITE_MOVE(LmnInstrVar, inst_arg_get_var(arg), c);
    break;
  case Label:
    st_insert(c->loc_to_label_ref, (st_data_t)c->loc, (st_data_t)inst_arg_get_label(arg));
    MOVE(LmnJumpOffset, c);
    break;
  case InstrVarList:
    {
      VarList var_list = inst_arg_get_var_list(arg);

//      WRITE_MOVE(int16_t, var_list_num(var_list), c);
      WRITE_MOVE(LmnInstrVar, var_list_num(var_list), c);
      for (i = 0; i < var_list_num(var_list); i++) {
        WRITE_MOVE(LmnInstrVar, var_list_get(var_list, i), c);
      }
    }
    break;
  case String:
    WRITE_MOVE(lmn_interned_str, inst_arg_get_str_id(arg), c);
    break;
  case LineNum:
    WRITE_MOVE(LmnLineNum, inst_arg_get_linenum(arg), c);
    break;
  case ArgFunctor:
    {
      Functor functor = inst_arg_get_functor(arg);

      switch (functor_get_type(functor)) {
      case STX_SYMBOL:
        WRITE_MOVE(LmnLinkAttr, LMN_ATTR_MAKE_LINK(0), c);
        WRITE_MOVE(LmnFunctor, functor_get_id(functor), c);
        break;
      case INT_FUNC:
        WRITE_MOVE(LmnLinkAttr, LMN_INT_ATTR, c);
        WRITE_MOVE(long, functor_get_int_value(functor), c);
        break;
      case FLOAT_FUNC:
        WRITE_MOVE(LmnLinkAttr, LMN_DBL_ATTR, c);
        WRITE_MOVE(double, functor_get_float_value(functor), c);
        break;
      case STRING_FUNC:
        {
          WRITE_MOVE(LmnLinkAttr, LMN_STRING_ATTR, c);
          WRITE_MOVE(lmn_interned_str, functor_get_string_value(functor), c);
        }
        break;
      case STX_IN_PROXY:
        WRITE_MOVE(LmnLinkAttr, LMN_ATTR_MAKE_LINK(0), c);
        WRITE_MOVE(LmnFunctor, LMN_IN_PROXY_FUNCTOR, c);
        break;
      case STX_OUT_PROXY:
        WRITE_MOVE(LmnLinkAttr, LMN_ATTR_MAKE_LINK(0), c);
        WRITE_MOVE(LmnFunctor, LMN_OUT_PROXY_FUNCTOR, c);
        break;
      case STX_UNIFY:
        WRITE_MOVE(LmnLinkAttr, LMN_ATTR_MAKE_LINK(0), c);
        WRITE_MOVE(LmnFunctor, LMN_UNIFY_FUNCTOR, c);
        break;
      default:
        LMN_ASSERT(FALSE);
        break;
      }
      break;
    }
  case ArgRuleset:
    WRITE_MOVE(LmnRulesetId, inst_arg_get_ruleset_id(arg), c);
    break;
  case InstrList:
    {
      unsigned int start, t;
      InstList inst_list;

      /* 命令列の長さを求めるため、開始位置を記録する */
      /* INSTR_NOTでサブ命令列の長さを知る必要がある */
      start = c->loc;
      MOVE(LmnSubInstrSize, c);

      inst_list = inst_arg_get_inst_list(arg);
      for (i = 0; i < inst_list_num(inst_list); i++) {
        load_instruction(inst_list_get(inst_list, i), c);
      }

      /* startの位置に現在の位置との差を書き込む */
      t = c->loc;
      c->loc = start;
      WRITE(LmnSubInstrSize, t - (start + sizeof(LmnSubInstrSize)), c);
      c->loc = t;
      break;
    }
  default:
    LMN_ASSERT(FALSE);
    break;
  }
}

static void load_instruction(Instruction inst, Context c)
{
  ArgList args;
  int i;
  int arg_num;

  args = inst_get_args(inst);

  WRITE_MOVE(LmnInstrOp, inst_get_id(inst), c);
  arg_num = arg_list_num(args);

  /* REMOVEATOMは引数の数が2と3の場合がある。第三引数の
     ファンクタは無視する */
  if (inst_get_id(inst) == INSTR_REMOVEATOM &&
      arg_num == 3) {
    arg_num = 2;
  }
  for (i = 0; i < arg_num; i++) {
    load_arg(arg_list_get(args, i), c);
  }
}

static void load_inst_block(InstBlock ib, Context c)
{
  InstList inst_list;
  unsigned int i;

  if (inst_block_has_label(ib)) {
    st_insert(c->label_to_loc, (st_data_t)inst_block_get_label(ib), (st_data_t)c->loc);
  }

  inst_list = inst_block_get_instructions(ib);

  for (i = 0; i < inst_list_num(inst_list); i++) {
    load_instruction(inst_list_get(inst_list, i), c);
  }
}

static int fill_label_ref(st_data_t loc, st_data_t label, void *c_)
{
  Context  c = (Context)c_;
  st_data_t target_loc;

  if (st_lookup(c->label_to_loc, label, &target_loc)) {
    WRITE_HERE(LmnJumpOffset, (int)target_loc - (int)loc - sizeof(LmnJumpOffset), c, (int)loc);
  } else {
    fprintf(stderr, "implementation error: label not found L%d\n", (int)label);
    exit(EXIT_FAILURE);
  }

  return ST_CONTINUE;
}

LmnRule load_rule(Rule rule)
{
  LmnRule runtime_rule;
  Context c;

  c = context_make();
  c->label_to_loc = st_init_numtable();
  c->loc_to_label_ref = st_init_numtable();

/*   load_inst_block(rule_get_amatch(rule), c); */
  load_inst_block(rule_get_mmatch(rule), c);
  load_inst_block(rule_get_guard(rule), c);
  load_inst_block(rule_get_body(rule), c);

  /* ラベルを参照している位置に、実際のラベルの位置を書き込む */
  st_foreach(c->loc_to_label_ref, fill_label_ref, (st_data_t)c);

  st_free_table(c->label_to_loc);
  st_free_table(c->loc_to_label_ref);

  runtime_rule = lmn_rule_make(c->byte_seq, c->cap, ANONYMOUS);
  if (rule_get_hasuniq(rule)) lmn_rule_init_uniq_rule(runtime_rule);
  context_free(c);
  return runtime_rule;
}

static LmnRuleSet load_ruleset(RuleSet rs)
{
  RuleList rl;
  LmnRuleSet runtime_ruleset;
  unsigned int i;

  runtime_ruleset = lmn_ruleset_make(ruleset_get_id(rs), 10);
  rl = ruleset_get_rulelist(rs);

  for (i = 0; i < rulelist_num(rl); i++) {
    lmn_ruleset_put(runtime_ruleset, load_rule(rulelist_get(rl, i)));
  }

  lmn_set_ruleset(runtime_ruleset, ruleset_get_id(rs));

  if (ruleset_is_system_ruleset(rs)) {
    /* 各ルールをシステムルールセットに追加する */
    for (i = 0; i < lmn_ruleset_rule_num(runtime_ruleset); i++) {
      LmnRule rule2 = lmn_rule_copy(lmn_ruleset_get_rule(runtime_ruleset, i));
      lmn_add_system_rule(rule2);
    }
  }

  return runtime_ruleset;
}

/* 最初のルールセットを返す */
static LmnRuleSet load_il(IL il)
{
  LmnRuleSet t, first_ruleset = NULL;
  RuleSets rulesets;
  ModuleList module_list;
  int i;
  rulesets = il_get_rulesets(il);

  /* load rules */
  for (i = 0; i < rulesets_num(rulesets); i++) {
    t = load_ruleset(rulesets_get(rulesets, i));
    if (i == 0) first_ruleset = t;
  }

  /* load module list */
  module_list = il_get_module_list(il);
  for (i = 0; i < module_list_num(module_list); i++) {
    Module m = module_list_get(module_list, i);
    lmn_set_module(module_get_name(m), lmn_ruleset_from_id(module_get_ruleset(m)));
  }

  if (first_ruleset == NULL) {
    fprintf(stderr, "implementation error: no ruleset in il\n");
    exit(EXIT_FAILURE);
  }

  return first_ruleset;
}

/* soから試しに呼び出す関数 */
void helloworld(const char *s)
{
  fprintf(stdout, "hello %s world!\n", s);
}

LmnRuleSet load_and_setting_trans_maindata(struct trans_maindata *maindata)
{
  int i;
  struct trans_ruleset ruleset;
  LmnRuleSet ret = 0; /* ワーニング抑制 */

  /* シンボルを読み込み+変換テーブルを設定 */
  for(i=1; i<maindata->count_of_symbol; ++i){
    lmn_interned_str gid = lmn_intern(maindata->symbol_table[i]);
    maindata->symbol_exchange[i] = gid;
  }

  /* ファンクタを読み込み+変換テーブルを設定 */
  for(i=0; i<maindata->count_of_functor; ++i){
    LmnFunctorEntry ent = maindata->functor_table[i];
    /* スペシャルファンクタは登録できないが,functor.c内で登録される共通部分以外出現しようがないはずなので問題ない */
    if(ent.special){
      /* 登録しないで変換も必要無し */
      maindata->functor_exchange[i] = i;
    }else{
      /* シンボルは変換を忘れないように */
      LmnFunctor gid = lmn_functor_intern(maindata->symbol_exchange[ent.module], maindata->symbol_exchange[ent.name], ent.arity);
      maindata->functor_exchange[i] = gid;
    }
  }

  /* ルールセット0番は数合わせ */
  /* システムルールセット読み込み */
  ruleset = maindata->ruleset_table[1];
  for(i=0; i<ruleset.size; ++i){
    LmnRule r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_system_rule(r);
  }
  /* ルールセット2番はinitial ruleset */
  ruleset = maindata->ruleset_table[2];
  for(i=0; i<ruleset.size; ++i){
    LmnRule r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_initial_rule(r);
  }
  /* ルールセット3番はinitial system ruleset */
  ruleset = maindata->ruleset_table[3];
  for(i=0; i<ruleset.size; ++i){
    LmnRule r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_initial_system_rule(r);
  }
  /* ルールセットを読み込み+変換テーブルを設定 */
  for(i=FIRST_ID_OF_NORMAL_RULESET; i<maindata->count_of_ruleset; ++i){
    int j;
    struct trans_ruleset tr = maindata->ruleset_table[i];
    int gid = lmn_gen_ruleset_id();
    LmnRuleSet rs = lmn_ruleset_make(gid, tr.size);
    lmn_set_ruleset(rs, gid);

    for(j=0; j<tr.size; ++j){
      LmnRule r = lmn_rule_make_translated(tr.rules[j].function,
                                           maindata->symbol_exchange[tr.rules[j].name]);
      lmn_ruleset_put(rs, r);
    }

    /* とりあえず最初の通常ルールセットを初期データ生成ルールと決め打ちしておく */
    if(i==FIRST_ID_OF_NORMAL_RULESET) ret = rs;
    maindata->ruleset_exchange[i] = gid;
  }

  /* モジュール読込み */
  for(i=0; i<maindata->count_of_module; ++i){
    struct trans_module mo = maindata->module_table[i];
    lmn_set_module(maindata->symbol_exchange[mo.name], lmn_ruleset_from_id(maindata->ruleset_exchange[mo.ruleset]));
  }

  return ret;
}

/* soハンドルから中間命令を読み出す load_extは開始ルールを認識しない */
/* 複数ファイルを1つのsoにした場合、初期データ生成ルールが複数あるはずだがとりあえず無視 */
/* TODO: 初期データ生成ルールセットのルールを1つのルールセットにまとめて出力すれば問題無し 1回適用成功したところで止めなければok) */
LmnRuleSet load_compiled_il(char *filename, void *sohandle)
{
  char *basename = create_formatted_basename(filename);
  int buf_len = strlen(basename) + 50;  /* 適当に50文字余分にとったけどこれでいいのか */
  char *buf = lmn_malloc(buf_len + 1); /* 必要ないけど一応最後に1byte余分をとっておく */
  void (*init_f)();
  struct trans_maindata *maindata;
  LmnRuleSet ret = 0;

  /* 初期化関数を呼び出し */
  snprintf(buf, buf_len, "init_%s", basename);
  init_f = dlsym(sohandle, buf);
  if(! init_f){
    fprintf(stderr, "init function \"%s\" not found in %s.\n", buf, filename);
    ret = 0;
    goto returning;
  }
  (*init_f)();

  /* データオブジェクトを取得 */
  snprintf(buf, buf_len, "trans_%s_maindata", basename);
  maindata = dlsym(sohandle, buf);
  if(! maindata){
    fprintf(stderr, "maindata \"%s\" not found in %s.\n", buf, basename);
    ret = 0;
    goto returning;
  }

  /* 読み込みと変換テーブルの設定 */
  ret = load_and_setting_trans_maindata(maindata);

 returning:
  lmn_free(buf);
  lmn_free(basename);
  return ret;
}

/* ファイルから中間言語を読み込みランタイム中に配置する。
 * 最初のルールセットを返す */
LmnRuleSet load(FILE *in)
{
  IL il;
  LmnRuleSet first_ruleset;

  if (il_parse(in, &il)) {
    /* 構文解析に失敗 */
    exit(EXIT_FAILURE);
  }

  first_ruleset = load_il(il);
  il_free(il);
  return first_ruleset;
}

static Vector *opened_so_files;
void init_so_handles()
{
  opened_so_files = vec_make(2);
}

void finalize_so_handles()
{
  int i;
  for(i=0; i<vec_num(opened_so_files); ++i){
    dlclose((void*)vec_get(opened_so_files, i));
  }
  vec_free(opened_so_files);
}

/* ファイルから中間言語を読み込みランタイム中に配置し、最初のルールセットを返す。
 * ファイルの拡張子が lmn の場合、Javaによる処理系でファイルをコンパイルし、
 * 中間言語を生成する。soの場合、dlopenしておきハンドラはopened_so_filesで管理。dlcloseはfinalize()でされる */
LmnRuleSet load_file(char *file_name)
{
  FILE *fp;
  int len;
  LmnRuleSet rs;
  void *sohandle;

  len = strlen(file_name);

  /* 拡張子がsoならリンクする */
  /* dlopenは環境変数にLD_LIBRARY_PATH="."と設定しないとカレントディレクトリを検索してくれないので注意 */
  if (!strcmp(file_name + len -3, ".so")) {
    sohandle = dlopen(file_name, RTLD_LAZY);
    if(! sohandle){
      fprintf(stderr, "Failed to open %s\n", file_name);
      fprintf(stderr, "dlopen: %s\n", dlerror());
      rs = 0;
    }else{
      dlerror();
      vec_push(opened_so_files, (vec_data_t)sohandle);
      rs = load_compiled_il(file_name, sohandle);
    }
  }else if ((fp = fopen(file_name, "r"))) {
    /* 拡張子がlmnならばJavaによる処理系で中間言語にコンパイルする */
    if (!strcmp(&file_name[len-4], ".lmn")) {
      if (getenv(ENV_LMNTAL_HOME)) {
        FILE *fp_compiled;

        fp_compiled = lmntal_compile_file(file_name);
        if (!fp_compiled) {
          fprintf(stderr, "Failed to run lmntal compiler");
          exit(EXIT_FAILURE);
        }

        rs = load(fp_compiled);
        fclose(fp_compiled);
      }
      else {
        fprintf(stderr, "environment variable \"LMNTAL_HOME\" is not set");
        exit(EXIT_FAILURE);
      }
    }
    else {
      rs = load(fp);
    }
    fclose(fp);
  }
  else {
    perror(file_name);
    exit(EXIT_FAILURE);
  }

  return rs;
}

static const char * const extension_table[] = {
  ""
  ,"lmn"
  ,"il"
  ,"so"
};

static int file_type(const char *extension)
{
  int i = 0;
  int filetype = 0;

  for(i=1; i<sizeof(extension_table)/sizeof(extension_table[0]); ++i){
    if(! strcmp(extension, extension_table[i])){
      filetype = i;
      break;
    }
  }

  return filetype;
}

int load_loading_tbl_entry(st_data_t basename, st_data_t filetype, void *path)
{
  const char *extension = extension_table[filetype];
  char *buf = LMN_NALLOC(char, strlen((char *)path) + NAME_MAX + 2);

  sprintf(buf, "%s%s%s.%s", (char *)path, DIR_SEPARATOR_STR, (char *)basename, extension);
  load_file(buf);
  LMN_FREE(buf);
  return ST_CONTINUE;
}

int free_loading_tbl_entry(st_data_t basename, st_data_t filetype, void *path)
{
  LMN_FREE(basename);
  return ST_CONTINUE;
}

/* pathのディレクトリ内のファイルを中間コードとしてロードする */
/* 拡張子を除いた名前がおなじファイルがある場合extension_tableで指定した優先順で1種類のみ読み込む */
void load_il_files(char *path)
{
  int path_len = strlen(path);
  char *buf = LMN_NALLOC(char, path_len + NAME_MAX + 2);
  DIR *dir = opendir(path);

  if (dir) {
    st_table_t loading_files_type = st_init_strtable();
    struct stat st;
    struct dirent* dp;

    /* 読み込むファイルをリストアップする */
    while ( (dp = readdir(dir)) != NULL ){
      sprintf(buf, "%s%s%s", path, DIR_SEPARATOR_STR, dp->d_name);
      stat(buf, &st);
      if (S_ISREG(st.st_mode)) {
        char *ext = extension(dp->d_name);
        int filetype = file_type(ext);

        if(filetype > 0){ /* 読み込むべき拡張子なら追加 */
          char *basename = basename_ext(dp->d_name);
          st_data_t old_filetype;
          if (st_lookup(loading_files_type, (st_data_t)basename, &old_filetype)) {
            if(filetype > old_filetype) {
              st_insert(loading_files_type, (st_data_t)basename, filetype);
            }
            LMN_FREE(basename);
          } else {
            st_insert(loading_files_type, (st_data_t)basename, filetype);
          }
        }
        LMN_FREE(ext);
      }
    }

    /* 読み込む */
    st_foreach(loading_files_type, load_loading_tbl_entry, (st_data_t)path);
    /* 開放 */
    st_foreach(loading_files_type, free_loading_tbl_entry, (st_data_t)path);
    st_free_table(loading_files_type);
  }
  closedir(dir);
  LMN_FREE(buf);
}

/* ファイルが*.lmnならコンパイル結果のFILE*を返し、
   ファイルが*.ilならfopen結果のFILE*を返し、
   それ以外の拡張子だったり存在しないファイルだったらNULLを返す */
FILE *fopen_il_file(char *file_name)
{
  FILE *fp;
  int len = strlen(file_name);

  if ((fp = fopen(file_name, "r"))) {
    /* 拡張子がlmnならばJavaによる処理系で中間言語にコンパイルする */
    if (!strcmp(&file_name[len-4], ".lmn")) {
      if (getenv(ENV_LMNTAL_HOME)) {
        FILE *fp_compiled;

        fp_compiled = lmntal_compile_file(file_name);
        if (!fp_compiled) {
          fprintf(stderr, "Failed to run lmntal compiler");
          exit(EXIT_FAILURE);
        }

        return fp_compiled;
      }
      else {
        fprintf(stderr, "environment variable \"LMNTAL_HOME\" is not set");
      }
    }
    else if(!strcmp(&file_name[len-3], ".il")) {
      return fp;
    }
  }

  return 0;
}

int ilparse(yyscan_t scanner, IL *il, Rule *rule);

/* inから中間言語を読み込み、構文木を作る。構文木はilに設定される。
   正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
int il_parse(FILE *in, IL *il)
{
  int r;
  yyscan_t scanner;
  struct lexer_context c;

  /* ルールセットのローカルなIDとグローバルなIDの対応表 */
  c.ruleset_id_tbl = st_init_numtable();

  illex_init(&scanner);
  ilset_extra(&c, scanner);
  ilset_in(in, scanner);
  r = ilparse(scanner, il, NULL);
  illex_destroy(scanner);

  st_free_table(c.ruleset_id_tbl);

  return r;
}

int il_parse_rule(FILE *in, Rule *rule)
{
  int r;
  yyscan_t scanner;
  struct lexer_context c;

  /* ルールセットのローカルなIDとグローバルなIDの対応表 */
  c.ruleset_id_tbl = st_init_numtable();

  illex_init(&scanner);
  ilset_extra(&c, scanner);
  ilset_in(in, scanner);
  r = ilparse(scanner, NULL, rule);
  illex_destroy(scanner);

  st_free_table(c.ruleset_id_tbl);

  return r;
}

char *create_formatted_basename(const char *filepath)
{
  const char *begin = strrchr(filepath, DIR_SEPARATOR_CHAR); /* パス内最後の/を探す */
  const char *end;
  char *basename;
  int i;
  const char *p;

  if(begin != NULL){ /* もし/があればその次がファイル名の先頭 */
    begin += 1;
  }else{
    begin = filepath; /* もし/がなければ全体の先頭がファイル名の先頭 */
  }

  end = strchr(begin, '.'); /* ファイル名最初の.を探す ないと困る */
  basename = lmn_malloc(end-begin +1);
  for (i=0,p=begin; i<end-begin; ++i,++p) {
    if (isalpha(*p) || isdigit(*p))
      basename[i] = *p;
    else
      basename[i] = 'O'; /* 記号は全部Oにする */
  }
  basename[end-begin] = '\0';

  return basename;
}

