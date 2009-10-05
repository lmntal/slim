/*
 * translate.c
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
 *    3. Neither the name of the Ueda Laboratory LMNtal Groupy LMNtal
 *       Group nor the names of its contributors may be used to
 *       endorse or promote products derived from this software
 *       without specific prior written permission.
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
 * $Id: translate.c,v 1.34 2008/10/16 18:12:27 sasaki Exp $
 */

#include "load.h"
#include "rule.h"
#include "translate.h"
#include "syntax.h"
#include "arch.h"
#include "symbol.h"
#include "vector.h"
#include "atom.h"
#include "error.h"
#include <stdio.h>

/* just for debug ! */
static FILE *OUT;

/*
  pの先頭から出力して行き,その階層のjump/proceedが出てくるまでを変換する
  jump先は中間命令ではアドレスになっているが,
  そのポインタ値が何個めのjump先として現れたか(index)を,その関数のシグネチャに使う
  物理的に次の読み込み場所を返す
*/
static const BYTE *translate_instructions(const BYTE *p, Vector *jump_points, const char *header, const char *successcode, const char *failcode)
{
  fprintf(OUT, "  %s;\n", failcode);
  return p; /* ルート以外でpを返すと無限ループ */
}

static void translate_rule(LmnRule rule, const char *header)
{
  Vector *jump_points = vec_make(4);
  int i;

  vec_push(jump_points, (LmnWord)lmn_rule_get_inst_seq(rule));

  for(i=0; i<vec_num(jump_points) /*変換中にjump_pointsは増えていく*/; ++i){
    BYTE *p = (BYTE*)vec_get(jump_points, i);
    fprintf(OUT, "BOOL %s_%d(struct ReactCxt* r, LmnMembrane* m)\n", header, i); /* TODO rとmじゃ気持ち悪い m=wt[0]なのでmは多分いらない */
    fprintf(OUT, "{\n");
    /* (変換するスタート地点, 変換する必要のある部分の記録, ルールのシグネチャ:trans_**_**_**, 成功時コード, 失敗時コード) */
    translate_instructions(p, jump_points, header, "return 1", "return 0");
    fprintf(OUT, "}\n");
  }

  /* 各関数の前方宣言をすることができないので,関数を呼ぶ時には自分で前方宣言をする */
  /* trans_***(); ではなく { extern trans_***(); trans_***(); } と書くだけ */
}

static void print_trans_header()
{
  fprintf(OUT, "#include \"so.h\"\n");
  fprintf(OUT, "#include \"load.h\"\n");
  fprintf(OUT, "#include \"rule.h\"\n");
  fprintf(OUT, "\n");
}

/* ルールセットの総数を数える. ルールセット0番, 1番を数に含む (0番は使わない(番号合わせ),1番はsystem) */
static int count_rulesets()
{
  int i;
  
  for(i=2; ; ++i){
    LmnRuleSet rs = lmn_ruleset_from_id(i);
    if(rs == NULL) break;
  }

  return i;
}

static void print_trans_maindata(const char *filename)
{
  int i;
  int ruleset_count;

  fprintf(OUT, "struct trans_maindata trans_%s_maindata = {\n", filename);

  /* シンボルの個数(0番anonymousも数える) */
  fprintf(OUT, "  %d, /*count of symbol*/\n", count_symbols());
  /* シンボルの配列 */
  fprintf(OUT, "  trans_%s_maindata_symbols, /*symboltable*/\n", filename);
  /* ファンクタの個数 */
  fprintf(OUT, "  %d, /*count of functor*/\n", lmn_functor_table.next_id);
  /* ファンクタの配列 */
  fprintf(OUT, "  trans_%s_maindata_functors, /*functortable*/\n", filename);
  /* ルールセットの個数 */
  fprintf(OUT, "  %d, /*count of ruleset*/\n", count_rulesets());
  /* ルールセットオブジェクトへのポインタの配列 */
  fprintf(OUT, "  trans_%s_maindata_rulesets, /*rulesettable*/\n", filename);
  /* シンボルid変換テーブル */
  fprintf(OUT, "  trans_%s_maindata_symbolexchange, /*symbol id exchange table*/\n", filename);
  /* ファンクタid変換テーブル */
  fprintf(OUT, "  trans_%s_maindata_functorexchange, /*functor id exchange table*/\n", filename);
  /* ルールセットid変換テーブル */
  fprintf(OUT, "  trans_%s_maindata_rulesetexchange /*ruleset id exchange table*/\n", filename);
  fprintf(OUT, "};\n\n");
}

static void print_trans_symbols(const char *filename)
{
  int i;
  int count = count_symbols();
  
  fprintf(OUT, "const char *trans_%s_maindata_symbols[%d] = {\n", filename, count);
  for(i=0; i<count; ++i){
    fprintf(OUT, "  \"%s\"", lmn_id_to_name(i));
    if(i != count-1) fprintf(OUT, ",");
    fprintf(OUT, "\n");
  }
  fprintf(OUT, "};\n");

  fprintf(OUT, "int trans_%s_maindata_symbolexchange[%d];\n\n", filename, count);
}

static void print_trans_functors(const char *filename)
{
  int i;
  int count = lmn_functor_table.next_id;
  /* idは0から, next_idが1なら既に1個登録済み => count==next_id */
  
  fprintf(OUT, "struct LmnFunctorEntry trans_%s_maindata_functors[%d] = {\n", filename, count);
  for(i=0; i<count; ++i){
    fprintf(OUT,
            "  {%d, %d, %d, %d}",
            lmn_functor_table.entry[i].special,
            lmn_functor_table.entry[i].module,
            lmn_functor_table.entry[i].name,
            lmn_functor_table.entry[i].arity);
    if(i != count-1) fprintf(OUT, ",");
    fprintf(OUT, "\n");
  }
  fprintf(OUT, "};\n");
  
  fprintf(OUT, "int trans_%s_maindata_functorexchange[%d];\n\n", filename, count);
}

static void print_trans_rules(const char *filename)
{
  int count = count_rulesets();
  int i;
  int buf_len = strlen(filename) + 50; /* 適当にこれだけあれば足りるはず */
  char *buf = malloc(buf_len + 1);

  /* システムルールセットの出力 */
  for(i=0; i<lmn_ruleset_rule_num(system_ruleset); ++i){
    /* システムルールの関数をここで出力 */
    snprintf(buf, buf_len, "trans_%s_1_%d", filename, i); /* シグネチャ作成 */
    translate_rule(lmn_ruleset_get_rule(system_ruleset, i), buf);
  }
  fprintf(OUT, "LmnTranslated trans_%s_1_rules[%d] = {", filename, lmn_ruleset_rule_num(system_ruleset));
  for(i=0; i<lmn_ruleset_rule_num(system_ruleset); ++i){
    if(i != 0) fprintf(OUT, ", ");
    fprintf(OUT, "trans_%s_1_%d_0", filename, i); /* 各ルールの先頭関数を配列に */
  }
  fprintf(OUT, "};\n\n");

  /* 通常ルールセットの出力 */
  for(i=2; i<count; ++i){
    int j;
    LmnRuleSet rs = lmn_ruleset_from_id(i);
    assert(rs != NULL); /* countで数えているからNULLにあたることはないはず */

    for(j=0; j<lmn_ruleset_rule_num(rs); ++j){
      /* ルールの関数をここで出力 */
      snprintf(buf, buf_len, "trans_%s_%d_%d", filename, i, j);
      translate_rule(lmn_ruleset_get_rule(rs, j), buf);
    }

    fprintf(OUT, "LmnTranslated trans_%s_%d_rules[%d] = {", filename, i, lmn_ruleset_rule_num(rs));
    for(j=0; j<lmn_ruleset_rule_num(rs); ++j){
      if(j != 0) fprintf(OUT, ", ");
      fprintf(OUT, "trans_%s_%d_%d_0", filename, i, j); /* 各ルールの先頭関数を配列に */
    }
    fprintf(OUT, "};\n\n");
  }
}

static void print_trans_rulesets(const char *filename)
{
  int count = count_rulesets();
  int i;

  /* ルールセットテーブルで各ルールセットのデータ名を参照するので、先に個々のデータを出力する */
  print_trans_rules(filename);

  fprintf(OUT, "struct trans_ruleset trans_%s_maindata_rulesets[%d] = {\n", filename, count);
  /* ruleset id is 2,3,4,5... ? 1:systemrulesetただし登録はされていない */
  /* ruleset0番は存在しないが数合わせに出力 */
  fprintf(OUT, "  {0,0},\n");
  /* ruleset1番はtableに登録されていないがsystemrulesetなので出力 */
  fprintf(OUT, "  {%d,trans_%s_1_rules},\n", lmn_ruleset_rule_num(system_ruleset), filename);
  /* 2番以降は普通のrulesetなので出力(どれが初期データルールかはload時に拾う) */
  for(i=2; i<count; ++i){
    LmnRuleSet rs = lmn_ruleset_from_id(i);
    assert(rs != NULL); /* countで数えているからNULLにあたることはないはず */

    fprintf(OUT, "  {%d,trans_%s_%d_rules}", lmn_ruleset_rule_num(rs), filename, i);
    if(i != count-1) fprintf(OUT, ",");
    fprintf(OUT, "\n");
  }
  fprintf(OUT, "};\n");

  fprintf(OUT, "int trans_%s_maindata_rulesetexchange[%d];\n\n", filename, count);
}

static void print_trans_initfunction(const char *filename)
{
  fprintf(OUT, "void init_%s(void){\n", filename);
  
  fprintf(OUT, "  extern void helloworld(const char*);\n");
  fprintf(OUT, "  helloworld(\"%s\");\n", filename);
  
  fprintf(OUT, "}\n\n");
}

void translate(char *filepath)
{
  IL il;
  char *filename;

  /* just for debug ! */
  //OUT = stderr;
  //OUT = stdout;
  OUT = fopen("/dev/null", "w");

  if(filepath == NULL){
    filename = strdup("anonymous");
  }else{
    filename = create_basename(filepath);
  }
  
  print_trans_header();
  print_trans_symbols(filename);
  print_trans_functors(filename);
  print_trans_rulesets(filename);
  print_trans_maindata(filename);
  print_trans_initfunction(filename);

  free(filename);
  fprintf(stderr, "--translate is under construction\n");
}
