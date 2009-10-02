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

void translate_rule(LmnRule r, char *header)
{
  Vector *v = vec_make(4);
  vec_push(v, (LmnWord)lmn_rule_get_inst_seq(r));
  fprintf(OUT, "/*%s*/\n", lmn_id_to_name(lmn_rule_get_name(r)));
  int i;
  for(i=0; i<vec_num(v); ++i){
    BYTE *p = (BYTE*)vec_get(v, i);
    fprintf(OUT, "BOOL trans_%s_%d(LmnMembrane *mem)\n{\n", header, i);
    //translate_instructions(p, v, header, "return 1", "return 0");
    fprintf(OUT, "}\n");
  }
}

void translate_ruleset(LmnRuleSet rs, char *filename)
{
  int rulesetid = lmn_ruleset_get_id(rs);
  int num = lmn_ruleset_rule_num(rs);
  int i;

  //fprintf(OUT, "BOOL trans_%s_%d_is_system_ruleset = %d;\n", filename, rulesetid, ruleset_is_system_ruleset(rs));
  fprintf(OUT, "int trans_%s_%d_num = %d;\n", filename, rulesetid, num);
  fprintf(OUT, "trans_%s_%d_data[] = {\n", filename, rulesetid);
  for(i=0; i<num; ++i){
    fprintf(OUT, "  trnas_%s_%d_%d_0", filename, rulesetid, i);
    if(i != num-1) fprintf(OUT, ",");
    fprintf(OUT, "\n");
  }
  fprintf(OUT, "};\n");
  fprintf(OUT, "\n");

  for(i=0; i<num; ++i){
    char header[500];
    sprintf(header, "%s_%d_%d", filename, rulesetid, i);
  
    translate_rule(lmn_ruleset_get_rule(rs, i), header);

    if(i != num-1) fprintf(OUT, "\n");
  }
}

static void print_trans_header()
{
  //出力されたファイル用のヘッダを用意して仮宣言
  //それとは別にtrans_maindata等の構造をしまうヘッダを用意(so.h)
  fprintf(OUT, "#include \"so.h\"\n");
  fprintf(OUT, "#include \"load.h\"\n");
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

static void print_trans_rulesets(const char *filename)
{
  int count = count_rulesets();
  int i;

  fprintf(OUT, "struct trans_ruleset trans_%s_maindata_rulesets[%d] = {\n", filename, count);
  /* ruleset id is 2,3,4,5... ? 1:systemrulesetただし登録はされていない */
  /* ruleset0番は存在しないが数合わせに出力 */
  fprintf(OUT, "  {0,0},\n");
  /* ruleset1番はtableに登録されていないがsystemrulesetなので出力 */
  //fprintf(OUT, "  {%d,trans_%s_1},\n", lmn_ruleset_rule_num(system_ruleset), filename);
  fprintf(OUT, "  {0,0},\n");
  /* 2番以降は普通のrulesetなので出力(どれが初期データルールかはload時に拾う) */
  for(i=2; i<count; ++i){
    LmnRuleSet rs = lmn_ruleset_from_id(i);
    assert(rs != NULL); /* countで数えているからNULLにあたることはないはず */

    //fprintf(OUT, "  {%d,trans_%s_%d}", lmn_ruleset_rule_num(rs), filename, i);
    fprintf(OUT, "  {0,0}");
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
