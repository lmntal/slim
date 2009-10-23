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
#include "translate.h"
#include "so.h"
#include "mc.h"

/* just for debug ! */
static FILE *OUT;

void tr_print_list(int indent, int argi, int list_num, const LmnWord *list)
{
  int i;
  
  print_indent(indent); fprintf(OUT, "int targ%d_num = %d;\n", argi, list_num);
  print_indent(indent); fprintf(OUT, "LmnWord targ%d[] = {", argi);
  for(i=0; i<list_num; ++i){
    if(i != 0) fprintf(OUT, ", ");
    fprintf(OUT, "%ld", list[i]);
  }
  fprintf(OUT, "};\n");
}

void tr_instr_commit_ready(struct ReactCxt *rc, LmnRule rule, lmn_interned_str rule_name, LmnLineNum line_num, LmnMembrane **ptmp_global_root, LmnWord **pwt_temp, LmnByte **pat_temp)
{
  if(rule) lmn_rule_set_name(rule, rule_name); /* LmnTranslatedがruleを持っていないので実行できない */
  
  if (RC_GET_MODE(rc, REACT_ND)) {
    unsigned int i;
    /* グローバルルート膜のコピー */
    SimpleHashtbl *copymap;
    LmnMembrane *tmp_global_root;
#ifdef PROFILE
    if (lmn_env.profile_level >= 1) {
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
      else if(hashtbl_contains(copymap, wt[i])) {
        wtcp[i] = hashtbl_get_default(copymap, wt[i], 0);
      }
      else if(wt[i] == (LmnWord)RC_GROOT_MEM(rc)) { /* グローバルルート膜 */
        wtcp[i] = (LmnWord)tmp_global_root;
      }
      else if (at[i] == LMN_DBL_ATTR) {
        LMN_COPY_DBL_ATOM(wtcp[i], wt[i]);
      }
    }
    hashtbl_free(copymap);

    /* 変数配列および属性配列をコピーと入れ換える */
    SWAP(LmnWord *, wtcp, wt);
    SWAP(LmnByte *, atcp, at);
#ifdef PROFILE
    if (lmn_env.profile_level >= 1) {
      status_finish_commit();
    }
#endif

    /* 処理中の変数を外へ持ち出す */
    *ptmp_global_root = tmp_global_root;
    *pwt_temp = wtcp;
    *pat_temp = atcp;
  }
}

BOOL tr_instr_commit_finish(struct ReactCxt *rc, LmnRule rule, lmn_interned_str rule_name, LmnLineNum line_num, LmnMembrane **ptmp_global_root, LmnWord **pwt_temp, LmnByte **pat_temp)
{
  if(RC_GET_MODE(rc, REACT_ND)) {
    /* 処理中の変数を外から持ち込む */
    LmnMembrane *tmp_global_root = *ptmp_global_root;
    LmnWord *wtcp = *pwt_temp;
    LmnByte *atcp = *pat_temp;

    lmn_react_systemruleset(rc, (LmnMembrane *)wt[0]);
    nd_react_cxt_add_expanded(rc, tmp_global_root, rule); /* このruleはNULLではまずい気がする */

    /* 変数配列および属性配列を元に戻す（いらないかも？） */
    SWAP(LmnWord *, wtcp, wt);
    SWAP(LmnByte *, atcp, at);

    LMN_FREE(wtcp);
    LMN_FREE(atcp);

    return FALSE;
  } else {
    return TRUE;
  }
}

BOOL tr_instr_jump(LmnTranslated f, struct ReactCxt *rc, LmnMembrane *thisisrootmembutnotused, int newid_num, const int *newid, LmnWord **pwt, LmnByte **pat, unsigned int *pwt_size)
{
  LmnWord *wt_org = *pwt;
  LmnByte *at_org = *pat;
  unsigned int wt_size_org = *pwt_size;
  LmnWord *wt2 = LMN_NALLOC(LmnWord, wt_size_org);
  LmnByte *at2 = LMN_NALLOC(LmnByte, wt_size_org);
  BOOL ret;
  int i;

  /* MCだと配列の中身を値に応じてコピーするためあらかじめクリアする必要がある */
  if (RC_GET_MODE(rc, REACT_ND)) {
    for(i = 0; i < wt_size; i++) {
      wt2[i] = at2[i] = 0;
    }
  }

  for(i=0; i<newid_num; ++i){
    wt2[i] = wt_org[newid[i]];
    at2[i] = at_org[newid[i]];
  }

  *pwt = wt2;
  *pat = at2;
  
  ret = (*f)(rc, thisisrootmembutnotused);

  LMN_FREE(wt);
  LMN_FREE(at);
  *pwt = wt_org;
  *pat = at_org;
  *pwt_size = wt_size_org;

  return ret;
}

Vector vec_const_temporary_from_array(int size, const LmnWord *w)
{
  Vector v;
  v.num = size;
  v.cap = size;
  v.tbl = (LmnWord*)w;
  return v; /* コピーして返す tblはwをそのまま使うのでvec_freeしてはいけない */
}

int vec_inserted_index(Vector *v, LmnWord w)
{
  int i;
  for(i=0; i<vec_num(v); ++i){
    if(vec_get(v, i) == w) return i;
  }
  vec_push(v, w);
  return vec_num(v) - 1;
}

char *automalloc_sprintf(const char *format, ...)
{
  char trush[2];
  va_list ap;
  int buf_len;
  char *buf;

  va_start(ap, format);
  buf_len = vsnprintf(trush, 2, format, ap);
  va_end(ap);
  
  buf = lmn_malloc(buf_len + 1);
  
  va_start(ap, format);
  vsnprintf(buf, buf_len+1, format, ap);
  va_end(ap);

  return buf;
}

void print_indent(int n)
{
  int i;
  for(i=0; i<n*2; ++i){
    fprintf(OUT, " ");
  }
}

/* 常に失敗する translate_generatorまわりがおかしくなったらこれをコメントイン */
/*
const BYTE *translate_instruction_generated(const BYTE *p, Vector *jump_points, const char *header, const char *successcode, const char *failcode, int indent, int *finishflag)
{
  *finishflag = -1;
  return p;
}
*/

const BYTE *translate_instruction(const BYTE *instr, Vector *jump_points, const char *header, const char *successcode, const char *failcode, int indent, int *finishflag)
{
  LmnInstrOp op;
  const BYTE *op_address = instr;
  
  READ_VAL(LmnInstrOp, instr, op);

  switch (op) {
  case INSTR_JUMP:{
    /* 残念ながら引数読み込み途中のinstrからオフセットジャンプするため */
    /* 先に全部読み込んでしまうと場所を忘れてしまう */
    LmnInstrVar num, i, n;
    LmnJumpOffset offset;
    LmnRuleInstr next;
    int next_index;
    
    READ_VAL(LmnJumpOffset, instr, offset);
    next = (BYTE*)instr + offset; /*ワーニング抑制 */
    next_index = vec_inserted_index(jump_points, (LmnWord)next);

    print_indent(indent); fprintf(OUT, "{\n");
    print_indent(indent); fprintf(OUT, "  static const int newid[] = {");

    i = 0;
    /* atom */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if(i != 0) fprintf(OUT, ",");
      fprintf(OUT, "%d", n);
    }
    /* mem */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if(i != 0) fprintf(OUT, ",");
      fprintf(OUT, "%d", n);
    }
    /* vars */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if(i != 0) fprintf(OUT, ",");
      fprintf(OUT, "%d", n);
    }

    fprintf(OUT, "};\n");
    print_indent(indent); fprintf(OUT, "  extern BOOL %s_%d();\n", header, next_index);
    print_indent(indent); fprintf(OUT, "  if(tr_instr_jump(%s_%d, rc, thisisrootmembutnotused, %d, newid, &wt, &at, &wt_size))\n", header, next_index, i);
    print_indent(indent); fprintf(OUT, "    %s;\n", successcode);
    print_indent(indent); fprintf(OUT, "  else\n");
    print_indent(indent); fprintf(OUT, "    %s;\n", failcode);
    print_indent(indent); fprintf(OUT, "}\n");
    
    *finishflag = 0;
    return instr;
  }

  default:
    *finishflag = -1; /* 常に失敗,終了 */
    return instr;
  }
}

/*
  pの先頭から出力して行き,その階層のjump/proceedが出てくるまでを変換する
  jump先は中間命令ではアドレスになっているが,
  そのポインタ値が何個めのjump先として現れたか(index)を,その関数のシグネチャに使う
  物理的に次の読み込み場所を返す
  (変換するスタート地点, 変換する必要のある部分の記録, ルールのシグネチャ:trans_**_**_**, 成功時コード, 失敗時コード, インデント)
*/
const BYTE *translate_instructions(const BYTE *p, Vector *jump_points, const char *header, const char *successcode, const char *failcode, int indent)
{
  while(1){
    /* 自動生成で変換可能な中間命令をトランスレートする */
    /* 終了フラグ: 正のとき変換成功+次を変換, 0のとき変換成功+jump/proceed等なので終了, 負のとき変換失敗 */
    int finishflag;
    const BYTE *next = translate_instruction_generated(p, jump_points, header, successcode, failcode, indent, &finishflag);

    if(finishflag > 0){
      p = next;
      continue;
    }else if(finishflag == 0){
      return next;
    }else{
      /* 自動生成で対処できない中間命令をトランスレートする */
      next = translate_instruction(p, jump_points, header, successcode, failcode, indent, &finishflag);
      if(finishflag > 0){
        p = next;
        continue;
      }else if(finishflag == 0){
        return next;
      }else{
        LmnInstrOp op;
        READ_VAL(LmnInstrOp, p, op);
        fprintf(stderr, "translator: unknown instruction: %d\n", op);
        exit(1);
      }
    }
  }
}

static void translate_rule(LmnRule rule, const char *header)
{
  Vector *jump_points = vec_make(4);
  int i;

  vec_push(jump_points, (LmnWord)lmn_rule_get_inst_seq(rule));

  for(i=0; i<vec_num(jump_points) /*変換中にjump_pointsは増えていく*/; ++i){
    BYTE *p = (BYTE*)vec_get(jump_points, i);
    fprintf(OUT, "BOOL %s_%d(struct ReactCxt* rc, LmnMembrane* thisisrootmembutnotused)\n", header, i); /* TODO rとmじゃ気持ち悪い m=wt[0]なのでmは多分いらない */
    fprintf(OUT, "{\n");
    /* (変換するスタート地点, 変換する必要のある部分の記録, ルールのシグネチャ:trans_**_**_**, 成功時コード, 失敗時コード, インデント) */
    translate_instructions(p, jump_points, header, "return TRUE", "return FALSE", 1);
    fprintf(OUT, "}\n");
  }

  /* 各関数の前方宣言をすることができないので,関数を呼ぶ時には自分で前方宣言をする */
  /* trans_***(); ではなく { extern trans_***(); trans_***(); } と書くだけ */
}

static void print_trans_header(const char *filename)
{
  fprintf(OUT, "/* this .c source is generated by slim --translate */\n");
  fprintf(OUT, "/* compile: gcc -shared -Wall -I includepathofslimsrc ----.c -o %s.so */\n", filename);
  fprintf(OUT, "/* run: LD_LIBRARY_PATH=\".\" slim ./%s.so */\n", filename);
  fprintf(OUT, "/* .so file name must be %s. */\n", filename);
  fprintf(OUT, "\n");
  fprintf(OUT, "#include \"so.h\"\n");
  fprintf(OUT, "\n");
  fprintf(OUT, "#define TR_GSID(x) (trans_%s_maindata.symbol_exchange[x])\n", filename);
  fprintf(OUT, "#define TR_GFID(x) (trans_%s_maindata.functor_exchange[x])\n", filename);
  fprintf(OUT, "#define TR_GRID(x) (trans_%s_maindata.ruleset_exchange[x])\n", filename);
  fprintf(OUT, "\n");
  fprintf(OUT, "extern struct trans_maindata trans_%s_maindata;\n", filename);
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
  /* モジュールの個数 */
  /* モジュールの配列 */
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
  
  /* fprintf(OUT, "  extern void helloworld(const char*);\n"); */
  /* fprintf(OUT, "  helloworld(\"%s\");\n", filename); */
  
  fprintf(OUT, "}\n\n");
}

static void print_trans_modules(const char *filename)
{

}

void translate(char *filepath)
{
  char *filename;

  /* just for debug ! */
  //OUT = stderr;
  OUT = stdout;
  //OUT = fopen("/dev/null", "w");

  if(filepath == NULL){
    filename = strdup("anonymous");
  }else{
    filename = create_formatted_basename(filepath);
  }
  
  print_trans_header(filename);
  print_trans_symbols(filename);
  print_trans_functors(filename);
  print_trans_rulesets(filename);
  print_trans_modules(filename);
  print_trans_maindata(filename);
  print_trans_initfunction(filename);

  free(filename);
  if(OUT != stdout) fprintf(stderr, "--translate is under construction\n");
}
