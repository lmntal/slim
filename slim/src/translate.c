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

#include "translate.h"
#include "syntax.h"
#include "arch.h"
#include "symbol.h"
#include <stdio.h>

/* just for debug ! */
FILE *OUT;

int parse(FILE *in, IL *il);

/* 各ブロックが関数になるように変えた */
/* 関数名は BOOL trans_FILENAME_RULESETID_RULEID_BLOCKID (?) */
/* SLIMから読み出すためのインターフェイスはまだ適当 */
/* 出力されたCソースをコンパイルする方法もまだ未定 */

/* とりあえずwtとatを参照する場合はWT,ATマクロを使うことにする */
/* とりあえずwt_sizeを参照する場合はWT_SIZEマクロを使うことにする */
/* その他グローバル変数hgoeはHOGEというマクロでアクセスする */
/* 関数はとりあえずそのまま */

/* 機械出力する部分の予定 */
void translate_instruction_generated(Instruction inst, ArgList al, char *header, int indent, char *failcode)
{
  fprintf(OUT, "other %d\n", inst_get_id(inst));
}

/* return nextbegin index */
int translate_instructions(InstList il, int begin, int end, char *header, int indent, char *failcode)
{
  int i;
  for(i=begin; i<end; ++i){
    Instruction inst = inst_list_get(il, i);
    ArgList al = inst_get_args(inst);

    switch(inst_get_id(inst)){
    case INSTR_SPEC:
    case INSTR_COMMIT:
      {
        break;
      }
    case INSTR_FINDATOM:
      {
        fprintf(OUT, "findatom for(;;){%d\n", inst_arg_get_var(arg_list_get(al,0)));
        i = translate_instructions(il, i+1, end, header, indent+1, "continue;\n");
        --i;
        fprintf(OUT, "}\n");
        fprintf(OUT, failcode);
        break;
      }
    case INSTR_ANYMEM:
      {
        fprintf(OUT, "anymem for(;;){%d\n", inst_arg_get_var(arg_list_get(al,0)));
        i = translate_instructions(il, i+1, end, header, indent+1, "continue;\n");
        --i;
        fprintf(OUT, "}\n");
        fprintf(OUT, failcode);
        break;
      }
    case INSTR_JUMP:
      {
        fprintf(OUT, "if(trans_%s_%d(mem)) return 1;\nelse %s", header, inst_arg_get_label(arg_list_get(al, 0)), failcode);
        break;
      }
    case INSTR_PROCEED:
      {
        fprintf(OUT, "return 1;\n");
        break;
      }
    default:
      {
        translate_instruction_generated(inst, al, header, indent, failcode);
        break;
      }
    }
  }

  return end;
}

void translate_block(InstBlock ib, char *header, char *defaultlabel)
{
  InstList il = inst_block_get_instructions(ib);
  int num = inst_list_num(il);
  int i;
  
  fprintf(OUT, "BOOL trans_%s_", header);
  if(inst_block_has_label(ib)){
    fprintf(OUT, "%d", inst_block_get_label(ib));
  }else{
    fprintf(OUT, "%s", defaultlabel);
  }
  fprintf(OUT, "(LmnMembrane *mem)\n{\n");

  translate_instructions(il, 0, num, header, 0, "return 0;\n");

  fprintf(OUT, "}\n");
}

void translate_rule(Rule r, char *header)
{
  fprintf(OUT, "/*%s*/\n", lmn_id_to_name(rule_get_name(r)));
  translate_block(rule_get_mmatch(r), header, "mmatch");
  translate_block(rule_get_guard(r), header, "guard");
  translate_block(rule_get_body(r), header, "body");
}

void translate_ruleset(RuleSet rs, char *filename)
{
  RuleList rl = ruleset_get_rulelist(rs);
  int rulesetid = ruleset_get_id(rs);
  int num = rulelist_num(rl);
  int i;

  fprintf(OUT, "BOOL trans_%s_%d_is_system_ruleset = %d;\n", filename, rulesetid, ruleset_is_system_ruleset(rs));
  fprintf(OUT, "int trans_%s_%d_num = %d;\n", filename, rulesetid, num);
  fprintf(OUT, "trans_%s_%d_data[] = {\n", filename, rulesetid);
  for(i=0; i<num; ++i){
    fprintf(OUT, "\ttrnas_%s_%d_%d_mmatch", filename, rulesetid, i);
    if(i != num-1) fprintf(OUT, ",");
    fprintf(OUT, "\n");
  }
  fprintf(OUT, "};\n");
  fprintf(OUT, "\n");

  for(i=0; i<num; ++i){
    char header[500];
    sprintf(header, "%s_%d_%d", filename, rulesetid, i);
  
    translate_rule(rulelist_get(rl, i), header);

    if(i != num-1) fprintf(OUT, "\n");
  }
}

void translate(char *filepath, FILE *in)
{
  IL il;
  char *filename;

  /* just for debug ! */
  //OUT = stderr;
  //OUT = stdout;
  OUT = fopen("/dev/null", "w");

  if (il_parse(in, &il)) {
    /* 構文解析に失敗 */
    exit(EXIT_FAILURE);
  }

  if(filepath == NULL){
    filename = strdup("anonymous");
  }else{
    char *begin = strrchr(filepath, DIR_SEPARATOR_CHAR);
    char *end;
    
    if(begin != NULL){
      begin += 1;
    }else{
      begin = filepath;
    }
    
    end = strrchr(begin, '.');
    filename = malloc(end-begin +1);
    strncpy(filename, begin, end-begin);
    filename[end-begin] = '\0';
  }

  fprintf(OUT, "int init_%s(void){}\n\n", filename);
  
  {
    RuleSets rss = il_get_rulesets(il);
    int num = rulesets_num(rss);
    int i;
    for(i=0; i<num; ++i){
      translate_ruleset(rulesets_get(rss, i), filename);
      if(i != num-1) fprintf(OUT, "\n");
    }
  }

  free(filename);
  fprintf(stderr, "--translate is under construction\n");
}
