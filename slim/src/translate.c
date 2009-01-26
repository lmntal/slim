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

/* コピペ開始 */

#define READ_VAL(T,I,X)      ((X)=*(T*)(I), I+=sizeof(T))

#define READ_DATA_ATOM(dest, attr)              \
  do {                                          \
    switch (attr) {                             \
    case LMN_INT_ATTR:                          \
       READ_VAL(int, instr, (dest));            \
       break;                                   \
     case LMN_DBL_ATTR:                         \
     {                                          \
        double *x;                              \
        x = LMN_MALLOC(double);                 \
        READ_VAL(double, instr, *x);            \
        (dest) = (LmnWord)x;                    \
        break;                                  \
     }                                          \
     default:                                   \
        lmn_fatal("Implementation error");      \
     }                                          \
   } while (0)

#define READ_CONST_DATA_ATOM(dest, attr)        \
  do {                                          \
    switch (attr) {                             \
    case LMN_INT_ATTR:                          \
       READ_VAL(int, instr, (dest));            \
       break;                                   \
     case LMN_DBL_ATTR:                         \
       (dest) = (LmnWord)instr;                 \
       instr += sizeof(double);                 \
       break;                                   \
     default:                                   \
        lmn_fatal("Implementation error");      \
     }                                          \
   } while (0)

#define READ_CMP_DATA_ATOM(attr, x, result)            \
       do {                                            \
          switch(attr) {                               \
          case LMN_INT_ATTR:                           \
            {                                          \
              int t;                                   \
              READ_VAL(int, instr, t);                 \
              (result) = ((int)(x) == t);              \
              break;                                   \
            }                                          \
          case LMN_DBL_ATTR:                           \
            {                                          \
              double t;                                \
              READ_VAL(double, instr, t);              \
              (result) = (*(double*)(x) == t);         \
              break;                                   \
            }                                          \
          default:                                     \
            lmn_fatal("Implementation error");         \
          }                                            \
          } while (0)

/* コピペ終わり */

/* translator/interpreter 共通の読み込み */
/* TYPE:読み込むデータの型 */
/* INSTR:命令列を表しているオブジェクト */
/* VAR:読込先変数 */
/* INDEX:読み込む引数番号 */
/* translatorではINSTR,VAR,INDEXのみ使用 */
/* interpreterではTYPE,INSTR,VARのみ使用 */
/* interpreterでは順不同 */
#define ABSTRUCT_READ(TYPE,INSTR,VAR,INDEX)

/* 再帰の呼び出し表現 */
/* translatorでは子を変換して挿入し,次のindexを返す */
/* interpreterではinterpret関数の呼び出しをそのまま挿入 */
/* translator:
  hogehoge;
  hogehoge;
  for(hoge;fuga;foo){
    hogehoge;
    hogehoge;
    子要素の展開(ただし失敗時コードはcontinue)
      (成功してたら勝手にreturnするのでここには来ない)
  }
  hogehoge;
  失敗時コード;
*/
/* interpreter:
  hogehoge;
  hogehoge;
  for(hoge;fuga;foo){
    hogehoge;
    hogehoge;
    if(interpret(hoge,fuga,foo)){
      return TRUE;
    }
  }
  hogehoge;
  return FALSE;
*/
/* 子の展開は return TRUEも含めて行う？あちこちほころびがあって無理 */
/* 開放のための再帰はreturn TRUEで脱出されてリーク */
/* insertconnectorsinnullとinsertconnectorsのみなので何か考える */
/* 成功時コードをgoto 何とかにすればOK？ */
/* interpreter:
  hogehoge;
  INSERTCONNECTORSINNULL
  if(interpret(続き)){ //必ず成功
    ハッシュ開放
    return TRUE;
  }
*/
/* translator:
  hogehoge;
  INSERTCONNECTORSINNULL
  {
    続きの展開(ただし成功時コードはgoto LABEL)
  }
 LABEL:
  ハッシュ開放
  成功時コード;
*/

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
//void translate_instruction_generated(Instruction inst, ArgList al, char *header, int indent, char *successcode, char *failcode);
//BYTE *translate_instruction_generated(BYTE *inst, char *header, int indent, char *successcode, char *failcode
/*
void translate_instruction_generated(Instruction inst, ArgList al, char *header, int indent, char *successcode, char *failcode)
{
  fprintf(OUT, "other %d\n", inst_get_id(inst));
}
*/

/* return nextbegin index */
// int translate_instructions_old(InstList il, int begin, int end, char *header, int indent, char *successcode, char *failcode)
// {
//   int instr_index;
//   for(instr_index=begin; instr_index<end; ++instr_index){
//     Instruction inst = inst_list_get(il, instr_index);
//     ArgList al = inst_get_args(inst);
// 
//     switch(inst_get_id(inst)){
//     case INSTR_FINDATOM:
//       {
//       /*
//         fprintf(OUT, "findatom for(;;){%d\n", inst_arg_get_var(arg_list_get(al,0)));
//         i = translate_instructions(il, i+1, end, header, indent+1, successcode, "continue;\n");
//         --i;
//         fprintf(OUT, "}\n");
//         fprintf(OUT, failcode);
//         */
//         break;
//       }
//     case INSTR_ANYMEM:
//       {
//       /*
//         fprintf(OUT, "anymem for(;;){%d\n", inst_arg_get_var(arg_list_get(al,0)));
//         i = translate_instructions(il, i+1, end, header, indent+1, successcode, "continue;\n");
//         --i;
//         fprintf(OUT, "}\n");
//         fprintf(OUT, failcode);
//         */
//         break;
//       }
//     case INSTR_JUMP:
//       {
//       /*
//         fprintf(OUT, "if(trans_%s_%d(mem)) %selse %s", header, inst_arg_get_label(arg_list_get(al, 0)), successcode, failcode);
//         */
//         break;
//       }
//     case INSTR_PROCEED:
//       {
//       /*
//         fprintf(OUT, "%s", successcode);
//         */
//         break;
//       }
//     default:
//       {
// //        translate_instruction_generated(inst, al, header, indent, successcode, failcode);
//         break;
//       }
//     }
//   }
// 
//   return end;
// }

BYTE *translate_instructions_generated_new(BYTE *instr, Vector *v, char *header, char *successcode, char *failcode, LmnInstrOp op)
{
  return instr;
}

/*
  wがVectorにすでに含まれる場合はそのindexを、
  含まれない場合はwを追加してからそのindexを返す
  */
int vec_inserted_index(Vector *v, LmnWord w)
{
  int i;
  for(i=0; i<vec_num(v); ++i){
    if(vec_get(v, i) == w) return i;
  }
  vec_push(v, w);
  return vec_num(v) - 1;
}

/*
  ibの先頭から出力して行き,その階層の
  jump / proceed が出てくるまでを変換する
  関数のヘッダ等はつけない
  successcode, failcodeを渡す
  jump先は中間命令ではポインタになっているが
  このポインタ値の出現indexをシグネチャに
  このポインタ数値をそのままシグネチャにする
  call trans_**_**_0
  物理的に次の読み込み場所を返す
  */
BYTE *translate_instructions(BYTE *instr, Vector *v, char *header, char *successcode, char *failcode)
{
  LmnInstrOp op;
  while(1){
    READ_VAL(LmnInstrOp, instr, op);
    fprintf(OUT, "%d\n", op);
    switch(op){
    case INSTR_SPEC:{
      LmnInstrVar s0, s1;

      READ_VAL(LmnInstrVar, instr, s0);
      READ_VAL(LmnInstrVar, instr, s1);
      break;
    }
    case INSTR_JUMP:{
      LmnJumpOffset offset;
      LmnInstrVar num;
      LmnInstrVar n;
      BYTE *next;
      int next_index;
      
      READ_VAL(LmnJumpOffset, instr, offset);
      next = instr + offset;
      next_index = vec_inserted_index(v, (LmnWord)next);
      
      fprintf(OUT, "call trans_%s_%d\n", header, next_index);
      
      READ_VAL(LmnInstrVar, instr, num);
      for(; num--; ){
        READ_VAL(LmnInstrVar, instr, n);
      }
      READ_VAL(LmnInstrVar, instr, num);
      for(; num--; ){
        READ_VAL(LmnInstrVar, instr, n);
      }
      READ_VAL(LmnInstrVar, instr, num);
      for(; num--; ){
        READ_VAL(LmnInstrVar, instr, n);
      }
      return instr;
    }
    case INSTR_PROCEED:{
      return instr;
    }
    case INSTR_FINDATOM:{
      LmnInstrVar atomi, memi;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnLinkAttr, instr, attr);

      if (LMN_ATTR_IS_DATA(attr)) {
        assert(FALSE);
      } else { /* symbol atom */
        LmnFunctor f;
        LmnAtomPtr atom;

        READ_VAL(LmnFunctor, instr, f);
        fprintf(OUT, "findatom\n");
        instr = translate_instructions(instr, v, header, successcode, failcode);
      }
      return instr;
    }
    case INSTR_DEREF:{
      LmnInstrVar atom1, atom2, pos1, pos2;
      LmnByte attr;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, pos2);
      break;
    }
    case INSTR_COMMIT:{
      lmn_interned_str rule_name;
      LmnLineNum line_num;

      READ_VAL(lmn_interned_str, instr, rule_name);
      READ_VAL(LmnLineNum, instr, line_num);
      break;
    }
    case INSTR_LOADRULESET:{
      LmnInstrVar memi;
      LmnRulesetId id;
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnRulesetId, instr, id);
      break;
    }
    case INSTR_NEWATOM:{
      LmnInstrVar atomi, memi;
      LmnWord ap;
      LmnLinkAttr attr;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);
      READ_VAL(LmnLinkAttr, instr, attr);
      if (LMN_ATTR_IS_DATA(attr)) {
        READ_DATA_ATOM(ap, attr);
      } else { /* symbol atom */
        LmnFunctor f;

        READ_VAL(LmnFunctor, instr, f);
      }
      break;
    }
    case INSTR_ALLOCLINK:{
      LmnInstrVar link, atom, n;

      READ_VAL(LmnInstrVar, instr, link);
      READ_VAL(LmnInstrVar, instr, atom);
      READ_VAL(LmnInstrVar, instr, n);
      break;
    }
    case INSTR_UNIFYLINKS:{
      LmnInstrVar link1, link2, mem;

      READ_VAL(LmnInstrVar, instr, link1);
      READ_VAL(LmnInstrVar, instr, link2);
      READ_VAL(LmnInstrVar, instr, mem);
      break;
    }
    case INSTR_ENQUEUEATOM:{
      LmnInstrVar atom;

      READ_VAL(LmnInstrVar, instr, atom);
      /* do nothing */
      break;
    }
    case INSTR_FUNC:{
      LmnInstrVar atomi;
      LmnFunctor f;
      LmnLinkAttr attr;
      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnLinkAttr, instr, attr);
      if(LMN_ATTR_IS_DATA(attr)){
        exit(1);
      }else{
        READ_VAL(LmnFunctor, instr, f);
      }
      break;
    }
    case INSTR_DEQUEUEATOM:{
      LmnInstrVar atom;

      READ_VAL(LmnInstrVar, instr, atom);
      break;
    }
    case INSTR_REMOVEATOM:{
      LmnInstrVar atomi, memi;

      READ_VAL(LmnInstrVar, instr, atomi);
      READ_VAL(LmnInstrVar, instr, memi);

      break;
    }
    case INSTR_RELINK:{
      LmnInstrVar atom1, atom2, pos1, pos2, memi;
      LmnAtomPtr ap;
      LmnByte attr;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);
      break;
    }
    case INSTR_NEWLINK:{
      LmnInstrVar atom1, atom2, pos1, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);
      break;
    }
    case INSTR_FREEATOM:{
      LmnInstrVar atomi;

      READ_VAL(LmnInstrVar, instr, atomi);

      break;
    }
    case INSTR_UNIFY:{
      LmnInstrVar atom1, pos1, atom2, pos2, memi;

      READ_VAL(LmnInstrVar, instr, atom1);
      READ_VAL(LmnInstrVar, instr, pos1);
      READ_VAL(LmnInstrVar, instr, atom2);
      READ_VAL(LmnInstrVar, instr, pos2);
      READ_VAL(LmnInstrVar, instr, memi);
      break;
    }
    default:{
      //機械生成
      instr = translate_instructions_generated_new(instr, v, header, successcode, failcode, op);
      return instr;
    }
    }
  }
/*
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

  translate_instructions(il, 0, num, header, 0, "return 1;\n", "return 0;\n");

  fprintf(OUT, "}\n");
  */
}

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
    fprintf(OUT, "\ttrnas_%s_%d_%d_0", filename, rulesetid, i);
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

void translate(char *filepath, FILE *in)
{
  IL il;
  char *filename;

  /* just for debug ! */
  //OUT = stderr;
  //OUT = stdout;
  OUT = fopen("/dev/null", "w");

  load(in);
  
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
  /* ruleset id is 2,3,4,5... ? */
    int i;
    for(i=2; ; ++i){
      LmnRuleSet rs = lmn_ruleset_from_id(i);
      if(rs == NULL) break;

      translate_ruleset(rs, filename);
    }
  }

  free(filename);
  fprintf(stderr, "--translate is under construction\n");
}
