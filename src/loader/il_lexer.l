/*
 * il_lexer.l - Intermediate Language scanner
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
 * $Id: il_lex.l,v 1.5 2008/09/19 05:18:17 taisuke Exp $
 */

%option reentrant noyywrap bison-bridge bison-locations yylineno


%{
/* need this for the call to atof() below */
#include <math.h>
#include <memory.h>
#include <string.h>
#include "syntax.h"
#include "il_parser.h"
#include "vm/vm.h"


#define _POSIX_SOURCE 1
#define YY_NO_INPUT
#define YY_NO_UNISTD_H

struct lexer_context;
int get_instr_id(char *);

int ilget_column  (yyscan_t yyscanner);
void ilset_column (int  column_no , yyscan_t yyscanner);

static struct InstrSpec spec[];

/* エスケープキャラクタから文字への対応表 */
static char escape_char_map[] =
  {0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, '"',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,'\\',   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,'\n',   0,
   0,   0, '\r',  0,'\t',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0};

char *unescape_c_str(const char *s);

%}

%option nounput

DIGIT    [0-9]

%%

 /* ルールは行の先頭から書く。ルール以外（コメントやアクション）を先頭から
    書くことはできない */
 /* Rules must NOT be indented. Others (comments and actions) must be indented. */

 /* This can handle illmatic cases such as
    'slash star star star slash' or 'slash star slash star slash'.
    cf. コンパイラの構成と最適化 第2版, pp.65-66, 中田育男, 朝倉書店, 1999 */
"/*"[^*/]*"*/" /* eat comment */
"//".*                             /* eat one line comment */
[ \t\n\r]+                         /* eat blanks */
 /* float */
-?{DIGIT}+"."{DIGIT}+ { yylval->_float = atof(yytext); return FLOAT; }
 /* integer */
-?{DIGIT}+ { yylval->_int = atoi(yytext); return INT; }

 /* ruleset id */
@{DIGIT}+ {
  /* この時点でVM内で一意のルールセットIDに変換する */
  /* Convert input text into ruleset ID unique in the VM */
  st_data_t id_local = atoi(yytext+1);
  st_data_t id_global;
  struct lexer_context *c;

  c = yyget_extra(yyscanner);
  if (!st_lookup(c->ruleset_id_tbl, id_local, &id_global)) {
    /* テーブルになければ新しく追加 */
    /* Update ruleset ID table */
    id_global = lmn_gen_ruleset_id();
    st_insert(c->ruleset_id_tbl, id_local, id_global);
  }
  yylval->_int = (int)id_global;
  return RULESET_ID; }
"null" { /* name of anonymous membrane */
         yylval->str = ANONYMOUS;
         return DQUOTED_STRING; }

 /* label */
L{DIGIT}+ { yylval->_int = atoi(yytext+1); return LABEL; }
,                    { return COMMA; }
\.                   { return PERIOD; }
:                    { return COLON; }
"_"                  { return UNDERBAR; }
\{                   { return LBRACE; }
\}                   { return RBRACE; }
\[                   { return LBRACKET; }
\]                   { return RBRACKET; }
"$in_2"              { return INSIDE_PROXY; }
"$out_2"             { return OUTSIDE_PROXY; }
"Compiled SystemRuleset"   { return KW_COMPILED_SYSTEM_RULESET; }
"Compiled Ruleset"   { return KW_COMPILED_RULESET; }
"Compiled Uniq Rule" { return KW_COMPILED_UNIQ_RULE; }
"Compiled Rule"      { return KW_COMPILED_RULE; }
"--atommatch"        { return KW_ATOMMATCH; }
"--memmatch"         { return KW_MEMMATCH; }
"--guard"            { return KW_GUARD; }
"--body"             { return KW_BODY; }
"Inline"             { return KW_INLINE; }
"Module"             { return KW_MODULE; }

 /* instruction name */
[a-z2]+ {
  yylval->_int = get_instr_id(yytext);
  /* 変数のリストと命令のリストは構文解析では判別不可能なので
　 　命令のリストを持つ中間語命令を特別に扱う */
  /* Lists of variables and lists of instructions cannot be
     parsed distinguishedly, so treat instructions accompanied by
     a list of instructions as special cases. */
  if (yylval->_int == INSTR_LOOP) { return INST_TK_LOOP; }
  if (yylval->_int == INSTR_RUN) { return INST_TK_RUN; }
  if (yylval->_int == INSTR_NOT) { return INST_TK_NOT; }
  if (yylval->_int == INSTR_GROUP) { return INST_TK_GROUP; }
  if (yylval->_int == INSTR_BRANCH) { return INST_TK_BRANCH; }

  if (yylval->_int < 0) {
    fprintf(stderr, "unknown instruction name %s\n", yytext);
    exit(EXIT_FAILURE);
  }
  return INST_NAME; }

 /* double quoted string */
\"("\\\""|[^"])*\" {
    char *t, *t2;
    t = malloc(sizeof(char)*yyleng-1);
    strncpy(t, yytext+1, yyleng-2);
    t[yyleng-2] = '\0';
    t2 = unescape_c_str(t);
    yylval->str = lmn_intern(t2);
    free(t);
    free(t2);
    return DQUOTED_STRING; }

 /* single quoted string */
'[^']*' { char *t, *t2;
          t = malloc(sizeof(char)*yyleng-1);
          strncpy(t, yytext+1, yyleng-2);
          t[yyleng-2] = '\0';
          t2 = unescape_c_str(t);
          yylval->id = lmn_intern(t2);
          free(t);
          free(t2);
          return SQUOTED_STRING; }
<<EOF>>              { yyterminate(); return _EOF; }

%%

/* 命令の名前からIDを得る */
/* gets instruction ID from instruction name */
int get_instr_id(char *name)
{
  int i;
  for (i = 0; spec[i].op_str; i++) {
    if (!strcmp(name, spec[i].op_str)) return spec[i].op;
  }
  return -1;
}

/* エスケープシーケンスを含むCの文字列を、エスケープキャラクタを実際の
   文字に変換した、新しい文字列を返す */
/* returns newly allocated string which is unescaped form of src */
char *unescape_c_str(const char *src)
{
  int len = strlen(src);
  char *s = malloc(sizeof(char) * len + 1);
  int i, j;

  for (i = 0, j = 0; i < len; i++, j++) {
    char c;
    if (i < len - 1 && src[i] == '\\' && escape_char_map[(int)src[i+1]]) {
      c = escape_char_map[(int)src[i+1]];
      i++;
    } else {
      c = src[i];
    }
    s[j] = c;
  }
  s[j] = '\0';
  return s;
}

static struct InstrSpec spec[] = {
    {"deref", INSTR_DEREF, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"derefatom", INSTR_DEREFATOM, {InstrVar, InstrVar, InstrVar}},
    {"dereflink", INSTR_DEREFLINK, {InstrVar, InstrVar, InstrVar}},
    {"findatom", INSTR_FINDATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"findatom2", INSTR_FINDATOM2, {InstrVar, InstrVar, InstrVar, ArgFunctor}},

    {"lockmem", INSTR_LOCKMEM, {InstrVar, InstrVar, String}},
    {"anymem", INSTR_ANYMEM, {InstrVar, InstrVar, InstrVar, String}},

    {"getmem", INSTR_GETMEM, {InstrVar, InstrVar, InstrVar, String}},
    {"getparent", INSTR_GETPARENT, {InstrVar, InstrVar}},

    {"testmem", INSTR_TESTMEM, {InstrVar, InstrVar}},
    {"norules", INSTR_NORULES, {InstrVar}},
    {"nfreelinks", INSTR_NFREELINKS, {InstrVar, InstrVar}},
    {"natoms", INSTR_NATOMS, {InstrVar, InstrVar}},
    {"natomsindirect", INSTR_NATOMSINDIRECT, {InstrVar, InstrVar}},
    {"nmems", INSTR_NMEMS, {InstrVar, InstrVar}},
    {"eqmem", INSTR_EQMEM, {InstrVar, InstrVar}},
    {"neqmem", INSTR_NEQMEM, {InstrVar, InstrVar}},
    {"stable", INSTR_STABLE, {InstrVar}},

    {"func", INSTR_FUNC, {InstrVar, ArgFunctor}},
    {"notfunc", INSTR_NOTFUNC, {InstrVar, ArgFunctor}},
    {"eqatom", INSTR_EQATOM, {InstrVar, InstrVar}},
    {"neqatom", INSTR_NEQATOM, {InstrVar, InstrVar}},
    {"samefunc", INSTR_SAMEFUNC, {InstrVar, InstrVar}},

    {"dereffunc", INSTR_DEREFFUNC, {InstrVar, InstrVar, InstrVar}},
    {"getfunc", INSTR_GETFUNC, {InstrVar, InstrVar}},
    {"loadfunc", INSTR_LOADFUNC, {InstrVar, ArgFunctor}},
    {"eqfunc", INSTR_EQFUNC, {InstrVar, InstrVar}},
    {"neqfunc", INSTR_NEQFUNC, {InstrVar, InstrVar}},

    {"removeatom", INSTR_REMOVEATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"removeatom", INSTR_REMOVEATOM, {InstrVar, InstrVar}},
    {"newatom", INSTR_NEWATOM, {InstrVar, InstrVar, ArgFunctor}},
    {"newatomindirect", INSTR_NEWATOMINDIRECT, {InstrVar, InstrVar, InstrVar}},
    {"enqueueatom", INSTR_ENQUEUEATOM, {InstrVar}},
    {"dequeueatom", INSTR_DEQUEUEATOM, {InstrVar}},
    {"freeatom", INSTR_FREEATOM, {InstrVar}},
    {"alterfunc", INSTR_ALTERFUNC, {InstrVar, ArgFunctor}},

    {"tailatom", INSTR_TAILATOM, {InstrVar, InstrVar}},
    {"headatom", INSTR_HEADATOM, {InstrVar, InstrVar}},
    {"tailatomlist", INSTR_TAILATOMLIST, {InstrVar, InstrVar}},
    {"atomtailatom", INSTR_ATOMTAILATOM, {InstrVar,InstrVar, InstrVar}},

    {"clearlink", INSTR_CLEARLINK, {InstrVar, InstrVar}},

    {"findatomp", INSTR_FINDATOMP, {InstrVar, InstrVar, ArgFunctor}},
    {"sync", INSTR_SYNC, {}},

    {"allocatom", INSTR_ALLOCATOM, {InstrVar, ArgFunctor}},
    {"allocatomindirect", INSTR_ALLOCATOMINDIRECT, {InstrVar, InstrVar}},
    {"copyatom", INSTR_COPYATOM, {InstrVar, InstrVar, InstrVar}},
    {"addatom", INSTR_ADDATOM, {InstrVar, InstrVar}},

    {"removemem", INSTR_REMOVEMEM, {InstrVar, InstrVar}},
    {"newmem", INSTR_NEWMEM, {InstrVar, InstrVar, InstrVar}},
    {"allocmem", INSTR_ALLOCMEM, {InstrVar}},
    {"newroot", INSTR_NEWROOT, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"movecells", INSTR_MOVECELLS, {InstrVar, InstrVar}},
    {"enqueueallatoms", INSTR_ENQUEUEALLATOMS, {InstrVar}},
    {"freemem", INSTR_FREEMEM, {InstrVar}},
    {"addmem", INSTR_ADDMEM, {InstrVar, InstrVar}},
    {"enqueuemem", INSTR_ENQUEUEMEM, {InstrVar}},
    {"unlockmem", INSTR_UNLOCKMEM, {InstrVar}},
    {"setmemname", INSTR_SETMEMNAME, {InstrVar, String}},

    {"getlink", INSTR_GETLINK, {InstrVar, InstrVar, InstrVar}},
    {"hypergetlink", INSTR_HYPERGETLINK, {InstrVar, InstrVar, InstrVar}},
    {"alloclink", INSTR_ALLOCLINK, {InstrVar, InstrVar, InstrVar}},

    {"newlink", INSTR_NEWLINK, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"relink", INSTR_RELINK, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"swaplink", INSTR_SWAPLINK, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"unify", INSTR_UNIFY, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},
    {"inheritlink", INSTR_INHERITLINK, {InstrVar, InstrVar, InstrVar, InstrVar}},
    {"unifylinks", INSTR_UNIFYLINKS, {InstrVar, InstrVar, InstrVar}},

    {"removeproxies", INSTR_REMOVEPROXIES, {InstrVar}},
    {"removetoplevelproxies", INSTR_REMOVETOPLEVELPROXIES, {InstrVar}},
    {"insertproxies", INSTR_INSERTPROXIES, {InstrVar, InstrVar}},
    {"removetemporaryproxies", INSTR_REMOVETEMPORARYPROXIES, {InstrVar}},

    {"loadruleset", INSTR_LOADRULESET, {InstrVar, ArgRuleset}},
    {"copyrules", INSTR_COPYRULES, {InstrVar, InstrVar}},
    {"clearrules", INSTR_CLEARRULES, {InstrVar}},
    {"loadmodule", INSTR_LOADMODULE, {InstrVar, String}},

    {"recursivelock", INSTR_RECURSIVELOCK, {InstrVar}},
    {"recursiveunlock", INSTR_RECURSIVEUNLOCK, {InstrVar}},
    {"copycells", INSTR_COPYCELLS, {InstrVar, InstrVar, InstrVar}},
    {"dropmem", INSTR_DROPMEM, {InstrVar}},
    {"lookuplink", INSTR_LOOKUPLINK, {InstrVar, InstrVar, InstrVar}},
    {"insertconnectors", INSTR_INSERTCONNECTORS, {InstrVar, InstrVarList, InstrVar}},
    {"insertconnectorsinnull", INSTR_INSERTCONNECTORSINNULL, {InstrVar, InstrVarList}},
    {"deleteconnectors", INSTR_DELETECONNECTORS, {InstrVar, InstrVar}},

    {"react", INSTR_REACT, {InstrVar, InstrVarList, InstrVarList, InstrVarList}},
    {"jump", INSTR_JUMP, {Label, InstrVarList, InstrVarList, InstrVarList}},
#ifdef KWBT_OPT
    {"commit", INSTR_COMMIT, {String, LineNum, InstrVar}},
#else
    {"commit", INSTR_COMMIT, {String, LineNum}},
#endif
    {"resetvars", INSTR_RESETVARS, {InstrVarList, InstrVarList, InstrVarList,}},
    {"changevars", INSTR_CHANGEVARS, {InstrVarList, InstrVarList, InstrVarList,}},
    {"spec", INSTR_SPEC, {InstrVar, InstrVar}},
    {"proceed", INSTR_PROCEED, {}},
    {"stop", INSTR_STOP, {}},

    {"loop", INSTR_LOOP, {InstrList}},
    {"run", INSTR_RUN, {InstrList}},
    {"not", INSTR_NOT, {InstrList}},
    {"inline", INSTR_INLINE, {InstrVar, String, InstrVar}},
    {"callback", INSTR_CALLBACK, {InstrVar, InstrVar}},
    {"unifyhlinks", INSTR_UNIFYHLINKS, {InstrVar, InstrVar}},
    {"findproccxt", INSTR_FINDPROCCXT, {InstrVar, InstrVar, InstrVar, InstrVar, InstrVar, InstrVar}},

    /* special */
    {"group", INSTR_GROUP, {InstrList}},
    {"branch", INSTR_BRANCH, {InstrList}},

    /* guard */
    {"eqground", INSTR_EQGROUND, {InstrVar, InstrVar}},
    {"neqground", INSTR_NEQGROUND, {InstrVar, InstrVar}},
    {"copyhlground", INSTR_COPYHLGROUND, {InstrVar, InstrVar, InstrVar, InstrVarList}},
    {"copyhlgroundindirect", INSTR_COPYHLGROUNDINDIRECT, {InstrVar, InstrVar, InstrVar, InstrVarList}},
    {"copyground", INSTR_COPYGROUND, {InstrVar, InstrVar, InstrVar}},
    {"removeground", INSTR_REMOVEGROUND, {InstrVar, InstrVar}},
    {"removehlground", INSTR_REMOVEHLGROUND, {InstrVar, InstrVar, InstrVarList}},
    {"removehlgroundindirect", INSTR_REMOVEHLGROUNDINDIRECT, {InstrVar, InstrVar, InstrVarList}},
    {"freeground", INSTR_FREEGROUND, {InstrVar}},
    {"freehlground", INSTR_FREEHLGROUND, {InstrVar, InstrVarList}},
    {"freehlgroundindirect", INSTR_FREEHLGROUNDINDIRECT, {InstrVar, InstrVarList}},
    {"isground", INSTR_ISGROUND, {InstrVar, InstrVar, InstrVar}},
    {"ishlground", INSTR_ISHLGROUND, {InstrVar, InstrVar, InstrVar, InstrVarList}},
    {"ishlgroundindirect", INSTR_ISHLGROUNDINDIRECT, {InstrVar, InstrVar, InstrVar, InstrVarList}},
    {"isunary", INSTR_ISUNARY, {InstrVar}},
    {"isint", INSTR_ISINT, {InstrVar}},
    {"isintfunc", INSTR_ISINTFUNC, {InstrVar}},

    {"isfloat", INSTR_ISFLOAT, {InstrVar}},
    {"isstring", INSTR_ISSTRING, {InstrVar}},

    {"uniq", INSTR_UNIQ, {InstrVarList}},

    /* guard: hyperlink */
    {"newhlink", INSTR_NEWHLINK, {InstrVar}},
    {"newhlinkwithattr", INSTR_NEWHLINKWITHATTR, {InstrVar, InstrVar}},
    {"newhlinkwithattrindirect", INSTR_NEWHLINKWITHATTRINDIRECT, {InstrVar, InstrVar}},
    {"makehlink", INSTR_MAKEHLINK, {InstrVar}},
    {"ishlink", INSTR_ISHLINK, {InstrVar}},
    {"getattratom", INSTR_GETATTRATOM, {InstrVar, InstrVar}},
    {"getnum", INSTR_GETNUM, {InstrVar, InstrVar}},

    /* guard: float */
    {"fadd", INSTR_FADD, {InstrVar, InstrVar, InstrVar}},
    {"fsub", INSTR_FSUB, {InstrVar, InstrVar, InstrVar}},
    {"fmul", INSTR_FMUL, {InstrVar, InstrVar, InstrVar}},
    {"fdiv", INSTR_FDIV, {InstrVar, InstrVar, InstrVar}},
    {"fneg", INSTR_FNEG, {InstrVar, InstrVar}},
    {"flt", INSTR_FLT, {InstrVar, InstrVar}},
    {"fle", INSTR_FLE, {InstrVar, InstrVar}},
    {"fgt", INSTR_FGT, {InstrVar, InstrVar}},
    {"fge", INSTR_FGE, {InstrVar, InstrVar}},
    {"feq", INSTR_FEQ, {InstrVar, InstrVar}},
    {"fne", INSTR_FNE, {InstrVar, InstrVar}},

    {"newlist", INSTR_NEWLIST, {InstrVar}},
    {"addtolist", INSTR_ADDTOLIST, {InstrVar, InstrVar}},
    {"getfromlist", INSTR_GETFROMLIST, {InstrVar, InstrVar, InstrVar}},

    /* guard: int */
    {"iadd", INSTR_IADD, {InstrVar, InstrVar, InstrVar}},
    {"isub", INSTR_ISUB, {InstrVar, InstrVar, InstrVar}},
    {"imul", INSTR_IMUL, {InstrVar, InstrVar, InstrVar}},
    {"idiv", INSTR_IDIV, {InstrVar, InstrVar, InstrVar}},
    {"ineg", INSTR_INEG, {InstrVar, InstrVar}},
    {"imod", INSTR_IMOD, {InstrVar, InstrVar, InstrVar}},
    {"ilt", INSTR_ILT, {InstrVar, InstrVar}},
    {"ile", INSTR_ILE, {InstrVar, InstrVar}},
    {"igt", INSTR_IGT, {InstrVar, InstrVar}},
    {"ige", INSTR_IGE, {InstrVar, InstrVar}},
    {"ieq", INSTR_IEQ, {InstrVar, InstrVar}},
    {"ine", INSTR_INE, {InstrVar, InstrVar}},
    {"iand", INSTR_IAND, {InstrVar, InstrVar, InstrVar}},
    {"ior", INSTR_IOR, {InstrVar, InstrVar, InstrVar}},
    {"ixor", INSTR_IXOR, {InstrVar, InstrVar, InstrVar}},

    {"iltfunc", INSTR_ILTFUNC, {InstrVar, InstrVar}},
    {"ilefunc", INSTR_ILEFUNC, {InstrVar, InstrVar}},
    {"igtfunc", INSTR_IGTFUNC, {InstrVar, InstrVar}},
    {"igefunc", INSTR_IGEFUNC, {InstrVar, InstrVar}},

    {"iaddfunc", INSTR_IADDFUNC, {InstrVar, InstrVar, InstrVar}},
    {"isubfunc", INSTR_ISUBFUNC, {InstrVar, InstrVar, InstrVar}},
    {"imulfunc", INSTR_IMULFUNC, {InstrVar, InstrVar, InstrVar}},
    {"idivfunc", INSTR_IDIVFUNC, {InstrVar, InstrVar, InstrVar}},
    {"imodfunc", INSTR_IMODFUNC, {InstrVar, InstrVar, InstrVar}},

    {"getclass", INSTR_GETCLASS, {InstrVar, InstrVar}},
    {"subclass", INSTR_SUBCLASS, {InstrVar, InstrVar}},

    /* etc */
    {"celldump", INSTR_CELLDUMP, {}},

    {0}
  };
