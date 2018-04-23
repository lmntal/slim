/*
 * load.cpp - Load Intermediate Language
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

#include <map>

extern "C" {
#include "load.h"
#include "lmntal.h"
#include "arch.h"
#include "vm/vm.h"
#include "ffi/lmntal_system_adapter.h"
#include "element/element.h"
#include "so.h"
#include <dirent.h>
#include <limits.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>

/* prototypes */

void build_cmd(char *buf, char *file_name);
FILE *compile(char *filename);
}

#include "syntax.hpp"
#include "il_lexer.hpp"
#include "il_parser.hpp"
#include "byte_encoder.hpp"

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



LmnRuleRef load_rule(Rule *rule)
{
  ByteEncoder encoder;

/*   load_inst_block(rule->amatch, encoder); */
  encoder.load(rule->mmatch);
  encoder.load(rule->guard);
  encoder.load(rule->body);

  /* ラベルを参照している位置に、実際のラベルの位置を書き込む */
  encoder.resolve_labels();

  auto runtime_rule = encoder.create_rule();
  if (rule->hasuniq) lmn_rule_init_uniq_rule(runtime_rule);
  
  return runtime_rule;
}

static LmnRuleSetRef load_ruleset(std::shared_ptr<RuleSet> rs)
{
  auto runtime_ruleset = lmn_ruleset_make(rs->id, 10);
  
  for (auto &r : rs->rules)
    lmn_ruleset_put(runtime_ruleset, load_rule(r.get()));

  lmn_set_ruleset(runtime_ruleset, rs->id);

  if (rs->is_system_ruleset) {
    /* 各ルールをシステムルールセットに追加する */
    for (int i = 0; i < lmn_ruleset_rule_num(runtime_ruleset); i++) {
      LmnRuleRef rule2 = lmn_rule_copy(lmn_ruleset_get_rule(runtime_ruleset, i));
      lmn_add_system_rule(rule2);
    }
  }

  return runtime_ruleset;
}

/* 最初のルールセットを返す */
static LmnRuleSetRef load_il(ILRef il)
{
  LmnRuleSetRef t, first_ruleset;

  /* load rules */
  auto &rulesets = il->rulesets;
  first_ruleset = NULL;
  for (int i = 0; i < rulesets.size(); i++) {
    t = load_ruleset(rulesets.at(i));
    if (i == 0) first_ruleset = t;
  }

  if (!first_ruleset) {
    lmn_fatal("implementation error: no ruleset in il");
  }


  /* load module list */
  for (auto &m : il->modules) {
    lmn_set_module(m->name_id, lmn_ruleset_from_id(m->ruleset_id));
  }

  return first_ruleset;
}

/* soから試しに呼び出す関数 */
void helloworld(const char *s)
{
  fprintf(stdout, "hello %s world!\n", s);
}

LmnRuleSetRef load_and_setting_trans_maindata(struct trans_maindata *maindata)
{
  int i;
  struct trans_ruleset ruleset;
  LmnRuleSetRef ret = 0; /* ワーニング抑制 */

  /* シンボルを読み込み+変換テーブルを設定 */
  for (i = 1; i < maindata->count_of_symbol; i++) {
    lmn_interned_str gid = lmn_intern(maindata->symbol_table[i]);
    maindata->symbol_exchange[i] = gid;
  }

  /* ファンクタを読み込み+変換テーブルを設定 */
  for (i = 0; i < maindata->count_of_functor; i++) {
    LmnFunctorEntry ent = maindata->functor_table[i];
    /* スペシャルファンクタは登録できないが,
     * functor.c内で登録される共通部分以外出現しようがないはずなので問題ない */
    if (ent.special) {
      /* 登録しないで変換も必要無し */
      maindata->functor_exchange[i] = i;
    } else {
      /* シンボルは変換を忘れないように */
      LmnFunctor gid = lmn_functor_intern(maindata->symbol_exchange[ent.module],
                                          maindata->symbol_exchange[ent.name],
                                          ent.arity);
      maindata->functor_exchange[i] = gid;
    }
  }

  /* ルールセット0番は数合わせ */
  /* システムルールセット読み込み */
  ruleset = maindata->ruleset_table[1];
  for (i = 0; i < ruleset.size; i++) {
    LmnRuleRef r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_system_rule(r);
  }

  /* ルールセット2番はinitial ruleset */
  ruleset = maindata->ruleset_table[2];
  for (i = 0; i < ruleset.size; i++) {
    LmnRuleRef r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_initial_rule(r);
  }

  /* ルールセット3番はinitial system ruleset */
  ruleset = maindata->ruleset_table[3];
  for(i = 0; i < ruleset.size; i++) {
    LmnRuleRef r = lmn_rule_make_translated(ruleset.rules[i].function,
                                         maindata->symbol_exchange[ruleset.rules[i].name]);
    lmn_add_initial_system_rule(r);
  }

  /* ルールセットを読み込み+変換テーブルを設定 */
  for (i = FIRST_ID_OF_NORMAL_RULESET; i < maindata->count_of_ruleset; i++) {
    int j, gid;
    LmnRuleSetRef rs;
    struct trans_ruleset tr;

    tr  = maindata->ruleset_table[i];
    gid = lmn_gen_ruleset_id();
    rs  = lmn_ruleset_make(gid, tr.size);
    lmn_set_ruleset(rs, gid);

    for (j = 0; j < tr.size; j++) {
      LmnRuleRef r = lmn_rule_make_translated(tr.rules[j].function,
                                           maindata->symbol_exchange[tr.rules[j].name]);
      lmn_ruleset_put(rs, r);
    }

    /* とりあえず最初の通常ルールセットを初期データ生成ルールと決め打ちしておく */
    if (i == FIRST_ID_OF_NORMAL_RULESET) {
      ret = rs;
    }
    maindata->ruleset_exchange[i] = gid;
  }

  /* モジュール読込み */
  for (i = 0; i < maindata->count_of_module; i++) {
    struct trans_module mo = maindata->module_table[i];
    lmn_set_module(maindata->symbol_exchange[mo.name],
                   lmn_ruleset_from_id(maindata->ruleset_exchange[mo.ruleset]));
  }

  return ret;
}



static inline LmnRuleSetRef load_compiled_il_inner(char *basename,
                                                char *buf, int buf_len,
                                                void *sohandle,
                                                char *filename);

/* soハンドルから中間命令を読み出す. load_extは開始ルールを認識しない.
 * 複数ファイルを1つのsoにした場合, 初期データ生成ルールが複数あるはずだがとりあえず無視.
 * TODO:
 *   初期データ生成ルールセットのルールを1つのルールセットにまとめて出力すれば問題無し.
 *   1回適用成功したところで止めなければok     */
LmnRuleSetRef load_compiled_il(char *filename, void *sohandle)
{
  char *basename, *buf;
  int buf_len;
  LmnRuleSetRef ret;

  basename = create_formatted_basename(filename);
  buf_len  = strlen(basename) + 50;   /* 適当に50文字余分にとったけどこれでいいのか  */
  buf      = (char *)lmn_malloc(buf_len + 1); /* 必要ないけど一応最後に1byte余分をとっておく */

  ret      = load_compiled_il_inner(basename, buf, buf_len, sohandle, filename);

  lmn_free(buf);
  lmn_free(basename);

  return ret;
}

static inline LmnRuleSetRef load_compiled_il_inner(char *basename,
                                                char *buf, int buf_len,
                                                void *sohandle,
                                                char *filename)
{
  void (*init_f)();

  snprintf(buf, buf_len, "init_%s", basename);
  init_f = (void (*)())dlsym(sohandle, buf);

  if (!init_f) {
    fprintf(stderr, "init function \"%s\" not found in %s.\n", buf, filename);
    return NULL;
  }
  else {
    struct trans_maindata *maindata;

    /* 初期化関数を呼び出し */
    (*init_f)();

    /* データオブジェクトを取得 */
    snprintf(buf, buf_len, "trans_%s_maindata", basename);
    maindata = (struct trans_maindata *)dlsym(sohandle, buf);

    if (!maindata) {
      fprintf(stderr, "maindata \"%s\" not found in %s.\n", buf, basename);
      return NULL;
    }
    else {
      /* 読み込みと変換テーブルの設定 */
      return load_and_setting_trans_maindata(maindata);
    }
  }
}

/* ファイルから中間言語を読み込みランタイム中に配置する。
 * 最初のルールセットを返す */
LmnRuleSetRef load(FILE *in)
{
  ILRef il;
  LmnRuleSetRef first_ruleset;

  if (il_parse(in, &il)) {
    /* 構文解析に失敗 */
    exit(EXIT_FAILURE);
  }

  first_ruleset = load_il(il);
  delete il;
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
  for (i = 0; i<vec_num(opened_so_files); i++) {
    dlclose((void*)vec_get(opened_so_files, i));
  }
  vec_free(opened_so_files);
}

/* ファイルから中間言語を読み込みランタイム中に配置し、最初のルールセットを返す。
 * ファイルの拡張子が lmn の場合、Javaによる処理系でファイルをコンパイルし、
 * 中間言語を生成する。soの場合、dlopenしておきハンドラはopened_so_filesで管理。
 * dlcloseはfinalize()でされる */
LmnRuleSetRef load_file(char *file_name)
{
  FILE *fp;
  int len;
  LmnRuleSetRef rs;
  void *sohandle;

  len = strlen(file_name);

  /* 拡張子がsoならリンクする */
  /* dlopenは環境変数にLD_LIBRARY_PATH="."と設定しないとカレントディレクトリを検索してくれないので注意 */
  if (!strcmp(file_name + len -3, ".so")) {
    sohandle = dlopen(file_name, RTLD_LAZY);
    if (!sohandle) {
      fprintf(stderr, "Failed to open %s\n", file_name);
      fprintf(stderr, "dlopen: %s\n", dlerror());
      rs = 0;
    } else {
      dlerror();
      vec_push(opened_so_files, (vec_data_t)sohandle);
      rs = load_compiled_il(file_name, sohandle);
    }
  } else if ((fp = fopen(file_name, "r"))) {
    /* 拡張子がlmnならばJavaによる処理系で中間言語にコンパイルする */
    const char *lmntal_home = getenv(ENV_LMNTAL_HOME);
    if (!strcmp(&file_name[len-4], ".lmn")) {
      if (lmntal_home) {
        FILE *fp_compiled;
        struct stat st;

        if (stat(lmntal_home, &st) == 0) {
          fp_compiled = lmntal_compile_file(file_name);
        }
        else {
          fprintf(stderr, "Failed to run lmntal compiler\n");
          fprintf(stderr, "lmntal don't exist at %s\n", lmntal_home);
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
  const char *extension;
  char *buf;

  extension = extension_table[filetype];
  buf       = LMN_NALLOC(char, strlen((char *)path) + NAME_MAX + 2);

  sprintf(buf, "%s%s%s.%s", (char *)path, DIR_SEPARATOR_STR,
                            (char *)basename, extension);
  load_file(buf);

  LMN_FREE(buf);
  return ST_CONTINUE;
}

int free_loading_tbl_entry(st_data_t basename, st_data_t filetype, void *path)
{
  LMN_FREE(basename);
  return ST_CONTINUE;
}

/* pathのディレクトリ内のファイルを中間コードとしてロードする.
 * 拡張子を除いてファイル名が同一な場合はextension_tableで指定した優先順で1種類のみ読み込む */
void load_il_files(const char *path)
{
  int path_len;
  char *buf;
  DIR *dir;

  path_len = strlen(path);
  buf      = LMN_NALLOC(char, path_len + NAME_MAX + 2);
  dir      = opendir(path);

  if (dir) {
    st_table_t loading_files_type;
    struct stat st;
    struct dirent* dp;

    loading_files_type = st_init_strtable();

    /* 読み込むファイルをリストアップする */
    while ((dp = readdir(dir))) {
      sprintf(buf, "%s%s%s", path, DIR_SEPARATOR_STR, dp->d_name);
      stat(buf, &st);
      if (S_ISREG(st.st_mode)) {
        char *ext;
        int filetype;

        ext = extension(dp->d_name);
        filetype = file_type(ext);

        if (filetype > 0) { /* 読み込むべき拡張子なら追加 */
          char *basename;
          st_data_t old_filetype;

          basename = basename_ext(dp->d_name);
          if (st_lookup(loading_files_type, (st_data_t)basename, &old_filetype)) {
            if (filetype > old_filetype) {
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
    st_foreach(loading_files_type, (st_iter_func)load_loading_tbl_entry, (st_data_t)path);
    /* 開放 */
    st_foreach(loading_files_type, (st_iter_func)free_loading_tbl_entry, (st_data_t)path);
    st_free_table(loading_files_type);

    closedir(dir);
  }

  LMN_FREE(buf);
}

/* ファイルが*.lmnならコンパイル結果のFILE*を返す.
 * ファイルが*.ilならfopen結果のFILE*を返す.
 * それ以外の拡張子だったり存在しないファイルだったらNULLを返す.   */
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

int ilparse(il::lexer* scanner, ILRef *il, RuleRef *rule);

/* inから中間言語を読み込み、構文木を作る。構文木はilに設定される。
   正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
int il_parse(FILE *in, ILRef *il)
{
  il::lexer scanner(in);
  return ilparse(&scanner, il, NULL);
}

int il_parse_rule(FILE *in, RuleRef *rule)
{
  il::lexer scanner(in);
  return ilparse(&scanner, NULL, rule);
}

char *create_formatted_basename(const char *filepath)
{
  const char *begin, *end, *p;
  char *basename;
  int i;

  begin = strrchr(filepath, DIR_SEPARATOR_CHAR); /* パス内最後の/を探す */

  if (!begin) { /* もし/があればその次がファイル名の先頭 */
    begin += 1;
  } else {
    begin = (char *)filepath; /* もし/がなければ全体の先頭がファイル名の先頭 */
  }

  end = strchr(begin, '.'); /* ファイル名最初の.を探す ないと困る */
  basename = (char *)lmn_malloc(end - begin + 1);
  for (i = 0, p = begin; i < end - begin; i++, p++){
    if (isalpha((unsigned char)*p) || isdigit((unsigned char)*p)) {
      basename[i] = *p;
    } else {
      basename[i] = 'O'; /* 記号は全部Oにする */
    }
  }

  basename[end - begin] = '\0';

  return basename;
}

