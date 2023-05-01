/*
 * lmntal_system_adapter.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

#include "lmntal_system_adapter.h"
#include "arch.h"
#include "element/element.h"
#include "lmntal.h"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using file_ptr = std::unique_ptr<FILE, decltype(&fclose)>;

/* Java処理系によるコンパイル時に用いる最適化オプション */
char const *OPTIMIZE_FLAGS[] = {"-O0", "-O1", "-O2", "-O3"};

#define LMNTAL_BIN_REL_PATH "/bin/lmntal"
#define OPT_SLIM_CODE "--slimcode"
#define OPT_COMPILE_RULE "--compile-rule"
#define OPT_EVAL "-e"
char const *HYPERLINK_FLAG = "--hl-opt";

/* コンパイラフラグの最大長。バッファあふれの対策 */
#define CFLAGS_MAX_SIZE 1024

static void  lmntal_build_cmd(char **program, char **ret_args[], va_list opt_args);
static void  add_arg(Vector *args, char const *arg);
static FILE *run_lmntal_system(int dummy, ...);

void add_arg(Vector *args, char const *arg) {
  args->push((vec_data_t)LMN_CALLOC<char>(strlen(arg) + 1));
  sprintf((char *)args->get(args->get_num() - 1), "%s", arg);
}

/* LMNtal systemを呼ぶためのコマンドと引数を構築する。
   opt_argsはコマンドに渡す引数を指定する。
   programにはコマンドのパス文字列を返す
   ret_argsにはコマンドの引数を返す。ret_argsの最後の要素は0である。
*/
void lmntal_build_cmd(char **program, char **ret_args[], va_list opt_args) {
  char const *lmntal_home = getenv(ENV_LMNTAL_HOME);
  Vector     *args        = new Vector(16);

  *program                = LMN_CALLOC<char>(strlen(lmntal_home) + strlen(LMNTAL_BIN_REL_PATH) + 1);
  sprintf(*program, "%s%s", lmntal_home, LMNTAL_BIN_REL_PATH);

  add_arg(args, ""); /* 第0引数には何か適当に入れておく */

  if (getenv(ENV_CFLAGS)) {
    add_arg(args, getenv(ENV_CFLAGS));
  }

  if (lmn_env.hyperlink) {
    add_arg(args, HYPERLINK_FLAG);
  }

  /* 最適化レベル */
  add_arg(args, OPTIMIZE_FLAGS[lmn_env.optimization_level]);

  /* opt_argsにある引数を追加 */
  while (TRUE) {
    char *p = va_arg(opt_args, char *);
    if (!p)
      break;
    add_arg(args, p);
  }

  /* 最後の要素は0で終端する */
  args->push(0);
  { /* vectorの要素をコピー */
    unsigned int i;
    *ret_args = LMN_CALLOC<char *>(args->get_num());
    for (i = 0; i < args->get_num(); i++) {
      (*ret_args)[i] = (char *)args->get(i);
    }
  }

  delete args;
}

/* LMNtalソースコードのファイルを中間言語にコンパイルし結果のストリームを返す*/
file_ptr lmntal_compile_file(char const *filename) {
  return file_ptr(run_lmntal_system(0 /*dummy*/, OPT_SLIM_CODE, filename, 0), fclose);
}

/* LMNtalのルールを中間言語にコンパイルし結果のストリームを返す*/
file_ptr lmntal_compile_rule_str(char const *rule_str) {
  return file_ptr(run_lmntal_system(0, /*dummy*/
                                    OPT_SLIM_CODE, OPT_COMPILE_RULE, OPT_EVAL, rule_str, 0),
                  fclose);
}

/* LMNtal systemを呼び出す。プログラムを呼び出し時に渡す引数を
   可変長引数で受け取る。引数の最後は 0 でなければならない。
   e.g.
     run_lmntal_system(0, "-O3", "--compileonly", 0);
*/
FILE *run_lmntal_system(int dummy, ...) {
  va_list      argp;
  char        *program_name;
  char       **args;
  FILE        *ret;
  unsigned int i;

  va_start(argp, dummy);
  lmntal_build_cmd(&program_name, &args, argp);
  ret = run_program(program_name, args);

  /* 解放処理 */
  for (i = 0; args[i]; i++)
    LMN_FREE(args[i]);
  LMN_FREE(args);
  LMN_FREE(program_name);

  return ret;
}
