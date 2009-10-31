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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include "lmntal_system_adapter.h"
#include "lmntal.h"
#include "arch.h"
#include "process_util.h"
#include "vector.h"

/* Java処理系によるコンパイル時に用いる最適化オプション */
const char* OPTIMIZE_FLAGS[] = {"-O0",
                                "-O1",
                                "-O2",
                                "-O3"};

#define LMNTAL_BIN_REL_PATH "/bin/lmntal"
#define OPT_SLIM_CODE "--slimcode"
#define OPT_COMPILE_RULE "--compile-rule"
#define OPT_EVAL "-e"

/* コンパイラフラグの最大長。バッファあふれの対策 */
#define CFLAGS_MAX_SIZE 1024

static void lmntal_build_cmd(char **program, char **ret_args[],  va_list opt_args);
static void add_arg(Vector *args, const char *arg);
static FILE *run_lmntal_system(int dummy, ... );

void add_arg(Vector *args, const char *arg)
{
  vec_push(args, (vec_data_t)LMN_CALLOC(char, strlen(arg) + 1));
  sprintf((char *)vec_get(args, vec_num(args)-1), "%s", arg);
}

/* LMNtal systemを呼ぶためのコマンドと引数を構築する。
   opt_argsはコマンドに渡す引数を指定する。
   programにはコマンドのパス文字列を返す
   ret_argsにはコマンドの引数を返す。ret_argsの最後の要素は0である。
*/
void lmntal_build_cmd(char **program, char **ret_args[], va_list opt_args) 
{
  const char *lmntal_home = getenv(ENV_LMNTAL_HOME);
  Vector *args = vec_make(16);

  *program = LMN_CALLOC(char,
                        strlen(lmntal_home) + strlen(LMNTAL_BIN_REL_PATH) + 1);
  sprintf(*program, "%s%s", lmntal_home, LMNTAL_BIN_REL_PATH);

  add_arg(args, ""); /* 第0引数には何か適当に入れておく */

  if (getenv(ENV_CFLAGS)) {
    add_arg(args, getenv(ENV_CFLAGS));
  }

  /* 最適化レベル */
  add_arg(args, OPTIMIZE_FLAGS[lmn_env.optimization_level]);

  /* opt_argsにある引数を追加 */
  while (TRUE) {
    char *p = va_arg(opt_args, char*);
    if (!p) break;
    add_arg(args, p);
  }
  
  /* 最後の要素は0で終端する */
  vec_push(args, 0);
  { /* vectorの要素をコピー */
    unsigned int i;
    *ret_args = LMN_CALLOC(char*, vec_num(args));
    for (i = 0; i < vec_num(args); i++) {
      (*ret_args)[i] = (char *)vec_get(args, i);
    }
  }

  vec_free(args);
}

/* LMNtalソースコードのファイルを中間言語にコンパイルし結果のストリームを返す*/
FILE *lmntal_compile_file(char *filename)
{
  return run_lmntal_system(0 /*dummy*/, OPT_SLIM_CODE, filename, 0);
}

/* LMNtalのルールを中間言語にコンパイルし結果のストリームを返す*/
FILE *lmntal_compile_rule_str(char *rule_str)
{
  return run_lmntal_system(0, /*dummy*/
                           OPT_SLIM_CODE,
                           OPT_COMPILE_RULE,
                           OPT_EVAL,
                           rule_str,
                           0);
}

/* LMNtal systemを呼び出す。プログラムを呼び出し時に渡す引数を
   可変長引数で受け取る。引数の最後は 0 でなければならない。
   e.g.
     run_lmntal_system(0, "-O3", "--compileonly", 0);
*/
FILE *run_lmntal_system(int dummy, ... )
{
  va_list argp;
  char *program_name;
  char **args;
  FILE *ret;
  unsigned int i;
  
  va_start(argp, dummy);
  lmntal_build_cmd(&program_name, &args, argp);
  ret = run_program(program_name, args);

  /* 解放処理 */
  for (i = 0; args[i]; i++) LMN_FREE(args[i]);
  LMN_FREE(args);
  LMN_FREE(program_name);

  return ret;
}
