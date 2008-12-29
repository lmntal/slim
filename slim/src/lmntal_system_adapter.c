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

/* Java処理系によるコンパイル時に用いる最適化オプション */
const char* OPTIMIZE_FLAGS[] = {"-O0",
                                "-O1",
                                "-O2",
                                "-O3"};

/* コンパイラフラグの最大長。バッファあふれの対策 */
#define CFLAGS_MAX_SIZE 1024

void lmntal_build_cmd(char *buf, ...);

void lmntal_build_cmd(char *buf, ...)
{
  va_list argp;

  buf[0] = '\0';
  strcat(buf, getenv(ENV_LMNTAL_HOME));
  strcat(buf, "/bin/lmntal");
  if (getenv(ENV_CFLAGS)) {
    strcat(buf, " ");
    strncat(buf, getenv(ENV_CFLAGS), CFLAGS_MAX_SIZE);
  }

  /* 最適化レベル */
  strcat(buf, " ");
  strcat(buf, OPTIMIZE_FLAGS[lmn_env.optimization_level]);

  va_start(argp, buf);

  while (TRUE) {
    char *p = va_arg(argp, char*);
    if (!p) break;
    strcat(buf, " ");
    strcat(buf, p);
    fflush(stdout);
  }

  va_end(argp);
}

FILE *lmntal_compile_file(char *filename)
{
  char buf[2048];

  lmntal_build_cmd(buf, "--slimcode", filename, 0);
  return popen(buf, "r");
}

FILE *lmntal_compile_rule_str(char *rule_str)
{
  char buf[2048], buf2[2048];

  buf[0] = '\0';
  sprintf(buf2, "\"%s\"", rule_str);
  lmntal_build_cmd(buf, "--slimcode", "--compile-rule", "-e", buf2, 0);
  return popen(buf, "r");
}
