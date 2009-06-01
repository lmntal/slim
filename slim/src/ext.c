/*
 * ext.c - external function
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


/* LMNtalからCの関数を呼び出すことのできる機能を提供する */

#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include "ext.h"
#include "lmntal.h"
#include "rule.h"
#include "symbol.h"
#include "st.h"
#include "ltdl.h"
#include "file_util.h"
#include "arch.h"
#include "ccallback.h"

#define INIT_F_PREFIX "init_"
typedef void (* init_f_type)(void);

void ext_init()
{
  lt_dlinit();
  if (lt_dlerror() != NULL) {
    fprintf(stderr, "EXT.C: %s\n", lt_dlerror());
    exit(1);
  }
}

void ext_finalize()
{
  lt_dlexit();
}

/* dirディレクトリにある共有ライブラリfile_nameを
   ロードし初期化関数を呼び出す */
int load_ext(const char *dir, const char *file_name)
{
  lt_dlhandle h = NULL;
  lt_ptr p = NULL;
  char *path;
  char *base_name;
  char *init_f_name;

  path = build_path(dir, file_name);
  h = lt_dlopen(path);
  free(path);
  if (h == NULL) {
    fprintf(stderr, "EXT.C: %s\n", lt_dlerror());
    printf("dl_open fail\n");
    goto ERROR;
  }


  base_name = basename_ext((char *)file_name);
  init_f_name = malloc(strlen(INIT_F_PREFIX) + strlen(base_name) + 1 + strlen(DL_FILE_TYPE) + 1);
  sprintf(init_f_name, "init_%s", base_name);
  p = lt_dlsym(h, init_f_name);
  free(init_f_name);
  free(base_name);

  if (lt_dlerror() != NULL) {
    fprintf(stderr, "EXT.C: init call(%s) fail, %s\n", file_name, lt_dlerror());
    goto ERROR;
  }
  
  ((init_f_type)p)();

  return TRUE;
  
 ERROR:
  fprintf(stderr, "EXT.C: %s\n", lt_dlerror());
    
  return FALSE;
}

/* pathディレクトリにある共有ライブラリをロードし初期化関数を呼び出す */
void load_ext_files(char *path)
{
  DIR* dir;
  struct dirent* dp;
  struct stat st;
  int path_len;
  int len;

  path_len = strlen(path);
  dir = opendir(path);
  if (dir) {
    while ( (dp = readdir(dir)) != NULL ) {
      char *buf = build_path(path, dp->d_name);
      stat(buf, &st);
      free(buf);
      if (S_ISREG(st.st_mode)) {
        len = strlen(dp->d_name);
        if (!strcmp(dp->d_name + len - strlen(DL_FILE_TYPE), DL_FILE_TYPE)) {
          load_ext(path, dp->d_name);
        }
      }
    }
  }
  closedir(dir);
}
