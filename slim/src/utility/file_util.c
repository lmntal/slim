/*
 * file_util.c - file utilities
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

#include "file_util.h"
#include <string.h>
#include "arch.h"
#include <stdio.h>
#include <stdlib.h>

char *build_path(const char *dir, const char *component)
{
  const int dir_len = strlen(dir);
  const int comp_len = strlen(component);
  char *buf;

  if (dir_len > 0 && dir[dir_len - 1] == DIR_SEPARATOR_CHAR) {
    buf = malloc(sizeof(char) * dir_len + comp_len + 1);
    sprintf(buf, "%s%s", dir, component);
  } else {
    buf = malloc(sizeof(char) * dir_len + comp_len +
                 strlen(DIR_SEPARATOR_STR) + comp_len + 1);
    sprintf(buf, "%s%s%s", dir, DIR_SEPARATOR_STR, component);
  }

  return buf;
}

char *basename_ext(const char *path)
{
  char *buf = strdup(path);
  int len = strlen(buf);
  int i;

  for (i = len-1; i >= 0; i--) {
    if (buf[i] == '.') {
      buf[i] = '\0';
      break;
    }
  }

  return buf;
}

char *extension(const char *path)
{
  int len = strlen(path);
  int i;
  char *ext;

  for (i = len-1; i >= 0; i--) {
    if (path[i] == '.') {
      break;
    }
  }

  if (i < 0) {
    ext = strdup("");
  } else {
    ext = malloc(sizeof(char) * (len - i));
    sprintf(ext, "%s", path + i + 1);
  }

  return ext;
}
