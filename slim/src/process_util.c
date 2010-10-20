/*
 * process_util.c - process utility
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

/* 外部プログラムの起動など、プロセス関連の便利関数の定義 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include "process_util.h"


/* program_pathにあるプログラムを引数argsで起動し、プログラムの出力の
   ストリームを返す。
*/
FILE *run_program(const char *program_path, char **args)
{
   pid_t pid;
   int pipes[2];

   if (pipe(pipes)) { /* fail : -1 */
     perror("pipe failed");
     exit(EXIT_FAILURE);
   }

   pid = fork();

   switch (pid) {
   case 0: /* 子プロセス */
   {
     int old; /* 警告されるので */
     close(fileno(stdout));
     old = dup(pipes[1]);
     close(pipes[0]);
     close(pipes[1]);

     if (execv(program_path, args) == -1) {
       perror("execv failed");
       exit(EXIT_FAILURE);
     }
   }
   case -1: /* fork失敗 */
     perror("fork failed");
     exit(EXIT_FAILURE);
   default: /* 親プロセス */
   {
     int status;

     close(pipes[1]);
     /* パイプのバッファを越える出力が起きると読み込むまでブロックされるためデッドロック
      */
     if (waitpid(pid, &status, 0) == -1) {
       perror("waitpid failed");
       exit(EXIT_FAILURE);
     }

     return fdopen(pipes[0], "r");
   }
   }
}
