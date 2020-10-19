/*
 * io.c
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
#include "vm/vm.h"
#include "element/element.h"

// void init_print(void);

/* 改行を出力する
 * +a0     : ポート
 * -a1     : 返すポート
 */
void cb_print_newline_with_port(LmnReactCxtRef rc,
                                LmnMembraneRef mem,
                                LmnAtomRef a0, LmnLinkAttr t0,
                                LmnAtomRef a1, LmnLinkAttr t1)
{
  port_put_raw_s(LMN_PORT(a0), "\n");

  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t1),
                  a0, t0, 0);
}

/* 文字列a1を出力し、次に改行を出力する
 * +a0     : ポート
 * +a1     : 文字列
 * -a2     : 返すポート
 */
void cb_print_line_with_port(LmnReactCxtRef rc,
                             LmnMembraneRef mem,
                             LmnAtomRef a0, LmnLinkAttr t0,
                             LmnAtomRef a1, LmnLinkAttr t1,
                             LmnAtomRef a2, LmnLinkAttr t2)
{
  port_puts(LMN_PORT(a0), LMN_STRING(a1));
  port_put_raw_s(LMN_PORT(a0), "\n");
  fflush(NULL);

  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
  lmn_mem_delete_atom(mem, a1, t1);
}

/* Use cb_port_read_line instead. */
/* ポートa0から一行読み込む
 * +a0     : ポート
 * -a1     : 返すポート
 * -a2     : 読み込んだ文字列 | eof()
 */
// void cb_input_line_with_port(LmnReactCxtRef rc,
//                              LmnMembraneRef mem,
//                              LmnAtomRef a0, LmnLinkAttr t0,
//                              LmnAtomRef a1, LmnLinkAttr t1,
//                              LmnAtomRef a2, LmnLinkAttr t2)
// {
//   const int N = 256;
//   char buf[N], *s=NULL, *p; /* sは行の文字列の先頭、pは作業用 */
//   int size;
// 
//   size =  0;
//   p = s;
//   while (fgets(buf, N, stdin)) {
//     int len = strlen(buf);
//     if (s == NULL) {
//       s = p = LMN_NALLOC(char, len+1);
//       s[0] = '\0';
//       size = len + 1;
//     } else {
//       s = LMN_REALLOC(char, s, size += len + 1);
//     }
//     strcat(p, buf);
//     p += len;
//     if (len < N-1) break;
//   }
// 
//   if (ferror(stdin)) {/* error */
//    LmnSAtom atom = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern("error"), 1));
//     lmn_mem_newlink(mem,
//                     a0, t0, LMN_ATTR_GET_VALUE(t0),
//                     LMN_ATOM(atom), LMN_ATTR_MAKE_LINK(0), 0);
//     mem_push_symbol_atom(mem, atom);
//     if (s) LMN_FREE(s);
//   }
//   else if (feof(stdin) && s == NULL) { /* eof */
//     LmnSAtom atom = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern("eof"), 1));
//     lmn_mem_newlink(mem,
//                     a0, t0, LMN_ATTR_GET_VALUE(t0),
//                     LMN_ATOM(atom), LMN_ATTR_MAKE_LINK(0), 0);
//     mem_push_symbol_atom(mem, atom);
//   }
//   else {
//     LmnString a;
// 
//     if (*(p-2) == '\n' || *(p-2)=='\r') p -= 2;
//     else if (*(p-1) == '\n' || *(p-1)=='\r') p -= 1;
//     *p = '\0';
// 
//     a = lmn_string_make(s);
//     LMN_FREE(s);
//     lmn_mem_push_atom(mem, LMN_ATOM(a), LMN_STRING_ATTR);
// 
//     lmn_mem_newlink(mem,
//                     a0, t0, LMN_ATTR_GET_VALUE(t0),
//                     LMN_ATOM(a), LMN_STRING_ATTR, 0);
//   }
// }

void init_io(void)
{
  lmn_register_c_fun("cb_print_newline_with_port", (void *)cb_print_newline_with_port, 2);
  lmn_register_c_fun("cb_print_line_with_port", (void *)cb_print_line_with_port, 3);
}
