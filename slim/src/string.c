/*
 * string.c - String implementation
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

#include "lmntal.h"
#include "atom.h"
#include "membrane.h"
#include "ccallback.h"
#include "functor.h"
#include "special_atom.h"
#include "slim_header/string.h"
#include <stdio.h>

inline static void string_expand_buf(LmnString s, unsigned long size);

struct LmnString {
  LMN_SP_ATOM_HEADER;

  unsigned long buf_size, len;
  char *buf; /* Cの文字列形式 */
};

#define LMN_STRING_LEN(obj) (LMN_STRING(obj)->len)
#define LMN_STRING_BUF(obj) (LMN_STRING(obj)->buf)
#define LMN_STRING_BUF_SIZE(obj) (LMN_STRING(obj)->buf_size)


#define LINK_STR(MEM, TO_ATOM, TO_ATTR, STR_ATOM)    \
  lmn_mem_newlink((MEM),                                         \
                    (TO_ATOM), (TO_ATTR), LMN_ATTR_GET_VALUE((TO_ATTR)), \
                  (LmnWord)(STR_ATOM), LMN_SP_ATOM_ATTR, 0)
  


static int string_atom_type;

BOOL lmn_is_string(LmnWord atom, LmnLinkAttr attr)
{
  return
    attr == LMN_SP_ATOM_ATTR &&
    LMN_SP_ATOM_TYPE(atom) == string_atom_type;
}

/* 文字列をCの文字列の形式で返す */
const char* lmn_string_c_str(LmnString atom)
{
  return (const char *)LMN_STRING_BUF(atom);
}

char *int_to_str(int n)
{
  char *s;
  int keta = 0;
  
  if (n == 0) keta = 1;
  else {
    int m = n;
    keta = 0;
    if (m < 0) { m = - m, keta = 1; }
    while (m > 0) {
      m /= 10;
      keta++;
    }
  }

  s = LMN_NALLOC(char, keta + 1);
  sprintf(s, "%d", n);

  return s;
}

LmnString lmn_string_make(char *s)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  LMN_SP_ATOM_SET_TYPE(p, string_atom_type);
  p->len = strlen(s);
  p->buf_size = p->len + 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  strcpy(p->buf, s);
  free(s);
  return p;
}

LmnString string_make_empty_with_size(unsigned long buf_size)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  LMN_SP_ATOM_SET_TYPE(p, string_atom_type);
  p->len = 0;
  p->buf_size = buf_size > 0 ? buf_size : 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  p->buf[0] = '\0';
  return p;
}

LmnString lmn_string_make_empty()
{
  return string_make_empty_with_size(1);
}

LmnString lmn_string_copy(LmnString s)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  *p = *s;
  p->buf = LMN_NALLOC(char, p->buf_size);
  strcpy(p->buf, s->buf);
  return p;
}

void lmn_string_free(LmnString s)
{
  LMN_FREE(s->buf);
  LMN_FREE(s);
}

inline static void string_expand_buf(LmnString s, unsigned long size)
{
  s->buf_size = size;
  s->buf = LMN_REALLOC(char, s->buf, size);
}

void lmn_string_push_raw_c(LmnString s, int c)
{
  if (s->len+1 == s->buf_size) {
    /* バッファのサイズをどれくらい増加すべきか */
    string_expand_buf(s, s->buf_size+1); 
  }
  s->len++;
  s->buf[s->len-1] = c;
  s->buf[s->len] = '\0';
}

/* srcの文字列をdstの末尾に追加する。srcの内容は変わらない */
void lmn_string_push(LmnString dst, const LmnString src)
{
  const unsigned long len = dst->len + src->len;
  if (len >= dst->buf_size) {
    /* バッファのサイズをどれくらい増加すべきか */
    string_expand_buf(dst, len + 1); 
  }
  dst->len = len;
  strcat(dst->buf, src->buf);
}

LmnString lmn_string_concat(LmnString s0, LmnString s1)
{
  LmnString ret_atom = string_make_empty_with_size(s0->len + s1->len + 1);

  lmn_string_push(ret_atom, s0);
  lmn_string_push(ret_atom, s1);

  return ret_atom;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

void cb_string_make(LmnMembrane *mem,
                    LmnWord a0, LmnLinkAttr t0,
                    LmnWord a1, LmnLinkAttr t1)
{
  char *s;
  
  if (LMN_ATTR_IS_DATA(t0)) {
    switch (t0) {
    case LMN_INT_ATTR:
      s = int_to_str(a0);
      break;
    case LMN_DBL_ATTR:
/*       s = double_to_str(a0); */
      s = strdup("not implemented");
      break;
    default:
      fprintf(stderr, "STRING.C: unexpected argument");
      s = strdup("error");
      break;
    }
  } else { /* symbol atom */
    s = strdup(LMN_ATOM_STR(a0));
  }

  LINK_STR(mem, a1, t1, lmn_string_make(s));
  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
}

void cb_string_concat(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      LmnWord a1, LmnLinkAttr t1,
                      LmnWord a2, LmnLinkAttr t2)
{
  LmnString s = lmn_string_concat(LMN_STRING(a0), LMN_STRING(a1));
  LINK_STR(mem, a2, t2, s);

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_free_atom(a1, t1);
}

void cb_string_length(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      LmnWord a1, LmnLinkAttr t1)
{
  int len = LMN_STRING_LEN(a0);

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                  len, LMN_INT_ATTR, 0);

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
}

void cb_string_reverse(LmnMembrane *mem,
                       LmnWord a0, LmnLinkAttr t0,
                       LmnWord a1, LmnLinkAttr t1)
{
  int i, j;
  char *s = LMN_STRING_BUF(a0);
  
  for (i = 0, j = strlen(s)-1; i < j; i++, j--) {
    char t = s[i];
    s[i] = s[j];
    s[j] = t;
  }
  
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                  a0, t0, 0);
}

void cb_string_substr(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      long begin, LmnLinkAttr t1,
                      long end, LmnLinkAttr t2,
                      LmnWord a3, LmnLinkAttr t3)
{
  const char *src = (const char *)LMN_STRING_BUF(a0);
  char *s;
  LmnString ret;

  if (begin <= end) {
    int len = strlen(src);
    if (begin < 0) begin = 0;
    if (end > len) end = len;
    s = LMN_NALLOC(char, end - begin + 1);
    snprintf(s, end - begin + 1, "%s", src+begin);
  } else {
    s = strdup("");
  }

  ret = lmn_string_make(s);
  lmn_mem_push_atom(mem, (LmnWord)ret, LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a3, t3, ret);

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, begin, t1);
  lmn_free_atom(begin, t1);
  lmn_mem_remove_atom(mem, end, t2);
  lmn_free_atom(end, t2);
}

void cb_string_substr_right(LmnMembrane *mem,
                            LmnWord a0, LmnLinkAttr t0,
                            long begin, LmnLinkAttr t1,
                            LmnWord a2, LmnLinkAttr t2)
{
  const char *src = LMN_STRING_BUF(a0);
  char *s;
  int len = strlen(src);
  LmnString ret;

  if (begin < 0) { begin = 0;}
  if (begin > len) begin = len;
  s = LMN_NALLOC(char, len - begin + 1);
  snprintf(s, len - begin + 1, "%s", src+begin);

  ret = lmn_string_make(s);
  lmn_mem_push_atom(mem, (LmnWord)ret, LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a2, t2, ret);

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, begin, t1);
  lmn_free_atom(begin, t1);
}


void *sp_cb_string_copy(void *s)
{
  
  return lmn_string_copy(s);
}

void sp_cb_string_free(void *s)
{
  lmn_string_free(s);
}

void sp_cb_string_dump(void *s, FILE *stream)
{
  fprintf(stream, "\"%s\"", LMN_STRING_BUF(s));
}

BOOL sp_cb_string_is_ground(void *data)
{
  return TRUE;
}

void string_init()
{
  string_atom_type = lmn_sp_atom_register("string",
                                          sp_cb_string_copy,
                                          sp_cb_string_free,
                                          sp_cb_string_dump,
                                          sp_cb_string_is_ground);
  lmn_register_c_fun("string_make", cb_string_make, 2);
  lmn_register_c_fun("string_concat", cb_string_concat, 3);
  lmn_register_c_fun("string_length", cb_string_length, 2);
  lmn_register_c_fun("string_reverse", cb_string_reverse, 2);
  lmn_register_c_fun("string_substr", cb_string_substr, 4);
  lmn_register_c_fun("string_substr_right", cb_string_substr_right, 3);
}

void string_finalize()
{
}
