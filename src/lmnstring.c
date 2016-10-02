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
#include "vm/vm.h"
#include "lmnstring.h"
#include "util.h"
#include "dumper.h"
#include <stdio.h>

inline static void string_expand_buf(LmnStringRef s, unsigned long size);

struct LmnString {
  LMN_SP_ATOM_HEADER;

  unsigned long buf_size, len;
  char *buf; /* Cの文字列形式 */
};

#define LMN_STRING_LEN(obj) (LMN_STRING(obj)->len)
#define LMN_STRING_BUF(obj) (LMN_STRING(obj)->buf)
#define LMN_STRING_BUF_SIZE(obj) (LMN_STRING(obj)->buf_size)

#define LINK_STR(MEM, TO_ATOM, TO_ATTR, STR_ATOM)                        \
        lmn_mem_newlink((MEM),                                           \
                        (TO_ATOM),                                       \
                        (TO_ATTR),                                       \
                        LMN_ATTR_GET_VALUE((TO_ATTR)),                   \
                        LMN_ATOM((STR_ATOM)),                            \
                        LMN_SP_ATOM_ATTR, 0)

static int string_atom_type;

BOOL lmn_is_string(LmnAtom atom, LmnLinkAttr attr)
{
  return
    attr == LMN_SP_ATOM_ATTR &&
    LMN_SP_ATOM_TYPE(atom) == string_atom_type;
}

unsigned long lmn_string_hash(LmnStringRef atom) {
  return lmn_byte_hash((unsigned char *)LMN_STRING_BUF(atom), (long)LMN_STRING_LEN(atom));
}

/* 文字列をCの文字列の形式で返す */
const char* lmn_string_c_str(LmnStringRef atom)
{
  return (const char *)LMN_STRING_BUF(atom);
}

LmnStringRef lmn_string_make(const char *s)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  LMN_SP_ATOM_SET_TYPE(p, string_atom_type);
  p->len = strlen(s);
  p->buf_size = p->len + 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  strcpy(p->buf, s);
  return p;
}

LmnStringRef string_make_empty_with_size(unsigned long buf_size)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  LMN_SP_ATOM_SET_TYPE(p, string_atom_type);
  p->len = 0;
  p->buf_size = buf_size > 0 ? buf_size : 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  p->buf[0] = '\0';
  return p;
}

LmnStringRef lmn_string_make_empty()
{
  return string_make_empty_with_size(1);
}

LmnStringRef lmn_string_copy(LmnStringRef s)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  *p = *s;
  p->buf = LMN_NALLOC(char, p->buf_size);
  strcpy(p->buf, s->buf);
  return p;
}

void lmn_string_free(LmnStringRef s)
{
  if (s) {
    if (s->buf) LMN_FREE(s->buf);
    LMN_FREE(s);
  }
}

BOOL lmn_string_eq(LmnStringRef s1, LmnStringRef s2)
{
  return LMN_STRING_LEN(s1) == LMN_STRING_LEN(s2) &&
         !strcmp(LMN_STRING_BUF(s1), LMN_STRING_BUF(s2));
}

inline static void string_expand_buf(LmnStringRef s, unsigned long size)
{
  s->buf_size = size;
  s->buf = LMN_REALLOC(char, s->buf, size);
}

void lmn_string_push_raw_c(LmnStringRef s, int c)
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
void lmn_string_push(LmnStringRef dst, const LmnStringRef src)
{
  lmn_string_push_raw_s(dst, src->buf);
}

/* Cの文字列srcをdstの末尾に追加する。*/
void lmn_string_push_raw_s(LmnStringRef dst, const char *src)
{
  const unsigned long len = dst->len + strlen(src);
  if (len >= dst->buf_size) {
    /* バッファのサイズをどれくらい増加すべきか */
    string_expand_buf(dst, len + 1);
  }
  dst->len = len;
  strcat(dst->buf, src);
}

LmnStringRef lmn_string_concat(LmnStringRef s0, LmnStringRef s1)
{
  LmnStringRef ret_atom = string_make_empty_with_size(s0->len + s1->len + 1);

  lmn_string_push(ret_atom, s0);
  lmn_string_push(ret_atom, s1);

  return ret_atom;
}

int lmn_string_get(LmnStringRef s, int i)
{
  if (i < 0 || i >= LMN_STRING_LEN(s)) return EOF;
  else return s->buf[i];
}

void lmn_string_set_raw_c(LmnStringRef s, int c, int i)
{
  s->buf[i] = c;
}

unsigned long lmn_string_len(LmnStringRef s)
{
  return LMN_STRING_LEN(s);
}

/*----------------------------------------------------------------------
 * Callbacks
 */

void cb_string_make(LmnReactCxtRef rc,
                    LmnMembraneRef mem,
                    LmnAtom a0, LmnLinkAttr t0,
                    LmnAtom a1, LmnLinkAttr t1)
{
  const char *s;
  char buf[64];
  BOOL to_be_freed = FALSE;

  if (LMN_ATTR_IS_DATA(t0)) {
    switch (t0) {
    case LMN_INT_ATTR:
      s = int_to_str(a0);
      to_be_freed = TRUE;
      break;
    case LMN_DBL_ATTR:
      sprintf(buf, "%#g", lmn_get_double(a0));
      s = buf;
      break;
    case LMN_STRING_ATTR:
      s = LMN_STRING_BUF(a0);
      break;
    default:
      fprintf(stderr, "STRING.C: unexpected argument");
      s = "error";
      break;
    }
  } else { /* symbol atom */
    s = LMN_SATOM_STR(a0);
  }

  LINK_STR(mem, a1, t1, lmn_string_make(s));
  if (to_be_freed) LMN_FREE(s);
  lmn_mem_delete_atom(mem, a0, t0);
}

void cb_string_concat(LmnReactCxtRef rc,
                      LmnMembraneRef mem,
                      LmnAtom a0, LmnLinkAttr t0,
                      LmnAtom a1, LmnLinkAttr t1,
                      LmnAtom a2, LmnLinkAttr t2)
{
  LmnStringRef s = lmn_string_concat(LMN_STRING(a0), LMN_STRING(a1));
  LINK_STR(mem, a2, t2, s);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);
}

void cb_string_length(LmnReactCxtRef rc,
                      LmnMembraneRef mem,
                      LmnAtom a0, LmnLinkAttr t0,
                      LmnAtom a1, LmnLinkAttr t1)
{
  int len = LMN_STRING_LEN(a0);

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                  len, LMN_INT_ATTR, 0);

  lmn_mem_delete_atom(mem, a0, t0);
}

void cb_string_reverse(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtom a0, LmnLinkAttr t0,
                       LmnAtom a1, LmnLinkAttr t1)
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

void cb_string_substr(LmnReactCxtRef rc,
                      LmnMembraneRef mem,
                      LmnAtom a0, LmnLinkAttr t0,
                      long begin, LmnLinkAttr t1,
                      long end, LmnLinkAttr t2,
                      LmnAtom a3, LmnLinkAttr t3)
{
  const char *src = (const char *)LMN_STRING_BUF(a0);
  char *s;
  LmnStringRef ret;

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
  LMN_FREE(s);
  lmn_mem_push_atom(mem, LMN_ATOM(ret), LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a3, t3, ret);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, begin, t1);
  lmn_mem_delete_atom(mem, end, t2);
}

void cb_string_substr_right(LmnReactCxtRef rc,
                            LmnMembraneRef mem,
                            LmnAtom a0, LmnLinkAttr t0,
                            long begin, LmnLinkAttr t1,
                            LmnAtom a2, LmnLinkAttr t2)
{
  const char *src = LMN_STRING_BUF(a0);
  char *s;
  int len = strlen(src);
  LmnStringRef ret;

  if (begin < 0) { begin = 0;}
  if (begin > len) begin = len;
  s = LMN_NALLOC(char, len - begin + 1);
  snprintf(s, len - begin + 1, "%s", src+begin);

  ret = lmn_string_make(s);
  LMN_FREE(s);
  lmn_mem_push_atom(mem, LMN_ATOM(ret), LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a2, t2, ret);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, begin, t1);
}


void *sp_cb_string_copy(void *s)
{

  return lmn_string_copy((LmnStringRef)s);
}

void sp_cb_string_free(void *s)
{
  lmn_string_free((LmnStringRef)s);
}

void sp_cb_string_dump(void *s, LmnPortRef port)
{
  port_put_raw_c(port, '"');
  dump_escaped(port, LMN_STRING_BUF(s));
  port_put_raw_c(port, '"');
}

BOOL sp_cb_string_is_ground(void *data)
{
  return TRUE;
}

BOOL sp_cb_string_eq(void *s1, void *s2)
{
  return lmn_string_eq((LmnStringRef)s1, (LmnStringRef)s2);
}

void string_init()
{
  string_atom_type = lmn_sp_atom_register("string",
                                          sp_cb_string_copy,
                                          sp_cb_string_free,
                                          sp_cb_string_eq,
                                          sp_cb_string_dump,
                                          sp_cb_string_is_ground);

  lmn_register_c_fun("string_make", (void *)cb_string_make, 2);
  lmn_register_c_fun("string_concat", (void *)cb_string_concat, 3);
  lmn_register_c_fun("string_length", (void *)cb_string_length, 2);
  lmn_register_c_fun("string_reverse", (void *)cb_string_reverse, 2);
  lmn_register_c_fun("string_substr", (void *)cb_string_substr, 4);
  lmn_register_c_fun("string_substr_right", (void *)cb_string_substr_right, 3);
}

void string_finalize()
{
}
