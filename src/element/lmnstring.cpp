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

#include "lmnstring.h"
#include "lmntal.h"
#include "util.h"
#include "vm/vm.h"

#include <algorithm>
#include <stdio.h>

#define LINK_STR(MEM, TO_ATOM, TO_ATTR, STR_ATOM)                              \
  lmn_mem_newlink((MEM), (TO_ATOM), (TO_ATTR), LMN_ATTR_GET_VALUE((TO_ATTR)),  \
                  (STR_ATOM), LMN_SP_ATOM_ATTR, 0)

int LmnString::string_atom_type;

BOOL lmn_is_string(LmnAtomRef atom, LmnLinkAttr attr) {
  return attr == LMN_SP_ATOM_ATTR && LMN_SP_ATOM_TYPE(atom) == LmnString::string_atom_type;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

void cb_string_make(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                    LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1) {
  const char *s;
  char buf[64];
  BOOL to_be_freed = FALSE;

  if (LMN_ATTR_IS_DATA(t0)) {
    switch (t0) {
    case LMN_INT_ATTR:
      s = int_to_str((LmnWord)a0);
      to_be_freed = TRUE;
      break;
    case LMN_DBL_ATTR:
      sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)a0));
      s = buf;
      break;
    case LMN_STRING_ATTR:
      s = reinterpret_cast<LmnString *>(a0)->str.c_str();
      break;
    default:
      fprintf(stderr, "STRING.C: unexpected argument");
      s = "error";
      break;
    }
  } else { /* symbol atom */
    s = LMN_SATOM_STR((LmnSymbolAtomRef)a0);
  }

  LINK_STR(mem, a1, t1, new LmnString(s));
  if (to_be_freed)
    LMN_FREE(s);
  lmn_mem_delete_atom(mem, a0, t0);
}

void cb_string_concat(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                      LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                      LmnAtomRef a2, LmnLinkAttr t2) {
  LmnStringRef s = new LmnString(reinterpret_cast<LmnString *>(a0)->str +
                                 reinterpret_cast<LmnString *>(a1)->str);
  LINK_STR(mem, a2, t2, s);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);
}

void cb_string_length(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                      LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1) {
  LmnWord len = reinterpret_cast<LmnString *>(a0)->str.size();

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), (LmnAtomRef)len,
                  LMN_INT_ATTR, 0);

  lmn_mem_delete_atom(mem, a0, t0);
}

void cb_string_reverse(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                       LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1) {
  int i, j;
  auto &s = reinterpret_cast<LmnString *>(a0)->str;

  for (i = 0, j = s.size() - 1; i < j; i++, j--) {
    char t = s[i];
    s[i] = s[j];
    s[j] = t;
  }

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a0, t0, 0);
}

void cb_string_substr(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
                      LmnLinkAttr t0, LmnAtomRef begin_, LmnLinkAttr t1,
                      LmnAtomRef end_, LmnLinkAttr t2, LmnAtomRef a3,
                      LmnLinkAttr t3) {
  const char *src =
      (const char *)reinterpret_cast<LmnString *>(a0)->str.c_str();
  char *s;
  LmnStringRef ret;
  LmnWord begin = (LmnWord)begin_;
  LmnWord end = (LmnWord)end_;

  if (begin <= end) {
    int len = strlen(src);
    if (end > len)
      end = len;
    s = LMN_NALLOC(char, end - begin + 1);
    snprintf(s, end - begin + 1, "%s", src + begin);
  } else {
    s = strdup("");
  }

  ret = new LmnString(s);
  LMN_FREE(s);
  lmn_mem_push_atom(mem, ret, LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a3, t3, ret);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, begin_, t1);
  lmn_mem_delete_atom(mem, end_, t2);
}

void cb_string_substr_right(LmnReactCxtRef rc, LmnMembraneRef mem,
                            LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef begin_,
                            LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  const char *src = reinterpret_cast<LmnString *>(a0)->str.c_str();
  char *s;
  int len = strlen(src);
  LmnStringRef ret;
  LmnWord begin = (LmnWord)begin_;

  if (begin > len)
    begin = len;
  s = LMN_NALLOC(char, len - begin + 1);
  snprintf(s, len - begin + 1, "%s", src + begin);

  ret = new LmnString(s);
  LMN_FREE(s);
  lmn_mem_push_atom(mem, ret, LMN_SP_ATOM_ATTR);
  LINK_STR(mem, a2, t2, ret);

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, begin_, t1);
}

void *sp_cb_string_copy(void *s) { return new LmnString(*(LmnStringRef)s); }

void sp_cb_string_free(void *s) { delete ((LmnStringRef)s); }

void sp_cb_string_dump(void *s, LmnPortRef port) {
  port_put_raw_c(port, '"');
  dump_escaped(port, reinterpret_cast<LmnString *>(s)->str.c_str());
  port_put_raw_c(port, '"');
}

BOOL sp_cb_string_is_ground(void *data) { return TRUE; }

BOOL sp_cb_string_eq(void *s1, void *s2) {
  return *(LmnStringRef)s1 == *(LmnStringRef)s2;
}

std::vector<uint8_t> sp_cb_string_encode(void *data) {
  std::vector<uint8_t> res(sizeof(lmn_interned_str));
  *(lmn_interned_str *)res.data() = lmn_intern(((LmnString *)data)->c_str());
  std::reverse(res.begin(), res.end()); // little endian
  return res;
}

void *sp_cb_string_decode(const std::vector<uint8_t> &bytes) {
  auto n_id = *(lmn_interned_str *)bytes.data();
  return new LmnString(lmn_id_to_name(n_id));
}

void string_init() {
  LmnString::string_atom_type = lmn_sp_atom_register(
      "string", sp_cb_string_copy, sp_cb_string_free, sp_cb_string_eq,
      sp_cb_string_dump, sp_cb_string_is_ground, sp_cb_string_encode,
      sp_cb_string_decode);

  lmn_register_c_fun("string_make", (void *)cb_string_make, 2);
  lmn_register_c_fun("string_concat", (void *)cb_string_concat, 3);
  lmn_register_c_fun("string_length", (void *)cb_string_length, 2);
  lmn_register_c_fun("string_reverse", (void *)cb_string_reverse, 2);
  lmn_register_c_fun("string_substr", (void *)cb_string_substr, 4);
  lmn_register_c_fun("string_substr_right", (void *)cb_string_substr_right, 3);
}

void string_finalize() {}
