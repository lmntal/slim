/*
 * string.h - String API
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

#ifndef LMN_STRING_H
#define LMN_STRING_H

/* cldoc:begin-category(Lmntal::String) */

#include "../lmntal.h"
typedef struct LmnString *LmnStringRef;

#define LMN_STRING(obj) ((struct LmnString *)(obj))

void string_init(void);
void string_finalize(void);

LmnStringRef lmn_string_make(const char *s);
LmnStringRef lmn_string_make_empty(void);
void lmn_string_free(LmnStringRef s);
BOOL lmn_string_eq(LmnStringRef s1, LmnStringRef s2);
LmnStringRef lmn_string_copy(LmnStringRef s);
BOOL lmn_is_string(LmnAtom atom, LmnLinkAttr attr);
unsigned long lmn_string_hash(LmnStringRef atom);
const char *lmn_string_c_str(LmnStringRef atom);
/* delete last character in <s> */
void lmn_string_pop(LmnStringRef s);
void lmn_string_push(LmnStringRef dst, const LmnStringRef src);
void lmn_string_push_raw_c(LmnStringRef s, int c);
void lmn_string_push_raw_s(LmnStringRef dst, const char *src);
int lmn_string_get(LmnStringRef s, int i);
void lmn_string_set_raw_c(LmnStringRef s, int c, int i);
unsigned long lmn_string_len(LmnStringRef s);

/* cldoc:end-category() */

#endif
