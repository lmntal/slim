/*
 * ccallback.c
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

#include "ccallback.h"
#include "st.h"
#include "symbol.h"

int free_v(st_data_t key, st_data_t v, st_data_t _t);

st_table_t ccallback_tbl;

void ccallback_init()
{
  ccallback_tbl = st_init_numtable();
}

void ccallback_finalize()
{
  st_foreach(ccallback_tbl, free_v, 0);
  st_free_table(ccallback_tbl);
}

int free_v(st_data_t key, st_data_t v, st_data_t _t)
{
  LMN_FREE(v);
  return ST_CONTINUE;
}

/* コールバックを名前nameで登録する。arityはコールバックに引数として
   渡されるアトムのリンク数 */
void lmn_register_c_fun(const char *name, void *f, int arity)
{
  struct CCallback *c = LMN_MALLOC(struct CCallback);
  c->f = f;
  c->arity = arity;
  st_insert(ccallback_tbl, (st_data_t)lmn_intern(name), (st_data_t)c);
}

/* nameで登録されたコールバック返す */
const struct CCallback *get_ccallback(lmn_interned_str name)
{
  st_data_t t;

  if (st_lookup(ccallback_tbl, name, &t)) {
    return (struct CCallback *)t;
  } else {
    return NULL;
  }
}

