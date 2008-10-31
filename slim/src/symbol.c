/*
 * symbol.c - mapping symbol names to their id
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lang@ueda.info.waseda.ac.jp>
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
 * $Id: symbol.c,v 1.4 2008/09/29 05:23:40 taisuke Exp $
 */

#include "symbol.h"
#include <stdarg.h>
#include "st.h"

static struct st_table *sym_tbl;
static struct st_table *sym_rev_tbl;
static lmn_interned_str next_sym_id;

/* prototypes */

void sym_tbl_init(void);
int free_sym_tbl_entry(st_data_t name, st_data_t _v, int _i);
void sym_tbl_destroy(void);
lmn_interned_str create_new_id(void);

void sym_tbl_init()
{
  sym_tbl = st_init_strtable();
  sym_rev_tbl = st_init_numtable();
  next_sym_id = 1; /* 0はIDに使わない */
}

int free_sym_tbl_entry(st_data_t name, st_data_t _v, int _i)
{
  LMN_FREE(name);
  return ST_DELETE;
}

void sym_tbl_destroy()
{
  /* テーブル中の文字列の領域を解放 */
  st_foreach(sym_tbl, free_sym_tbl_entry, 0);

  st_free_table(sym_tbl);
  st_free_table(sym_rev_tbl);
}


lmn_interned_str create_new_id()
{
  return next_sym_id++;
}

lmn_interned_str lmn_intern(char *name)
{
  lmn_interned_str new_id;

  /* すでにnameに対応する値があるならそれを返す */
  if (st_lookup(sym_tbl, name, (st_data_t *)&new_id)) return new_id;

  /* 新しいIDを作る */
  new_id = create_new_id();
  name = strdup(name);
  st_add_direct(sym_tbl, name, (st_data_t)new_id);
  st_add_direct(sym_rev_tbl, (st_data_t)new_id, (st_data_t)name);
  return new_id;
}

const char *lmn_id_to_name(lmn_interned_str id)
{
  char *name;

  if (id == ANONYMOUS) return "";
  else if (st_lookup(sym_rev_tbl, (st_data_t)id, (st_data_t *)&name)) return name;
  else return NULL;
}

