/*
 * special_atom.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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

extern "C" {
#include "special_atom.h"
#include "symbol.h"
}

Vector *sp_atom_callback_tbl;

void sp_atom_init()
{
  sp_atom_callback_tbl = vec_make(64);
}

void sp_atom_finalize()
{
  int i;

  for (i = 0; i < vec_num(sp_atom_callback_tbl); i++) {
    LMN_FREE(vec_get(sp_atom_callback_tbl, i));
  }
  vec_free(sp_atom_callback_tbl);
}

int lmn_sp_atom_register(const char *name,
                         f_copy f_copy,
                         f_free f_free,
                         f_eq f_eq,
                         f_dump f_dump,
                         f_is_ground f_is_ground)
{
  struct SpecialAtomCallback *c = LMN_MALLOC(struct SpecialAtomCallback);
  c->name = lmn_intern(name),
  c->copy = f_copy;
  c->free = f_free;
  c->eq = f_eq;
  c->dump = f_dump;
  c->is_ground = f_is_ground;

  vec_push(sp_atom_callback_tbl, (LmnWord)c);
  return vec_num(sp_atom_callback_tbl) - 1;
}

struct SpecialAtomCallback *sp_atom_get_callback(int id)
{
  LMN_ASSERT(vec_num(sp_atom_callback_tbl) > id);
  return (struct SpecialAtomCallback *)vec_get(sp_atom_callback_tbl, id);
}

