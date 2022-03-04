/*
 * functor.h - functor operations
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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
 * $Id: functor.h,v 1.3 2008/09/29 05:23:40 taisuke Exp $
 */

#ifndef LMN_FUNCTOR_H
#define LMN_FUNCTOR_H

/**
 * @ingroup VM
 * @defgroup Functor
 * @{
 */

#include "element/element.h"
#include "lmntal.h"

/* Functor Information */

struct LmnFunctorEntry {
  BOOL special;
  lmn_interned_str module;
  lmn_interned_str name;
  LmnArity arity;
};

class LmnFunctorTable {
  unsigned int size;
  unsigned int next_id;
  LmnFunctorEntry *entry;
  st_table_t functor_id_tbl; /* ファンクタ構造体からIDへの対応を要素に持つテーブル */

  LmnFunctor
  functor_intern(BOOL special, lmn_interned_str module, lmn_interned_str name, int arity);
  LmnFunctorEntry *lmn_id_to_functor(int functor_id) const;

 public:
  LmnFunctorTable();
  ~LmnFunctorTable();
  static int functor_cmp(LmnFunctorEntry *x, LmnFunctorEntry *y);
  static long functor_hash(LmnFunctorEntry *x);
  void lmn_register_predefined_functor(void);  // not found
  LmnFunctor intern(lmn_interned_str module, lmn_interned_str name, int arity);
  void
  register_functor(int id, BOOL special, lmn_interned_str module, lmn_interned_str name, int arity);
  static int functor_entry_free(LmnFunctorEntry *e);

  LmnFunctorEntry *get_entry(unsigned int f);
  unsigned int get_size();
  unsigned int get_next_id();

#ifdef DEBUG
  void print(void);
  void functor_printer(LmnFunctor f);
#endif
};

#define LMN_FUNCTOR_NAME_ID(T, F)   (T->get_entry(F)->name)
#define LMN_FUNCTOR_ARITY(T, F)     (T->get_entry(F)->arity)
#define LMN_FUNCTOR_MODULE_ID(T, F) (T->get_entry(F)->module)

extern LmnFunctorTable *lmn_functor_table;

#define FUNCTOR_MAX ((1 << (8 * sizeof(LmnFunctor))) - 1)

/* predefined functors */

#define IN_PROXY_NAME    "$in"
#define OUT_PROXY_NAME   "$out"
#define STAR_PROXY_NAME  "$*"
#define UNIFY_ATOM_NAME  "="
#define CONS_ATOM_NAME   "."
#define NIL_ATOM_NAME    "[]"
#define RESUME_ATOM_NAME "$res"
#define IADD_ATOM_NAME   "+"
#define ISUB_ATOM_NAME   "-"
#define IMUL_ATOM_NAME   "*"
#define IDIV_ATOM_NAME   "/"
#define IMOD_ATOM_NAME   "mod"
#define FADD_ATOM_NAME   "+."
#define FSUB_ATOM_NAME   "-."
#define FMUL_ATOM_NAME   "*."
#define FDIV_ATOM_NAME   "/."
#define UNARY_PLUS_NAME  "+"
#define UNARY_MINUS_NAME "-"
#define MEM_EQ_ATOM_NAME "mem_eq"
#define TRUE_ATOM_NAME   "true"
#define FALSE_ATOM_NAME  "false"
#define EXCLAMATION_NAME "!"

#define LMN_IN_PROXY_FUNCTOR        0
#define LMN_OUT_PROXY_FUNCTOR       1
#define LMN_STAR_PROXY_FUNCTOR      2
#define LMN_UNIFY_FUNCTOR           3
#define LMN_LIST_FUNCTOR            4
#define LMN_NIL_FUNCTOR             5
#define LMN_RESUME_FUNCTOR          6
#define LMN_ARITHMETIC_IADD_FUNCTOR 7
#define LMN_ARITHMETIC_ISUB_FUNCTOR 8
#define LMN_ARITHMETIC_IMUL_FUNCTOR 9
#define LMN_ARITHMETIC_IDIV_FUNCTOR 10
#define LMN_ARITHMETIC_MOD_FUNCTOR  11
#define LMN_ARITHMETIC_FADD_FUNCTOR 12
#define LMN_ARITHMETIC_FSUB_FUNCTOR 13
#define LMN_ARITHMETIC_FMUL_FUNCTOR 14
#define LMN_ARITHMETIC_FDIV_FUNCTOR 15
#define LMN_UNARY_PLUS_FUNCTOR      16
#define LMN_UNARY_MINUS_FUNCTOR     17
#define LMN_MEM_EQ_FUNCTOR          18
#define LMN_TRUE_FUNCTOR            19
#define LMN_FALSE_FUNCTOR           20
#define LMN_EXCLAMATION_FUNCTOR     21

#ifdef USE_FIRSTCLASS_RULE
#define COLON_MINUS_ATOM_NAME   ":-"
#define LMN_COLON_MINUS_FUNCTOR 22
#endif

/* @} */

#endif /* LMN_FUNCTOR_H */
