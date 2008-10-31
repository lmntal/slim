/*
 * atom.h
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
 * $Id: atom.h,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_ATOM_H
#define LMN_ATOM_H

#include "lmntal.h"

/*
 * Atom Structure
 *
 *  * Atom
 *      first Word                        : pointer to previous atom
 *      second Word                       : pointer to next atom
 *      next 2 Bytes                      : Functor
 *      next N Bytes                      : Link Attribute
 *      N Word from next aligned position : Link
 *    (N is the arity of atom)
 *
 *  * Link Attribute
 *     If high 1 bit is 1 then other bits represent 'Link Number'
 *     else other bits represent 'Proxy' or 'Premitive data types'.
 *
 *     [Link Number]
 *       0-------
 *     [int]
 *       10000000
 *     [double]
 *       10000001
 *
 *       We are going to support some primitive data types.
 *
 *       (signed/unsigned) int, short int, long int, byte, long long int,
 *       float, double, long double,
 *       bool, string, character,
 *       ground array, ground with membrane array, primitive arrays
 *
 *       But, incompletely-specified.
 *
 */

/* Proxy
 *  Proxy is implememted as a special atom. The 3rd link is a pointer
 *  to the current membrane.
 */
/* プロキシの3番目の引数番号の領域を remove_proxy, insert_proxy
   で利用中 */

typedef LmnWord *LmnAtomPtr;
typedef uint8_t LmnLinkAttr;

#define LMN_ATOM_ATTR(X)   ((LmnLinkAttr)(X))
#define LMN_ATTR_BYTES     (sizeof(LmnLinkAttr))
#define LMN_ATTR_MASK      (0x7fU)
#define LMN_ATTR_FLAG      (0x80U)

/* リンク番号のタグのワード数。ファンクタと同じワードにある分は
   数えない */
#define LMN_ATTR_WORDS(ARITY)  \
  (((ARITY)+(LMN_FUNCTOR_BYTES - 1))>>LMN_WORD_SHIFT)

#define LMN_ATOM(X)                 ((LmnAtomPtr)(X))

#define LMN_ATOM_PPREV(ATOM)        (((LmnWord*)(ATOM)))
#define LMN_ATOM_PNEXT(ATOM)        (((LmnWord*)(ATOM))+1)
#define LMN_ATOM_PATTR(ATOM,N)                        \
  ((LmnLinkAttr*)(((BYTE*)(((LmnWord*)(ATOM))+2))+    \
              LMN_FUNCTOR_BYTES+(N)*LMN_ATTR_BYTES))
#define LMN_ATOM_PLINK(ATOM,N)                            \
  (((LmnWord*)(ATOM))+3+LMN_ATTR_WORDS(LMN_ATOM_GET_ARITY(ATOM))+(N))

/* get/set prev atom of ATOM */
#define LMN_ATOM_GET_PREV(ATOM)           \
  ((LmnAtomPtr)*LMN_ATOM_PPREV(ATOM))
#define LMN_ATOM_SET_PREV(ATOM,X)         \
  (*LMN_ATOM_PPREV(ATOM)=(LmnWord)(X))
/* get/set next atom of ATOM */
#define LMN_ATOM_GET_NEXT(ATOM)           \
  ((LmnAtomPtr)*LMN_ATOM_PNEXT(ATOM))
#define LMN_ATOM_SET_NEXT(ATOM,X)         \
  (*LMN_ATOM_PNEXT(ATOM)=(LmnWord)(X))
/* get/set ATOM functor */
#define LMN_ATOM_GET_FUNCTOR(ATOM)        \
  LMN_FUNCTOR(*(((LmnWord*)(ATOM))+2))
#define LMN_ATOM_SET_FUNCTOR(ATOM,X)      \
  (*(LmnFunctor*)((LmnWord*)(ATOM)+2)=(X))
#define LMN_ATOM_GET_ARITY(ATOM)          (LMN_FUNCTOR_ARITY(LMN_ATOM_GET_FUNCTOR(ATOM)))
/* アトムのリンクの数（プロキシは第三引数は所属膜） */
#define LMN_FUNCTOR_GET_LINK_NUM(F)   \
  ((LMN_FUNCTOR_ARITY(F)) -  \
   (LMN_IS_PROXY_FUNCTOR(F) ? 1U : 0U))

/* get/set N th link attribute of  ATOM */
#define LMN_ATOM_GET_ATTR(ATOM,N)    \
  (*LMN_ATOM_PATTR(ATOM,N))
#define LMN_ATOM_SET_ATTR(ATOM,N,X)  \
  ((*LMN_ATOM_PATTR(ATOM,N))=(X))
/* get/set N th link of ATOM */ 
#define LMN_ATOM_GET_LINK(ATOM, N)        \
  (*LMN_ATOM_PLINK(ATOM,N))
#define LMN_ATOM_SET_LINK(ATOM,N,X)       \
  (*LMN_ATOM_PLINK(ATOM,N)=(X))

/* word size of atom */
/* 3の加算は prev,next,functorのワード */
#define LMN_ATOM_WORDS(ARITY)          \
  (3+LMN_ATTR_WORDS(ARITY)+(ARITY))

/* operations for link attribute */
#define LMN_ATTR_IS_DATA(X)           ((X)&~LMN_ATTR_MASK)

/* make data/link link attribute from value */
#define LMN_ATTR_MAKE_DATA(X)         (0x80U|(X))
#define LMN_ATTR_MAKE_LINK(X)         (X)
/* get link attribute value (remove tag) */
#define LMN_ATTR_GET_VALUE(X)         ((X)&LMN_ATTR_MASK)
/* set link attribute value. Tag is not changed. */
#define LMN_ATTR_SET_VALUE(PATTR,X)   \
  (*(PATTR)=((((X)&~LMN_ATTR_MASK))|X))

/* get/set membrane of proxy */ 
#define LMN_PROXY_GET_MEM(PROXY_ATOM)  \
  ((LmnMembrane *)LMN_ATOM_GET_LINK((PROXY_ATOM), 2))
#define LMN_PROXY_SET_MEM(PROXY_ATOM,X)  LMN_ATOM_SET_LINK((PROXY_ATOM), 2, (X))
#define LMN_IS_PROXY_FUNCTOR(FUNC)   ((FUNC) < 3)

/* DEBUG: print atom name (for debug) */
#define LMN_ATOM_STR(ATOM) LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(ATOM)))

/* link attribute of primitive data type */
/* low 7 bits of link attribute */

#define LMN_INT_ATTR        (LMN_ATTR_FLAG | 0)
#define LMN_DBL_ATTR        (LMN_ATTR_FLAG | 1)

/* special functors */

#define LMN_IN_PROXY_FUNCTOR   0
#define LMN_OUT_PROXY_FUNCTOR  1
#define LMN_STAR_PROXY_FUNCTOR 2
#define LMN_UNIFY_FUNCTOR      3
#define LMN_LIST_FUNCTOR       4
#define LMN_NIL_FUNCTOR        5
#define LMN_RESUME_FUNCTOR     6
#define LMN_ARITHMETIC_IADD_FUNCTOR 7
#define LMN_ARITHMETIC_ISUB_FUNCTOR 8
#define LMN_ARITHMETIC_IMUL_FUNCTOR 9
#define LMN_ARITHMETIC_IDIV_FUNCTOR 10
#define LMN_ARITHMETIC_MOD_FUNCTOR  11
#define LMN_ARITHMETIC_FADD_FUNCTOR 12
#define LMN_ARITHMETIC_FSUB_FUNCTOR 13
#define LMN_ARITHMETIC_FMUL_FUNCTOR 14
#define LMN_ARITHMETIC_FDIV_FUNCTOR 15
#define LMN_PRINT_FUNCTOR 16

/*----------------------------------------------------------------------
 * functions
 */

LMN_EXTERN LmnWord lmn_copy_atom(LmnWord atom, LmnLinkAttr attr);
LMN_EXTERN LmnWord lmn_copy_data_atom(LmnWord atom, LmnLinkAttr);
LMN_EXTERN void lmn_free_atom(LmnWord atom, LmnLinkAttr attr);
LMN_EXTERN void free_symbol_atom_with_buddy_data(LmnAtomPtr atom);
LMN_EXTERN BOOL lmn_eq_func(LmnWord atom0, LmnLinkAttr attr0, LmnWord atom1, LmnLinkAttr attr1);

/*----------------------------------------------------------------------
 * allocation
 */

LMN_EXTERN LmnAtomPtr lmn_new_atom(LmnFunctor f);
LMN_EXTERN void lmn_delete_atom(LmnAtomPtr ap);
LMN_EXTERN void free_atom_memory_pools(void);
  
#endif /* LMN_ATOM_H */

