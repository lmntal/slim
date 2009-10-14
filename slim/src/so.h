/*
 * so.h
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
 *    3. Neither the name of the Ueda Laboratory LMNtal Groupy LMNtal
 *       Group nor the names of its contributors may be used to
 *       endorse or promote products derived from this software
 *       without specific prior written permission.
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
 * $Id: so.h,v 1.00 2009/10/01 18:18:00 riki Exp $
 */

/* このファイルはトランスレータから出力された.cファイルの中で必要なものをすべて含む */
/* .cの中でのみ必要な情報はここに直接書く */

#ifndef SO_H
#define SO_H

#include "lmntal.h"
#include "rule.h"
#include "functor.h"
#include "translate.h"
#include "load.h"
#include "symbol.h"
#include "react_context.h"
#include "slim_header/memstack.h"
#include "special_atom.h"

/* TR_GSID(x) translate global symbol id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GFID(x) translate global functor id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GRID(x) translate global ruleset id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* インタプリタ用の定義では変換は必要ないため TR_G*ID(x) = x となる */

extern LmnWord *wt, *wt_t;
extern LmnByte *at, *at_t;
extern unsigned int wt_size;

#define TR_INSTR_ALLOCLINK(link, atom, n)       \
  do{                                           \
    if (LMN_ATTR_IS_DATA(at[atom])) {           \
      wt[link] = wt[atom];                      \
      at[link] = at[atom];                              \
    } else { /* link to atom */                         \
      wt[link] = (LmnWord)LMN_SATOM(wt[atom]);          \
      at[link] = LMN_ATTR_MAKE_LINK(n);                 \
    }                                                                   \
  }while(0)

#define LINKED_ATOM(x) wt[x]
#define LINKED_ATTR(x) at[x]

#define TR_INSTR_SPEC(size)                     \
  do{                                           \
    if(size > wt_size){                         \
      wt_size = size;                           \
      wt = LMN_REALLOC(LmnWord, wt, wt_size);   \
      at = LMN_REALLOC(LmnLinkAttr, at, wt_size);       \
    }\
  }while(0)

#define TR_INSTR_UNIFYLINKS(link1, link2, mem)  \
  do{                                           \
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(link1))) {                         \
      if (LMN_ATTR_IS_DATA(LINKED_ATTR(link2))) { /* 1, 2 are data */   \
        lmn_mem_link_data_atoms((LmnMembrane *)wt[mem], wt[link1], at[link1], LINKED_ATOM(link2), LINKED_ATTR(link2)); \
      }                                                                 \
      else { /* 1 is data */                                            \
        LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATOM(link1)); \
        LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATTR(link1)); \
      }                                                                 \
    }                                                                   \
    else if (LMN_ATTR_IS_DATA(LINKED_ATTR(link2))) { /* 2 is data */    \
      LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATOM(link2)); \
      LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATTR(link2)); \
    }                                                                   \
    else { /* 1, 2 are symbol atom */                                   \
      LMN_SATOM_SET_LINK(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATOM(link2)); \
      LMN_SATOM_SET_LINK(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATOM(link1)); \
      LMN_SATOM_SET_ATTR(LINKED_ATOM(link1), LMN_ATTR_GET_VALUE(LINKED_ATTR(link1)), LINKED_ATTR(link2)); \
      LMN_SATOM_SET_ATTR(LINKED_ATOM(link2), LMN_ATTR_GET_VALUE(LINKED_ATTR(link2)), LINKED_ATTR(link1)); \
    }                                                                   \
  }while(0)

#define TR_INSTR_RELINK(atom1,pos1,atom2,pos2,memi)     \
  do{                                                                   \
    LmnSAtom ap = LMN_SATOM(LMN_SATOM_GET_LINK(wt[atom2], pos2));       \
    LmnByte attr = LMN_SATOM_GET_ATTR(wt[atom2], pos2);                 \
    if(LMN_ATTR_IS_DATA(at[atom1]) && LMN_ATTR_IS_DATA(attr)) {         \
      fprintf(stderr, "Two data atoms are connected each other.\n");    \
    }                                                                   \
    else if (LMN_ATTR_IS_DATA(at[atom1])) {                             \
      LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);                          \
      LMN_SATOM_SET_ATTR(ap, attr, at[atom1]);                          \
    }                                                                   \
    else if (LMN_ATTR_IS_DATA(attr)) {                                  \
      LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);               \
      LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);             \
    }                                                                   \
    else {                                                              \
      LMN_SATOM_SET_LINK(ap, attr, wt[atom1]);                          \
      LMN_SATOM_SET_ATTR(ap, attr, pos1);                               \
      LMN_SATOM_SET_LINK(LMN_SATOM(wt[atom1]), pos1, ap);               \
      LMN_SATOM_SET_ATTR(LMN_SATOM(wt[atom1]), pos1, attr);             \
    }                                                                   \
  }while(0)

#define TR_INSTR_COPYRULES(destmemi, srcmemi)   \
  do{                                           \
    unsigned int i;                             \
    struct Vector *v;                           \
    v = &((LmnMembrane *)wt[srcmemi])->rulesets;        \
    for (i = 0; i< v->num; i++) {                       \
      if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {        \
        lmn_mem_add_ruleset((LmnMembrane *)wt[destmemi], (LmnRuleSet)lmn_ruleset_copy((LmnRuleSet)vec_get(v, i))); \
      }else{                                                            \
        lmn_mem_add_ruleset((LmnMembrane *)wt[destmemi], (LmnRuleSet)vec_get(v, i)); \
      }                                                                 \
    }                                                                   \
  }while(0)

extern BOOL tr_instr_jump(LmnTranslated, struct ReactCxt*, LmnMembrane*, int newid_num, const int *newid);

#endif

