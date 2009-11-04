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
#include "error.h"
#include "task.h"
#include "mc.h"
#include "visitlog.h"

/* TR_GSID(x) translate global symbol id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GFID(x) translate global functor id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GRID(x) translate global ruleset id xのグローバルidを得る (定義に出力ファイル名を含むため.c内で出力) */
/* インタプリタ用の定義では変換は必要ないため TR_G*ID(x) = x となる */

extern LmnWord *wt, *wt_t;
extern LmnByte *at, *at_t;
extern unsigned int wt_size;

#define LINKED_ATOM(x) wt[x]
#define LINKED_ATTR(x) at[x]

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

#define TR_INSTR_LOOKUPLINK(destlinki, tbli, srclinki)  \
  do{                                                   \
    at[destlinki] = LINKED_ATTR(srclinki);              \
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(srclinki))) {      \
      wt[destlinki] = LINKED_ATOM(srclinki);            \
    }                                                   \
    else { /* symbol atom */                            \
      ProcessTbl ht = (ProcessTbl)wt[tbli];             \
      proc_tbl_get_by_atom(ht, LMN_SATOM(LINKED_ATOM(srclinki)), &wt[destlinki]); \
    }                                                   \
  }while(0)

#define TR_INSTR_DELETECONNECTORS(srcset, srcmap)       \
  do{                                                   \
    HashSet *delset;                                    \
    ProcessTbl delmap;                                  \
    HashSetIterator it;                                 \
    delset = (HashSet *)wt[srcset];                     \
    delmap = (ProcessTbl)wt[srcmap];                                    \
    for(it = hashset_iterator(delset); !hashsetiter_isend(&it); hashsetiter_next(&it)) { \
      LmnSAtom orig = LMN_SATOM(hashsetiter_entry(&it));                \
      LmnSAtom copy;                                                    \
      LmnWord t;                                                        \
      proc_tbl_get_by_atom(delmap, orig, &t);                           \
      copy = LMN_SATOM(t);                                              \
      lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);                 \
      /* mem がないので仕方なく直接アトムリストをつなぎ変える           \
         UNIFYアトムはnatomに含まれないので大丈夫 */                    \
      LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(copy), LMN_SATOM_GET_PREV(copy)); \
      LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(copy), LMN_SATOM_GET_NEXT_RAW(copy)); \
      lmn_delete_atom(orig);                                            \
    }                                                                   \
    proc_tbl_free(delmap);                                              \
  }while(0)

#define TR_INSTR_DEREFFUNC(funci, atomi, pos)         \
  do{                                           \
    LmnLinkAttr attr = LMN_SATOM_GET_ATTR(LMN_SATOM(wt[atomi]), pos);   \
    if (LMN_ATTR_IS_DATA(attr)) {                                       \
      wt[funci] = LMN_SATOM_GET_LINK(LMN_SATOM(wt[atomi]), pos);        \
    }                                                                   \
    else { /* symbol atom */                                            \
      wt[funci] = LMN_SATOM_GET_FUNCTOR(LMN_SATOM_GET_LINK(LMN_SATOM(wt[atomi]), pos)); \
    }                                                                   \
    at[funci] = attr;                                                   \
  }while(0)

extern void tr_instr_commit_ready(struct ReactCxt *rc, LmnRule rule, lmn_interned_str rule_name, LmnLineNum line_num, LmnMembrane **ptmp_global_root, LmnWord **pwt_temp, LmnByte **pat_temp);
extern BOOL tr_instr_commit_finish(struct ReactCxt *rc, LmnRule rule, lmn_interned_str rule_name, LmnLineNum line_num, LmnMembrane **ptmp_global_root, LmnWord **pwt_temp, LmnByte **pat_temp);
extern BOOL tr_instr_jump(LmnTranslated f, struct ReactCxt *rc, LmnMembrane *thisisrootmembutnotused, int newid_num, const int *newid, LmnWord **pwt, LmnByte **pat, unsigned int *pwt_size);
/* insertconnectors touches wt and at */
extern HashSet *insertconnectors(LmnMembrane *mem, const Vector *links);
extern Vector *links_from_idxs(Vector *link_idxs, LmnWord *wt, LmnByte *at);
extern void free_links(Vector *links);

#endif

