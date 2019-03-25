/*
 * so.h
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

/* このファイルはトランスレータから出力された.cファイルの中で必要なものをすべて含む
 */
/* .cの中でのみ必要な情報はここに直接書く */

#ifndef SO_H
#define SO_H

#include "element/element.h"
#include "lmntal.h"
#include "translate.hpp"
#include "verifier/verifier.h"
#include "vm/vm.h"

#ifdef PROFILE
#include "verifier/runtime_status.h"
#endif

/**
 * @ingroup  Loader
 * @defgroup SO
 * @{
 */

/* TR_GSID(x) translate global symbol id xのグローバルidを得る
 * (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GFID(x) translate global functor id xのグローバルidを得る
 * (定義に出力ファイル名を含むため.c内で出力) */
/* TR_GRID(x) translate global ruleset id xのグローバルidを得る
 * (定義に出力ファイル名を含むため.c内で出力) */
/* インタプリタ用の定義では変換は必要ないため TR_G*ID(x) = x となる */

#define LINKED_ATOM(x) rc->wt(x)
#define LINKED_ATTR(x) rc->at(x)

#define TR_INSTR_ALLOCLINK(rc, link, atom, n)                                  \
  do {                                                                         \
    if (LMN_ATTR_IS_DATA(rc->at(atom))) {                                      \
      rc->reg(link) = {rc->wt(atom), rc->at(atom), TT_ATOM};                   \
    } else { /* link to atom */                                                \
      rc->reg(link) = {rc->wt(atom), LMN_ATTR_MAKE_LINK(n), TT_ATOM};          \
    }                                                                          \
  } while (0)

/* @see INSTR_SPEC in task.c */
#define TR_INSTR_SPEC(rc, size)                                                \
  do {                                                                         \
    rc->resize(size);                                                          \
  } while (0)

#define TR_INSTR_UNIFYLINKS(rc, link1, link2, mem)                             \
  do {                                                                         \
    LmnLinkAttr attr1 = LINKED_ATTR(link1);                                    \
    LmnLinkAttr attr2 = LINKED_ATTR(link2);                                    \
                                                                               \
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr1)) {                                  \
      if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 1, 2 are data */            \
        lmn_mem_link_data_atoms((LmnMembraneRef)rc->wt(mem),                   \
                                (LmnAtomRef)rc->wt(link1), rc->at(link1),      \
                                (LmnAtomRef)LINKED_ATOM(link2), attr2);        \
      } else { /* 1 is data */                                                 \
        ((LmnSymbolAtomRef)LINKED_ATOM(link2))->set_link(               \
                           LMN_ATTR_GET_VALUE(attr2),                          \
                           (LmnAtomRef)LINKED_ATOM(link1));                    \
        ((LmnSymbolAtomRef)LINKED_ATOM(link2))->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);                  \
      }                                                                        \
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr2)) { /* 2 is data */           \
      ((LmnSymbolAtomRef)LINKED_ATOM(link1))->set_link(                 \
                         LMN_ATTR_GET_VALUE(attr1),                            \
                         (LmnAtomRef)LINKED_ATOM(link2));                      \
      ((LmnSymbolAtomRef)LINKED_ATOM(link1))->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);                    \
    } else { /* 1, 2 are symbol atom */                                        \
                                                                               \
      if (LMN_ATTR_IS_EX(attr1)) {                                             \
        if (LMN_ATTR_IS_EX(attr2)) { /* 1, 2 are ex */                         \
          lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),                     \
                              (LmnSymbolAtomRef)(LINKED_ATOM(link1)), attr1,   \
                              0, (LmnSymbolAtomRef)(LINKED_ATOM(link2)),       \
                              attr2, 0);                                       \
        } else { /* 1 is ex */                                                 \
          lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),                     \
                              (LmnSymbolAtomRef)(LINKED_ATOM(link1)), attr1,   \
                              0, (LmnSymbolAtomRef)(LINKED_ATOM(link2)),       \
                              attr2, attr2);                                   \
        }                                                                      \
      } else if (LMN_ATTR_IS_EX(attr2)) { /* 2 is ex */                        \
        lmn_newlink_with_ex((LmnMembraneRef)rc->wt(mem),                       \
                            (LmnSymbolAtomRef)(LINKED_ATOM(link1)), attr1,     \
                            attr1, (LmnSymbolAtomRef)(LINKED_ATOM(link2)),     \
                            attr2, 0);                                         \
      } else {                                                                 \
        ((LmnSymbolAtomRef)LINKED_ATOM(link1))->set_link(               \
                           LMN_ATTR_GET_VALUE(attr1),                          \
                           (LmnAtomRef)LINKED_ATOM(link2));                    \
        ((LmnSymbolAtomRef)LINKED_ATOM(link2))->set_link(             \
                           LMN_ATTR_GET_VALUE(attr2),                          \
                           (LmnAtomRef)LINKED_ATOM(link1));                    \
        ((LmnSymbolAtomRef)LINKED_ATOM(link1))->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);                  \
        ((LmnSymbolAtomRef)LINKED_ATOM(link2))->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);                  \
      }                                                                        \
    }                                                                          \
  } while (0)

#define TR_INSTR_RELINK(rc, atom1, pos1, atom2, pos2, memi)                    \
  do {                                                                         \
    LmnSymbolAtomRef ap;                                                       \
    LmnByte attr;                                                              \
    ap = (LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atom2))->get_link(pos2);                           \
    attr = ((LmnSymbolAtomRef)rc->wt(atom2))->get_attr(pos2);          \
                                                                               \
    if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1)) &&                          \
        LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {                                   \
      fprintf(stderr, "Two data atoms are connected each other.\n");           \
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(rc->at(atom1))) {                   \
      ap->set_link(attr, (LmnAtomRef)rc->wt(atom1));                 \
      ap->set_attr(attr, rc->at(atom1));                             \
    } else if (LMN_ATTR_IS_DATA_WITHOUT_EX(attr)) {                            \
      ((LmnSymbolAtomRef)(rc->wt(atom1)))->set_link(pos1,(LmnAtomRef)ap);                                      \
      ((LmnSymbolAtomRef)(rc->wt(atom1)))->set_attr(pos1, attr);       \
    } else if (!LMN_ATTR_IS_EX(rc->at(atom1)) && !LMN_ATTR_IS_EX(attr)) {      \
      ap->set_link(attr, (LmnAtomRef)rc->wt(atom1));                 \
      ap->set_attr(attr, pos1);                                      \
      ((LmnSymbolAtomRef)(rc->wt(atom1)))->set_link(pos1,              \
                         (LmnAtomRef)ap);                                      \
      ((LmnSymbolAtomRef)(rc->wt(atom1)))->set_attr(pos1, attr);       \
    } else if (LMN_ATTR_IS_EX(rc->at(atom1))) {                                \
      lmn_newlink_with_ex((LmnMembraneRef)rc->wt(memi),                        \
                          (LmnSymbolAtomRef)(rc->wt(atom1)), rc->at(atom1),    \
                          pos1, ap, 0, attr);                                  \
    } else {                                                                   \
      lmn_newlink_with_ex((LmnMembraneRef)rc->wt(memi),                        \
                          (LmnSymbolAtomRef)(rc->wt(atom1)), rc->at(atom1),    \
                          pos1, ap, attr, 0);                                  \
    }                                                                          \
  } while (0)

#define TR_INSTR_COPYRULES(rc, destmemi, srcmemi)                              \
  do {                                                                         \
    unsigned int i;                                                            \
    struct Vector *v;                                                          \
    v = lmn_mem_get_rulesets((LmnMembraneRef)rc->wt(srcmemi));                 \
    for (i = 0; i < v->get_num(); i++) {                                             \
      LmnRuleSetRef cp = new LmnRuleSet(*((LmnRuleSetRef)v->get(i)));      \
      lmn_mem_add_ruleset((LmnMembraneRef)rc->wt(destmemi), cp);               \
    }                                                                          \
  } while (0)

#define TR_INSTR_LOOKUPLINK(rc, destlinki, tbli, srclinki)                     \
  do {                                                                         \
    rc->at(destlinki) = LINKED_ATTR(srclinki);                                 \
    rc->tt(destlinki) = TT_ATOM;                                               \
    if (LMN_ATTR_IS_DATA(LINKED_ATTR(srclinki))) {                             \
      rc->wt(destlinki) = LINKED_ATOM(srclinki);                               \
    } else { /* symbol atom */                                                 \
      ProcessTableRef ht = (ProcessTableRef)rc->wt(tbli);                      \
      LmnWord w = rc->wt(destlinki);                                           \
      proc_tbl_get_by_atom(ht, (LmnSymbolAtomRef)(LINKED_ATOM(srclinki)), &w); \
      rc->wt(destlinki) = w;                                                   \
    }                                                                          \
  } while (0)

#define TR_INSTR_DELETECONNECTORS(srcset, srcmap)                              \
  do {                                                                         \
    HashSet *delset;                                                           \
    ProcessTableRef delmap;                                                    \
    HashSetIterator it;                                                        \
    delset = (HashSet *)rc->wt(srcset);                                        \
    delmap = (ProcessTableRef)rc->wt(srcmap);                                  \
                                                                               \
    for (it = hashset_iterator(delset); !hashsetiter_isend(&it);               \
         hashsetiter_next(&it)) {                                              \
      LmnSymbolAtomRef orig, copy;                                             \
      LmnWord t;                                                               \
                                                                               \
      orig = (LmnSymbolAtomRef)(hashsetiter_entry(&it));                       \
      t = 0; /* warningを黙らす */                                         \
      proc_tbl_get_by_atom(delmap, orig, &t);                                  \
      copy = (LmnSymbolAtomRef)(t);                                            \
      lmn_mem_unify_symbol_atom_args(orig, 0, orig, 1);                        \
      lmn_mem_unify_symbol_atom_args(copy, 0, copy, 1);                        \
                                                                               \
      lmn_delete_atom(orig);                                                   \
      lmn_delete_atom(copy);                                                   \
    }                                                                          \
                                                                               \
    if (delmap)                                                                \
      proc_tbl_free(delmap);                                                   \
  } while (0)

#define TR_INSTR_DEREFFUNC(rc, funci, atomi, pos)                              \
  do {                                                                         \
    LmnLinkAttr attr =                                                         \
        ((LmnSymbolAtomRef)rc->wt(atomi))->get_attr(pos);              \
    if (LMN_ATTR_IS_DATA(attr)) {                                              \
      rc->reg(funci) = {                                                       \
          (LmnWord)((LmnSymbolAtomRef)rc->wt(atomi))->get_link(pos),   \
          attr, TT_OTHER};                                                     \
    } else { /* symbol atom */                                                 \
      rc->reg(funci) = {                                                       \
          (LmnWord)((LmnSymbolAtomRef)((LmnSymbolAtomRef)rc->wt(atomi))->get_link(pos))->get_functor(),                          \
          attr, TT_OTHER};                                                     \
    }                                                                          \
  } while (0)

void tr_instr_commit_ready(LmnReactCxtRef rc, LmnRuleRef rule,
                           lmn_interned_str rule_name, LmnLineNum line_num,
                           LmnMembraneRef *ptmp_global_root,
                           LmnRegisterArray *V, unsigned int *org_next_id);
BOOL tr_instr_commit_finish(LmnReactCxtRef rc, LmnRuleRef rule,
                            lmn_interned_str rule_name, LmnLineNum line_num,
                            LmnMembraneRef *ptmp_global_root,
                            LmnRegisterArray *p_v_tmp,
                            unsigned int warray_use_org,
                            unsigned int warray_size_org);
BOOL tr_instr_jump(LmnTranslated f, LmnReactCxtRef rc,
                   LmnMembraneRef thisisrootmembutnotused, LmnRuleRef rule,
                   int newid_num, const int *newid);

/* @} */

#endif
