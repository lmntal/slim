/*
 * delta_membrane.cpp
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
#include "delta_membrane.h"
#ifdef PROFILE
#include "runtime_status.h"
#endif
enum {
  TAG_DEL_MEM = 1U /* 0000 0001 */,
  TAG_NEW_ATOM = 2U /* 0000 0010 */,
  TAG_DEL_ATOM = 4U /* 0000 0100 */,
  TAG_NEW_MEM = 8U /* 0000 1000 */,
  TAG_DELTA_MEM = 16U /* 0001 0000 */,
  TAG_MODIFIED_ATOM = 32U /* 0010 0000 */
};

static inline BOOL dmem_root_is_new_mem(struct MemDeltaRoot *d,
                                        LmnMembraneRef m);
static inline BOOL dmem_root_is_delta_mem(struct MemDeltaRoot *d,
                                          LmnMembraneRef m);

static LmnSymbolAtomRef dmem_root_alter_functor(struct MemDeltaRoot *root_d,
                                                LmnMembraneRef mem,
                                                LmnSymbolAtomRef atom,
                                                LmnFunctor f);
static struct NewMemInfo *dmem_root_get_new_mem_info(struct MemDeltaRoot *d,
                                                     LmnMembraneRef m);
static LmnMembraneRef dmem_root_get_parent(struct MemDeltaRoot *root_d,
                                           LmnMembraneRef m);
static inline LmnSymbolAtomRef
dmem_root_copy_satom_with_data(struct MemDeltaRoot *d, LmnSymbolAtomRef atom);
static inline LmnSymbolAtomRef dmem_root_copy_satom(struct MemDeltaRoot *d,
                                                    LmnSymbolAtomRef atom);
static inline LmnSymbolAtomRef
dmem_root_copy_eqatom_with_data(LmnSymbolAtomRef atom);
static inline void dmem_root_commit_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef src,
                                         LmnSymbolAtomRef atom);
static inline void dmem_root_revert_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef src,
                                         LmnSymbolAtomRef atom);
static inline BOOL dmem_root_is_freed_atom(struct MemDeltaRoot *d,
                                           LmnSymbolAtomRef a);
static inline LmnSymbolAtomRef dmem_root_modified_atom(struct MemDeltaRoot *d,
                                                       LmnSymbolAtomRef a);
static inline void dmem_root_free_satom(struct MemDeltaRoot *d,
                                        LmnSymbolAtomRef atom);
static inline BOOL dmem_root_is_new_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef a);
static inline BOOL dmem_root_is_modified_atom(struct MemDeltaRoot *d,
                                              LmnSymbolAtomRef a);
static inline LmnMembraneRef dmem_root_atom_mem(struct MemDeltaRoot *d,
                                                LmnSymbolAtomRef a) LMN_UNUSED;
static inline struct MemDelta *dmem_root_get_mem_delta(struct MemDeltaRoot *d,
                                                       LmnMembraneRef m);

static struct MemDelta *mem_delta_make(struct MemDeltaRoot *root_d,
                                       LmnMembraneRef m, unsigned long next_id);
static void mem_delta_free(struct MemDelta *p);

static inline BOOL dmem_is_new_atom(struct MemDelta *d, LmnMembraneRef m,
                                    LmnSymbolAtomRef a) LMN_UNUSED;
static inline BOOL dmem_is_removed_atom(struct MemDelta *d, LmnMembraneRef m,
                                        LmnSymbolAtomRef a);
static inline BOOL dmem_is_removed_mem(struct MemDelta *d,
                                       LmnMembraneRef parent,
                                       LmnMembraneRef child);
static inline void dmem_put_atom(struct MemDelta *d, LmnMembraneRef m,
                                 LmnAtomRef atom, LmnLinkAttr attr);
static inline void dmem_put_symbol_atom(struct MemDelta *d, LmnMembraneRef m,
                                        LmnSymbolAtomRef atom);
static inline void dmem_remove_symbol_atom(struct MemDelta *d, LmnMembraneRef m,
                                           LmnSymbolAtomRef atom);
static void dmem_link_data_atoms(struct MemDelta *d, LmnMembraneRef m,
                                 LmnDataAtomRef d0, LmnLinkAttr attr0,
                                 LmnDataAtomRef d1, LmnLinkAttr attr1);
static inline LmnSymbolAtomRef
dmem_modify_atom(struct MemDelta *d, LmnMembraneRef mem, LmnSymbolAtomRef src);
static inline void dmem_modify_link(struct MemDelta *d, LmnMembraneRef m,
                                    LmnSymbolAtomRef atom, int i, LmnAtomRef l,
                                    LmnLinkAttr attr);

static void dmem_copy_cells(struct MemDeltaRoot *root_d, struct MemDelta *d,
                            LmnMembraneRef destmem, struct MemDelta *d2,
                            LmnMembraneRef srcmem, ProcessTableRef atoms);
static void dmem_add_ruleset(struct MemDelta *d, LmnMembraneRef m,
                             LmnRuleSetRef ruleset);
static inline void dmem_clear_ruleset(struct MemDelta *d, LmnMembraneRef m);
static void dmem_add_child_mem(struct MemDelta *d, LmnMembraneRef parent,
                               LmnMembraneRef child);
static inline void dmem_remove_atom(struct MemDelta *d, LmnMembraneRef m,
                                    LmnAtomRef atom, LmnLinkAttr attr);
static void modify_free_link(struct MemDeltaRoot *d, LmnMembraneRef m);
static void modify_free_link_sub(struct MemDeltaRoot *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef in);
static inline void dmem_copy_rules(struct MemDelta *d, LmnMembraneRef dest,
                                   LmnMembraneRef src);
static void dmem_drop(struct MemDelta *d, LmnMembraneRef mem);
static void dmem_commit(struct MemDelta *d);
static inline void dmem_commit_delete_mem(struct MemDelta *d);
static void dmem_revert(struct MemDelta *d);
static inline void dmem_revert_new_mem(struct MemDelta *d);
static void dmem_dump(struct MemDelta *d);
static inline void dmem_relink(struct MemDelta *d, LmnMembraneRef m,
                               LmnSymbolAtomRef atom1, int pos1,
                               LmnSymbolAtomRef atom2, int pos2);
static void dmem_unify_atom_args(struct MemDelta *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef atom1, int pos1,
                                 LmnSymbolAtomRef atom2, int pos2);

static struct NewMemInfo *new_mem_info_make(LmnMembraneRef mem) {
  struct NewMemInfo *p = LMN_MALLOC(struct NewMemInfo);

  p->mem = mem;
  vec_init(&p->new_child_mems, 16);
  vec_init(&p->removed_child_mems, 16);
  return p;
}

static void new_mem_info_free(struct NewMemInfo *p) {
  vec_destroy(&p->new_child_mems);
  vec_destroy(&p->removed_child_mems);
  LMN_FREE(p);
}

#define dmem_get_attr(d, m, atom, i) LMN_SATOM_GET_ATTR((atom), (i))

#define DMEM_ORG_EACH_FUNC_ATOM(D, MEM, F, V, CODE)                            \
  do {                                                                         \
    AtomListEntryRef __ent = lmn_mem_get_atomlist((MEM), (F));                 \
    LmnSymbolAtomRef __next;                                                   \
    if (__ent) {                                                               \
      for ((V) = atomlist_head(__ent); (V) != lmn_atomlist_end(__ent);         \
           (V) = __next) {                                                     \
        __next = LMN_SATOM_GET_NEXT_RAW((V));                                  \
        if (LMN_SATOM_GET_FUNCTOR((V)) != LMN_RESUME_FUNCTOR &&                \
            !((D) && dmem_is_removed_atom((D), (MEM), (V)))) {                 \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (0)

#define DMEM_EACH_FUNC_ATOM(D, MEM, F, V, CODE)                                \
  do {                                                                         \
    int __i;                                                                   \
    DMEM_ORG_EACH_FUNC_ATOM(D, MEM, F, V, CODE);                               \
    if ((D)) {                                                                 \
      for (__i = 0; __i < vec_num(&(D)->new_proxies); __i++) {                 \
        (V) = (LmnSymbolAtomRef)vec_get(&(D)->new_proxies, __i);               \
        if (LMN_SATOM_GET_FUNCTOR(V) == F &&                                   \
            !dmem_is_removed_atom((D), (MEM), (V))) {                          \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (0)

#define DMEM_ALL_ATOMS(D, MEM, V, CODE)                                        \
  do {                                                                         \
    if (!(D)) {                                                                \
      ALL_ATOMS(MEM, V, CODE);                                                 \
    } else {                                                                   \
      int __i, i_atomlist;                                                     \
      for (i_atomlist = 0; i_atomlist <= (D)->max_functor; i_atomlist++) {     \
        DMEM_ORG_EACH_FUNC_ATOM(D, MEM, i_atomlist, V, CODE);                  \
      }                                                                        \
      for (__i = 0; __i < vec_num(&(D)->new_atoms); __i++) {                   \
        (V) = (LmnSymbolAtomRef)vec_get(&(D)->new_atoms, __i);                 \
        if (!dmem_is_removed_atom((D), (MEM), (V))) {                          \
          (CODE);                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (0)

#define DMEM_ALL_MEMS(D, MEM, V, CODE)                                         \
  do {                                                                         \
    unsigned int i;                                                            \
    LmnMembraneRef __next;                                                     \
    for ((V) = MEM->mem_child_head(); (V); (V) = __next) {                   \
      __next = V->mem_next();                                                \
      if (!(D) || !dmem_is_removed_mem((D), (MEM), (V))) {                     \
        (CODE);                                                                \
      }                                                                        \
    }                                                                          \
    if ((D)) {                                                                 \
      for (i = 0; i < vec_num(&(D)->new_mems); i++) {                          \
        (V) = (LmnMembraneRef)vec_get(&(D)->new_mems, i);                      \
        (CODE);                                                                \
      }                                                                        \
    }                                                                          \
  } while (0)

struct MemDeltaRoot *dmem_root_make(LmnMembraneRef root_mem, LmnRuleRef rule,
                                    unsigned long next_id) {
  struct MemDeltaRoot *p;
  int size;

  p = LMN_MALLOC(struct MemDeltaRoot);
  size = round2up(next_id + 10); /* TODO: 引数に渡す適当なサイズは？ */

  p->next_id = next_id;
  p->root_mem = root_mem;
  p->committed = FALSE;
  p->applied_rule = rule;
  p->proc_tbl = proc_tbl_make_with_size(size);
  vec_init(&p->new_mems, 16);
  vec_init(&p->mem_deltas, 16);
  vec_init(&p->modified_atoms, 32);
  p->owner_tbl = proc_tbl_make_with_size(size);
  p->flag_tbl = sproc_tbl_make_with_size(size);

  /* add an appried history for constraint handling rules */
  p->applied_history = rule ? rule->latest_history() : ANONYMOUS;

  /* この時点で最大のIDを記録しておくことで,
   * 以降に生成されたIDが新規か否かを判定する */
  p->new_proc_id_lower_limit = next_id;
  return p;
}

void dmem_root_free(struct MemDeltaRoot *p) {
  unsigned int i;

  for (i = 0; i < vec_num(&p->mem_deltas); i++) {
    mem_delta_free((struct MemDelta *)vec_get(&p->mem_deltas, i));
  }
  vec_destroy(&p->mem_deltas);
  vec_destroy(&p->modified_atoms);
  proc_tbl_free(p->owner_tbl);
  sproc_tbl_free(p->flag_tbl);

  for (i = 0; i < vec_num(&p->new_mems); i++) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)vec_get(&p->new_mems, i);
    if (proc_tbl_get_by_mem(p->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      new_mem_info_free(new_mem_info);
      lmn_mem_drop(mem);
      delete mem;
    } else {
      lmn_fatal("unexpected");
    }
  }

  vec_destroy(&p->new_mems);
  proc_tbl_free(p->proc_tbl);

  LMN_FREE(p);
}

LmnMembraneRef dmem_root_get_root_mem(struct MemDeltaRoot *d) {
  return d->root_mem;
}

unsigned long dmem_root_get_next_id(struct MemDeltaRoot *d) {
  return d->next_id;
}

static inline struct MemDelta *dmem_root_get_mem_delta(struct MemDeltaRoot *d,
                                                       LmnMembraneRef m) {
  LmnWord t;
  /* /\*d*\/ if (dmem_root_is_new_mem(d, m)) lmn_fatal("unexpected"); */
  if (proc_tbl_get_by_mem(d->proc_tbl, m, &t))
    return (struct MemDelta *)t;
  else {
    struct MemDelta *mem_delta = mem_delta_make(d, m, d->next_id);
    proc_tbl_put_mem(d->proc_tbl, m, (LmnWord)mem_delta);
    sproc_tbl_set_mem_flag(d->flag_tbl, m, TAG_DELTA_MEM);
    vec_push(&d->mem_deltas, (vec_data_t)mem_delta);
    return mem_delta;
  }
}

static inline BOOL dmem_root_is_delta_mem(struct MemDeltaRoot *d,
                                          LmnMembraneRef m) {
  return sproc_tbl_get_flag_by_mem(d->flag_tbl, m, TAG_DELTA_MEM);
}

static inline BOOL dmem_root_is_new_mem(struct MemDeltaRoot *d,
                                        LmnMembraneRef m) {
  return m->mem_id() >= d->new_proc_id_lower_limit;
}

/* 膜mにアトムatomを追加 */
void dmem_root_push_atom(struct MemDeltaRoot *d, LmnMembraneRef m,
                         LmnAtomRef atom, LmnLinkAttr attr) {
  if (dmem_root_is_new_mem(d, m)) { /* BODYで生成された膜の場合 */
    lmn_mem_push_atom(m, atom, attr);
  } else { /* 既にある膜への追加 */
    dmem_put_atom(dmem_root_get_mem_delta(d, m), m, atom, attr);
  }
}

void dmem_root_remove_atom(struct MemDeltaRoot *d, LmnMembraneRef m,
                           LmnAtomRef atom, LmnLinkAttr attr) {
  if (dmem_root_is_new_mem(d, m)) {
    lmn_mem_remove_atom(m, atom, attr);
  } else {
    dmem_remove_atom(dmem_root_get_mem_delta(d, m), m, atom, attr);
  }
}

void dmem_root_newlink(struct MemDeltaRoot *root_d, LmnMembraneRef m,
                       LmnAtomRef atom0, LmnLinkAttr attr0, int pos0,
                       LmnAtomRef atom1, LmnLinkAttr attr1, int pos1) {
  if (dmem_root_is_new_mem(root_d, m)) {
    lmn_mem_newlink(m, atom0, attr0, pos0, atom1, attr1, pos1);
  } else {
    struct MemDelta *d =
        dmem_root_get_mem_delta(root_d, m); /* 膜m用の差分データを呼び出す */
    if (LMN_ATTR_IS_DATA(attr0) &&
        LMN_ATTR_IS_DATA(attr1)) { /* UNIFYファンクタを作ってデータを埋め込む */
      dmem_link_data_atoms(d, m, (LmnDataAtomRef)atom0, attr0,
                           (LmnDataAtomRef)atom1, attr1);
    } else if (LMN_ATTR_IS_DATA(attr0)) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)atom1, pos1, atom0);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom1, pos1, attr0);
    } else if (LMN_ATTR_IS_DATA(attr1)) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)(atom0), pos0, atom1);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)(atom0), pos0, attr1);
    } else {
      lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom0), pos0,
                             (LmnSymbolAtomRef)(atom1), pos1);
    }
  }
}

/* データアトム同士の接続 */
void dmem_root_link_data_atoms(struct MemDeltaRoot *d, LmnMembraneRef m,
                               LmnDataAtomRef d1, LmnLinkAttr attr1,
                               LmnDataAtomRef d2, LmnLinkAttr attr2) {
  dmem_link_data_atoms(dmem_root_get_mem_delta(d, m), m, d1, attr1, d2, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void dmem_root_unify_atom_args(struct MemDeltaRoot *d, LmnMembraneRef m,
                               LmnSymbolAtomRef atom1, int pos1,
                               LmnSymbolAtomRef atom2, int pos2) {
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  if (dmem_root_is_new_mem(d, m)) {
    ap1 = dmem_root_get_link(d, atom1, pos1);
    ap2 = dmem_root_get_link(d, atom2, pos2);
    attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
    attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);
    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      lmn_mem_link_data_atoms(m, ap1, attr1, ap2, attr2);
    } else {
      if (!LMN_ATTR_IS_DATA(attr1)) {
        if (LMN_IS_PROXY_FUNCTOR(
                LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)ap1)) &&
            LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1) != m &&
            !dmem_root_is_new_mem(d,
                                  LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1))) {
          dmem_modify_link(dmem_root_get_mem_delta(
                               d, LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1)),
                           LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1),
                           (LmnSymbolAtomRef)(ap1), LMN_ATTR_GET_VALUE(attr1),
                           ap2, attr2);
        } else {
          LMN_SATOM_SET_LINK((LmnSymbolAtomRef)ap1, LMN_ATTR_GET_VALUE(attr1),
                             (LmnSymbolAtomRef)ap2);
          LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)ap1, LMN_ATTR_GET_VALUE(attr1),
                             attr2);
        }
      }
      if (!LMN_ATTR_IS_DATA(attr2)) {
        if (LMN_IS_PROXY_FUNCTOR(
                LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)ap2)) &&
            LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2) != m &&
            !dmem_root_is_new_mem(d,
                                  LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2))) {
          dmem_modify_link(dmem_root_get_mem_delta(
                               d, LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2)),
                           LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2),
                           (LmnSymbolAtomRef)(ap2), LMN_ATTR_GET_VALUE(attr2),
                           ap1, attr1);
        } else {
          LMN_SATOM_SET_LINK((LmnSymbolAtomRef)ap2, LMN_ATTR_GET_VALUE(attr2),
                             ap1);
          LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)ap2, LMN_ATTR_GET_VALUE(attr2),
                             attr1);
        }
      }
    }
  } else {
    dmem_unify_atom_args(dmem_root_get_mem_delta(d, m), m,
                         dmem_root_modified_atom(d, atom1), pos1,
                         dmem_root_modified_atom(d, atom2), pos2);
  }
}

/* シンボルアトムatom1と、シンボルorデータアトムatom2の間にリンクを張る。 */
void dmem_root_unify_links(struct MemDeltaRoot *d, LmnMembraneRef m,
                           LmnAtomRef atom1, LmnLinkAttr attr1,
                           LmnAtomRef atom2, LmnLinkAttr attr2) {
  if (LMN_ATTR_IS_DATA(attr2)) { /* 2 is data */
    if (dmem_root_is_new_atom(d, (LmnSymbolAtomRef)(atom1))) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)atom1, LMN_ATTR_GET_VALUE(attr1),
                         (LmnSymbolAtomRef)atom2);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom1, LMN_ATTR_GET_VALUE(attr1),
                         attr2);
    } else {
      dmem_modify_link(dmem_root_get_mem_delta(d, m), m,
                       (LmnSymbolAtomRef)(atom1), LMN_ATTR_GET_VALUE(attr1),
                       (LmnSymbolAtomRef)atom2, attr2);
    }
  } /* 1, 2 are symbol atom */
  else if (dmem_root_is_new_atom(d, (LmnSymbolAtomRef)(atom1))) {
    LMN_SATOM_SET_LINK((LmnSymbolAtomRef)atom1, LMN_ATTR_GET_VALUE(attr1),
                       (LmnSymbolAtomRef)atom2);
    LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom1, LMN_ATTR_GET_VALUE(attr1),
                       attr2);
    if (dmem_root_is_new_atom(d, (LmnSymbolAtomRef)(atom2))) {
      LMN_SATOM_SET_LINK((LmnSymbolAtomRef)atom2, LMN_ATTR_GET_VALUE(attr2),
                         (LmnSymbolAtomRef)atom1);
      LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom2, LMN_ATTR_GET_VALUE(attr2),
                         attr1);
    } else {
      dmem_modify_link(dmem_root_get_mem_delta(d, m), m,
                       (LmnSymbolAtomRef)(atom2), LMN_ATTR_GET_VALUE(attr2),
                       atom1, attr1);
    }
  } else if (dmem_root_is_new_atom(d, (LmnSymbolAtomRef)(atom2))) {
    dmem_modify_link(dmem_root_get_mem_delta(d, m), m,
                     (LmnSymbolAtomRef)(atom1), LMN_ATTR_GET_VALUE(attr1),
                     (LmnSymbolAtomRef)atom2, attr2);
    LMN_SATOM_SET_LINK((LmnSymbolAtomRef)atom2, LMN_ATTR_GET_VALUE(attr2),
                       (LmnSymbolAtomRef)atom1);
    LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom2, LMN_ATTR_GET_VALUE(attr2),
                       attr1);
  } else {
    /* atom1,2を複製 */
    LmnSymbolAtomRef copy1, copy2;

    copy1 = dmem_modify_atom(dmem_root_get_mem_delta(d, m), m,
                             (LmnSymbolAtomRef)(atom1));
    copy2 = dmem_modify_atom(dmem_root_get_mem_delta(d, m), m,
                             (LmnSymbolAtomRef)(atom2));
    /* 複製したatom1とatom2を接続させる */
    LMN_SATOM_SET_LINK(copy1, LMN_ATTR_GET_VALUE(attr1), copy2);
    LMN_SATOM_SET_ATTR(copy1, LMN_ATTR_GET_VALUE(attr1), attr2);
    LMN_SATOM_SET_LINK(copy2, LMN_ATTR_GET_VALUE(attr2), copy1);
    LMN_SATOM_SET_ATTR(copy2, LMN_ATTR_GET_VALUE(attr2), attr1);
  }
}

/* atom1は新規アトム、atom2はもともと接続していたアトム */
void dmem_root_relink(struct MemDeltaRoot *root_d, LmnMembraneRef m,
                      LmnAtomRef atom1, LmnLinkAttr attr1, int pos1,
                      LmnAtomRef atom2, LmnLinkAttr attr2, int pos2) {
  /* atom1は新規アトムのはず */
  LmnAtomRef ap; /* 現在atom2と接続していて、将来的にatom1に接続するアトム */
  LmnLinkAttr attr;

  /* if (!dmem_root_is_new_atom(root_d, LMN_SATOM(atom1)))
   * lmn_fatal("unexpected"); */

  if (dmem_root_is_new_mem(root_d, m)) {
    /* mが新規膜の場合 */
    /* if (LMN_ATTR_IS_DATA(attr2)) lmn_fatal("unexpected"); */
    ap = dmem_root_get_link(root_d, (LmnSymbolAtomRef)(atom2), pos2);
    attr = LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)atom2, pos2);

    if (LMN_ATTR_IS_DATA(attr)) {
      lmn_mem_relink_atom_args(m, atom1, attr1, pos1, atom2, attr2, pos2);
    } else if (LMN_IS_PROXY_FUNCTOR(
                   LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)ap)) &&
               LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap) != m) {
      /* atom1（またはatom2）とapが別膜の場合（atom1,apが$in-$outの関係） */
      LmnMembraneRef m2 = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap);
      if (dmem_root_is_new_mem(root_d, m2)) {
        /* apの膜が新規膜 */
        //        lmn_mem_relink_atom_args(m, atom1, attr1, pos1, atom2, attr2,
        //        pos2);
        lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom1), pos1,
                               (LmnSymbolAtomRef)(ap),
                               LMN_ATTR_GET_VALUE(attr));
      } else {
        /* apの膜が既存膜 */
        LMN_SATOM_SET_LINK(
            (LmnSymbolAtomRef)atom1, pos1,
            dmem_root_modified_atom(root_d, (LmnSymbolAtomRef)(ap)));
        LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)atom1, pos1, attr);

        dmem_modify_link(
            dmem_root_get_mem_delta(root_d, m2), m2,
            dmem_root_modified_atom(root_d, (LmnSymbolAtomRef)(ap)),
            LMN_ATTR_GET_VALUE(attr), atom1, LMN_ATTR_MAKE_LINK(attr1));
      }
    } else {
      lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom1), pos1,
                             (LmnSymbolAtomRef)(ap), LMN_ATTR_GET_VALUE(attr));
    }
  } else {
    /* mが既存膜の場合 */
    dmem_relink(
        dmem_root_get_mem_delta(root_d, m), m,
        dmem_root_modified_atom(root_d, (LmnSymbolAtomRef)(atom1)), pos1,
        dmem_root_modified_atom(root_d, (LmnSymbolAtomRef)(atom2)), pos2);
  }
}

static inline void dmem_root_move_satom(struct MemDeltaRoot *d, LmnWord key,
                                        LmnWord dest) {
  proc_tbl_put_new(d->proc_tbl, key, dest);
}

int dmem_root_move_satom_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  dmem_root_move_satom((struct MemDeltaRoot *)_arg, _k, (LmnWord)_v);
  return 1;
}

/* destが移動先、srcが移動元 */
void dmem_root_move_cells(struct MemDeltaRoot *d, LmnMembraneRef destmem,
                          LmnMembraneRef srcmem) {
  if (dmem_root_is_new_mem(d, destmem) && dmem_root_is_new_mem(d, srcmem)) {
    /* 移動先・移動元ともに新規膜 */
    lmn_mem_move_cells(destmem, srcmem);
  } else {
    ProcessTableRef atoms = dmem_root_copy_cells(d, destmem, srcmem);
    if (!dmem_root_is_new_mem(d, srcmem)) {
      /* 移動先が既存膜 */
      /* printf("move cells\n"); */
      /* printf("src mem :" ); lmn_dump_mem_dev(srcmem); */
      /* printf("dest mem: "); lmn_dump_mem_dev(destmem); */
      dmem_drop(dmem_root_get_mem_delta(d, srcmem), srcmem);
      modify_free_link(d, destmem);
    } else {
      lmn_fatal("unexpected");
    }
    /* relink命令等のために移動先と移動元のアトムを対応付ける必要がある */
    proc_tbl_foreach(atoms, dmem_root_move_satom_f, (LmnWord)d);
    proc_tbl_free(atoms);
  }
}

ProcessTableRef dmem_root_copy_cells(struct MemDeltaRoot *d,
                                     LmnMembraneRef destmem,
                                     LmnMembraneRef srcmem) {
  if (dmem_root_is_new_mem(d, destmem) && dmem_root_is_new_mem(d, srcmem)) {
    return lmn_mem_copy_cells(destmem, srcmem);
  } else {
    ProcessTableRef atoms = proc_tbl_make_with_size(64);
    /* /\*d*\/ if (dmem_root_is_new_mem(d, srcmem)) lmn_fatal("unexpected"); */
    if (dmem_root_is_new_mem(d, destmem)) {
      /* 移動先が新規膜 */
      dmem_copy_cells(d, NULL, destmem, dmem_root_get_mem_delta(d, srcmem),
                      srcmem, atoms);
    } else if (dmem_root_is_new_mem(d, srcmem)) {
      /* 移動元が新規膜 */
      dmem_copy_cells(d, dmem_root_get_mem_delta(d, destmem), destmem, NULL,
                      srcmem, atoms);
    } else {
      dmem_copy_cells(d, dmem_root_get_mem_delta(d, destmem), destmem,
                      dmem_root_get_mem_delta(d, srcmem), srcmem, atoms);
    }
    return atoms;
  }
}

static void dmem_copy_cells(struct MemDeltaRoot *root_d, struct MemDelta *d,
                            LmnMembraneRef destmem, struct MemDelta *d2,
                            LmnMembraneRef srcmem, ProcessTableRef atoms) {
  LmnMembraneRef m;
  unsigned int i;
  LmnWord t;
  LmnSymbolAtomRef srcatom;

  /* binstr_comparef("copy cells :"); lmn_dump_mem_dev(srcmem); */
  /* dmem_root_dump(root_d); */

  /* copy child mems */
  DMEM_ALL_MEMS(d2, srcmem, m, {
    /* binstr_comparef("child mem = %p\n", m); */
    LmnMembraneRef new_mem = dmem_root_new_mem(root_d);
    /* 子膜はマッチングに使われないので、そのままコピーしていいはず */
    if (dmem_root_is_delta_mem(root_d, m)) {
      dmem_copy_cells(root_d, NULL, new_mem, dmem_root_get_mem_delta(root_d, m),
                      m, atoms);
    } else
      dmem_copy_cells(root_d, NULL, new_mem, NULL, m, atoms);

    if (d)
      dmem_add_child_mem(d, destmem, new_mem);
    else
      dmem_root_add_child_mem(root_d, destmem, new_mem);

    proc_tbl_put_mem(atoms, m, (LmnWord)new_mem);
    /* copy name */
    new_mem->set_name(m->NAME_ID());
    /* copy rulesets */
    for (i = 0; i < vec_num(m->get_rulesets()); i++) {
      if (d)
        dmem_add_ruleset(d, new_mem,
                         (LmnRuleSetRef)vec_get(m->get_rulesets(), i));
      else
        lmn_mem_add_ruleset(new_mem,
                            (LmnRuleSetRef)vec_get(m->get_rulesets(), i));
    }
  });

  /* copy atoms */
  DMEM_ALL_ATOMS(
      d2, srcmem, srcatom, ({
        LmnFunctor f;
        LmnSymbolAtomRef newatom;
        unsigned int start, end;
        srcatom = dmem_root_modified_atom(
            root_d,
            srcatom); /* プロキシ操作命令等で既にmodifyされている場合のために */

        /* すでにコピー済みなら次の候補へ */
        if (proc_tbl_get_by_atom(atoms, srcatom, NULL))
          continue;

        f = LMN_SATOM_GET_FUNCTOR(srcatom);
        newatom = dmem_root_copy_satom_with_data(root_d, srcatom);
        if (d)
          dmem_put_symbol_atom(d, destmem, newatom);
        else
          mem_push_symbol_atom(destmem, newatom);
        proc_tbl_put_atom(atoms, srcatom, (LmnWord)newatom);
        start = 0;
        end = LMN_SATOM_GET_ARITY(srcatom);

        if (LMN_IS_PROXY_FUNCTOR(f)) {
          start = 1, end = 2;
          LMN_PROXY_SET_MEM(newatom, destmem);

          /* oproxyの場合はコピー済みの内側にあるiproxyと接続させる必要がある（starは未検証？）
           */
          if (f == LMN_OUT_PROXY_FUNCTOR) {
            LmnSymbolAtomRef srcinside;
            LmnSymbolAtomRef newinside;

            srcinside =
                (LmnSymbolAtomRef)(dmem_root_get_link(root_d, srcatom, 0));
            if (proc_tbl_get_by_atom(atoms, srcinside, &t)) {

              newinside = (LmnSymbolAtomRef)(t);

              /* 必ず子膜につながっているはず */
              LMN_ASSERT(LMN_SATOM_GET_FUNCTOR(srcinside) ==
                             LMN_IN_PROXY_FUNCTOR &&
                         LMN_PROXY_GET_MEM(srcinside)->parent ==
                             LMN_PROXY_GET_MEM(srcatom));
              lmn_newlink_in_symbols(newatom, 0, newinside, 0);
            }
          }
        }

        /* リンク先と接続 */
        for (i = start; i < end; i++) {
          LmnAtomRef a;
          LmnLinkAttr attr;

          attr = LMN_SATOM_GET_ATTR(srcatom, i);
          a = LMN_SATOM_GET_LINK(srcatom, i);
          if (LMN_ATTR_IS_DATA(attr)) {
            if (d)
              dmem_put_atom(d, destmem, a, attr);
            else
              lmn_mem_push_atom(destmem, a, attr);
          } else if (proc_tbl_get_by_atom(atoms, (LmnSymbolAtomRef)(a), &t)) {
            lmn_newlink_in_symbols(newatom, i, (LmnSymbolAtomRef)(t),
                                   LMN_ATTR_MAKE_LINK(attr));
            /* LMN_SATOM_SET_LINK(newatom, i, t); */
            /* LMN_SATOM_SET_ATTR(newatom, i, attr); */
          }
        }
      }));

  /* copy activated flag */
  /* destmem->is_activated = srcmem->is_activated; /\* MC *\/ */
}

/* 移動膜と移動元の外側にあるプロキシ同士を接続させる */
static void modify_free_link(struct MemDeltaRoot *root_d, LmnMembraneRef m) {
  LmnSymbolAtomRef in;
  struct MemDelta *d;
  if (dmem_root_is_new_mem(root_d, m))
    d = NULL;
  else
    d = dmem_root_get_mem_delta(root_d, m);

  DMEM_EACH_FUNC_ATOM(d, m, LMN_IN_PROXY_FUNCTOR, in,
                      { modify_free_link_sub(root_d, m, in); });
  DMEM_EACH_FUNC_ATOM(d, m, LMN_STAR_PROXY_FUNCTOR, in,
                      { modify_free_link_sub(root_d, m, in); });
}

static void modify_free_link_sub(struct MemDeltaRoot *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef in) {
  LmnSymbolAtomRef out;
  out = (LmnSymbolAtomRef)(dmem_root_get_link(d, in, 0));

  if ((LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(out, 0)) != in) {
    if (dmem_root_is_new_mem(d, LMN_PROXY_GET_MEM(out))) {
      LMN_SATOM_SET_LINK(out, 0, in);
    } else {
      dmem_modify_link(dmem_root_get_mem_delta(d, LMN_PROXY_GET_MEM(out)),
                       LMN_PROXY_GET_MEM(out), out, 0, in,
                       LMN_SATOM_GET_ATTR(out, 0));
    }
  }
}

inline void dmem_root_remove_symbol_atom_with_buddy_data(
    struct MemDeltaRoot *d, LmnMembraneRef m, LmnSymbolAtomRef atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      if (dmem_root_is_new_mem(d, m))
        lmn_mem_remove_data_atom(m, (LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                                 LMN_SATOM_GET_ATTR(atom, i));
      else
        dmem_root_get_mem_delta(d, m)->data_atom_diff--;
    }
  }

  if (dmem_root_is_new_mem(d, m)) {
    mem_remove_symbol_atom(m, atom);
  } else {
    dmem_remove_symbol_atom(dmem_root_get_mem_delta(d, m), m, atom);
  }
}

int dmem_root_remove_symbol_atom_with_buddy_data_new_f(LmnWord _k, LmnWord _v,
                                                       LmnWord _arg) {
  LmnMembraneRef m;
  LmnSymbolAtomRef atom;
  unsigned int i;
  unsigned int end;

  m = (LmnMembraneRef)_arg;
  atom = (LmnSymbolAtomRef)_v;

  end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      lmn_mem_remove_data_atom(m, (LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                               LMN_SATOM_GET_ATTR(atom, i));
    }
  }

  mem_remove_symbol_atom(m, atom);
  return 1;
}

int dmem_root_remove_symbol_atom_with_buddy_data_dmem_f(LmnWord _k, LmnWord _v,
                                                        LmnWord _arg) {
  struct MemDelta *d;
  LmnSymbolAtomRef atom;
  unsigned int i;
  unsigned int end;

  d = (struct MemDelta *)_arg;
  atom = (LmnSymbolAtomRef)_v;

  end = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      d->data_atom_diff--;
    }
  }

  //  dmem_remove_symbol_atom(d, m, atom);
  vec_push(&d->del_atoms, (vec_data_t)atom);
  proc_tbl_put_atom(d->root_d->owner_tbl, atom, 0);
  return 1;
}

void dmem_root_remove_ground(struct MemDeltaRoot *root_d, LmnMembraneRef mem,
                             Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);

  /* memは既存膜のはず */
  proc_tbl_foreach(atoms, dmem_root_remove_symbol_atom_with_buddy_data_dmem_f,
                   (LmnWord)dmem_root_get_mem_delta(root_d, mem));

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    if (LMN_ATTR_IS_DATA(LinkObjGetPos(l))) {
      if (dmem_root_is_new_mem(root_d, mem))
        lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)LinkObjGetAtom(l),
                                 LinkObjGetPos(l));
      else
        dmem_root_get_mem_delta(root_d, mem)->data_atom_diff--;
    }
  }

  proc_tbl_free(atoms);
}

int dmem_root_free_satom_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  dmem_root_free_satom((struct MemDeltaRoot *)_arg, (LmnSymbolAtomRef)_v);
  return 1;
}

void dmem_root_free_ground(struct MemDeltaRoot *root_d, Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);

  proc_tbl_foreach(atoms, dmem_root_free_satom_f, (LmnWord)root_d);
  proc_tbl_free(atoms);
}

void dmem_root_copy_ground(struct MemDeltaRoot *root_d, LmnMembraneRef mem,
                           Vector *srcvec, Vector **ret_dstlovec,
                           ProcessTableRef *ret_atommap) {
  ProcessTableRef atommap;
  Vector stack;
  LmnWord t;
  struct MemDelta *d;
  unsigned int i;

  atommap = proc_tbl_make_with_size(64);
  t = 0UL; /* 警告されるので.. */
  atommap = proc_tbl_make_with_size(64);
  vec_init(&stack, 16);
  d = dmem_root_get_mem_delta(root_d, mem);
  *ret_dstlovec = vec_make(16);

  /* 根をスタックに積む。スタックにはリンクオブジェクトではなくアトムを積むため、
   * ここで根の先のアトムをコピーしスタックに積む必要がある */
  for (i = 0; i < vec_num(srcvec); i++) {
    LinkObjRef l = (LinkObjRef)vec_get(srcvec, i);
    LmnSymbolAtomRef cpatom;

    if (LMN_ATTR_IS_DATA(LinkObjGetPos(l))) {
      cpatom = (LmnSymbolAtomRef)lmn_copy_data_atom(
          (LmnDataAtomRef)LinkObjGetAtom(l), LinkObjGetPos(l));
      if (d) {
        dmem_put_atom(d, mem, cpatom, LinkObjGetPos(l));
      } else {
        lmn_mem_push_atom(mem, cpatom, LinkObjGetPos(l));
      }
    } else if (!proc_tbl_get_by_atom(atommap,
                                     (LmnSymbolAtomRef)LinkObjGetAtom(l), &t)) {
      /* symbol atom: コピー済みでなければコピーする */
      if (LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)LinkObjGetAtom(l)) ==
          LMN_UNIFY_FUNCTOR) {
        /* insertconnectorsinnullされた'='アトムは膜に所属しない */
        cpatom = dmem_root_copy_eqatom_with_data(
            (LmnSymbolAtomRef)LinkObjGetAtom(l));
      } else {
        cpatom = (LmnSymbolAtomRef)dmem_root_copy_satom_with_data(
            root_d, (LmnSymbolAtomRef)LinkObjGetAtom(l));
        if (d) {
          dmem_put_symbol_atom(d, mem, cpatom);
        } else {
          mem_push_symbol_atom(mem, cpatom);
        }
      }

      proc_tbl_put_atom(atommap, (LmnSymbolAtomRef)(LinkObjGetAtom(l)),
                        (LmnWord)cpatom);
      /* 根のリンクのリンクポインタを0に設定する */
      LMN_SATOM_SET_LINK(cpatom, LinkObjGetPos(l), 0);
      vec_push(&stack, (LmnWord)LinkObjGetAtom(l));
    } else {
      /* コピー済みの場合はスタックには追加しない */
      cpatom = (LmnSymbolAtomRef)t;
      LMN_SATOM_SET_LINK(cpatom, LinkObjGetPos(l), 0);
    }
    vec_push(*ret_dstlovec, (LmnWord)LinkObj_make(cpatom, LinkObjGetPos(l)));
  }

  while (vec_num(&stack) > 0) {
    LmnSymbolAtomRef src_atom = (LmnSymbolAtomRef)vec_pop(&stack);
    LmnSymbolAtomRef copied;

    proc_tbl_get_by_atom(atommap, src_atom, &t);
    copied = (LmnSymbolAtomRef)t;

    for (i = 0; i < LMN_SATOM_GET_ARITY(src_atom); i++) {
      LmnSymbolAtomRef next_src =
          (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(src_atom, i);
      LmnLinkAttr next_attr = LMN_SATOM_GET_ATTR(src_atom, i);

      /* LMN_SATOM_GET_LINK(copied, i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        if (d) {
          dmem_put_atom(d, mem, next_src, next_attr);
        } else {
          lmn_mem_push_atom(mem, next_src, next_attr);
        }
      } else if (LMN_SATOM_GET_LINK(copied, i) != 0) {
        LmnSymbolAtomRef next_copied;
        if (proc_tbl_get_by_atom(atommap, next_src, &t)) {
          next_copied = (LmnSymbolAtomRef)t;
        } else { /* next_srcは未訪問 */
          next_copied = dmem_root_copy_satom_with_data(root_d, next_src);
          if (d) {
            dmem_put_symbol_atom(d, mem, next_copied);
          } else {
            mem_push_symbol_atom(mem, next_copied);
          }
          proc_tbl_put_atom(atommap, next_src, (LmnWord)next_copied);
          vec_push(&stack, (LmnWord)next_src);
        }
        LMN_SATOM_SET_LINK(copied, i, next_copied);
      }
    }
  }

  vec_destroy(&stack);
  *ret_atommap = atommap;
}

void dmem_root_commit(struct MemDeltaRoot *d) {
  int i, j;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__DMEM_COMMIT);
  }
#endif

#ifdef DEBUG
  if (lmn_env.debug_delta) {
    printf("before commit : ");
    lmn_dump_mem_dev(d->root_mem);
    //    printf("before commit %s %p: ",
    //    lmn_id_to_name(d->applied_rule->name), d->root_mem);
    //    lmn_dump_cell_stdout(d->root_mem); printf("before commit : ");
    //    lmn_dump_cell_stdout(d->root_mem);
  }
#endif

  for (i = 0; i < vec_num(&d->mem_deltas); i++) {
    dmem_commit_delete_mem((struct MemDelta *)vec_get(&d->mem_deltas, i));
  }

  for (i = 0; i < vec_num(&d->mem_deltas); i++) {
    dmem_commit((struct MemDelta *)vec_get(&d->mem_deltas, i));
  }

  for (i = 0; i < vec_num(&d->new_mems); i++) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)vec_get(&d->new_mems, i);
    if (proc_tbl_get_by_mem(d->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      for (j = 0; j < vec_num(&new_mem_info->removed_child_mems); j++) {
        lmn_mem_remove_mem(
            mem, (LmnMembraneRef)vec_get(&new_mem_info->removed_child_mems, j));
      }
      for (j = 0; j < vec_num(&new_mem_info->new_child_mems); j++) {
        mem->add_child_mem((LmnMembraneRef)vec_get(&new_mem_info->new_child_mems, j));
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  for (i = 0; i < vec_num(&d->modified_atoms); i += 2) {
    LmnSymbolAtomRef src = (LmnSymbolAtomRef)(vec_get(&d->modified_atoms, i));
    LmnSymbolAtomRef atm =
        (LmnSymbolAtomRef)(vec_get(&d->modified_atoms, i + 1));

    if (!dmem_root_is_freed_atom(d, atm)) {
      dmem_root_commit_atom(d, src, atm);
    }
  }

  if (d->applied_history != ANONYMOUS) {
    d->applied_rule->add_history(d->applied_history);
  }

  d->committed = TRUE;

#ifdef DEBUG
  if (lmn_env.debug_delta) {
    printf("after  commit : ");
    lmn_dump_mem_dev(d->root_mem);
    //    printf("after commit %p : ", d->root_mem);
    //    lmn_dump_cell_stdout(d->root_mem); printf("after commit : ");
    //    lmn_dump_cell_stdout(d->root_mem);
  }
#endif
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__DMEM_COMMIT);
  }
#endif
}

static inline void dmem_root_commit_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef src,
                                         LmnSymbolAtomRef atm) {
  int i;
  int arity = LMN_SATOM_GET_LINK_NUM(src);

  /* printf("commit atom %s %p -> %p\n", LMN_SATOM_STR(src), src, atm); */
  /* printf("%s(", LMN_SATOM_STR(atm)); */
  /* for (i = 0; i < arity; i++) { */
  /*   if (i>0) printf(","); */
  /*   printf("%p", LMN_SATOM(LMN_SATOM_GET_LINK(atm, i))); */
  /* } */
  /* printf(")\n"); */
  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr = LMN_SATOM_GET_ATTR(atm, i);
    if (!LMN_ATTR_IS_DATA(attr)) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d, (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(atm, i)));
      if (LMN_SATOM_GET_LINK(a, LMN_ATTR_GET_VALUE(attr)) != atm) {
        /* printf("  modify link %p %d -> %p\n", a, LMN_ATTR_GET_VALUE(attr),
         * atm); */
        LMN_SATOM_SET_LINK(a, LMN_ATTR_GET_VALUE(attr), atm);
      }
    }
  }
}

void dmem_root_revert(struct MemDeltaRoot *d) {
  int i, j;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__DMEM_REVERT);
  }
#endif
#ifdef DEBUG
  if (lmn_env.debug_delta) {
    //    printf("before revert : %p", d->root_mem);
    //    lmn_dump_cell_stdout(d->root_mem);
    printf("before revert : ");
    lmn_dump_cell_stdout(d->root_mem);
  }
#endif
  for (i = vec_num(&d->new_mems) - 1; i >= 0; i--) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)vec_get(&d->new_mems, i);
    if (proc_tbl_get_by_mem(d->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      for (j = vec_num(&new_mem_info->new_child_mems) - 1; j >= 0; j--) {
        lmn_mem_remove_mem(
            mem, (LmnMembraneRef)vec_get(&new_mem_info->new_child_mems, j));
      }
      for (j = vec_num(&new_mem_info->removed_child_mems) - 1; j >= 0; j--) {
        mem->add_child_mem((LmnMembraneRef)vec_get(&new_mem_info->removed_child_mems, j));
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  for (i = vec_num(&d->mem_deltas) - 1; i >= 0; i--) {
    dmem_revert_new_mem((struct MemDelta *)vec_get(&d->mem_deltas, i));
  }

  for (i = vec_num(&d->mem_deltas) - 1; i >= 0; i--) {
    dmem_revert((struct MemDelta *)vec_get(&d->mem_deltas, i));
  }

  for (i = vec_num(&d->modified_atoms) - 1; i >= 0; i -= 2) {
    LmnSymbolAtomRef src =
        (LmnSymbolAtomRef)(vec_get(&d->modified_atoms, i - 1));
    LmnSymbolAtomRef atm = (LmnSymbolAtomRef)(vec_get(&d->modified_atoms, i));

    if (!dmem_root_is_freed_atom(d, atm)) {
      dmem_root_revert_atom(d, src, atm);
    }
  }

  /* 履歴除去.
   * uniqルールは, deep copyされたルールオブジェクトなはずなので,
   * 直接メンバ変数を書き換えてしまってもMT-safeなはず.これでとりあえずuniqも動くようになるはず.
   * TODO: dmem_copyrulesでuniq rulesetを複製(deep copy)するコードが必要. */
  if (d->applied_history != ANONYMOUS) {
    d->applied_rule->delete_history(d->applied_history);
  }

  d->committed = FALSE;
#ifdef DEBUG
  if (lmn_env.debug_delta) {
    printf("after  revert : ");
    lmn_dump_cell_stdout(d->root_mem);
  }
#endif
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__DMEM_REVERT);
  }
#endif
}

static inline void dmem_root_revert_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef src,
                                         LmnSymbolAtomRef atom) {
  int i;
  int arity = LMN_SATOM_GET_LINK_NUM(src);

  /* printf("revert atom: %s %p -> %p\n", LMN_SATOM_STR(src), atom, src); */

  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr = LMN_SATOM_GET_ATTR(src, i);
    if (!LMN_ATTR_IS_DATA(attr)) {
      if (LMN_SATOM_GET_LINK((LmnSymbolAtomRef)LMN_SATOM_GET_LINK(src, i),
                             LMN_ATTR_GET_VALUE(attr)) != src) {
        LMN_SATOM_SET_LINK((LmnSymbolAtomRef)LMN_SATOM_GET_LINK(src, i),
                           LMN_ATTR_GET_VALUE(attr), src);
      }
    }
  }
}

void dmem_root_add_child_mem(struct MemDeltaRoot *d, LmnMembraneRef parent,
                             LmnMembraneRef child) {
  if (dmem_root_is_new_mem(d, child)) { /* 追加する方が新しい膜 */
    //lmn_mem_set_parent(child, parent);
    child->set_parent(parent);
  } else { /* そうじゃない場合 */
    dmem_root_get_mem_delta(d, child)->new_parent = parent;
  }
  if (dmem_root_is_new_mem(d, parent)) { /* 新しい膜 */
    vec_push(&dmem_root_get_new_mem_info(d, parent)->new_child_mems,
             (vec_data_t)child);
  } else { /* 既出の膜 */
    dmem_add_child_mem(dmem_root_get_mem_delta(d, parent), parent, child);
  }

  proc_tbl_put_mem(d->owner_tbl, child, (LmnWord)parent);
}

void dmem_root_remove_mem(struct MemDeltaRoot *root_d, LmnMembraneRef parent,
                          LmnMembraneRef child) {
  if (dmem_root_is_new_mem(root_d, child)) {
    //lmn_mem_set_parent(child, NULL);
    child->set_parent(NULL);
  } else {
    dmem_root_get_mem_delta(root_d, child)->new_parent = NULL;
  }

  if (dmem_root_is_new_mem(root_d, parent)) {
    vec_push(&dmem_root_get_new_mem_info(root_d, parent)->removed_child_mems,
             (vec_data_t)child);
  } else {
    struct MemDelta *d = dmem_root_get_mem_delta(root_d, parent);

#ifdef DEBUG
    if (dmem_root_is_new_mem(root_d, child))
      lmn_fatal("unexpected");
    if (dmem_is_removed_mem(d, parent, child))
      lmn_fatal("unexpected");
#endif
    vec_push(&d->del_mems, (vec_data_t)child);
  }
  proc_tbl_put_mem(root_d->owner_tbl, child, 0);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void dmem_root_remove_toplevel_proxies(struct MemDeltaRoot *root_d,
                                       LmnMembraneRef mem) {
  if (dmem_root_is_new_mem(root_d, mem)) {
    lmn_mem_remove_toplevel_proxies(mem);
  } else {
    Vector remove_list;
    LmnSymbolAtomRef outside;
    unsigned int i;
    struct MemDelta *d;

    d = dmem_root_get_mem_delta(root_d, mem);
    /*   printf("before remove toplevel proxy "); lmn_dump_mem_stdout(mem); */

    vec_init(&remove_list, 16);

    DMEM_EACH_FUNC_ATOM(d, mem, LMN_OUT_PROXY_FUNCTOR, outside, {
      LmnSymbolAtomRef a0;
      a0 = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, outside, 0));
      if (LMN_PROXY_GET_MEM(a0) &&
          dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)) != mem) {
        if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(outside, 1))) {
          LmnSymbolAtomRef a1 =
              (LmnSymbolAtomRef)(dmem_root_get_link(root_d, outside, 1));
          if (LMN_SATOM_GET_FUNCTOR(a1) == LMN_OUT_PROXY_FUNCTOR) {
            LmnSymbolAtomRef a10 =
                (LmnSymbolAtomRef)(dmem_root_get_link(root_d, a1, 0));
            if (LMN_PROXY_GET_MEM(a10) &&
                dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a10)) != mem) {
              if (!vec_contains(&remove_list, (LmnWord)outside)) {
                dmem_unify_atom_args(d, mem, outside, 0, a1, 0);
                vec_push(&remove_list, (LmnWord)outside);
                vec_push(&remove_list, (LmnWord)a1);
              }
            }
          }
        }
      }
    });

    for (i = 0; i < remove_list.num; i++) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d->root_d, (LmnSymbolAtomRef)(vec_get(&remove_list, i)));
      dmem_remove_symbol_atom(d, mem, a);
      dmem_root_free_satom(root_d, a);
    }
    vec_destroy(&remove_list);

    /*   printf("after remove toplevel proxy "); lmn_dump_mem_stdout(mem); */
  }
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void dmem_root_remove_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem) {
  /* printf("before remove proxies : "); lmn_dump_cell_stdout(root_d->root_mem);
   */
  if (dmem_root_is_new_mem(root_d, mem)) {
    /* printf("remove proxies new mem\n"); */
    lmn_mem_remove_proxies(mem);
  } else {
    unsigned int i;
    Vector remove_list, change_list;
    LmnSymbolAtomRef opxy;
    LmnSymbolAtomRef a;
    struct MemDelta *d;

    d = dmem_root_get_mem_delta(root_d, mem);

    /* printf("before remove proxy "); lmn_dump_mem_stdout(root_d->root_mem); */
    /* printf("remove proxies mem = %p\n", mem); */
    /* lmn_dump_mem_dev(mem); */

    vec_init(&remove_list, 16);
    vec_init(&change_list, 16);

    DMEM_EACH_FUNC_ATOM(d, mem, LMN_OUT_PROXY_FUNCTOR, opxy, {
      LmnSymbolAtomRef a0 =
          (LmnSymbolAtomRef)(dmem_root_get_link(root_d, opxy, 0));
      /* printf("a0 = %s %p, parent = %p, mem = %p\n", LMN_SATOM_STR(a0), a0,
       * dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)), mem); */
      if (dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)) !=
              mem && /* opxyのリンク先が子膜でない場合 */
          !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(opxy, 1))) {
        /* if (LMN_PROXY_GET_MEM(a0)->parent != mem && /\*
         * opxyのリンク先が子膜でない場合 *\/ */
        /*     !LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(opxy, 1))) { */
        LmnSymbolAtomRef a1 =
            (LmnSymbolAtomRef)(dmem_root_get_link(root_d, opxy, 1));
        LmnFunctor f1 = LMN_SATOM_GET_FUNCTOR(a1);
        if (f1 == LMN_IN_PROXY_FUNCTOR) { /* (1) */
          /* printf("(1)\n"); */
          dmem_unify_atom_args(d, mem, opxy, 0, a1, 0);
          vec_push(&remove_list, (LmnWord)opxy);
          vec_push(&remove_list, (LmnWord)a1);
        } else {
          if (f1 == LMN_OUT_PROXY_FUNCTOR &&LMN_PROXY_GET_MEM((LmnSymbolAtomRef)dmem_root_get_link(root_d, a1, 0))->mem_parent() !=mem) { /* (3) */
            if (!vec_contains(&remove_list, (LmnWord)opxy)) {
              dmem_unify_atom_args(d, mem, opxy, 0, a1, 0);
              vec_push(&remove_list, (LmnWord)opxy);
              vec_push(&remove_list, (LmnWord)a1);
            }
          } else { /* (2) */
            /* printf("(2)\n"); */

            vec_push(&change_list, (LmnWord)opxy);
          }
        }
      }
    });

    for (i = 0; i < vec_num(&remove_list); i++) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d->root_d, (LmnSymbolAtomRef)(vec_get(&remove_list, i)));
      dmem_remove_symbol_atom(d, mem, a);
      dmem_root_free_satom(root_d, a);
    }
    vec_destroy(&remove_list);

    /* add inside proxy to change list */
    DMEM_EACH_FUNC_ATOM(d, mem, LMN_IN_PROXY_FUNCTOR, a, {
      /* clear mem attribute */
      vec_push(&change_list, (LmnWord)a);
    });

    { /* change to star proxy */
      for (i = 0; i < change_list.num; i++) {
        dmem_root_alter_functor(
            root_d, mem,
            dmem_root_modified_atom(
                root_d, (LmnSymbolAtomRef)(vec_get(&change_list, i))),
            LMN_STAR_PROXY_FUNCTOR);
      }
    }
    vec_destroy(&change_list);
    /*   printf("after remove proxy "); lmn_dump_mem_stdout(mem); */
    /* printf("after remove proxies : "); */
    /* lmn_dump_mem_dev(mem); */
  }
}

void dmem_root_insert_proxies(struct MemDeltaRoot *root_d, LmnMembraneRef mem,
                              LmnMembraneRef child_mem) {
  unsigned int i;
  Vector remove_list, change_list;
  LmnSymbolAtomRef star, oldstar;
  struct MemDelta *parent_d, *child_d;
  /* printf("before insert proxy "); lmn_dump_mem_stdout(root_d->root_mem); */

  if (dmem_root_is_new_mem(root_d, mem))
    parent_d = NULL;
  else
    parent_d = dmem_root_get_mem_delta(root_d, mem);
  if (dmem_root_is_new_mem(root_d, child_mem))
    child_d = NULL;
  else
    child_d = dmem_root_get_mem_delta(root_d, child_mem);

  /* printf("insert proxies parent = %p child_mem = %p(delta=%p)\n", mem,
   * child_mem, child_d); */

  vec_init(&remove_list, 16);
  vec_init(&change_list, 16); /* inside proxy にするアトム */

  DMEM_EACH_FUNC_ATOM(
      child_d, child_mem, LMN_STAR_PROXY_FUNCTOR, star, ({
        /* スタープロキシは新規アトムのはず */
        oldstar = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, star, 0));
        if (child_d)
          oldstar = dmem_modify_atom(child_d, child_mem, oldstar);
        if (LMN_PROXY_GET_MEM(oldstar) == child_mem) { /* (1) */
          if (!vec_contains(&remove_list, (LmnWord)star)) {
            if (child_d)
              dmem_unify_atom_args(child_d, child_mem, star, 1, oldstar, 1);
            else
              dmem_root_unify_atom_args(root_d, child_mem, star, 1, oldstar, 1);
            vec_push(&remove_list, (LmnWord)star);
            vec_push(&remove_list, (LmnWord)oldstar);
          }
        } else {
          vec_push(&change_list, (LmnWord)star);
          if (LMN_PROXY_GET_MEM(oldstar) == mem) { /* (2) */
            oldstar = dmem_root_alter_functor(root_d, mem, oldstar,
                                              LMN_OUT_PROXY_FUNCTOR);
            lmn_newlink_in_symbols(star, 0, oldstar, 0);
          } else { /* (3) */
            LmnSymbolAtomRef outside =
                dmem_root_new_atom(root_d, LMN_OUT_PROXY_FUNCTOR);
            LmnSymbolAtomRef newstar =
                dmem_root_new_atom(root_d, LMN_STAR_PROXY_FUNCTOR);
            dmem_root_push_atom(root_d, mem, outside, LMN_ATTR_MAKE_LINK(0));
            dmem_root_push_atom(root_d, mem, newstar, LMN_ATTR_MAKE_LINK(0));
            lmn_newlink_in_symbols(outside, 1, newstar, 1);
            if (parent_d)
              dmem_relink(parent_d, mem, newstar, 0, star, 0);
            else
              dmem_root_relink(root_d, mem, newstar, LMN_ATTR_MAKE_LINK(0), 0,
                               star, LMN_ATTR_MAKE_LINK(0), 0);
            lmn_newlink_in_symbols(star, 0, outside, 0);
          }
        }
      }));

  for (i = 0; i < vec_num(&change_list); i++) {
    /* printf("change list %p\n", LMN_SATOM(vec_get(&change_list, i))); */
    dmem_root_alter_functor(root_d, child_mem,
                            (LmnSymbolAtomRef)(vec_get(&change_list, i)),
                            LMN_IN_PROXY_FUNCTOR);
  }
  vec_destroy(&change_list);

  for (i = 0; i < vec_num(&remove_list); i++) {
    /* printf("insert proxy remove atom %s %p\n",
     * LMN_SATOM_STR(LMN_SATOM(vec_get(&remove_list, i))),
     * LMN_SATOM(vec_get(&remove_list, i))); */
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)(vec_get(&remove_list, i));
    if (parent_d)
      dmem_remove_symbol_atom(parent_d, child_mem, a);
    else
      mem_remove_symbol_atom(child_mem, a);
    dmem_root_free_satom(root_d, a);
    /*    lmn_delete_atom(LMN_SATOM(vec_get(&remove_list, i))); */
  }
  vec_destroy(&remove_list);
  /* printf("after insert proxy "); lmn_dump_mem_stdout(root_d->root_mem); */
  /* printf("end insert proxies\n"); */
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void dmem_root_remove_temporary_proxies(struct MemDeltaRoot *root_d,
                                        LmnMembraneRef mem) {
  unsigned int i;
  Vector remove_list;
  LmnSymbolAtomRef star, outside;
  struct MemDelta *d;

  if (dmem_root_is_new_mem(root_d, mem))
    d = NULL;
  else
    d = dmem_root_get_mem_delta(root_d, mem);

  vec_init(&remove_list, 16);

  DMEM_EACH_FUNC_ATOM(d, mem, LMN_STAR_PROXY_FUNCTOR, star, {
    outside = dmem_root_modified_atom(
        root_d, (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(star, 0)));
    if (!vec_contains(&remove_list, (LmnWord)star)) {
      if (d)
        dmem_unify_atom_args(d, mem, star, 1, outside, 1);
      else
        dmem_root_unify_atom_args(root_d, mem, star, 1, outside, 1);
      vec_push(&remove_list, (LmnWord)star);
      vec_push(&remove_list, (LmnWord)outside);
    }
  });
  for (i = 0; i < remove_list.num; i++) {
    LmnSymbolAtomRef a = dmem_root_modified_atom(
        root_d, (LmnSymbolAtomRef)(vec_get(&remove_list, i)));
    if (d)
      dmem_remove_symbol_atom(d, mem, a);
    else {
      mem_remove_symbol_atom(mem, a);
      dmem_root_free_satom(root_d, a);
    }
  }

  vec_destroy(&remove_list);
}

void dmem_root_clear_ruleset(struct MemDeltaRoot *d, LmnMembraneRef m) {
  if (dmem_root_is_new_mem(d, m)) {
    dmem_clear_ruleset(dmem_root_get_mem_delta(d, m), m);
  }
}

LmnMembraneRef dmem_root_new_mem(struct MemDeltaRoot *d) {
  LmnMembraneRef m;

  m = new LmnMembrane();
  proc_tbl_put_mem(d->proc_tbl, m, (LmnWord)new_mem_info_make(m));
  sproc_tbl_set_mem_flag(d->flag_tbl, m, TAG_NEW_MEM);
  vec_push(&d->new_mems, (vec_data_t)m);
  return m;
}

static struct NewMemInfo *dmem_root_get_new_mem_info(struct MemDeltaRoot *d,
                                                     LmnMembraneRef m) {
  LmnWord t = 0;

#ifdef DEBUG
  if (!dmem_root_is_new_mem(d, m))
    lmn_fatal("unexpected");
#endif
  proc_tbl_get_by_mem(d->proc_tbl, m, &t);
  return (struct NewMemInfo *)t;
}

static LmnSymbolAtomRef dmem_root_alter_functor(struct MemDeltaRoot *root_d,
                                                LmnMembraneRef mem,
                                                LmnSymbolAtomRef atom,
                                                LmnFunctor f) {
  if (dmem_root_is_new_mem(root_d, mem)) {
    alter_functor(mem, atom, f);
    return atom;
  } else {
    struct MemDelta *d = dmem_root_get_mem_delta(root_d, mem);

    if (dmem_root_is_new_atom(root_d, atom)) {
      if (f > d->max_functor)
        d->max_functor = f;
      LMN_SATOM_SET_FUNCTOR(atom, f);
      return atom;
    } else {
      LmnSymbolAtomRef new_atom = dmem_root_copy_satom(root_d, atom);
      LmnSymbolAtomRef a0;

#ifdef DEBUG
      if (LMN_PROXY_GET_MEM(atom) != mem)
        lmn_fatal("unexpected");
      if (dmem_root_is_modified_atom(root_d, atom))
        lmn_fatal("unexpected");
#endif
      LMN_SATOM_SET_FUNCTOR(new_atom, f);
      dmem_remove_symbol_atom(d, mem, atom);
      dmem_root_free_satom(root_d, atom);
      dmem_put_symbol_atom(d, mem, new_atom);

      a0 = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, atom, 0));
      if (LMN_PROXY_GET_MEM(a0) != mem) {
        LmnMembraneRef m2 = LMN_PROXY_GET_MEM(a0);
#ifdef DEBUG
        if (dmem_root_is_new_mem(root_d, m2))
          lmn_fatal("unexpected");
#endif
        a0 = dmem_modify_atom(dmem_root_get_mem_delta(root_d, m2), m2, a0);
      } else {
        a0 = dmem_modify_atom(d, mem,
                              (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(atom, 0)));
      }
      lmn_newlink_in_symbols(new_atom, 0, a0, 0);

      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 1))) {
        LmnSymbolAtomRef a1 = dmem_modify_atom(
            d, mem, (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(atom, 1)));
        lmn_newlink_in_symbols(new_atom, 1, a1,
                               LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(atom, 1)));
      }
      return new_atom;
    }
  }
}

LmnAtomRef dmem_root_get_link(struct MemDeltaRoot *d, LmnSymbolAtomRef atom,
                              int i) {
  LmnSymbolAtomRef a;

  atom = (LmnSymbolAtomRef)dmem_root_modified_atom(d, atom);
  a = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, (i));
  if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR((atom), (i))))
    return a;
  else
    return dmem_root_modified_atom(d, a);
}

void dmem_root_dump(struct MemDeltaRoot *d) {
  int i;

  for (i = 0; i < vec_num(&d->mem_deltas); i++) {
    dmem_dump((struct MemDelta *)vec_get(&d->mem_deltas, i));
  }
}

static void dmem_dump(struct MemDelta *d) {
  int i;

  /* if (!d) lmn_fatal("unexpected"); */

  /* for (i = 0; i < vec_num(&d->modified_atoms); i+=2) { */
  /*   LmnSymbolAtomRef src = LMN_SATOM(vec_get(&d->modified_atoms, i)); */
  /*   LmnSymbolAtomRef new = LMN_SATOM(vec_get(&d->modified_atoms, i+1)); */

  /*   printf("modify %s %p -> %p\n", LMN_SATOM_STR(src), src, new); */
  /* } */

  for (i = 0; i < vec_num(&d->new_atoms); i++) {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)(vec_get(&d->new_atoms, i));
    int j;
    int arity = LMN_SATOM_GET_LINK_NUM(a);

    printf("new atom mem= %p, %p ", d->mem, a);

    printf("%s(", LMN_SATOM_STR(a));
    for (j = 0; j < arity; j++) {
      if (j > 0)
        printf(",");
      printf("%p", (LmnSymbolAtomRef)(LMN_SATOM_GET_LINK(a, j)));
    }
    printf(")\n");
  }

  for (i = 0; i < vec_num(&d->del_atoms); i++) {
    printf("del atom %s %p\n",
           LMN_SATOM_STR((LmnSymbolAtomRef)(vec_get(&d->del_atoms, i))),
           (LmnSymbolAtomRef)(vec_get(&d->del_atoms, i)));
  }

  /* Membrane */

  for (i = 0; i < vec_num(&d->del_mems); i++) {
    printf("del mem: %p\n", (LmnMembraneRef)vec_get(&d->del_mems, i));
    lmn_dump_mem_dev((LmnMembraneRef)vec_get(&d->del_mems, i));
  }

  for (i = 0; i < vec_num(&d->new_mems); i++) {
    printf("new mem: %p\n", (LmnMembraneRef)vec_get(&d->new_mems, i));
    lmn_dump_mem_dev((LmnMembraneRef)vec_get(&d->new_mems, i));
  }
}

static LmnMembraneRef dmem_root_get_parent(struct MemDeltaRoot *root_d,
                                           LmnMembraneRef m) {
  if (dmem_root_is_new_mem(root_d, m))
    return m->mem_parent();
  else if (dmem_root_is_delta_mem(root_d, m)) {
    struct MemDelta *d = dmem_root_get_mem_delta(root_d, m);
    return d->new_parent;
  } else {
    return m->mem_parent();
  }
}

BOOL dmem_root_is_committed(struct MemDeltaRoot *root_d) {
  return root_d->committed;
}

void dmem_root_drop(struct MemDeltaRoot *root_d, LmnMembraneRef m) {
  if (dmem_root_is_new_mem(root_d, m))
    lmn_mem_drop(m);
  else
    dmem_drop(dmem_root_get_mem_delta(root_d, m), m);
}

void dmem_root_set_mem_name(struct MemDeltaRoot *root_d, LmnMembraneRef m,
                            lmn_interned_str name) {
  if (dmem_root_is_new_mem(root_d, m))
    m->set_name(name);
  else {
    dmem_root_get_mem_delta(root_d, m)->new_name = name;
  }
}

void dmem_root_copy_rules(struct MemDeltaRoot *root_d, LmnMembraneRef dest,
                          LmnMembraneRef src) {
  if (dmem_root_is_new_mem(root_d, dest))
    lmn_mem_copy_rules(dest, src);
  else {
    dmem_copy_rules(dmem_root_get_mem_delta(root_d, dest), dest, src);
  }
}

LmnSymbolAtomRef dmem_root_new_atom(struct MemDeltaRoot *d, LmnFunctor f) {
  LmnSymbolAtomRef atom = lmn_new_atom(f);

  if (LMN_SATOM_ID(atom) == 0)
    LMN_SATOM_SET_ID(atom, env_gen_next_id());

  sproc_tbl_set_atom_flag(d->flag_tbl, atom, TAG_NEW_ATOM);
  return atom;
}

static inline LmnSymbolAtomRef dmem_root_copy_satom(struct MemDeltaRoot *d,
                                                    LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_atom;

  new_atom = lmn_copy_satom(atom);

  if (LMN_SATOM_ID(atom) == 0)
    LMN_SATOM_SET_ID(new_atom, env_gen_next_id());

  sproc_tbl_set_atom_flag(d->flag_tbl, new_atom, TAG_NEW_ATOM);

  return new_atom;
}

static inline LmnSymbolAtomRef
dmem_root_copy_satom_with_data(struct MemDeltaRoot *d, LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_atom = dmem_root_copy_satom(d, atom);
  unsigned int i, arity = LMN_SATOM_GET_LINK_NUM(atom);

  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      LMN_SATOM_SET_LINK(new_atom, i,
                         (LmnAtomRef)lmn_copy_data_atom(
                             (LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                             LMN_SATOM_GET_ATTR(atom, i)));
    }
  }
  return new_atom;
}

/* '='アトムのコピー用。ルール適用中に消されるので差分に含めないようにする */
static inline LmnSymbolAtomRef
dmem_root_copy_eqatom_with_data(LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_eqatom;
  unsigned int i, arity;

  new_eqatom = lmn_copy_satom(atom);
  LMN_SATOM_SET_ID(new_eqatom, env_gen_next_id());

  /* リンク先のデータアトムをコピーする */
  arity = LMN_SATOM_GET_LINK_NUM(atom);
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, i))) {
      LMN_SATOM_SET_LINK(new_eqatom, i,
                         (LmnAtomRef)lmn_copy_data_atom(
                             (LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, i),
                             LMN_SATOM_GET_ATTR(atom, i)));
    }
  }
  return new_eqatom;
}

LmnAtomRef dmem_root_copy_atom(struct MemDeltaRoot *d, LmnAtomRef atom,
                               LmnLinkAttr attr) {
  LmnAtomRef new_atom;
  if (LMN_ATTR_IS_DATA(attr)) {
    return (LmnAtomRef)lmn_copy_data_atom((LmnDataAtomRef)atom, attr);
  } else { /* symbol atom */
    new_atom = lmn_copy_satom((LmnSymbolAtomRef)atom);
    LMN_SATOM_SET_ID((LmnSymbolAtomRef)new_atom, env_gen_next_id());
    sproc_tbl_set_atom_flag(d->flag_tbl, (LmnSymbolAtomRef)new_atom,
                            TAG_NEW_ATOM);
    return new_atom;
  }
}

static inline void dmem_root_free_satom(struct MemDeltaRoot *d,
                                        LmnSymbolAtomRef atom) {
  sproc_tbl_set_atom_flag(d->flag_tbl, dmem_root_modified_atom(d, atom),
                          TAG_DEL_ATOM);
}

void dmem_root_free_atom(struct MemDeltaRoot *d, LmnAtomRef atom,
                         LmnLinkAttr attr) {
  if (!LMN_ATTR_IS_DATA(attr)) {
    dmem_root_free_satom(d, (LmnSymbolAtomRef)(atom));
  }
}

static inline BOOL dmem_root_is_freed_atom(struct MemDeltaRoot *d,
                                           LmnSymbolAtomRef a) {
  return sproc_tbl_get_flag_by_atom(d->flag_tbl, a, TAG_DEL_ATOM);
}

static inline LmnSymbolAtomRef dmem_root_modified_atom(struct MemDeltaRoot *d,
                                                       LmnSymbolAtomRef a) {
  LmnWord t;

  if (d && proc_tbl_get_by_atom(d->proc_tbl, a, &t)) {
    return (LmnSymbolAtomRef)(t);
  } else {
    return a;
  }
}

static inline BOOL dmem_root_is_new_atom(struct MemDeltaRoot *d,
                                         LmnSymbolAtomRef a) {
  return LMN_SATOM_ID(a) >= d->new_proc_id_lower_limit;
}

static inline BOOL dmem_root_is_modified_atom(struct MemDeltaRoot *d,
                                              LmnSymbolAtomRef a) {
  return sproc_tbl_get_flag_by_atom(d->flag_tbl, a, TAG_MODIFIED_ATOM);
}

static inline LmnMembraneRef dmem_root_atom_mem(struct MemDeltaRoot *d,
                                                LmnSymbolAtomRef a) {
  LmnWord t;

  return proc_tbl_get_by_atom(d->owner_tbl, a, &t) ? (LmnMembraneRef)t : NULL;
}

void dmem_root_finish(struct MemDeltaRoot *d) { d->next_id = env_next_id(); }

/* ---------------------------------------------------------------------- */

static struct MemDelta *mem_delta_make(struct MemDeltaRoot *root_d,
                                       LmnMembraneRef m,
                                       unsigned long next_id) {
  struct MemDelta *p = LMN_MALLOC(struct MemDelta);

  p->root_d = root_d;
  p->mem = m;

  /* TODO:
   * 配列をメモリ上に4本確保しているため,
   * キャッシュ効率を考慮した改良ができるはず. */

  vec_init(&p->new_atoms, 16);
  vec_init(&p->del_atoms, 16);

  vec_init(&p->del_mems, 16);
  vec_init(&p->new_mems, 16);

  p->new_rulesets = NULL;
  p->org_rulesets = NULL;
  p->ruleset_removed = FALSE;

  vec_init(&p->new_proxies, 16);

  p->max_functor = m->mem_max_functor();

  p->data_atom_diff = 0;

  p->new_parent = m->mem_parent();

  p->org_name = p->new_name = m->NAME_ID();
  return p;
}

static void mem_delta_free(struct MemDelta *p) {
  int i;

  vec_destroy(&p->del_mems);
  vec_destroy(&p->new_mems);
  vec_destroy(&p->del_atoms);

  for (i = 0; i < vec_num(&p->new_atoms); i++) {
    free_symbol_atom_with_buddy_data(
        (LmnSymbolAtomRef)(vec_get(&p->new_atoms, i)));
  }
  vec_destroy(&p->new_atoms);

  if (p->new_rulesets)
    vec_free(p->new_rulesets);

  vec_destroy(&p->new_proxies);

  LMN_FREE(p);
}

LmnMembraneRef dmem_mem(struct MemDelta *d) { return d->mem; }

static inline void dmem_remove_symbol_atom(struct MemDelta *d, LmnMembraneRef m,
                                           LmnSymbolAtomRef atom) {
#ifdef DEBUG
  if (dmem_root_is_modified_atom(d->root_d, atom))
    lmn_fatal("unexpected");
  if (dmem_is_removed_atom(d, m, atom))
    lmn_fatal("unexpected");
#endif
  /* if (dmem_is_new_atom(d, m, atom)) { */
  /*   sproc_tbl_unset_atom_flag(d->flag_tbl, atom, TAG_NEW_ATOM); */
  /* } else { */
  /*   sproc_tbl_set_atom_flag(d->flag_tbl, atom, TAG_DEL_ATOM); */
  /* } */

  vec_push(&d->del_atoms, (vec_data_t)atom);
  proc_tbl_put_atom(d->root_d->owner_tbl, atom, 0);
}

static inline void dmem_remove_atom(struct MemDelta *d, LmnMembraneRef m,
                                    LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    d->data_atom_diff--;
  } else {
    dmem_remove_symbol_atom(d, m, (LmnSymbolAtomRef)(atom));
  }
}

/* BODY命令で, 既に存在していた膜mにアトムatomを追加する場合呼ぶ */
static inline void dmem_put_atom(struct MemDelta *d, LmnMembraneRef m,
                                 LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) { /* データアトムの場合は, 参照数を増やすのみ */
    d->data_atom_diff++;
  } else {
    dmem_put_symbol_atom(d, m, (LmnSymbolAtomRef)(atom));
  }
}

static inline BOOL dmem_is_new_atom(struct MemDelta *d, LmnMembraneRef m,
                                    LmnSymbolAtomRef a) {
  LmnWord t;
  return proc_tbl_get_by_atom(d->root_d->owner_tbl, a, &t) &&
         (LmnMembraneRef)t == m;
}

static inline LmnSymbolAtomRef
dmem_modify_atom(struct MemDelta *d, LmnMembraneRef mem, LmnSymbolAtomRef src) {
  LmnSymbolAtomRef new_atom;

  if (dmem_root_is_modified_atom(d->root_d, src)) {
    /* srcのリンク先が既に変わっている場合 */
    return dmem_root_modified_atom(d->root_d, src);
  } else if (dmem_root_is_new_atom(d->root_d, src)) {
    /* srcが新規作成アトムである場合 */
    return src;
  } else {
    /* srcは既存アトムだが、まだリンク先が変わっていない場合 */
    /* revert作業のために、srcを残す必要があるのでnew_atomを作成してnew_atomのリンク先が変更されるようにする
     */
    new_atom = dmem_root_copy_satom(d->root_d, src);

    if (dmem_is_removed_atom(d, mem, src)) {
      dmem_put_symbol_atom(d, mem, new_atom);
      dmem_remove_symbol_atom(d, mem, new_atom);
    } else {
      dmem_remove_symbol_atom(d, mem, src);
      dmem_root_free_satom(d->root_d, src);
      dmem_put_symbol_atom(d, mem, new_atom);
    }

    /* new_atomとsrcの対応をproc_tblに保存しておく */
    proc_tbl_put_atom(d->root_d->proc_tbl, src, LMN_ATOM(new_atom));
    sproc_tbl_set_atom_flag(d->root_d->flag_tbl, src, TAG_MODIFIED_ATOM);

    vec_push(&d->root_d->modified_atoms, (vec_data_t)src);
    vec_push(&d->root_d->modified_atoms, (vec_data_t)new_atom);

    return new_atom;
  }
}

/* atomのリンク先がlに変わった */
static inline void dmem_modify_link(struct MemDelta *d, LmnMembraneRef m,
                                    LmnSymbolAtomRef atom, int i, LmnAtomRef l,
                                    LmnLinkAttr attr) {
  LmnSymbolAtomRef modified;

  modified = dmem_modify_atom(d, m, atom);

  LMN_SATOM_SET_LINK(modified, i, l);
  LMN_SATOM_SET_ATTR(modified, i, attr);
}

static inline BOOL dmem_is_removed_atom(struct MemDelta *d, LmnMembraneRef m,
                                        LmnSymbolAtomRef a) {
  LmnWord t;
  return d && proc_tbl_get_by_atom(d->root_d->owner_tbl, a, &t) &&
         (LmnMembraneRef)t != m;
}

static inline BOOL dmem_is_removed_mem(struct MemDelta *d,
                                       LmnMembraneRef parent,
                                       LmnMembraneRef child) {
  LmnWord t;
  return proc_tbl_get_by_mem(d->root_d->owner_tbl, child, &t) &&
         (LmnMembraneRef)t != parent;
}

/* BODY命令で,
 * 既に存在していた膜mへシンボルアトムatomが追加される情報を差分データdへ追加する
 */
static inline void dmem_put_symbol_atom(struct MemDelta *d, LmnMembraneRef m,
                                        LmnSymbolAtomRef atom) {
  LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);

  /* アトムへIDを付加する */
  if (LMN_SATOM_ID(atom) == 0) {
    LMN_SATOM_SET_ID(atom, env_gen_next_id());
  }

  if (f > d->max_functor)
    d->max_functor = f;
#ifdef DEBUG
  if (dmem_is_removed_atom(d, m, atom))
    lmn_fatal("unexpected");
  if (!dmem_root_is_new_atom(d->root_d, atom))
    lmn_fatal("unexpected");
#endif
  vec_push(&d->new_atoms, (vec_data_t)atom);
  proc_tbl_put_atom(d->root_d->owner_tbl, atom,
                    (LmnWord)m); /* IDをkeyにした配列へ投げる */
  /* sproc_tbl_unset_atom_flag(d->flag_tbl, atom, TAG_DEL_ATOM); */

  if (LMN_IS_PROXY_FUNCTOR(f)) { /* 更にproxyの場合は, 情報を追加 */
    LMN_PROXY_SET_MEM(atom, m);
    vec_push(&d->new_proxies, (vec_data_t)atom);
  }
}

static void dmem_add_child_mem(struct MemDelta *d, LmnMembraneRef parent,
                               LmnMembraneRef child) {
  vec_push(&d->new_mems, (vec_data_t)child);
}

static void dmem_add_ruleset(struct MemDelta *d, LmnMembraneRef m,
                             LmnRuleSetRef ruleset) {
  if (!d->new_rulesets) {
    d->new_rulesets = vec_make(16);
  }
  vec_push(d->new_rulesets, (vec_data_t)ruleset);
}

static inline void dmem_clear_ruleset(struct MemDelta *d, LmnMembraneRef m) {
  d->ruleset_removed = TRUE;
}

static void dmem_link_data_atoms(struct MemDelta *d, LmnMembraneRef m,
                                 LmnDataAtomRef d0, LmnLinkAttr attr0,
                                 LmnDataAtomRef d1, LmnLinkAttr attr1) {
  LmnSymbolAtomRef ap = dmem_root_new_atom(d->root_d, LMN_UNIFY_FUNCTOR);
  LMN_SATOM_SET_LINK(ap, 0, (LmnAtomRef)d0);
  LMN_SATOM_SET_LINK(ap, 1, (LmnAtomRef)d1);
  LMN_SATOM_SET_ATTR(ap, 0, attr0);
  LMN_SATOM_SET_ATTR(ap, 1, attr1);
  dmem_put_symbol_atom(d, m, ap);
}

static void dmem_drop(struct MemDelta *d, LmnMembraneRef mem) {
  LmnMembraneRef m;
  int i;
  LmnSymbolAtomRef a;

  /* drop and free child mems */
  m = mem->mem_child_head();
  DMEM_ALL_MEMS(d, mem, m, { dmem_root_remove_mem(d->root_d, mem, m); });
  /* mem->child_head = NULL; */

  /* free all atoms */
  DMEM_ALL_ATOMS(d, mem, a, ({
                   for (i = 0; i < LMN_SATOM_GET_LINK_NUM(a); i++) {
                     if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(a, i))) {
                       dmem_remove_atom(d, mem, LMN_SATOM_GET_LINK(a, i),
                                        LMN_SATOM_GET_ATTR(a, i));
                       dmem_root_free_atom(d->root_d, LMN_SATOM_GET_LINK(a, i),
                                           LMN_SATOM_GET_ATTR(a, i));
                     }
                   }
                   dmem_remove_symbol_atom(d, mem, a);
                   dmem_root_free_satom(d->root_d, a);
                 }));
}

/* atom1は新規作成アトム、atom2はもともと接続していたアトム */
static inline void dmem_relink(struct MemDelta *d, LmnMembraneRef m,
                               LmnSymbolAtomRef atom1, int pos1,
                               LmnSymbolAtomRef atom2, int pos2) {
  LmnAtomRef ap0;
  LmnLinkAttr attr;

  ap0 = LMN_SATOM_GET_LINK(atom2, pos2);
  attr = LMN_SATOM_GET_ATTR(atom2, pos2);

  atom1 = dmem_modify_atom(d, m, (LmnSymbolAtomRef)(atom1));

  if (LMN_ATTR_IS_DATA(attr)) {
    LMN_SATOM_SET_LINK((LmnSymbolAtomRef)(atom1), pos1, ap0);
    LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)(atom1), pos1, attr);
  } else {
    /* 最終的にapとatom1が接続する */
    LmnSymbolAtomRef ap = (LmnSymbolAtomRef)(ap0);

    if (LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(ap)) &&
        LMN_PROXY_GET_MEM(ap) != m) {
      if (dmem_root_is_new_mem(d->root_d, m)) {
        LMN_SATOM_SET_LINK(ap, LMN_ATTR_GET_VALUE(attr), atom1);
        LMN_SATOM_SET_ATTR(ap, LMN_ATTR_GET_VALUE(attr),
                           LMN_ATTR_MAKE_LINK(pos1));
      } else {
        LmnMembraneRef m2 = LMN_PROXY_GET_MEM(ap);
        struct MemDelta *d2 = dmem_root_get_mem_delta(d->root_d, m2);
        ap = dmem_modify_atom(d2, m2, (LmnSymbolAtomRef)(ap));

        LMN_SATOM_SET_LINK(ap, LMN_ATTR_GET_VALUE(attr), atom1);
        LMN_SATOM_SET_ATTR(ap, LMN_ATTR_GET_VALUE(attr),
                           LMN_ATTR_MAKE_LINK(pos1));
      }
    } else {
      ap = dmem_modify_atom(d, m, ap);
      LMN_SATOM_SET_LINK(ap, LMN_ATTR_GET_VALUE(attr), atom1);
      LMN_SATOM_SET_ATTR(ap, LMN_ATTR_GET_VALUE(attr),
                         LMN_ATTR_MAKE_LINK(pos1));
    }

    LMN_SATOM_SET_LINK(atom1, pos1, ap);
    LMN_SATOM_SET_ATTR(atom1, pos1, attr);
  }
}

static void dmem_unify_atom_args(struct MemDelta *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef atom1, int pos1,
                                 LmnSymbolAtomRef atom2, int pos2) {
  LmnSymbolAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;
  LmnMembraneRef m1, m2;

  ap1 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom1, pos1);
  ap2 = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom2, pos2);
  attr1 = LMN_SATOM_GET_ATTR(atom1, pos1);
  attr2 = LMN_SATOM_GET_ATTR(atom2, pos2);

  if (LMN_ATTR_IS_DATA(attr1) || LMN_ATTR_IS_DATA(attr2)) {
    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      dmem_link_data_atoms(dmem_root_get_mem_delta(d->root_d, m), m,
                           (LmnDataAtomRef)ap1, attr1, (LmnDataAtomRef)ap2,
                           attr2);
    } else if (!LMN_ATTR_IS_DATA(attr1)) {
      m1 = LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(ap1))
               ? LMN_PROXY_GET_MEM(ap1)
               : m;

      dmem_modify_link(dmem_root_get_mem_delta(d->root_d, m1), m1,
                       (LmnSymbolAtomRef)(ap1), LMN_ATTR_GET_VALUE(attr1), ap2,
                       attr2);
    } else if (!LMN_ATTR_IS_DATA(attr2)) {
      m2 = LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(ap2))
               ? LMN_PROXY_GET_MEM(ap2)
               : m;
      dmem_modify_link(dmem_root_get_mem_delta(d->root_d, m2), m2,
                       (LmnSymbolAtomRef)(ap2), LMN_ATTR_GET_VALUE(attr2), ap1,
                       attr1);
    }
  } else {
    m1 = LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)ap1))
             ? LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1)
             : m;
    m2 = LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)ap2))
             ? LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2)
             : m;
    ap1 = dmem_modify_atom(dmem_root_get_mem_delta(d->root_d, m1), m1,
                           (LmnSymbolAtomRef)ap1);
    ap2 = dmem_modify_atom(dmem_root_get_mem_delta(d->root_d, m2), m2,
                           (LmnSymbolAtomRef)ap2);

    LMN_SATOM_SET_LINK((LmnSymbolAtomRef)ap1, LMN_ATTR_GET_VALUE(attr1), ap2);
    LMN_SATOM_SET_LINK((LmnSymbolAtomRef)ap2, LMN_ATTR_GET_VALUE(attr2), ap1);
    LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)ap1, LMN_ATTR_GET_VALUE(attr1), attr2);
    LMN_SATOM_SET_ATTR((LmnSymbolAtomRef)ap2, LMN_ATTR_GET_VALUE(attr2), attr1);
  }
}

static inline void dmem_copy_rules(struct MemDelta *d, LmnMembraneRef dest,
                                   LmnMembraneRef src) {
  int i;

  if (!d->new_rulesets) {
    d->new_rulesets = vec_make(16);
  }

  for (i = 0; i < src->ruleset_num(); i++) {
    vec_push(d->new_rulesets, (vec_data_t)lmn_mem_get_ruleset(src, i));
  }
}

/* TODO: rename*/
static void dmem_commit(struct MemDelta *d) {
  int i;

  for (i = 0; i < vec_num(&d->new_atoms); i++) {
    mem_push_symbol_atom(d->mem, (LmnSymbolAtomRef)(vec_get(&d->new_atoms, i)));
  }

  for (i = 0; i < vec_num(&d->del_atoms); i++) {
    mem_remove_symbol_atom(d->mem,
                           (LmnSymbolAtomRef)(vec_get(&d->del_atoms, i)));
  }

  /* Membrane */

  //  int n = 0;
  //  for (i = 0; i < vec_num(&d->del_mems); i++) {
  //    lmn_mem_remove_mem(d->mem, (LmnMembraneRef*)vec_get(&d->del_mems, i));
  //    n--;
  //  }

  for (i = 0; i < vec_num(&d->new_mems); i++) {
    /* printf("commit new mem: %p\n", (LmnMembraneRef)vec_get(&d->new_mems, i));
     */
    /* lmn_dump_cell_stdout((LmnMembraneRef)vec_get(&d->new_mems, i)); */
    (d->mem)->add_child_mem((LmnMembraneRef)vec_get(&d->new_mems, i));
    //    n++;
  }

  //  printf("child num = %d\n", n);

  (d->mem)->data_atom_add(d->data_atom_diff);

  if (d->ruleset_removed || d->new_rulesets) {
    d->org_rulesets = vec_copy((d->mem)->get_rulesets());

    if (d->ruleset_removed) {
      vec_clear((d->mem)->get_rulesets());
    }

    if (d->new_rulesets) {
      for (i = 0; i < vec_num(d->new_rulesets); i++) {
        lmn_mem_add_ruleset(d->mem, (LmnRuleSetRef)vec_get(d->new_rulesets, i));
      }
    }
  }

  (d->mem)->set_name(d->new_name);
}

static inline void dmem_commit_delete_mem(struct MemDelta *d) {
  int i;
  /* Membrane */
  for (i = 0; i < vec_num(&d->del_mems); i++) {
    lmn_mem_remove_mem(d->mem, (LmnMembraneRef)vec_get(&d->del_mems, i));
  }
}

static void dmem_revert(struct MemDelta *d) {
  int i;

  for (i = 0; i < vec_num(&d->del_atoms); i++) {
    mem_push_symbol_atom(d->mem, (LmnSymbolAtomRef)(vec_get(&d->del_atoms, i)));
  }

  for (i = 0; i < vec_num(&d->new_atoms); i++) {
    mem_remove_symbol_atom(d->mem,
                           (LmnSymbolAtomRef)(vec_get(&d->new_atoms, i)));
  }

  /* Membrane */

  //  for (i = 0; i < vec_num(&d->new_mems); i++) {
  //    lmn_mem_remove_mem(d->mem, (LmnMembraneRef*)vec_get(&d->new_mems, i));
  //  }

  for (i = 0; i < vec_num(&d->del_mems); i++) {
    (d->mem)->add_child_mem((LmnMembraneRef)vec_get(&d->del_mems, i));
  }

  (d->mem)->data_atom_sub(d->data_atom_diff);

  if (d->ruleset_removed || d->new_rulesets) {
    vec_clear((d->mem)->get_rulesets());
    for (i = 0; i < vec_num(d->org_rulesets); i++) {
      lmn_mem_add_ruleset(d->mem, (LmnRuleSetRef)vec_get(d->org_rulesets, i));
    }

    vec_free(d->org_rulesets);
    d->org_rulesets = NULL;
  }

  (d->mem)->set_name(d->org_name);
}

static inline void dmem_revert_new_mem(struct MemDelta *d) {
  int i;
  /* Membrane */
  for (i = 0; i < vec_num(&d->new_mems); i++) {
    lmn_mem_remove_mem(d->mem, (LmnMembraneRef)vec_get(&d->new_mems, i));
  }
}
