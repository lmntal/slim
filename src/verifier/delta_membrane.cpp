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

MemDeltaRoot::MemDeltaRoot(LmnMembraneRef root_mem, LmnRuleRef rule, unsigned long next_id) {
  int size;

  size = round2up(next_id + 10); /* TODO: 引数に渡す適当なサイズは？ */

  this->next_id = next_id;
  this->root_mem = root_mem;
  this->committed = FALSE;
  this->applied_rule = rule;
  this->proc_tbl = new ProcessTbl(size);
  this->new_mems.init(16);
  this->mem_deltas.init(16);
  this->modified_atoms.init(32);
  this->owner_tbl = new ProcessTbl(size);
  this->flag_tbl = new SimpleProcessTable(size);

  /* add an appried history for constraint handling rules */
  this->applied_history = rule ? rule->latest_history() : ANONYMOUS;

  /* この時点で最大のIDを記録しておくことで,
  * 以降に生成されたIDが新規か否かを判定する */
  this->new_proc_id_lower_limit = next_id;
}
MemDeltaRoot::~MemDeltaRoot() {
  unsigned int i;

  for (i = 0; i < this->mem_deltas.get_num(); i++) {
    mem_delta_free((struct MemDelta *)this->mem_deltas.get(i));
  }
  this->mem_deltas.destroy();
  this->modified_atoms.destroy();
  delete this->owner_tbl;
  delete this->flag_tbl;

  for (i = 0; i < this->new_mems.get_num(); i++) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)this->new_mems.get(i);
    if (proc_tbl_get_by_mem(this->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      delete new_mem_info;
      mem->drop();
      delete mem;
    } else {
      lmn_fatal("unexpected");
    }
  }

  this->new_mems.destroy();
  delete this->proc_tbl;
}

NewMemInfo::NewMemInfo(LmnMembraneRef mem) {
  this->mem = mem;
  this->new_child_mems.init(16);
  this->removed_child_mems.init(16);
}

NewMemInfo::~NewMemInfo() {
  this->new_child_mems.destroy();
  this->removed_child_mems.destroy();
}

LmnMembraneRef MemDeltaRoot::get_root_mem() const{
  return this->root_mem;
}

unsigned long MemDeltaRoot::get_next_id() const{
  return this->next_id;
}

struct MemDelta *MemDeltaRoot::get_mem_delta(LmnMembraneRef m){
  LmnWord t;
  /* /\*d*\/ if (this->is_new_mem(m)) lmn_fatal("unexpected"); */
  if (proc_tbl_get_by_mem(this->proc_tbl, m, &t))
    return (struct MemDelta *)t;
  else {
    struct MemDelta *mem_delta = mem_delta_make(this, m, this->next_id);
    (this->proc_tbl)->proc_tbl_put_mem(m, (LmnWord)mem_delta);
    this->flag_tbl->tbl_set_mem_flag(m, TAG_DELTA_MEM);
    this->mem_deltas.push((vec_data_t)mem_delta);
    return mem_delta;
  }
}

BOOL MemDeltaRoot::is_delta_mem(LmnMembraneRef m) const{
  return this->flag_tbl->tbl_get_flag_by_mem(m, TAG_DELTA_MEM);
}

BOOL MemDeltaRoot::is_new_mem(LmnMembraneRef m) const{
  return m->mem_id() >= this->new_proc_id_lower_limit;
}

/* 膜mにアトムatomを追加 */
void MemDeltaRoot::push_atom(LmnMembraneRef m, LmnAtomRef atom, LmnLinkAttr attr) {
  if (this->is_new_mem(m)) { /* BODYで生成された膜の場合 */
    lmn_mem_push_atom(m, atom, attr);
  } else { /* 既にある膜への追加 */
    dmem_put_atom(this->get_mem_delta(m), m, atom, attr);
  }
}

void MemDeltaRoot::remove_atom(LmnMembraneRef m,
                           LmnAtomRef atom, LmnLinkAttr attr) {
  if (this->is_new_mem(m)) {
    lmn_mem_remove_atom(m, atom, attr);
  } else {
    dmem_remove_atom(this->get_mem_delta(m), m, atom, attr);
  }
}

void MemDeltaRoot::newlink(LmnMembraneRef m,
                       LmnAtomRef atom0, LmnLinkAttr attr0, int pos0,
                       LmnAtomRef atom1, LmnLinkAttr attr1, int pos1) {
  if (this->is_new_mem(m)) {
    lmn_mem_newlink(m, atom0, attr0, pos0, atom1, attr1, pos1);
  } else {
    struct MemDelta *d =
        this->get_mem_delta(m); /* 膜m用の差分データを呼び出す */
    if (LMN_ATTR_IS_DATA(attr0) &&
        LMN_ATTR_IS_DATA(attr1)) { /* UNIFYファンクタを作ってデータを埋め込む */
      dmem_link_data_atoms(d, m, (LmnDataAtomRef)atom0, attr0,
                           (LmnDataAtomRef)atom1, attr1);
    } else if (LMN_ATTR_IS_DATA(attr0)) {
      ((LmnSymbolAtomRef)atom1)->set_link(pos1, atom0);
      ((LmnSymbolAtomRef)atom1)->set_attr(pos1, attr0);
    } else if (LMN_ATTR_IS_DATA(attr1)) {
      ((LmnSymbolAtomRef)(atom0))->set_link(pos0, atom1);
      ((LmnSymbolAtomRef)(atom0))->set_attr(pos0, attr1);
    } else {
      lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom0), pos0,
                             (LmnSymbolAtomRef)(atom1), pos1);
    }
  }
}

/* データアトム同士の接続 */
void MemDeltaRoot::link_data_atoms(LmnMembraneRef m,
                               LmnDataAtomRef d1, LmnLinkAttr attr1,
                               LmnDataAtomRef d2, LmnLinkAttr attr2) {
  dmem_link_data_atoms(this->get_mem_delta(m), m, d1, attr1, d2, attr2);
}

/* atom1, atom2はシンボルアトムのはず */
void MemDeltaRoot::unify_atom_args(LmnMembraneRef m,
                               LmnSymbolAtomRef atom1, int pos1,
                               LmnSymbolAtomRef atom2, int pos2) {
  LmnAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;

  if (this->is_new_mem(m)) {
    ap1 = dmem_root_get_link(this, atom1, pos1);
    ap2 = dmem_root_get_link(this, atom2, pos2);
    attr1 = atom1->get_attr(pos1);
    attr2 = atom2->get_attr(pos2);
    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      lmn_mem_link_data_atoms(m, ap1, attr1, ap2, attr2);
    } else {
      if (!LMN_ATTR_IS_DATA(attr1)) {
        if (LMN_IS_PROXY_FUNCTOR(
                ((LmnSymbolAtomRef)ap1)->get_functor()) &&
            LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1) != m &&
            !this->is_new_mem(LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1))) {
          dmem_modify_link(this->get_mem_delta(LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1)),
                           LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1),
                           (LmnSymbolAtomRef)(ap1), LMN_ATTR_GET_VALUE(attr1),
                           ap2, attr2);
        } else {
          ((LmnSymbolAtomRef)ap1)->set_link(LMN_ATTR_GET_VALUE(attr1),(LmnSymbolAtomRef)ap2);
          ((LmnSymbolAtomRef)ap1)->set_attr(LMN_ATTR_GET_VALUE(attr1),attr2);
        }
      }
      if (!LMN_ATTR_IS_DATA(attr2)) {
        if (LMN_IS_PROXY_FUNCTOR(
                ((LmnSymbolAtomRef)ap2)->get_functor()) &&
            LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2) != m &&
            !this->is_new_mem(LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2))) {
          dmem_modify_link(this->get_mem_delta(
                               LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2)),
                           LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2),
                           (LmnSymbolAtomRef)(ap2), LMN_ATTR_GET_VALUE(attr2),
                           ap1, attr1);
        } else {
          ((LmnSymbolAtomRef)ap2)->set_link(LMN_ATTR_GET_VALUE(attr2),ap1);
          ((LmnSymbolAtomRef)ap2)->set_attr(LMN_ATTR_GET_VALUE(attr2),attr1);
        }
      }
    }
  } else {
    dmem_unify_atom_args(this->get_mem_delta(m), m,
                         dmem_root_modified_atom(this, atom1), pos1,
                         dmem_root_modified_atom(this, atom2), pos2);
  }
}

/* シンボルアトムatom1と、シンボルorデータアトムatom2の間にリンクを張る。 */
void MemDeltaRoot::unify_links(LmnMembraneRef m,
                           LmnAtomRef atom1, LmnLinkAttr attr1,
                           LmnAtomRef atom2, LmnLinkAttr attr2) {
  if (LMN_ATTR_IS_DATA(attr2)) { /* 2 is data */
    if (dmem_root_is_new_atom(this, (LmnSymbolAtomRef)(atom1))) {
      ((LmnSymbolAtomRef)atom1)->set_link(LMN_ATTR_GET_VALUE(attr1),
                         (LmnSymbolAtomRef)atom2);
      ((LmnSymbolAtomRef)atom1)->set_attr(LMN_ATTR_GET_VALUE(attr1),attr2);
    } else {
      dmem_modify_link(this->get_mem_delta(m), m,
                       (LmnSymbolAtomRef)(atom1), LMN_ATTR_GET_VALUE(attr1),
                       (LmnSymbolAtomRef)atom2, attr2);
    }
  } /* 1, 2 are symbol atom */
  else if (dmem_root_is_new_atom(this, (LmnSymbolAtomRef)(atom1))) {
    ((LmnSymbolAtomRef)atom1)->set_link(LMN_ATTR_GET_VALUE(attr1),
                       (LmnSymbolAtomRef)atom2);
    ((LmnSymbolAtomRef)atom1)->set_attr(LMN_ATTR_GET_VALUE(attr1),attr2);
    if (dmem_root_is_new_atom(this, (LmnSymbolAtomRef)(atom2))) {
      ((LmnSymbolAtomRef)atom2)->set_link(LMN_ATTR_GET_VALUE(attr2),
                         (LmnSymbolAtomRef)atom1);
      ((LmnSymbolAtomRef)atom2)->set_attr(LMN_ATTR_GET_VALUE(attr2),attr1);
    } else {
      dmem_modify_link(this->get_mem_delta(m), m,
                       (LmnSymbolAtomRef)(atom2), LMN_ATTR_GET_VALUE(attr2),
                       atom1, attr1);
    }
  } else if (dmem_root_is_new_atom(this, (LmnSymbolAtomRef)(atom2))) {
    dmem_modify_link(this->get_mem_delta(m), m,
                     (LmnSymbolAtomRef)(atom1), LMN_ATTR_GET_VALUE(attr1),
                     (LmnSymbolAtomRef)atom2, attr2);
    ((LmnSymbolAtomRef)atom2)->set_link(LMN_ATTR_GET_VALUE(attr2),
                       (LmnSymbolAtomRef)atom1);
    ((LmnSymbolAtomRef)atom2)->set_attr(LMN_ATTR_GET_VALUE(attr2),attr1);
  } else {
    /* atom1,2を複製 */
    LmnSymbolAtomRef copy1, copy2;

    copy1 = dmem_modify_atom(this->get_mem_delta(m), m,
                             (LmnSymbolAtomRef)(atom1));
    copy2 = dmem_modify_atom(this->get_mem_delta(m), m,
                             (LmnSymbolAtomRef)(atom2));
    /* 複製したatom1とatom2を接続させる */
    copy1->set_link(LMN_ATTR_GET_VALUE(attr1), copy2);
    copy1->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);
    copy2->set_link(LMN_ATTR_GET_VALUE(attr2), copy1);
    copy2->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);
  }
}

/* atom1は新規アトム、atom2はもともと接続していたアトム */
void MemDeltaRoot::relink(LmnMembraneRef m,
                      LmnAtomRef atom1, LmnLinkAttr attr1, int pos1,
                      LmnAtomRef atom2, LmnLinkAttr attr2, int pos2) {
  /* atom1は新規アトムのはず */
  LmnAtomRef ap; /* 現在atom2と接続していて、将来的にatom1に接続するアトム */
  LmnLinkAttr attr;

  /* if (!dmem_root_is_new_atom(this, LMN_SATOM(atom1)))
   * lmn_fatal("unexpected"); */

  if (this->is_new_mem(m)) {
    /* mが新規膜の場合 */
    /* if (LMN_ATTR_IS_DATA(attr2)) lmn_fatal("unexpected"); */
    ap = dmem_root_get_link(this, (LmnSymbolAtomRef)(atom2), pos2);
    attr = ((LmnSymbolAtomRef)atom2)->get_attr(pos2);

    if (LMN_ATTR_IS_DATA(attr)) {
      lmn_mem_relink_atom_args(m, atom1, attr1, pos1, atom2, attr2, pos2);
    } else if (LMN_IS_PROXY_FUNCTOR(
                   ((LmnSymbolAtomRef)ap)->get_functor()) &&
               LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap) != m) {
      /* atom1（またはatom2）とapが別膜の場合（atom1,apが$in-$outの関係） */
      LmnMembraneRef m2 = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap);
      if (this->is_new_mem(m2)) {
        /* apの膜が新規膜 */
        //        lmn_mem_relink_atom_args(m, atom1, attr1, pos1, atom2, attr2,
        //        pos2);
        lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom1), pos1,
                               (LmnSymbolAtomRef)(ap),
                               LMN_ATTR_GET_VALUE(attr));
      } else {
        /* apの膜が既存膜 */
        ((LmnSymbolAtomRef)atom1)->set_link(pos1,
            dmem_root_modified_atom(this, (LmnSymbolAtomRef)(ap)));
        ((LmnSymbolAtomRef)atom1)->set_attr(pos1, attr);

        dmem_modify_link(
            this->get_mem_delta(m2), m2,
            dmem_root_modified_atom(this, (LmnSymbolAtomRef)(ap)),
            LMN_ATTR_GET_VALUE(attr), atom1, LMN_ATTR_MAKE_LINK(attr1));
      }
    } else {
      lmn_newlink_in_symbols((LmnSymbolAtomRef)(atom1), pos1,
                             (LmnSymbolAtomRef)(ap), LMN_ATTR_GET_VALUE(attr));
    }
  } else {
    /* mが既存膜の場合 */
    dmem_relink(
        this->get_mem_delta(m), m,
        dmem_root_modified_atom(this, (LmnSymbolAtomRef)(atom1)), pos1,
        dmem_root_modified_atom(this, (LmnSymbolAtomRef)(atom2)), pos2);
  }
}

void MemDeltaRoot::move_satom(LmnWord key, LmnWord dest) {
  (this->proc_tbl)->put_new(key, dest);
}

/* destが移動先、srcが移動元 */
void MemDeltaRoot::move_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem) {
  if (this->is_new_mem(destmem) && this->is_new_mem(srcmem)) {
    /* 移動先・移動元ともに新規膜 */
    destmem->move_cells(srcmem);
  } else {
    ProcessTableRef atoms = this->copy_cells(destmem, srcmem);
    if (!this->is_new_mem(srcmem)) {
      /* 移動先が既存膜 */
      /* printf("move cells\n"); */
      /* printf("src mem :" ); lmn_dump_mem_dev(srcmem); */
      /* printf("dest mem: "); lmn_dump_mem_dev(destmem); */
      dmem_drop(this->get_mem_delta(srcmem), srcmem);
      modify_free_link(this, destmem);
    } else {
      lmn_fatal("unexpected");
    }
    /* relink命令等のために移動先と移動元のアトムを対応付ける必要がある */
    atoms->tbl_foreach([](LmnWord _k, LmnWord _v, LmnWord _arg){
      ((struct MemDeltaRoot *)_arg)->move_satom(_k, _v);
      return 1;
    }, (LmnWord)this);
    delete atoms;
  }
}

ProcessTableRef MemDeltaRoot::copy_cells(LmnMembraneRef destmem, LmnMembraneRef srcmem) {
  if (this->is_new_mem(destmem) && this->is_new_mem(srcmem)) {
    return lmn_mem_copy_cells(destmem, srcmem);
  } else {
    ProcessTableRef atoms = new ProcessTbl(64);
    /* /\*d*\/ if (d->is_new_mem(srcmem)) lmn_fatal("unexpected"); */
    if (this->is_new_mem(destmem)) {
      /* 移動先が新規膜 */
      this->copy_cells(NULL, destmem, this->get_mem_delta(srcmem),
                      srcmem, atoms);
    } else if (this->is_new_mem(srcmem)) {
      /* 移動元が新規膜 */
      this->copy_cells(this->get_mem_delta(destmem), destmem, NULL,
                      srcmem, atoms);
    } else {
      this->copy_cells(this->get_mem_delta(destmem), destmem,
                      this->get_mem_delta(srcmem), srcmem, atoms);
    }
    return atoms;
  }
}

void MemDeltaRoot::copy_cells(struct MemDelta *d,
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
    LmnMembraneRef new_mem = dmem_root_new_mem(this);
    /* 子膜はマッチングに使われないので、そのままコピーしていいはず */
    if (this->is_delta_mem(m)) {
      this->copy_cells(NULL, new_mem, this->get_mem_delta(m),
                      m, atoms);
    } else
      this->copy_cells(NULL, new_mem, NULL, m, atoms);

    if (d)
      dmem_add_child_mem(d, destmem, new_mem);
    else
      dmem_root_add_child_mem(this, destmem, new_mem);

    atoms->proc_tbl_put_mem(m, (LmnWord)new_mem);
    /* copy name */
    new_mem->set_name(m->NAME_ID());
    /* copy rulesets */
    for (i = 0; i < m->get_rulesets()->get_num(); i++) {
      if (d)
        dmem_add_ruleset(d, new_mem,
                         (LmnRuleSetRef)m->get_rulesets()->get(i));
      else
        lmn_mem_add_ruleset(new_mem,
                            (LmnRuleSetRef)m->get_rulesets()->get(i));
    }
  });

  /* copy atoms */
  DMEM_ALL_ATOMS(
      d2, srcmem, srcatom, ({
        LmnFunctor f;
        LmnSymbolAtomRef newatom;
        unsigned int start, end;
        srcatom = dmem_root_modified_atom(
            this,
            srcatom); /* プロキシ操作命令等で既にmodifyされている場合のために */

        /* すでにコピー済みなら次の候補へ */
        if (proc_tbl_get_by_atom(atoms, srcatom, NULL)) continue;

        f = srcatom->get_functor();
        newatom = dmem_root_copy_satom_with_data(this, srcatom);
        if (d)
          dmem_put_symbol_atom(d, destmem, newatom);
        else
          mem_push_symbol_atom(destmem, newatom);
        atoms->proc_tbl_put_atom(srcatom, (LmnWord)newatom);
        start = 0;
        end = srcatom->get_arity();

        if (LMN_IS_PROXY_FUNCTOR(f)) {
          start = 1, end = 2;
          LMN_PROXY_SET_MEM(newatom, destmem);

          /* oproxyの場合はコピー済みの内側にあるiproxyと接続させる必要がある（starは未検証？）
           */
          if (f == LMN_OUT_PROXY_FUNCTOR) {
            LmnSymbolAtomRef srcinside;
            LmnSymbolAtomRef newinside;

            srcinside =
                (LmnSymbolAtomRef)(dmem_root_get_link(this, srcatom, 0));
            if (proc_tbl_get_by_atom(atoms, srcinside, &t)) {
              newinside = (LmnSymbolAtomRef)(t);

              /* 必ず子膜につながっているはず */
              LMN_ASSERT(srcinside->get_functor() ==
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

          attr = srcatom->get_attr(i);
          a = srcatom->get_link(i);
          if (LMN_ATTR_IS_DATA(attr)) {
            if (d)
              dmem_put_atom(d, destmem, a, attr);
            else
              lmn_mem_push_atom(destmem, a, attr);
          } else if (proc_tbl_get_by_atom(atoms, (LmnSymbolAtomRef)(a), &t)) {
            lmn_newlink_in_symbols(newatom, i, (LmnSymbolAtomRef)(t),
                                   LMN_ATTR_MAKE_LINK(attr));
            /* newatom->set_link(i, t); */
            /* newatom->set_attr(i, attr); */
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
  if (root_d->is_new_mem(m))
    d = NULL;
  else
    d = root_d->get_mem_delta(m);

  DMEM_EACH_FUNC_ATOM(d, m, LMN_IN_PROXY_FUNCTOR, in,
                      { modify_free_link_sub(root_d, m, in); });
  DMEM_EACH_FUNC_ATOM(d, m, LMN_STAR_PROXY_FUNCTOR, in,
                      { modify_free_link_sub(root_d, m, in); });
}

static void modify_free_link_sub(struct MemDeltaRoot *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef in) {
  LmnSymbolAtomRef out;
  out = (LmnSymbolAtomRef)(dmem_root_get_link(d, in, 0));

  if ((LmnSymbolAtomRef)(out->get_link(0)) != in) {
    if (d->is_new_mem(LMN_PROXY_GET_MEM(out))) {
      out->set_link(0, in);
    } else {
      dmem_modify_link(d->get_mem_delta(LMN_PROXY_GET_MEM(out)),
                       LMN_PROXY_GET_MEM(out), out, 0, in,
                       out->get_attr(0));
    }
  }
}

inline void dmem_root_remove_symbol_atom_with_buddy_data(
    struct MemDeltaRoot *d, LmnMembraneRef m, LmnSymbolAtomRef atom) {
  unsigned int i;
  unsigned int end = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      if (d->is_new_mem(m))
        lmn_mem_remove_data_atom(m, (LmnDataAtomRef)atom->get_link(i), atom->get_attr(i));
      else
        d->get_mem_delta(m)->data_atom_diff--;
    }
  }

  if (d->is_new_mem(m)) {
    mem_remove_symbol_atom(m, atom);
  } else {
    dmem_remove_symbol_atom(d->get_mem_delta(m), m, atom);
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

  end = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      lmn_mem_remove_data_atom(m, (LmnDataAtomRef)atom->get_link(i), atom->get_attr(i));
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

  end = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
  /* free linked data atoms */
  for (i = 0; i < end; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      d->data_atom_diff--;
    }
  }

  //  dmem_remove_symbol_atom(d, m, atom);
  d->del_atoms.push((vec_data_t)atom);
  (d->root_d->owner_tbl)->proc_tbl_put_atom(atom, 0);
  return 1;
}

void dmem_root_remove_ground(struct MemDeltaRoot *root_d, LmnMembraneRef mem,
                             Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long i, t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);

  /* memは既存膜のはず */
  atoms->tbl_foreach(dmem_root_remove_symbol_atom_with_buddy_data_dmem_f,
                   (LmnWord)root_d->get_mem_delta(mem));

  /* atomsはシンボルアトムしか含まないので、
   * srcvecのリンクが直接データアトムに接続している場合の処理をする */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
    if (LMN_ATTR_IS_DATA(LinkObjGetPos(l))) {
      if (root_d->is_new_mem(mem))
        lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)LinkObjGetAtom(l),
                                 LinkObjGetPos(l));
      else
        root_d->get_mem_delta(mem)->data_atom_diff--;
    }
  }

  delete atoms;
}

int dmem_root_free_satom_f(LmnWord _k, LmnWord _v, LmnWord _arg) {
  dmem_root_free_satom((struct MemDeltaRoot *)_arg, (LmnSymbolAtomRef)_v);
  return 1;
}

void dmem_root_free_ground(struct MemDeltaRoot *root_d, Vector *srcvec) {
  ProcessTableRef atoms;
  unsigned long t;

  ground_atoms(srcvec, NULL, &atoms, &t, NULL, NULL, NULL, NULL);

  atoms->tbl_foreach(dmem_root_free_satom_f, (LmnWord)root_d);
  delete atoms;
}

void dmem_root_copy_ground(struct MemDeltaRoot *root_d, LmnMembraneRef mem,
                           Vector *srcvec, Vector **ret_dstlovec,
                           ProcessTableRef *ret_atommap) {
  ProcessTableRef atommap;
  Vector stack;
  LmnWord t;
  struct MemDelta *d;
  unsigned int i;

  atommap = new ProcessTbl(64);
  t = 0UL; /* 警告されるので.. */
  atommap = new ProcessTbl(64);
  stack.init(16);
  d = root_d->get_mem_delta(mem);
  *ret_dstlovec = new Vector(16);

  /* 根をスタックに積む。スタックにはリンクオブジェクトではなくアトムを積むため、
   * ここで根の先のアトムをコピーしスタックに積む必要がある */
  for (i = 0; i < srcvec->get_num(); i++) {
    LinkObjRef l = (LinkObjRef)srcvec->get(i);
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
      if (((LmnSymbolAtomRef)LinkObjGetAtom(l))->get_functor() ==
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

      atommap->proc_tbl_put_atom((LmnSymbolAtomRef)(LinkObjGetAtom(l)),
                        (LmnWord)cpatom);
      /* 根のリンクのリンクポインタを0に設定する */
      cpatom->set_link(LinkObjGetPos(l), 0);
      stack.push((LmnWord)LinkObjGetAtom(l));
    } else {
      /* コピー済みの場合はスタックには追加しない */
      cpatom = (LmnSymbolAtomRef)t;
      cpatom->set_link(LinkObjGetPos(l), 0);
    }
    (*ret_dstlovec)->push((LmnWord)LinkObj_make(cpatom, LinkObjGetPos(l)));
  }

  while (stack.get_num() > 0) {
    LmnSymbolAtomRef src_atom = (LmnSymbolAtomRef)stack.pop();
    LmnSymbolAtomRef copied;

    proc_tbl_get_by_atom(atommap, src_atom, &t);
    copied = (LmnSymbolAtomRef)t;

    for (i = 0; i < src_atom->get_arity(); i++) {
      LmnSymbolAtomRef next_src =
          (LmnSymbolAtomRef)src_atom->get_link(i);
      LmnLinkAttr next_attr = src_atom->get_attr(i);

      /* copied->get_link(i)が0になる場合は、根に到達した場合 */
      if (LMN_ATTR_IS_DATA(next_attr)) {
        if (d) {
          dmem_put_atom(d, mem, next_src, next_attr);
        } else {
          lmn_mem_push_atom(mem, next_src, next_attr);
        }
      } else if (copied->get_link(i) != 0) {
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
          atommap->proc_tbl_put_atom(next_src, (LmnWord)next_copied);
          stack.push((LmnWord)next_src);
        }
        copied->set_link(i, next_copied);
      }
    }
  }

  stack.destroy();
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

  for (i = 0; i < d->mem_deltas.get_num(); i++) {
    dmem_commit_delete_mem((struct MemDelta *)d->mem_deltas.get(i));
  }

  for (i = 0; i < d->mem_deltas.get_num(); i++) {
    dmem_commit((struct MemDelta *)d->mem_deltas.get(i));
  }

  for (i = 0; i < d->new_mems.get_num(); i++) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)d->new_mems.get(i);
    if (proc_tbl_get_by_mem(d->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      for (j = 0; j < new_mem_info->removed_child_mems.get_num(); j++) {
        mem->remove_mem((LmnMembraneRef)new_mem_info->removed_child_mems.get(j));
      }
      for (j = 0; j < new_mem_info->new_child_mems.get_num(); j++) {
        mem->add_child_mem((LmnMembraneRef)new_mem_info->new_child_mems.get(j));
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  for (i = 0; i < d->modified_atoms.get_num(); i += 2) {
    LmnSymbolAtomRef src = (LmnSymbolAtomRef)(d->modified_atoms.get(i));
    LmnSymbolAtomRef atm =
        (LmnSymbolAtomRef)(d->modified_atoms.get(i + 1));

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
  int arity = src->get_link_num();

  /* printf("commit atom %s %p -> %p\n", src->str(), src, atm); */
  /* printf("%s(", atm->str()); */
  /* for (i = 0; i < arity; i++) { */
  /*   if (i>0) printf(","); */
  /*   printf("%p", LMN_SATOM(atm->get_link(i))); */
  /* } */
  /* printf(")\n"); */
  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr = atm->get_attr(i);
    if (!LMN_ATTR_IS_DATA(attr)) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d, (LmnSymbolAtomRef)(atm->get_link(i)));
      if (a->get_link(LMN_ATTR_GET_VALUE(attr)) != atm) {
        /* printf("  modify link %p %d -> %p\n", a, LMN_ATTR_GET_VALUE(attr),
         * atm); */
        a->set_link(LMN_ATTR_GET_VALUE(attr), atm);
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
  for (i = d->new_mems.get_num() - 1; i >= 0; i--) {
    LmnWord t;
    LmnMembraneRef mem;
    struct NewMemInfo *new_mem_info;

    mem = (LmnMembraneRef)d->new_mems.get(i);
    if (proc_tbl_get_by_mem(d->proc_tbl, mem, &t)) {
      new_mem_info = (struct NewMemInfo *)t;

      for (j = new_mem_info->new_child_mems.get_num() - 1; j >= 0; j--) {
        mem->remove_mem((LmnMembraneRef)new_mem_info->new_child_mems.get(j));
      }
      for (j = new_mem_info->removed_child_mems.get_num() - 1; j >= 0; j--) {
        mem->add_child_mem((LmnMembraneRef)new_mem_info->removed_child_mems.get(j));
      }
    } else {
      lmn_fatal("unexpected");
    }
  }

  for (i = d->mem_deltas.get_num() - 1; i >= 0; i--) {
    dmem_revert_new_mem((struct MemDelta *)d->mem_deltas.get(i));
  }

  for (i = d->mem_deltas.get_num() - 1; i >= 0; i--) {
    dmem_revert((struct MemDelta *)d->mem_deltas.get(i));
  }

  for (i = d->modified_atoms.get_num() - 1; i >= 0; i -= 2) {
    LmnSymbolAtomRef src =
        (LmnSymbolAtomRef)(d->modified_atoms.get(i - 1));
    LmnSymbolAtomRef atm = (LmnSymbolAtomRef)(d->modified_atoms.get(i));

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
  int arity = src->get_link_num();

  /* printf("revert atom: %s %p -> %p\n", src->str(), atom, src); */

  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr = src->get_attr(i);
    if (!LMN_ATTR_IS_DATA(attr)) {
      if (((LmnSymbolAtomRef)src->get_link(i))->get_link(LMN_ATTR_GET_VALUE(attr)) != src) {
        ((LmnSymbolAtomRef)src->get_link(i))->set_link(LMN_ATTR_GET_VALUE(attr), src);
      }
    }
  }
}

void dmem_root_add_child_mem(struct MemDeltaRoot *d, LmnMembraneRef parent,
                             LmnMembraneRef child) {
  if (d->is_new_mem(child)) { /* 追加する方が新しい膜 */
    child->set_parent(parent);
  } else { /* そうじゃない場合 */
    d->get_mem_delta(child)->new_parent = parent;
  }
  if (d->is_new_mem(parent)) { /* 新しい膜 */
    dmem_root_get_new_mem_info(d, parent)->new_child_mems.push((vec_data_t)child);
  } else { /* 既出の膜 */
    dmem_add_child_mem(d->get_mem_delta(parent), parent, child);
  }

  (d->owner_tbl)->proc_tbl_put_mem(child, (LmnWord)parent);
}

void dmem_root_remove_mem(struct MemDeltaRoot *root_d, LmnMembraneRef parent,
                          LmnMembraneRef child) {
  if (root_d->is_new_mem(child)) {
    child->set_parent(NULL);
  } else {
    root_d->get_mem_delta(child)->new_parent = NULL;
  }

  if (root_d->is_new_mem(parent)) {
    dmem_root_get_new_mem_info(root_d, parent)->removed_child_mems.push((vec_data_t)child);
  } else {
    struct MemDelta *d = root_d->get_mem_delta(parent);

#ifdef DEBUG
    if (root_d->is_new_mem(child)) lmn_fatal("unexpected");
    if (dmem_is_removed_mem(d, parent, child)) lmn_fatal("unexpected");
#endif
    d->del_mems.push((vec_data_t)child);
  }
  (root_d->owner_tbl)->proc_tbl_put_mem(child, 0);
}

/* cf. Java処理系 */
/*
 * TODO:
 * とても非効率なので，以前のREMOVEタグを使った実装に戻すか
 * HashSetを使うようにする
 */
void dmem_root_remove_toplevel_proxies(struct MemDeltaRoot *root_d,
                                       LmnMembraneRef mem) {
  if (root_d->is_new_mem(mem)) {
    mem->remove_toplevel_proxies();
  } else {
    Vector remove_list;
    LmnSymbolAtomRef outside;
    unsigned int i;
    struct MemDelta *d;

    d = root_d->get_mem_delta(mem);
    /*   printf("before remove toplevel proxy "); lmn_dump_mem_stdout(mem); */

    remove_list.init(16);

    DMEM_EACH_FUNC_ATOM(d, mem, LMN_OUT_PROXY_FUNCTOR, outside, {
      LmnSymbolAtomRef a0;
      a0 = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, outside, 0));
      if (LMN_PROXY_GET_MEM(a0) &&
          dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)) != mem) {
        if (!LMN_ATTR_IS_DATA(outside->get_attr(1))) {
          LmnSymbolAtomRef a1 =
              (LmnSymbolAtomRef)(dmem_root_get_link(root_d, outside, 1));
          if (a1->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
            LmnSymbolAtomRef a10 =
                (LmnSymbolAtomRef)(dmem_root_get_link(root_d, a1, 0));
            if (LMN_PROXY_GET_MEM(a10) &&
                dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a10)) != mem) {
              if (!remove_list.contains((LmnWord)outside)) {
                dmem_unify_atom_args(d, mem, outside, 0, a1, 0);
                remove_list.push((LmnWord)outside);
                remove_list.push((LmnWord)a1);
              }
            }
          }
        }
      }
    });

    for (i = 0; i < remove_list.get_num(); i++) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d->root_d, (LmnSymbolAtomRef)(remove_list.get(i)));
      dmem_remove_symbol_atom(d, mem, a);
      dmem_root_free_satom(root_d, a);
    }
    remove_list.destroy();

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
  if (root_d->is_new_mem(mem)) {
    /* printf("remove proxies new mem\n"); */
    mem->remove_proxies();
  } else {
    unsigned int i;
    Vector remove_list, change_list;
    LmnSymbolAtomRef opxy;
    LmnSymbolAtomRef a;
    struct MemDelta *d;

    d = root_d->get_mem_delta(mem);

    /* printf("before remove proxy "); lmn_dump_mem_stdout(root_d->root_mem); */
    /* printf("remove proxies mem = %p\n", mem); */
    /* lmn_dump_mem_dev(mem); */

    remove_list.init(16);
    change_list.init(16);

    DMEM_EACH_FUNC_ATOM(d, mem, LMN_OUT_PROXY_FUNCTOR, opxy, {
      LmnSymbolAtomRef a0 =
          (LmnSymbolAtomRef)(dmem_root_get_link(root_d, opxy, 0));
      /* printf("a0 = %s %p, parent = %p, mem = %p\n", a0->str(), a0,
       * dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)), mem); */
      if (dmem_root_get_parent(root_d, LMN_PROXY_GET_MEM(a0)) !=
              mem && /* opxyのリンク先が子膜でない場合 */
          !LMN_ATTR_IS_DATA(opxy->get_attr(1))) {
        /* if (LMN_PROXY_GET_MEM(a0)->parent != mem && /\*
         * opxyのリンク先が子膜でない場合 *\/ */
        /*     !LMN_ATTR_IS_DATA(opxy->get_attr(1))) { */
        LmnSymbolAtomRef a1 =
            (LmnSymbolAtomRef)(dmem_root_get_link(root_d, opxy, 1));
        LmnFunctor f1 = a1->get_functor();
        if (f1 == LMN_IN_PROXY_FUNCTOR) { /* (1) */
          /* printf("(1)\n"); */
          dmem_unify_atom_args(d, mem, opxy, 0, a1, 0);
          remove_list.push((LmnWord)opxy);
          remove_list.push((LmnWord)a1);
        } else {
          if (f1 == LMN_OUT_PROXY_FUNCTOR &&
              (LMN_PROXY_GET_MEM((LmnSymbolAtomRef)dmem_root_get_link(root_d, a1, 0)))->mem_parent() !=
                  mem) { /* (3) */
            if (!remove_list.contains((LmnWord)opxy)) {
              dmem_unify_atom_args(d, mem, opxy, 0, a1, 0);
              remove_list.push((LmnWord)opxy);
              remove_list.push((LmnWord)a1);
            }
          } else { /* (2) */
            /* printf("(2)\n"); */

            change_list.push((LmnWord)opxy);
          }
        }
      }
    });

    for (i = 0; i < remove_list.get_num(); i++) {
      LmnSymbolAtomRef a = dmem_root_modified_atom(
          d->root_d, (LmnSymbolAtomRef)(remove_list.get(i)));
      dmem_remove_symbol_atom(d, mem, a);
      dmem_root_free_satom(root_d, a);
    }
    remove_list.destroy();

    /* add inside proxy to change list */
    DMEM_EACH_FUNC_ATOM(d, mem, LMN_IN_PROXY_FUNCTOR, a, {
      /* clear mem attribute */
      change_list.push((LmnWord)a);
    });

    { /* change to star proxy */
      for (i = 0; i < change_list.get_num(); i++) {
        dmem_root_alter_functor(
            root_d, mem,
            dmem_root_modified_atom(
                root_d, (LmnSymbolAtomRef)(change_list.get(i))),
            LMN_STAR_PROXY_FUNCTOR);
      }
    }
    change_list.destroy();
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

  if (root_d->is_new_mem(mem))
    parent_d = NULL;
  else
    parent_d = root_d->get_mem_delta(mem);
  if (root_d->is_new_mem(child_mem))
    child_d = NULL;
  else
    child_d = root_d->get_mem_delta(child_mem);

  /* printf("insert proxies parent = %p child_mem = %p(delta=%p)\n", mem,
   * child_mem, child_d); */

  remove_list.init(16);
  change_list.init(16); /* inside proxy にするアトム */

  DMEM_EACH_FUNC_ATOM(
      child_d, child_mem, LMN_STAR_PROXY_FUNCTOR, star, ({
        /* スタープロキシは新規アトムのはず */
        oldstar = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, star, 0));
        if (child_d) oldstar = dmem_modify_atom(child_d, child_mem, oldstar);
        if (LMN_PROXY_GET_MEM(oldstar) == child_mem) { /* (1) */
          if (!remove_list.contains((LmnWord)star)) {
            if (child_d)
              dmem_unify_atom_args(child_d, child_mem, star, 1, oldstar, 1);
            else
              root_d->unify_atom_args(child_mem, star, 1, oldstar, 1);
            remove_list.push((LmnWord)star);
            remove_list.push((LmnWord)oldstar);
          }
        } else {
          change_list.push((LmnWord)star);
          if (LMN_PROXY_GET_MEM(oldstar) == mem) { /* (2) */
            oldstar = dmem_root_alter_functor(root_d, mem, oldstar,
                                              LMN_OUT_PROXY_FUNCTOR);
            lmn_newlink_in_symbols(star, 0, oldstar, 0);
          } else { /* (3) */
            LmnSymbolAtomRef outside =
                dmem_root_new_atom(root_d, LMN_OUT_PROXY_FUNCTOR);
            LmnSymbolAtomRef newstar =
                dmem_root_new_atom(root_d, LMN_STAR_PROXY_FUNCTOR);
            root_d->push_atom(mem, outside, LMN_ATTR_MAKE_LINK(0));
            root_d->push_atom(mem, newstar, LMN_ATTR_MAKE_LINK(0));
            lmn_newlink_in_symbols(outside, 1, newstar, 1);
            if (parent_d)
              dmem_relink(parent_d, mem, newstar, 0, star, 0);
            else
              root_d->relink(mem, newstar, LMN_ATTR_MAKE_LINK(0), 0,
                               star, LMN_ATTR_MAKE_LINK(0), 0);
            lmn_newlink_in_symbols(star, 0, outside, 0);
          }
        }
      }));

  for (i = 0; i < change_list.get_num(); i++) {
    /* printf("change list %p\n", LMN_SATOM(change_list.get(i))); */
    dmem_root_alter_functor(root_d, child_mem,
                            (LmnSymbolAtomRef)(change_list.get(i)),
                            LMN_IN_PROXY_FUNCTOR);
  }
  change_list.destroy();

  for (i = 0; i < remove_list.get_num(); i++) {
    /* printf("insert proxy remove atom %s %p\n",
     * LMN_SATOM(remove_list.get(i))->str(),
     * LMN_SATOM(remove_list.get(i))); */
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)(remove_list.get(i));
    if (parent_d)
      dmem_remove_symbol_atom(parent_d, child_mem, a);
    else
      mem_remove_symbol_atom(child_mem, a);
    dmem_root_free_satom(root_d, a);
    /*    lmn_delete_atom(LMN_SATOM(remove_list.get(i))); */
  }
  remove_list.destroy();
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

  if (root_d->is_new_mem(mem))
    d = NULL;
  else
    d = root_d->get_mem_delta(mem);

  remove_list.init(16);

  DMEM_EACH_FUNC_ATOM(d, mem, LMN_STAR_PROXY_FUNCTOR, star, {
    outside = dmem_root_modified_atom(
        root_d, (LmnSymbolAtomRef)(star->get_link(0)));
    if (!remove_list.contains((LmnWord)star)) {
      if (d)
        dmem_unify_atom_args(d, mem, star, 1, outside, 1);
      else
        root_d->unify_atom_args(mem, star, 1, outside, 1);
      remove_list.push((LmnWord)star);
      remove_list.push((LmnWord)outside);
    }
  });
  for (i = 0; i < remove_list.get_num(); i++) {
    LmnSymbolAtomRef a = dmem_root_modified_atom(
        root_d, (LmnSymbolAtomRef)(remove_list.get(i)));
    if (d)
      dmem_remove_symbol_atom(d, mem, a);
    else {
      mem_remove_symbol_atom(mem, a);
      dmem_root_free_satom(root_d, a);
    }
  }

  remove_list.destroy();
}

void dmem_root_clear_ruleset(struct MemDeltaRoot *d, LmnMembraneRef m) {
  if (d->is_new_mem(m)) {
    dmem_clear_ruleset(d->get_mem_delta(m), m);
  }
}

LmnMembraneRef dmem_root_new_mem(struct MemDeltaRoot *d) {
  LmnMembraneRef m;

  m = new LmnMembrane();
  (d->proc_tbl)->proc_tbl_put_mem(m, (LmnWord)new NewMemInfo(m));
  d->flag_tbl->tbl_set_mem_flag(m, TAG_NEW_MEM);
  d->new_mems.push((vec_data_t)m);
  return m;
}

static struct NewMemInfo *dmem_root_get_new_mem_info(struct MemDeltaRoot *d,
                                                     LmnMembraneRef m) {
  LmnWord t = 0;

#ifdef DEBUG
  if (!d->is_new_mem(m)) lmn_fatal("unexpected");
#endif
  proc_tbl_get_by_mem(d->proc_tbl, m, &t);
  return (struct NewMemInfo *)t;
}

static LmnSymbolAtomRef dmem_root_alter_functor(struct MemDeltaRoot *root_d,
                                                LmnMembraneRef mem,
                                                LmnSymbolAtomRef atom,
                                                LmnFunctor f) {
  if (root_d->is_new_mem(mem)) {
    alter_functor(mem, atom, f);
    return atom;
  } else {
    struct MemDelta *d = root_d->get_mem_delta(mem);

    if (dmem_root_is_new_atom(root_d, atom)) {
      if (f > d->max_functor)
        d->max_functor = f;
      atom->set_functor(f);
      return atom;
    } else {
      LmnSymbolAtomRef new_atom = dmem_root_copy_satom(root_d, atom);
      LmnSymbolAtomRef a0;

#ifdef DEBUG
      if (LMN_PROXY_GET_MEM(atom) != mem) lmn_fatal("unexpected");
      if (dmem_root_is_modified_atom(root_d, atom)) lmn_fatal("unexpected");
#endif
      new_atom->set_functor(f);
      dmem_remove_symbol_atom(d, mem, atom);
      dmem_root_free_satom(root_d, atom);
      dmem_put_symbol_atom(d, mem, new_atom);

      a0 = (LmnSymbolAtomRef)(dmem_root_get_link(root_d, atom, 0));
      if (LMN_PROXY_GET_MEM(a0) != mem) {
        LmnMembraneRef m2 = LMN_PROXY_GET_MEM(a0);
#ifdef DEBUG
        if (root_d->is_new_mem(m2)) lmn_fatal("unexpected");
#endif
        a0 = dmem_modify_atom(root_d->get_mem_delta(m2), m2, a0);
      } else {
        a0 = dmem_modify_atom(d, mem,
                              (LmnSymbolAtomRef)(atom->get_link(0)));
      }
      lmn_newlink_in_symbols(new_atom, 0, a0, 0);

      if (!LMN_ATTR_IS_DATA(atom->get_attr(1))) {
        LmnSymbolAtomRef a1 = dmem_modify_atom(
            d, mem, (LmnSymbolAtomRef)(atom->get_link(1)));
        lmn_newlink_in_symbols(new_atom, 1, a1,
                               LMN_ATTR_GET_VALUE(atom->get_attr(1)));
      }
      return new_atom;
    }
  }
}

LmnAtomRef dmem_root_get_link(struct MemDeltaRoot *d, LmnSymbolAtomRef atom,
                              int i) {
  LmnSymbolAtomRef a;

  atom = (LmnSymbolAtomRef)dmem_root_modified_atom(d, atom);
  a = (LmnSymbolAtomRef)atom->get_link(i);
  if (LMN_ATTR_IS_DATA(atom->get_attr(i)))
    return a;
  else
    return dmem_root_modified_atom(d, a);
}

void dmem_root_dump(struct MemDeltaRoot *d) {
  int i;

  for (i = 0; i < d->mem_deltas.get_num(); i++) {
    dmem_dump((struct MemDelta *)d->mem_deltas.get(i));
  }
}

static void dmem_dump(struct MemDelta *d) {
  int i;

  /* if (!d) lmn_fatal("unexpected"); */

  /* for (i = 0; i < d->modified_atoms.get_num(); i+=2) { */
  /*   LmnSymbolAtomRef src = LMN_SATOM(d->modified_atoms.get(i)); */
  /*   LmnSymbolAtomRef new = LMN_SATOM(d->modified_atoms.get(i+1)); */

  /*   printf("modify %s %p -> %p\n", src->str(), src, new); */
  /* } */

  for (i = 0; i < d->new_atoms.get_num(); i++) {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)(d->new_atoms.get(i));
    int j;
    int arity = a->get_link_num();

    printf("new atom mem= %p, %p ", d->mem, a);

    printf("%s(", a->str());
    for (j = 0; j < arity; j++) {
      if (j > 0)
        printf(",");
      printf("%p", (LmnSymbolAtomRef)(a->get_link(j)));
    }
    printf(")\n");
  }

  for (i = 0; i < d->del_atoms.get_num(); i++) {
    printf("del atom %s %p\n",
           ((LmnSymbolAtomRef)(d->del_atoms.get(i)))->str(),
           (LmnSymbolAtomRef)(d->del_atoms.get(i)));
  }

  /* Membrane */

  for (i = 0; i < d->del_mems.get_num(); i++) {
    printf("del mem: %p\n", (LmnMembraneRef)d->del_mems.get(i));
    lmn_dump_mem_dev((LmnMembraneRef)d->del_mems.get(i));
  }

  for (i = 0; i < d->new_mems.get_num(); i++) {
    printf("new mem: %p\n", (LmnMembraneRef)d->new_mems.get(i));
    lmn_dump_mem_dev((LmnMembraneRef)d->new_mems.get(i));
  }
}

static LmnMembraneRef dmem_root_get_parent(struct MemDeltaRoot *root_d,
                                           LmnMembraneRef m) {
  if (root_d->is_new_mem(m))
    return m->mem_parent();
  else if (root_d->is_delta_mem(m)) {
    struct MemDelta *d = root_d->get_mem_delta(m);
    return d->new_parent;
  } else {
    return m->mem_parent();
  }
}

BOOL dmem_root_is_committed(struct MemDeltaRoot *root_d) {
  return root_d->committed;
}

void dmem_root_drop(struct MemDeltaRoot *root_d, LmnMembraneRef m) {
  if (root_d->is_new_mem(m))
    m->drop();
  else
    dmem_drop(root_d->get_mem_delta(m), m);
}

void dmem_root_set_mem_name(struct MemDeltaRoot *root_d, LmnMembraneRef m,
                            lmn_interned_str name) {
  if (root_d->is_new_mem(m))
    m->set_name(name);
  else {
    root_d->get_mem_delta(m)->new_name = name;
  }
}

void dmem_root_copy_rules(struct MemDeltaRoot *root_d, LmnMembraneRef dest,
                          LmnMembraneRef src) {
  if (root_d->is_new_mem(dest))
    dest->copy_rules(src);
  else {
    dmem_copy_rules(root_d->get_mem_delta(dest), dest, src);
  }
}

LmnSymbolAtomRef dmem_root_new_atom(struct MemDeltaRoot *d, LmnFunctor f) {
  LmnSymbolAtomRef atom = lmn_new_atom(f);

  if (atom->get_id() == 0)
    atom->set_id(env_gen_next_id());

  d->flag_tbl->tbl_set_atom_flag(atom, TAG_NEW_ATOM);
  return atom;
}

static inline LmnSymbolAtomRef dmem_root_copy_satom(struct MemDeltaRoot *d,
                                                    LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_atom;

  new_atom = lmn_copy_satom(atom);

  if (atom->get_id() == 0)
    new_atom->set_id(env_gen_next_id());

  d->flag_tbl->tbl_set_atom_flag(new_atom, TAG_NEW_ATOM);

  return new_atom;
}

static inline LmnSymbolAtomRef dmem_root_copy_satom_with_data(
    struct MemDeltaRoot *d, LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_atom = dmem_root_copy_satom(d, atom);
  unsigned int i, arity = atom->get_link_num();

  /* リンク先のデータアトムをコピーする */
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      new_atom->set_link(i,
                         (LmnAtomRef)lmn_copy_data_atom(
                             (LmnDataAtomRef)atom->get_link(i),
                             atom->get_attr(i)));
    }
  }
  return new_atom;
}

/* '='アトムのコピー用。ルール適用中に消されるので差分に含めないようにする */
static inline LmnSymbolAtomRef dmem_root_copy_eqatom_with_data(
    LmnSymbolAtomRef atom) {
  LmnSymbolAtomRef new_eqatom;
  unsigned int i, arity;

  new_eqatom = lmn_copy_satom(atom);
  new_eqatom->set_id(env_gen_next_id());

  /* リンク先のデータアトムをコピーする */
  arity = atom->get_link_num();
  for (i = 0; i < arity; i++) {
    if (LMN_ATTR_IS_DATA(atom->get_attr(i))) {
      new_eqatom->set_link(i,
                         (LmnAtomRef)lmn_copy_data_atom(
                             (LmnDataAtomRef)atom->get_link(i),
                             atom->get_attr(i)));
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
    ((LmnSymbolAtomRef)new_atom)->set_id(env_gen_next_id());
    d->flag_tbl->tbl_set_atom_flag((LmnSymbolAtomRef)new_atom, TAG_NEW_ATOM);
    return new_atom;
  }
}

static inline void dmem_root_free_satom(struct MemDeltaRoot *d,
                                        LmnSymbolAtomRef atom) {
  d->flag_tbl->tbl_set_atom_flag(dmem_root_modified_atom(d, atom),
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
  return d->flag_tbl->tbl_get_flag_by_atom(a, TAG_DEL_ATOM);
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
  return a->get_id() >= d->new_proc_id_lower_limit;
}

static inline BOOL dmem_root_is_modified_atom(struct MemDeltaRoot *d,
                                              LmnSymbolAtomRef a) {
  return d->flag_tbl->tbl_get_flag_by_atom(a, TAG_MODIFIED_ATOM);
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

  p->new_atoms.init(16);
  p->del_atoms.init(16);

  p->del_mems.init(16);
  p->new_mems.init(16);

  p->new_rulesets = NULL;
  p->org_rulesets = NULL;
  p->ruleset_removed = FALSE;

  p->new_proxies.init(16);

  p->max_functor = m->mem_max_functor();

  p->data_atom_diff = 0;

  p->new_parent = m->mem_parent();

  p->org_name = p->new_name = m->NAME_ID();
  return p;
}

static void mem_delta_free(struct MemDelta *p) {
  int i;

  p->del_mems.destroy();
  p->new_mems.destroy();
  p->del_atoms.destroy();

  for (i = 0; i < p->new_atoms.get_num(); i++) {
    free_symbol_atom_with_buddy_data(
        (LmnSymbolAtomRef)(p->new_atoms.get(i)));
  }
  p->new_atoms.destroy();

  if (p->new_rulesets) delete p->new_rulesets;

  p->new_proxies.destroy();

  LMN_FREE(p);
}

LmnMembraneRef dmem_mem(struct MemDelta *d) { return d->mem; }

static inline void dmem_remove_symbol_atom(struct MemDelta *d, LmnMembraneRef m,
                                           LmnSymbolAtomRef atom) {
#ifdef DEBUG
  if (dmem_root_is_modified_atom(d->root_d, atom)) lmn_fatal("unexpected");
  if (dmem_is_removed_atom(d, m, atom)) lmn_fatal("unexpected");
#endif
  /* if (dmem_is_new_atom(d, m, atom)) { */
  /*   sproc_tbl_unset_atom_flag(d->flag_tbl, atom, TAG_NEW_ATOM); */
  /* } else { */
  /*   d->flag_tbl->tbl_set_atom_flag(atom, TAG_DEL_ATOM); */
  /* } */

  d->del_atoms.push((vec_data_t)atom);
  (d->root_d->owner_tbl)->proc_tbl_put_atom(atom, 0);
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

static inline LmnSymbolAtomRef dmem_modify_atom(struct MemDelta *d,
                                                LmnMembraneRef mem,
                                                LmnSymbolAtomRef src) {
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
    (d->root_d->proc_tbl)->proc_tbl_put_atom(src, LMN_ATOM(new_atom));
    d->root_d->flag_tbl->tbl_set_atom_flag(src, TAG_MODIFIED_ATOM);

    d->root_d->modified_atoms.push((vec_data_t)src);
    d->root_d->modified_atoms.push((vec_data_t)new_atom);

    return new_atom;
  }
}

/* atomのリンク先がlに変わった */
static inline void dmem_modify_link(struct MemDelta *d, LmnMembraneRef m,
                                    LmnSymbolAtomRef atom, int i, LmnAtomRef l,
                                    LmnLinkAttr attr) {
  LmnSymbolAtomRef modified;

  modified = dmem_modify_atom(d, m, atom);

  modified->set_link(i, l);
  modified->set_attr(i, attr);
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
  LmnFunctor f = atom->get_functor();

  /* アトムへIDを付加する */
  if (atom->get_id() == 0) {
    atom->set_id(env_gen_next_id());
  }

  if (f > d->max_functor) d->max_functor = f;
#ifdef DEBUG
  if (dmem_is_removed_atom(d, m, atom)) lmn_fatal("unexpected");
  if (!dmem_root_is_new_atom(d->root_d, atom)) lmn_fatal("unexpected");
#endif
  d->new_atoms.push((vec_data_t)atom);
  (d->root_d->owner_tbl)->proc_tbl_put_atom(atom,
                    (LmnWord)m); /* IDをkeyにした配列へ投げる */
  /* sproc_tbl_unset_atom_flag(d->flag_tbl, atom, TAG_DEL_ATOM); */

  if (LMN_IS_PROXY_FUNCTOR(f)) { /* 更にproxyの場合は, 情報を追加 */
    LMN_PROXY_SET_MEM(atom, m);
    d->new_proxies.push((vec_data_t)atom);
  }
}

static void dmem_add_child_mem(struct MemDelta *d, LmnMembraneRef parent,
                               LmnMembraneRef child) {
  d->new_mems.push((vec_data_t)child);
}

static void dmem_add_ruleset(struct MemDelta *d, LmnMembraneRef m,
                             LmnRuleSetRef ruleset) {
  if (!d->new_rulesets) {
    d->new_rulesets = new Vector(16);
  }
  d->new_rulesets->push((vec_data_t)ruleset);
}

static inline void dmem_clear_ruleset(struct MemDelta *d, LmnMembraneRef m) {
  d->ruleset_removed = TRUE;
}

static void dmem_link_data_atoms(struct MemDelta *d, LmnMembraneRef m,
                                 LmnDataAtomRef d0, LmnLinkAttr attr0,
                                 LmnDataAtomRef d1, LmnLinkAttr attr1) {
  LmnSymbolAtomRef ap = dmem_root_new_atom(d->root_d, LMN_UNIFY_FUNCTOR);
  ap->set_link(0, (LmnAtomRef)d0);
  ap->set_link(1, (LmnAtomRef)d1);
  ap->set_attr(0, attr0);
  ap->set_attr(1, attr1);
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
                   for (i = 0; i < a->get_link_num(); i++) {
                     if (LMN_ATTR_IS_DATA(a->get_attr(i))) {
                       dmem_remove_atom(d, mem, a->get_link(i),
                                        a->get_attr(i));
                       dmem_root_free_atom(d->root_d, a->get_link(i), a->get_attr(i));
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

  ap0 = atom2->get_link(pos2);
  attr = atom2->get_attr(pos2);

  atom1 = dmem_modify_atom(d, m, (LmnSymbolAtomRef)(atom1));

  if (LMN_ATTR_IS_DATA(attr)) {
    ((LmnSymbolAtomRef)(atom1))->set_link(pos1, ap0);
    ((LmnSymbolAtomRef)(atom1))->set_attr(pos1, attr);
  } else {
    /* 最終的にapとatom1が接続する */
    LmnSymbolAtomRef ap = (LmnSymbolAtomRef)(ap0);

    if (LMN_IS_PROXY_FUNCTOR(ap->get_functor()) &&
        LMN_PROXY_GET_MEM(ap) != m) {
      if (d->root_d->is_new_mem(m)) {
        ap->set_link(LMN_ATTR_GET_VALUE(attr), atom1);
        ap->set_attr(LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos1));
      } else {
        LmnMembraneRef m2 = LMN_PROXY_GET_MEM(ap);
        struct MemDelta *d2 = d->root_d->get_mem_delta(m2);
        ap = dmem_modify_atom(d2, m2, (LmnSymbolAtomRef)(ap));

        ap->set_link(LMN_ATTR_GET_VALUE(attr), atom1);
        ap->set_attr(LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos1));
      }
    } else {
      ap = dmem_modify_atom(d, m, ap);
      ap->set_link(LMN_ATTR_GET_VALUE(attr), atom1);
      ap->set_attr(LMN_ATTR_GET_VALUE(attr), LMN_ATTR_MAKE_LINK(pos1));
    }

    atom1->set_link(pos1, ap);
    atom1->set_attr(pos1, attr);
  }
}

static void dmem_unify_atom_args(struct MemDelta *d, LmnMembraneRef m,
                                 LmnSymbolAtomRef atom1, int pos1,
                                 LmnSymbolAtomRef atom2, int pos2) {
  LmnSymbolAtomRef ap1, ap2;
  LmnLinkAttr attr1, attr2;
  LmnMembraneRef m1, m2;

  ap1 = (LmnSymbolAtomRef)atom1->get_link(pos1);
  ap2 = (LmnSymbolAtomRef)atom2->get_link(pos2);
  attr1 = atom1->get_attr(pos1);
  attr2 = atom2->get_attr(pos2);

  if (LMN_ATTR_IS_DATA(attr1) || LMN_ATTR_IS_DATA(attr2)) {
    if (LMN_ATTR_IS_DATA(attr1) && LMN_ATTR_IS_DATA(attr2)) {
      dmem_link_data_atoms(d->root_d->get_mem_delta(m), m,
                           (LmnDataAtomRef)ap1, attr1, (LmnDataAtomRef)ap2,
                           attr2);
    } else if (!LMN_ATTR_IS_DATA(attr1)) {
      m1 = LMN_IS_PROXY_FUNCTOR(ap1->get_functor())
               ? LMN_PROXY_GET_MEM(ap1)
               : m;

      dmem_modify_link(d->root_d->get_mem_delta(m1), m1,
                       (LmnSymbolAtomRef)(ap1), LMN_ATTR_GET_VALUE(attr1), ap2,
                       attr2);
    } else if (!LMN_ATTR_IS_DATA(attr2)) {
      m2 = LMN_IS_PROXY_FUNCTOR(ap2->get_functor())
               ? LMN_PROXY_GET_MEM(ap2)
               : m;
      dmem_modify_link(d->root_d->get_mem_delta(m2), m2,
                       (LmnSymbolAtomRef)(ap2), LMN_ATTR_GET_VALUE(attr2), ap1,
                       attr1);
    }
  } else {
    m1 = LMN_IS_PROXY_FUNCTOR(((LmnSymbolAtomRef)ap1)->get_functor())
             ? LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap1)
             : m;
    m2 = LMN_IS_PROXY_FUNCTOR(((LmnSymbolAtomRef)ap2)->get_functor())
             ? LMN_PROXY_GET_MEM((LmnSymbolAtomRef)ap2)
             : m;
    ap1 = dmem_modify_atom(d->root_d->get_mem_delta(m1), m1,
                           (LmnSymbolAtomRef)ap1);
    ap2 = dmem_modify_atom(d->root_d->get_mem_delta(m2), m2,
                           (LmnSymbolAtomRef)ap2);

    ((LmnSymbolAtomRef)ap1)->set_link(LMN_ATTR_GET_VALUE(attr1), ap2);
    ((LmnSymbolAtomRef)ap2)->set_link(LMN_ATTR_GET_VALUE(attr2), ap1);
    ((LmnSymbolAtomRef)ap1)->set_attr(LMN_ATTR_GET_VALUE(attr1), attr2);
    ((LmnSymbolAtomRef)ap2)->set_attr(LMN_ATTR_GET_VALUE(attr2), attr1);
  }
}

static inline void dmem_copy_rules(struct MemDelta *d, LmnMembraneRef dest,
                                   LmnMembraneRef src) {
  int i;

  if (!d->new_rulesets) {
    d->new_rulesets = new Vector(16);
  }

  for (i = 0; i < src->ruleset_num(); i++) {
    d->new_rulesets->push((vec_data_t)lmn_mem_get_ruleset(src, i));
  }
}

/* TODO: rename*/
static void dmem_commit(struct MemDelta *d) {
  int i;

  for (i = 0; i < d->new_atoms.get_num(); i++) {
    mem_push_symbol_atom(d->mem, (LmnSymbolAtomRef)(d->new_atoms.get(i)));
  }

  for (i = 0; i < d->del_atoms.get_num(); i++) {
    mem_remove_symbol_atom(d->mem,
                           (LmnSymbolAtomRef)(d->del_atoms.get(i)));
  }

  /* Membrane */

  //  int n = 0;
  //  for (i = 0; i < d->del_mems.get_num(); i++) {
  //    lmn_mem_remove_mem(d->mem, (LmnMembraneRef*)d->del_mems.get(i));
  //    n--;
  //  }

  for (i = 0; i < d->new_mems.get_num(); i++) {
    /* printf("commit new mem: %p\n", (LmnMembraneRef)d->new_mems.get(i));
     */
    /* lmn_dump_cell_stdout((LmnMembraneRef)d->new_mems.get(i)); */
    (d->mem)->add_child_mem((LmnMembraneRef)d->new_mems.get(i));
    //    n++;
  }

  //  printf("child num = %d\n", n);

  (d->mem)->data_atom_add(d->data_atom_diff);

  if (d->ruleset_removed || d->new_rulesets) {
    d->org_rulesets = new Vector(*(d->mem)->get_rulesets());

    if (d->ruleset_removed) {
      (d->mem)->get_rulesets()->clear();
    }

    if (d->new_rulesets) {
      for (i = 0; i < d->new_rulesets->get_num(); i++) {
        lmn_mem_add_ruleset(d->mem, (LmnRuleSetRef)d->new_rulesets->get(i));
      }
    }
  }

  (d->mem)->set_name(d->new_name);
}

static inline void dmem_commit_delete_mem(struct MemDelta *d) {
  int i;
  /* Membrane */
  for (i = 0; i < d->del_mems.get_num(); i++) {
    (d->mem)->remove_mem((LmnMembraneRef)d->del_mems.get(i));
  }
}

static void dmem_revert(struct MemDelta *d) {
  int i;

  for (i = 0; i < d->del_atoms.get_num(); i++) {
    mem_push_symbol_atom(d->mem, (LmnSymbolAtomRef)(d->del_atoms.get(i)));
  }

  for (i = 0; i < d->new_atoms.get_num(); i++) {
    mem_remove_symbol_atom(d->mem,
                           (LmnSymbolAtomRef)(d->new_atoms.get(i)));
  }

  /* Membrane */

  //  for (i = 0; i < d->new_mems.get_num(); i++) {
  //    lmn_mem_remove_mem(d->mem, (LmnMembraneRef*)d->new_mems.get(i));
  //  }

  for (i = 0; i < d->del_mems.get_num(); i++) {
    (d->mem)->add_child_mem((LmnMembraneRef)d->del_mems.get(i));
  }

  (d->mem)->data_atom_sub(d->data_atom_diff);

  if (d->ruleset_removed || d->new_rulesets) {
    (d->mem)->get_rulesets()->clear();
    for (i = 0; i < d->org_rulesets->get_num(); i++) {
      lmn_mem_add_ruleset(d->mem, (LmnRuleSetRef)d->org_rulesets->get(i));
    }

    delete d->org_rulesets;
    d->org_rulesets = NULL;
  }

  (d->mem)->set_name(d->org_name);
}

static inline void dmem_revert_new_mem(struct MemDelta *d) {
  int i;
  /* Membrane */
  for (i = 0; i < d->new_mems.get_num(); i++) {
    (d->mem)->remove_mem((LmnMembraneRef)d->new_mems.get(i));
  }
}
