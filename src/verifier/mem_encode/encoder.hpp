/*
 * encode.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 */

#ifndef SLIM_VERIFIER_MEM_ENCODE_ENCODER_HPP
#define SLIM_VERIFIER_MEM_ENCODE_ENCODER_HPP

#include "../visitlog.h"
#include "binstr.hpp"

#include "vm/vm.h"

#include <algorithm>
#include <vector>

namespace slim {
namespace verifier {
namespace mem_encode {
struct encoder {
  /* 膜memに存在する全てのアトムへのポインタをVectorに積めて返す.
   * アトムはfunctor_idの降順 */
  std::vector<LmnSymbolAtomRef> mem_atoms(LmnMembraneRef mem) {
    std::vector<LmnSymbolAtomRef> atoms;
    atoms.reserve(mem->symb_atom_num());

    for (int i = mem->mem_max_functor() - 1; i >= 0; i--) {
      auto ent = mem->get_atomlist(i);
      if (ent && !LMN_IS_PROXY_FUNCTOR(i)) {
        for (auto a : *ent) {
          atoms.push_back(a);
        }
      }
    }

    return atoms;
  }

  void write_mem_atoms(LmnMembraneRef mem, BinStrCursor &bsp,
                       VisitLogRef visited) {
    if (!bsp.is_valid())
      return;

    write_mols(mem_atoms(mem), bsp, visited);
  }

  /* write_atomsの膜バージョン.
   * ここで書き込む計算する分子には膜のみが含まれている */
  void write_mems(LmnMembraneRef mem, BinStrCursor &bsp, VisitLogRef visited) {
    BinStrCursor last_valid_bsp;
    CheckpointRef last_valid_checkpoint = nullptr;

    if (!bsp.is_valid())
      return;

    bool last_valid = false;
    for (auto m = mem->mem_child_head(); m; m = m->mem_next()) {
      if (visited->get_mem(m, NULL))
        continue;

      BinStrCursor new_bsptr = bsp;

      visited->set_checkpoint();

      write_mem(m, 0, -1, -1, new_bsptr, visited, TRUE);

      if (new_bsptr.is_valid()) {
        /* mからたどった分子が書き込みに成功したので、last_validに記憶する */
        if (last_valid) {
          delete last_valid_checkpoint;
        }
        last_valid_bsp = new_bsptr;
        last_valid_checkpoint = visited->pop_checkpoint();
        last_valid = true;
      } else {
        visited->revert_checkpoint();
      }
    }

    if (last_valid) {
      /* 書き込みに成功した分子をログに記録して、次の分子に進む */
      visited->push_checkpoint(last_valid_checkpoint);
      write_mems(mem, last_valid_bsp, visited);

      if (last_valid_bsp.is_valid()) {
        bsp = last_valid_bsp;
        visited->commit_checkpoint();
      } else {
        visited->revert_checkpoint();
      }
    }
  }

  /* 膜memの全てのアトムのバイナリストリングを書き込む.
   * 辿ってきたアトムfrom_atomとそのアトムの属性attrとこちらからのリンク番号fromを受け取る.
   */
  void write_mem(LmnMembraneRef mem, LmnAtomRef from_atom, LmnLinkAttr attr,
                 int from, BinStrCursor &bsp, VisitLogRef visited, BOOL is_id) {
    LmnWord n_visited;

    if (!bsp.is_valid())
      return;

    /* 訪問済み */
    if (visited->get_mem(mem, &n_visited)) {
      bsp.push_visited_mem(n_visited);

      if (from_atom) { /* 引き続きアトムをたどる */
        write_mol(from_atom, attr, from, bsp, visited, is_id);
      }
      return;
    }

    visited->put_mem(mem);
    bsp.push_start_mem(mem->NAME_ID());

    if (!bsp.is_valid())
      return;

    if (from_atom) {
      write_mol(from_atom, attr, from, bsp, visited, is_id);
    }

    /* アトム・膜・ルールセットの順に書込み */
    if (is_id) { /* 膜に対して一意なIDとなるバイナリストリングへエンコードする場合
                  */
      write_mem_atoms(mem, bsp, visited);
      write_mems(mem, bsp, visited);
    } else { /* 単なるバイナリストリングへエンコードする場合 */
      dump_mem_atoms(mem, bsp, visited);
      dump_mems(mem, bsp, visited);

      /* 膜memに存在するデータアトムを起点にしたinside
       * proxyアトムをちゃんと書き込んでおく */

      auto ent = mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);
      if (ent) {
        for (auto in : *ent) {
          if (!LMN_ATTR_IS_DATA(in->get_attr(1)) ||
              visited->get_atom(in, NULL)) {
            continue;
          }
          /* -------------------------+
           * [DATA ATOM]-0--1-[in]-0--|--0-[out]-1--..
           * -------------------------+
           */
          bsp.push_escape_mem_data(in->get_link(1),
                                   in->get_attr(1), visited);
          auto out = (LmnSymbolAtomRef)in->get_link(0);
          write_mol(out->get_link(1), out->get_attr(1),
                    LMN_ATTR_GET_VALUE(out->get_attr(1)), bsp,
                    visited, is_id);
        };
      }
    }

    write_rulesets(mem, bsp);
    bsp.push_end_mem();
  }

  /* アトムatomをバイナリストリングへ書き込む
   * 入力:
   *   アトムatomと, atomのリンク属性attr,
   *   アトムatomへ辿ってきた際に辿ったリンクと接続するリンク番号from,
   *   エンコード領域bsp, 訪問管理visited,
   * is_idは計算するバイナリストリングがmem_idならば真 */
  void write_mol(LmnAtomRef atom, LmnLinkAttr attr, int from, BinStrCursor &bsp,
                 VisitLogRef visited, BOOL is_id) {
    LmnWord n_visited;

    if (!bsp.is_valid())
      return;

    /* データアトムの場合 */
    if (LMN_ATTR_IS_DATA(attr)) {
      bsp.push_data_atom(atom, attr, visited);
      return;
    }

    auto satom = reinterpret_cast<LmnSymbolAtomRef>(atom);
    auto f = satom->get_functor();
    if (f == LMN_OUT_PROXY_FUNCTOR) {
      /* outside proxyの場合, inside proxy側の膜をwrite_memで書き込む */
      auto in = (LmnSymbolAtomRef)satom->get_link(0);
      auto in_mem = LMN_PROXY_GET_MEM(in);
      if (visited->get_atom(in, NULL)) {
        visited->put_atom(in);
      }
      write_mem(in_mem, in->get_link(1), in->get_attr(1),
                LMN_ATTR_GET_VALUE(in->get_attr(1)), bsp, visited,
                is_id);
    } else if (f == LMN_IN_PROXY_FUNCTOR) {
      /* inside proxyの場合, 親膜へ抜ける旨を示すタグTAG_ESCAPE_MEMを書き込む.
       * その後, outside proxyから分子のトレース(write_mol)を引き続き実行する */
      auto out = (LmnSymbolAtomRef)satom->get_link(0);
      bsp.push_escape_mem();

      if (visited->get_atom(satom, NULL)) {
        visited->put_atom(satom);
      }

      write_mol(out->get_link(1), out->get_attr(1),
                LMN_ATTR_GET_VALUE(out->get_attr(1)), bsp, visited,
                is_id);
    } else if (!visited->get_atom(satom, &n_visited)) {
      /* 未訪問のシンボルアトムの場合 */
      visited->put_atom(satom);
      bsp.push_atom(satom);
      if (!bsp.is_valid())
        return;

      auto arity = LMN_FUNCTOR_GET_LINK_NUM(f);
      for (auto i_arg = 0; i_arg < arity; i_arg++) {
        if (i_arg == from) { /* 辿ってきたリンクに接続 */
          bsp.push_from();
          continue;
        }
        write_mol(satom->get_link(i_arg),
                  satom->get_attr(i_arg),
                  LMN_ATTR_GET_VALUE(satom->get_attr(i_arg)), bsp,
                  visited, is_id);
      }
    } else {
      /* 訪問済のシンボルアトムの場合 */
      bsp.push_visited_atom(n_visited, from);
    }
  }

  /* atomsに含まれるアトムを起点とする未訪問分子を、バイナリストリングが
     最小となるように書き込む */
  void write_mols(std::vector<LmnSymbolAtomRef> atoms, BinStrCursor &bsp,
                  VisitLogRef visited) {
    BinStrCursor last_valid_bsp;
    int last_valid_i = -1, first_func = 0;
    CheckpointRef last_valid_checkpoint = NULL;

    if (!bsp.is_valid())
      return;

    /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
    for (int i = 0; i < atoms.size(); i++) {
      LmnSymbolAtomRef atom = atoms[i];

      if (!atom || LMN_IS_HL(atom))
        continue;

      /* 最適化: 最小のファンクタ以外は試す必要なし */
      if (last_valid_i >= 0 && atom->get_functor() != first_func)
        break;

      if (visited->get_atom(atom, NULL))
        continue;

      BinStrCursor new_bsptr = bsp;
      visited->set_checkpoint();

      write_mol(atom, LMN_ATTR_MAKE_LINK(0), -1, new_bsptr, visited, TRUE);
      if (new_bsptr.is_valid()) {
        /* atomからたどった分子が書き込みに成功したので、last_validに記憶する */
        if (last_valid_i < 0) {
          first_func = atom->get_functor();
        } else {
          delete last_valid_checkpoint;
        }

        last_valid_bsp = new_bsptr;
        last_valid_checkpoint = visited->pop_checkpoint();
        last_valid_i = i;
      } else {
        visited->revert_checkpoint();
      }
    }

    if (last_valid_i >= 0) {
      /* 書き込みに成功した分子をログに記録して、次の分子に進む */
      auto t = atoms[last_valid_i];
      atoms[last_valid_i] = 0;
      visited->push_checkpoint(last_valid_checkpoint);
      write_mols(atoms, last_valid_bsp, visited);
      atoms[last_valid_i] = t;

      if (last_valid_bsp.is_valid()) {
        bsp = last_valid_bsp;
        visited->commit_checkpoint();
      } else {
        visited->revert_checkpoint();
      }
    }
  }

  void write_rulesets(LmnMembraneRef mem, BinStrCursor &bsp) {
    /* ルールセットがルールセットIDでソートされていることに基づいたコード */
    auto n = mem->ruleset_num();
    if (n == 0)
      return;

    bool has_uniq = FALSE;
    /* TODO: uniqルールセットが存在するか否かを検査するためだけに
     *       O(ルールセット数)かかってしまうため.
     *       ルールの移動や複製を行うプログラムで非効率 */
    for (auto i = 0; i < n; i++) {
      if (lmn_mem_get_ruleset(mem, i)->has_unique()) {
        has_uniq = TRUE;
        break;
      }
    }

    if (!has_uniq) {
      bsp.push_start_rulesets(n);

      for (auto i = 0; i < n; i++) {
        bsp.push_ruleset(lmn_mem_get_ruleset(mem, i));
      }
    } else {
      bsp.push_ruleset_uniq(mem, n);
    }
  }

  /* 膜memに存在する全てのアトムをファンクタIDの降順の列で求め,
   * 求めた列をdump_molsする */
  void dump_mem_atoms(LmnMembraneRef mem, BinStrCursor &bsp,
                      VisitLogRef visited) {
    dump_mols(mem_atoms(mem), bsp, visited);
  }

  /* アトム列atomsから, visitedに未登録のアトムに対し, write_molを行う
   * つまり, mhash同様に, 各アトムを起点とした分子単位でエンコードを行っている
   */
  void dump_mols(const std::vector<LmnSymbolAtomRef> &atoms, BinStrCursor &bsp,
                 VisitLogRef visited) {
    /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
    for (auto atom : atoms) {
      if (visited->get_atom(atom, NULL) || LMN_IS_HL(atom))
        continue;

      write_mol((LmnAtomRef)atom, LMN_ATTR_MAKE_LINK(0), -1, bsp, visited,
                FALSE);
    }
  }

  /* 膜中心の計算単位. 未訪問の子膜Xに対して, 子膜の分子を書き込み,
   * dump_memsへ再起する 兄弟膜は子膜内のプロセスを全て書き込んでから訪問される
   */
  void dump_mems(LmnMembraneRef mem, BinStrCursor &bsp,
                        VisitLogRef visited) {
    for (auto m = mem->mem_child_head(); m; m = m->mem_next()) {
      if (!visited->get_mem(m, NULL)) {
        write_mem(m, 0, -1, -1, bsp, visited, FALSE);
      }
    }
  }

  static LmnBinStr *encode(LmnMembraneRef mem, unsigned int tbl_size = 0 /* hint */) {
    encoder e(mem, false, tbl_size);
    auto &visited = e.visit_log;
    auto &bs = e.binstr;
    auto &bsp = e.cur;

    e.write_mem_atoms(mem, *bsp, visited);
    e.write_mems(mem, *bsp, visited);
    e.write_rulesets(mem, *bsp);

    return e.binary_string();
  }

  static LmnBinStr *dump(LmnMembraneRef mem, unsigned long tbl_size = 0 /* hint */) {
    encoder e(mem, true, tbl_size);
    auto &visitlog = e.visit_log;
    auto &bs = e.binstr;
    auto &bsp = e.cur;

    e.dump_mem_atoms(mem, *bsp, visitlog); /* 1. アトムから */
    e.dump_mems(mem, *bsp, visitlog);      /* 2. 子膜から */
    e.write_rulesets(mem, *bsp);           /* 3. 最後にルール */

    return e.binary_string();
  }

  LmnMembraneRef root_mem;
  VisitLog *visit_log;
  BinStr binstr;
  std::unique_ptr<BinStrCursor> cur;

  encoder(LmnMembraneRef mem, bool direct, unsigned int tbl_size = 0) : root_mem(mem), visit_log(new VisitLog()) {
    cur = direct ? binstr.head_direct() : binstr.head();
    visit_log->init_with_size(tbl_size);
  }

  ~encoder() {
    delete visit_log;
  }

  LmnBinStr *binary_string() {
    return binstr.to_lmn_binstr();
  }
};
} // namespace mem_encode
} // namespace verifier
} // namespace slim

#endif /* SLIM_VERIFIER_MEM_ENCODE_ENCODER_HPP */
