/*
 * decoder.cpp
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

#include "decoder.hpp"

int binstr_decoder::decode_cell(LmnMembraneRef mem, LmnSymbolAtomRef from_atom,
                                int from_arg) {
  for (int i = 0; scanner.location() < scanner.size; i++) {
    auto tag = scanner.scan_tag();

    if (tag == TAG_MEM_END) {
      /* 膜の終わり */
      break;
    } else if (tag == TAG_RULESET1) {
      /* ルールセット(only 1) */
      auto rs_id = scanner.scan_ruleset();
      lmn_mem_add_ruleset(mem, LmnRuleSetTable::at(rs_id));
    } else if (tag == TAG_RULESET) {
      /* 複数のルールセット */
      auto n = scanner.scan_ruleset_num();
      for (auto j = 0; j < n; j++)
        lmn_mem_add_ruleset(mem, LmnRuleSetTable::at(scanner.scan_ruleset()));
    } else if (tag == TAG_RULESET_UNIQ) {
      auto rs_num = scanner.scan_ruleset_num();
      decode_rulesets(rs_num, lmn_mem_get_rulesets(mem));
    } else {
      scanner.unput_tag();
      /* 最初の要素は膜の外からアトムをたどって来た可能性がある */
      /* それ以外は、アトムからたどられて到達されていない */
      decode_mol(mem, (i == 0) ? from_atom : nullptr, from_arg);
    }
  }
  return scanner.location();
}

/* UNIQ制約を含むルールセットrulesetsを再構築する */
void binstr_decoder::decode_rulesets(int rs_num, Vector *rulesets) {
  for (auto i = 0; i < rs_num; i++) {
    auto rs = new LmnRuleSet(*LmnRuleSetTable::at(scanner.scan_ruleset()));

    for (auto j = 0; j < rs->size(); j++) {
      /* ruleset idから復元したrulesetには既に履歴が存在しており,
       * 履歴ごと複製した可能性がある. そのため,
       * バイナリスストリングから履歴をデコードする前に,
       * ruleset上の履歴を一旦解放する必要がある. MEMO: 現実装では,
       * コピー元となるルールセットオブジェクトに直接履歴を持たせていないため,
       *       上記コメントは考慮しなくてよい. */

      auto r = rs->get_rule(j);
      auto his_num = scanner.scan_history_num();

      for (auto k = 0; k < his_num; k++) {
        auto id = scanner.scan_history();
        r->add_history(id);
      }
    }
    lmn_mem_add_ruleset_sort(rulesets, rs);
  }
}

int binstr_decoder::decode_mol(LmnMembraneRef mem, LmnSymbolAtomRef from_atom,
                               int from_arg) {
  if (scanner.location() >= scanner.size)
    return scanner.location();

  auto tag = scanner.scan_tag();

  lmn_interned_str mem_name = ANONYMOUS;

  switch (tag) {
  case TAG_ATOM_START:
    return decode_atom(mem, from_atom, from_arg);
  case TAG_NAMED_MEM_START:
    mem_name = scanner.scan_mem_name();
    /* FALL THROUGH */
  case TAG_MEM_START: {
    auto new_mem = lmn_mem_make();
    lmn_mem_set_name(new_mem, mem_name);
    lmn_mem_set_active(new_mem, TRUE);
    lmn_mem_add_child_mem(mem, new_mem);

    log[(nvisit)].v = (LmnWord)new_mem;
    log[(nvisit)].type = BS_LOG_TYPE_MEM;
    (nvisit)++;

    if (from_atom) {
      auto in = lmn_mem_newatom(new_mem, LMN_IN_PROXY_FUNCTOR);
      auto out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
      lmn_newlink_in_symbols(in, 0, out, 0);
      lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
      return decode_cell(new_mem, in, 1);
    } else {
      return decode_cell(new_mem, from_atom, from_arg);
    }
  } break;
  case TAG_MEM_END:
    break;
  case TAG_ESCAPE_MEM_DATA: {
    LmnWord n;
    LmnLinkAttr n_attr;

    auto in = lmn_mem_newatom(mem, LMN_IN_PROXY_FUNCTOR);
    auto out = lmn_mem_newatom(lmn_mem_parent(mem), LMN_OUT_PROXY_FUNCTOR);
    auto sub_tag = scanner.scan_tag();

    if (sub_tag == TAG_INT_DATA) {
      n = scanner.scan_integer();
      n_attr = LMN_INT_ATTR;
    } else if (sub_tag == TAG_DBL_DATA) {
      n = scanner.scan_double();
      n_attr = LMN_DBL_ATTR;
    } else if (sub_tag == TAG_STR_DATA) {
      lmn_interned_str n_id = scanner.scan_strid();
      n = (LmnWord)lmn_string_make(lmn_id_to_name(n_id));
      n_attr = LMN_STRING_ATTR;
    } else {
      n = 0;
      n_attr = 0; /* false positive対策 */
      lmn_fatal("unexpected");
    }

    /* -----------------+
     * [n]-0--1-[in]-0--|--0-[out]-1--?-..
     * -----------------+
     */
    lmn_newlink_in_symbols(in, 0, out, 0);
    LMN_SATOM_SET_LINK(in, 1, (LmnAtomRef)n);
    LMN_SATOM_SET_ATTR(in, 1, n_attr);
    lmn_mem_push_atom(mem, (LmnAtomRef)n, n_attr);
    return decode_mol(lmn_mem_parent(mem), out, 1);
  }
  case TAG_ESCAPE_MEM: {
    LmnMembraneRef parent = lmn_mem_parent(mem);
    if (from_atom) {
      auto in = lmn_mem_newatom(mem, LMN_IN_PROXY_FUNCTOR);
      auto out = lmn_mem_newatom(parent, LMN_OUT_PROXY_FUNCTOR);
      lmn_newlink_in_symbols(in, 0, out, 0);
      lmn_newlink_in_symbols(in, 1, from_atom, from_arg);

      return decode_mol(parent, out, 1);
    } else {
      return decode_mol(parent, NULL, 1);
    }
  }
  case TAG_HLINK: {
    LmnSymbolAtomRef hl_atom = lmn_hyperlink_new();
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    printf("location-1=%d\n", scanner.location()-1);
    pos_to_id->at(scanner.location()-1).second=LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(hl_atom));
    log[(nvisit)].v = (LmnWord)hl_atom;
    log[(nvisit)].type = BS_LOG_TYPE_HLINK;
    (nvisit)++;

    lmn_mem_push_atom(mem, hl_atom, LMN_HL_ATTR);
    lmn_mem_newlink(mem, from_atom, LMN_ATTR_GET_VALUE((LmnWord)from_atom),
                    from_arg, hl_atom, LMN_HL_ATTR, 0);
    scanner.scan_hlink_num();

    tag = scanner.scan_tag();
    switch (tag) {
    case TAG_FROM:
      break;
    case TAG_ATOM_START: {
      LmnFunctor f = scanner.scan_functor();
      ;
      LmnSymbolAtomRef atom = lmn_new_atom(f);
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), atom, 0);
    } break;
    case TAG_INT_DATA: {
      long n = scanner.scan_integer();
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), (LmnAtomRef)n,
                             LMN_INT_ATTR);
    } break;
    case TAG_DBL_DATA: {
      LmnAtomRef n = (LmnAtomRef)lmn_create_double_atom(scanner.scan_double());
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), n, LMN_DBL_ATTR);
    } break;
    case TAG_STR_DATA: {
      auto n = scanner.scan_strid();
      auto str = lmn_string_make(lmn_id_to_name(n));
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), (LmnAtomRef)str,
                             LMN_SP_ATOM_ATTR);
    } break;
    default:
      printf("tag = %d\n", tag);
      lmn_fatal("binstr decode, unexpected");
      break;
    }
    return scanner.location();
  } break;
  case TAG_VISITED_ATOMHLINK:
  case TAG_VISITED_MEM: {
    int pos=scanner.location()-1;
    unsigned int ref = scanner.scan_ref_num();

    switch (log[ref].type) {
    case BS_LOG_TYPE_ATOM: {
      unsigned int arg = scanner.scan_arg_ref();
      LmnSymbolAtomRef atom = (LmnSymbolAtomRef)log[ref].v;
      if (from_atom) {
        lmn_newlink_in_symbols(atom, arg, from_atom, from_arg);
      }
    } break;
    case BS_LOG_TYPE_MEM: {
      LmnMembraneRef ref_mem = (LmnMembraneRef)log[ref].v;
      if (!from_atom) {
        return decode_mol(ref_mem, NULL, from_arg);
      } else {
        LmnSymbolAtomRef in, out;

        in = lmn_mem_newatom(ref_mem, LMN_IN_PROXY_FUNCTOR);
        out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);

        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
        return decode_mol(ref_mem, in, 1);
      }
    } break;
    case BS_LOG_TYPE_HLINK: {
      auto ref_hl_atom = (LmnAtomRef)log[ref].v;
      auto hl_atom = (LmnSymbolAtomRef)lmn_copy_atom(ref_hl_atom, LMN_HL_ATTR);

      lmn_newlink_in_symbols(hl_atom, 0, from_atom, from_arg);
      lmn_mem_push_atom(mem, hl_atom, LMN_HL_ATTR);
      lmn_mem_newlink(mem, from_atom, LMN_ATTR_GET_VALUE((LmnWord)from_atom),
                      from_arg, hl_atom, LMN_HL_ATTR, 0);
      // printf("%s:%d\n", __FUNCTION__, __LINE__);
      // printf("pos=%d\n", pos);
      // pos_to_id->at(pos).second=LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(hl_atom));
    } break;
    default:
      lmn_fatal("unexpected reference");
      break;
    }
    return scanner.location();
  }
  case TAG_INT_DATA: {
    long n = scanner.scan_integer();
    LMN_SATOM_SET_LINK(from_atom, from_arg, (LmnAtomRef)n);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_INT_ATTR);
    lmn_mem_push_atom(mem, (LmnAtomRef)n, LMN_INT_ATTR);
  } break;
  case TAG_DBL_DATA: {
    LmnAtomRef n = (LmnAtomRef)lmn_create_double_atom(scanner.scan_double());
    LMN_SATOM_SET_LINK(from_atom, from_arg, n);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_DBL_ATTR);
    lmn_mem_push_atom(mem, n, LMN_DBL_ATTR);
  } break;
  case TAG_STR_DATA: {
    lmn_interned_str n = scanner.scan_strid();
    LmnStringRef str = lmn_string_make(lmn_id_to_name(n));
    LMN_SATOM_SET_LINK(from_atom, from_arg, str);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_SP_ATOM_ATTR);
    lmn_mem_push_atom(mem, str, LMN_STRING_ATTR);
  } break;
  default:
    printf("tag = %d\n", tag);
    lmn_fatal("binstr decode, unexpected");
    break;
  }

  return scanner.location();
}

/* bsの位置posから膜memにデコードしたアトムを書き込む
 * *nvisitは出現番号
 * 辿って来た場合, from_atomとそのリンク番号が渡される */
int binstr_decoder::decode_atom(LmnMembraneRef mem, LmnSymbolAtomRef from_atom,
                                int from_arg) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  int pos = scanner.location()-1;
  std::pair<int,int> pa = pos_to_id->at(pos);
  printf("%d %d\n", pa.first, pa.second);
  auto f = scanner.scan_functor();

  auto atom = lmn_mem_newatom(mem, f); /* アトムを生成する */
  pos_to_id->at(pos).second=LMN_SATOM_ID(atom);
  log[(nvisit)].v = (LmnWord)atom; /* アドレスを記録(*nvisitは初期値1) */
  log[(nvisit)].type = BS_LOG_TYPE_ATOM;
  (nvisit)++;

  for (auto i = 0; i < LMN_FUNCTOR_ARITY(f); i++)
    LMN_SATOM_SET_LINK(atom, i, 0);

  for (auto i = 0; i < LMN_FUNCTOR_ARITY(f); i++) {
    unsigned int tag = scanner.scan_tag();

    if (tag == TAG_FROM) {
      lmn_newlink_in_symbols(from_atom, from_arg, atom, i);
    } else {
      scanner.unput_tag();
      bool visited = LMN_SATOM_GET_LINK(atom, i);
      /* すでにリンクが設定されているので、相手側から訪問済み */
      decode_mol(mem, visited ? nullptr : atom, i);
    }
  }

  return scanner.location();
}
