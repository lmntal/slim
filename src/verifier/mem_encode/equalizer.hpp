/*
 * equalizer.hpp
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

#ifndef SLIM_VERIFIER_MEM_ENCODE_EQUALIZER_HPP
#define SLIM_VERIFIER_MEM_ENCODE_EQUALIZER_HPP

#include "../visitlog.h"
#include "decoder.hpp"
#include "lmn_binstr.hpp"

struct equalizer_base {
  /* TAG_RULESET1 */
  BOOL mem_eq_enc_ruleset(LmnBinStrRef bs, int *i_bs, LmnRuleSetRef rs) {
    auto id = binstr_get_ruleset(bs->v, *i_bs);

    if (id != rs->id)
      return FALSE;

    (*i_bs) += BS_RULESET_SIZE;
    return TRUE;
  }

  /* TAG_RULESETS */
  BOOL mem_eq_enc_rulesets(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem) {
    auto n = binstr_get_ruleset_num(bs->v, *i_bs);
    if (n != mem->ruleset_num())
      return FALSE;

    (*i_bs) += BS_RULESET_NUM_SIZE;

    for (auto i = 0; i < n; i++) {
      if (!mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, i)))
        return FALSE;
    }
    return TRUE;
  }

  BOOL mem_eq_enc_rulesets_uniq(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem) {
    auto rs_num = binstr_get_ruleset_num(bs->v, *i_bs);
    if (rs_num != mem->ruleset_num())
      return FALSE;
    (*i_bs) += BS_RULESET_NUM_SIZE;

    /* TODO: on-the-flyにできるはず */
    std::vector<LmnRuleSet *> rulesets;
    binstr_decoder            dec(bs->v, bs->len, *i_bs);
    dec.decode_rulesets(rs_num, &rulesets);
    *i_bs = dec.scanner.location();

    auto result = lmn_rulesets_equals(rulesets, mem->get_rulesets());

    lmn_mem_rulesets_destroy(rulesets);

    return result;
  }
};

template <typename Log> struct equalizer : public equalizer_base {
  int mem_eq_enc_mols(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, BsDecodeLog *ref_log, int *i_ref, Log *log);
};

template <> struct equalizer<TraceLog> : public equalizer_base {
  LmnBinStrRef   bs;
  int            i_bs_;
  int           *i_bs;
  LmnMembraneRef mem;
  int           *i_ref;
  int            i_ref_;
  TraceLog      *log;
  struct bt_counter {
    int bs_pos;
    int refcon;
  };

  equalizer(LmnBinStrRef bs, LmnMembraneRef mem)
      : bs(bs), i_bs_(0), i_bs(&i_bs_), mem(mem), i_ref_(VISITLOG_INIT_N), i_ref(&i_ref_), log(new TraceLog) {
    log->visit_mem(mem, TLOG_MATCHED_ID_NONE);
  }
  ~equalizer() { delete log; }

  int check() { return mem_eq_enc_mols(i_bs, mem, i_ref); }

private:
  bool log_contains(TraceLog *log, LmnSymbolAtomRef atom) { return tracelog_contains_atom(log, atom); }
  bool log_contains(TraceLog *log, LmnMembraneRef atom) { return tracelog_contains_mem(log, atom); }
  void log_set_backtrack_point(TraceLog *log) { log->set_btpoint(); }
  void log_continue_trace(TraceLog *log) { log->continue_trace(); }
  void log_backtrack(TraceLog *log) { log->backtrack(); }

  BOOL mem_eq_enc_end(LmnMembraneRef mem, BOOL rule_flag) {
    if (!rule_flag && mem->ruleset_num() != 0)
      return false;

    return log->eq_traversed_proc_num(mem, mem->get_atomlist(LMN_IN_PROXY_FUNCTOR),
                                      mem->get_atomlist(LMN_EXCLAMATION_FUNCTOR));
  }

  int mem_eq_enc_mols(int *i_bs, LmnMembraneRef mem, int *i_ref) {
    BOOL rule_flag = FALSE;
    while (*i_bs < bs->len) {
      auto tag = BS_GET(bs->v, *i_bs);
      (*i_bs)++;

      switch (tag) {
      case TAG_ATOM_START: {
        auto f = binstr_get_functor(bs->v, *i_bs);

        /* 1. 読み出したファンクタのエントリリストを取得する */
        auto ent = mem->get_atomlist(f);
        if (!ent)
          return FALSE;

        for (auto atom : *ent) {
          /* 2. 未チェックのアトムを選択する */
          if (log_contains(log, atom))
            continue;
          log_set_backtrack_point(log);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          /* 3. 未チェックのアトムを起点にトレースを行う.
           * 　 失敗した場合は読み出し位置を現時点にバックトラックさせるため,
           * tmp変数を使用
           * 4. この先の探索で失敗したらバックトラック */
          if (mem_eq_enc_atom(&tmp_i_bs, mem, atom, LMN_ATTR_MAKE_LINK(0), &tmp_i_ref) &&
              mem_eq_enc_mols(&tmp_i_bs, mem, &tmp_i_ref)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            return true;
          } else {
            log_backtrack(log);
          }
        }

        return false;
      } break;
      case TAG_NAMED_MEM_START:
      case TAG_MEM_START: {
        lmn_interned_str mem_name = ANONYMOUS;

        if (tag == TAG_NAMED_MEM_START) {
          mem_name = binstr_get_mem_name(bs->v, *i_bs);
          (*i_bs)  += BS_MEM_NAME_SIZE;
        }

        for (auto m = mem->mem_child_head(); m; m = m->mem_next()) {
          /* 1. 未チェックの子膜を選択する. 同時に膜名チェックを行う */
          if (m->NAME_ID() != mem_name || log_contains(log, m))
            continue;
          log_set_backtrack_point(log);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          /* 膜を起点にした判定を行う.
           * 1. 子以下の階層を検査
           * 2. この先の探索に失敗したらバックトラック　@rev.450 */
          if (mem_eq_enc_mem(&tmp_i_bs, m, &tmp_i_ref) && mem_eq_enc_mols(&tmp_i_bs, mem, &tmp_i_ref)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            return true;
          } else {
            log_backtrack(log);
          }
        }

        return false;
      } break;
      case TAG_ESCAPE_MEM_DATA: {
        auto sub_tag = BS_GET(bs->v, *i_bs);
        (*i_bs)++;
        auto ent = mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);
        if (!ent)
          return FALSE;

        for (auto in : *ent) {
          /* -----------------+
           * [n]-0--1-[in]-0--|--0-[out]-1--?-..
           * -----------------+
           */

          auto data      = (LmnSymbolAtomRef)in->get_link(1);
          auto data_attr = in->get_attr(1);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          if (!LMN_ATTR_IS_DATA(data_attr) || !mem_eq_enc_data_atom(sub_tag, &tmp_i_bs, data, data_attr)) {
            /* nがデータアトムでない, もしくは等しいデータアトムでない */
            continue;
          }

          /* シンボルアトムを起点としていないため, バックトラックポインtがない.
           * proxyをバックトラックポイントにする必要がある */
          log_set_backtrack_point(log);

          if (mem_eq_enc_escape_mem(&tmp_i_bs, mem, in, LMN_ATTR_MAKE_LINK(0), &tmp_i_ref) &&
              mem_eq_enc_mols(&tmp_i_bs, mem, &tmp_i_ref)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            return true;
            break;
          } else {
            log_backtrack(log);
          }
        }

        return false;
      } break;
      case TAG_MEM_END:
        return mem_eq_enc_end(mem, rule_flag);
      case TAG_RULESET1:
        if ((mem->ruleset_num() != 1) || !mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, 0))) {
          return FALSE;
        }
        rule_flag = TRUE;
        break;
      case TAG_RULESET:
        if (!mem_eq_enc_rulesets(bs, i_bs, mem))
          return FALSE;
        rule_flag = TRUE;
        break;
      case TAG_RULESET_UNIQ:
        if (!mem_eq_enc_rulesets_uniq(bs, i_bs, mem))
          return FALSE;
        rule_flag = TRUE;
        break;
      default:
        lmn_fatal("unexpected");
        break;
      }
    }

    return mem_eq_enc_end(mem, rule_flag);
  }

  BOOL mem_eq_enc_atom(int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref) {
    if (LMN_ATTR_IS_DATA(attr))
      return FALSE;

    auto satom = reinterpret_cast<LmnSymbolAtomRef>(atom);

    /* 1. ファンクタを取得 */
    auto f  = binstr_get_functor(bs->v, *i_bs);
    (*i_bs) += BS_FUNCTOR_SIZE;

    /* アトムatomがファンクタfのアトムでない場合,
     * 既にチェック済みのアトムの場合, FALSEを返す */

    if (f != satom->get_functor())
      return FALSE;

    if (!log->visit_atom(satom, *i_ref, mem))
      return FALSE;

    (*i_ref)++;

    /* アトムatomの接続先を検査する */
    for (auto i = 0; i < LMN_FUNCTOR_ARITY(lmn_functor_table, f); i++) {
      if (!mem_eq_enc_mol(i_bs, mem, satom->get_link(i), satom->get_attr(i), i_ref)) {
        return FALSE;
      }
    }

    return TRUE;
  }

  BOOL mem_eq_enc_traced_mem(BOOL is_named, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr,
                             int *i_ref) {
    if (LMN_ATTR_IS_DATA(attr))
      return false;

    auto satom = reinterpret_cast<LmnSymbolAtomRef>(atom);
    if (satom->get_functor() != LMN_OUT_PROXY_FUNCTOR)
      return false;

    /* 子膜側のinside proxyアトムを取得 */
    auto in     = (LmnSymbolAtomRef)((LmnSymbolAtomRef)atom)->get_link(0);
    auto in_mem = LMN_PROXY_GET_MEM(in);

    if (is_named) {
      const lmn_interned_str mem_name = binstr_get_mem_name(bs->v, *i_bs);
      (*i_bs)                         += BS_MEM_NAME_SIZE;
      if (mem_name != in_mem->NAME_ID()) {
        return FALSE;
      }
    }

    log->visit_mem(in_mem, *i_ref);
    log->visit_atom(in, TLOG_MATCHED_ID_NONE, in_mem);
    (*i_ref)++;

    /* 1. mem_eq_enc_mol : 引き続き子膜側へ踏み込んで連結分子をトレース
     * 2. mem_eq_enc_mols: 1のトレースに成功したならば,
     * 残りの子膜のコンテンツをトレース. */
    return mem_eq_enc_mol(i_bs, in_mem, in->get_link(1), in->get_attr(1), i_ref) &&
           mem_eq_enc_mols(i_bs, in_mem, i_ref);
  }

  BOOL mem_eq_enc_mem(int *i_bs, LmnMembraneRef mem, int *i_ref) {
    if (!log->visit_mem(mem, *i_ref))
      return FALSE;
    (*i_ref)++;

    return mem_eq_enc_mols(i_bs, mem, i_ref);
  }

  BOOL mem_eq_enc_data_atom(unsigned int tag, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr) {
    /* データアトムはシンボルアトムに埋め込まれている.
     * よって, シンボルアトム単位でトレースをバックトラックする現状では
     * データアトムの訪問数を記録しておかなくてもよい.
     * つまり, 最終的にトレースokなシンボルアトムの数さえ一致してさえいれば,
     * そこに埋め込まれたデータアトムの比較結果も全て真である.
     *
     * @see struct LmnMembraneRef: アトムの記録方式を変更 */
    halfbyte_scanner scanner(bs->v, bs->len, *i_bs);
    if (tag == TAG_INT_DATA) {
      auto n  = scanner.scan_integer();
      (*i_bs) = scanner.location();
      return ((attr == LMN_INT_ATTR) && (n == (LmnWord)atom));
    }

    if (tag == TAG_DBL_DATA) {
      auto n  = scanner.scan_double();
      (*i_bs) = scanner.location();
      return ((attr == LMN_DBL_ATTR) && (n == lmn_get_double((LmnDataAtomRef)atom)));
    }

    if (tag == TAG_SP_ATOM_DATA) {
      auto type  = scanner.scan_sp_atom_type();
      auto bytes = scanner.scan_bytes();
      auto n     = sp_atom_decoder(type)(bytes);
      (*i_bs)    = scanner.location();
      return (attr == LMN_SP_ATOM_ATTR) && SP_ATOM_EQ(n, atom);
    }

    lmn_fatal("unexpected.");
    return false;
  }

  BOOL mem_eq_enc_mol(int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref) {
    auto tag = BS_GET(bs->v, *i_bs);
    (*i_bs)++;
    switch (tag) {
    case TAG_ATOM_START:
      return mem_eq_enc_atom(i_bs, mem, atom, attr, i_ref);
    case TAG_NAMED_MEM_START:
      return mem_eq_enc_traced_mem(TRUE, i_bs, mem, atom, attr, i_ref);
    case TAG_MEM_START:
      return mem_eq_enc_traced_mem(FALSE, i_bs, mem, atom, attr, i_ref);
    case TAG_VISITED_ATOMHLINK:
    case TAG_VISITED_MEM:
      return mem_eq_enc_visited(tag, i_bs, atom, attr, i_ref);
    case TAG_ESCAPE_MEM:
      return mem_eq_enc_escape_mem(i_bs, mem, atom, attr, i_ref);
    case TAG_HLINK:
      return mem_eq_enc_hlink(i_bs, mem, atom, attr, i_ref);
    case TAG_FROM:
      return TRUE;
    case TAG_INT_DATA: /* FALLTHROUGH */
    case TAG_DBL_DATA: /* FALLTHROUGH */
    case TAG_SP_ATOM_DATA:
      return mem_eq_enc_data_atom(tag, i_bs, atom, attr);
    default:
      lmn_fatal("not implemented");
      break;
    }
  }

  BOOL mem_eq_enc_visited(unsigned int tag, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref) {
    auto ref = binstr_get_ref_num(bs->v, *i_bs);
    (*i_bs)  += BS_PROC_REF_SIZE;

    if (tag == TAG_VISITED_MEM) {
      return mem_eq_enc_mem_ref(i_bs, atom, attr, i_ref, ref);
    } else if (attr == LMN_HL_ATTR) {
      return mem_eq_enc_hlink_ref(i_bs, atom, attr, i_ref, ref);
    } else {
      return mem_eq_enc_atom_ref(i_bs, atom, attr, ref);
    }
  }

  BOOL mem_eq_enc_hlink(int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref) {
    if (attr != LMN_HL_ATTR)
      return false;
    /* 比較先属性がハイパーリンクアトムかチェック */

    auto hl_root   = (lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->get_root();
    auto bs_hl_num = binstr_get_ref_num(bs->v, *i_bs);
    (*i_bs)        += BS_HLINK_NUM_SIZE;

    if (hl_root->element_num() != bs_hl_num)
      return false;

    if (tracelog_contains_hlink(log, hl_root))
      return false;

    unsigned int tag;
    tag = BS_GET(bs->v, *i_bs);
    (*i_bs)++;

    switch (tag) {
    case TAG_FROM:
      if (LMN_HL_HAS_ATTR(hl_root))
        return FALSE;
      break;
    case TAG_ATOM_START: {
      auto f = binstr_get_functor(bs->v, *i_bs); /* functorを持ってくる */
      *i_bs  += BS_FUNCTOR_SIZE;
      if (LMN_ATTR_IS_DATA(LMN_HL_ATTRATOM_ATTR(hl_root)) ||
          f != ((LmnSymbolAtomRef)LMN_HL_ATTRATOM(hl_root))->get_functor()) {
        return FALSE;
      }
    } break;
    case TAG_INT_DATA: {
      auto n = binstr_get_int(bs->v, *i_bs);
      *i_bs  += BS_INT_SIZE;
      if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_INT_ATTR || n != (LmnWord)LMN_HL_ATTRATOM(hl_root)) {
        return FALSE;
      }
    } break;
    case TAG_DBL_DATA: {
      double *n = LMN_MALLOC<double>();

      *n    = binstr_get_dbl(bs->v, *i_bs);
      *i_bs += BS_DBL_SIZE;
      if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_DBL_ATTR ||
          *n != lmn_get_double((LmnDataAtomRef)LMN_HL_ATTRATOM(hl_root))) {
        return FALSE;
      }
    } break;
    case TAG_SP_ATOM_DATA: {
      halfbyte_scanner scanner(bs->v, bs->len, *i_bs);
      auto             type  = scanner.scan_sp_atom_type();
      auto             bytes = scanner.scan_bytes();
      auto             atom  = sp_atom_decoder(type)(bytes);
      *i_bs                  = scanner.location();
      if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_SP_ATOM_ATTR || !SP_ATOM_EQ(atom, LMN_HL_ATTRATOM(hl_root)))
        return false;
    } break;
    default:
      printf("tag = %d\n", tag);
      lmn_fatal("binstr decode, unexpected");
      break;
    }

    log->visit_hlink(hl_root, *i_ref);
    (*i_ref)++;

    return true;
  }

  BOOL mem_eq_enc_escape_mem(int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref) {
    if (LMN_ATTR_IS_DATA(attr) || ((LmnSymbolAtomRef)atom)->get_functor() != LMN_IN_PROXY_FUNCTOR)
      return false;

    auto out = ((LmnSymbolAtomRef)atom)->get_link(0);
    log->visit_atom((LmnSymbolAtomRef)atom, TLOG_MATCHED_ID_NONE, LMN_PROXY_GET_MEM((LmnSymbolAtomRef)atom));
    return mem_eq_enc_mol(i_bs, mem->mem_parent(), ((LmnSymbolAtomRef)out)->get_link(1),
                          ((LmnSymbolAtomRef)out)->get_attr(1), i_ref);
  }

  BOOL mem_eq_enc_atom_ref(int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, unsigned int ref) {
    if (LMN_ATTR_IS_DATA(attr))
      return FALSE;

    auto arg = binstr_get_arg_ref(bs->v, *i_bs);
    (*i_bs)  += BS_ATOM_REF_ARG_SIZE;

    return LMN_ATTR_GET_VALUE(attr) == arg && ref == tracelog_get_atomMatched(log, (LmnSymbolAtomRef)atom);
  }

  BOOL mem_eq_enc_mem_ref(int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref, unsigned int ref) {
    if (LMN_ATTR_IS_DATA(attr) || ((LmnSymbolAtomRef)atom)->get_functor() != LMN_OUT_PROXY_FUNCTOR)
      return false;

    auto in     = ((LmnSymbolAtomRef)atom)->get_link(0);
    auto in_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
    log->visit_atom((LmnSymbolAtomRef)in, TLOG_MATCHED_ID_NONE, in_mem);
    if (ref != tracelog_get_memMatched(log, in_mem))
      return false;

    /*  out, inをskipし, nextアトムからmem_eq_enc_molを行う.
     * ------------------------+
     * ..--[next]-0--1-[in]-0--|--0-[out]-1--..
     * ------------------------+
     */
    return mem_eq_enc_mol(i_bs, in_mem, ((LmnSymbolAtomRef)in)->get_link(1), ((LmnSymbolAtomRef)in)->get_attr(1),
                          i_ref);
  }

  BOOL mem_eq_enc_hlink_ref(int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, int *i_ref, unsigned int ref) {
    if (attr != LMN_HL_ATTR)
      return FALSE; /* 比較先属性がハイパーリンクアトムでなければ偽 */

    auto hl_root = (lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->get_root();
    return ref == tracelog_get_hlinkMatched(log, hl_root);
  }
};

template <> struct equalizer<VisitLog> : public equalizer_base {
  bool log_contains(VisitLog *log, LmnSymbolAtomRef atom) { return log->get_atom(atom, NULL); }

  bool log_contains(VisitLog *log, LmnMembraneRef atom) { return log->get_mem(atom, NULL); }

  void log_set_backtrack_point(VisitLog *log) { log->set_checkpoint(); }

  void log_continue_trace(VisitLog *log) { log->commit_checkpoint(); }

  void log_backtrack(VisitLog *log) { log->revert_checkpoint(); }

  /* TAG_MEM_ENDが出たときに,
   * 対象の膜に対して訪問したプロセス数が等しい場合に真を返す. 訪問プロセスは,
   * シンボルアトム(except proxies), 子膜, inside proxies */
  BOOL mem_eq_enc_end(LmnMembraneRef mem, BOOL rule_flag, VisitLog *log) {
    return rule_flag || mem->ruleset_num() == 0;
  }

  /* 膜memに対するトレースを初めて行う際に呼び出す. */
  int mem_eq_enc_mols(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    BOOL rule_flag = FALSE;
    while (*i_bs < bs->len) {
      auto tag = BS_GET(bs->v, *i_bs);
      (*i_bs)++;

      switch (tag) {
      case TAG_ATOM_START: {
        auto f  = binstr_get_functor(bs->v, *i_bs);
        auto ok = FALSE;

        /* 1. 読み出したファンクタのエントリリストを取得する */
        auto ent = mem->get_atomlist(f);
        if (!ent)
          return FALSE;

        for (auto atom : *ent) {
          /* 2. 未チェックのアトムを選択する */
          if (log_contains(log, atom))
            continue;
          log_set_backtrack_point(log);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          /* 3. 未チェックのアトムを起点にトレースを行う.
           * 　 失敗した場合は読み出し位置を現時点にバックトラックさせるため,
           * tmp変数を使用
           * 4. この先の探索で失敗したらバックトラック */
          if (mem_eq_enc_atom(bs, &tmp_i_bs, mem, atom, LMN_ATTR_MAKE_LINK(0), ref_log, &tmp_i_ref, log) &&
              mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            ok     = TRUE;
            break;
          } else {
            log_backtrack(log);
          }
        }

        return ok;
      } break;
      case TAG_NAMED_MEM_START:
      case TAG_MEM_START: {
        lmn_interned_str mem_name = ANONYMOUS;

        if (tag == TAG_NAMED_MEM_START) {
          mem_name = binstr_get_mem_name(bs->v, *i_bs);
          (*i_bs)  += BS_MEM_NAME_SIZE;
        }

        auto ok = FALSE;
        for (auto m = mem->mem_child_head(); m; m = m->mem_next()) {
          /* 1. 未チェックの子膜を選択する. 同時に膜名チェックを行う */
          if (m->NAME_ID() != mem_name || log_contains(log, m))
            continue;
          log_set_backtrack_point(log);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          /* 膜を起点にした判定を行う.
           * 1. 子以下の階層を検査
           * 2. この先の探索に失敗したらバックトラック　@rev.450 */
          if (mem_eq_enc_mem(bs, &tmp_i_bs, m, ref_log, &tmp_i_ref, log) &&
              mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            ok     = TRUE;
            break;
          } else {
            log_backtrack(log);
          }
        }

        return ok;
      } break;
      case TAG_ESCAPE_MEM_DATA: {
        auto sub_tag = BS_GET(bs->v, *i_bs);
        (*i_bs)++;
        auto ok  = FALSE;
        auto ent = mem->get_atomlist(LMN_IN_PROXY_FUNCTOR);
        if (!ent)
          return FALSE;

        for (auto in : *ent) {
          /* -----------------+
           * [n]-0--1-[in]-0--|--0-[out]-1--?-..
           * -----------------+
           */

          auto data      = (LmnSymbolAtomRef)in->get_link(1);
          auto data_attr = in->get_attr(1);

          auto tmp_i_bs  = *i_bs;
          auto tmp_i_ref = *i_ref;

          if (!LMN_ATTR_IS_DATA(data_attr) || !mem_eq_enc_data_atom(sub_tag, bs, &tmp_i_bs, data, data_attr, log)) {
            /* nがデータアトムでない, もしくは等しいデータアトムでない */
            continue;
          }

          /* シンボルアトムを起点としていないため, バックトラックポインtがない.
           * proxyをバックトラックポイントにする必要がある */
          log_set_backtrack_point(log);

          if (mem_eq_enc_escape_mem(bs, &tmp_i_bs, mem, in, LMN_ATTR_MAKE_LINK(0), ref_log, &tmp_i_ref, log) &&
              mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
            log_continue_trace(log);
            *i_bs  = tmp_i_bs;
            *i_ref = tmp_i_ref;
            ok     = TRUE;
            break;
          } else {
            log_backtrack(log);
          }
        }

        return ok;
      } break;
      case TAG_MEM_END:
        return mem_eq_enc_end(mem, rule_flag, log);
      case TAG_RULESET1:
        if ((mem->ruleset_num() != 1) || !mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, 0))) {
          return FALSE;
        }
        rule_flag = TRUE;
        break;
      case TAG_RULESET:
        if (!mem_eq_enc_rulesets(bs, i_bs, mem))
          return FALSE;
        rule_flag = TRUE;
        break;
      case TAG_RULESET_UNIQ:
        if (!mem_eq_enc_rulesets_uniq(bs, i_bs, mem))
          return FALSE;
        rule_flag = TRUE;
        break;
      default:
        lmn_fatal("unexpected");
        break;
      }
    }

    return mem_eq_enc_end(mem, rule_flag, log);
  }

  /* バイト列bsの*i_bs番目からシンボルアトムデータを読み出し,
   * 膜memに存在するアトムatomと比較する.
   * 既にトレース済みである場合や異なるアトムと比較した場合は直ちに偽を返す.
   * 引き続きバイト列を読み出し,
   * アトムatomを起点にしたプロセスと等価な構造をトレースできた場合に 真を返し,
   * 等価な構造をトレースできない場合は偽を返す  */
  BOOL mem_eq_enc_atom(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr,
                       BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    if (LMN_ATTR_IS_DATA(attr))
      return FALSE;

    auto satom = reinterpret_cast<LmnSymbolAtomRef>(atom);

    /* 1. ファンクタを取得 */
    auto f  = binstr_get_functor(bs->v, *i_bs);
    (*i_bs) += BS_FUNCTOR_SIZE;

    /* アトムatomがファンクタfのアトムでない場合,
     * 既にチェック済みのアトムの場合, FALSEを返す */

    if (f != satom->get_functor())
      return FALSE;
    if (!log->put_atom(satom))
      return FALSE;
    ref_log[*i_ref].v    = (LmnWord)atom;
    ref_log[*i_ref].type = BS_LOG_TYPE_ATOM;

    (*i_ref)++;
    auto arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);

    /* アトムatomの接続先を検査する */
    for (auto i = 0; i < arity; i++) {
      if (!mem_eq_enc_mol(bs, i_bs, mem, satom->get_link(i), satom->get_attr(i), ref_log, i_ref, log)) {
        return FALSE;
      }
    }

    return TRUE;
  }

  /* outsideproxyアトムatomへ到達した際のトレース(子膜へリンクが突き抜けた場合)
   * -----------------+
   * ...-0--1-[in]-0--|--0-[atom]-1--..
   * -----------------+
   */
  BOOL mem_eq_enc_traced_mem(BOOL is_named, LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom,
                             LmnLinkAttr attr, BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    if (LMN_ATTR_IS_DATA(attr))
      return false;

    auto satom = reinterpret_cast<LmnSymbolAtomRef>(atom);
    if (satom->get_functor() != LMN_OUT_PROXY_FUNCTOR)
      return false;

    /* 子膜側のinside proxyアトムを取得 */
    auto in     = (LmnSymbolAtomRef)satom->get_link(0);
    auto in_mem = LMN_PROXY_GET_MEM(in);

    if (is_named) {
      const lmn_interned_str mem_name = binstr_get_mem_name(bs->v, *i_bs);
      (*i_bs)                         += BS_MEM_NAME_SIZE;
      if (mem_name != in_mem->NAME_ID()) {
        return FALSE;
      }
    }

    log->put_mem(in_mem);
    ref_log[*i_ref].v    = (LmnWord)in_mem;
    ref_log[*i_ref].type = BS_LOG_TYPE_MEM;
    (*i_ref)++;

    /* 1. mem_eq_enc_mol : 引き続き子膜側へ踏み込んで連結分子をトレース
     * 2. mem_eq_enc_mols: 1のトレースに成功したならば,
     * 残りの子膜のコンテンツをトレース. */
    return mem_eq_enc_mol(bs, i_bs, in_mem, in->get_link(1), in->get_attr(1), ref_log, i_ref, log) &&
           mem_eq_enc_mols(bs, i_bs, in_mem, ref_log, i_ref, log);
  }

  BOOL mem_eq_enc_mem(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    if (!log->put_mem(mem))
      return FALSE;
    ref_log[*i_ref].v    = (LmnWord)mem;
    ref_log[*i_ref].type = BS_LOG_TYPE_MEM;
    (*i_ref)++;

    return mem_eq_enc_mols(bs, i_bs, mem, ref_log, i_ref, log);
  }

  /* tagに応じたデータアトムをbsから読み出し, アトムatomと比較する.
   * 等しければTRUE, 等しくなければFALSEを返す. */
  BOOL mem_eq_enc_data_atom(unsigned int tag, LmnBinStrRef bs, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr,
                            VisitLog *log) {

    /* データアトムはシンボルアトムに埋め込まれている.
     * よって, シンボルアトム単位でトレースをバックトラックする現状では
     * データアトムの訪問数を記録しておかなくてもよい.
     * つまり, 最終的にトレースokなシンボルアトムの数さえ一致してさえいれば,
     * そこに埋め込まれたデータアトムの比較結果も全て真である.
     *
     * @see struct LmnMembraneRef: アトムの記録方式を変更 */
    if (tag == TAG_INT_DATA) {
      long n  = binstr_get_int(bs->v, *i_bs);
      (*i_bs) += BS_INT_SIZE;

      if ((attr == LMN_INT_ATTR) && (n == (LmnWord)atom)) {
        log->put_data();
        return TRUE;
      }
    } else if (tag == TAG_DBL_DATA) {
      double n = binstr_get_dbl(bs->v, *i_bs);
      (*i_bs)  += BS_DBL_SIZE;

      if ((attr == LMN_DBL_ATTR) && (n == lmn_get_double((LmnDataAtomRef)atom))) {
        log->put_data();
        return TRUE;
      }
    } else if (tag == TAG_SP_ATOM_DATA) {
      halfbyte_scanner scanner(bs->v, bs->len, *i_bs);
      auto             type  = scanner.scan_sp_atom_type();
      auto             bytes = scanner.scan_bytes();
      auto             n     = sp_atom_decoder(type)(bytes);
      *i_bs                  = scanner.location();
      if (attr == LMN_SP_ATOM_ATTR && SP_ATOM_EQ(n, atom)) {
        log->put_data();
        return true;
      }
    } else {
      lmn_fatal("unexpected.");
    }

    return FALSE;
  }

  /* アトムのリンク先(連結分子)を辿る際の入り口.
   * 基本的にはタグを読み出し, タグに応じて比較関数を呼び出す.
   * バイト列bsの*i_bs番目からデータを読み出すことで,
   * 膜memのアトムatomを起点に等価な構造をトレースできた場合に真を返す. */
  BOOL mem_eq_enc_mol(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr,
                      BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    auto tag = BS_GET(bs->v, *i_bs);
    (*i_bs)++;
    switch (tag) {
    case TAG_ATOM_START:
      return mem_eq_enc_atom(bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
    case TAG_NAMED_MEM_START:
      return mem_eq_enc_traced_mem(TRUE, bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
    case TAG_MEM_START:
      return mem_eq_enc_traced_mem(FALSE, bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
    case TAG_VISITED_ATOMHLINK:
    case TAG_VISITED_MEM:
      return mem_eq_enc_visited(tag, bs, i_bs, atom, attr, ref_log, i_ref, log);
    case TAG_ESCAPE_MEM:
      return mem_eq_enc_escape_mem(bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
    case TAG_HLINK:
      return mem_eq_enc_hlink(bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
    case TAG_FROM:
      return TRUE;
    case TAG_INT_DATA: /* FALLTHROUGH */
    case TAG_DBL_DATA: /* FALLTHROUGH */
    case TAG_SP_ATOM_DATA:
      return mem_eq_enc_data_atom(tag, bs, i_bs, atom, attr, log);
    default:
      lmn_fatal("not implemented");
      break;
    }
  }

  /* 訪問済みのプロセスとの比較処理. 等価ならば真を返す. */
  BOOL mem_eq_enc_visited(unsigned int tag, LmnBinStrRef bs, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr,
                          BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    auto ref = binstr_get_ref_num(bs->v, *i_bs);
    (*i_bs)  += BS_PROC_REF_SIZE;

    switch (ref_log[ref].type) {
    case BS_LOG_TYPE_ATOM:
      return mem_eq_enc_atom_ref(bs, i_bs, atom, attr, ref_log, ref, log);
    case BS_LOG_TYPE_MEM:
      return mem_eq_enc_mem_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
    case BS_LOG_TYPE_HLINK:
      return mem_eq_enc_hlink_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
    default:
      lmn_fatal("unexpected reference");
      return false;
    }
  }

  /* 既に一度訪れたアトムにリンク接続する場合 */
  BOOL mem_eq_enc_atom_ref(LmnBinStrRef bs, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, BsDecodeLog *ref_log,
                           unsigned int ref, VisitLog *log) {
    if (LMN_ATTR_IS_DATA(attr))
      return FALSE;

    auto arg = binstr_get_arg_ref(bs->v, *i_bs);
    (*i_bs)  += BS_ATOM_REF_ARG_SIZE;

    return (ref_log[ref].v == (LmnWord)atom) && (LMN_ATTR_GET_VALUE(attr) == arg);
  }

  /* outside proxyアトムatomを経て, 既に1度訪れた膜へ再訪問する場合.
   * -----------------+
   * ...-0--1-[in]-0--|--0-[atom:out]-1--..
   * -----------------+ */
  BOOL mem_eq_enc_mem_ref(LmnBinStrRef bs, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, BsDecodeLog *ref_log,
                          int *i_ref, unsigned int ref, VisitLog *log) {
    if (LMN_ATTR_IS_DATA(attr) || ((LmnSymbolAtomRef)atom)->get_functor() != LMN_OUT_PROXY_FUNCTOR)
      return false;

    auto in     = ((LmnSymbolAtomRef)atom)->get_link(0);
    auto in_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
    if (ref_log[ref].v != (LmnWord)in_mem)
      return false;
    /*  out, inをskipし, nextアトムからmem_eq_enc_molを行う.
     * ------------------------+
     * ..--[next]-0--1-[in]-0--|--0-[out]-1--..
     * ------------------------+
     */
    return mem_eq_enc_mol(bs, i_bs, in_mem, ((LmnSymbolAtomRef)in)->get_link(1), ((LmnSymbolAtomRef)in)->get_attr(1),
                          ref_log, i_ref, log);
  }

  /* -- */
  BOOL mem_eq_enc_hlink_ref(LmnBinStrRef bs, int *i_bs, LmnAtomRef atom, LmnLinkAttr attr, BsDecodeLog *ref_log,
                            int *i_ref, unsigned int ref, VisitLog *log) {
    if (attr != LMN_HL_ATTR)
      return FALSE; /* 比較先属性がハイパーリンクアトムでなければ偽 */

    auto hl_root = (lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->get_root();

    log->put_data();
    return ((HyperLink *)ref_log[ref].v)->eq_hl(hl_root);
  }

  /* inside proxyアトムatomからのトレース (トレース中に親膜へ抜ける場合)
   * なお, 親膜側からトレースを行うので, 子膜から親膜へ辿る際,
   * 既に親膜は訪問済みテーブルに記録済み.
   * -----------------+
   * ...--1-[atom]-0--|--0-[out]-1--..
   * -----------------+
   */
  BOOL mem_eq_enc_escape_mem(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr,
                             BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    if (LMN_ATTR_IS_DATA(attr) || ((LmnSymbolAtomRef)atom)->get_functor() != LMN_IN_PROXY_FUNCTOR)
      return false;

    auto out = ((LmnSymbolAtomRef)atom)->get_link(0);
    return mem_eq_enc_mol(bs, i_bs, mem->mem_parent(), ((LmnSymbolAtomRef)out)->get_link(1),
                          ((LmnSymbolAtomRef)out)->get_attr(1), ref_log, i_ref, log);
  }

  BOOL mem_eq_enc_hlink(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem, LmnAtomRef atom, LmnLinkAttr attr,
                        BsDecodeLog *ref_log, int *i_ref, VisitLog *log) {
    if (attr != LMN_HL_ATTR)
      return false;
    /* 比較先属性がハイパーリンクアトムかチェック */

    auto hl_root   = (lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->get_root();
    auto bs_hl_num = binstr_get_ref_num(bs->v, *i_bs);
    (*i_bs)        += BS_HLINK_NUM_SIZE;

    if (hl_root->element_num() != bs_hl_num)
      return false;

    if (!log->put_hlink(hl_root))
      return false;
    ref_log[*i_ref].v    = (LmnWord)hl_root;
    ref_log[*i_ref].type = BS_LOG_TYPE_HLINK;
    (*i_ref)++;

    return true;
  }

  /* mem以下にあるアトムと膜の数を返す */
  long process_num(LmnMembrane *mem) {
    if (mem == NULL)
      return 0;

    auto n = mem->atom_num() + mem->child_mem_num();

    for (auto m = mem->child_head; m; m = m->next) {
      n += process_num(m);
    }
    return n;
  }
};

#endif /* SLIM_VERIFIER_MEM_ENCODE_EQUALIZER_HPP */
