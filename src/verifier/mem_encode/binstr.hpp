/*
 * binstr.hpp
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
 *
 */

#ifndef SLIM_VERIFIER_MEM_ENCODE_BINSTR_HPP
#define SLIM_VERIFIER_MEM_ENCODE_BINSTR_HPP

#include "lmn_binstr.hpp"
#include "visitlog.h"

#include "lmntal.h"
#include "vm/vm.h"

#include <memory>

#define TAG_IN_BYTE 2
#define TAG_BIT_SIZE 4

/* Tags */
enum {
  TAG_ATOM_START        = 0x0,
  TAG_MEM_START         = 0x1,
  TAG_MEM_END           = 0x2,
  TAG_NAMED_MEM_START   = 0x3,
  TAG_VISITED_ATOMHLINK = 0x4,
  TAG_VISITED_MEM       = 0x5,
  TAG_ESCAPE_MEM        = 0x6,
  TAG_ESCAPE_MEM_DATA   = 0x7,
  TAG_FROM              = 0x8,
  TAG_RULESET1          = 0x9,
  TAG_RULESET           = 0xa,
  TAG_RULESET_UNIQ      = 0xb,
  TAG_INT_DATA          = 0xc,
  TAG_DBL_DATA          = 0xd,
  TAG_SP_ATOM_DATA      = 0xe,
  TAG_HLINK             = 0xf,
};

/* Binary Stringのpositionを進めるためのカウンタ群. */
namespace {
/* 訪問番号(4byteへ拡張したが, sizeof(ProcessID)とするのが好ましいはず) */
int const BS_PROC_REF_SIZE = (TAG_IN_BYTE * sizeof(uint32_t));
/* アトムあたりのリンク本数は127本までなので1Byteで良い */
int const BS_ATOM_REF_ARG_SIZE = (TAG_IN_BYTE * sizeof(LmnArity));
/* Functor ID */
int const BS_FUNCTOR_SIZE = (TAG_IN_BYTE * sizeof(LmnFunctor));
/* lmntalにおける整数データは1wordであるためlongで良い */
int const BS_INT_SIZE = (TAG_IN_BYTE * SIZEOF_LONG);
/* 膜名 */
int const BS_MEM_NAME_SIZE = (TAG_IN_BYTE * sizeof(lmn_interned_str));
/* ルールセットID */
int const BS_RULESET_SIZE = (TAG_IN_BYTE * sizeof(LmnRulesetId));
/* ルールセット数 */
int const BS_RULESET_NUM_SIZE = (TAG_IN_BYTE * sizeof(uint32_t));
/* ルール数 */
int const BS_RULE_NUM_SIZE = (TAG_IN_BYTE * sizeof(uint32_t));
/* 浮動小数点数 */
int const BS_DBL_SIZE = (TAG_IN_BYTE * sizeof(double));
/* 文字列に対応させたID */
int const BS_STR_ID_SIZE      = (TAG_IN_BYTE * sizeof(lmn_interned_str));
int const BS_HISTORY_NUM_SIZE = (BS_RULE_NUM_SIZE);
/* UNIQ制約が管理する履歴型のサイズ. lmn_histroy_tみたいな型にしたい */
int const BS_HISTORY_SIZE = (BS_STR_ID_SIZE);
/* ハイパーリンクの接続個数. 同型性判定で使う */
int const BS_HLINK_NUM_SIZE = (TAG_IN_BYTE * sizeof(LmnHlinkRank));
} // namespace

/*----------------------------------------------------------------------
 * Binary String Pointer
 */

/* BinStrの特定の位置を指し示すポインタ。BinStrへの書き込みはBinStrPtr
   を介して行う。他のBinStrPtrの書き込みにより、現在のBinStrPtrが無G効に
   なる場合があり、binstr_validが真を返すようになる。 */

class BinStrCursor {
  struct BinStr *binstr; /* 所属するBinStrを指す */
  int            pos_;   /* bit (0で初期化) */
  bool           valid;  /* TRUEで初期化 */
  bool           direct; /* FALSEで初期化, directメソッドを用いた場合はTRUEで初期化 */

public:
  BinStrCursor() : binstr(nullptr), pos_(0), valid(false), direct(false) {}

  BinStrCursor(BinStr *bs, bool direct = false) : binstr(bs), pos_(0), valid(true), direct(direct) {}

  BinStrCursor const &operator=(BinStrCursor const &p) {
    binstr = p.binstr;
    pos_   = p.pos_;
    valid  = p.valid;
    direct = p.direct;
    return p;
  }

  int pos() const { return pos_; }

private:
  void invalidate() { valid = false; }

public:
  bool is_valid() const { return valid; }

  /* ポインタpが指すバイナリストリングに、vからサイズsize分だけ書き込む.
   * 書き込みに成功した場合は1を, 失敗した場合は0を返す.
   * BYTE型(8bit)へキャストした任意のサイズの書き込みデータvを,
   * サイズsizeまでポインタアクセスで読み出し,
   * 4bitずつバイナリストリングpへ書き込む. sizeが奇数の場合(size & 1),
   * 8bit単位からあぶれる最後の4bitをv[size>>1] & 0x0fで求めて書き込む.
   * bit単位で書き込むため, ループを回し, 1度のループで, 4bitずつ2回書き込む.
   * 故に, 渡されたsizeの半分half_lenを予め求めておき処理に活用している.  */
  bool push(const BYTE *v, int size);
  bool push(BYTE v);

  bool push_start_mem(lmn_interned_str name) {
    if (name == ANONYMOUS) {
      return push(TAG_MEM_START);
    } else {
      return push(TAG_NAMED_MEM_START) && push((BYTE *)&name, sizeof(lmn_interned_str) * TAG_IN_BYTE);
    }
  }
  bool push_end_mem() { return push(TAG_MEM_END); }

  int push_atom(LmnSymbolAtomRef a) {
    /* ファンクタの最大値からファンクタの値を引いて、大小を反転させる */
    LmnFunctor f = (LmnFunctor)FUNCTOR_MAX - a->get_functor();

    return push(TAG_ATOM_START) && push((const BYTE *)&f, BS_FUNCTOR_SIZE);
  }

  int push_hlink(LmnAtomRef atom, VisitLogRef log) {
    LmnWord ref;

    /* hyperlink構造を圧縮する際は, rootオブジェクトをバイト列に記録する. */
    auto hl_root = (lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom))->get_root();

    if (log->get_hlink(hl_root, &ref)) {
      return push(TAG_VISITED_ATOMHLINK) && push((BYTE *)&ref, BS_PROC_REF_SIZE);
    }

    auto hl_num = hl_root->element_num();

    log->put_hlink(hl_root); /* 訪問済みにした */
    push(TAG_HLINK);
    push((const BYTE *)&hl_num, BS_HLINK_NUM_SIZE);

    if (LMN_HL_HAS_ATTR(hl_root)) {
      LmnLinkAttr attr     = LMN_HL_ATTRATOM_ATTR(hl_root);
      LmnAtomRef  attrAtom = LMN_HL_ATTRATOM(hl_root);

      /* データアトムの場合 */
      if (LMN_ATTR_IS_DATA(attr)) {
        return push_data_atom(attrAtom, attr, log);
      } else {
        return push_atom((LmnSymbolAtomRef)attrAtom);
      }
    } else {
      push_from();
    }

    return is_valid();
  }

  int push(std::vector<uint8_t> const &bytes) {
    uint64_t size = bytes.size();
    return push((uint8_t *)&size, sizeof(size) * TAG_IN_BYTE) && push(bytes.data(), size * TAG_IN_BYTE);
  }

  /* データアトムをバイト列へ書き込む. まずTAG_INT_DATA or
   * TAG_DBL_DATAを書き込んでから, 4bitずつ値を書き込んでいく.
   * ハイパーリンクの処理を追加 @rev.461 */
  int push_data_atom(LmnAtomRef atom, LmnLinkAttr attr, VisitLogRef log) {
    switch (attr) {
    case LMN_INT_ATTR:
      return push(TAG_INT_DATA) && push((const BYTE *)&atom, BS_INT_SIZE);
    case LMN_DBL_ATTR:
      return push(TAG_DBL_DATA) && push((const BYTE *)LMN_GETREF_DOUBLE(atom), BS_DBL_SIZE);
    case LMN_HL_ATTR:
      return push_hlink(atom, log);
    case LMN_SP_ATOM_ATTR:
      if (sp_atom_encoder(atom)) {
        auto bytes = sp_atom_encoder(atom)(atom);
        auto type  = LMN_SP_ATOM_TYPE(atom);
        return push(TAG_SP_ATOM_DATA) && push(&type, sizeof(type) * TAG_IN_BYTE) && push(bytes);
      }
    default:
      lmn_fatal("no implementations");
      break;
    }
    return 0;
  }

  /* 書き込んだ"順番"を参照IDとして書き込む */
  int push_visited_atom(int n, int arg) {
    return push(TAG_VISITED_ATOMHLINK) && push((BYTE *)&n, BS_PROC_REF_SIZE) &&
           push((BYTE *)&arg, BS_ATOM_REF_ARG_SIZE);
  }

  int push_visited_mem(int n) { return push(TAG_VISITED_MEM) && push((BYTE *)&n, BS_PROC_REF_SIZE); }

  int push_escape_mem() { return push(TAG_ESCAPE_MEM); }

  int push_escape_mem_data(LmnAtomRef atom, LmnLinkAttr attr, VisitLogRef log) {
    return push(TAG_ESCAPE_MEM_DATA) && push_data_atom(atom, attr, log);
  }

  int push_from() { return push(TAG_FROM); }

  int push_start_rulesets(int n) {
    if (n == 1)
      return push(TAG_RULESET1);

    return push(TAG_RULESET) && push((BYTE *)&n, BS_RULESET_NUM_SIZE);
  }

  int push_ruleset(LmnRuleSetRef rs) { return push((BYTE *)&rs->id, BS_RULESET_SIZE); }

  void push_rule_histories(LmnRuleRef r) {
    auto &his_tbl = r->history();
    auto  his_num = his_tbl.size();

    push((BYTE *)&his_num, BS_HISTORY_NUM_SIZE); /* write history num */

    if (his_num == 0)
      return;

    /* write each id of histories */
    /* 履歴表は, interned_idをkeyに, valueを0にしている */
    for (auto id : his_tbl) {
      push((BYTE *)&id, BS_HISTORY_SIZE);
    }
  }

  void push_ruleset_uniq(LmnMembraneRef mem, int n) {
    unsigned int i, j;

    /* write UNIQ_TAG and Number of All Rulesets */
    push(TAG_RULESET_UNIQ);
    push((BYTE *)&n, BS_RULESET_NUM_SIZE);

    for (auto i = 0; i < n; i++) { /* foreach ruleset */
      LmnRuleSetRef rs = lmn_mem_get_ruleset(mem, i);
      push_ruleset(rs); /* write ruleset id */

      for (auto j = 0; j < rs->size(); j++) { /* foreach rule history */
        push_rule_histories(rs->get_rule(j));
      }
    }
  }
};

#define BS_TBL_SIZE (128)

#define BS_SET(a, pos, v)                                                                                              \
  (((pos)&1) ? ((a)[(pos) >> 1] = ((a)[(pos) >> 1] & 0x0f) | ((v) << TAG_BIT_SIZE))                                    \
             : ((a)[(pos) >> 1] = (v & 0x0f) | ((a)[(pos) >> 1] & 0xf0)))
#define BS_GET(a, pos) (((pos)&1) ? ((a)[(pos) >> 1] & 0xf0) >> TAG_BIT_SIZE : (a)[(pos) >> 1] & 0x0f)

/* エンコード処理(計算中)に用いるバイナリストリング(作業領域) */
class BinStr {
  BYTE *v;    /* バイト列(128個で初期化) */
  int   size; /* バッファのサイズ（4ビット単位）: 現在のバイト列の大きさ(128 *
                 TAG_IN_BYTEで初期化) */
  int cur;    /* 書き込み位置（4ビット単位）    : 次に書き込む位置(0で初期化) */

public:
  BinStr() {
    size = BS_TBL_SIZE * TAG_IN_BYTE;
    v    = LMN_NALLOC<BYTE>(size / TAG_IN_BYTE);
    memset(v, 0x0U, sizeof(BYTE) * BS_TBL_SIZE);
    cur = 0;
  }

  ~BinStr() { LMN_FREE(v); }

  std::unique_ptr<BinStrCursor> head() { return std::make_unique<BinStrCursor>(this); }

  std::unique_ptr<BinStrCursor> head_direct() { return std::make_unique<BinStrCursor>(this, true); }

public:
  /* bsの位置posにbの下位4ビットを書き込む。書き込みに成功した場合は真を
   返し、失敗した場合は偽を返す。*/
  bool write(BYTE b, int pos) {
    resize(pos);

    /* cur==posならば無条件で書き込み成功 */
    if (cur == pos) {
      BS_SET(v, pos, b);
      cur++;
      return true;
    } else if (cur > pos) {
      /* 下位4ビットの比較を行う */
      if (BS_GET(v, pos) < b)
        return false;
      else if (BS_GET(v, pos) > b) {
        cur = pos + 1;
        BS_SET(v, pos, b);
        return true;
      } else { /* v[pos] == b */
        return true;
      }
    } else { /*(cur < pos)*/
      lmn_fatal("unexpected");
      return false;
    }
  }

  bool write_direct(BYTE b, int pos) {
    resize(pos);

    BS_SET(v, pos, b);
    cur = pos + 1;
    return true;
  }

  LmnBinStr *to_lmn_binstr() const {
    struct LmnBinStr *ret_bs;
    int               size = (this->cur + 1) / 2;
    ret_bs                 = LMN_MALLOC<struct LmnBinStr>();
    ret_bs->v              = LMN_NALLOC<BYTE>(size);
    ret_bs->type           = 0x00U;
    memcpy(ret_bs->v, this->v, size);
    ret_bs->len = this->cur;
    if (ret_bs->len & 1) {
      ret_bs->v[ret_bs->len >> 1] = ret_bs->v[ret_bs->len >> 1] & 0x0f;
    }

    return ret_bs;
  }

private:
  void resize(int pos) {
    int org_size = size;
    while (size <= pos) {
      size *= 2;
    }

    if (org_size >= size)
      return;

    v = LMN_REALLOC<BYTE>(v, size / TAG_IN_BYTE);
  }
};

#endif
