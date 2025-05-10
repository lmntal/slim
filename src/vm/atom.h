/*
 * atom.h
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

struct ProcessTbl;
struct LmnMembrane;

/**
 * @ingroup VM
 * @defgroup Atom
 * @{
 */

#include "lmntal.h"
#include "element/element.h"
#include <type_traits>

/**
 * @interface LmnAtom
 * @brief Represents all kinds of atoms.
 */
typedef void *LmnAtomRef;
/**
 * @struct LmnDataAtom
 * @implements LmnAtom
 */
typedef LmnWord LmnDataAtomRef;
/**
 * @struct LmnSymbolAtom
 * @implements LmnAtom
 *
 *  Atom Structure
 *
 *  * Atom (Nはリンクの数)
 *   -  1st Word      : アトムリストにおけるprevポインタ
 *   -  2nd Word      : アトムリストにおけるnextポインタ
 *   -  3rd Word      : アトムと膜に割り当てる一意な整数ID
 *   -  aligned Word  : 以下のByte要素の合計をWordサイズへアラインメント
 *   --   next 2 Bytes:
 * アトムの種類(アトム名とリンク数の組)を表すfunctorに対応した整数ID
 *   --   N Bytes     : リンク属性   (1 Byte * N本)
 *   -  N Words       : リンクデータ (1 Word * N本)
 *
 *  * Link Attribute
 *     リンク属性は, 先頭1ビットが立っていない場合は,
 * 下位7bitが接続先リンクの番号を記録しており, 先頭1ビットが立っている場合は,
 * Primitiveデータの種類を記録する。 
 * [Link Number]  0--- ---- 
 * [int]          1000 0000 
 * [double]       1000 0001 
 * [special]      1000 0011 
 * [string]       1000 0011
 * [const string] 1000 0100
 * [const double] 1000 0101
 * [hyper link]   1000 1010
 *
 *     We are going to support some primitive data types.
 *     (signed/unsigned) int, short int, long int, byte, long long int,
 *     float, double, long double,
 *     bool, string, character,
 *     ground array, ground with membrane array, primitive arrays
 *
 *     But, incompletely-specified.
 *
 */
typedef struct LmnSymbolAtom *LmnSymbolAtomRef;

/* プロキシの3番目の引数番号の領域を remove_proxy, insert_proxyで利用中。
 * 所属する膜へのポインタを持っている */

using namespace binary_literal;

namespace slim {
namespace vm {
  /**
   * リンク属性の定数を定義する列挙体
   */
  enum class link_attribute_flag : LmnLinkAttr {
    //! データを抽出するビットマスク
    mask_bits = 01111111_b,
    //! プリミティブデータかどうかを判定するフラグビット
    data_flag = 10000000_b,

    //! integer literal
    integer       = data_flag | 00000000_b,
    //! double literal
    decimal       = data_flag | 00000001_b,
    //! special atom
    special       = data_flag | 00000010_b,
    //! string literal (same as special atom)
    string        = special,
    //! @deprecated constant string literal
    const_string  = data_flag | 00000100_b,
    //! @deprecated constant double literal
    const_decimal = data_flag | 00000101_b,
    //! exclamation atom
    hyperlink     = data_flag | 00001010_b,
  };

  /**
   * リンク属性に可能な演算を定義するクラス
   */
  class link_attribute final {
    LmnLinkAttr _value;

    constexpr link_attribute(LmnLinkAttr value) : _value(value) {}

  public:
    link_attribute() = default;
    constexpr link_attribute(link_attribute_flag value) : _value((LmnLinkAttr)value) {}

    static constexpr link_attribute symbol(int n) { return link_attribute(static_cast<LmnLinkAttr>(n)); }

    constexpr link_attribute operator|(const link_attribute &other) const {
      return link_attribute(_value | other._value);
    }
    constexpr link_attribute operator&(const link_attribute &other) const {
      return link_attribute(_value & other._value);
    }
    constexpr link_attribute operator~() const {
      return link_attribute(~_value);
    }
    constexpr bool operator ==(const link_attribute &other) const {
      return _value == other._value;
    }
    constexpr bool operator !=(const link_attribute &other) const {
      return !(*this == other);
    }
    constexpr explicit operator LmnLinkAttr() const {
      return _value;
    }

    /**
     * @brief check whether a link attribute is for data.
     */
    constexpr bool is_data() const {
      return (*this & slim::vm::link_attribute_flag::data_flag) == slim::vm::link_attribute_flag::data_flag;
    }

    /**
     * @brief check whether a link attribute is for data except an exclamation atom.
     */
    constexpr bool is_data_except_hyperlink() const {
      return is_data() && *this != link_attribute_flag::hyperlink;
    }

    /**
     * @brief check whether a link attribute is for an exclamation atom.
     */
    constexpr bool is_hyperlink() const {
      return *this == link_attribute_flag::hyperlink;
    }

    /**
     * @brief get link attribute value (remove tag)
     */
    constexpr int get_link_value() const {
      return (*this & slim::vm::link_attribute_flag::mask_bits)._value;
    }
  };

  // メモリ的にはLmnLinkAttrと同じように扱えることを保証しておきたい
  static_assert(sizeof(link_attribute) == sizeof(LmnLinkAttr), "");
  static_assert(std::is_trivially_default_constructible<link_attribute>::value, "");
  static_assert(std::is_trivially_copyable<link_attribute>::value, "");
  static_assert(std::is_trivially_copy_assignable<link_attribute>::value, "");
  static_assert(std::is_standard_layout<link_attribute>::value, "");
}
}

#define LMN_ATOM(X) ((LmnAtom)(X))
#define LMN_SATOM(X) ((LmnSAtom)(X))

/**
 * @brief link attributes of primitive data type
 *
 * low 7 bits of link attribute <br>
 * ハイパーリンクアトム (⊂ extended atom ⊂ data atom ⊂ unary) <br>
 * ハイパーリンクアトムはプロキシと同様シンボルアトムとしても扱われることに注意
 */
enum LmnLinkAttribute : LmnLinkAttr {
  LMN_INT_ATTR       = (LmnLinkAttr)slim::vm::link_attribute_flag::integer,
  LMN_DBL_ATTR       = (LmnLinkAttr)slim::vm::link_attribute_flag::decimal,
  LMN_SP_ATOM_ATTR   = (LmnLinkAttr)slim::vm::link_attribute_flag::special,
  LMN_STRING_ATTR    = (LmnLinkAttr)slim::vm::link_attribute_flag::string,
  LMN_CONST_STR_ATTR = (LmnLinkAttr)slim::vm::link_attribute_flag::const_string,
  LMN_CONST_DBL_ATTR = (LmnLinkAttr)slim::vm::link_attribute_flag::const_decimal,
  LMN_HL_ATTR        = (LmnLinkAttr)slim::vm::link_attribute_flag::hyperlink,
};

#include "element/element.h"
#include "functor.h"
#include "special_atom.h"
#include "symbol.h"

struct LmnSymbolAtom {
  LmnSymbolAtomRef prev;
  LmnSymbolAtomRef next;
  LmnWord procId;
  bool record_flag = false;
  int rule_number = -1;
  union {
    struct {
      slim::vm::functor functor;
      slim::vm::link_attribute attr[0];
    } link_header;
    LmnAtomRef links[0];
  };
  /**
   * @brief アトムリストからATOMのprevアトムを取得する.
   *        アトムリストから履歴アトムを読み飛ばさないので,
   * 呼び出し側で適宜なんとかする
   * @memberof LmnSymbolAtom
   */
  LmnSymbolAtomRef get_prev() const { return this->prev; }
  /**
   * @brief アトムリストからATOMのprevアトムを設定する.
   *        アトムリストから履歴アトムを読み飛ばさないので,
   * 呼び出し側で適宜なんとかする
   * @memberof LmnSymbolAtom
   */
  void set_prev(LmnSymbolAtomRef prev) { this->prev = prev; }
  /**
   * @brief アトムリストからATOMのnextアトムを取得する.
   *        アトムリストから履歴アトムを読み飛ばさないので,
   * 呼び出し側で適宜なんとかする
   * @memberof LmnSymbolAtom
   */
  LmnSymbolAtomRef get_next() const { return this->next; }
  /**
   * @brief アトムリストからATOMのnextアトムを設定する.
   *        アトムリストから履歴アトムを読み飛ばさないので,
   * 呼び出し側で適宜なんとかする
   * @memberof LmnSymbolAtom
   */
  void set_next(LmnSymbolAtomRef next) { this->next = next; }
  /** ファンクタIDの取得/設定,
   * ファンクタIDからリンク数の取得のユーティリティ
   * （プロキシはリンク1本分余分にデータ領域があるので分岐する）
   */

  /**
   * @brief アトムATOMのプロセスIDを取得
   * @memberof LmnSymbolAtom
   */
  LmnWord get_id() const { return this->procId; }
  /**
   * @brief アトムATOMのプロセスIDを設定
   * @memberof LmnSymbolAtom
   */
  void set_id(LmnWord id) { this->procId = id; }
  /**
   * @brief ファンクタIDの取得
   * @memberof LmnSymbolAtom
   */
  LmnFunctor get_functor() const { return static_cast<LmnFunctor>(this->link_header.functor); }
  /**
   * @brief ファンクタIDの設定
   * @memberof LmnSymbolAtom
   */
  void set_functor(LmnFunctor func) { this->link_header.functor = slim::vm::functor(func); }
  /**
   * @brief 価数の取得
   * @memberof LmnSymbolAtom
   */
  int get_arity() const { return LMN_FUNCTOR_ARITY(lmn_functor_table, this->get_functor()); }
  /**
   * @brief リンク本数の取得
   * @memberof LmnSymbolAtom
   */
  int get_link_num() const { return slim::vm::functor(get_functor()).get_link_num(); }

  /* アトムATOMのN番目のリンク属性/リンクデータを取得 */
  /**
   * @brief アトムATOMのN番目のリンク属性を取得
   * @memberof LmnSymbolAtom
   */
  LmnLinkAttr get_attr(int n) const { return static_cast<LmnLinkAttr>(this->link_header.attr[n]); }
  /**
   * @brief アトムATOMのN番目のリンク属性を設定
   * @memberof LmnSymbolAtom
   */
  void set_attr(int n, LmnLinkAttr attr) { this->link_header.attr[n] = static_cast<slim::vm::link_attribute_flag>(attr); }
  /**
   * @brief アトムATOMのN番目のリンク情報を取得
   * @memberof LmnSymbolAtom
   */
  LmnAtomRef get_link(int n) const { return this->links[get_attr_word_size(this->get_arity()) + n]; }
  /**
   * @brief アトムATOMのN番目のリンク属性を設定
   * @memberof LmnSymbolAtom
   */
  void set_link(int n, LmnAtomRef v) { this->links[get_attr_word_size(this->get_arity()) + n] = v; }

  /**
   * @brief アトムATOMのN番目のリンク情報のフィールドへのポインタを取得する
   * @memberof LmnSymbolAtom
   */
  const LmnAtomRef *get_plink(int n) const { return &this->links[get_attr_word_size(this->get_arity()) + n]; }

  /**
   * @brief check whether an atom is a proxy atom.
   * @memberof LmnSymbolAtom
   */
  bool is_proxy() const { return slim::vm::functor(get_functor()).is_proxy(); }

  /**
   * @brief get a string representation of a symbol atom.
   * @memberof LmnSymbolAtom
   */
  const char *str() const { return LMN_SYMBOL_STR(LMN_FUNCTOR_NAME_ID(lmn_functor_table, this->get_functor())); }

  /**
   * @brief リンク属性ATTRであるアトムATOMのファンクタがFUNCならばTRUEを返す
   * @memberof LmnSymbolAtom
   */
  bool has_functor(slim::vm::link_attribute attr, LmnFunctor functor) const {
    return !attr.is_data() && (get_functor() == functor);
  }

  /**
   * @brief get the membrane of a proxy
   * @memberof LmnSymbolAtom
   */
  LmnMembrane *get_proxy_membrane() const {
    return (LmnMembrane *)get_link(2);
  }

  /**
   * @brief set the membrane of a proxy
   * @memberof LmnSymbolAtom
   */
  void set_proxy_membrane(LmnMembrane *X) {
    set_link(2, X);
  }

  /* 以下, 履歴管理用アトム(nakata)の追加関数*/
  void atom_swap_forward();
  void swap_to_head(LmnSymbolAtomRef head);
  void remove_atom();
  /* ここまで(nakata)*/

  /**
   * @brief アトムのサイズを取得する
   * @memberof LmnSymbolAtom
   *
   * @details size of atom の加算は prev, next, id, functorのワード
   */
  static inline size_t calc_size(int arity) {
    return offsetof(struct LmnSymbolAtom, links) + (get_attr_word_size(arity) + arity) * LMN_WORD_BYTES;
  }

  /* リンク番号のタグのワード数。ファンクタと同じワードにある分も数える */
  static inline int get_attr_word_size(int arity) {
    return 1 + ((arity + sizeof(LmnFunctor) - 1) >> LMN_WORD_SHIFT);
  }
};

/**
 * @brief ファンクタから価数を取得する
 * @memberof LmnSymbolAtom
 */
static inline int LMN_FUNCTOR_GET_LINK_NUM(LmnFunctor func) {
  return slim::vm::functor(func).get_link_num();
}

/**
 * @brief アトムのサイズを取得する
 * @memberof LmnSymbolAtom
 *
 * @details size of atom の加算は prev, next, id, functorのワード
 */
static inline size_t LMN_SATOM_SIZE(int arity) {
  return LmnSymbolAtom::calc_size(arity);
}

/**
 * @brief リンク属性ATTRであるアトムATOMのファンクタがFUNCならばTRUEを返す
 * @memberof LmnSymbolAtom
 */
static inline bool LMN_HAS_FUNCTOR(LmnSymbolAtomRef atom, LmnLinkAttr attr, LmnFunctor func) {
  return atom->has_functor(slim::vm::link_attribute(static_cast<slim::vm::link_attribute_flag>(attr)), func);
}

/**
 * @brief check whether a link attribute is for data.
 * @memberof LmnLinkAttr
 */
static inline bool LMN_ATTR_IS_DATA(LmnLinkAttr attr) {
  return slim::vm::link_attribute(static_cast<slim::vm::link_attribute_flag>(attr)).is_data();
}

/**
 * @brief make a link attribute for link from value
 * @memberof LmnLinkAttr
 * TODO: this function should probably be deleted.
 */
static inline LmnLinkAttr LMN_ATTR_MAKE_LINK(int X) { return X; }
/**
 * @brief get link attribute value (remove tag)
 * @memberof LmnLinkAttr
 */
static inline int LMN_ATTR_GET_VALUE(LmnLinkAttr X) {
  return slim::vm::link_attribute(static_cast<slim::vm::link_attribute_flag>(X)).get_link_value();
}

/**
 * @brief get the membrane of a proxy
 * @memberof LmnSymbolAtom
 */
static inline LmnMembrane *LMN_PROXY_GET_MEM(LmnSymbolAtomRef PROXY_ATM) {
  return PROXY_ATM->get_proxy_membrane();
}
/**
 * @brief set the membrane of a proxy
 * @memberof LmnSymbolAtom
 */
static inline void LMN_PROXY_SET_MEM(LmnSymbolAtomRef PROXY_ATM, LmnMembrane *X) {
  return PROXY_ATM->set_proxy_membrane(X);
}
/**
 * @brief check whether a functor is a proxy functor.
 * @memberof LmnFunctor
 */
static inline BOOL LMN_IS_PROXY_FUNCTOR(LmnFunctor FUNC) {
  return slim::vm::functor(FUNC).is_proxy();
}
/**
 * @brief check whether a functor represents a symbol atom.
 * @memberof LmnFunctor
 */
static inline BOOL LMN_IS_SYMBOL_FUNCTOR(LmnFunctor FUNC) {
  return slim::vm::functor(FUNC).is_symbol();
}

/**
 * @brief get a string representation of a functor.
 * @memberof LmnFunctor
 */
static inline const char *LMN_FUNCTOR_STR(LmnFunctor F) {
  return slim::vm::functor(F).to_string();
}

/**
 * @brief check whether a link attribute is for data except an exclamation atom.
 * @memberof LmnLinkAttr
 */
static inline BOOL LMN_ATTR_IS_DATA_WITHOUT_EX(LmnLinkAttr ATTR) {
  return slim::vm::link_attribute(static_cast<slim::vm::link_attribute_flag>(ATTR)).is_data_except_hyperlink();
}
/**
 * @brief check whether a link attribute is for an exclamation atom.
 * @memberof LmnLinkAttr
 */
static inline BOOL LMN_ATTR_IS_EX(LmnLinkAttr ATTR) {
  return slim::vm::link_attribute(static_cast<slim::vm::link_attribute_flag>(ATTR)).is_hyperlink();
}
/**
 * @brief check whether a functor represents an exclamation atom.
 * @memberof LmnFunctor
 */
static inline BOOL LMN_IS_EX_FUNCTOR(LmnFunctor FUNC) {
  return slim::vm::functor(FUNC).is_hyperlink();
}

/**
 * @brief create a new symbol atom.
 * @member LmnSymbolAtom
 */
LmnSymbolAtomRef lmn_new_atom(LmnFunctor f);
/**
 * @brief delete a symbol atom
 * @member LmnSymbolAtom
 */
void lmn_delete_atom(LmnSymbolAtomRef ap);

/**
 * @brief copy an atom which \e attr indicates the kind of \e atom.
 * @memberof LmnAtom
 */
LmnAtomRef lmn_copy_atom(LmnAtomRef atom, LmnLinkAttr attr);
/**
 * @brief copy a symbol atom.
 * @memberof LmnSymbolAtom
 */
LmnSymbolAtomRef lmn_copy_satom(LmnSymbolAtomRef atom);
/**
 * @brief copy a data atom which \e attr indicates the kind of \e atom.
 * @memberof LmnDataAtom
 */
LmnDataAtomRef lmn_copy_data_atom(LmnDataAtomRef atom, LmnLinkAttr attr);
/**
 * @brief copy a symbol atom and its arguments of data atoms.
 * @memberof LmnSymbolAtom
 * @sa free_symbol_atom_with_buddy_data
 */
LmnSymbolAtomRef lmn_copy_satom_with_data(LmnSymbolAtomRef atom,
                                          BOOL is_new_hl);
/**
 * @brief free an atom which \e attr indicates the kind of \e atom.
 * @memberof LmnAtom
 */
void lmn_free_atom(LmnAtomRef atom, LmnLinkAttr attr);
/**
 * @brief シンボルアトムとリンクで接続しているデータアトムを解放する
 * @memberof LmnSymbolAtom
 * @sa lmn_copy_satom_with_data
 */
void free_symbol_atom_with_buddy_data(LmnSymbolAtomRef atom);
/**
 * @brief check whether two atoms have the same functors.
 * @memberof LmnAtom
 */
BOOL lmn_eq_func(LmnAtomRef atom0, LmnLinkAttr attr0, LmnAtomRef atom1,
                 LmnLinkAttr attr1);
/**
 * @brief check whether a data atom is ground.
 * @memberof LmnDataAtom
 *
 * @details A data atom is usually ground, while for a special atom it depends
 * on its user callback.
 */
BOOL lmn_data_atom_is_ground(LmnDataAtomRef atom, LmnLinkAttr attr,
                             ProcessTbl **hlinks);
/**
 * @brief check whether two data atoms equal.
 * @memberof LmnDataAtom
 */
BOOL lmn_data_atom_eq(LmnDataAtomRef atom1, LmnLinkAttr attr1,
                      LmnDataAtomRef atom2, LmnLinkAttr attr2);
/**
 * @brief get double value from a data atom.
 * @memberof LmnDataAtom
 *
 * @details if \e atom does not represent double, the return value is undefined.
 */
double lmn_get_double(LmnDataAtomRef atom);
/**
 * @brief create double atom from double value.
 * @memberof LmnDataAtom
 */
LmnDataAtomRef lmn_create_double_atom(double d);
/**
 * @brief delete double atom.
 * @memberof LmnDataAtom
 */
void lmn_destroy_double_atom(LmnDataAtomRef atom);

BOOL lmn_is_string(LmnAtomRef atom, LmnLinkAttr attr);

/**
 * @brief get the pointer to a double atom.
 * @memberof LmnDataAtom
 */
#ifdef LMN_DOUBLE_IS_IMMEDIATE
#define LMN_GETREF_DOUBLE(Atom) ((double *)&Atom)
#else
#define LMN_GETREF_DOUBLE(Atom) ((double *)Atom)
#endif

#define LMN_COPY_DBL_ATOM(Dst, Src)                                            \
  do {                                                                         \
    (Dst) = lmn_create_double_atom(lmn_get_double(Src));                       \
  } while (0)

/* @} */

/**
 * @brief free memory pools for atoms.
 */
void free_atom_memory_pools(void);

/**
 * @brief initialize memory pools for atoms.
 */
void mpool_init(void);

#endif /* LMN_ATOM_H */
