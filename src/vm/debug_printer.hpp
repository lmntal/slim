#ifndef LMN_DEBUG_PRINTER_H
#define LMN_DEBUG_PRINTER_H

#include "lmntal.h"
#include "atom.h"
#include "react_context.hpp"

#include <sstream>

namespace slim {
namespace debug_printer {
/**
 * 数値の16進表現を返す
 * 必ず先頭に0xが付加される
 * 桁数は最小限になる
 */
template <class T>
inline std::string to_hex_string(T value) {
  if (value == 0) {
    return "0x0";
  } else {
    std::ostringstream oss;
    oss << std::showbase << std::hex << value;
    return oss.str();
  }
}
/**
 * 数値の16進表現を返す
 * 必ず先頭に0xが付加される
 * 桁数は最小限になる
 */
template <>
inline std::string to_hex_string<unsigned char>(unsigned char value) {
  return to_hex_string((unsigned int) value);
}
/**
 * アトムの文字列表現を返す
 * リンクをたどって接続先のアトムを表現できるならその表現を含む
 */
std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr);
/**
 * アトムの文字列表現を返す
 * リンクをたどらないで短く表現する
 */
std::string to_string_atom_short(const LmnAtomRef atom, LmnLinkAttr attr);
/**
 * レジスタの文字列表現を返す
 * ttの値によって，表現の形式は変更される
 */
std::string to_string_reg(const LmnRegisterRef reg);
/**
 * レジスタ配列の文字列表現を返す
 * 各レジスタに格納されているアトム間のリンク関係は適切に表現されない
 */
std::string to_string_regarray(const LmnRegisterArray* reg_array);
/**
 *  命令名の文字列表現を返す
 */
std::string to_string_instrop(LmnInstrOp op);
/**
 * 命令の引数を含む文字列表現を返す
*/
std::string to_string_instr(const LmnRuleInstr instr);
/**
 * アトムリストの文字列表現を返す
 * リスト内の各アトムのリンク関係は適切に表現される
*/
std::string to_string_atomlist(const AtomListEntry* atomlist);
/**
 * 膜内のすべてのアトムリストの文字列表現を返す
 * すべてのアトムのリンク関係は適切に表現される
 */
std::string to_string_atomlists(std::map<LmnFunctor,AtomListEntry*> atomlists);
/**
 * 膜の文字列表現を返す
 */
std::string to_string_mem(const LmnMembraneRef mem);
/**
 * ファンクタの文字列表現を返す
 * 「アトム名_価数」の形式
 */
std::string to_string_functor(LmnFunctor func);
/**
 * アトムの文字列表現を返す
 * データアトムなら，データ・属性の16進数表現を含む
 * シンボルアトムなら，自アトムおよび直接リンクしているアトムの，アドレス・ID・ファンクタ・価数・接続先でのリンク番号を含む
 */
std::string to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr);
/**
 * レジスタの文字列表現を返す
 * ttの値に応じて，各データ構造のより詳細な表現を返す
 */
std::string to_string_dev_reg(const LmnRegisterRef reg);
/**
 * 膜の文字列表現を返す
 * 膜内の各アトムのより詳細な情報を含む
 */
std::string to_string_dev_mem(const LmnMembraneRef mem);
/**
 * 各種データ構造に典型的なアドレスおよびIDの文字列表現を返す
 */
inline std::string print_object_ids(const void *address, unsigned long id) {
  return "Addr[" + to_hex_string(address) + "], ID[" + std::to_string(id) + "]";
}
/**
 * 各種データ構造に典型的な名前およびIDの文字列表現を返す
 */
inline std::string print_object_ids(lmn_interned_str name, unsigned long id) {
  std::string str = name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(name);
  return "Name[" + str + "], ID[" + std::to_string(id) + "]";
}
/**
 * 各種データ構造に典型的な名前・識別子・IDの文字列表現を返す
 */
inline std::string print_object_ids(lmn_interned_str name, const void *address, unsigned long id) {
  std::string str = name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(name);
  return "Name[" + str + "], " + print_object_ids(address, id);
}

} // namespace debug_printer
} // namespace slim

#endif
