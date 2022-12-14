#ifndef LMN_DEBUG_PRINTER_H
#define LMN_DEBUG_PRINTER_H

#include "lmntal.h"
#include "atom.h"
#include "react_context.hpp"

#include <sstream>

namespace slim {
namespace debug_printer {

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

template <>
inline std::string to_hex_string<unsigned char>(unsigned char value) {
  return to_hex_string((unsigned int) value);
}

constexpr char reg_array_delimiter[] = " | ";
constexpr char atomlist_delimiter[] = " | ";
constexpr char reg_array_prefix[] = "[ ";
constexpr char reg_array_suffix[] = " ]";
constexpr char atomlist_prefix[] = "[ ";
constexpr char atomlist_suffix[] = " ]";

std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr);
std::string to_string_satom(const LmnSymbolAtomRef atom);
std::string to_string_datom(const LmnDataAtomRef atom);
std::string to_string_reg(const LmnRegisterRef reg);
std::string to_string_regarray(const LmnRegisterArray* reg_array);
std::string to_string_instrop(LmnInstrOp op);
std::string to_string_instr(const LmnRuleInstr instr);
std::string to_string_atomlist(const AtomListEntry* atomlist);
std::string to_string_mem(const LmnMembraneRef mem);
std::string to_string_functor(LmnFunctor func);

std::string to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr);
std::string to_string_dev_satom(const LmnSymbolAtomRef atom);
std::string to_string_dev_datom(const LmnDataAtomRef atom);
std::string to_string_dev_reg(const LmnRegisterRef reg);
std::string to_string_dev_regarray(const LmnRegisterArray* reg_array);
std::string to_string_dev_atomlist(const AtomListEntry* atomlist);
std::string to_string_dev_mem(const LmnMembraneRef mem);

inline std::string print_object_ids(const void *address, unsigned long id) {
  return "Addr[" + to_hex_string(address) + "], ID[" + std::to_string(id) + "]";
}

inline std::string print_object_ids(lmn_interned_str name, unsigned long id) {
  std::string str = name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(name);
  return "Name[" + str + "], ID[" + std::to_string(id) + "]";
}

inline std::string print_object_ids(lmn_interned_str name, const void *address, unsigned long id) {
  std::string str = name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(name);
  return "Name[" + str + "], " + print_object_ids(address, id);
}

} // namespace debug_printer
} // namespace slim

#endif
