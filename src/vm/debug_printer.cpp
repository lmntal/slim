#include "debug_printer.hpp"
#include "stringifier.hpp"
#include "task.h"
#include "atomlist.hpp"

#include <iostream>

namespace slim {
namespace debug_printer {

using link_name_table = std::map<int,std::string>;

struct PrintRecord {
  bool done;
  link_name_table table;
};

using dump_history_table = std::map<LmnSymbolAtomRef,PrintRecord>;

// constants
constexpr size_t MAX_CALL_DEPTH = 1000;
constexpr char LINK_PREFIX[] = "L";

// prototype
// symbol atom
static LmnLinkAttr get_attr(LmnSymbolAtomRef atom);
// maps
static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom);
static int get_overall_links(dump_history_table &table);

// internal dumper
static std::string _to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth);
static std::string _to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht);
static std::string _to_string_satom_arg(const LmnSymbolAtomRef atom, int arg_index, dump_history_table &ht, size_t call_depth);
static std::string _to_string_satom_args(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth);
static std::string _to_string_atom_name(const LmnSymbolAtomRef atom);
static std::string _to_string_atom_name(const LmnSymbolAtomRef atom, LmnFunctor functor);
static std::string _to_string_satom(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht, size_t call_depth);
std::string _to_string_datom(const LmnAtomRef data, LmnLinkAttr attr);
std::string _to_string_mem(const LmnMembraneRef mem, dump_history_table &ht);
// prototype end

// Get PrintRecord from table.
static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom) {
  auto ent = table.find(atom);
  if (ent == table.end()) { // not found
    auto iter = table.emplace(atom, PrintRecord({false,{}}));
    return (*(iter.first)).second;
  } else { // found
    return (*ent).second;
  }
}

static int get_overall_links(dump_history_table &table) {
  int links = 0;
  for (auto &p : table) {
    for (auto &q : p.second.table) {
      links++;
    }
  }
  return links;
}

// Return string representation of an atom. Depending on its attribute, it's treated as a symbol atom or a data atom.
std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  dump_history_table ht;
  return _to_string_atom(atom, attr, ht, 0);
}

static std::string _to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return _to_string_datom(atom, attr);
  } else {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    LmnFunctor f = satom->get_functor(); // currently ignore functor
    int link_target_n = LMN_ATTR_GET_VALUE(attr); // 接続先リンクの番号
    return _to_string_satom(satom, link_target_n, ht, call_depth);
  }
}

static std::string _to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht) {
  PrintRecord &record = get_history_entry(ht, atom);

  auto res = record.table.find(link_position);

  if (res == record.table.end()) { // not found
    std::string name = LINK_PREFIX + std::to_string(get_overall_links(ht));
    record.table.emplace(link_position, name);
    return name;
  } else { // found
    return (*res).second;
  }
}

// Return string representation of atom's link.
static std::string _to_string_satom_arg(const LmnSymbolAtomRef atom, int arg_index, dump_history_table &ht, size_t call_depth) {
  PrintRecord &src_record = get_history_entry(ht, atom);

  LmnAtomRef linked_atom = atom->get_link(arg_index);
  LmnLinkAttr link_attr = atom->get_attr(arg_index);

  if (src_record.table.find(arg_index) != src_record.table.end()) { // found
    return _to_string_link(atom, arg_index, ht);
  } else { // not found
    return _to_string_atom(linked_atom, link_attr, ht, call_depth);
  }
}

// Return string representation of atom's links.
static std::string _to_string_satom_args(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth) {
  int link_num = atom->get_link_num();

  if (call_depth > 0) {
    link_num--;
  }

  if (link_num == 0) { // if atom has no links, do not show parenthes
    return "";
  } else {
    std::string retVal = "";
    retVal += "(";

    for (int i = 0; i < link_num; i++) {
      if (i > 0) {
        retVal += ",";
      }
      retVal += _to_string_satom_arg(atom, i, ht, call_depth + 1);
    }

    retVal += ")";
    return retVal;
  }
}

// Return string representation of a symbol atom with its linked atoms.
static std::string _to_string_satom(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht, size_t call_depth) {
  LmnFunctor functor = atom->get_functor();
  LmnArity arity = atom->get_arity();

  PrintRecord& record = get_history_entry(ht, atom);

  if (
    (call_depth > 0 && link_position != arity - 1) || // ダンプの主な対象ではなく，このアトムに最終リンク以外で繋がっている
    (call_depth > 0 && record.done) || // ダンプの主な対象ではなく，すでにダンプされた
    (call_depth > MAX_CALL_DEPTH) // 再帰呼び出しの最大の深さを超えた
  ) {
    return _to_string_link(atom, link_position, ht);
  }

  record.done = true;

  return _to_string_atom_name(atom, functor) + _to_string_satom_args(atom, ht, call_depth);
}

static std::string _to_string_atom_name(const LmnSymbolAtomRef atom) {
  LmnFunctor functor = atom->get_functor();
  lmn_interned_str name = lmn_functor_table->get_entry(functor)->name;
  return lmn_id_to_name(name);
}

static std::string _to_string_atom_name(const LmnSymbolAtomRef atom, LmnFunctor functor) {
  lmn_interned_str name = lmn_functor_table->get_entry(functor)->name;
  return lmn_id_to_name(name);
}

std::string _to_string_datom(const LmnAtomRef data, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    return std::to_string((long)data);
  case LMN_DBL_ATTR:
    return std::to_string(lmn_get_double((LmnDataAtomRef)data));
  case LMN_SP_ATOM_ATTR: { // string etc...
    LmnPortRef port = lmn_make_output_string_port();
    SP_ATOM_DUMP(data, port);
    LmnStringRef str = lmn_port_output_string(port);
    lmn_port_free(port);
    return str->str;
  }
  default:
    return "[data(" + to_hex_string(data) + "), attr(" + to_hex_string(data) + ")]";
  }
}

std::string to_string_reg(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  switch (reg->tt) {
    case TT_ATOM:
      return to_string_atom((LmnAtomRef)reg->wt, reg->at);
    case TT_MEM:
      return to_hex_string(reg->wt) + "(" + std::to_string(reg->at) + ",mem)";
    case TT_OTHER:
      return to_hex_string(reg->wt) + "(" + std::to_string(reg->at) + ",other)";
    default:
      return "unknown tt type";
  }
}

std::string to_string_regarray(const LmnRegisterArray* reg_array) {
  if (reg_array == nullptr) {
    return "null";
  }

  std::string retVal = reg_array_prefix;
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    if (i > 0) {
      retVal += reg_array_delimiter;
    }
    retVal += to_string_reg((LmnRegisterRef) &(reg_array->at(i)));
  }
  retVal += reg_array_suffix;

  return retVal;
}

std::string to_string_instrop(LmnInstrOp op) {
  for (auto &p : instr_spec) {
    if (op == p.first) {
      return p.second.op_str;
    }
  }
  return "unknown";
}

std::string to_string_instr(const LmnRuleInstr instr) {
  if (instr == nullptr) {
    return "null";
  }

  LmnRuleInstr instr_copy = instr;
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr_copy, op);

  std::vector<ArgType> argtype_vec;

  for (auto &p : instr_spec) {
    if (op == p.first) {
      argtype_vec = p.second.args;
      break;
    }
  }

  if (argtype_vec.empty()) {
    return "unknown";
  }

  std::ostringstream retVal;
  retVal << to_string_instrop(op) << " ";

  for (size_t i = 0, argSize = argtype_vec.size(); i < argSize; i++) {
    if (i > 0) {
      retVal << ", ";
    }

    ArgType argType = argtype_vec.at(i);
    if (op == INSTR_NEWATOM && argType == ArgFunctor) {
      LmnLinkAttr at;
      READ_VAL(LmnLinkAttr, instr_copy, at);
      retVal << (unsigned int) at;
      continue;
    }
    switch (argType) {
      case InstrVar:
        LmnInstrVar arg_instrvar;
        READ_VAL(LmnInstrVar, instr_copy, arg_instrvar);
        retVal << arg_instrvar;
        break;
      case Label:
        LmnJumpOffset arg_label;
        READ_VAL(LmnJumpOffset, instr_copy, arg_label);
        retVal << arg_label;
        break;
      case InstrVarList:
        retVal << "{ ";
        int16_t num;
        READ_VAL(int16_t, instr_copy, num);
        while (num--) {
          LmnInstrVar n;
          READ_VAL(LmnInstrVar, instr_copy, n);
          retVal << n;
          if (num) {
            retVal << ", ";
          }
        }
        retVal << " }";
        break;
      case String:
        LmnInstrVar arg_string;
        READ_VAL(lmn_interned_str, instr_copy, arg_string);
        retVal << "\"" << lmn_id_to_name(arg_string) << "\"";
        break;
      case LineNum:
        LmnLineNum arg_linenum;
        READ_VAL(LmnLineNum, instr_copy, arg_linenum);
        retVal << arg_linenum;
        break;
      case ArgFunctor:
        LmnLinkAttr arg_functor;
        READ_VAL(LmnLinkAttr, instr_copy, arg_functor);
        retVal << (unsigned int) arg_functor;
        break;
      case ArgRuleset:
        LmnRulesetId arg_ruleset;
        READ_VAL(LmnRulesetId, instr_copy, arg_ruleset);
        retVal << arg_ruleset;
        break;
      case InstrList:
        LmnSubInstrSize arg_instrlist;
        READ_VAL(LmnSubInstrSize, instr_copy, arg_instrlist);
        retVal << arg_instrlist;
        break;
      default:
        break;
    }
  }

  if (op == INSTR_FINDATOM || op == INSTR_FINDATOM2 || op == INSTR_FINDATOMP || op == INSTR_NEWATOM) {
    LmnFunctor func;
    READ_VAL(LmnFunctor, instr_copy, func);
    retVal << ", ";
    retVal << to_string_functor(func);
  }

  return retVal.str();
}

std::string to_string_atomlist(const AtomListEntry* atomlist) {
  if(atomlist == nullptr) {
    return "null";
  }

  auto begin = std::begin(*atomlist);
  auto end = std::end(*atomlist);

  std::string retVal = atomlist_prefix;
  while (begin != end) {
    // TODO check
    retVal += to_string_atom(*begin, LMN_ATTR_MAKE_LINK(0));
    ++begin;
    if (begin != end) {
      retVal += atomlist_delimiter;
    }
  }
  retVal += atomlist_suffix;

  return retVal;
}

std::string to_string_functor(LmnFunctor func) {
  if (lmn_functor_table == nullptr) {
    return "Functor table is null.\n";
  }
  auto entry = lmn_functor_table->get_entry(func);
  return std::string(lmn_id_to_name(entry->name)) + "_" + std::to_string(entry->arity);
}

std::string _to_string_mem(const LmnMembraneRef mem, dump_history_table &ht) {
  return "no impl.";
}

std::string to_string_mem(const LmnMembraneRef mem) {
  dump_history_table ht;
  return _to_string_mem(mem, ht);
}

std::string to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  return "Dev!" + to_string_atom(atom, attr);
}

std::string to_string_dev_reg(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  switch (tt) {
    case TT_ATOM:
      if (LMN_ATTR_IS_DATA(at)) {
        return "\n   " + to_string_atom((LmnAtomRef)wt, at);
      } else {
        return to_string_atom((LmnAtomRef)wt, at) + "\n" + to_string_dev_atom((LmnAtomRef)wt, at);
      }
    case TT_MEM:
      return print_object_ids(((LmnMembraneRef)wt)->NAME_ID(), (LmnMembraneRef)wt, ((LmnMembraneRef)wt)->mem_id())
              + "\n" + to_string_dev_mem((LmnMembraneRef)wt);
    case TT_OTHER:
      return "wt[" + to_hex_string(wt) + "], at[" + to_hex_string(at) + "], tt[other]";
    default:
      return "unknown tt type";
  }
}

std::string to_string_dev_regarray(const LmnRegisterArray* reg_array) {
  return "Dev!" + to_string_regarray(reg_array);
}

std::string to_string_dev_atomlist(const AtomListEntry* atomlist) {
  return "Dev!" + to_string_atomlist(atomlist);
}

std::string to_string_dev_mem(const LmnMembraneRef mem) {
  return "Dev!" + to_string_mem(mem);
}

static LmnLinkAttr get_attr(LmnSymbolAtomRef atom) {
  int link_num = atom->get_link_num();
  for (int i = 0; i < link_num; i++) {
    LmnAtomRef a = atom->get_link(i);
    LmnLinkAttr pair_attr = atom->get_attr(i);
    if (LMN_ATTR_IS_DATA(pair_attr)) {
      continue;
    }
    LmnSymbolAtomRef pair_atom = (LmnSymbolAtomRef)a;
    int pair_link_num = pair_atom->get_link_num();
    for (int j = 0; j < pair_link_num; j++) {
      if (pair_atom->get_link(j) == atom) {
        return LMN_ATTR_MAKE_LINK(j);
      }
    }
  }
  return LMN_ATTR_MAKE_LINK(0);
}

} // namespace debug_printer
} // namespace slim
