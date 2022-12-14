#include "debug_printer.hpp"
#include "stringifier.hpp"
#include "task.h"
#include "atomlist.hpp"

namespace slim {
namespace debug_printer {

std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  return slim::stringifier::lmn_stringify_atom(atom, attr);
}

std::string to_string_satom(const LmnSymbolAtomRef atom) {
  return slim::stringifier::lmn_stringify_atom(atom, LMN_ATTR_MAKE_LINK(0));
}

std::string to_string_datom(const LmnDataAtomRef atom) {
  return to_hex_string(atom);
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
    retVal += to_string_reg((LmnRegisterRef) &(reg_array->at(i)));
    if (i != reg_len - 1) {
      retVal += reg_array_delimiter;
    }
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

    if (i != argSize - 1) {
      retVal << ", ";
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
    // FIXME
    retVal += to_string_satom(*begin);
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

std::string to_string_mem(const LmnMembraneRef mem) {
  return slim::stringifier::lmn_stringify_mem(mem);
}

std::string to_string_dev_satom(const LmnSymbolAtomRef atom) {
  return "Dev!" + to_string_satom(atom);
}

std::string to_string_dev_datom(const LmnDataAtomRef atom) {
  return "Dev!" + to_string_datom(atom);
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
        return "\n   " + to_string_dev_datom((LmnDataAtomRef)wt);
      } else {
        return to_string_satom((LmnSymbolAtomRef)wt) + "\n" + to_string_dev_satom((LmnSymbolAtomRef)wt);
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

} // namespace debug_printer
} // namespace slim
