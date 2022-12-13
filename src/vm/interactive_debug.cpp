#include "interactive_debug.hpp"
#include "stringifier.hpp"

#include "task.h"
#include "../verifier/statespace.h"

#include <iostream>
#include <string>
#include <sstream>
#include <stdexcept>
#include <cctype>
#include <iomanip>

#include <sys/ioctl.h>
#include <unistd.h>
#include <termios.h>

enum class DebugCommand {
  CONTINUE,
  HELP,
  FINISH,
  RUN,
  START,
  STEP_RULE,
  STEP_INSTR,
  INFO_REGISTER,
  INFO_ATOMLIST,
  INFO_MEMBRANE,
  INFO_BREAKPOINT,
  INFO_STATESPACE,
  BREAK_RULE,
  BREAK_INSTR,
  DELETE_RULE,
  DELETE_INSTR,
  __UNKNOWN,
  __AMBIGUOUS,
  __EMPTY
};

static const std::map<DebugCommand, std::vector<std::string>> debug_commands = {
  {DebugCommand::CONTINUE, {"continue"}},
  {DebugCommand::HELP, {"help"}},
  {DebugCommand::FINISH, {"finish"}},
  {DebugCommand::RUN, {"run"}},
  {DebugCommand::START, {"start"}},
  {DebugCommand::STEP_RULE, {"step", "rule"}},
  {DebugCommand::STEP_INSTR, {"step", "instruction"}},
  {DebugCommand::INFO_REGISTER, {"info", "registers"}},
  {DebugCommand::INFO_ATOMLIST, {"info", "atomlist"}},
  {DebugCommand::INFO_MEMBRANE, {"info", "membrane"}},
  {DebugCommand::INFO_BREAKPOINT, {"info", "breakpoints"}},
  {DebugCommand::INFO_STATESPACE, {"info", "statespaces"}},
  {DebugCommand::BREAK_RULE, {"break", "rule"}},
  {DebugCommand::BREAK_INSTR, {"break", "instruction"}},
  {DebugCommand::DELETE_RULE, {"delete", "rule"}},
  {DebugCommand::DELETE_INSTR, {"delete", "instruction"}}
};

// prototype
// Get hex form of value
template <class T>
static inline std::string to_hex_string(T value);
template <>
inline std::string to_hex_string<unsigned char>(unsigned char value);

// check if vector contains value
template <class T>
static inline bool vector_contains(const std::vector<T> &vector, T value);

static inline bool string_starts_with(const std::string &str1, const std::string &str2);

static std::string stringify_register(const LmnRegisterRef reg);
static std::string stringify_register_dev(const LmnRegisterRef reg);
static std::string stringify_regarray(const LmnRegisterArray *reg_array);
static std::string stringify_instr(const LmnRuleInstr instr);
static std::string stringify_atomlist(const AtomListEntry *atomlist);
static std::pair<DebugCommand,std::vector<std::string>> parse_command(const std::string &command);
static std::string get_membrane_tree(const LmnMembraneRef mem, std::string prefix, const LmnMembraneRef global, const LmnMembraneRef current);
static LmnMembraneRef get_pointer_to_membrane_by_id(const LmnMembraneRef mem, unsigned long id);
static bool print_section(const std::vector<std::string> &lines, size_t from_line, size_t screen_height, size_t screen_width);
// prototype end

const char* get_instr_name(int id) {
  for (auto &p : instr_spec)
    if (id == p.first)
      return p.second.op_str;
  return "unknown";
}

template <class T>
static inline std::string to_hex_string(T value) {
  std::ostringstream oss;
  oss << std::showbase << std::hex << value;
  return oss.str();
}

template <>
inline std::string to_hex_string<unsigned char>(unsigned char value) {
  if (value == 0) {
    return "0x0";
  } else {
    std::ostringstream oss;
    oss << std::showbase << std::hex << (unsigned int) value;
    return oss.str();
  }
}

static inline bool string_starts_with(const std::string &str1, const std::string &str2) {
  return str1.size() < str2.size() ? false : std::equal(str2.cbegin(), str2.cend(), str1.cbegin());
}

template <class T>
static inline bool vector_contains(const std::vector<T> &vector, T value) {
  auto end = vector.cend();
  auto res = std::find(vector.cbegin(), end, value);
  return res != end;
}

InteractiveDebugger::InteractiveDebugger() {
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  screen_height = w.ws_row;
  screen_width = w.ws_col;
}

InteractiveDebugger::~InteractiveDebugger() {}

// print register content
static std::string stringify_register(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  switch (tt) {
    case TT_ATOM:
      return slim::stringifier::lmn_stringify_atom((LmnAtomRef)wt, at);
    case TT_MEM:
      return to_hex_string(wt) + "(" + std::to_string(at) + ",mem)";
    case TT_OTHER:
      return to_hex_string(wt) + "(" + std::to_string(at) + ",other)";
    default:
      return "unknown tt type";
  }
}

static std::string stringify_register_dev(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  std::ostringstream retVal;
  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  switch (tt) {
    case TT_ATOM:
      if (LMN_ATTR_IS_DATA(at)) {
        retVal << "\n   " << slim::stringifier::lmn_stringify_atom((LmnAtomRef) wt, at);
      } else {
        retVal << slim::stringifier::lmn_stringify_atom((LmnAtomRef) wt, at) << "\n";
        retVal << slim::stringifier::stringify_atom_dev((LmnSymbolAtomRef) wt);
      }
      break;
    case TT_MEM: {
      lmn_interned_str str = ((LmnMembraneRef) wt)->NAME_ID();
      retVal << "Name[" << (str == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(str)) << "], ";
      retVal << "Addr[" << to_hex_string(wt) << "], ";
      retVal << "ID[" << ((LmnMembraneRef) wt)->mem_id() << "]\n";
      retVal << slim::stringifier::lmn_stringify_mem_dev((LmnMembraneRef) wt);
      break;
    }
    case TT_OTHER:
      retVal << "wt[" << to_hex_string(wt) << "], ";
      retVal << "at[" << to_hex_string(at) << "], ";
      retVal << "tt[other]";
      break;
    default:
      retVal << "unknown tt type";
      break;
  }

  return retVal.str();
}

static std::string stringify_regarray(const LmnRegisterArray *reg_array) {
  if (reg_array == nullptr) {
    return "null";
  }

  std::ostringstream retVal;

  retVal << "[ ";
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    retVal << stringify_register((LmnRegisterRef) &(reg_array->at(i)));
    if (i != reg_len - 1) {
      retVal << " | ";
    }
  }
  retVal << " ]";

  return retVal.str();
}

static std::string stringify_instr(const LmnRuleInstr instr) {
  if (instr == nullptr) {
    return "null";
  }

  std::ostringstream retVal;

  LmnRuleInstr instr_copy = instr;
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr_copy, op);

  retVal << get_instr_name(op) << " ";
  for (auto &p : instr_spec) {
    if (op == p.first) {
      for (size_t i = 0, argSize = p.second.args.size(); i < argSize; i++) {
        ArgType argType = p.second.args.at(i);
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
      break;
    }
  }

  if (op == INSTR_FINDATOM || op == INSTR_FINDATOM2 || op == INSTR_FINDATOMP || op == INSTR_NEWATOM) {
    LmnFunctor func;
    READ_VAL(LmnFunctor, instr_copy, func);
    retVal << ", ";
    retVal << lmn_id_to_name(lmn_functor_table->get_entry(func)->name);
    retVal << "_";
    retVal << (unsigned int) lmn_functor_table->get_entry(func)->arity;
  }

  return retVal.str();
}

static std::string stringify_atomlist(const AtomListEntry *atomlist) {
  if(atomlist == nullptr) {
    return "null";
  }

  std::ostringstream retVal;
  auto begin = std::begin(*atomlist);
  auto end = std::end(*atomlist);

  retVal << "[ ";

  while (begin != end) {
    retVal << slim::stringifier::lmn_stringify_atom(*begin, LMN_ATTR_MAKE_LINK(0));
    ++begin;
    if (begin != end) {
      retVal << " | ";
    }
  }

  retVal << " ]";
  return retVal.str();
}

static std::pair<DebugCommand,std::vector<std::string>> parse_command(const std::string &command) {
  if (command.empty()) {
    return {DebugCommand::__EMPTY, {}};
  }

  std::vector<std::string> command_vec;

  for (size_t start_index = 0, max = command.size(); start_index < max; ) {
    size_t space_index = command.find(' ', start_index);
    std::string temp = command.substr(start_index, space_index - start_index);

    if (!temp.empty()) {
      command_vec.push_back(temp);
    }
    if (space_index == std::string::npos) {
      break;
    }

    start_index = space_index + 1;
  }

  const size_t argc = command_vec.size();
  // reduce ambiguity
  bool has_ambiguous_arg = false;
  for (size_t i = 0; i < argc && !has_ambiguous_arg; i++) {
    std::vector<std::string> argument_candidate;
    for (auto &p : debug_commands) {
      if (i < p.second.size()) {
        if (std::equal(p.second.begin(), p.second.begin() + i, command_vec.begin())) {
          if (string_starts_with(p.second.at(i), command_vec.at(i))) {
            if (!vector_contains(argument_candidate, p.second.at(i))) {
              argument_candidate.push_back(p.second.at(i));
            }
          }
        }
      }
    }
    if (argument_candidate.size() == 1) {
      command_vec.at(i) = argument_candidate.at(0);
    } else if (argument_candidate.size() > 1) {
      argument_candidate.insert(argument_candidate.begin(), command_vec.at(i));
      return {DebugCommand::__AMBIGUOUS, argument_candidate};
    }
  }

  // identify command
  auto cmd_iter_end = debug_commands.end();
  auto cmd_iter_found = std::find_if(
    debug_commands.begin(), cmd_iter_end,
    [&command_vec, argc](std::pair<DebugCommand, std::vector<std::string>> p) -> bool {
        return std::equal(
          p.second.cbegin(), p.second.cend(),
          command_vec.cbegin()
        );
    }
  );

  if (cmd_iter_found == cmd_iter_end) {
    return {DebugCommand::__UNKNOWN, {}};
  } else {
    command_vec.erase(command_vec.begin(), command_vec.begin() + (*cmd_iter_found).second.size());
    return {(*cmd_iter_found).first, command_vec};
  }
}

static std::string get_membrane_tree(const LmnMembraneRef mem, std::string prefix, const LmnMembraneRef global, const LmnMembraneRef current) {
  if (mem == nullptr) {
    return "null";
  }

  // 自身の名前の取得
  std::string myname = "Membrane (Name[";
  lmn_interned_str in_str = mem->NAME_ID();
  myname += (in_str == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(in_str));
  myname += "], Addr[";
  myname += to_hex_string(mem);
  myname += "], ID[";
  myname += std::to_string(mem->mem_id());
  myname += "])";

  if (mem == global) {
    myname += " *global";
  }
  if (mem == current) {
    myname += " *current";
  }

  // 自身の名前を追加
  std::string retVal = prefix;
  retVal += "|--";
  retVal += myname;
  retVal += "\n";

  // 子があれば先に取得
  if (mem->child_head != nullptr) {
    retVal += get_membrane_tree(mem->child_head, prefix + "|  ", global, current);
    retVal += "\n";
  }

  // 兄弟(？)があれば続けて表示
  if (mem->next != nullptr) {
    // 間を調整
    if (mem->child_head != nullptr) {
      retVal += prefix;
      retVal += "|";
    }
    retVal += get_membrane_tree(mem->next, prefix, global, current);
    retVal += "\n";
  }

  retVal.pop_back();
  return retVal;
}

static LmnMembraneRef get_pointer_to_membrane_by_id(const LmnMembraneRef mem, unsigned long id) {
  if (mem == nullptr) {
    return nullptr;
  } else if (mem->mem_id() == id) {
    return mem;
  } else {
    LmnMembraneRef ref = get_pointer_to_membrane_by_id(mem->child_head, id);
    return ref != nullptr ? ref : get_pointer_to_membrane_by_id(mem->next, id);
  }
}

void InteractiveDebugger::start_session_on_entry() {
  bool continue_session = true;
  while (continue_session) {
    if (std::cin.eof()) {
      std::cout << "^D" << std::endl;
      input_eof = true;
      break;
    }

    std::string arg_string;
    std::cout << "(debugger) ";
    std::getline(std::cin, arg_string);

    const auto parse_result = parse_command(arg_string);
    const auto cmd = parse_result.first;
    const auto opt_argv = parse_result.second;
    const size_t opt_argc = opt_argv.size();

    if (cmd == DebugCommand::__EMPTY) {
      continue;
    }
    if (cmd == DebugCommand::__AMBIGUOUS) {
      std::cerr << "Ambiguous input.\n";
      continue;
    }

    switch (cmd) {
      // info breakpoints
      case DebugCommand::INFO_BREAKPOINT: {
        std::string s = "Breakpoints for instructions :\n";
        for (auto &i : breakpoints_on_instr) {
          s += "    ";
          s += get_instr_name(i);
          s += "\n";
        }

        s += "Breakpoints for rules :\n";
        for (auto &str : breakpoints_on_rule) {
          s += "    ";
          s += str;
          s += "\n";
        }
        print_feeding(s);
        break;
      }
      // break instruction
      case DebugCommand::BREAK_INSTR: {
        int instr_id = get_instr_id(opt_argv.at(0).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: breakpoint is not set.\n";
          break;
        }
        if (vector_contains(breakpoints_on_instr, (LmnInstruction)instr_id)) {
          std::cout << "Breakpoint is already set to instruction: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_instr.push_back((LmnInstruction) instr_id);
          std::cout << "Breakpoint is set to instruction: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // break rule
      case DebugCommand::BREAK_RULE: {
        if (vector_contains(breakpoints_on_rule, opt_argv.at(0))) {
          std::cout << "Breakpoint is already set to rule: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_rule.push_back(opt_argv.at(0));
          std::cout << "Breakpoint is set to rule: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // delete instruction
      case DebugCommand::DELETE_INSTR: {
        int instr_id = get_instr_id(opt_argv.at(0).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: no breakpoint is deleted\n";
        } else {
          auto end = breakpoints_on_instr.end();
          auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
          if (res == end) {
            std::cout << "Breakpoint is not set to instruction: " << opt_argv.at(0) << "\n";
          } else {
            breakpoints_on_instr.erase(res);
            std::cout << "Breakpoint is deleted for instruction: " << opt_argv.at(0) << "\n";
          }
        }
        break;
      }
      // delete rule
      case DebugCommand::DELETE_RULE: {
        auto end = breakpoints_on_rule.end();
        auto res = std::find(breakpoints_on_rule.begin(), end, opt_argv.at(0));
        if (res == end) {
          std::cout << "Breakpoint is not set to rule: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_rule.erase(res);
          std::cout << "Breakpoint is deleted for rule: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // run
      case DebugCommand::RUN: {
        continue_session = false;
        break;
      }
      // start
      case DebugCommand::START: {
        break_on_entry = true;
        continue_session = false;
        break;
      }
      // help
      case DebugCommand::HELP: {
        print_feeding(
          "info breakpoints -- list all breakpoints\n"
          "break instruction <NAME> -- set breakpoint on instruction named <NAME>\n"
          "break rule <NAME> -- set breakpoint on rule named <NAME>\n"
          "delete instruction <NAME> -- delete breakpoint on instruction named <NAME>\n"
          "delete rule <NAME> -- delete breakpoint on rule named <NAME>\n"
          "run -- start execution\n"
          "start -- start execution with temporary breakpoint on reaction of first rule\n"
          "help -- show this help\n"
        );
        break;
      }
      case DebugCommand::__UNKNOWN: {
        std::cout << "Unknown command.\n";
        break;
      }
      default: {
        std::cout << "The command was not executed because program is not running.\n";
        break;
      }
    }
  }
}

void InteractiveDebugger::start_session_with_interpreter(const slim::vm::interpreter *interpreter) {
  if (input_eof) {
    return;
  }

  fflush(stdout);
  std::flush(std::cout);

  const LmnReactCxtRef rc = interpreter == nullptr ? nullptr : interpreter->rc;
  const LmnRuleRef rule = interpreter == nullptr ? nullptr : interpreter->rule;
  const LmnRuleInstr instr = interpreter == nullptr ? nullptr : interpreter->instr;

  std::cout << "Rule        : ";
  std::cout << (rule == nullptr ? "null" : rule->name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(rule->name)) << "\n";
  std::cout << "Instruction : " << stringify_instr(instr) << "\n";

  bool continue_session = true;
  while (continue_session) {
    if (std::cin.eof()) {
      std::cout << "^D" << std::endl;
      input_eof = true;
      break;
    }

    std::string arg_string;
    std::cout << "(debugger) ";
    std::getline(std::cin, arg_string);

    const auto parse_result = parse_command(arg_string);
    const auto cmd = parse_result.first;
    const auto opt_argv = parse_result.second;
    const size_t opt_argc = opt_argv.size();

    if (cmd == DebugCommand::__EMPTY) {
      continue;
    }

    if (cmd == DebugCommand::__AMBIGUOUS) {
      std::cout << "Ambiguous command \"" << opt_argv.at(0) << "\": ";
      for (size_t i = 1; i < opt_argc; i++) {
        std::cout << opt_argv.at(i);
        if (i != opt_argc - 1) {
          std::cout << ", ";
        }
      }
      std::cout << "\n";
      continue;
    }

    switch(cmd) {
      // continue
      case DebugCommand::CONTINUE: {
        continue_session = false;
        break;
      }
      // info registers
      case DebugCommand::INFO_REGISTER: {
        if (rc == nullptr) {
          std::cout << "LmnReactCxtRef is null.\n";
          break;
        }
        // info registers
        if (opt_argc == 0) {
          print_feeding(stringify_regarray(&rc->work_array));
        }
        // info registers (dev) <N>...
        else {
          bool dev = opt_argc > 1 && opt_argv.at(0) == "dev";
          std::string s = "";
          for (size_t i = (dev ? 1 : 0); i < opt_argc; i++) {
            size_t l;
            try {
              l = std::stoul(opt_argv.at(i));
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument at " << i << ": Not a valid integer.\n";
              continue;
            }
            if (l >= rc->capacity()) {
              std::cout << "Invalid argument at " << i << ": Exceeds length of register.\n";
              continue;
            }
            s += "Register[";
            s += std::to_string(l);
            s += "] : ";
            if (dev) {
              s += stringify_register_dev(&rc->reg(l));
            } else if (rc->tt(l) == TT_MEM) {
              s += slim::stringifier::lmn_stringify_mem((LmnMembraneRef)rc->wt(l));
            } else {
              s += stringify_register(&rc->reg(l));
            }
            s += "\n";
          }
          print_feeding(s);
        }
        break;
      }
      // info atomlist
      case DebugCommand::INFO_ATOMLIST: {
        if (rc == nullptr) {
          std::cout << "LmnReactCxtRef is null.\n";
          break;
        }
        auto atom_lists = ((LmnMembraneRef)rc->wt(0))->atom_lists(); 
        // info atomlist
        if (opt_argc == 0) {
          std::string s = "";
          for (auto &p : atom_lists) {
            auto func_entry = lmn_functor_table->get_entry(p.first);
            s += lmn_id_to_name(func_entry->name);
            s += "_";
            s += std::to_string(func_entry->arity);
            s += "   : ";
            s += stringify_atomlist(p.second);
            s += "\n";
          }
          print_feeding(s);
        }
        // info atomlist <FUNCTOR>
        else if (opt_argc == 1) {
          size_t underscore = opt_argv.at(0).find("_");
          if (underscore != std::string::npos && underscore > 0 && underscore < opt_argv.at(0).size() - 1) {
            unsigned int input_arity;
            std::string input_name = opt_argv.at(0).substr(0, underscore);
            AtomListEntryRef atomlist = nullptr;
            try {
              input_arity = std::stoul(opt_argv.at(0).substr(underscore + 1));
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument: Not a valid integer for arity.\n";
              continue;
            }
            for (auto &p : atom_lists) {
              auto func_entry = lmn_functor_table->get_entry(p.first);
              std::string functor_name = lmn_id_to_name(func_entry->name);
              auto functor_arity = func_entry->arity;
              if (functor_name == input_name && functor_arity == input_arity) {
                atomlist = p.second;
                break;
              }
            }
            if (atomlist == nullptr) {
              std::cout << "Atomlist with specified functor could not be found.\n";
              break;
            } else {
              std::string s = "  ";
              s += opt_argv.at(0);
              s += "   : ";
              s += stringify_atomlist(atomlist);
              s += "\n";
              print_feeding(s);
            }
          } else {
            std::cout << "Malformed functor.\n";
            break;
          }
        }
        // info atomlist <FUNCTOR> <N> (dev)
        else {
          bool dev = opt_argc > 2 && opt_argv.at(2) == "dev";
          size_t input_n;
          try {
            input_n = std::stoul(opt_argv.at(1));
          } catch (const std::invalid_argument &e) {
            std::cout << "Invalid argument at 3: Not a valid integer.\n";
            break;
          }
          AtomListEntryRef atomlist = nullptr;
          size_t underscore = opt_argv.at(1).find("_");
          if (underscore != std::string::npos && underscore > 0 && underscore < opt_argv.at(1).size() - 1) {
            unsigned int input_arity;
            std::string input_name = opt_argv.at(1).substr(0, underscore);
            try {
              input_arity = std::stoul(opt_argv.at(1).substr(underscore + 1));
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument at 2: Not a valid integer for arity.\n";
              continue;
            }
            for (auto &p : atom_lists) {
              auto func_entry = lmn_functor_table->get_entry(p.first);
              std::string functor_name = lmn_id_to_name(func_entry->name);
              auto functor_arity = func_entry->arity;
              if (functor_name == input_name && functor_arity == input_arity) {
                atomlist = p.second;
                break;
              }
            }
            if (atomlist == nullptr) {
              std::cout << "Atomlist with specified functor could not be found.\n";
              break;
            }
            size_t i = 0;
            for (auto &s : *atomlist) {
              if (i == input_n) {
                std::string str = slim::stringifier::lmn_stringify_atom(s, LMN_ATTR_MAKE_LINK(input_arity));
                if (dev) {
                  str += "\n";
                  str += slim::stringifier::stringify_atom_dev(s);
                }
                print_feeding(str);
                break;
              }
              i++;
            }
            if (i != input_n) {
              std::cout << "Invalid argument: Exceeds length of atomlist.\n";
              break;
            }
          } else {
            std::cout << "Malformed functor.\n";
            break;
          }
        }
        break;
      }
      // info breakpoints
      case DebugCommand::INFO_BREAKPOINT: {
        std::string s = "Breakpoints for instructions :\n";
        for (auto &i : breakpoints_on_instr) {
          s += "    ";
          s += get_instr_name(i);
          s += "\n";
        }

        s += "Breakpoints for rules :\n";
        for (auto &str : breakpoints_on_rule) {
          s += "    ";
          s += str;
          s += "\n";
        }
        print_feeding(s);
        break;
      }
      // info membrane
      case DebugCommand::INFO_MEMBRANE: {
        if (rc == nullptr) {
          std::cout << "LmnReactCxtRef is null.\n";
          break;
        }
        if (rc->global_root == nullptr) {
          std::cout << "Global Root Membrane is null.\n";
          break;
        }
        // info membrane
        if (opt_argc == 0) {
          print_feeding(get_membrane_tree(rc->global_root, "", rc->global_root, (LmnMembraneRef) rc->wt(0)));
        }
        // info membrane <ARG>...
        else {
          // info membrane current
          if (string_starts_with("current", opt_argv.at(0))) {
            std::string s = "Current membrane (Addr[";
            s += to_hex_string((LmnMembraneRef)rc->wt(0));
            s += "]) :\n";
            s += slim::stringifier::lmn_stringify_mem((LmnMembraneRef)rc->wt(0));
            s += "\n";
            print_feeding(s);
          }
          // info membrane global
          else if (string_starts_with("global", opt_argv.at(0))) {
            std::string s = "Global root membrane (Addr[";
            s += to_hex_string((LmnMembraneRef)rc->global_root);
            s += "]) :\n";
            s += slim::stringifier::lmn_stringify_mem(rc->global_root);
            s += "\n";
            print_feeding(s);
          }
          // info membrane <N>
          else {
            unsigned long l;
            try {
              l = std::stoul(opt_argv.at(0));
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument at 2: Not a valid integer.\n";
              break;
            }
            LmnMembraneRef mem = get_pointer_to_membrane_by_id(rc->global_root, l);
            if (mem != nullptr) {
              print_feeding("Membrane (Addr[" + to_hex_string(mem) + "], ID[" + std::to_string(l) + "]) : \n"
                             + slim::stringifier::lmn_stringify_mem(mem) + "\n");
            } else {
              std::cout << "Invalid argument at 2: Membrane with specified ID could not be found in a tree rooted from global root membrane.\n";
              break;
            }
          }
        }
        break;
      }
      // info statespace
      case DebugCommand::INFO_STATESPACE: {
        if (!lmn_env.nd) {
          std::cout << "StateSpace could not be shown in normal execution.\n";
          break;
        }
        if (statespace == nullptr) {
          std::cout << "StateSpaceRef is null.\n";
          break;
        }
        // info statespace
        if (opt_argc == 0) {
          std::string s = "StateSpace (Addr[";
          s += to_hex_string(statespace);
          s += "])\n";
          auto states_vec = statespace->all_states();
          std::sort(states_vec.begin(), states_vec.end(),
            [](State *state_l, State *state_r) -> bool {
              return state_id(state_l) < state_id(state_r);
            }
          );
          for (State *state : states_vec) {
            // state info
            s += "   State[";
            s += std::to_string(state_id(state));
            s += "] : ";

            // membrane
            LmnMembraneRef mem = state->restore_membrane_inner(FALSE);
            s += slim::stringifier::lmn_stringify_mem(mem);
            if (state->is_binstr_user()) {
              mem->free_rec();
            }

            s += " -> [";
            for (unsigned int i = 0, max = state->successor_num; i < max; i++) {
              s += std::to_string(state_id(state_succ_state(state, i)));
              if (i != max - 1) {
                s += ",";
              }
            }
            s += "]";

            if (state == expanding_state) {
              s += " *expanding";
            }

            s += "\n";
          }
          print_feeding(s);
        }
        // info statespace <N>
        else {
          unsigned long l;
          try {
            l = std::stoul(opt_argv.at(0));
          } catch (const std::invalid_argument &e) {
            std::cout << "Invalid argument: Not a valid integer.\n";
            break;
          }

          auto states_vec = statespace->all_states();
          auto end = states_vec.end();
          auto res = std::find_if(states_vec.begin(), end,
            [l](State *state) -> bool { return state_id(state) == l; }
          );

          if (res == end) {
            std::cout << "State with given id could not be found in this statespace.\n";
            break;
          }

          State *state = *res;
          std::string s = "StateSpace (Addr[";
          s += to_hex_string(statespace);
          s += "])\n";

          s += " State (Addr[";
          s += to_hex_string(state);
          s += "], ID[";
          s += std::to_string(state_id(state));
          s += "]) -> ";

          for (unsigned int i = 0, max = state->successor_num; i < max; i++) {
            s += std::to_string(state_id(state_succ_state(state, i)));
            if (i != max - 1) {
              s += ",";
            }
          }

          s += "\n";

          LmnMembraneRef mem = state->restore_membrane_inner(FALSE);
          s += "  Membrane (Name[";
          lmn_interned_str str = mem->NAME_ID();
          s += (str == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(str));
          s += "], ID[";
          s += std::to_string(mem->mem_id());
          s += "])\n   ";
          s += slim::stringifier::lmn_stringify_mem(mem);
          if (state->is_binstr_user()) {
            mem->free_rec();
          }

          print_feeding(s);
        }
        break;
      }
      // FIXME 1ステップ余分に進んでいるかも
      // step rule
      case DebugCommand::STEP_RULE: {
        reset_step_execution();

        long l = 1;
        // step rule <N>
        if (opt_argc > 0) {
          try {
            l = std::stol(opt_argv.at(0));
          } catch (const std::invalid_argument& e) {
            l = -1;
          }
        }

        if (l >= 1) {
          rule_reaction_stop_at = l;
          continue_session = false;
        } else {
          std::cout << "Invalid argument: Not a valid integer.\n";
        }
        break;
      }
      // step instruction
      case DebugCommand::STEP_INSTR: {
        reset_step_execution();

        long l = 1;
        // step instruction <N>
        if (opt_argc > 0) {
          try {
            l = std::stol(opt_argv.at(0));
          } catch (const std::invalid_argument& e) {
            l = -1;
          }
        }

        if (l >= 1) {
          instr_execution_stop_at = l;
          continue_session = false;
        } else {
          std::cout << "Invalid argument: Not a valid integer.\n";
        }
        break;
      }
      // break rule <RULE>
      case DebugCommand::BREAK_RULE: {
        if (opt_argc == 0) {
          std::cout << "Missing target rule name.\n";
          break;
        }
        if (vector_contains(breakpoints_on_rule, opt_argv.at(0))) {
          std::cout << "Breakpoint is already set to rule: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_rule.push_back(opt_argv.at(0));
          std::cout << "Breakpoint is set to rule: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // break instruction <INSTR>
      case DebugCommand::BREAK_INSTR: {
        if (opt_argc == 0) {
          std::cout << "Missing target instruction name.\n";
          break;
        }
        int instr_id = get_instr_id(opt_argv.at(0).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: breakpoint is not set.\n";
          break;
        }
        if (vector_contains(breakpoints_on_instr, (LmnInstruction)instr_id)) {
          std::cout << "Breakpoint is already set to instruction: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_instr.push_back((LmnInstruction) instr_id);
          std::cout << "Breakpoint is set to instruction: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // delete rule <RULE>
      case DebugCommand::DELETE_RULE: {
        if (opt_argc == 0) {
          std::cout << "Missing target rule name.\n";
          break;
        }
        auto end = breakpoints_on_rule.end();
        auto res = std::find(breakpoints_on_rule.begin(), end, opt_argv.at(0));
        if (res == end) {
          std::cout << "Breakpoint is not set to rule: " << opt_argv.at(0) << "\n";
        } else {
          breakpoints_on_rule.erase(res);
          std::cout << "Breakpoint is deleted for rule: " << opt_argv.at(0) << "\n";
        }
        break;
      }
      // delete instruction <INSTR>
      case DebugCommand::DELETE_INSTR: {
        if (opt_argc == 0) {
          std::cout << "Missing target instruction name.\n";
          break;
        }
        int instr_id = get_instr_id(opt_argv.at(0).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: no breakpoint is deleted\n";
        } else {
          auto end = breakpoints_on_instr.end();
          auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
          if (res == end) {
            std::cout << "Breakpoint is not set to instruction: " << opt_argv.at(0) << "\n";
          } else {
            breakpoints_on_instr.erase(res);
            std::cout << "Breakpoint is deleted for instruction: " << opt_argv.at(0) << "\n";
          }
        }
        break;
      }
      case DebugCommand::FINISH: {
        if (*(LmnInstrOp*)instr == INSTR_PROCEED) {
          std::cout << "Already at the end of current rule.\n";
          break;
        }
        finish_current_rule = true;
        continue_session = false;
        break;
      }
      // help
      case DebugCommand::HELP: {
        print_feeding(
          "continue -- continue execution until next breakpoint\n"
          "step instruction -- execute one intermediate instruction\n"
          "step instruction <N> -- execute N intermediate instructions\n"
          "step rule -- apply one rule\n"
          "step rule <N> -- apply N rules\n"
          "info registers -- print register content of current react context\n"
          "info registers <N>... -- print Nth register content of current react context in detail\n"
          "info registers dev <N>... -- print Nth register content of current react context in more detail\n"
          "info atomlist -- print atomlist of current membrane\n"
          "info atomlist <FUNCTOR> -- print atomlist whose functor is <FUNCTOR> in current membrane\n"
          "info atomlist <FUNCTOR> <N> -- print Nth element of atomlist whose functor is <FUNCTOR> in current membrane\n"
          "info atomlist <FUNCTOR> <N> dev -- print Nth element of atomlist whose functor is <FUNCTOR> in current membrane in detail\n"
          "info membrane -- print all membranes' family tree\n"
          "info membrane current -- print currently reacting membrane\n"
          "info membrane global -- print global root membrane\n"
          "info membrane <N> -- print membrane whose ID is <N>\n"
          "info statespace -- print all states\n"
          "info statespace <N> -- print a state whose ID is <N>\n"
          "info breakpoints -- list all breakpoints\n"
          "finish -- continue execution till end of current rule\n"
          "break instruction <NAME> -- set breakpoint on instruction named <NAME>\n"
          "break rule <NAME> -- set breakpoint on rule named <NAME>\n"
          "delete instruction <NAME> -- delete breakpoint on instruction named <NAME>\n"
          "delete rule <NAME> -- delete breakpoint on rule named <NAME>\n"
          "help -- show this help\n"
        );
        break;
      }
      case DebugCommand::__UNKNOWN: {
        std::cout << "Unknown command.\n";
        break;
      }
      case DebugCommand::START:
      case DebugCommand::RUN: {
        std::cout << "Program is already running.\n";
        break;
      }
      default: {
        std::cout << "Not implemented.\n";
        break;
      }
    }

    std::flush(std::cout);
  }
}

void InteractiveDebugger::break_on_instruction(const slim::vm::interpreter *interpreter) {
  if (input_eof || reacting_system_ruleset) {
    return;
  }
  instr_execution_count++;
  LmnInstrOp instr_op = *(LmnInstrOp*)interpreter->instr;
  // step execution
  if (instr_execution_stop_at != 0 && instr_execution_count == instr_execution_stop_at) {
    esc_code_add(CODE__FORECOLOR_YELLOW);
    printf("\nSteped %ld instructions.\n", instr_execution_stop_at);
    esc_code_clear();
    reset_step_execution();
    start_session_with_interpreter(interpreter);
  }
  // finishing rule
  else if (finish_current_rule && instr_op == INSTR_PROCEED) {
    esc_code_add(CODE__FORECOLOR_YELLOW);
    printf("\nFinishing current rule.\n");
    esc_code_clear();
    finish_current_rule = false;
    start_session_with_interpreter(interpreter);
  }
  // hit breakpoint
  else {
    if (vector_contains(breakpoints_on_instr, (LmnInstruction)instr_op)) {
      esc_code_add(CODE__FORECOLOR_YELLOW);
      printf("\nBreakpoint on instruction \"%s\" hit.\n", get_instr_name(instr_op));
      esc_code_clear();
      reset_step_execution();
      start_session_with_interpreter(interpreter);
    }
  }

  previous_instr = interpreter->instr;
}

void InteractiveDebugger::break_on_rule(const slim::vm::interpreter *interpreter) {
  if (input_eof || reacting_system_ruleset) {
    return;
  }
  // break on entry
  if (break_on_entry) {
    break_on_entry = false;
    start_session_with_interpreter(interpreter);
  }
  rule_reaction_count++;
  // step execution
  if (rule_reaction_stop_at != 0 && rule_reaction_count == rule_reaction_stop_at) {
    esc_code_add(CODE__FORECOLOR_YELLOW);
    printf("\nSteped %ld rules.\n", rule_reaction_stop_at);
    esc_code_clear();
    reset_step_execution();
    start_session_with_interpreter(interpreter);
  }
  // hit breakpoint
  else {
    if (vector_contains(breakpoints_on_rule, std::string(lmn_id_to_name(interpreter->rule->name)))) {
      esc_code_add(CODE__FORECOLOR_YELLOW);
      printf("\nBreakpoint on rule \"%s\" hit.\n", lmn_id_to_name(interpreter->rule->name));
      esc_code_clear();
      reset_step_execution();
      start_session_with_interpreter(interpreter);
    }
  }

  previous_rule = interpreter->rule;
}

void InteractiveDebugger::finish_debugging() {
  std::cout << "\n";
  std::flush(std::cout);
}

static bool print_section(const std::vector<std::string> &lines, size_t from_line, size_t screen_height, size_t screen_width) {
  size_t line_count = lines.size();
  if (line_count < screen_height) {
    for (auto s : lines) {
      std::cout << s << "\n";
    }
    return false;
  }

  if (from_line > line_count - (screen_height - 1)) {
    from_line = line_count - (screen_height - 1);
  }

  std::cout << "\e[2J\e[0;0H"; // delete screen and move cursor to top left

  for (size_t i = from_line; i < from_line + (screen_height - 1); i++) {
    std::cout << lines.at(i) << "\n";
  }

  std::cout << "(" << from_line << "-" << (from_line + (screen_height - 1)) << "/" << line_count << ") :";

  std::flush(std::cout);
  return true;
}

void InteractiveDebugger::print_feeding(const std::string &str) {
  if (screen_height == -1) {
    std::cout << str;
    return;
  }

  std::vector<std::string> lines;

  for (size_t start = 0, i = 0, max = str.size(); i < max; i++) {
    if (str[i] == '\n') {
      lines.push_back(str.substr(start, i - start));
      start = i + 1;
    } else if (i - start + 1 == screen_width) {
      lines.push_back(str.substr(start, i - start + 1));
      start = i + 1;
    } else if (i == max - 1) {
      lines.push_back(str.substr(start));
      break;
    }
  }

  while(true) {
    size_t len = lines.size();
    if (len > 0 && lines.at(len - 1).empty()) {
      lines.pop_back();
    } else {
      break;
    }
  }

  size_t current_line = 0, max_line = lines.size() - (screen_height - 1);
  bool feeding = print_section(lines, current_line, screen_height, screen_width);

  if (!feeding) {
    return;
  }

  struct termios old_termios, new_termios;
  tcgetattr(STDIN_FILENO, &old_termios);
  new_termios = old_termios;
  new_termios.c_lflag &= ~(ICANON | ECHO);
  tcsetattr(STDIN_FILENO, TCSANOW, &new_termios);

  bool esc_pressed = false;
  while (feeding) {
    size_t new_line = current_line;
    int c = getchar();
    switch (c) {
      case EOF: // eof may not be returned
        input_eof = true;
        feeding = false;
        break;
      case 'q':
        feeding = false;
        std::cout << "\e[2K\e[0G";
        break;
      case '\n':
      case 'j':
        new_line += 1;
        break;
      case 'd':
        new_line += screen_height / 2;
        break;
      case 'u':
        if (new_line >= screen_height / 2) {
          new_line -= screen_height / 2;
        } else {
          new_line = 0;
        }
        break;
      case 'k':
        if (new_line >= 1) {
          new_line -= 1;
        }
        break;
      case ' ':
        new_line += (screen_height - 1);
        break;
      case '\e':
        esc_pressed = true;
        break;
      case 'A': // up arrow
        if (esc_pressed) {
          if (new_line >= 1) {
            new_line -= 1;
          }
          esc_pressed = false;
        }
        break;
      case 'B': // down arrow
        if (esc_pressed) {
          new_line += 1;
          esc_pressed = false;
        }
        break;
      case 'H': // home
        if (esc_pressed) {
          new_line = 0;
          esc_pressed = false;
        }
        break;
      case 'F': // end
        if (esc_pressed) {
          new_line = max_line;
          esc_pressed = false;
        }
        break;
      default:
        break;
    }

    if (new_line > max_line) {
      new_line = max_line;
    }
    if (new_line != current_line) {
      current_line = new_line;
      feeding = print_section(lines, current_line, screen_height, screen_width);
    }
  }

  tcsetattr(STDIN_FILENO, TCSANOW, &old_termios);
}
