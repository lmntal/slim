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
  UNKNOWN
};

static const std::map<DebugCommand, std::vector<std::string>> debug_commands = {
  {DebugCommand::CONTINUE, {"continue"}},
  {DebugCommand::HELP, {"help"}},
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

const char* get_instr_name(int id) {
  for (auto &p : instr_spec)
    if (id == p.first)
      return p.second.op_str;
  return "unknown";
}

template <class T>
static inline std::string to_hex_string(T value) {
  constexpr int width = sizeof(T) * 2;
  std::ostringstream oss;
  oss << "0x";
  oss << std::noshowbase;
  oss << std::hex << std::setw(width) << std::setfill('0') << value;
  return oss.str();
}

template <>
inline std::string to_hex_string<unsigned char>(unsigned char value) {
  if (value == 0) {
    return "0x00";
  }
  std::ostringstream oss;
  oss << "0x";
  oss << std::noshowbase;
  oss << std::hex << std::setw(2) << std::setfill('0') << (unsigned int) value;
  return oss.str();
}

template <class T>
static inline std::string to_hex_string(T* value) {
  return to_hex_string((uintptr_t)value);
}

static inline bool string_starts_with(const std::string &str1, const std::string &str2) {
  return str1.size() < str2.size() ? false : std::equal(str2.begin(), str2.end(), str1.begin());
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

  std::ostringstream retVal;
  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  switch (tt) {
    case TT_ATOM:
      retVal << slim::stringifier::lmn_stringify_atom((LmnAtomRef)wt, at);
      break;
    case TT_MEM:
      retVal << (LmnMembraneRef) wt << "(" << (unsigned int) at << ",mem)";
      break;
    case TT_OTHER:
      retVal << wt << "(" << (unsigned int) at << ",other)";
      break;
    default:
      retVal << "unknown tt type";
      break;
  }

  return retVal.str();
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
      retVal << "Addr[" << (LmnMembraneRef) wt << "], ";
      retVal << "ID[" << (unsigned long) ((LmnMembraneRef) wt)->mem_id() << "]\n";
      retVal << slim::stringifier::lmn_stringify_mem_dev((LmnMembraneRef) wt);
      break;
    }
    case TT_OTHER:
      retVal << "wt[0x" << std::hex << wt << std::dec << "], ";
      retVal << "at[" << (unsigned int) at << "], ";
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

static void split_command(const std::string &command, std::vector<std::string> &argv) {
  if (command.empty()) {
    return;
  }

  for (size_t start_index = 0, max = command.size(); start_index < max; ) {
    size_t space_index = command.find(' ', start_index);
    std::string temp = command.substr(start_index, space_index - start_index);

    if (!temp.empty()) {
      argv.push_back(temp);
    }
    if (space_index == std::string::npos) {
      break;
    }

    start_index = space_index + 1;
  }

  return;
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

static bool is_pointer_valid_for_membrane(const LmnMembraneRef mem, const void *p) {
  if (mem == nullptr) {
    return false;
  } else if (mem == p) {
    return true;
  } else {
    return is_pointer_valid_for_membrane(mem->child_head, p) || is_pointer_valid_for_membrane(mem->next, p);
  }
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

void InteractiveDebugger::start_session(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  if (input_eof) {
    return;
  }

  fflush(stdout);
  std::flush(std::cout);

  std::cout << "\nRule        : ";
  std::cout << (rule == nullptr ? "null" : rule->name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(rule->name)) << "\n";

  std::cout << "Instruction : " << stringify_instr(instr) << "\n";

  if (instr != nullptr && *((LmnInstrOp *)instr) != INSTR_SPEC && previous_instr >= instr) {
    std::cout << "Possible backtracking. (From: " << stringify_instr(previous_instr) << ")\n";
  }

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

    std::vector<std::string> argv;
    split_command(arg_string, argv);

    const size_t argc = argv.size();

    if (argc == 0) {
      continue;
    }

    // reduce ambiguity
    bool has_ambiguous_arg = false;
    for (size_t i = 0; i < argc && !has_ambiguous_arg; i++) {
      std::vector<std::string> argument_candidate;
      for (auto &p : debug_commands) {
        if (i < p.second.size()) {
          if (std::equal(p.second.begin(), p.second.begin() + i, argv.begin())) {
            if (string_starts_with(p.second.at(i), argv.at(i))) {
              auto end = argument_candidate.end();
              auto res = std::find(argument_candidate.begin(), end, p.second.at(i));
              if (res == end) {
                argument_candidate.push_back(p.second.at(i));
              }
            }
          }
        }
      }
      if (argument_candidate.size() == 1) {
        argv.at(i) = argument_candidate.at(0);
      } else if (argument_candidate.size() > 1) {
        has_ambiguous_arg = true;
        std::cout << "Ambiguous command \"" << argv.at(i) << "\": ";
        for (auto &s : argument_candidate) {
          std::cout << s << ", ";
        }
        std::cout << "\b\b.\n";
      }
    }

    if (has_ambiguous_arg) {
      continue;
    }

    // identify command
    auto cmd_iter_end = debug_commands.end();
    auto cmd_iter_found = std::find_if(
      debug_commands.begin(), cmd_iter_end,
      [&argv, argc](std::pair<DebugCommand, std::vector<std::string>> p) -> bool {
          return std::equal(
            p.second.begin(), p.second.end(),
            argv.begin()
          );
      }
    );

    DebugCommand cmd = cmd_iter_found == cmd_iter_end ? DebugCommand::UNKNOWN : (*cmd_iter_found).first;

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
        if (argc == 2) {
          print_feeding(stringify_regarray(&rc->work_array));
        }
        // info registers (dev) <N>...
        else {
          bool dev = argc > 3 && argv.at(2) == "dev";
          std::string s = "";
          for (size_t i = (dev ? 3 : 2); i < argc; i++) {
            size_t l;
            try {
              l = std::stoul(argv.at(i));
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
        // info atomlist
        if (argc == 2) {
          std::string s = "";
          for (size_t i = 0, max = ((LmnMembraneRef) rc->wt(0))->max_functor; i < max; i++) {
            auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
            if (list != nullptr) {
              s += lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
              s += "_";
              s += std::to_string(lmn_functor_table->get_entry(i)->arity);
              s += " \t: ";
              s += stringify_atomlist(list);
              s += "\n";
            }
          }
          print_feeding(s);
        }
        // info atomlist <FUNCTOR>...
        else {
          std::string s = "";
          for (size_t i = 0, max = ((LmnMembraneRef) rc->wt(0))->max_functor; i < max; i++) {
            auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
            if (list != nullptr) {
              std::string functor_name = lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
              unsigned int functor_arity = lmn_functor_table->get_entry(i)->arity;

              for (size_t j = 2; j < argc; j++) {
                size_t underscore = argv.at(j).find("_");
                if (underscore != std::string::npos && underscore > 0 && underscore < argv.at(j).size() - 1) {
                  unsigned int input_arity;
                  std::string input_name = argv.at(j).substr(0, underscore);
                  try {
                    input_arity = std::stoul(argv.at(j).substr(underscore + 1));
                  } catch (const std::invalid_argument& e) {
                    std::cout << "Invalid argument at " << j << ": Not a valid integer for arity.\n";
                    continue;
                  }
                  if (functor_name == input_name && functor_arity == input_arity) {
                    s += "  ";
                    s += functor_name;
                    s += "_";
                    s += functor_arity;
                    s += " \t: ";
                    s += stringify_atomlist(list);
                    s += "\n";
                  }
                }
              }
              print_feeding(s);
            }
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
        if (argc == 2) {
          std::string s = get_membrane_tree(rc->global_root, "", rc->global_root, (LmnMembraneRef) rc->wt(0));
          print_feeding(s);
        }
        // info membrane <ARG>...
        else {
          // info membrane current
          if (string_starts_with("current", argv.at(2))) {
            std::string s = "Current membrane (Addr[";
            s += to_hex_string((LmnMembraneRef)rc->wt(0));
            s += "]) :\n";
            s += slim::stringifier::lmn_stringify_mem((LmnMembraneRef)rc->wt(0));
            s += "\n";
            print_feeding(s);
          }
          // info membrane global
          else if (string_starts_with("global", argv.at(2))) {
            std::string s = "Global root membrane (Addr[";
            s += to_hex_string((LmnMembraneRef)rc->global_root);
            s += "]) :\n";
            s += slim::stringifier::lmn_stringify_mem(rc->global_root);
            s += "\n";
            print_feeding(s);
          }
          // info membrane <HEX>
          else if (string_starts_with(argv.at(2), "0x")) {
            unsigned long l;
            try {
              l = std::stoul(argv.at(2), 0, 16);
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument at 2: Not a valid hexadecimal integer.\n";
              break;
            }
            if (is_pointer_valid_for_membrane(rc->global_root, (void *)l)) {
              std::string s = "Membrane (Addr[";
              s += to_hex_string((LmnMembraneRef)l);
              s += "]) :\n";
              s += slim::stringifier::lmn_stringify_mem((LmnMembraneRef)l);
              s += "\n";
              print_feeding(s);
            } else {
              std::cout << "Invalid argument at 2: Not a valid pointer to LmnMembrane.\n";
              break;
            }
          }
          // info membrane <N>
          else {
            unsigned long l;
            try {
              l = std::stoul(argv.at(2));
            } catch (const std::invalid_argument& e) {
              std::cout << "Invalid argument at 2: Not a valid integer.\n";
              break;
            }
            LmnMembraneRef mem = get_pointer_to_membrane_by_id(rc->global_root, l);
            if (mem != nullptr) {
              std::string s = "Membrane (Addr[";
              s += to_hex_string(mem);
              s += "], ID[";
              s += std::to_string(l);
              s += "]) : \n";
              s += slim::stringifier::lmn_stringify_mem(mem);
              s += "\n";
              print_feeding(s);
            } else {
              std::cout << "Invalid argument at 2: Membrane with specified ID could not be found.\n";
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
        if (argc == 2) {
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
            s += "State[";
            s += std::to_string(state_id(state));
            s += "] : ";

            // membrane
            LmnMembraneRef mem = state->restore_membrane_inner(FALSE);
            s += slim::stringifier::lmn_stringify_mem(mem);
            if (state->is_binstr_user()) {
              mem->free_rec();
            }

            s += " -> ";
            for (unsigned int i = 0, max = state->successor_num; i < max; i++) {
              s += std::to_string(state_id(state_succ_state(state, i)));
              if (i != max - 1) {
                s += ",";
              }
            }

            s += "\n";
          }
          print_feeding(s);
        }
        // info statespace <ARG>...
        else {
          // info statespace <N>
          if (true) {
            unsigned long l;
            try {
              l = std::stoul(argv.at(2));
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
              std::cout << "State with given id could not be found.\n";
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
          } else {
            std::cout << "Unknown subcommand.\n";
            break;
          }
        }
        break;
      }
      // step rule
      case DebugCommand::STEP_RULE: {
        instr_execution_count = 0;
        instr_execution_stop_at = -1;
        rule_reaction_count = 0;

        long l = 1;
        // step rule <N>
        if (argc >= 3) {
          try {
            l = std::stol(argv.at(2));
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
        rule_reaction_count = 0;
        rule_reaction_stop_at = -1;
        instr_execution_count = 0;

        long l = 1;
        // step instruction <N>
        if (argc >= 3) {
          try {
            l = std::stol(argv.at(2));
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
        if (argc < 3) {
          std::cout << "Too few arguments.\n";
          break;
        }
        auto end = breakpoints_on_rule.end();
        auto res = std::find(breakpoints_on_rule.begin(), end, argv.at(2));
        if (res != end) {
          std::cout << "Breakpoint is already set to rule: " << argv.at(2) << "\n";
        } else {
          breakpoints_on_rule.push_back(argv.at(2));
          std::cout << "Breakpoint is set to rule: " << argv.at(2) << "\n";
        }
        break;
      }
      // break instruction <INSTR>
      case DebugCommand::BREAK_INSTR: {
        if (argc < 3) {
          std::cout << "Too few arguments.\n";
          break;
        }
        int instr_id = get_instr_id(argv.at(2).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: breakpoint is not set.\n";
          break;
        }
        auto end = breakpoints_on_instr.end();
        auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
        if (res != end) {
          std::cout << "Breakpoint is already set to instruction: " << argv.at(2) << "\n";
        } else {
          breakpoints_on_instr.push_back((LmnInstruction) instr_id);
          std::cout << "Breakpoint is set to instruction: " << argv.at(2) << "\n";
        }
        break;
      }
      // delete rule <RULE>
      case DebugCommand::DELETE_RULE: {
        if (argc < 3) {
          std::cout << "Too few arguments.\n";
          break;
        }
        auto end = breakpoints_on_rule.end();
        auto res = std::find(breakpoints_on_rule.begin(), end, argv.at(2));
        if (res == end) {
          std::cout << "Breakpoint is not set to rule: " << argv.at(2) << "\n";
        } else {
          breakpoints_on_rule.erase(res);
          std::cout << "Breakpoint is deleted for rule: " << argv.at(2) << "\n";
        }
        break;
      }
      // delete instruction <INSTR>
      case DebugCommand::DELETE_INSTR: {
        if (argc < 3) {
          std::cout << "Too few arguments.\n";
          break;
        }
        int instr_id = get_instr_id(argv.at(2).c_str());
        if (instr_id == -1) {
          std::cout << "Unknown Instruction: no breakpoint is deleted\n";
        } else {
          auto end = breakpoints_on_instr.end();
          auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
          if (res == end) {
            std::cout << "Breakpoint is not set to instruction: " << argv.at(2) << "\n";
          } else {
            breakpoints_on_instr.erase(res);
            std::cout << "Breakpoint is deleted for instruction: " << argv.at(2) << "\n";
          }
        }
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
          "info atomlist -- print atomlist of current membrane\n"
          "info atomlist <FUNCTOR>... -- print atomlist with functor FUNCTOR of current membrane\n"
          "info membrane -- print all membranes' family tree\n"
          "info membrane current -- print currently reacting membrane\n"
          "info membrane global -- print global root membrane\n"
          "info membrane <HEX> -- print membrane whose address is <HEX>\n"
          "info membrane <N> -- print membrane whose ID is <N>\n"
          "info statespace -- print all states\n"
          "info statespace <N> -- print a state whose ID is <N>\n"
          "info breakpoints -- list all breakpoints\n"
          "break instruction NAME -- set breakpoint on instruction named NAME\n"
          "break rule NAME -- set breakpoint on rule named NAME\n"
          "delete instruction NAME -- delete breakpoint on instruction named NAME\n"
          "delete rule NAME -- delete breakpoint on rule named NAME\n"
          "help -- show this help\n"
        );
        break;
      }
      case DebugCommand::UNKNOWN: {
        std::cout << "Unknown command.\n";
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

void InteractiveDebugger::break_on_instruction(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  instr_execution_count++;
  if (instr_execution_count == instr_execution_stop_at) {
    instr_execution_count = 0;
    instr_execution_stop_at = -1;
    start_session(rc, rule, instr);
  } else {
    auto end = breakpoints_on_instr.end();
    auto res = std::find(breakpoints_on_instr.begin(), end, *(LmnInstrOp *)instr);
    if (res != end) {
      start_session(rc, rule, instr);
    }
  }

  previous_instr = instr;
}

void InteractiveDebugger::break_on_rule(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  rule_reaction_count++;
  if (rule_reaction_count == rule_reaction_stop_at) {
    rule_reaction_count = 0;
    rule_reaction_stop_at = -1;
    start_session(rc, rule, instr);
  } else {
    auto end = breakpoints_on_rule.end();
    auto res = std::find(breakpoints_on_rule.begin(), end, lmn_id_to_name(rule->name));
    if (res != end) {
      start_session(rc, rule, instr);
    }
  }

  previous_rule = rule;
}

void InteractiveDebugger::finish_debugging(void) {
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

  struct termios old_termios, new_termios;
  tcgetattr(STDIN_FILENO, &old_termios);
  new_termios = old_termios;
  new_termios.c_lflag &= ~(ICANON | ECHO);
  tcsetattr(STDIN_FILENO, TCSANOW, &new_termios);

  size_t current_line = 0, max_line = lines.size() - (screen_height - 1);
  bool feeding = print_section(lines, current_line, screen_height, screen_width);
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

  return;
}

void InteractiveDebugger::register_automata(AutomataRef ref) {
  automata = ref;
}

void InteractiveDebugger::register_statespace(StateSpaceRef ref) {
  statespace = ref;
}