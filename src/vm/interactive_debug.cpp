#include "interactive_debug.hpp"
#include "task.h"
#include "stringifier.hpp"
#include <iostream>
#include <string>
#include <sstream>
#include <stdexcept>
#include <sys/ioctl.h>
#include <unistd.h>
#include <termios.h>

enum class DebugCommand {
  DBGCMD_CONTINUE,
  DBGCMD_STEP,
  DBGCMD_INFO,
  DBGCMD_PRINT,
  DBGCMD_BREAK,
  DBGCMD_DELETE,
  DBGCMD_HELP,
  DBGCMD_UNKNOWN
};

static const std::map<std::string, DebugCommand> debug_commands = {
  {"continue", DebugCommand::DBGCMD_CONTINUE},
  {"c", DebugCommand::DBGCMD_CONTINUE},
  {"step", DebugCommand::DBGCMD_STEP},
  {"s", DebugCommand::DBGCMD_STEP},
  {"info", DebugCommand::DBGCMD_INFO},
  {"i", DebugCommand::DBGCMD_INFO},
  {"print", DebugCommand::DBGCMD_PRINT},
  {"p", DebugCommand::DBGCMD_PRINT},
  {"break", DebugCommand::DBGCMD_BREAK},
  {"b", DebugCommand::DBGCMD_BREAK},
  {"delete", DebugCommand::DBGCMD_DELETE},
  {"d", DebugCommand::DBGCMD_DELETE},
  {"help", DebugCommand::DBGCMD_HELP},
  {"h", DebugCommand::DBGCMD_HELP}
};

enum class DebugCommandArg {
  DBGARG_INSTR,
  DBGARG_RULE_REG,
  DBGARG_ATOMLIST,
  DBGARG_BRKPNT,
  DBGARG_MEMBRANE,
  DBGARG_UNKNOWN
};

static const std::map<std::string, DebugCommandArg> debug_command_args = {
  {"instruction", DebugCommandArg::DBGARG_INSTR},
  {"i", DebugCommandArg::DBGARG_INSTR},
  {"rule", DebugCommandArg::DBGARG_RULE_REG},
  {"registers", DebugCommandArg::DBGARG_RULE_REG},
  {"r", DebugCommandArg::DBGARG_RULE_REG},
  {"atomlist", DebugCommandArg::DBGARG_ATOMLIST},
  {"a", DebugCommandArg::DBGARG_ATOMLIST},
  {"breakpoints", DebugCommandArg::DBGARG_BRKPNT},
  {"b", DebugCommandArg::DBGARG_BRKPNT},
  {"membrane", DebugCommandArg::DBGARG_MEMBRANE},
  {"m", DebugCommandArg::DBGARG_MEMBRANE}
};

const char* get_instr_name(int id) {
  for (auto &p : instr_spec)
    if (id == p.first)
      return p.second.op_str;
  return "unknown";
}

InteractiveDebugger::InteractiveDebugger() {
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  screen_height = w.ws_row;
  screen_width = w.ws_col;
}

InteractiveDebugger::~InteractiveDebugger() {}

static std::string stringify_atom(const LmnAtomRef atom, const LmnByte at) {
  if (atom == nullptr) {
    return "null";
  }
  return slim::stringifier::lmn_stringify_atom(atom, at);
}

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
      retVal << stringify_atom((LmnAtomRef)wt, at);
      break;
    case TT_MEM:
      retVal << std::hex << (LmnMembraneRef) wt << std::dec << "(" << (unsigned int) at << ",mem)";
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
        retVal << stringify_atom((LmnAtomRef) wt, at);
      } else {
        retVal << slim::stringifier::stringify_atom_dev((LmnSymbolAtomRef) wt);
      }
      break;
    case TT_MEM:
      retVal << slim::stringifier::lmn_stringify_mem_dev((LmnMembraneRef) wt);
      break;
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
        }

        if (i != argSize - 1) {
          retVal << ", ";
        }
      }
      break;
    }
  }

  if (op == INSTR_FINDATOM || op == INSTR_FINDATOM2 || op == INSTR_FINDATOMP) {
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
    retVal << stringify_atom(*begin, LMN_ATTR_MAKE_LINK(0));
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

void InteractiveDebugger::start_session(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  if (input_eof) {
    return;
  }

  fflush(stdout);
  std::flush(std::cout);

  std::cout << "\nRule        : ";
  std::cout << (rule == nullptr ? "null" : rule->name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(rule->name)) << '\n';

  std::cout << "Instruction : " << stringify_instr(instr) << '\n';

  if (instr != nullptr && *((LmnInstrOp *)instr) != INSTR_SPEC && previous_instr >= instr) {
    std::cout << "Possible backtracking. (Prev Inst : " << stringify_instr(previous_instr) << ")\n";
  }

  bool continue_session = true;
  std::string arg_string;
  std::vector<std::string> argv;
  while (continue_session) {
    if (std::cin.eof()) {
      std::cout << "^D" << std::endl;
      input_eof = true;
      break;
    }

    std::cout << "(debugger) ";
    std::getline(std::cin, arg_string);

    argv.clear();
    split_command(arg_string, argv);

    const size_t argc = argv.size();

    if (argc == 0) {
      continue;
    }

    DebugCommand cmd = DebugCommand::DBGCMD_UNKNOWN;

    for (auto &p : debug_commands) {
      if (p.first == argv.at(0)) {
        cmd = p.second;
        break;
      }
    }

    DebugCommandArg arg1 = DebugCommandArg::DBGARG_UNKNOWN;

    if (argc >= 2) {
      for (auto &p : debug_command_args) {
        if (p.first == argv.at(1)) {
          arg1 = p.second;
          break;
        }
      }
    }

    switch(cmd) {
      case DebugCommand::DBGCMD_CONTINUE:
        continue_session = false;
        break;
      case DebugCommand::DBGCMD_INFO:
        if (argc < 2) {
          std::cout << few_arg_message << '\n';
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          if (rc == nullptr) {
            std::cout << "LmnReactCxtRef is null\n";
            break;
          }
          if (argc == 2) {
            print_feeding(stringify_regarray(&rc->work_array));
          } else {
            for (size_t i = 2; i < argc; i++) {
              size_t l;
              try {
                l = std::stoul(argv.at(i));
              } catch (const std::invalid_argument& e) {
                l = rc->capacity();
              }
              if (l >= rc->capacity()) {
                std::cout << invalid_arg_message << " at " << i << " (" << argv.at(i) << ")\n";
                continue;
              }
              print_feeding("Register[" + argv.at(i) + "] : \n" + stringify_register_dev(&rc->reg(l)));
            }
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_ATOMLIST) {
          if (rc == nullptr) {
            std::cout << "LmnReactCxtRef is null\n";
            break;
          }

          size_t list_count = ((LmnMembraneRef) rc->wt(0))->max_functor;

          if (argc == 2) {
            std::string s = "";
            for (size_t i = 0; i < list_count; i++) {
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
          } else {
            std::string s = "";
            for (size_t i = 0; i < list_count; i++) {
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
                      std::cout << invalid_arg_message << " at " << j << " (" << argv.at(j) << ")\n";
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
        }
        else if (arg1 == DebugCommandArg::DBGARG_BRKPNT) {
          std::string s = "Breakpoints for instructions :\n";
          for (auto &i : breakpoints_on_instr) {
            s += "  ";
            s += get_instr_name(i);
            s += "\n";
          }

          s += "\nBreakpoints for rules :\n";
          for (auto &str : breakpoints_on_rule) {
            s += "  ";
            s += str;
            s += "\n";
          }
          print_feeding(s);
        }
        else if (arg1 == DebugCommandArg::DBGARG_MEMBRANE) {
          std::string s = "Current membrane :\n";
          s += slim::stringifier::lmn_stringify_mem((LmnMembraneRef)rc->wt(0));
          s += "\n";
          print_feeding(s);
        }
        else {
          std::cout << invalid_args_message << '\n';
        }

        break;
      case DebugCommand::DBGCMD_STEP:
        if (argc < 2) {
          std::cout << few_arg_message << '\n';
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          instr_execution_count = 0;
          instr_execution_stop_at = -1;
          rule_reaction_count = 0;

          long l = 1;
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
            std::cout << invalid_arg_message << ": " << argv.at(2) << '\n';
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          rule_reaction_count = 0;
          rule_reaction_stop_at = -1;
          instr_execution_count = 0;

          long l = 1;
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
            std::cout << invalid_arg_message << ": " << argv.at(2) << '\n';
          }
        }
        else {
          std::cout << invalid_args_message << '\n';
        }

        break;
      case DebugCommand::DBGCMD_BREAK:
        if (argc < 3) {
          std::cout << few_arg_message << '\n';
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          auto end = breakpoints_on_rule.end();
          auto res = std::find(breakpoints_on_rule.begin(), end, argv.at(2));
          if (res != end) {
            std::cout << "Breakpoint is already set to instruction: " << argv.at(2) << '\n';
          } else {
            breakpoints_on_rule.push_back(argv.at(2));
            std::cout << "Breakpoint is set to rule: " << argv.at(2) << '\n';
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(argv.at(2).c_str());

          if (instr_id == -1) {
            std::cout << "Unknown Instruction: breakpoint is not set\n";
            break;
          }

          auto end = breakpoints_on_instr.end();
          auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
          if (res != end) {
            std::cout << "Breakpoint is already set to instruction: " << argv.at(2) << '\n';
          } else {
            breakpoints_on_instr.push_back((LmnInstruction) instr_id);
            std::cout << "Breakpoint is set to instruction: " << argv.at(2) << '\n';
          }
        }
        else {
          std::cout << invalid_args_message << '\n';
        }

        break;
      case DebugCommand::DBGCMD_DELETE:
        if (argc < 3) {
          std::cout << few_arg_message << '\n';
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          auto end = breakpoints_on_rule.end();
          auto res = std::find(breakpoints_on_rule.begin(), end, argv.at(2));
          if (res == end) {
            std::cout << "Breakpoint is not set to rule: " << argv.at(2) << '\n';
          } else {
            breakpoints_on_rule.erase(res);
            std::cout << "Breakpoint is deleted for rule: " << argv.at(2) << '\n';
          }
          break;
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(argv.at(2).c_str());
          if (instr_id == -1) {
            std::cout << "Unknown Instruction: no breakpoint is deleted" << '\n';
          } else {
            auto end = breakpoints_on_instr.end();
            auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
            if (res == end) {
              std::cout << "Breakpoint is not set to instruction: " << argv.at(2) << '\n';
            } else {
              breakpoints_on_instr.erase(res);
              std::cout << "Breakpoint is deleted for instruction: " << argv.at(2) << '\n';
            }
          }
        }
        else {
          std::cout << invalid_args_message << std::endl;
        }

        break;
      case DebugCommand::DBGCMD_HELP:
        std::cout << "(c)ontinue -- continue execution until next breakpoint\n";
        std::cout << "(s)tep (i)nstruction -- execute one intermediate instruction\n";
        std::cout << "(s)tep (i)nstruction <N> -- execute N intermediate instructions\n";
        std::cout << "(s)tep (r)ule -- apply one rule\n";
        std::cout << "(s)tep (r)ule <N> -- apply N rules\n";
        std::cout << "(i)nfo (r)egisters -- print register content of current react context\n";
        std::cout << "(i)nfo (r)egisters <N> -- print Nth register content of current react context in detail\n";
        std::cout << "(i)nfo (a)tomlist -- print atomlist of current membrane\n";
        std::cout << "(i)nfo (a)tomlist FUNCTOR -- print atomlist with functor FUNCTOR of current membrane\n";
        std::cout << "(i)nfo (m)embrane -- print currently reacting membrane\n";
        std::cout << "(i)nfo (b)reakpoints -- list all breakpoints\n";
        std::cout << "(b)reak (i)nstruction NAME -- set breakpoint on instruction named NAME\n";
        std::cout << "(b)reak (r)ule NAME -- set breakpoint on rule named NAME\n";
        std::cout << "(d)elete (i)nstruction NAME -- delete breakpoint on instruction named NAME\n";
        std::cout << "(d)elete (r)ule NAME -- delete breakpoint on rule named NAME\n";
        std::cout << "(h)elp -- show this help\n";
        break;
      case DebugCommand::DBGCMD_UNKNOWN:
        std::cout << "Unknown command\n";
        break;
      default:
        std::cout << "Not implemented\n";
        break;
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
  std::cout << '\n';
  std::flush(std::cout);
}

static bool print_section(const std::vector<std::string> &lines, size_t from_line, size_t screen_height, size_t screen_width) {
  size_t line_count = lines.size();
  if (line_count < screen_height) {
    for (auto s : lines) {
      std::cout << s << '\n';
    }
    return false;
  }

  if (from_line > line_count - (screen_height - 1)) {
    from_line = line_count - (screen_height - 1);
  }

  std::cout << "\e[2J\e[0;0H"; // delete screen and move cursor to top left

  for (size_t i = from_line; i < from_line + (screen_height - 1); i++) {
    std::cout << lines.at(i) << '\n';
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
