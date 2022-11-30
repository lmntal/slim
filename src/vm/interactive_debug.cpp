#include "interactive_debug.hpp"
#include "task.h"
#include "stringifier.hpp"
#include <iostream>
#include <string>
#include <sstream>

const char* get_instr_name(const int id) {
  for (auto &p : instr_spec)
    if (id == p.first)
      return p.second.op_str;
  return "unknown";
}

std::string InteractiveDebugger::stringify_atom(const LmnAtomRef atom, const LmnByte at) {
  return slim::stringifier::lmn_stringify_atom(atom, at);
}

// print register content
std::string InteractiveDebugger::stringify_register(const LmnRegister *reg) {
  if (!reg) {
    return "null";
  }

  std::ostringstream retVal;
  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  if (tt == TT_ATOM) {
    retVal << stringify_atom((LmnAtomRef)wt, at);
  } else if (tt == TT_MEM) {
    retVal << "0x" << std::hex << wt << std::dec << "(" << (unsigned int) at << ",mem)";
  } else {
    retVal << wt << "(" << (unsigned int) at << ",other)";
  }

  return retVal.str();
}

std::string InteractiveDebugger::stringify_regarray(const LmnRegisterArray *reg_array) {
  if (!reg_array) {
    return "null";
  }

  std::ostringstream retVal;

  retVal << "[ ";
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    retVal << stringify_register(&(reg_array->at(i)));
    if (i != reg_len - 1) {
      retVal << " | ";
    }
  }
  retVal << " ]";

  return retVal.str();
}

std::string InteractiveDebugger::stringify_instr(const LmnRuleInstr instr) {
  if (!instr) {
    return "null";
  }

  std::ostringstream retVal;

  LmnRuleInstr instr_copy = instr;
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr_copy, op);

  retVal << get_instr_name(op) << " ";
  for (auto &p : instr_spec) {
    if (op == p.first) {
      size_t argSize = p.second.args.size();
      for (size_t i = 0; i < argSize; i++) {
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

std::string InteractiveDebugger::stringify_atomlist(const AtomListEntry *atomlist) {
  if (!atomlist) {
    return "null";
  }

  std::ostringstream retVal;
  auto iter = std::begin(*atomlist);
  auto end = std::end(*atomlist);

  retVal << "[ ";

  while (iter != end) {
    retVal << stringify_atom(*iter, LMN_ATTR_MAKE_LINK(0));
    ++iter;
    if (iter != end) {
      retVal << " | ";
    }
  }

  retVal << " ]";
  return retVal.str();
}

std::vector<std::string> InteractiveDebugger::split_command(std::string command) {
  auto v = std::vector<std::string>();

  if (command.empty()) {
    return v;
  }

  command += " ";

  for (size_t start_index = 0; start_index < command.size(); ) {
    size_t space_index = command.find(" ", start_index);
    std::string temp = command.substr(start_index, space_index - start_index);

    if (!temp.empty()) {
      v.push_back(temp);
    }

    start_index = space_index + 1;
  }

  return v;
}

void InteractiveDebugger::start_session(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  if (input_eof) {
    return;
  }

  fflush(stdout);

  std::cout << std::endl;
  std::cout << "Rule        : ";
  std::cout << (!rule ? "null" : rule->name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(rule->name)) << std::endl;

  std::cout << "Instruction : " << stringify_instr(instr) << std::endl;

  if (instr && *((LmnInstrOp *)instr) != INSTR_SPEC && previous_instr >= instr) {
    std::cout << "Possible backtracking. (Prev Inst : " << stringify_instr(previous_instr) << ")" << std::endl;
  }

  bool continue_session = true;
  while (continue_session) {
    std::string debug_command_input;

    if (std::cin.eof()) {
      std::cout << "^D" << std::endl;
      input_eof = true;
      break;
    }

    std::cout << "(debugger) ";
    std::getline(std::cin, debug_command_input);

    auto v = split_command(debug_command_input);

    size_t arg_len = v.size();

    if (arg_len == 0) {
      continue;
    }

    DebugCommand cmd = DebugCommand::DBGCMD_UNKNOWN;

    for (auto p : debug_commands) {
      if (p.first == v.at(0)) {
        cmd = p.second;
        break;
      }
    }

    DebugCommandArg arg1 = DebugCommandArg::DBGARG_UNKNOWN;

    if (arg_len >= 2) {
      for (auto p : debug_command_args) {
        if (p.first == v.at(1)) {
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
        if (arg_len < 2) {
          std::cout << "Too few arguments" << std::endl;
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          if (!rc) {
            std::cout << "LmnReactCxtRef is null" << std::endl;
            break;
          }
          std::cout << stringify_regarray(&rc->work_array) << std::endl;
        }
        else if (arg1 == DebugCommandArg::DBGARG_ATOMLIST) {
          if (!rc) {
            std::cout << "LmnReactCxtRef is null" << std::endl;
            break;
          }

          size_t list_count = ((LmnMembraneRef) rc->wt(0))->max_functor;

          if (arg_len == 2) {
            for (size_t i = 0; i < list_count; i++) {
              auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
              if (list) {
                std::cout << lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
                std::cout << "_";
                std::cout << (unsigned int) lmn_functor_table->get_entry(i)->arity;
                std::cout << " \t: " << stringify_atomlist(list) << std::endl;
              }
            }
          } else {
            for (size_t i = 0; i < list_count; i++) {
              auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
              if (list) {
                std::string functor_name = lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
                unsigned int functor_arity = lmn_functor_table->get_entry(i)->arity;

                for (size_t j = 2; j < arg_len; j++) {
                  size_t underscore = v.at(j).find("_");
                  if (underscore != std::string::npos && underscore > 0 && underscore < v.at(j).size()) {
                    std::string input_name = v.at(j).substr(0, underscore);
                    unsigned int input_arity = stoul(v.at(j).substr(underscore + 1));
                    if (functor_name == input_name && functor_arity == input_arity) {
                      std::cout << "  " << functor_name << "_" << functor_arity << " \t: ";
                      std::cout << stringify_atomlist(list) << std::endl;
                    }
                  }
                }
              }
            }
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_BRKPNT) {
          std::cout << "Breakpoints for instructions :" << std::endl;
          for (auto i : breakpoints_on_instr) {
            std::cout << "  " << get_instr_name(i) << std::endl;
          }

          std::cout << std::endl << "Breakpoints for rules :" << std::endl;
          for (auto str : breakpoints_on_rule) {
            std::cout << "  " << str << std::endl;
          }
          std::cout << std::endl;
        }
        else if (arg1 == DebugCommandArg::DBGARG_MEMBRANE) {
          std::cout << "Current membrane : " << std::endl;
          std::cout << slim::stringifier::lmn_stringify_mem((LmnMembraneRef)rc->wt(0)) << std::endl;
        }
        else {
          std::cout << "Invalid arguments" << std::endl;
        }

        break;
      case DebugCommand::DBGCMD_STEP:
        if (arg_len < 2) {
          std::cout << "Too few arguments" << std::endl;
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          instr_execution_count = 0;
          instr_execution_stop_at = -1;
          rule_reaction_count = 0;

          long l = 1;
          if (arg_len >= 3) {
            l = stol(v.at(2));
          }

          if (l >= 1) {
            rule_reaction_stop_at = l;
            continue_session = false;
          } else {
            std::cout << "Invalid step count: " << l << std::endl;
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          rule_reaction_count = 0;
          rule_reaction_stop_at = -1;
          instr_execution_count = 0;

          long l = 1;
          if (arg_len >= 3) {
            l = stol(v.at(2));
          }

          if (l >= 1) {
            instr_execution_stop_at = l;
            continue_session = false;
          } else {
            std::cout << "Invalid step count: " << l << std::endl;
          }
        }
        else {
          std::cout << "Invalid arguments" << std::endl;
        }

        break;
      case DebugCommand::DBGCMD_BREAK:
        if (arg_len < 3) {
          std::cout << "Too few arguments" << std::endl;
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          bool exists = false;
          for (auto str : breakpoints_on_rule) {
            if (v.at(2) == str) {
              exists = true;
              break;
            }
          }
          if (exists) {
            std::cout << "Breakpoint is already set to instruction: " << v.at(2) << std::endl;
          } else {
            breakpoints_on_rule.push_back(v.at(2));
            std::cout << "Breakpoint is set to rule: " << v.at(2) << std::endl;
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(v.at(2).c_str());

          if (instr_id == -1) {
            std::cout << "Unknown Instruction: breakpoint is not set." << std::endl;
            break;
          }

          bool already_exists = false;
          for (auto i : breakpoints_on_instr) {
            if (instr_id == i) {
              already_exists = true;
              break;
            }
          }

          if (already_exists) {
            std::cout << "Breakpoint is already set to instruction: " << v.at(2) << std::endl;
          } else {
            breakpoints_on_instr.push_back((LmnInstruction) instr_id);
            std::cout << "Breakpoint is set to instruction: " << v.at(2) << std::endl;
          }
        }
        else {
          std::cout << "Invalid arguments" << std::endl;
        }

        break;
      case DebugCommand::DBGCMD_DELETE:
        if (arg_len < 3) {
          std::cout << "Too few arguments" << std::endl;
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          size_t index = 0, max = breakpoints_on_rule.size();
          for ( ; index < max; index++) {
            if (v.at(2) == breakpoints_on_rule.at(index)) {
              break;
            }
          }

          if (index == max) {
            std::cout << "Breakpoint is not set to rule: " << v.at(2) << std::endl;
          } else {
            breakpoints_on_rule.erase(breakpoints_on_rule.begin() + index);
            std::cout << "Breakpoint is deleted for rule: " << v.at(2) << std::endl;
          }
          break;
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(v.at(2).c_str());
          if (instr_id == -1) {
            std::cout << "Unknown Instruction: no breakpoint is deleted" << std::endl;
          } else {
            size_t index = 0, max = breakpoints_on_instr.size();
            for ( ; index < max; index++) {
              if (instr_id == breakpoints_on_instr.at(index)) {
                break;
              }
            }

            if (index == max) {
              std::cout << "Breakpoint is not set to instruction: " << v.at(2) << std::endl;
            } else {
              breakpoints_on_instr.erase(breakpoints_on_instr.begin() + index);
              std::cout << "Breakpoint is deleted for instruction: " << v.at(2) << std::endl;
            }
          }
        }
        else {
          std::cout << "Invalid arguments" << std::endl;
        }

        break;
      case DebugCommand::DBGCMD_HELP:
        std::cout << "(c)ontinue -- continue execution until next breakpoint" << std::endl;
        std::cout << "(s)tep (i)nstruction -- execute one intermediate instruction" << std::endl;
        std::cout << "(s)tep (i)nstruction <N> -- execute N intermediate instructions" << std::endl;
        std::cout << "(s)tep (r)ule -- apply one rule" << std::endl;
        std::cout << "(s)tep (r)ule <N> -- apply N rules" << std::endl;
        std::cout << "(i)nfo (r)egisters -- print register content of current react context" << std::endl;
        std::cout << "(i)nfo (a)tomlist -- print atomlist of current membrane" << std::endl;
        std::cout << "(i)nfo (a)tomlist FUNCTOR -- print atomlist with functor FUNCTOR of current membrane" << std::endl;
        std::cout << "(i)nfo (m)embrane -- print currently reacting membrane" << std::endl;
        std::cout << "(i)nfo (b)reakpoints -- list all breakpoints" << std::endl;
        std::cout << "(b)reak (i)nstruction NAME -- set breakpoint on instruction named NAME" << std::endl;
        std::cout << "(b)reak (r)ule NAME -- set breakpoint on rule named NAME" << std::endl;
        std::cout << "(d)elete (i)nstruction NAME -- delete breakpoint on instruction named NAME" << std::endl;
        std::cout << "(d)elete (r)ule NAME -- delete breakpoint on rule named NAME" << std::endl;
        std::cout << "(h)elp -- show this help" << std::endl;
        break;
      case DebugCommand::DBGCMD_UNKNOWN:
        std::cout << "Unknown command" << std::endl;
        break;
      default:
        std::cout << "Not implemented" << std::endl;
        break;
    }
  }
}

void InteractiveDebugger::break_on_instruction(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr) {
  instr_execution_count++;
  if (instr_execution_count == instr_execution_stop_at) {
    instr_execution_count = 0;
    instr_execution_stop_at = -1;
    start_session(rc, rule, instr);
  } else {
    LmnInstrOp op = *(LmnInstrOp *)instr;
    bool should_break = false;
    for (auto i : breakpoints_on_instr) {
      if (op == i) {
        should_break = true;
        break;
      }
    }

    if (should_break) {
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
    bool should_break = false;
    for (auto str : breakpoints_on_rule) {
      if (!strcmp(lmn_id_to_name(rule->name), str.c_str())) {
        should_break = true;
        break;
      }
    }

    if (should_break) {
      start_session(rc, rule, instr);
    }
  }

  previous_rule = rule;
}

void InteractiveDebugger::finish_debugging(void) {
  std::cout << std::endl;
  std::cout.flush();
}
