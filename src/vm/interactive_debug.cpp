#include "interactive_debug.hpp"
#include "task.h"
#include <iostream>
#include <string>
#include <sstream>

const char* get_instr_name(const int id) {
  for (auto &p : instr_spec)
    if (id == p.first)
      return p.second.op_str;
  return "unknown";
}

std::string InteractiveDebugger::stringify(const LmnAtomRef atom, const LmnByte at) {
  std::ostringstream retVal;

  if (LMN_ATTR_IS_DATA(at)) {
    LmnDataAtomRef dAtomRef = (LmnDataAtomRef) atom;
    switch (at) {
      case LMN_INT_ATTR:
        retVal << *((int *)&dAtomRef);
        break;
      case LMN_DBL_ATTR:
        retVal << lmn_get_double(dAtomRef);
        break;
      default:
        retVal << *((long *)&dAtomRef);
        break;
    }
  } else if (!atom) {
    retVal << "null";
  } else {
    LmnSymbolAtomRef sAtomRef = (LmnSymbolAtomRef) atom;
    retVal << sAtomRef->str() << "(";

    for (size_t i = 0, linkNum = sAtomRef->get_link_num(); i < linkNum; i++) {
      LmnAtomRef ref = sAtomRef->get_link(i);
      LmnLinkAttr attr = sAtomRef->get_attr(i);
      if (LMN_ATTR_IS_DATA(attr)) {
        switch (attr) {
          case LMN_INT_ATTR:
            retVal << *((int *)&ref);
            break;
          case LMN_DBL_ATTR:
            retVal << lmn_get_double((LmnDataAtomRef) ref);
            break;
          default:
            retVal << *((long *)&ref);
            break;
        }
      } else if (!ref) {
        retVal << "null";
      } else {
        // TODO fix segmentation fault
        retVal << ((LmnSymbolAtomRef)ref)->str();
        // retVal << std::hex << ref << dec << "(" << attr << ",?)";
      }
      if (i != linkNum - 1) {
        retVal << ",";
      }
    }
    retVal << ")";
  }

  return retVal.str();
}

// print register content
std::string InteractiveDebugger::stringify(const LmnRegister *reg) {
  if (!reg) {
    return "null";
  }

  std::ostringstream retVal;
  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  if (tt == TT_ATOM) {
    retVal << stringify((LmnAtomRef) wt, at);
  } else if (tt == TT_MEM) {
    retVal << "0x" << std::hex << wt << std::dec << "(" << std::to_string(at) << ",mem)";
  } else {
    retVal << wt << "(" << std::to_string(at) << ",other)";
  }

  return retVal.str();
}

std::string InteractiveDebugger::stringify(const LmnRegisterArray *reg_array) {
  if (!reg_array) {
    return "null";
  }

  std::ostringstream retVal;

  retVal << "[ ";
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    retVal << stringify(&(reg_array->at(i)));
    if (i != reg_len - 1) {
      retVal << " | ";
    }
  }
  retVal << " ]";

  return retVal.str();
}

std::string InteractiveDebugger::stringify(const LmnRuleInstr instr) {
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
            retVal << std::to_string(arg_functor);
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
    retVal << std::to_string(lmn_functor_table->get_entry(func)->arity);
  }

  return retVal.str();
}

std::string InteractiveDebugger::stringify(const AtomListEntry *atomlist) {
  if (!atomlist) {
    return "null";
  }

  std::ostringstream retVal;
  auto iter = std::begin(*atomlist);
  auto end = std::end(*atomlist);

  retVal << "[ ";

  while (iter != end) {
    retVal << stringify(*iter, LMN_ATTR_MAKE_LINK(0));
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
  using namespace std;

  if (input_eof) {
    return;
  }

  fflush(stdout);

  cout << endl;
  cout << "Rule        : ";
  cout << (!rule ? "null" : rule->name == ANONYMOUS ? "ANONYMOUS" : lmn_id_to_name(rule->name)) << endl;

  cout << "Instruction : " << stringify(instr) << endl;

  if (instr && *((LmnInstrOp *)instr) != INSTR_SPEC && previous_instr >= instr) {
    cout << "Possible backtracking. (Prev Inst : " << stringify(previous_instr) << ")" << endl;
  }

  bool continue_session = true;
  while (continue_session) {
    string debug_command_input;

    if (cin.eof()) {
      cout << "^D" << endl;
      input_eof = true;
      break;
    }

    cout << "(debugger) ";
    getline(cin, debug_command_input);

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
          cout << "Too few arguments" << endl;
          break;
        }

        if (arg1 == DebugCommandArg::DBGARG_RULE_REG) {
          if (!rc) {
            cout << "LmnReactCxtRef is null" << endl;
            break;
          }
          cout << stringify(&rc->work_array) << endl;
        }
        else if (arg1 == DebugCommandArg::DBGARG_ATOMLIST) {
          if (!rc) {
            cout << "LmnReactCxtRef is null" << endl;
            break;
          }

          size_t list_count = ((LmnMembraneRef) rc->wt(0))->max_functor;

          if (arg_len == 2) {
            for (size_t i = 0; i < list_count; i++) {
              auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
              if (list) {
                cout << lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
                cout << "_";
                cout << lmn_functor_table->get_entry(i)->arity;
                cout << " \t: " << stringify(list) << endl;
              }
            }
          } else {
            for (size_t i = 0; i < list_count; i++) {
              auto list = ((LmnMembraneRef) rc->wt(0))->atomset[i];
              if (list) {
                string functor_name = lmn_id_to_name(lmn_functor_table->get_entry(i)->name);
                unsigned int functor_arity = lmn_functor_table->get_entry(i)->arity;

                for (size_t j = 2; j < arg_len; j++) {
                  size_t underscore = v.at(j).find("_");
                  if (underscore != string::npos && underscore > 0 && underscore < v.at(j).size()) {
                    string input_name = v.at(j).substr(0, underscore);
                    unsigned int input_arity = stoul(v.at(j).substr(underscore + 1));
                    if (functor_name == input_name && functor_arity == input_arity) {
                      cout << "  " << functor_name << "_" << functor_arity << " \t: ";
                      cout << stringify(list) << endl;
                    }
                  }
                }
              }
            }
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_BRKPNT) {
          cout << "Breakpoints for instructions :" << endl;
          for (auto i : breakpoints_on_instr) {
            cout << "  " << get_instr_name(i) << endl;
          }

          cout << endl << "Breakpoints for rules :" << endl;
          for (auto str : breakpoints_on_rule) {
            cout << "  " << str << endl;
          }
          cout << endl;
        }
        else {
          cout << "Invalid arguments" << endl;
        }

        break;
      case DebugCommand::DBGCMD_STEP:
        if (arg_len < 2) {
          cout << "Too few arguments" << endl;
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
            cout << "Invalid step count: " << l << endl;
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
            cout << "Invalid step count: " << l << endl;
          }
        }
        else {
          cout << "Invalid arguments" << endl;
        }

        break;
      case DebugCommand::DBGCMD_BREAK:
        if (arg_len < 3) {
          cout << "Too few arguments" << endl;
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
            cout << "Breakpoint is already set to instruction: " << v.at(2) << endl;
          } else {
            breakpoints_on_rule.push_back(v.at(2));
            cout << "Breakpoint is set to rule: " << v.at(2) << endl;
          }
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(v.at(2).c_str());

          if (instr_id == -1) {
            cout << "Unknown Instruction: breakpoint is not set." << endl;
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
            cout << "Breakpoint is already set to instruction: " << v.at(2) << endl;
          } else {
            breakpoints_on_instr.push_back((LmnInstruction) instr_id);
            cout << "Breakpoint is set to instruction: " << v.at(2) << endl;
          }
        }
        else {
          cout << "Invalid arguments" << endl;
        }

        break;
      case DebugCommand::DBGCMD_DELETE:
        if (arg_len < 3) {
          cout << "Too few arguments" << endl;
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
            cout << "Breakpoint is not set to rule: " << v.at(2) << endl;
          } else {
            breakpoints_on_rule.erase(breakpoints_on_rule.begin() + index);
            cout << "Breakpoint is deleted for rule: " << v.at(2) << endl;
          }
          break;
        }
        else if (arg1 == DebugCommandArg::DBGARG_INSTR) {
          int instr_id = get_instr_id(v.at(2).c_str());
          if (instr_id == -1) {
            cout << "Unknown Instruction: no breakpoint is deleted" << endl;
          } else {
            size_t index = 0, max = breakpoints_on_instr.size();
            for ( ; index < max; index++) {
              if (instr_id == breakpoints_on_instr.at(index)) {
                break;
              }
            }

            if (index == max) {
              cout << "Breakpoint is not set to instruction: " << v.at(2) << endl;
            } else {
              breakpoints_on_instr.erase(breakpoints_on_instr.begin() + index);
              cout << "Breakpoint is deleted for instruction: " << v.at(2) << endl;
            }
          }
        }
        else {
          cout << "Invalid arguments" << endl;
        }

        break;
      case DebugCommand::DBGCMD_HELP:
        cout << "(c)ontinue -- continue execution until next breakpoint" << endl;
        cout << "(s)tep (i)nstruction -- execute one intermediate instruction" << endl;
        cout << "(s)tep (i)nstruction <N> -- execute N intermediate instructions" << endl;
        cout << "(s)tep (r)ule -- apply one rule" << endl;
        cout << "(s)tep (r)ule <N> -- apply N rules" << endl;
        cout << "(i)nfo (r)egisters -- print register content of current react context" << endl;
        cout << "(i)nfo (a)tomlist -- print atomlist of current membrane" << endl;
        cout << "(i)nfo (a)tomlist FUNCTOR -- print atomlist with functor FUNCTOR of current membrane" << endl;
        cout << "(i)nfo (b)reakpoints -- list all breakpoints" << endl;
        cout << "(b)reak (i)nstruction NAME -- set breakpoint on instruction named NAME" << endl;
        cout << "(b)reak (r)ule NAME -- set breakpoint on rule named NAME" << endl;
        cout << "(d)elete (i)nstruction NAME -- delete breakpoint on instruction named NAME" << endl;
        cout << "(d)elete (r)ule NAME -- delete breakpoint on rule named NAME" << endl;
        cout << "(h)elp -- show this help" << endl;
        break;
      case DebugCommand::DBGCMD_UNKNOWN:
        cout << "Unknown command" << endl;
        break;
      default:
        cout << "Not implemented" << endl;
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
