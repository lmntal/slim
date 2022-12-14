#include "interactive_debug.hpp"
#include "debug_printer.hpp"

#include "task.h"
#include "../verifier/statespace.h"

#include <iostream>

#include <sys/ioctl.h>
#include <unistd.h>
#include <termios.h>

// constants
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
// container
template <class T>
static inline bool vector_contains(const std::vector<T> &vector, T value);

// string
static inline bool string_starts_with(const std::string &str1, const std::string &str2);

// commands
static std::pair<DebugCommand,std::vector<std::string>> parse_command(const std::string &command);
static std::string get_membrane_tree(const LmnMembraneRef mem, std::string prefix, const LmnMembraneRef global, const LmnMembraneRef current);
static LmnMembraneRef get_pointer_to_membrane_by_id(const LmnMembraneRef mem, ProcessID id);

// output
static bool print_section(const std::vector<std::string> &lines, size_t from_line, size_t screen_height, size_t screen_width);
// prototype end

template <class T>
static inline bool vector_contains(const std::vector<T> &vector, T value) {
  auto end = vector.cend();
  auto res = std::find(vector.cbegin(), end, value);
  return res != end;
}

static inline bool string_starts_with(const std::string &str1, const std::string &str2) {
  return str1.size() < str2.size() ? false : std::equal(str2.cbegin(), str2.cend(), str1.cbegin());
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

  if (cmd_iter_found == cmd_iter_end) { // not found
    return {DebugCommand::__UNKNOWN, {}};
  } else { // found
    command_vec.erase(command_vec.begin(), command_vec.begin() + (*cmd_iter_found).second.size());
    return {(*cmd_iter_found).first, command_vec};
  }
}

static std::string get_membrane_tree(const LmnMembraneRef mem, std::string prefix, const LmnMembraneRef global, const LmnMembraneRef current) {
  if (mem == nullptr) {
    return "null";
  }

  // 自身の名前の取得
  std::string myname = "Membrane(" + slim::debug_printer::print_object_ids(mem->NAME_ID(), mem, mem->mem_id()) + ")";

  if (mem == global) {
    myname += " *global";
  }
  if (mem == current) {
    myname += " *current";
  }

  // 自身の名前を追加
  std::string retVal = prefix + "|--" + myname + "\n";

  // 子があれば先に取得
  if (mem->child_head != nullptr) {
    retVal += get_membrane_tree(mem->child_head, prefix + "|  ", global, current) + "\n";
  }

  // 兄弟があれば続けて表示
  if (mem->next != nullptr) {
    // 間を調整
    if (mem->child_head != nullptr) {
      retVal += prefix + "|";
    }
    retVal += get_membrane_tree(mem->next, prefix, global, current) + "\n";
  }

  retVal.pop_back();
  return retVal;
}

static LmnMembraneRef get_pointer_to_membrane_by_id(const LmnMembraneRef mem, ProcessID id) {
  if (mem == nullptr) {
    return nullptr;
  } else if (mem->mem_id() == id) {
    return mem;
  } else {
    LmnMembraneRef ref = get_pointer_to_membrane_by_id(mem->child_head, id);
    return ref != nullptr ? ref : get_pointer_to_membrane_by_id(mem->next, id);
  }
}

InteractiveDebugger::InteractiveDebugger() {
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  screen_height = w.ws_row;
  screen_width = w.ws_col;
}

InteractiveDebugger::~InteractiveDebugger() {}

void InteractiveDebugger::set_breakpoint_rule(std::string rule) {
  if (vector_contains(breakpoints_on_rule, rule)) {
    std::cout << "Breakpoint is already set to rule: " << rule << "\n";
  } else {
    breakpoints_on_rule.push_back(rule);
    std::cout << "Breakpoint is set to rule: " << rule << "\n";
  }
}

void InteractiveDebugger::set_breakpoint_instr(std::string instr) {
  int instr_id = get_instr_id(instr.c_str());
  if (instr_id == -1) {
    std::cerr << "Unknown Instruction: breakpoint is not set.\n";
  }
  else if (vector_contains(breakpoints_on_instr, (LmnInstruction)instr_id)) {
    std::cout << "Breakpoint is already set to instruction: " << instr << "\n";
  } else {
    breakpoints_on_instr.push_back((LmnInstruction) instr_id);
    std::cout << "Breakpoint is set to instruction: " << instr << "\n";
  }
}

void InteractiveDebugger::delete_breakpoint_rule(std::string rule) {
  auto end = breakpoints_on_rule.end();
  auto res = std::find(breakpoints_on_rule.begin(), end, rule);
  if (res == end) {
    std::cout << "Breakpoint is not set to rule: " << rule << "\n";
  } else {
    breakpoints_on_rule.erase(res);
    std::cout << "Breakpoint is deleted for rule: " << rule << "\n";
  }
}

void InteractiveDebugger::delete_breakpoint_instr(std::string instr) {
  int instr_id = get_instr_id(instr.c_str());
  if (instr_id == -1) {
    std::cerr << "Unknown Instruction: no breakpoint is deleted\n";
  } else {
    auto end = breakpoints_on_instr.end();
    auto res = std::find(breakpoints_on_instr.begin(), end, instr_id);
    if (res == end) {
      std::cout << "Breakpoint is not set to instruction: " << instr << "\n";
    } else {
      breakpoints_on_instr.erase(res);
      std::cout << "Breakpoint is deleted for instruction: " << instr << "\n";
    }
  }
}

void InteractiveDebugger::list_breakpoints() {
  std::string s = "Breakpoints for instructions :\n";
  for (auto &i : breakpoints_on_instr) {
    s += "    " + slim::debug_printer::to_string_instrop(i) + "\n";
  }
  s += "Breakpoints for rules :\n";
  for (auto &str : breakpoints_on_rule) {
    s += "    " + str + "\n";
  }
  print_feeding(s);
}

void InteractiveDebugger::start_session_on_entry() {
  fflush(stdout);
  std::flush(std::cout);

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

    switch (cmd) {
      // info breakpoints
      case DebugCommand::INFO_BREAKPOINT: {
        list_breakpoints();
        break;
      }
      // break instruction
      case DebugCommand::BREAK_INSTR: {
        if (opt_argc == 0) {
          std::cerr << "Missing target instruction name.\n";
        } else {
          set_breakpoint_instr(opt_argv.at(0));
        }
        break;
      }
      // break rule
      case DebugCommand::BREAK_RULE: {
        if (opt_argc == 0) {
          std::cerr << "Missing target rule name.\n";
        } else {
          set_breakpoint_rule(opt_argv.at(0));
        }
        break;
      }
      // delete instruction
      case DebugCommand::DELETE_INSTR: {
        if (opt_argc == 0) {
          std::cerr << "Missing target instruction name.\n";
        } else {
          delete_breakpoint_instr(opt_argv.at(0));
        }
        break;
      }
      // delete rule
      case DebugCommand::DELETE_RULE: {
        if (opt_argc == 0) {
          std::cerr << "Missing target rule name.\n";
        } else {
          delete_breakpoint_rule(opt_argv.at(0));
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
        std::cerr << "Unknown command.\n";
        break;
      }
      default: {
        std::cerr << "The command was not executed because program is not running.\n";
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
  std::cout << "Instruction : " << slim::debug_printer::to_string_instr(instr) << "\n";

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
          std::cerr << "LmnReactCxtRef is null.\n";
          break;
        }
        // info registers
        if (opt_argc == 0) {
          print_feeding(slim::debug_printer::to_string_regarray(&rc->work_array));
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
              std::cerr << "Invalid argument at " << i << ": Not a valid integer.\n";
              continue;
            }
            if (l >= rc->capacity()) {
              std::cerr << "Invalid argument at " << i << ": Exceeds length of register.\n";
              continue;
            }
            s += "Register[" + std::to_string(l) + "] : " + (dev ? slim::debug_printer::to_string_dev_reg(&rc->reg(l)) : slim::debug_printer::to_string_reg(&rc->reg(l))) + "\n";
          }
          print_feeding(s);
        }
        break;
      }
      // info atomlist
      case DebugCommand::INFO_ATOMLIST: {
        if (rc == nullptr) {
          std::cerr << "LmnReactCxtRef is null.\n";
          break;
        }
        auto atom_lists = ((LmnMembraneRef)rc->wt(0))->atom_lists();
        // info atomlist
        if (opt_argc == 0) {
          std::string s = "";
          for (auto &p : atom_lists) {
            s += slim::debug_printer::to_string_functor(p.first) + "   : " + slim::debug_printer::to_string_atomlist(p.second) + "\n";
          }
          print_feeding(s);
        }
        // info atomlist <FUNCTOR>
        else if (opt_argc == 1) {
          auto end = atom_lists.end();
          auto res = std::find_if(atom_lists.begin(), end,
            [&opt_argv](std::pair<LmnFunctor, AtomListEntry*> p) -> bool {
              return slim::debug_printer::to_string_functor(p.first) == opt_argv.at(0);
            }
          );
          if (res == end) { // not found
            std::cerr << "Atomlist with specified functor could not be found.\n";
          } else { // found
            print_feeding("  " + opt_argv.at(0) + "   : " + slim::debug_printer::to_string_atomlist((*res).second) + "\n");
          }
        }
        // info atomlist <FUNCTOR> <N> (dev)
        else {
          bool dev = opt_argc > 2 && opt_argv.at(2) == "dev";
          size_t input_n;
          try {
            input_n = std::stoul(opt_argv.at(1));
          } catch (const std::invalid_argument &e) {
            std::cerr << "Invalid argument at 3: Not a valid integer.\n";
            break;
          }
          auto end = atom_lists.end();
          auto res = std::find_if(atom_lists.begin(), end,
            [&opt_argv](std::pair<LmnFunctor,AtomListEntry*> p) {
              return slim::debug_printer::to_string_functor(p.first) == opt_argv.at(1);
            }
          );
          if (res == end) { // not found
            std::cerr << "Atomlist with specified functor could not be found.\n";
          } else { // found
            size_t i = 0;
            std::string str = "";
            for (auto &s : *(*res).second) {
              if (i == input_n) {
                str = slim::debug_printer::to_string_satom(s);
                if (dev) {
                  str += "\n" + slim::debug_printer::to_string_dev_satom(s);
                }
                break;
              }
              i++;
            }
            if (i != input_n) { // not found
              std::cerr << "Invalid argument: Exceeds length of atomlist.\n";
            } else { // found
              print_feeding(str);
            }
          }
        }
        break;
      }
      // info breakpoints
      case DebugCommand::INFO_BREAKPOINT: {
        list_breakpoints();
        break;
      }
      // info membrane
      case DebugCommand::INFO_MEMBRANE: {
        if (rc == nullptr) {
          std::cerr << "LmnReactCxtRef is null.\n";
          break;
        }
        if (rc->global_root == nullptr) {
          std::cerr << "Global Root Membrane is null.\n";
          break;
        }
        // info membrane
        if (opt_argc == 0) {
          print_feeding(get_membrane_tree(rc->global_root, "", rc->global_root, (LmnMembraneRef) rc->wt(0)));
        }
        // info membrane current
        else if (string_starts_with("current", opt_argv.at(0))) {
          print_feeding("Current membrane (Addr[" + slim::debug_printer::to_hex_string(rc->wt(0)) + "]) :\n"
                        + slim::debug_printer::to_string_mem((LmnMembraneRef)rc->wt(0)) + "\n");
        }
        // info membrane global
        else if (string_starts_with("global", opt_argv.at(0))) {
          print_feeding("Global root membrane (Addr[" + slim::debug_printer::to_hex_string(rc->global_root) + "]) :\n"
                        + slim::debug_printer::to_string_mem(rc->global_root) + "\n");
        }
        // info membrane <N>
        else {
          size_t l;
          try {
            l = std::stoul(opt_argv.at(0));
          } catch (const std::invalid_argument& e) {
            std::cerr << "Invalid argument at 2: Not a valid integer.\n";
            break;
          }
          LmnMembraneRef mem = get_pointer_to_membrane_by_id(rc->global_root, l);
          if (mem == nullptr) {
            std::cerr << "Invalid argument at 2: Membrane with specified ID could not be found in a tree rooted from global root membrane.\n";
          } else {
            print_feeding("Membrane (" + slim::debug_printer::print_object_ids(mem, l) + ") : \n" + slim::debug_printer::to_string_mem(mem));
          }
        }
        break;
      }
      // info statespace
      case DebugCommand::INFO_STATESPACE: {
        if (!lmn_env.nd) {
          std::cerr << "StateSpace could not be shown in normal execution.\n";
          break;
        }
        if (statespace == nullptr) {
          std::cerr << "StateSpaceRef is null.\n";
          break;
        }
        // info statespace
        if (opt_argc == 0) {
          std::string str = "StateSpace (Addr[" + slim::debug_printer::to_hex_string(statespace) + "])\n";
          auto states_vec = statespace->all_states();
          std::sort(states_vec.begin(), states_vec.end(),
            [](State *state_l, State *state_r) -> bool {
              return state_id(state_l) < state_id(state_r);
            }
          );
          for (State *state : states_vec) {
            // state info
            str += "   State[" + std::to_string(state_id(state)) + "] : ";

            // membrane
            LmnMembraneRef mem = state->restore_membrane_inner(FALSE);
            str += slim::debug_printer::to_string_mem(mem);
            if (state->is_binstr_user()) {
              mem->free_rec();
            }

            str += " -> [";
            for (unsigned int i = 0, max = state->successor_num; i < max; i++) {
              str += std::to_string(state_id(state_succ_state(state, i)));
              if (i != max - 1) {
                str += ",";
              }
            }
            str += "]";

            if (state == expanding_state) {
              str += " *expanding";
            }

            str += "\n";
          }
          print_feeding(str);
        }
        // info statespace <N>
        else {
          unsigned long l;
          try {
            l = std::stoul(opt_argv.at(0));
          } catch (const std::invalid_argument &e) {
            std::cerr << "Invalid argument: Not a valid integer.\n";
            break;
          }

          auto states_vec = statespace->all_states();
          auto end = states_vec.end();
          auto res = std::find_if(states_vec.begin(), end,
            [l](State *state) -> bool { return state_id(state) == l; }
          );

          if (res == end) {
            std::cerr << "State with given id could not be found in this statespace.\n";
          } else {
            State *state = *res;
            std::string s = "StateSpace (Addr[" + slim::debug_printer::to_hex_string(statespace) + "])\n";
            s += " State (" + slim::debug_printer::print_object_ids(state, state_id(state)) + ") -> ";

            for (unsigned int i = 0, max = state->successor_num; i < max; i++) {
              s += std::to_string(state_id(state_succ_state(state, i)));
              if (i != max - 1) {
                s += ",";
              }
            }
            s += "\n";

            LmnMembraneRef mem = state->restore_membrane_inner(FALSE);
            s += ("  Membrane (" + slim::debug_printer::print_object_ids(mem->NAME_ID(), mem->mem_id()) + ")\n   "
                  + slim::debug_printer::to_string_mem(mem));
            if (state->is_binstr_user()) {
              mem->free_rec();
            }

            print_feeding(s);
          }
        }
        break;
      }
      // FIXME 1ステップ余分に進んでいるかも
      // step rule
      case DebugCommand::STEP_RULE: {
        reset_step_execution();

        size_t l = 1;
        // step rule <N>
        if (opt_argc > 0) {
          try {
            l = std::stoul(opt_argv.at(0));
          } catch (const std::invalid_argument& e) {
            l = 0;
          }
        }

        if (l >= 1) {
          rule_reaction_stop_at = l;
          continue_session = false;
        } else {
          std::cerr << "Invalid argument: Not a valid integer.\n";
        }
        break;
      }
      // step instruction
      case DebugCommand::STEP_INSTR: {
        reset_step_execution();

        size_t l = 1;
        // step instruction <N>
        if (opt_argc > 0) {
          try {
            l = std::stoul(opt_argv.at(0));
          } catch (const std::invalid_argument& e) {
            l = 0;
          }
        }

        if (l >= 1) {
          instr_execution_stop_at = l;
          continue_session = false;
        } else {
          std::cerr << "Invalid argument: Not a valid integer.\n";
        }
        break;
      }
      // break rule <RULE>
      case DebugCommand::BREAK_RULE: {
        if (opt_argc == 0) {
          std::cerr << "Missing target rule name.\n";
        } else {
          set_breakpoint_rule(opt_argv.at(0));
        }
        break;
      }
      // break instruction <INSTR>
      case DebugCommand::BREAK_INSTR: {
        if (opt_argc == 0) {
          std::cerr << "Missing target instruction name.\n";
        } else {
          set_breakpoint_instr(opt_argv.at(0));
        }
        break;
      }
      // delete rule <RULE>
      case DebugCommand::DELETE_RULE: {
        if (opt_argc == 0) {
          std::cerr << "Missing target rule name.\n";
        } else {
          delete_breakpoint_rule(opt_argv.at(0));
        }
        break;
      }
      // delete instruction <INSTR>
      case DebugCommand::DELETE_INSTR: {
        if (opt_argc == 0) {
          std::cerr << "Missing target instruction name.\n";
        } else {
          delete_breakpoint_instr(opt_argv.at(0));
        }
        break;
      }
      case DebugCommand::FINISH: {
        if (*(LmnInstrOp*)instr == INSTR_PROCEED) {
          std::cerr << "Already at the end of current rule.\n";
        } else {
          finish_current_rule = true;
          continue_session = false;
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
        std::cerr << "Unknown command.\n";
        break;
      }
      case DebugCommand::START:
      case DebugCommand::RUN: {
        std::cerr << "Program is already running.\n";
        break;
      }
      default: {
        std::cerr << "Not implemented.\n";
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
      printf("\nBreakpoint on instruction \"%s\" hit.\n", slim::debug_printer::to_string_instrop(instr_op).c_str());
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
    for (auto &s : lines) {
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
  if (screen_height == -1 || screen_width == -1) {
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
    if (lines.size() > 0 && lines.back().empty()) {
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
    if (c == EOF) { // eof may not be returned
      input_eof = true;
      feeding = false;
    } else if (c == 'q') {
      feeding = false;
      std::cout << "\e[2K\e[0G";
    } else if (c == '\n' || c == 'j') {
      new_line += 1;
    } else if (c == 'd') {
      new_line += screen_height / 2;
    } else if (c == 'u') {
      if (new_line >= screen_height / 2) {
        new_line -= screen_height / 2;
      } else {
        new_line = 0;
      }
    } else if (c == 'k' && new_line >= 1) {
      new_line -= 1;
    } else if (c == ' ') {
      new_line += (screen_height - 1);
    } else if (c == '\e') {
      esc_pressed = true;
    } else if (c == 'A' && esc_pressed) { // up arrow
      if (new_line >= 1) {
        new_line -= 1;
      }
      esc_pressed = false;
    } else if (c == 'B' && esc_pressed) { // down arrow
      new_line += 1;
      esc_pressed = false;
    } else if (c == 'H' && esc_pressed) { // home
      new_line = 0;
      esc_pressed = false;
    } else if (c == 'F' && esc_pressed) { // end
      new_line = max_line;
      esc_pressed = false;
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
