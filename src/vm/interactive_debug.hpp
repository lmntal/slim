#ifndef LMN_INTERACTIVE_DEBUG_H
#define LMN_INTERACTIVE_DEBUG_H

#include "atomlist.hpp"
#include "interpret/interpreter.hpp"
#include "lmntal.h"
#include "react_context.hpp"
#include "rule.h"
#include "rule.hpp"

class InteractiveDebugger {
private:
  InteractiveDebugger();
  ~InteractiveDebugger();

  // for managing breakpoints
  std::vector<LmnInstruction> breakpoints_on_instr;
  std::vector<std::string>    breakpoints_on_rule;

  // for outputs
  LmnRuleInstr previous_instr = nullptr;
  LmnRuleRef   previous_rule  = nullptr;

  // for step execution
  size_t rule_reaction_count     = 0;
  size_t rule_reaction_stop_at   = 0;
  size_t instr_execution_count   = 0;
  size_t instr_execution_stop_at = 0;

  // for managing inputs
  bool input_eof = false;

  // for feeding outputs
  int  screen_height = -1;
  int  screen_width  = -1;
  void print_feeding(std::string str);

  // for non-deterministic execution
  StateSpaceRef statespace      = nullptr;
  State        *expanding_state = nullptr;

  // for other controls
  bool break_on_entry      = false;
  bool finish_current_rule = false;

  void reset_step_execution() {
    rule_reaction_count     = 0;
    rule_reaction_stop_at   = 0;
    instr_execution_count   = 0;
    instr_execution_stop_at = 0;
  }

  void set_breakpoint_rule(std::string rule);
  void set_breakpoint_instr(std::string instr);
  void delete_breakpoint_rule(std::string rule);
  void delete_breakpoint_instr(std::string instr);
  void list_breakpoints();

public:
  InteractiveDebugger(InteractiveDebugger const &)            = delete;
  InteractiveDebugger &operator=(InteractiveDebugger const &) = delete;
  InteractiveDebugger(InteractiveDebugger &&)                 = delete;
  InteractiveDebugger &operator=(InteractiveDebugger &&)      = delete;

  void start_session_on_entry();
  void start_session_with_interpreter(slim::vm::interpreter const *interpreter);
  void break_on_instruction(slim::vm::interpreter const *interpreter);
  void break_on_rule(slim::vm::interpreter const *interpreter);
  void finish_debugging();

  static InteractiveDebugger &get_instance() {
    static InteractiveDebugger instance;
    return instance;
  }

  void register_statespace(StateSpaceRef ref) { statespace = ref; }

  void register_expanding_state(State *state) { expanding_state = state; }
};

#endif
