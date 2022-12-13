#ifndef LMN_INTERACTIVE_DEBUG_H
#define LMN_INTERACTIVE_DEBUG_H

#include "lmntal.h"
#include "react_context.hpp"
#include "atomlist.hpp"
#include "rule.h"
#include "rule.hpp"
#include "interpret/interpreter.hpp"

// get name from id
const char* get_instr_name(int id);

class InteractiveDebugger {
private:
  InteractiveDebugger();
  ~InteractiveDebugger();

  // for managing breakpoints
  std::vector<LmnInstruction> breakpoints_on_instr;
  std::vector<std::string> breakpoints_on_rule;

  // for outputs
  LmnRuleInstr previous_instr = nullptr;
  LmnRuleRef previous_rule = nullptr;

  // for step execution
  long rule_reaction_count = 0;
  long rule_reaction_stop_at = -1;
  long instr_execution_count = 0;
  long instr_execution_stop_at = -1;
  bool finish_current_rule = false;

  // for managing inputs
  bool input_eof = false;

  // for feeding outputs
  int screen_height = -1;
  int screen_width = -1;
  void print_feeding(const std::string &str);

  // for non-deterministic execution
  StateSpaceRef statespace = nullptr;

public:
  InteractiveDebugger(const InteractiveDebugger&) = delete;
  InteractiveDebugger& operator=(const InteractiveDebugger&) = delete;
  InteractiveDebugger(InteractiveDebugger&&) = delete;
  InteractiveDebugger& operator=(InteractiveDebugger&&) = delete;

  void start_session_on_entry();
  void start_session_with_interpreter(const slim::vm::interpreter *interpreter);
  void break_on_instruction(const slim::vm::interpreter *interpreter);
  void break_on_rule(const slim::vm::interpreter *interpreter);
  void finish_debugging();

  static InteractiveDebugger& get_instance() {
    static InteractiveDebugger instance;
    return instance;
  }

  void register_statespace(StateSpaceRef ref);
};

#endif
