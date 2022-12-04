#ifndef LMN_INTERACTIVE_DEBUG_H
#define LMN_INTERACTIVE_DEBUG_H

#include "lmntal.h"
#include "react_context.hpp"
#include "atomlist.hpp"
#include "rule.h"
#include "rule.hpp"

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

  // for managing inputs
  bool input_eof = false;

  // for outputs
  const std::string invalid_arg_message = "Invalid argument";
  const std::string invalid_args_message = "Invalid arguments";
  const std::string few_arg_message = "Too less arguments";

  // for feeding outputs
  int screen_height;
  int screen_width;
  void print_feeding(const std::string &str);

public:
  InteractiveDebugger(const InteractiveDebugger&) = delete;
  InteractiveDebugger& operator=(const InteractiveDebugger&) = delete;
  InteractiveDebugger(InteractiveDebugger&&) = delete;
  InteractiveDebugger& operator=(InteractiveDebugger&&) = delete;

  void start_session(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr);
  void break_on_instruction(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr);
  void break_on_rule(const LmnReactCxtRef rc, const LmnRuleRef rule, const LmnRuleInstr instr);
  void finish_debugging(void);

  static InteractiveDebugger& get_instance() {
    static InteractiveDebugger instance;
    return instance;
  }
};

#endif
