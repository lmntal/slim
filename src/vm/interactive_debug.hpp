#ifndef LMN_INTERACTIVE_DEBUG_H
#define LMN_INTERACTIVE_DEBUG_H

#include "lmntal.h"
#include "react_context.hpp"
#include "atomlist.hpp"
#include "rule.h"
#include "rule.hpp"

class InteractiveDebugger {
  private:
    InteractiveDebugger() = default;
    ~InteractiveDebugger() = default;
    
    std::vector<LmnInstruction> breakpoints_on_instr;
    std::vector<std::string> breakpoints_on_rule;

    LmnRuleInstr previous_instr = nullptr;
    LmnRuleRef previous_rule = nullptr;

    long rule_reaction_count = 0;
    long rule_reaction_stop_at = -1;
    long instr_execution_count = 0;
    long instr_execution_stop_at = -1;

    bool input_eof = false;

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

    const std::map<std::string, DebugCommand> debug_commands = {
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

    const std::map<std::string, DebugCommandArg> debug_command_args = {
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

  std::vector<std::string> split_command(std::string);
  std::string stringify_atom(const LmnAtomRef atom, const LmnByte at);
  std::string stringify_register(const LmnRegister *reg);
  std::string stringify_regarray(const LmnRegisterArray *reg_array);
  std::string stringify_atomlist(const AtomListEntry *atomlist);
  std::string stringify_instr(const LmnRuleInstr instr);

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

// get name from id
const char* get_instr_name(const int id);

#endif
