/*
 * state_dumper.cpp
 *
 *   Copyright (c) 2029, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "state_dumper.h"

#include "fmt/core.h"

#include "statespace.h"

namespace state_dumper {
class CUI : public StateDumper {
  friend StateDumper;
  using StateDumper::StateDumper;

  MCdumpFormat dump_format() const override { return MCdumpFormat::CUI; }
  bool         need_id_foreach_trans() const override { return false; }
  /* なぜかspaceが混ざるとlavitは読み込むことができない */
  std::string state_separator() const override { return "::"; }
  std::string trans_separator() const override { return ","; }
  std::string label_begin() const override { return "("; }
  std::string label_end() const override { return ")"; }

  void dump(StateSpace *ss) override;
  void dump_state_data(State *s, unsigned long print_id, StateSpace *owner) override;
  void print_state_label(State *s, StateSpace *owner) override;
};

class LaViT : public StateDumper {
  friend StateDumper;
  using StateDumper::StateDumper;
  MCdumpFormat dump_format() const override { return MCdumpFormat::LaViT; }
  bool         need_id_foreach_trans() const override { return false; }
  /* なぜかspaceが混ざるとlavitは読み込むことができない */
  std::string state_separator() const override { return "::"; }
  std::string trans_separator() const override { return ","; }
  std::string label_begin() const override { return "("; }
  std::string label_end() const override { return ")"; }

  void dump(StateSpace *ss) override;
  void dump_state_data(State *s, unsigned long print_id, StateSpace *owner) override;
  void print_state_label(State *s, StateSpace *owner) override;
  void print_mem(LmnMembrane *mem) override;
};

class Dir_DOT : public StateDumper {
  friend StateDumper;
  using StateDumper::StateDumper;
  MCdumpFormat dump_format() const override { return MCdumpFormat::Dir_DOT; }
  bool         need_id_foreach_trans() const override { return true; }
  std::string  state_separator() const override { return " -> "; }
  std::string  trans_separator() const override { return ""; }
  std::string  label_begin() const override { return " [ label = \""; }
  std::string  label_end() const override { return "\" ];"; }

  void dump(StateSpace *ss) override;
  void dump_state_data(State *s, unsigned long print_id, StateSpace *owner) override;
  void print_state_label(State *s, StateSpace *owner) override;
};

class LMN_FSM_GRAPH_MEM_NODE : public StateDumper {
  friend StateDumper;
  using StateDumper::StateDumper;
  MCdumpFormat dump_format() const override { return MCdumpFormat::LMN_FSM_GRAPH_MEM_NODE; }
  bool         need_id_foreach_trans() const override { return true; }
  std::string  state_separator() const override { return ""; }
  std::string  trans_separator() const override { return ""; }
  std::string  label_begin() const override { return ""; }
  std::string  label_end() const override { return ""; }

  void         dump(StateSpace *ss) override;
  virtual void dump(State *s) {}
};

class LMN_FSM_GRAPH : public LMN_FSM_GRAPH_MEM_NODE {
  friend StateDumper;
  using LMN_FSM_GRAPH_MEM_NODE::LMN_FSM_GRAPH_MEM_NODE;
  void dump(State *s) override;
};

class LMN_FSM_GRAPH_HL_NODE : public StateDumper {
  friend StateDumper;
  using StateDumper::StateDumper;
  MCdumpFormat dump_format() const override { return MCdumpFormat::LMN_FSM_GRAPH_HL_NODE; }
  bool         need_id_foreach_trans() const override { return true; }
  std::string  state_separator() const override { return ""; }
  std::string  trans_separator() const override { return ""; }
  std::string  label_begin() const override { return ""; }
  std::string  label_end() const override { return ""; }

  void dump(StateSpace *ss) override;
};
} // namespace state_dumper

StateDumper *StateDumper::from_env_ptr(FILE *fp) {
  switch (lmn_env.mc_dump_format) {
  case MCdumpFormat::CUI:
    return new state_dumper::CUI(fp);
  case MCdumpFormat::LaViT:
    return new state_dumper::LaViT(fp);
  case MCdumpFormat::Dir_DOT:
    return new state_dumper::Dir_DOT(fp);
  case MCdumpFormat::LMN_FSM_GRAPH:
    return new state_dumper::LMN_FSM_GRAPH(fp);
  case MCdumpFormat::LMN_FSM_GRAPH_MEM_NODE:
    return new state_dumper::LMN_FSM_GRAPH_MEM_NODE(fp);
  case MCdumpFormat::LMN_FSM_GRAPH_HL_NODE:
    return new state_dumper::LMN_FSM_GRAPH_HL_NODE(fp);
  default:
    throw std::runtime_error("invalid mc dump format.");
  }
}

namespace state_dumper {
void CUI::dump(StateSpace *ss) {
  auto states = ss->all_states();
  if (lmn_env.sp_dump_format != INCREMENTAL) {
    fprintf(_fp, "States\n");
    for (auto &s : states)
      this->StateDumper::dump(s, ss);
  }
  fprintf(_fp, "\n");
  fprintf(_fp, "Transitions\n");
  fprintf(_fp, "init:%lu\n", state_format_id(ss->initial_state(), ss->is_formatted()));
  for (auto &s : states)
    this->state_print_transition(s, ss);
  fprintf(_fp, "\n");
}

void Dir_DOT::dump(StateSpace *ss) {
  fprintf(_fp, "digraph StateTransition {\n");
  fprintf(_fp, "  node [shape = circle];\n");
  fprintf(_fp, "  %lu [style=filled, color = \"#ADD8E6\", shape = Msquare];\n",
          state_format_id(ss->initial_state(), ss->is_formatted()));
  auto states = ss->all_states();
  for (auto &s : states)
    this->StateDumper::dump(s, ss);
  for (auto &s : states)
    this->state_print_transition(s, ss);
  for (auto &s : states)
    this->state_print_label(s, ss);
  fprintf(_fp, "}\n");
}

void LaViT::dump(StateSpace *ss) {
  auto states = ss->all_states();
  if (lmn_env.sp_dump_format != INCREMENTAL) {
    fprintf(_fp, "States\n");
    for (auto &s : states)
      this->StateDumper::dump(s, ss);
  }
  fprintf(_fp, "\n");
  fprintf(_fp, "Transitions\n");
  fprintf(_fp, "init:%lu\n", state_format_id(ss->initial_state(), ss->is_formatted()));
  for (auto &s : states)
    this->state_print_transition(s, ss);
  fprintf(_fp, "\n");

  if (ss->has_property()) {
    fprintf(_fp, "Labels\n");
    for (auto &s : states)
      this->state_print_label(s, ss);
    fprintf(_fp, "\n");
  }
}

void LMN_FSM_GRAPH_MEM_NODE::dump(StateSpace *ss) {
  auto states      = ss->all_states();
  auto predecessor = ss->predecessor();

  auto formatted = ss ? ss->is_formatted() : FALSE;
  for (auto &s : states) {
    if (s->is_dummy() && !s->is_encoded())
      continue;

    auto src_id = state_format_id(s, formatted);
    fprintf(_fp, "{");

    dump(s);

    if (s->successors) {
      for (int i = 0; i < s->successor_num; i++) {
        auto dst_id = state_format_id(state_succ_state(s, i), formatted);
        if (i > 0)
          fprintf(_fp, ",");
        fprintf(_fp, "+L%lu_%lu", src_id, dst_id);
      }
    }

    auto &ps = predecessor[s];

    if (s->successors && s->successor_num > 0 && !ps.empty())
      fprintf(_fp, ",");

    for (int i = 0; i < ps.size(); i++) {
      auto dst_id = state_format_id(ps[i], formatted);
      if (i > 0)
        fprintf(_fp, ",");
      fprintf(_fp, "-L%lu_%lu", dst_id, src_id);
    }

    fprintf(_fp, "}.\n");
  }
  fprintf(_fp, "\n");
}

void LMN_FSM_GRAPH::dump(State *s) {
  auto org_next_id = env_next_id();
  auto mem         = s->restore_membrane_inner(FALSE);
  env_set_next_id(org_next_id);

  print_mem(mem);
  fprintf(_fp, ". ");

  if (s->is_binstr_user()) {
    mem->free_rec();
  }
}

void LMN_FSM_GRAPH_HL_NODE::dump(StateSpace *ss) {
  auto states    = ss->all_states();
  auto formatted = ss ? ss->is_formatted() : FALSE;
  for (auto &s : states) {
    if (s->is_dummy() && !s->is_encoded())
      continue;

    auto src_id = state_format_id(s, formatted);

    if (s->successors) {
      for (int i = 0; i < s->successor_num; i++) {
        auto dst_id = state_format_id(state_succ_state(s, i), formatted);
        if (i > 0)
          fprintf(_fp, ",");
        fprintf(_fp, "to(!H%lu,!H%lu)", src_id, dst_id);
      }
      if (s->successor_num > 0 && s != states.back())
        fprintf(_fp, ",\n");
    }
  }
  fprintf(_fp, ".\n");
}
} // namespace state_dumper

/** Printer
 * ownerはNULLでもok */
void StateDumper::dump(State *s, StateSpace const *_owner) {
  /* Rehashが発生している場合,
   * dummyフラグが真かつエンコード済みフラグが偽のStateオブジェクトが存在する.
   * このようなStateオブジェクトのバイナリストリングは
   * Rehashされた側のテーブルに存在するStateオブジェクトに登録されているためcontinueする.
   */
  if (s->is_dummy() && !s->is_encoded())
    return;

  auto owner = (StateSpaceRef)_owner;
  /* この時点で状態は, ノーマル || (dummyフラグが立っている &&
   * エンコード済)である. dummyならば,
   * バイナリストリング以外のデータはオリジナル側(parent)に記録している. */
  State *target       = !s->is_dummy() ? s : state_get_parent(s);
  auto   is_formatted = (owner) ? owner->is_formatted() : false;
  auto   print_id     = state_format_id(target, is_formatted);
  dump_state_data(s, print_id, owner);
}

void StateDumper::print_mem(LmnMembrane *mem) {
  fprintf(_fp, "%s\n", slim::to_string_membrane(mem).c_str());
  if (lmn_env.show_hyperlink)
    lmn_hyperlink_print(_fp, mem);
}

void StateDumper::state_print_mem(State *s) {
  auto org_next_id = env_next_id();
  auto mem         = s->restore_membrane_inner(FALSE);
  env_set_next_id(org_next_id);

  print_mem(mem);

  if (s->is_binstr_user()) {
    mem->free_rec();
  }
}

/* TODO: 美しさ */
void StateDumper::state_print_transition(State *s, StateSpace const *_owner) {
  /* Rehashが発生している場合,
   * サクセッサへの情報は,
   * RehashしたオリジナルのStateオブジェクトが保持しているため,
   * dummyフラグが真かつエンコード済みの状態には遷移情報は載っていない.
   * (エンコード済のバイナリストリングしか載っていない) */
  if ((s->is_dummy() && s->is_encoded()))
    return;

  auto owner    = (StateSpaceRef)_owner;
  auto formated = owner ? owner->is_formatted() : FALSE;
  if (!need_id_foreach_trans()) {
    fprintf(_fp, "%lu%s", state_format_id(s, formated), state_separator().c_str());
  }

  if (s->successors) {
    for (int i = 0; i < s->successor_num; i++) { /* dump dst state's IDs */
      if (need_id_foreach_trans()) {
        fprintf(_fp, "%lu%s", state_format_id(s, formated), state_separator().c_str());
      } else if (i > 0) {
        fprintf(_fp, "%s", trans_separator().c_str());
      }

      /* MEMO: rehashが発生していても, successorポインタを辿る先は, オリジナル
       */
      fprintf(_fp, "%lu", state_format_id(state_succ_state(s, i), formated));

      if (s->has_trans_obj()) {
        fprintf(_fp, "%s", label_begin().c_str());
        auto t = transition(s, i);

        for (int j = 0; j < transition_rule_num(t); j++) {
          if (j > 0)
            fprintf(_fp, " "); /* ルール名の区切りは半角スペース1文字 */
          fprintf(_fp, "%s", lmn_id_to_name(transition_rule(t, j)));
        }
        fprintf(_fp, "%s", label_end().c_str());
      }

      if (i + 1 < s->successor_num && need_id_foreach_trans()) {
        fprintf(_fp, "\n");
      }
    }
    fprintf(_fp, "\n");
  } else if (!need_id_foreach_trans()) {
    fprintf(_fp, "\n");
  }
}

namespace state_dumper {
void CUI::dump_state_data(State *s, unsigned long print_id, StateSpace *owner) {
  BOOL has_property = owner && owner->has_property();
  fprintf(_fp, "%lu::", print_id);
#ifdef KWBT_OPT
  fprintf(_fp, "%lu::", cost);
#endif
  fmt::print(_fp, "{}", has_property ? owner->automata()->state_name(state_property_state(s)) : "");
  state_print_mem(s);
}

void CUI::print_state_label(State *s, StateSpace *owner) { /* 状態のグローバルルート膜の膜名としてdump済 */
}

void LaViT::dump_state_data(State *s, unsigned long print_id, StateSpace *owner) {
  fprintf(_fp, "%lu::", print_id);
  state_print_mem(s);
}

void LaViT::print_state_label(State *s, StateSpace *owner) {
  auto *a = owner->automata();
  fmt::print(_fp, "{}::", state_format_id(s, owner->is_formatted()));
  fmt::print(_fp, "{}\n", a->state_name(state_property_state(s)));
}

void LaViT::print_mem(LmnMembrane *mem) { fprintf(_fp, "%s\n", slim::to_string(mem).c_str()); }

void Dir_DOT::dump_state_data(State *s, unsigned long print_id, StateSpace *owner) {
  if (s->successor_num == 0) {
    fprintf(_fp, "  %lu [style=filled, fillcolor = \"#C71585\", shape = Msquare];\n", print_id);
  }
}

void Dir_DOT::print_state_label(State *s, StateSpace *owner) {
  auto a = owner->automata();
  if (state_is_accept(a, s) || state_is_end(a, s)) {
    fprintf(_fp, "  %lu [peripheries = 2]\n", state_format_id(s, owner->is_formatted()));
  }
}
} // namespace state_dumper

void StateDumper::state_print_label(State *s, StateSpace const *_owner) {
  auto owner = (StateSpaceRef)_owner;
  if (!owner->has_property() || (s->is_dummy() && s->is_encoded())) {
    return;
  }

  print_state_label(s, owner);
}
