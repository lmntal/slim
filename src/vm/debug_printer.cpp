#include "debug_printer.hpp"
#include "task.h"
#include "atomlist.hpp"

#include <iostream>

namespace slim {
namespace debug_printer {

using link_name_table = std::map<int,std::string>;

struct PrintRecord {
  bool done;
  link_name_table table;
};

using dump_history_table = std::map<LmnSymbolAtomRef,PrintRecord>;

constexpr size_t MAX_CALL_DEPTH = 1000;
constexpr char LINK_PREFIX[] = "L";

static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom);
static int get_overall_links(dump_history_table &table);

static std::string __to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth);
static std::string __to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht);
static std::string __to_string_satom_arg(const LmnSymbolAtomRef atom, int arg_index, dump_history_table &ht, size_t call_depth);
static std::string __to_string_satom_args(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth);
static bool __is_direct_printable(LmnFunctor f);
static std::string __to_string_atom_name(const LmnSymbolAtomRef atom);
static std::string __to_string_atom_name(LmnFunctor functor);
static std::string __to_string_list(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth);
static std::string __to_string_satom(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht, size_t call_depth);
std::string __to_string_datom(const LmnAtomRef data, LmnLinkAttr attr);
std::string __to_string_mem(const LmnMembraneRef mem, dump_history_table &ht);
std::string __to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr);

// テーブルから PrintRecord の参照を取得
static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom) {
  auto ent = table.find(atom);
  if (ent == table.end()) { // not found
    auto iter = table.emplace(atom, PrintRecord({false,{}}));
    return (*(iter.first)).second;
  } else { // found
    return (*ent).second;
  }
}

// テーブル内に保存されたリンク情報の合計数
static int get_overall_links(dump_history_table &table) {
  int links = 0;
  for (auto &p : table) {
    links += p.second.table.size();
  }
  return links;
}

// アトムを表示
std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  dump_history_table ht;
  return __to_string_atom(atom, attr, ht, 0);
}

// アトムを表示。データアトムならデータのみを表示。
static std::string __to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return __to_string_datom(atom, attr);
  } else {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    LmnFunctor f = satom->get_functor(); // currently ignore functor
    // LMN_ATTR_GET_VALUE(attr); // 接続先リンクの番号
    return __to_string_satom(satom, LMN_ATTR_GET_VALUE(attr), ht, call_depth);
  }
}

// シンボルアトムのリンクのうち1つをリンクとして表示。すでにリンクに名前がついていればその名前を使用。
static std::string __to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht) {
  PrintRecord &record = get_history_entry(ht, atom);

  auto res = record.table.find(link_position);

  if (res == record.table.end()) { // not found
    std::string name = LINK_PREFIX + std::to_string(get_overall_links(ht));
    record.table.emplace(link_position, name);
    return name;
  } else { // found
    return (*res).second;
  }
}

// シンボルアトムのリンクのうち1つを表示。すでにリンクに名前がついていればリンクとして表示，そうでなければアトムとして表示することを試みる。
static std::string __to_string_satom_arg(const LmnSymbolAtomRef atom, int arg_index, dump_history_table &ht, size_t call_depth) {
  PrintRecord &src_record = get_history_entry(ht, atom);

  LmnAtomRef linked_atom = atom->get_link(arg_index);
  LmnLinkAttr link_attr = atom->get_attr(arg_index);

  if (src_record.table.find(arg_index) != src_record.table.end()) { // found
    return __to_string_link(atom, arg_index, ht);
  } else { // not found
    return __to_string_atom(linked_atom, link_attr, ht, call_depth);
  }
}

// アトムのリンク全体を表示。リンクがなければ丸括弧は表示しない。
static std::string __to_string_satom_args(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth) {
  int link_num = atom->get_link_num();

  if (call_depth > 0) {
    link_num--;
  }

  if (link_num == 0) { // if atom has no links, do not show parenthes
    return "";
  } else {
    std::string retVal = "";
    retVal += "(";

    for (int i = 0; i < link_num; i++) {
      if (i > 0) {
        retVal += ",";
      }
      retVal += __to_string_satom_arg(atom, i, ht, call_depth + 1);
    }

    retVal += ")";
    return retVal;
  }
}

// シンボルアトム'.'をリスト用の表記を用いて表示
static std::string __to_string_list(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth) {
  if (get_history_entry(ht, atom).done) { // 出力済み
    return __to_string_link(atom, 2, ht);
  }

  LmnSymbolAtomRef a = atom;
  LmnSymbolAtomRef prev_a;
  LmnLinkAttr attr = LMN_ATTR_MAKE_LINK(2);
  bool first = true;
  std::string retVal = "[";

  while (true) {
    if (LMN_HAS_FUNCTOR(a, attr, LMN_LIST_FUNCTOR) && LMN_ATTR_GET_VALUE(attr) == 2) {
      PrintRecord &record = get_history_entry(ht, a);
      if (record.done) {
        retVal += "|";
        std::string name = LINK_PREFIX + std::to_string(get_overall_links(ht));
        record.table.emplace(LMN_ATTR_GET_VALUE(attr), name);
        retVal += name;
        break;
      }
      if (!first) {
        retVal += ",";
      }
      first = false;
      record.done = true;
      retVal += __to_string_satom_arg(a, 0, ht, call_depth + 1);
      attr = a->get_attr(1);
      prev_a = a;
      a = (LmnSymbolAtomRef)a->get_link(1);
    } else if (LMN_HAS_FUNCTOR(a, attr, LMN_NIL_FUNCTOR)) { // list end ([])
      PrintRecord record;
      record.done = true;
      ht.emplace(a, record);
      break;
    } else {
      retVal += "|";
      retVal += __to_string_satom_arg(prev_a, 1, ht, call_depth + 1);
      break;
    }
  }

  retVal += "]";
  return retVal;
}

/** シンボルアトムを表示。
  * ダンプの主な対象でなく，元のアトムと最終リンク以外で繋がっているなら，リンクとして表示。
  * ダンプの主な対象でなく，すでにダンプされているなら，リンクとして表示。
  * シンボルアトムの表示のための再帰呼び出しの最大の深さを超えたら，リンクとして表示。
  * ダンプ済みなら何も表示しない。
  * リストならリストとして表示。
  * それ以外ならアトムとして表示。
  */
static std::string __to_string_satom(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht, size_t call_depth) {
  LmnFunctor functor = atom->get_functor();
  LmnArity arity = atom->get_arity();

  PrintRecord& record = get_history_entry(ht, atom);

  if (
    (call_depth > 0 && link_position != arity - 1) || // ダンプの主な対象ではなく，このアトムに最終リンク以外で繋がっている
    (call_depth > 0 && record.done) || // ダンプの主な対象ではなく，すでにダンプされた
    (call_depth > MAX_CALL_DEPTH) // 再帰呼び出しの最大の深さを超えた
  ) {
    return __to_string_link(atom, link_position, ht);
  }

  //if (record.done) { // ダンプ済み
   // return "";
  //}

  // リストならリスト用の表示
  if (/* call_depth == 0 &&  */functor == LMN_LIST_FUNCTOR) {
    return __to_string_list(atom, ht, call_depth) + "=" + __to_string_satom_arg(atom, 2, ht, call_depth + 1);
  }

  record.done = true;

  return __to_string_atom_name(functor) + __to_string_satom_args(atom, ht, call_depth);
}

// 名前が /[a-z][a-zA-Z_]*/ ならtrue
static bool __is_direct_printable(LmnFunctor f) {
  if (LMN_IS_PROXY_FUNCTOR(f) || f == LMN_NIL_FUNCTOR) {
    return true;
  }
  std::string s = LMN_FUNCTOR_STR(f);

  // 1文字目
  if (!isalpha(s[0]) || !islower(s[0])) {
    return false;
  }
  // 2文字目以降
  return std::all_of(s.cbegin() + 1, s.cend(),
    [](char c) -> bool { return isalpha(c) || isdigit(c) || c == '_'; }
  );
}

// 適切にクォートされたアトムの名前を表示。
static std::string __to_string_atom_name(const LmnSymbolAtomRef atom) {
  return __to_string_atom_name(atom->get_functor());
}

// 適切にクォートされたアトムの名前を表示。
static std::string __to_string_atom_name(LmnFunctor functor) {
  std::string str = LMN_FUNCTOR_STR(functor);
  return __is_direct_printable(functor) ? str : "'" + str + "'";
}

// データアトムを表示
std::string __to_string_datom(const LmnAtomRef data, LmnLinkAttr attr) {
  switch (attr) {
  case LMN_INT_ATTR:
    return std::to_string((long)data);
  case LMN_DBL_ATTR:
    return std::to_string(lmn_get_double((LmnDataAtomRef)data));
  case LMN_SP_ATOM_ATTR: { // string etc...
    LmnPortRef port = lmn_make_output_string_port();
    SP_ATOM_DUMP(data, port);
    LmnStringRef str = lmn_port_output_string(port);
    lmn_port_free(port);
    return str->str;
  }
  default:
    return "[data(" + to_hex_string(data) + "), attr(" + to_hex_string(data) + ")]";
  }
}

// レジスタの内容を表示
std::string to_string_reg(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  switch (reg->tt) {
    case TT_ATOM:
      return to_string_atom((LmnAtomRef)reg->wt, reg->at);
    case TT_MEM:
      return to_hex_string(reg->wt) + "(" + std::to_string(reg->at) + ",mem)";
    case TT_OTHER:
      return to_hex_string(reg->wt) + "(" + std::to_string(reg->at) + ",other)";
    default:
      return "unknown tt type";
  }
}

// レジスタ配列を表示
std::string to_string_regarray(const LmnRegisterArray* reg_array) {
  if (reg_array == nullptr) {
    return "null";
  }

  std::string retVal = reg_array_prefix;
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    if (i > 0) {
      retVal += reg_array_delimiter;
    }
    retVal += to_string_reg((LmnRegisterRef) &(reg_array->at(i)));
  }
  retVal += reg_array_suffix;

  return retVal;
}

// 命令名を表示
std::string to_string_instrop(LmnInstrOp op) {
  for (auto &p : instr_spec) {
    if (op == p.first) {
      return p.second.op_str;
    }
  }
  return "unknown";
}

// 命令名と引数を表示
std::string to_string_instr(const LmnRuleInstr instr) {
  if (instr == nullptr) {
    return "null";
  }

  LmnRuleInstr instr_copy = instr;
  LmnInstrOp op;
  READ_VAL(LmnInstrOp, instr_copy, op);

  std::vector<ArgType> argtype_vec;

  for (auto &p : instr_spec) {
    if (op == p.first) {
      argtype_vec = p.second.args;
      break;
    }
  }

  if (argtype_vec.empty()) {
    return "unknown";
  }

  std::ostringstream retVal;
  retVal << to_string_instrop(op) << " ";

  for (size_t i = 0, argSize = argtype_vec.size(); i < argSize; i++) {
    if (i > 0) {
      retVal << ", ";
    }

    ArgType argType = argtype_vec.at(i);
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
  }

  if (op == INSTR_FINDATOM || op == INSTR_FINDATOM2 || op == INSTR_FINDATOMP || op == INSTR_NEWATOM) {
    LmnFunctor func;
    READ_VAL(LmnFunctor, instr_copy, func);
    retVal << ", ";
    retVal << to_string_functor(func);
  }

  return retVal.str();
}

// アトムリストを表示
std::string to_string_atomlist(const AtomListEntry* atomlist) {
  if(atomlist == nullptr) {
    return "null";
  }

  auto begin = std::begin(*atomlist);
  auto end = std::end(*atomlist);
  dump_history_table ht;

  std::string retVal = atomlist_prefix;
  while (begin != end) {
    // TODO check if attribute is appropriate
    retVal += to_string_atom(*begin, LMN_ATTR_MAKE_LINK(0));
    // retVal += __to_string_atom(*begin, LMN_ATTR_MAKE_LINK(0), ht, 0);
    ++begin;
    if (begin != end) {
      retVal += atomlist_delimiter;
    }
  }
  retVal += atomlist_suffix;

  return retVal;
}

// ファンクタを アトム名_価数 の表現で表示
std::string to_string_functor(LmnFunctor func) {
  if (lmn_functor_table == nullptr) {
    return "Functor table is null.\n";
  }
  auto entry = lmn_functor_table->get_entry(func);
  return std::string(lmn_id_to_name(entry->name)) + "_" + std::to_string(entry->arity);
}

// 膜を表示
std::string __to_string_mem(const LmnMembraneRef mem, dump_history_table &ht) {
  enum { P0, P1, P2, P3, PROXY, PRI_NUM };
  std::array<std::vector<LmnSymbolAtomRef>,PRI_NUM> pred_atoms;

  // アトムリストごとに処理
  for (auto &pair : mem->atom_lists()) {
    LmnFunctor functor = pair.first;
    AtomListEntryRef atomlist = pair.second;

    for (auto &atom : *atomlist) {
      int arity = atom->get_arity();
      if (atom->get_functor() == LMN_RESUME_FUNCTOR) { // ?
        continue;
      }
      if (functor == LMN_IN_PROXY_FUNCTOR || functor == LMN_OUT_PROXY_FUNCTOR) {
        pred_atoms[PROXY].push_back(atom);
      }
      else if (arity == 0 && !atom->record_flag) {
        pred_atoms[P0].push_back(atom);
      }
      else if (arity == 1 && functor != LMN_NIL_FUNCTOR &&
              (LMN_ATTR_IS_DATA(atom->get_attr(0)) ||
               (int)LMN_ATTR_GET_VALUE(atom->get_attr(0)) == ((LmnSymbolAtomRef)atom->get_link(0))->get_arity() - 1))
      {
        pred_atoms[P1].push_back(atom);
      }
      else {
        if (arity > 1 && (
          (LMN_ATTR_IS_DATA(atom->get_attr(arity - 1)) ||
           (int)LMN_ATTR_GET_VALUE(atom->get_attr(arity - 1) == ((LmnSymbolAtomRef)atom->get_link(arity - 1))->get_arity() - 1))
        )) {
          pred_atoms[P2].push_back(atom);
        } else {
          pred_atoms[P3].push_back(atom);
        }
      }
    }
  }

  std::string retVal = "{";
  for (auto &v : pred_atoms) {
    for (auto &atom : v) {
      // TODO complete
      std::string s = __to_string_satom(atom, 0, ht, 0);
      retVal += s;
      if (!s.empty()) {
        retVal += ". ";
      }
    }
  }

  // 子
  bool dumped = false;
  for (LmnMembraneRef m = mem->mem_child_head(); m != nullptr; m = m->mem_next()) {
    // TODO complete
    std::string s = __to_string_mem(m, ht);
    retVal += s;
    dumped = !s.empty();
    if (dumped && m->mem_next() != nullptr) {
      retVal += ", ";
    }
  }
  if (dumped) {
    retVal += ". ";
  }

  retVal += "}";
  return retVal;
}

// 膜を表示
std::string to_string_mem(const LmnMembraneRef mem) {
  if (mem == nullptr) {
    return "null";
  }
  dump_history_table ht;
  return __to_string_mem(mem, ht);
}

// データアトムならデータと属性を，シンボルアトムならアドレス・ID・ファンクタ・価数・接続先リンクの番号を表示
std::string __to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  std::string retVal = "";
  retVal += __ESC_START__ + std::to_string(CODE__FORECOLOR_LIGHTBLUE) + __ESC_END__;
  if (LMN_ATTR_IS_DATA(attr)) {
    retVal += "Data[" + to_hex_string(atom) + "], ";
    retVal += "Attr[" + to_hex_string(attr) + "]";
  } else {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    retVal += "Addr[" + to_hex_string(satom) + "], ";
    retVal += "ID[" + std::to_string(satom->get_id()) + "], ";
    retVal += "Func[" + std::to_string(satom->get_functor()) + "(" + to_string_functor(satom->get_functor()) + ")], ";
    retVal += "Arity[" + std::to_string(satom->get_arity()) + "], ";
    retVal += "Link[" + std::to_string(LMN_ATTR_GET_VALUE(attr)) + "]";
  }
  retVal += __ESC_START__;
  retVal += __ESC_END__;
  return retVal;
}

// 通常のアトムの文字列表現に加えて，接続先のアトムやそれらのアドレス・属性・IDなどを合わせて表示
std::string to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  if (atom == nullptr) {
    return "null";
  }
  std::string retVal = to_string_atom(atom, attr) + "\n ";
  retVal += __to_string_dev_atom(atom, attr) + "\n";
  if (!LMN_ATTR_IS_DATA(attr)) {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    for (int i = 0, max = satom->get_arity(); i < max; i++) {
      retVal += "   " + std::to_string(i) + ": " + to_string_atom(satom->get_link(i), satom->get_attr(i)) + "\n        " + __to_string_dev_atom(satom->get_link(i), satom->get_attr(i)) + "\n";
    }
  }
  return retVal;
}

std::string to_string_dev_reg(const LmnRegisterRef reg) {
  if (reg == nullptr) {
    return "null";
  }

  LmnWord wt = reg->wt;
  LmnByte at = reg->at;
  LmnByte tt = reg->tt;

  switch (tt) {
    case TT_ATOM:
        return to_string_dev_atom((LmnAtomRef)wt, at);
    case TT_MEM:
      return print_object_ids(((LmnMembraneRef)wt)->NAME_ID(), (LmnMembraneRef)wt, ((LmnMembraneRef)wt)->mem_id())
              + "\n" + to_string_dev_mem((LmnMembraneRef)wt);
    case TT_OTHER:
      return "wt[" + to_hex_string(wt) + "], at[" + to_hex_string(at) + "], tt[other]";
    default:
      return "unknown tt type";
  }
}

// TODO implement
std::string to_string_dev_regarray(const LmnRegisterArray* reg_array) {
  if (reg_array == nullptr) {
    return "null";
  }
  return "Dev!" + to_string_regarray(reg_array);
}

// TODO implement
std::string to_string_dev_atomlist(const AtomListEntry* atomlist) {
  if (atomlist == nullptr) {
    return "null";
  }
  return "Dev!" + to_string_atomlist(atomlist);
}

// TODO implement
std::string to_string_dev_mem(const LmnMembraneRef mem) {
  if (mem == nullptr) {
    return "null";
  }
  return "Dev!" + to_string_mem(mem);
}

} // namespace debug_printer
} // namespace slim
