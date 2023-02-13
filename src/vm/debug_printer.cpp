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

/**
 * テーブルからアトムに対応するPrintRecordの参照を返す
 * 対応する要素がなければ，テーブルに空のエントリを作成してその参照を返す
 */
static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom);
/**
 * テーブル内に保存されたリンク情報の合計数を返す
 */
static int get_overall_links(dump_history_table &table);
/**
 * アトム名がそのままで有効ならtrueを返す
 * クォートが必要ならfalseを返す
 */
static bool __is_direct_printable(LmnFunctor f);
/**
 * アトムの文字列表現を返す
 * 属性attrの値によってデータアトムとして表現するかシンボルアトムとして表現されるか決定される
 */
static std::string __to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth);
/**
 * データアトムの文字列表現を返す
 */
static std::string __to_string_datom(const LmnAtomRef data, LmnLinkAttr attr);
/**
 * シンボルアトムの文字列表現を返す
 * ダンプの主な対象でなく，元のアトムと最終リンク以外で繋がっているなら，リンクとして表現する
 * ダンプの主な対象でなく，すでにダンプされているなら，リンクとして表現する
 * シンボルアトムの表示のための再帰呼び出しの最大の深さを超えたら，リンクとして表現する
 * ダンプ済みなら空文字列を返す
 * リストならリストとして表現する
 * それ以外ならアトムとして表現する
 */
static std::string __to_string_satom(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht, size_t call_depth);
/**
 * '.'で表される特殊なシンボルアトムを含む構造を，各括弧を使った簡潔な表現にして返す
 */
static std::string __to_string_list(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth);
/**
 * ファンクタから名前の文字列表現を返す
 * 場合に応じて適切にクォートしてから返す
 */
static std::string __to_string_atom_name(LmnFunctor functor);
/**
 * シンボルアトムのリンクを一切たどらず，簡潔な表現を返す
 */
static std::string __to_string_satom_short(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht);
/**
 * アトムの引数列の文字列表現を返す
 * 引数なしの場合は空文字列を返す
 * 1つ以上引数がある場合は前後を丸括弧でくくり，それぞれを','で区切って表現する
 */
static std::string __to_string_satom_args(const LmnSymbolAtomRef atom, dump_history_table &ht, size_t call_depth);
/**
 * アトムの引数列の簡潔な文字列表現を返す。リンクを一切たどらない。
 */
static std::string __to_string_satom_args_short(const LmnSymbolAtomRef atom, dump_history_table &ht);
/**
 * アトムのある引数の文字列表現を返す
 * リンク先がデータアトムならその文字列表現を返す
 * リンク先がシンボルアトムの場合，そのリンクに既に名前がついていればその名前を返す
 * 名前がなく，リンク先のアトムの最終リンク以外と接続されている場合，リンクとして表現される
 * 名前がなく，リンク先のアトムの最終リンクで自アトムと接続されている場合，アトムとして表現される
 */
static std::string __to_string_satom_arg(const LmnSymbolAtomRef atom, int arg_index, dump_history_table &ht, size_t call_depth);
/**
 * アトムのある引数のリンクとしての文字列表現を返す
 */
static std::string __to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht);
/**
 * アトムリストの文字列表現を返す
 * リスト内の各アトムのリンク関係は適切に表現される
*/
static std::string __to_string_atomlist(const AtomListEntry* atomlist, dump_history_table &ht);
/**
 * レジスタの文字列表現を返す
 * ttの値によって，表現の形式は変更される
 */
static std::string __to_string_reg(const LmnRegisterRef reg, dump_history_table &ht);
/**
 * 膜の文字列表現を返す
 * 先頭と最後の中括弧を含まない
 */
static std::string __to_string_cell_internal(const LmnMembraneRef mem, dump_history_table &ht);
/**
 * 膜の文字列表現を返す
 * 先頭と最後の中括弧を含む
 */
static std::string __to_string_mem_internal(const LmnMembraneRef mem, dump_history_table &ht);
/**
 * データアトムなら，データ・属性の16進数表現を返す
 * シンボルアトムなら，このアトムのアドレス・ID・ファンクタ・価数・リンク番号を返す
 */
static std::string __to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr);

// ===============================================================

static PrintRecord& get_history_entry(dump_history_table &table, const LmnSymbolAtomRef atom) {
  auto ent = table.find(atom);
  if (ent == table.end()) { // not found
    auto iter = table.emplace(atom, PrintRecord({false,{}}));
    return (*(iter.first)).second;
  } else { // found
    return (*ent).second;
  }
}

static int get_overall_links(dump_history_table &table) {
  int links = 0;
  for (auto &p : table) {
    links += p.second.table.size();
  }
  return links;
}

std::string to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  dump_history_table ht;
  return __to_string_atom(atom, attr, ht, 0);
}

static std::string __to_string_atom(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht, size_t call_depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return __to_string_datom(atom, attr);
  } else {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    LmnFunctor f = satom->get_functor();
    return __to_string_satom(satom, LMN_ATTR_GET_VALUE(attr), ht, call_depth);
  }
}

static std::string __to_string_atom_short(const LmnAtomRef atom, LmnLinkAttr attr, dump_history_table &ht) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return __to_string_datom(atom, attr);
  } else {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    LmnFunctor f = satom->get_functor();
    return __to_string_satom_short(satom, LMN_ATTR_GET_VALUE(attr), ht);
  }
}

std::string to_string_atom_short(const LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return __to_string_datom(atom, attr);
  } else {
    dump_history_table ht;
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    LmnFunctor f = satom->get_functor();
    return __to_string_satom_short(satom, LMN_ATTR_GET_VALUE(attr), ht);
  }
}

static std::string __to_string_link(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht) {
  PrintRecord &record = get_history_entry(ht, atom);

  auto res = record.table.find(link_position);

  if (res == record.table.end()) { // not found
    LmnLinkAttr dst_attr = atom->get_attr(link_position);
    if (LMN_ATTR_IS_DATA(dst_attr)) {
      return __to_string_datom(atom->get_link(link_position), dst_attr);
    } else {
      LmnSymbolAtomRef dst_atom = (LmnSymbolAtomRef)atom->get_link(link_position);
      int l = LMN_ATTR_GET_VALUE(dst_attr);
      std::string name = LINK_PREFIX + std::to_string(get_overall_links(ht) / 2);
      record.table.emplace(link_position, name);
      get_history_entry(ht, dst_atom).table.emplace(l, name);
      return name;
    }
  } else { // found
    return (*res).second;
  }
}

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

  if (record.done) { // ダンプ済み
    return "";
  }

  // リストならリスト用の表示
  if (/* call_depth == 0 &&  */functor == LMN_LIST_FUNCTOR) {
    return __to_string_list(atom, ht, call_depth)/*  + "=" + __to_string_satom_arg(atom, 2, ht, call_depth + 1) */;
  }

  record.done = true;

  return __to_string_atom_name(functor) + __to_string_satom_args(atom, ht, call_depth);
}

static std::string __to_string_satom_short(const LmnSymbolAtomRef atom, int link_position, dump_history_table &ht) {
  LmnFunctor functor = atom->get_functor();
  return __to_string_atom_name(functor) + __to_string_satom_args_short(atom, ht);
}

static std::string __to_string_satom_args_short(const LmnSymbolAtomRef atom, dump_history_table &ht) {
  int link_num = atom->get_link_num();

  if (link_num == 0) { // if atom has no links, do not show parenthes
    return "";
  } else {
    std::string retVal = "";
    retVal += "(";

    for (int i = 0; i < link_num; i++) {
      if (i > 0) {
        retVal += ",";
      }
      retVal += __to_string_link(atom, i, ht);
    }

    retVal += ")";
    return retVal;
  }
}

static bool __is_direct_printable(LmnFunctor f) {
  if (LMN_IS_PROXY_FUNCTOR(f)/*  || f == LMN_NIL_FUNCTOR */) {
    return true;
  }
  std::string s = LMN_FUNCTOR_STR(f);

  // /[a-z][a-zA-Z_]*/
  // 1文字目
  if (!isalpha(s[0]) || !islower(s[0])) {
    return false;
  }
  // 2文字目以降
  return std::all_of(s.cbegin() + 1, s.cend(),
    [](char c) -> bool { return isalpha(c) || isdigit(c) || c == '_'; }
  );
}

static std::string __to_string_atom_name(LmnFunctor functor) {
  std::string retVal = "";
  if (LMN_FUNCTOR_MODULE_ID(lmn_functor_table, functor) != ANONYMOUS) {
    retVal += lmn_id_to_name(LMN_FUNCTOR_MODULE_ID(lmn_functor_table, functor));
    retVal += ".";
  }

  std::string str = LMN_FUNCTOR_STR(functor);
  if (__is_direct_printable(functor)) {
    retVal += LMN_FUNCTOR_STR(functor);
  } else {
    retVal += "'";
    retVal += LMN_FUNCTOR_STR(functor);
    retVal += "'";
  }

  return retVal;
}

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

std::string to_string_regarray(const LmnRegisterArray* reg_array) {
  if (reg_array == nullptr) {
    return "null";
  }

  std::string retVal = "[ ";
  for (size_t i = 0, reg_len = reg_array->size(); i < reg_len; i++) {
    if (i > 0) {
      retVal += " | ";
    }
    retVal += to_string_reg((LmnRegisterRef) &(reg_array->at(i)));
  }
  retVal += " ]";

  return retVal;
}

std::string to_string_instrop(LmnInstrOp op) {
  for (auto &p : instr_spec) {
    if (op == p.first) {
      return p.second.op_str;
    }
  }
  return "unknown";
}

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

std::string to_string_atomlist(const AtomListEntry* atomlist) {
  if(atomlist == nullptr) {
    return "null";
  }
  dump_history_table ht;
  return __to_string_atomlist(atomlist, ht);
}

static std::string __to_string_atomlist(const AtomListEntry* atomlist, dump_history_table &ht) {
  size_t len = 0;
  std::string list = "";
  for (auto begin = std::begin(*atomlist), end = std::end(*atomlist); begin != end; ++begin) {
    std::string s = __to_string_atom_short(*begin, LMN_ATTR_MAKE_LINK(0), ht);
    if (!s.empty()) {
      list += s;
      list += ". ";
    }
    len++;
  }
  LmnFunctor functor = (*(atomlist->begin()))->get_functor();
  std::string header = to_string_functor(functor) + " (" + std::to_string(len) + ")  : ";

  return header + list;
}

std::string to_string_atomlists(std::map<LmnFunctor,AtomListEntry*> atomlists) {
  dump_history_table ht;
  std::string retVal = "";
  for (auto &p : atomlists) {
    retVal += __to_string_atomlist(p.second, ht);
    retVal += "\n";
  }
  return retVal;
}

std::string to_string_functor(LmnFunctor func) {
  if (lmn_functor_table == nullptr) {
    return "Functor table is null.\n";
  }
  auto entry = lmn_functor_table->get_entry(func);
  return __to_string_atom_name(func) + "_" + std::to_string(entry->arity);
}

static std::string __to_string_cell_internal(const LmnMembraneRef mem, dump_history_table &ht) {
  enum { P0, P1, P2, P3, PROXY, PRI_NUM };
  std::vector<std::vector<LmnSymbolAtomRef>> pred_atoms(PRI_NUM);

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

  std::string retVal = "";
  for (auto &v : pred_atoms) {
    for (auto &atom : v) {
      std::string s = __to_string_satom(atom, 0, ht, 0);
      retVal += s;
      if (!s.empty()) {
        retVal += ". ";
      }
    }
  }

  bool dumped = false;
  for (LmnMembraneRef m = mem->mem_child_head(); m != nullptr; m = m->mem_next()) {
    std::string s = __to_string_mem_internal(m, ht);
    retVal += s;
    if (!s.empty()) {
      dumped = true;
    }
    if (dumped && m->mem_next() != nullptr && ht.find((LmnSymbolAtomRef)m->mem_next()) == ht.end()) {
      retVal += ", ";
    }
  }
  if (dumped) {
    retVal += ". ";
  }

  return retVal;
}

static std::string __to_string_mem_internal(const LmnMembraneRef mem, dump_history_table &ht) {
  if (ht.find((LmnSymbolAtomRef)mem) != ht.end()) {
    return "";
  }
  ht.emplace((LmnSymbolAtomRef)mem, PrintRecord({false,{}}));
  std::string retVal = "";
  if (mem->NAME_ID() != ANONYMOUS) {
    retVal += lmn_id_to_name(mem->NAME_ID());
  }
  retVal += "{";
  retVal += __to_string_cell_internal(mem, ht);
  retVal += "}";
  return retVal;
}

std::string to_string_mem(const LmnMembraneRef mem) {
  if (mem == nullptr) {
    return "null";
  }
  dump_history_table ht;
  return "{" + __to_string_cell_internal(mem, ht) + "}";
}

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

std::string to_string_dev_atom(const LmnAtomRef atom, LmnLinkAttr attr) {
  if (atom == nullptr) {
    return "null";
  }
  dump_history_table ht;
  std::string retVal = to_string_atom(atom, attr) + "\n ";
  retVal += __to_string_dev_atom(atom, attr) + "\n";
  if (!LMN_ATTR_IS_DATA(attr)) {
    LmnSymbolAtomRef satom = (LmnSymbolAtomRef)atom;
    for (int i = 0, max = satom->get_arity(); i < max; i++) {
      retVal += "   " + std::to_string(i) + ": " + to_string_atom(satom->get_link(i), satom->get_attr(i)) + "\n";
      retVal += "        " + __to_string_dev_atom(satom->get_link(i), satom->get_attr(i)) + "\n";
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
std::string to_string_dev_mem(const LmnMembraneRef mem) {
  if (mem == nullptr) {
    return "null";
  }
  return "Dev!" + to_string_mem(mem);
}

} // namespace debug_printer
} // namespace slim
