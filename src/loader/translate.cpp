/*
 * translate.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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
 *    3. Neither the name of the Ueda Laboratory LMNtal Groupy LMNtal
 *       Group nor the names of its contributors may be used to
 *       endorse or promote products derived from this software
 *       without specific prior written permission.
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
 *
 * $Id: translate.c,v 1.34 2008/10/16 18:12:27 sasaki Exp $
 */

#include "translate.hpp"

#include <iterator>
#include <memory>
#include <string_view>
#include <vector>

#include "ankerl/unordered_dense.hpp"
#include "fmt/core.h"
#include "fmt/format.h"

#include "arch.h"
#include "element/element.h"
#include "lmntal.h"
#include "load.h"
#include "so.h"
#include "syntax.hpp"
#include "verifier/verifier.h"
#include "vm/rule.h"

#ifdef PROFILE
#include "verifier/runtime_status.h"
#endif

/* just for debug ! */
static FILE *OUT;

static lmn_interned_str translating_rule_name = 0;
void                    set_translating_rule_name(lmn_interned_str rule_name) { translating_rule_name = rule_name; }

void tr_print_list(int indent, int argi, int list_num, LmnWord const *list) {
  int i;

  print_indent(indent);
  fmt::print(OUT, "int targ{}_num = {};\n", argi, list_num);
  print_indent(indent);
  fmt::print(OUT, "LmnWord targ{}[] = {{", argi);
  for (i = 0; i < list_num; i++) {
    if (i != 0)
      fmt::print(OUT, ", ");
    fmt::print(OUT, "%ld", list[i]);
  }
  fmt::print(OUT, "}};\n");
}

void tr_instr_commit_ready(LmnReactCxtRef rc, LmnRuleRef rule, lmn_interned_str rule_name, LmnLineNum line_num,
                           LmnMembraneRef *ptmp_global_root, LmnRegisterArray *p_v_tmp, unsigned int *org_next_id) {
  LMN_ASSERT(rule);
  rule->name = rule_name;

  *org_next_id = env_next_id();

#ifdef KWBT_OPT
  if (lmn_env.opt_mode != OPT_NONE) {
    lmn_fatal("translter mode, optimize mode is not supported");
  }
#endif

  if (rc->has_mode(REACT_PROPERTY)) {
    return;
  }

  if (!rc->has_mode(REACT_ND))
    return;
  auto *mcrc = dynamic_cast<MCReactContext *>(rc);
  if (mcrc->has_optmode(DeltaMembrane)) {
    /* dmemインタプリタ(body命令)を書かないとだめだ */
    lmn_fatal("translater mode, delta-membrane execution is not supported.");
    // never reach
  }

  LmnRegisterArray tmp;
  ProcessTableRef  copymap;
  LmnMembraneRef   tmp_global_root;
  unsigned int     i;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
  }
#endif

  tmp_global_root = lmn_mem_copy_with_map(rc->get_global_root(), &copymap);

  /** 変数配列および属性配列のコピー */
  auto v = LmnRegisterArray(rc->capacity());

  /** copymapの情報を基に変数配列を書換える */
  for (i = 0; i < rc->capacity(); i++) {
    LmnWord        t;
    LmnRegisterRef r = &v.at(i);
    r->register_set_at(rc->at(i));
    r->register_set_tt(rc->tt(i));
    if (r->register_tt() == TT_ATOM) {
      if (LMN_ATTR_IS_DATA(r->register_at())) {
        r->register_set_wt((LmnWord)lmn_copy_data_atom((LmnAtom)rc->wt(i), (LmnLinkAttr)r->register_at()));
      } else if (proc_tbl_get_by_atom(copymap, (LmnSymbolAtomRef)rc->wt(i), &t)) {
        r->register_set_wt((LmnWord)t);
      } else {
        t = 0;
        lmn_fatal("implementation error");
      }
    } else if (r->register_tt() == TT_MEM) {
      if (rc->wt(i) == (LmnWord)rc->get_global_root()) { /* グローバルルート膜 */
        r->register_set_wt((LmnWord)tmp_global_root);
      } else if (proc_tbl_get_by_mem(copymap, (LmnMembraneRef)rc->wt(i), &t)) {
        r->register_set_wt((LmnWord)t);
      } else {
        t = 0;
        lmn_fatal("implementation error");
      }
    } else { /* TT_OTHER */
      r->register_set_wt(rc->wt(i));
    }
  }
  delete copymap;

  /** SWAP */
  tmp = std::move(rc->work_array);
  rc->warray_set(std::move(v));
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COPY_IN_COMMIT);
  }
#endif

  /* 処理中の変数を外へ持ち出す */
  *ptmp_global_root = tmp_global_root;
  *p_v_tmp          = std::move(tmp);
}

BOOL tr_instr_commit_finish(LmnReactCxtRef rc, LmnRuleRef rule, lmn_interned_str rule_name, LmnLineNum line_num,
                            LmnMembraneRef *ptmp_global_root, LmnRegisterArray *p_v_tmp) {
  if (!rc->has_mode(REACT_ND))
    return true;

  /* 処理中の変数を外から持ち込む */
  LmnMembraneRef   tmp_global_root;
  LmnRegisterArray v;

  tmp_global_root = *ptmp_global_root;
  v               = std::move(*p_v_tmp);

  mc_react_cxt_add_expanded(dynamic_cast<MCReactContext *>(rc), tmp_global_root,
                            rule); /* このruleはNULLではまずい気がする */

  rule->undo_history();

  /* 変数配列および属性配列を元に戻す */

  rc->warray_set(std::move(v));

  return false;
}

BOOL tr_instr_jump(LmnTranslated const &f, LmnReactCxtRef rc, LmnMembraneRef thisisrootmembutnotused, LmnRuleRef rule,
                   int newid_num, int const *newid) {
  auto v = LmnRegisterArray(rc->capacity());
  for (int i = 0; i < newid_num; i++) {
    v.at(i) = rc->reg(newid[i]);
  }

  auto tmp = std::move(rc->work_array);
  rc->warray_set(std::move(v));

  auto ret = f(rc, thisisrootmembutnotused, rule);

  rc->warray_set(std::move(tmp));

  return ret;
}

Vector vec_const_temporary_from_array(int size, LmnWord const *w) {
  Vector v;
  v.set_num(size);
  v.set_cap(size);
  v.set_list((LmnWord *)w);
  return v; /* コピーして返す tblはwをそのまま使うのでdelete Vectorしてはいけない */
}

size_t vec_inserted_index(std::vector<BYTE *> &v, LmnWord w) {
  int i;
  for (i = 0; i < v.size(); i++) {
    if (reinterpret_cast<LmnWord>(v.at(i)) == w)
      return i;
  }
  v.emplace_back(reinterpret_cast<BYTE *>(w));
  return v.size() - 1;
}

char *automalloc_sprintf(char const *format, ...) {
  char    trush[2];
  va_list ap;
  int     buf_len;
  char   *buf;

  va_start(ap, format);
  buf_len = vsnprintf(trush, 2, format, ap);
  va_end(ap);

  buf = (char *)lmn_malloc(buf_len + 1);

  va_start(ap, format);
  vsnprintf(buf, buf_len + 1, format, ap);
  va_end(ap);

  return buf;
}

void print_indent(int n) {
  for (auto i = 0; i < n * 2; ++i) {
    fmt::print(OUT, " ");
  }
}

/* 常に失敗する translate_generatorまわりがおかしくなったらこれをコメントイン */
/*
const BYTE *translate_instruction_generated(const BYTE *p, Vector *jump_points,
const char *header, const char *successcode, const char *failcode, int indent,
int *finishflag)
{
  *finishflag = -1;
  return p;
}
*/

const BYTE *translate_instruction(const BYTE *instr, std::vector<BYTE *> &jump_points, std::string_view header,
                                  std::string_view successcode, std::string_view failcode, int indent,
                                  int *finishflag) {
  LmnInstrOp op;
  /* const BYTE *op_address = instr; */

  READ_VAL(LmnInstrOp, instr, op);

  switch (op) {
  case INSTR_JUMP: {
    /* 残念ながら引数読み込み途中のinstrからオフセットジャンプするため */
    /* 先に全部読み込んでしまうと場所を忘れてしまう */
    LmnInstrVar   num, i, n;
    LmnJumpOffset offset;

    READ_VAL(LmnJumpOffset, instr, offset);
    auto *next       = (BYTE *)instr + offset; /*ワーニング抑制 */
    auto  next_index = vec_inserted_index(jump_points, (LmnWord)next);

    print_indent(indent);
    fmt::print(OUT, "{{\n");
    print_indent(indent);
    fmt::print(OUT, "  static const int newid[] = {{");

    i = 0;
    /* atom */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if (i != 0)
        fmt::print(OUT, ",");
      fmt::print(OUT, "{}", n);
    }

    /* mem */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if (i != 0)
        fmt::print(OUT, ",");
      fmt::print(OUT, "{}", n);
    }

    /* vars */
    READ_VAL(LmnInstrVar, instr, num);
    for (; num--; i++) {
      READ_VAL(LmnInstrVar, instr, n);
      if (i != 0)
        fmt::print(OUT, ",");
      fmt::print(OUT, "{}", n);
    }

    fmt::print(OUT, "}};\n");
    print_indent(indent);
    fmt::print(OUT, "  extern BOOL {}_{}();\n", header, next_index);
    print_indent(indent);
    fmt::print(OUT,
               "  if (tr_instr_jump({}_{}, rc, thisisrootmembutnotused, rule, {}, "
               "newid))\n",
               header, next_index, i);
    print_indent(indent);
    fmt::print(OUT, "    {};\n", successcode);
    print_indent(indent);
    fmt::print(OUT, "  else\n");
    print_indent(indent);
    fmt::print(OUT, "    {};\n", failcode);
    print_indent(indent);
    fmt::print(OUT, "}}\n");

    *finishflag = 0;
    return instr;
  }

  default:
    *finishflag = -1; /* 常に失敗,終了 */
    return instr;
  }
}

/*
  pの先頭から出力して行き,その階層のjump/proceedが出てくるまでを変換する
  jump先は中間命令ではアドレスになっているが,
  そのポインタ値が何個めのjump先として現れたか(index)を,その関数のシグネチャに使う
  物理的に次の読み込み場所を返す
  (変換するスタート地点, 変換する必要のある部分の記録,
  ルールのシグネチャ:trans_**_**_**, 成功時コード, 失敗時コード, インデント)
*/
const BYTE *translate_instructions(const BYTE *p, std::vector<BYTE *> &jump_points, std::string_view header,
                                   std::string_view successcode, std::string_view failcode, int indent) {
  while (true) {
    /* 自動生成で変換可能な中間命令をトランスレートする */
    /* 終了フラグ: 正のとき変換成功+次を変換,
     * 0のとき変換成功+jump/proceed等なので終了, 負のとき変換失敗 */
    const BYTE *next;
    int         finishflag;

    next = translate_instruction_generated(p, jump_points, header.data(), successcode.data(), failcode.data(), indent,
                                           &finishflag);

    if (finishflag > 0) {
      p = next;
      continue;
    }

    if (finishflag == 0) {
      return next;
    }
    /* 自動生成で対処できない中間命令をトランスレートする */
    next = translate_instruction(p, jump_points, header, successcode, failcode, indent, &finishflag);
    if (finishflag > 0) {
      p = next;
      continue;
    }
    if (finishflag == 0) {
      return next;
    }
    LmnInstrOp op;
    READ_VAL(LmnInstrOp, p, op);
    fmt::print(stderr, "translator: unknown instruction: {}\n", op);
    exit(1);
  }
}

static void translate_rule(LmnRuleRef rule, std::string_view header) {
  // Vector *jump_points = new Vector(4);
  std::vector<BYTE *> jump_points(4);

  jump_points.emplace_back(rule->inst_seq);

  for (auto i = 0; i < jump_points.size() /*変換中にjump_pointsは増えていく*/; i++) {
    auto *p = jump_points.at(i);
    fmt::print(OUT,
               "BOOL {}_{}(LmnReactCxt* rc, LmnMembraneRef "
               "thisisrootmembutnotused, LmnRule rule)\n",
               header, i); /* TODO m=wt[0]なのでmは多分いらない */
    fmt::print(OUT, "{{\n");
    /* (変換するスタート地点, 変換する必要のある部分の記録,
     * ルールのシグネチャ:trans_**_**_**, 成功時コード, 失敗時コード,
     * インデント) */
    translate_instructions(p, jump_points, header, "return TRUE", "return FALSE", 1);
    fmt::print(OUT, "}}\n");
  }

  /* 各関数の前方宣言をすることができないので,関数を呼ぶ時には自分で前方宣言をする
   */
  /* trans_***(); ではなく { extern trans_***(); trans_***(); } と書くだけ */
}

static void translate_ruleset(LmnRuleSetRef ruleset, std::string_view header) {
  fmt::memory_buffer buf{};
  lmn_interned_str  *rule_names{};
  auto               inserter = std::back_inserter(buf);

  if (ruleset->size() > 0) {
    rule_names = LMN_CALLOC<lmn_interned_str>(ruleset->size());
  }

  for (int i = 0; i < ruleset->size(); i++) {
    fmt::format_to(inserter, "{}_{}", header, i); /* ルールのシグネチャ */
    translate_rule(ruleset->get_rule(i), buf.data());
    rule_names[i] = translating_rule_name;
  }

  fmt::print(OUT, "struct trans_rule {}_rules[{}] = {{", header, ruleset->size());

  for (int i = 0; i < ruleset->size(); i++) {
    if (i != 0)
      fmt::print(OUT, ", ");
    fmt::print(OUT, "{{{}, {}_{}_0}}", rule_names[i], header, i); /* 各ルールの名前と先頭関数を配列に */
  }
  fmt::print(OUT, "}};\n\n");

  LMN_FREE(rule_names);
}

static void print_trans_header(std::string_view filename) {
  fmt::print(OUT, "/* this .c source is generated by slim --translate */\n");
  fmt::print(OUT, "/* compile: gcc -o {}.so ---.c -shared -Wall -fPIC -I SlimSrcPath*/\n", filename);
  fmt::print(OUT, "/* run    : LD_LIBRARY_PATH=\".\" slim ./{}.so */\n", filename);
  fmt::print(OUT, "/* .so file name must be \"{}\". */\n", filename);
  fmt::print(OUT, "\n");
  fmt::print(OUT, "#include \"so.h\"\n");
  fmt::print(OUT, "\n");
  fmt::print(OUT, "#define TR_GSID(x) (trans_{}_maindata.symbol_exchange[x])\n", filename);
  fmt::print(OUT, "#define TR_GFID(x) (trans_{}_maindata.functor_exchange[x])\n", filename);
  fmt::print(OUT, "#define TR_GRID(x) (trans_{}_maindata.ruleset_exchange[x])\n", filename);
  fmt::print(OUT, "\n");
  fmt::print(OUT, "extern struct trans_maindata trans_{}_maindata;\n", filename);
  fmt::print(OUT, "\n");
}

/* 全ルールセットの総数を数える. ルールセット0番, 1番を数に含む
 * (0番は使わない(番号合わせ),1番はsystem) */
/* 2番3番にinitial_rulesetが入った模様 なので4番から通常ルールセット */
static int count_rulesets() {
  int i;

  for (i = FIRST_ID_OF_NORMAL_RULESET;; i++) {
    if (!LmnRuleSetTable::at(i))
      break;
  }

  return i;
}

extern ankerl::unordered_dense::map<lmn_interned_str, LmnRuleSetRef> mod_table;

static auto count_modules() { return mod_table.size(); }

static void print_trans_maindata(std::string_view filename) {
  fmt::print(OUT, "struct trans_maindata trans_{}_maindata = {{\n", filename);

  /* シンボルの個数(0番anonymousも数える) */
  fmt::print(OUT, "  {}, /*count of symbol*/\n", count_symbols());
  /* シンボルの配列 */
  fmt::print(OUT, "  trans_{}_maindata_symbols, /*symboltable*/\n", filename);
  /* ファンクタの個数 */
  fmt::print(OUT, "  {}, /*count of functor*/\n", lmn_functor_table->get_next_id());
  /* ファンクタの配列 */
  fmt::print(OUT, "  trans_{}_maindata_functors, /*functortable*/\n", filename);
  /* ルールセットの個数 */
  fmt::print(OUT, "  {}, /*count of ruleset*/\n", count_rulesets());
  /* ルールセットオブジェクトへのポインタの配列 */
  fmt::print(OUT, "  trans_{}_maindata_rulesets, /*rulesettable*/\n", filename);
  /* モジュールの個数 */
  fmt::print(OUT, "  {}, /*count of module*/\n", count_modules());
  /* モジュールの配列 */
  fmt::print(OUT, "  trans_{}_maindata_modules, /*moduletable*/\n", filename);
  /* シンボルid変換テーブル */
  fmt::print(OUT, "  trans_{}_maindata_symbolexchange, /*symbol id exchange table*/\n", filename);
  /* ファンクタid変換テーブル */
  fmt::print(OUT, "  trans_{}_maindata_functorexchange, /*functor id exchange table*/\n", filename);
  /* ルールセットid変換テーブル */
  fmt::print(OUT, "  trans_{}_maindata_rulesetexchange /*ruleset id exchange table*/\n", filename);
  fmt::print(OUT, "}};\n\n");
}

static void print_trans_symbols(std::string_view filename) {
  auto count = count_symbols();

  fmt::print(OUT, "const char *trans_{}_maindata_symbols[{}] = {{\n", filename, count);
  for (auto i = 0; i < count; ++i) {
    fmt::print(OUT, "  \"{}\"", lmn_id_to_name(i));
    if (i != count - 1)
      fmt::print(OUT, ",");
    fmt::print(OUT, "\n");
  }
  fmt::print(OUT, "}};\n");

  fmt::print(OUT, "int trans_{}_maindata_symbolexchange[{}];\n\n", filename, count);
}

static void print_trans_functors(std::string_view filename) {
  auto count = lmn_functor_table->get_next_id();
  /* idは0から, next_idが1なら既に1個登録済み => count==next_id */

  fmt::print(OUT, "struct LmnFunctorEntry trans_{}_maindata_functors[{}] = {{\n", filename, count);
  for (auto i = 0; i < count; ++i) {
    fmt::print(OUT, "  {{{}, {}, {}, {}}}", lmn_functor_table->get_entry(i)->special,
               lmn_functor_table->get_entry(i)->module, lmn_functor_table->get_entry(i)->name,
               lmn_functor_table->get_entry(i)->arity);
    if (i != count - 1)
      fmt::print(OUT, ",");
    fmt::print(OUT, "\n");
  }
  fmt::print(OUT, "}};\n");

  fmt::print(OUT, "int trans_{}_maindata_functorexchange[{}];\n\n", filename, count);
}

static void print_trans_rules(std::string_view filename) {
  fmt::memory_buffer buf{};

  auto inserter = std::back_inserter(buf);
  auto count    = count_rulesets();

  /* システムルールセットの出力 */
  fmt::format_to(inserter, "trans_{}_1", filename);
  translate_ruleset(system_ruleset, buf.data());

  /* initial ruleset */
  fmt::format_to(inserter, "trans_{}_2", filename);
  translate_ruleset(initial_ruleset, buf.data());

  /* initial systemruleset */
  fmt::format_to(inserter, "trans_{}_3", filename);
  translate_ruleset(initial_system_ruleset, buf.data());

  /* 通常ルールセットの出力 */
  for (auto i = FIRST_ID_OF_NORMAL_RULESET; i < count; ++i) {
    fmt::format_to(inserter, "trans_{}_{}", filename, i);
    translate_ruleset(LmnRuleSetTable::at(i), buf.data());
  }
}

static void print_trans_rulesets(std::string_view filename) {
  int count, i;

  count = count_rulesets();

  /* ルールセットテーブルで各ルールセットのデータ名を参照するので、先に個々のデータを出力する
   */
  print_trans_rules(filename);

  fmt::print(OUT, "struct trans_ruleset trans_{}_maindata_rulesets[{}] = {{\n", filename, count);
  /* ruleset id is 2,3,4,5... ? 1:systemrulesetただし登録はされていない */
  /* ruleset0番は存在しないが数合わせに出力 */
  fmt::print(OUT, "  {{0,0}},\n");
  /* ruleset1番はtableに登録されていないがsystemrulesetなので出力 */
  fmt::print(OUT, "  {{{},trans_{}_1_rules}},\n", system_ruleset->size(), filename);
  /* ruleset2番,3番はinitial ruleset, initial system ruleset */
  fmt::print(OUT, "  {{{},trans_{}_2_rules}},\n", initial_ruleset->size(), filename);
  fmt::print(OUT, "  {{{},trans_{}_3_rules}},\n", initial_system_ruleset->size(), filename);
  /* 以降は普通のrulesetなので出力(どれが初期データルールかはload時に拾う) */
  for (i = FIRST_ID_OF_NORMAL_RULESET; i < count; i++) {
    LmnRuleSetRef rs = LmnRuleSetTable::at(i);
    LMN_ASSERT(rs); /* countで数えているからNULLにあたることはないはず */

    fmt::print(OUT, "  {{{},trans_{}_{}_rules}}", rs->size(), filename, i);
    if (i != count - 1) {
      fmt::print(OUT, ",");
    }
    fmt::print(OUT, "\n");
  }
  fmt::print(OUT, "}};\n");

  fmt::print(OUT, "int trans_{}_maindata_rulesetexchange[{}];\n\n", filename, count);
}

static int print_trans_module_f(lmn_interned_str const &key, LmnRuleSetRef value, int &counter_p) {
  if (counter_p > 0) {
    fmt::print(OUT, "  ,");
  } else {
    fmt::print(OUT, "   ");
  }

  fmt::print(OUT, "{{ {}, {} }}\n", key, value->id);

  ++counter_p;
  return ST_CONTINUE;
}

static void print_trans_modules(std::string_view filename) {
  auto count = count_modules();
  auto counter{0};

  fmt::print(OUT, "struct trans_module trans_{}_maindata_modules[{}] = {{\n", filename, count);
  for (auto const [key, value] : mod_table) {
    print_trans_module_f(key, value, counter);
  }
  fmt::print(OUT, "}};\n\n");
}

static void print_trans_initfunction(char const *filename) { fmt::print(OUT, "void init_{}(){{\n}}\n\n", filename); }

void translate(std::string_view filepath) {
  std::string filename;

  /* just for debug ! */
  /* OUT = stderr; */
  OUT = stdout;
  /* OUT = fopen("/dev/null", "w"); */

  if (!filepath.empty()) {
    filename = create_formatted_basename(filepath.data());
  } else {
    filename = "anonymous";
  }

  print_trans_header(filename.c_str());
  print_trans_symbols(filename.c_str());
  print_trans_functors(filename.c_str());
  print_trans_rulesets(filename.c_str());
  print_trans_modules(filename.c_str());
  print_trans_maindata(filename.c_str());
  print_trans_initfunction(filename.c_str());

  if (OUT != stdout) {
    fmt::print(stderr, "--translate is under construction\n");
  }
}
