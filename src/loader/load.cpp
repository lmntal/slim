/*
 * load.cpp - Load Intermediate Language
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
 *
 * $Id: load.c,v 1.13 2008/10/17 08:40:50 sasaki Exp $
 */

#include <map>

#include "arch.h"
#include "element/element.h"
#include "ffi/lmntal_system_adapter.h"
#include "lmntal.h"
#include "load.h"
#include "so.h"
#include "vm/vm.h"
#include <ctype.h>
#include <dirent.h>
#include <dlfcn.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
/* prototypes */

void build_cmd(char *buf, char *file_name);
FILE *compile(char *filename);

#include "byte_encoder.hpp"
#include "exception.hpp"
#include "il_lexer.hpp"
#include "il_parser.hpp"
#include "syntax.hpp"

#include <algorithm>
#include <map>
#include <string>

using loader_error = slim::loader::exception;

int ilparse(il::lexer *scanner, std::unique_ptr<IL> *il,
            std::unique_ptr<Rule> *rule);

/*
 *  Instruction Format
 *
 *  * instructions
 *     sequence of instruction
 *
 *  * instruction
 *     LmnInstrOp              : instruction ID
 *     sequence of argument    : arguments of the instruction
 *
 *  * argument
 *    * integer functor
 *        BYTE(LMN_INT_ATTR)
 *        long                  : integer value
 *    * float functor
 *        BYTE(LMN_FLOAT_ATTR)
 *        double                : double nvalue
 *    * string functor
 *        BYTE(LMN_STRING_ATTR)
 *        lmn_interned_str      : string id
 *    * symbol functor
 *        BYTE(0)
 *        LmnFunctor(functor id)
 *    * in/out proxy functor
 *        BYTE(0)
 *        LmnFunctor(LMN_IN(OUT)_PROXY_FUNCTOR)
 *    * unify functor
 *        BYTE(0)
 *        LmnFunctor(LMN_UNIFY_FUNCTOR)
 *    * ruleset
 *        LmnRulesetId           : RuleSet ID
 *    * instruction list
 *        LmnSubInstrSize         : size of instruction list
 *        sequence of instruction
 *    * InstrVar
 *        LmnInstrVar      : integer, variable number
 *    * InstrVarList
 *        int16_t          : # of elements (N)
 *        LmnInstrVar * N
 *    * Label
 *        LmnJumpOffset      : difference between the next location to
 * destination
 *
 */

std::unique_ptr<LmnRule> load_rule(Rule const &rule) {
  ByteEncoder encoder;

  /*   load_inst_block(rule->amatch, encoder); */
  encoder.load(rule.mmatch);
  encoder.load(rule.guard);
  encoder.load(rule.body);

  /* ラベルを参照している位置に、実際のラベルの位置を書き込む */
  encoder.resolve_labels();

  auto runtime_rule = encoder.create_rule();
  if (rule.hasuniq)
    lmn_rule_init_uniq_rule(runtime_rule.get());

  return runtime_rule;
}

static LmnRuleSetRef load_ruleset(const RuleSet &rs) {
  auto runtime_ruleset = new LmnRuleSet(rs.id, 10);

  for (auto &r : rs.rules) {
    auto rule = load_rule(*r);
    runtime_ruleset->put(rule.get());
    rule.release();
  }

  ruleset_table->register_ruleset(runtime_ruleset, rs.id);

  if (rs.is_system_ruleset) {
    /* 各ルールをシステムルールセットに追加する */
    for (int i = 0; i < runtime_ruleset->num; i++) {
      LmnRuleRef rule2 = lmn_rule_copy(runtime_ruleset->get_rule(i));
      lmn_add_system_rule(rule2);
    }
  }

  return runtime_ruleset;
}

/* 最初のルールセットを返す */
static LmnRuleSetRef load_il(const IL &il) {
  LmnRuleSetRef first_ruleset;

  /* load rules */
  auto &rulesets = il.rulesets;
  first_ruleset = NULL;
  for (int i = 0; i < rulesets.size(); i++) {
    auto t = load_ruleset(*rulesets.at(i));
    if (i == 0)
      first_ruleset = t;
  }

  if (!first_ruleset)
    throw loader_error("no ruleset in il");

  /* load module list */
  for (auto &m : il.modules)
    lmn_set_module(m->name_id, ruleset_table->get(m->ruleset_id));

  return first_ruleset;
}

/* soから試しに呼び出す関数 */
void helloworld(const char *s) { fprintf(stdout, "hello %s world!\n", s); }

LmnRuleSetRef load_and_setting_trans_maindata(struct trans_maindata *maindata) {
  LmnRuleSetRef ret = 0; /* ワーニング抑制 */

  /* シンボルを読み込み+変換テーブルを設定 */
  for (int i = 1; i < maindata->count_of_symbol; i++)
    maindata->symbol_exchange[i] = lmn_intern(maindata->symbol_table[i]);

  /* ファンクタを読み込み+変換テーブルを設定 */
  for (int i = 0; i < maindata->count_of_functor; i++) {
    LmnFunctorEntry ent = maindata->functor_table[i];
    /* スペシャルファンクタは登録できないが,
     * functor.c内で登録される共通部分以外出現しようがないはずなので問題ない */
    if (ent.special) {
      /* 登録しないで変換も必要無し */
      maindata->functor_exchange[i] = i;
    } else {
      /* シンボルは変換を忘れないように */
      LmnFunctor gid =
          lmn_functor_intern(maindata->symbol_exchange[ent.module],
                             maindata->symbol_exchange[ent.name], ent.arity);
      maindata->functor_exchange[i] = gid;
    }
  }

  /* ルールセット0番は数合わせ */
  /* システムルールセット読み込み */
  for (auto &rule : maindata->ruleset_table[1]) {
    lmn_add_system_rule(lmn_rule_make_translated(
        rule.function, maindata->symbol_exchange[rule.name]));
  }

  /* ルールセット2番はinitial ruleset */
  for (auto &rule : maindata->ruleset_table[2]) {
    lmn_add_initial_rule(lmn_rule_make_translated(
        rule.function, maindata->symbol_exchange[rule.name]));
  }

  /* ルールセット3番はinitial system ruleset */
  for (auto &rule : maindata->ruleset_table[3]) {
    lmn_add_initial_system_rule(lmn_rule_make_translated(
        rule.function, maindata->symbol_exchange[rule.name]));
  }

  /* ルールセットを読み込み+変換テーブルを設定 */
  for (int i = FIRST_ID_OF_NORMAL_RULESET; i < maindata->count_of_ruleset;
       i++) {
    auto &tr = maindata->ruleset_table[i];
    auto gid = lmn_gen_ruleset_id();
    auto rs = new LmnRuleSet(gid, tr.size);
    ruleset_table->register_ruleset(rs, gid);

    for (auto &r : tr)
      rs->put(lmn_rule_make_translated(r.function,
                                       maindata->symbol_exchange[r.name]));

    /* とりあえず最初の通常ルールセットを初期データ生成ルールと決め打ちしておく
     */
    if (i == FIRST_ID_OF_NORMAL_RULESET) {
      ret = rs;
    }
    maindata->ruleset_exchange[i] = gid;
  }

  /* モジュール読込み */
  for (int i = 0; i < maindata->count_of_module; i++) {
    auto &mo = maindata->module_table[i];
    lmn_set_module(maindata->symbol_exchange[mo.name],
                   ruleset_table->get(maindata->ruleset_exchange[mo.ruleset]));
  }

  return ret;
}

/* soハンドルから中間命令を読み出す. load_extは開始ルールを認識しない.
 * 複数ファイルを1つのsoにした場合,
 * 初期データ生成ルールが複数あるはずだがとりあえず無視.
 * TODO:
 *   初期データ生成ルールセットのルールを1つのルールセットにまとめて出力すれば問題無し.
 *   1回適用成功したところで止めなければok     */
LmnRuleSetRef load_compiled_il(const std::string &filename, void *sohandle) {
  auto basename = create_formatted_basename(filename);
  auto init_str = std::string("init_") + basename;
  auto init_f = (void (*)())dlsym(sohandle, init_str.c_str());

  if (!init_f)
    throw loader_error(std::string("init function \"") + init_str +
                       "\" not found in " + filename);

  /* 初期化関数を呼び出し */
  (*init_f)();

  /* データオブジェクトを取得 */
  auto maindata_str = std::string("trans_") + basename + "_maindata";
  auto maindata = (trans_maindata *)dlsym(sohandle, maindata_str.c_str());

  if (!maindata)
    throw loader_error(std::string("maindata \"") + maindata_str +
                       " not found in " + basename + ".");

  /* 読み込みと変換テーブルの設定 */
  return load_and_setting_trans_maindata(maindata);
}

/* ファイルから中間言語を読み込みランタイム中に配置する。
 * 最初のルールセットを返す */
LmnRuleSetRef load(FILE *in) {
  std::unique_ptr<IL> il;
  il::lexer scanner(in);

  if (ilparse(&scanner, &il, NULL))
    throw loader_error("failed in parsing il files");

  return load_il(*il);
}

static std::vector<void *> opened_so_files;
void init_so_handles() {}

void finalize_so_handles() {
  for (auto v : opened_so_files)
    dlclose(v);
  opened_so_files.clear();
}

namespace fs = slim::element::filesystem;

/* 拡張子がsoならリンクする */
/* dlopenは環境変数にLD_LIBRARY_PATH="."と設定しないとカレントディレクトリを検索してくれないので注意
 */
LmnRuleSetRef load_so_file(const std::string &file_name) {
  auto sohandle = dlopen(file_name.c_str(), RTLD_LAZY);
  if (!sohandle)
    throw loader_error(std::string("Failed to open ") + file_name + " (" +
                       dlerror() + ")");

  dlerror();
  opened_so_files.push_back(sohandle);
  return load_compiled_il(file_name.c_str(), sohandle);
}

/* 拡張子がlmnならばJavaによる処理系で中間言語にコンパイルする */
LmnRuleSetRef load_lmn_file(const std::string &file_name) {
  const char *lmntal_home = getenv(ENV_LMNTAL_HOME);
  if (!lmntal_home)
    throw loader_error("environment variable \"LMNTAL_HOME\" is not set");

  if (fs::exists(std::string(lmntal_home)))
    throw loader_error(
        std::string("Failed to run lmntal compiler (lmntal don't exist at ") +
        lmntal_home + ")");

  auto fp_compiled = lmntal_compile_file(file_name.c_str());
  slim::element::scope free_fp_compiled{[=] {
    if (fp_compiled)
      fclose(fp_compiled);
  }};
  return load(fp_compiled);
}

/* ファイルから中間言語を読み込みランタイム中に配置し、最初のルールセットを返す。
 * ファイルの拡張子が lmn の場合、Javaによる処理系でファイルをコンパイルし、
 * 中間言語を生成する。soの場合、dlopenしておきハンドラはopened_so_filesで管理。
 * dlcloseはfinalize()でされる */

LmnRuleSetRef load_file(const std::string &file_name) {
  fs::path filepath(file_name);
  if (filepath.extension().string() == "so")
    return load_so_file(file_name);

  if (filepath.extension().string() == "lmn")
    return load_lmn_file(file_name);

  auto fp = fopen(filepath.string().c_str(), "r");
  if (!fp) {
    perror(filepath.string().c_str());
    throw loader_error(filepath.string());
  }
  slim::element::scope free_fp{[=] { fclose(fp); }};
  return load(fp);
}

static std::string extension_table[] = {"", "lmn", "il", "so"};
static int file_type(const fs::path &filename) {
  auto ext = filename.extension().string();
  auto i =
      std::find(std::begin(extension_table), std::end(extension_table), ext);
  if (i == std::end(extension_table))
    return 0;
  return i - std::begin(extension_table);
}

/* pathのディレクトリ内のファイルを中間コードとしてロードする.
 * 拡張子を除いてファイル名が同一な場合はextension_tableで指定した優先順で1種類のみ読み込む
 */
void load_il_files(const char *path_string) {
  try {
    fs::path path(path_string);
    fs::directory_iterator dir = path;

    std::map<std::string, int> loading_files_type;

    /* 読み込むファイルをリストアップする */
    for (auto dp : dir) {
      if (!fs::is_regular_file(dp.status()))
        continue;

      auto dname = dp.path().filename();
      auto filetype = file_type(dname);
      if (filetype == 0)
        continue;

      auto basename = dname.stem().string();
      if (filetype > loading_files_type[basename])
        loading_files_type[basename] = filetype;
    }

    /* 読み込む */
    for (auto &p : loading_files_type) {
      fs::path basename(p.first);
      auto filetype = p.second;
      auto extension = extension_table[filetype];

      auto file = path / basename.replace_extension(extension);
      load_file(file.string());
    }
  }
  catch (const fs::filesystem_error &e) {
    fprintf(stderr, "%s\n", e.what());
  }
}

/* inから中間言語を読み込み、構文木を作る。構文木はruleに設定される。
   正常に処理された場合は0，エラーが起きた場合は0以外を返す。*/
std::unique_ptr<Rule> il_parse_rule(FILE *in) {
  il::lexer scanner(in);
  std::unique_ptr<Rule> rule;
  if (ilparse(&scanner, NULL, &rule))
    throw loader_error("failed in parsing il files");
  return rule;
}

std::string create_formatted_basename(const std::string &filepath) {
  auto sep = filepath.find_last_of(DIR_SEPARATOR_CHAR);
  auto begin = (sep == std::string::npos) ? filepath.begin() : (filepath.begin() + sep + 1);
  auto end = std::find(begin, filepath.end(), '.'); /* ファイル名最初の.を探す ないと困る */
  std::string basename(begin, end + 1);
  std::replace_if(basename.begin(), basename.end(), [](char &c) { return !(isalpha(c) || isdigit(c)); }, 'O');
  return basename;
}
