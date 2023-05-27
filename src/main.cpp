/*
 * main.c - main
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
 * $Id: main.c,v 1.17 2008/10/16 18:14:00 sasaki Exp $
 */

#include <cctype>
#include <cstdio>
#include <iostream>
#include <unistd.h>
#include <vector>

#include "cxxopts/cxxopts.hpp"
#include "fmt/color.h"

#include "arch.h"
#include "element/element.h"
#include "ffi/jni_lmntal.h"
#include "ffi/lmntal_system_adapter.h"
#include "lmntal.h"
#include "loader/loader.h"
#include "options.h"
#include "verifier/runtime_status.h"
#include "verifier/verifier.h"
#include "vm/vm.h"

#ifdef USE_CUNIT
#include "test/unit_test.h"
#endif

void install_builtin_extensions();
void init_builtin_extensions(); /* ext/init_exts.c */

void init_default_system_ruleset();
void init_rules();
void destroy_rules();
void sym_tbl_destroy();
void sym_tbl_init();

void slim_version(FILE *f = stdout) {
  fmt::print(f, fmt::fg(fmt::color::light_blue) | fmt::emphasis::underline, "S");
  fmt::print(f, "lim ");
  fmt::print(f, fmt::fg(fmt::color::light_blue) | fmt::emphasis::underline, "L");
  fmt::print(f, "mntal ");
  fmt::print(f, fmt::fg(fmt::color::light_blue) | fmt::emphasis::underline, "IM");
  fmt::print(f, "plementation ");
  fmt::print(f, "- version {} ({})\n", SLIM_VERSION, COMMIT_ID);
}

static auto parse_options(int argc, char *argv[], cxxopts::ParseResult &result) {
  try {
    result = slim_options.parse(argc, argv);
  } catch (cxxopts::exceptions::exception &e) {
    std::cerr << e.what() << std::endl;
    return CommandLineParseResult::ERROR;
  }

  return parse_command_line(result);
}

/* 処理系内部の初期化処理 */
static void init_internal() {
  lmn_profiler_init(lmn_env.core_num);
  sym_tbl_init();
  lmn_functor_table = new LmnFunctorTable();
  init_rules();

  if (lmn_env.translate == FALSE) {
    init_so_handles();
    init_default_system_ruleset();
    if (lmn_env.enable_por == TRUE) {
      dpor_env_init();
    }
    mpool_init();
    mem_isom_init();
    /*    ext_init(); */
    sp_atom_init();
    CCallback::ccallback_init();
    init_builtin_extensions();

    dumper_init();
    string_init();
    port_init();
  }
}

enum class InitResult { SUCCESS, ERROR, EXIT };

static inline InitResult slim_init(int argc, char **argv, cxxopts::ParseResult &result) {
  lmn_stream_init();
  init_options();
  if (auto res = parse_options(argc, argv, result); res != CommandLineParseResult::OK) {
    return res == CommandLineParseResult::EXIT ? InitResult::EXIT : InitResult::ERROR;
  }
  init_internal();

  /** load directories(system & load path) */
  for (auto i = lmn_env.load_path_num - 1; i >= 0; i--) {
    load_il_files(lmn_env.load_path[i]);
  }

  return InitResult::SUCCESS;
}

static inline void slim_finalize() {
  if (lmn_env.translate == FALSE) {
    port_finalize();
    string_finalize();
    dumper_finalize();

    if (lmn_env.enable_por == TRUE) {
      dpor_env_destroy();
    }
    mem_isom_finalize();
    /*    ext_finalize(); */
    CCallback::ccallback_finalize();
    sp_atom_finalize();
    free_atom_memory_pools();
    finalize_so_handles();
  }

  lmn_profiler_finalize();
  destroy_rules();
  delete lmn_functor_table;
  sym_tbl_destroy();

  lmn_stream_destroy();
  slim::element::LifetimeProfiler::check_memory_leak();
}

static inline bool load_input_files(std::vector<LmnRuleSetRef> &start_rulesets, std::vector<std::string> const &files) {
  for (auto const &f : files) {
    LmnRuleSetRef t;

    try {
      if (f == "-") { /* 標準入力からの読込み */
        t = load(std::unique_ptr<FILE, decltype(&fclose)>(stdin, [](FILE *) -> int { return 0; }));
      } else {
        t = load_file(f);
        if (t != nullptr) {
          start_rulesets.push_back(t);
        }
      }
    } catch (slim::loader::exception const &e) {
      fprintf(stderr, "loader error: %s\n", e.what());
      return false;
    }
  }

  if (start_rulesets.empty()) {
    /** detected invalid file */
    fprintf(stderr, "bad input file.\n");
    return false;
  }

  return true;
}

static inline void slim_exec(std::vector<LmnRuleSetRef> const &start_rulesets) {
  if (lmn_env.nd == FALSE) {
    /* プログラム実行 */
    Task::lmn_run(start_rulesets);
  } else {
    /* プログラム検証 */
    AutomataRef automata{};
    PVector     prop_defs{};
    int         ret{1};

    if (((lmn_env.automata_file != nullptr) || (lmn_env.ltl_exp != nullptr)) &&
        (lmn_env.propositional_symbol != nullptr)) {
      /* load property automata, definition of atomic propositional symbol */
      ret = mc_load_property(&automata, &prop_defs);
      if (ret != 0) {
        mc_explain_error(ret);
        return;
      }
      if (lmn_env.prop_scc_driven == TRUE) {
        automata->analysis();
      }
      if (lmn_env.property_dump == TRUE) {
        automata->print_property();
        return;
      }
    }

    run_mc(start_rulesets, automata, prop_defs);

    if (ret == FALSE) {
      delete automata;
      propsyms_free(prop_defs);
    }
  }
}

int main(int argc, char *argv[]) {
  cxxopts::ParseResult result;

  if (auto init = slim_init(argc, argv, result); init != InitResult::SUCCESS) {
    return init == InitResult::EXIT ? 0 : 1;
  }

  if (lmn_env.run_test == TRUE) {
#ifdef USE_CUNIT
    test_main();
#else
    fprintf(stderr, "CUnit is disabled. Please configure with --enable-cunit option.\n");
#endif
    return 0;
  }

  if (result.count("filenames") == 0) {
    /** no input file */
    if (lmn_env.interactive == TRUE) {
      run_jni_interactive(); /* interactive execution */
    } else {
      fprintf(stderr, "no input file\n");
    }
    slim_finalize();
    return 0;
  }

  std::vector<LmnRuleSetRef> start_rulesets;

  auto files = result["filenames"].as<std::vector<std::string>>();

  if (load_input_files(start_rulesets, files)) {
    if (lmn_env.translate == TRUE) [[unlikely]] { // lmntalコードからCへの変換実行の場合
      // TODO: 現在、複数ファイル入力への対応は簡単に実装できるはず？
      // files[0] is first input file name
      translate(files[0] == "-" ? files[0] : "");
    } else [[likely]] {
      if (lmn_env.profile_level >= 1) {
        profile_start_slim();
      }

      slim_exec(start_rulesets);

      if (lmn_env.profile_level >= 1) {
        profile_finish_slim();
        dump_profile_data(stderr);
      }
    }
  }

  slim_finalize();
  return 0;
}
