/*
 * main.c - main
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 *
 * $Id: main.c,v 1.17 2008/10/16 18:14:00 sasaki Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include "lmntal.h"
#include "task.h"
#include "symbol.h"
#include "functor.h"
#include "load.h"
#include "translate.h"
#include "arch.h"
#include "automata.h"
#include "lmntal_system_adapter.h"
#include "propositional_symbol.h"
#include "mc.h"
#include "ext.h"

#ifdef PROFILE
#include "runtime_status.h"
#endif

void install_builtin_extensions(void);
void init_builtin_extensions(void); /* ext/init_exts.c */

static void usage(void)
{
  fprintf(stderr,
          "Usage: slim [OPTION]... FILE\n"
          "     When FILE is  -, read standard input.\n"
          "options:\n"
          "  -I<path>        Adds <path> to the head of the load path list.\n"
          "  -O[<0-9>] (-O=-O1)  Optimization level.\n"
          "                  Intermediate instruction sequences are optimized.\n"
          "  -t              trace mode\n"
          "  --showproxy     Show proxy atoms\n"
          "  --hideruleset   Hide ruleset from result\n"
          "  --dot           Output result in dot language\n"
          "  --nd            Nondeterministic execution mode, print all execution paths\n"
          "  --nd_result     Nondeterministic execution mode, print only deadlock paths\n"
          "  --nd_dump       Nondeterministic execution mode, print all state instantly\n"
          "  --ltl           LTL model checking mode\n"
          "  --ltl_all       LTL model checking mode, print all errors\n"
          "  --ltl_nd        --ltl_all and print all seached states and paths\n"
          "  --por           Enable partial order reduction to build the state graph\n"
          "  --nc <file>     Never claim\n"
          "  --psym <file>   Propositional symbol definition file\n"
          "  --ltl_f <ltl>   LTL formula\n"
          "  --translate     Output Translated C code -- under construction\n"
          "  --version       Prints version and exits.\n"
          "  --help          This Help.\n"
          );
  exit(1);
}

static void version(void)
{
  printf("The Slim LMNtal Implementation, version %s\n", SLIM_VERSION);
  exit(1);
}

static int parse_options(int argc, char *argv[])
{
  int c, option_index;

  struct option long_options[] = {
    {"version", 0, 0, 1000},
    {"help",    0, 0, 1001},
    {"showproxy",  0, 0, 1002},
    {"hideruleset",  0, 0, 1003},
    {"dot", 0, 0, 1004},
    {"nd", 0, 0, 1005},
    {"nd_result", 0, 0, 1006},
    {"nd_dump", 0, 0, 1007},
    {"ltl", 0, 0, 1008},
    {"ltl_all", 0, 0, 1009},
    {"nc", 1, 0, 1010},
    {"psym", 1, 0, 1011},
    {"ltl_f", 1, 0, 1012},
    {"translate", 0, 0, 1013},
    {"ltl_nd", 0, 0, 1014},
    {"por", 0, 0, 1015},
    {0, 0, 0, 0}
  };

  while ((c = getopt_long(argc, argv, "+dtI:O::", long_options, &option_index)) != -1) {
    switch (c) {
    case 0:
      printf("log_options entries must have positive 4th member.\n");
      exit(1);
      break;
    case 'd': /* 開発用. dumpの表示を開発用にする */
      lmn_env.output_format = DEV;
      break;
    case 't': /* trace mode */
      lmn_env.trace = TRUE;
      break;
    case 1000: version(); break;
    case 1001: /* help */ /*FALLTHROUGH*/
    case '?': usage(); break;
    case 1002:
      lmn_env.show_proxy = TRUE;
      break;
    case 1003:
      lmn_env.show_ruleset = FALSE;
      break;
    case 1004:
      lmn_env.output_format = DOT;
      break;
    case 1005:
      lmn_env.nd = TRUE;
      break;
    case 1006:
      lmn_env.nd = TRUE;
      lmn_env.nd_result = TRUE;
      break;
    case 1007:
      lmn_env.nd = TRUE;
      lmn_env.nd_dump = TRUE;
      break;
    case 1008:
      lmn_env.ltl = TRUE;
      break;
    case 1009:
      lmn_env.ltl = TRUE;
      lmn_env.ltl_all = TRUE;
      break;
    case 1010:
      lmn_env.automata_file = optarg;
      break;
    case 1011:
      lmn_env.propositional_symbol = optarg;
      break;
    case 1012:
      lmn_env.ltl_exp = optarg;
      break;
    case 1013:
      lmn_env.translate = TRUE;
      break;
    case 1014:
      lmn_env.ltl = TRUE;
      lmn_env.ltl_all = TRUE;
      lmn_env.ltl_nd = TRUE;
      break;
    case 1015:
      lmn_env.por = TRUE;
      break;
    case 'I':
      lmn_env.load_path[lmn_env.load_path_num++] = optarg;
      break;
    case 'O':
      /* -Oに引数が付かない場合 optargは 0 に設定される */
      if (optarg) {
        if (isdigit(optarg[0])) {
          int l = optarg[0] - '0';
          lmn_env.optimization_level = l <= OPTIMIZE_LEVEL_MAX ? l : OPTIMIZE_LEVEL_MAX;
        } else {
          fprintf(stderr, "invalid argument: -O %s\n", optarg);
          exit(EXIT_FAILURE);
        }
      } else {
        lmn_env.optimization_level = 1;
      }
      break;
    default:
      printf("?? getopt returned character code 0x%x ??\n", c);
      exit(1);
      break;
    }
  }

  return optind;
}

/* lmn_env構造体の初期化 */
static void init_env(void)
{
  lmn_env.trace = FALSE;
  lmn_env.show_proxy = FALSE;
  lmn_env.show_ruleset = TRUE;
  lmn_env.output_format = DEFAULT;
  lmn_env.nd = FALSE;
  lmn_env.nd_result = FALSE;
  lmn_env.nd_dump = FALSE;
  lmn_env.ltl = FALSE;
  lmn_env.ltl_all = FALSE;
  lmn_env.ltl_nd = FALSE;
  lmn_env.por = FALSE;
  lmn_env.translate = FALSE;
  lmn_env.optimization_level = 0;
  lmn_env.load_path_num = 0;
  lmn_env.automata_file = NULL;
  lmn_env.propositional_symbol = NULL;
  lmn_env.ltl_exp = NULL;
}

void init_default_system_ruleset();
void init_rules();
void destroy_rules();
void sym_tbl_destroy();
void sym_tbl_init();

/* 処理系内部の初期化処理 */
static void init_internal(void)
{
  init_env();
  sym_tbl_init();
  lmn_functor_tbl_init();
  init_rules();

  init_default_system_ruleset();
  task_init();
/*   ext_init(); */

  ccallback_init();
  init_builtin_extensions();
#ifdef PROFILE
  runtime_status_init();
#endif
}

static void finalize(void)
{
  sym_tbl_destroy();
  lmn_functor_tbl_destroy();
  destroy_rules();
  task_finalize();
  free_atom_memory_pools();
/*   ext_finalize(); */
  
#ifdef PROFILE
  runtime_status_finalize();
#endif

}

int main(int argc, char *argv[])
{
  int optid;
  int i;

  init_internal();

  optid = parse_options(argc, argv);

  if (optid < argc) {
    FILE *in;
    char *f = argv[optid];
    LmnRuleSet start_ruleset;

    if (!strcmp("-", f)) {
      in = stdin;
      start_ruleset = load(stdin);
    }
    else start_ruleset = load_file(f);

    /* load directories(system & load path) */
    load_il_files(SLIM_LIB_DIR);
    for (i = lmn_env.load_path_num-1; i >= 0; i--) {
      load_il_files(lmn_env.load_path[i]);
    }

#ifdef PROFILE
      status_start_running();
#endif

    if (lmn_env.ltl) {
      Automata automata;
      PVector prop_defs;
      int r;

      r = mc_load_property(&automata, &prop_defs);
      if (!r) {
        run_mc(start_ruleset, automata, prop_defs);
      } else {
        mc_explain_error(r);
      }

      automata_free(automata);
      propsyms_free(prop_defs);
    }
    else if (lmn_env.translate) {
      if (!strcmp("-", f)) {
        in = stdin;
        translate(NULL, stdin);
      }
      else{
        FILE *fp = fopen_il_file(f);
        translate(f, fp);
        fclose(fp);
      }
    }
    else if (lmn_env.nd) {
      run_nd(start_ruleset);
    } else {
      /* シミュレーション実行 */
      lmn_run(start_ruleset);
    }
  } else {
    fprintf(stderr, "no input file\n");
    exit(1);
  }

#ifdef PROFILE
      status_finish_running();
#endif

#ifdef PROFILE
  output_runtime_status(stdout);
  output_hash_conflict(stdout);
#endif

  finalize();
  return 0;
}
