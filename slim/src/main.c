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
#include "nd.h"
#include "ccallback.h"
#include "special_atom.h"
#include "slim_header/string.h"
#include "slim_header/port.h"
#include "dumper.h"
#include "mem_encode.h"
/* #include "ext.h" */
#include "runtime_status.h"

#ifdef USE_JNI
#include "jni_lmntal.h"
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
          "  -p[<0-3>] (-p=-p1)  Profiling level.\n"
          "  -t              trace mode\n"
          "  --showproxy     Show proxy atoms\n"
//          "  --showrule      Show details of ruleset (@RS_ID/[NAME1\"HIS1\"\"HIS2\"..][NAME2\"\"..]..)\n"
          "  --hideruleset   Hide ruleset from result\n"
          "  --dot           Output result in dot language\n"
          "  --nd            Nondeterministic execution mode, print all execution paths.\n"
          "  --ltl           LTL model checking mode\n"
          "  --ltl_all       LTL model checking mode, print all errors\n"
          "  --ltl_nd        --ltl_all and print all seached states and paths\n"
          "  --mem-enc       Use canonical membrane representation in ND and LTL MC.\n"
          "  --compact-stack Compress states on stack in ND and LTL MC.\n"
          "  --por           Enable partial order reduction to build the state graph\n"
          "  --nc <file>     Never claim\n"
          "  --psym <file>   Propositional symbol definition file\n"
          "  --ltl_f <ltl>   LTL formula\n"
          "  --translate     Output translated C code\n"
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
    {"showrule", 0, 0, 1016},
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
    {"interactive", 0, 0, 1400},
    {"bfs", 0, 0, 1500},
    {"bfs_depth", 1, 0, 1501},
    {"bfs_final", 0, 0, 1502},
    {"mem-enc", 0, 0, 2000},
    {"compact-stack", 0, 0, 2001},
    {"opt-hash", 0, 0, 2003},
    {"no_dump", 0,0, 5000},
    {"benchmark_dump", 0, 0, 5001},
    {0, 0, 0, 0}
  };

  while ((c = getopt_long(argc, argv, "+dtI:O::p::", long_options, &option_index)) != -1) {
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
    case 'p':
      if (optarg) {
        if (isdigit(optarg[0])) {
          int l = optarg[0] - '0';
          lmn_env.profile_level = l <= 3 ? l : 3;
        } else {
          fprintf(stderr, "invalid argument: -p %s\n", optarg);
          exit(EXIT_FAILURE);
        }
      } else {
          lmn_env.profile_level = 1;
      }

#ifndef PROFILE
      if (lmn_env.profile_level > 1) {
        fprintf(stderr, "please configure with --enable-profile\n");
        exit(EXIT_FAILURE);
      }
#endif
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
    case 1016:
      lmn_env.show_rule = TRUE;
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
    case 1400: /* jni interactive mode */
#ifdef USE_JNI
      lmn_env.interactive = TRUE;
#else
      printf("Sorry, the interactive mode is disabled.\n");
      printf("Check the --enable-jni option at configure.\n");
#endif
      break;
    case 1500:
      lmn_env.bfs = TRUE;
      break;
    case 1501:
      lmn_env.bfs = TRUE;
      lmn_env.bfs_depth = atoi(optarg);
      break;
    case 1502:
      lmn_env.bfs = TRUE;
      lmn_env.bfs_final = TRUE;
      break;
    case 2000:
      lmn_env.mem_enc = TRUE;
      break;
    case 2001:
      lmn_env.compact_stack = TRUE;
      break;
    case 2003:
      lmn_env.optimize_hash_value = TRUE;
      break;
    case 5000: /* 状態遷移グラフのdumpをしない */
      lmn_env.dump = FALSE;
      break;
#ifdef PROFILE
    case 5001: /* 性能測定時のデータ収集用に仮設. 無視してください(gocho) */
      lmn_env.benchmark = TRUE;
      break;
#endif
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
  lmn_env.show_rule = FALSE;
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
  lmn_env.profile_level = 0;
  lmn_env.load_path_num = 0;
  lmn_env.automata_file = NULL;
  lmn_env.propositional_symbol = NULL;
  lmn_env.ltl_exp = NULL;
  lmn_env.bfs = FALSE;
  lmn_env.bfs_depth = 0;
  lmn_env.bfs_final = FALSE;
  lmn_env.bfs_has_final = FALSE;
  lmn_env.mem_enc = FALSE;
  lmn_env.compact_stack = FALSE;
  lmn_env.optimize_hash_value = FALSE;
  lmn_env.dump = TRUE;
  lmn_env.benchmark = FALSE;
#ifdef USE_JNI
  /* only jni-interactive mode */
  lmn_env.interactive = FALSE;
  lmn_env.normal_remain = FALSE;
  lmn_env.normal_remaining = FALSE;
  lmn_env.normal_cleaning = FALSE;
  lmn_env.nd_remain = FALSE;
  lmn_env.nd_remaining = FALSE;
  lmn_env.nd_cleaning = FALSE;
#endif
}

void init_default_system_ruleset();
void init_rules();
void destroy_rules();
void sym_tbl_destroy();
void sym_tbl_init();

/* 処理系内部の初期化処理 */
static void init_internal(void)
{
  sym_tbl_init();
  lmn_functor_tbl_init();
  init_rules();

  if(! lmn_env.translate){
    init_so_handles();
    init_default_system_ruleset();
    task_init();
    mem_isom_init();
    /*   ext_init(); */
    sp_atom_init();
    ccallback_init();
    init_builtin_extensions();

    dumper_init();
    string_init();
    port_init();

    if (lmn_env.profile_level > 0) {
      runtime_status_init();
    }
  }
}

static void finalize(void)
{
  if(! lmn_env.translate){
    port_finalize();
    string_finalize();
    dumper_finalize();

    task_finalize();
    mem_isom_finalize();
    /*   ext_finalize(); */
    ccallback_finalize();
    sp_atom_finalize();

    finalize_so_handles();

    if (lmn_env.profile_level > 0) {
      runtime_status_finalize();
    }
  }

  destroy_rules();
  lmn_functor_tbl_destroy();
  sym_tbl_destroy();
  free_atom_memory_pools();
}

int main(int argc, char *argv[])
{
  int optid;
  int i;

  init_env();
  optid = parse_options(argc, argv);
  init_internal();

  if (optid < argc) {
    Vector *start_rulesets = vec_make(2);

    /* load inputfiles */
    for(i=optid; i<argc; ++i){
      FILE *in;
      char *f = argv[i];
      LmnRuleSet t;

      if (!strcmp("-", f)) {
        in = stdin;
        t = load(stdin);
        vec_push(start_rulesets, (vec_data_t)t);
      }
      else{
        t = load_file(f);
        if(t) vec_push(start_rulesets, (vec_data_t)t);
      }
    }
    /* まともな入力ファイルが無ければ抜ける(終了処理してないけど) */
    if(vec_num(start_rulesets) == 0){
      fprintf(stderr, "bad input file.\n");
      exit(1);
    }

    if (lmn_env.profile_level >= 1) {
      status_start_running();
    }

    if (lmn_env.translate) { /*変換をする場合*/
      if (!strcmp("-", argv[optid])) { /* argv[optid] is first input file name */
        translate(NULL);
      }
      else{
        translate(argv[optid]);
      }
    }
    else{ /*実行をする場合*/
      /* load directories(system & load path) */
      load_il_files(SLIM_LIB_DIR);
      for (i = lmn_env.load_path_num-1; i >= 0; i--) {
        load_il_files(lmn_env.load_path[i]);
      }

      if (lmn_env.ltl) {
        Automata automata;
        PVector prop_defs;
        int r;

        r = mc_load_property(&automata, &prop_defs);
        if (!r) {
          run_mc(start_rulesets, automata, prop_defs);
        } else {
          mc_explain_error(r);
        }

        automata_free(automata);
        propsyms_free(prop_defs);
      }
      else if (lmn_env.nd) {
        run_nd(start_rulesets);
      } else {
        /* シミュレーション実行 */
        lmn_run(start_rulesets);
      }
    }

    vec_free(start_rulesets);
#ifdef USE_JNI
  } else if (lmn_env.interactive) {
  // no file, but requested interactive mode
  run_jni_interactive();
#endif
  } else {
    fprintf(stderr, "no input file\n");
    exit(1);
  }

  if (lmn_env.profile_level >= 1) {
    status_finish_running();
    output_runtime_status(stderr);
  }

  finalize();
  return 0;
}
