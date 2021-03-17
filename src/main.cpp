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

#include "arch.h"
#include "element/element.h"
#include "ffi/jni_lmntal.h"
#include "ffi/lmntal_system_adapter.h"
#include "lmntal.h"
#include "loader/loader.h"
#include "verifier/verifier.h"
#include "vm/vm.h"
#include <ctype.h>
#include <getopt.h>
#include <unistd.h>
/* #include "ext.h" */
#include "verifier/runtime_status.h"

#ifdef USE_CUNIT
#include "test/unit_test.h"
#endif

#include <vector>

void install_builtin_extensions(void);
void init_builtin_extensions(void); /* ext/init_exts.c */

void init_default_system_ruleset();
void init_rules();
void destroy_rules();
void sym_tbl_destroy();
void sym_tbl_init();

static void usage(void) {
  fprintf(
      stderr,
      "Usage: slim [OPTION]... FILE\n"
      "     When FILE is  -, read standard input.\n"
      "options:\n"
      "  -I<path>             Adds <path> to the head of the load path list.\n"
      "  -O[<0-9>] (-O=-O3)   Optimization level. [DEFAULT:-O3]\n"
      "                       Intermediate instruction sequences are "
      "optimized.\n"
      "  -p[<0-3>] (-p=-p1)   Profiler level.\n"
      "  --use-builtin-rule   Load the rules builtin this application for "
      "arithmetic, nlmem, etc\n"
      "  --findatomopt        optimization with atoms for history management\n"
      "  --nd                 Change the execution mode from RunTime(RT) to "
      "ModelChecker(MC)\n"
      "  --translate          Change the execution mode to Output translated C "
      "from LMNtal\n"
      "  -t                   (RT) Show execution path\n"
      "                       (MC) Show state space\n"
      "  --hide-ruleset       Hide ruleset from result\n"
      "  --hl                 (RT) Allow using hyperlink system\n"
      "  --show-proxy         Show proxy atoms\n"
      "  --show-chr           Show applied history in uniq rulesets "
      "(constraint handling rules)\n"
      "  --show-transition    (MC) Show transition information in state "
      "transition graph\n"
      "  --show-ends          (MC) Show all of terminated states\n"
      "  --show-hl            (RT) Show all hyperlinks details\n"
      "  --dump-dot           (RT) Print format: DOT language (LMNtal "
      "hierarchical graph)\n"
      "                       (MC) Print format: DOT language (State "
      "Transition graph) \n"
      "  --dump-lavit         (MC) Print format: LaViT - LMNtal IDE (State "
      "Transition Graph)\n"
      "  --dump-inc           (MC) State Generation and Output of states at "
      "the same time\n"
      "  --dump-json          Print format: JSON\n"
      "  --nc <file>          (MC) Input <file> as a property automata (LTL2BA "
      "format)\n"
      "  --psym <file>        (MC) Input <file> as propositional symbol "
      "definitions\n"
      "  --ltl                (MC) Do LTL model checking (need --psym, --nc)\n"
      "  --ltl-all            (MC) Generate full state space and exhaustive "
      "search\n"
      "  --bfs                (MC) Use BFS strategy\n"
      "  --bfs-lsync          (MC) Use Layer Synchronized BFS strategy\n"
      "  --limited-step=<N>   (MC) Run only first <N> steps (BFS)\n"
      "  --use-owcty          (MC) Use OWCTY algorithm  (LTL model checking)\n"
      "  --use-map            (MC) Use MAP algorithm    (LTL model checking)\n"
      "  --use-mapndfs        (MC) Use Map+NDFS algorithm (LTL model "
      "checking)\n"
#ifndef MINIMAL_STATE
      "  --use-mcndfs         (MC) Use Multicore NDFS algorithm (LTL model "
      "checking)\n"
#endif
      "  --use-bledge         (MC) Use BLEDGE algorithm (LTL model checking)\n"
      "  --disable-map-h      (MC) No use MAP heuristics(LTL model checking)\n"
      "  --pscc-driven        (MC) Use SCC analysis of property automata (LTL "
      "model checking)\n"
      "  --use-Ncore=<N>      (MC) Use <N>threads\n"
      "  --delta-mem          (MC) Use delta membrane generator\n"
      "  --tree-compress=<N>  (MC) Use Tree Compression with 2^N table size "
      "default(N=20)\n"
      "  --hash-compaction    (MC) Use Hash Compaction\n"
      "  --hash-depth=<N>     (MC) Set <N> Depth of Hash Function\n"
      "  --mem-enc            (MC) Use canonical membrane representation\n"
      "  --ltl-f <ltl>        (MC) Input <ltl> formula directly. (need LTL2BA "
      "env)\n"
      "  --visualize          (MC) Output information for visualize\n"
      "  --run-test           Run CUnit\n"
      "  --version            Prints version and exits.\n"
      "  --help               This Help.\n");
  exit(1);
}

void ver_print_with_esc_code(FILE *f, char *str, int color) {
  esc_code_add_f(f, CODE__UNDER_LINE);
  esc_code_add_f(f, color);
  fprintf(f, "%s", str);
  esc_code_clear_f(f);
}

void slim_version(FILE *f) {
  // ver_print_with_esc_code(f, "S", CODE__FORECOLOR_LIGHTBLUE);
  // fprintf(f, "lim ");
  // ver_print_with_esc_code(f, "L", CODE__FORECOLOR_LIGHTBLUE);
  // fprintf(f, "mntal ");
  // ver_print_with_esc_code(f, "IM", CODE__FORECOLOR_LIGHTBLUE);
  fprintf(f, "Slim Lmntal IMplementation ");
  fprintf(f, "- version %s (%s)\n", SLIM_VERSION, COMMIT_ID);
}

static void parse_options(int *optid, int argc, char *argv[]) {
  int c, option_index;

  struct option long_options[] = {{"version", 0, 0, 1000},
                                  {"help", 0, 0, 1001},
                                  {"show-proxy", 0, 0, 1002},
                                  {"hide-ruleset", 0, 0, 1003},
                                  {"show-chr", 0, 0, 1004},
                                  {"show-transition", 0, 0, 1005},
                                  {"show-ends", 0, 0, 1006},
                                  {"show-hl", 0, 0, 1007},
                                  {"use-builtin-rule", 0, 0, 1008},
                                  {"dump-dot", 0, 0, 1100},
                                  {"dump-fsm-lmn", 0, 0, 1101},
                                  {"dump-lavit", 0, 0, 1102},
                                  {"dump-inc", 0, 0, 1103},
                                  {"dump-lmn", 0, 0, 1104},
                                  {"dump-json", 0, 0, 1105},
                                  {"dump-fsm-lmn-detail", 0, 0, 1106},
                                  {"dump-fsm-hl", 0, 0, 1107},
                                  {"interactive", 0, 0, 1200},
                                  {"translate", 0, 0, 1300},
                                  {"hl", 0, 0, 1350},
                                  {"ltl-all", 0, 0, 1400},
                                  {"ltl", 0, 0, 1401},
                                  {"nd", 0, 0, 1402},
                                  {"opt-min", 0, 0, 1403},
                                  {"opt-max", 0, 0, 1404},
                                  {"nc", 1, 0, 1410},
                                  {"psym", 1, 0, 1411},
                                  {"ltl-f", 1, 0, 1412},
                                  {"pscc-driven", 0, 0, 1413},
                                  {"por-old", 0, 0, 1419},
                                  {"por", 0, 0, 1420},
                                  {"bfs", 0, 0, 1421},
                                  {"limited-step", 1, 0, 1422},
                                  {"search-ends", 0, 0, 1423},
                                  {"mem-enc", 0, 0, 2000},
                                  {"disable-compress", 0, 0, 2003},
                                  {"delta-mem", 0, 0, 2005},
                                  {"z-compress", 0, 0, 2007},
                                  {"d-compress", 0, 0, 2008},
                                  {"r-compress", 0, 0, 2009},
                                  {"use-owcty", 0, 0, 3000},
                                  {"use-map", 0, 0, 3001},
                                  {"use-bledge", 0, 0, 3002},
                                  {"bfs-lsync", 0, 0, 3003},
                                  {"use-mapndfs", 0, 0, 3004},
#ifndef MINIMAL_STATE
                                  {"use-mcndfs", 0, 0, 3005},
#endif
                                  {"disable-map-h", 0, 0, 3100},
                                  {"use-Ncore", 1, 0, 5000},
                                  {"cutoff-depth", 1, 0, 5001},
                                  {"independent", 0, 0, 5002},
                                  {"disable-loadbalancer", 0, 0, 5015},
                                  {"opt-lock", 0, 0, 5025},
                                  {"disable-opt-hash", 0, 0, 5026},
                                  {"opt-hash-old", 0, 0, 5027},
                                  {"no-dump", 0, 0, 6000},
                                  {"benchmark-dump", 0, 0, 6001},
                                  {"property-dump", 0, 0, 6002},
                                  {"debug-id", 0, 0, 6007},
                                  {"debug-delta", 0, 0, 6008},
                                  {"debug-hash", 0, 0, 6009},
                                  {"debug-isomor", 0, 0, 6010},
                                  {"debug-mc", 0, 0, 6011},
                                  {"debug-por", 0, 0, 6012},
                                  {"show-rgraph", 0, 0, 6013},
                                  {"debug-tr-dep", 0, 0, 6014},
                                  {"prof-nomemeq", 0, 0, 6050},
                                  {"visualize", 0, 0, 6100},
                                  {"hash-compaction", 0, 0, 6060},
                                  {"hash-depth", 1, 0, 6061},
                                  {"tree-compress", 1, 0, 6062},
                                  {"run-test", 0, 0, 6070},
				  {"findatomopt", 0, 0, 6071},
                                  {0, 0, 0, 0}};

  while ((c = getopt_long(argc, argv, "+dvhtI:O::p::", long_options,
                          &option_index)) != -1) {
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
        if (isdigit((unsigned char)optarg[0])) {
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
      if (lmn_env.profile_level > 2) {
        fprintf(stderr, "please configure with --enable-profile\n");
        exit(EXIT_FAILURE);
      }
#endif
      break;
    case 'v':
    case 1000:
      slim_version(stdout);
      exit(1);
      break;
    case 'h':
    case 1001: /* help */ /* FALLTHROUGH */
    case '?':
      usage();
      break;
    case 1002:
      lmn_env.show_proxy = TRUE;
      break;
    case 1003:
      lmn_env.show_ruleset = FALSE;
      break;
    case 1004:
      lmn_env.show_chr = TRUE;
      break;
    case 1005:
      lmn_env.show_transition = TRUE;
      break;
    case 1006:
      lmn_env.end_dump = TRUE;
      break;
    case 1007:
      lmn_env.show_hyperlink = TRUE;
      break;
    case 1008:
      lmn_env.load_path[lmn_env.load_path_num++] = SLIM_LIB_DIR;
      break;
    case 1100:
      lmn_env.output_format = DOT;
      lmn_env.mc_dump_format = Dir_DOT;
      break;
    case 1101:
      lmn_env.mc_dump_format = LMN_FSM_GRAPH_MEM_NODE;
      break;
    case 1102:
      lmn_env.mc_dump_format = LaViT;
      break;
    case 1103:
      lmn_env.sp_dump_format = INCREMENTAL;
      break;
    case 1104:
      lmn_env.sp_dump_format = LMN_SYNTAX;
      break;
    case 1105:
      lmn_env.output_format = JSON;
      break;
    case 1106:
      lmn_env.mc_dump_format = LMN_FSM_GRAPH;
      break;
    case 1107:
      lmn_env.mc_dump_format = LMN_FSM_GRAPH_HL_NODE;
      break;
    case 1200: /* jni interactive mode */
#ifdef HAVE_JNI_H
      lmn_env.interactive = TRUE;
#else
      fprintf(stderr, "Sorry, the interactive mode is disabled.\n");
      fprintf(stderr, "please configure with --enable-jni\n");
      exit(EXIT_FAILURE);
#endif
      break;
    case 1300:
      lmn_env.translate = TRUE;
      break;
    case 1350:
      lmn_env.hyperlink = TRUE;
      break;
    case 1400:
      lmn_env.ltl_all = TRUE; /* FALLTHROUGH */
    case 1401:
      lmn_env.ltl = TRUE; /* FALLTHROUGH */
    case 1402:
      lmn_env.nd = TRUE;
      break;
    case 1403:
      lmn_env.nd = TRUE;
      lmn_env.opt_mode = OPT_MINIMIZE;
      lmn_env.show_transition = TRUE;
      break;
    case 1404:
      lmn_env.nd = TRUE;
      lmn_env.opt_mode = OPT_MAXIMIZE;
      lmn_env.show_transition = TRUE;
      break;
    case 1410:
      lmn_env.automata_file = optarg;
      break;
    case 1411:
      lmn_env.propositional_symbol = optarg;
      break;
    case 1412:
      lmn_env.ltl_exp = optarg;
      break;
    case 1413:
      lmn_env.prop_scc_driven = TRUE;
      break;
    case 1419:
      lmn_env.enable_por_old = TRUE;
      lmn_env.enable_por = TRUE;
      break;
    case 1420:
      lmn_env.delta_mem = TRUE; /* 新PORはdelta-mem方式に依存 */
      lmn_env.enable_por = TRUE;
      break;
    case 1421:
      lmn_env.bfs = TRUE;
      break;
    case 1422:
      lmn_env.bfs_layer_sync = TRUE;
      lmn_env.depth_limits = atoi(optarg);
      break;
    case 1423:
      lmn_env.nd_search_end = TRUE;
      break;
    case 2000:
      lmn_env.mem_enc = TRUE;
      break;
    case 2003:
      lmn_env.enable_compress_mem = FALSE;
      break;
    case 2005:
      lmn_env.delta_mem = TRUE;
      break;
    case 2007:
#ifdef HAVE_LIBZ
      lmn_env.z_compress = TRUE;
#else
      fprintf(stderr, "Sorry, z library cannot be found on your environment\n");
      fprintf(stderr,
              "if you installed z library, please re-configure & make slim\n");
      exit(EXIT_FAILURE);
#endif
      break;
    case 2008:
      lmn_env.d_compress = TRUE;
      break;
    case 2009:
      lmn_env.r_compress = TRUE;
      break;
    case 3000:
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_owcty = TRUE;
      break;
    case 3001:
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_map = TRUE;
      lmn_env.enable_map_heuristic = FALSE;
      break;
    case 3002:
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_bledge = TRUE; /* FALLTROUGH */
    case 3003:
      lmn_env.bfs = TRUE;
      lmn_env.bfs_layer_sync = TRUE;
      break;
    case 3004:
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_mapndfs = TRUE;
      // lmn_env.enable_map_heuristic = FALSE;
      break;
#ifndef MINIMAL_STATE
    case 3005:
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_mcndfs = TRUE;
      // lmn_env.enable_map_heuristic = FALSE;
      break;
#endif
    case 3100:
      lmn_env.enable_map_heuristic = FALSE;
      break;
#ifdef ENABLE_PARALLEL
    case 5000: /* parallel */
    {
      int core = atoi(optarg);
      if (core > 1) {
        lmn_env.core_num = core;
        env_set_threads_num(core);
      }
      if (core != 0)
        lmn_env.enable_parallel = TRUE;
      break;
    }
    case 5001: {
      int cut = atoi(optarg);
      if (cut > 1) {
        lmn_env.cutoff_depth = cut;
      }
      break;
    }
    case 5002:
      lmn_env.findatom_parallel_inde = TRUE;
      break;
    case 5015: /* optimize Load Balancing */
      lmn_env.optimize_loadbalancing = FALSE;
      break;
    case 5025: /* optimize lock under constructions.. */
      lmn_env.optimize_lock = TRUE;
      break;
#else
    case 5000:
    case 5001:
    case 5015:
    case 5025:
      fprintf(
          stderr,
          "Sorry, parallel execution is not supported on your environment.\n");
      fprintf(stderr, "Requirement: GCC keyword __thread, pthread library \n");
      exit(EXIT_FAILURE);
      break;
#endif
    case 5026:
      lmn_env.optimize_hash = FALSE;
      break;
    case 5027:
#ifdef PROFILE
      lmn_env.optimize_hash = FALSE;
      lmn_env.optimize_hash_old = TRUE;
#else
      usage();
#endif
      break;
    case 6000:
      lmn_env.dump = FALSE;
      break;
    case 6001: /* 性能測定時のデータ収集用に仮設. 無視してください(gocho) */
      lmn_env.benchmark = TRUE;
      lmn_env.dump = FALSE;
      lmn_env.end_dump = FALSE;
      break;
    case 6002:
      lmn_env.property_dump = TRUE;
      break;
#ifdef DEBUG
    case 6007:
      lmn_env.debug_id = TRUE;
      break;
    case 6008:
      lmn_env.debug_delta = TRUE;
      break;
    case 6009:
      lmn_env.debug_hash = TRUE;
      break;
    case 6010:
      lmn_env.debug_isomor = TRUE;
      break;
    case 6011:
      lmn_env.debug_mc = TRUE;
      break;
    case 6012:
      lmn_env.debug_por = TRUE;
      break;
    case 6013:
      lmn_env.show_reduced_graph = TRUE;
      lmn_env.show_transition = TRUE;
      break;
    case 6014:
      lmn_env.debug_por_dep = TRUE;
      lmn_env.enable_por = TRUE;
      break;
#else
    case 6007:
    case 6008:
    case 6009:
    case 6010:
    case 6011:
    case 6012:
    case 6013:
    case 6014:
      usage();
      break;
#endif

#ifdef PROFILE
    case 6050:
      lmn_env.prof_no_memeq = TRUE;
      break;
#else
    case 6050:
      usage();
      break;
#endif
    case 6100:
      lmn_env.enable_visualize = TRUE;
      break;
    case 6060:
      lmn_env.hash_compaction = TRUE;
      break;
    case 6061: {
      int depth = atoi(optarg);
      if (depth > 0) {
        lmn_env.hash_depth = depth;
      }
      break;
    }
    case 6062: {
      lmn_env.hash_compaction = FALSE;
      lmn_env.tree_compress = TRUE;
      int size = atoi(optarg);
      if (size >= 15) {
        lmn_env.tree_compress_table_size = size;
      } else {
        lmn_env.tree_compress_table_size = 15;
      }
      break;
    }
    case 6070:
      lmn_env.run_test = TRUE;
      break;
    case 6071:
      slimopt_test_flag = true;
      break;
    case 'I':
      lmn_env.load_path[lmn_env.load_path_num++] = optarg;
      break;
    case 'O':
      /* -Oに引数が付かない場合 optargは 0 に設定される */
      if (optarg) {
        if (isdigit((unsigned char)optarg[0])) {
          int l = optarg[0] - '0';
          lmn_env.optimization_level =
              l <= OPTIMIZE_LEVEL_MAX ? l : OPTIMIZE_LEVEL_MAX;
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

  (*optid) = optind;
}

/* 処理系内部の初期化処理 */
static void init_internal(void) {
  lmn_profiler_init(lmn_env.core_num);
  sym_tbl_init();
  lmn_functor_table = new LmnFunctorTable();
  init_rules();

  if (!lmn_env.translate) {
    init_so_handles();
    init_default_system_ruleset();
    if (lmn_env.enable_por)
      dpor_env_init();
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

static inline void slim_init(int *optid, int argc, char **argv) {
  int i;

  lmn_stream_init();
  parse_options(optid, argc, argv);
  init_internal();

  /** load directories(system & load path) */
  for (i = lmn_env.load_path_num - 1; i >= 0; i--) {
    load_il_files(lmn_env.load_path[i]);
  }
}

static inline void slim_finalize(void) {
  if (!lmn_env.translate) {
    port_finalize();
    string_finalize();
    dumper_finalize();

    if (lmn_env.enable_por)
      dpor_env_destroy();
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

static inline int load_input_files(std::vector<LmnRuleSetRef> &start_rulesets, int optid, int argc,
                                   char **argv) {
  int i;

  /** load input files */
  for (i = optid; i < argc; i++) {
    LmnRuleSetRef t;
    char *f = argv[i];

    try {
      if (!strcmp("-", f)) { /* 標準入力からの読込み */
        t = load(std::unique_ptr<FILE, decltype(&fclose)>(stdin, [](FILE *) -> int { return 0; }));
        start_rulesets.push_back(t);
      } else {
        t = load_file(f);
        if (t)
          start_rulesets.push_back(t);
      }
    }
    catch (const slim::loader::exception &e) {
      fprintf(stderr, "loader error: %s\n", e.what());
      return 0;
    }
  }

  if (start_rulesets.empty()) {
    /** detected invalid file */
    fprintf(stderr, "bad input file.\n");
    return 0;
  } else {
    return 1;
  }
}

static inline void slim_exec(const std::vector<LmnRuleSetRef> &start_rulesets) {
  if (!lmn_env.nd) {
    /* プログラム実行 */
    lmn_run(new Vector(start_rulesets));
  } else {
    /* プログラム検証 */
    AutomataRef automata;
    PVector prop_defs;
    int ret;

    automata = NULL;
    prop_defs = NULL;
    ret = 1;

    if ((lmn_env.automata_file || lmn_env.ltl_exp) &&
        lmn_env.propositional_symbol) {
      /* load property automata, definition of atomic propositional symbol */
      ret = mc_load_property(&automata, &prop_defs);
      if (ret) {
        mc_explain_error(ret);
        return;
      } else {
        if (lmn_env.prop_scc_driven)
          automata->analysis();
        if (lmn_env.property_dump) {
          automata->print_property();
          return;
        }
      }
    }

    run_mc(new Vector(start_rulesets), automata, prop_defs);

    if (!ret) {
      delete automata;
      propsyms_free(prop_defs);
    }
  }
}

int main(int argc, char *argv[]) {
  int optid;
  slim_init(&optid, argc, argv);

  if (lmn_env.run_test) {
#ifdef USE_CUNIT
    test_main();
#else
    fprintf(
        stderr,
        "CUnit is disabled. Please configure with --enable-cunit option.\n");
#endif
    return 0;
  }

  if (optid >= argc) {
    /** no input file */
    if (lmn_env.interactive) {
      run_jni_interactive(); /* interactive execution */
    } else {
      fprintf(stderr, "no input file\n");
    }
  } else {
    std::vector<LmnRuleSetRef> start_rulesets;

    if (load_input_files(start_rulesets, optid, argc, argv)) {
      if (lmn_env.translate) { /** lmntalコードからCへの変換実行の場合 */
        /* TODO: 複数ファイル入力への対応 */
        translate(strcmp("-", argv[optid]) ? argv[optid] : NULL);
        /* argv[optid] is first input file name */
      } else {

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
  }

  slim_finalize();
  return 0;
}
