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

#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include "lmntal.h"
#include "util.h"
#include "task.h"
#include "symbol.h"
#include "functor.h"
#include "load.h"
#include "translate.h"
#include "arch.h"
#include "lmntal_system_adapter.h"
#include "automata.h"
#include "propositional_symbol.h"
#include "por.h"
#include "mc.h"
#include "mc_generator.h"
#include "ccallback.h"
#include "special_atom.h"
#include "slim_header/string.h"
#include "slim_header/port.h"
#include "dumper.h"
#include "jni_lmntal.h"
#include "hyperlink.h"//seiji
/* #include "ext.h" */
#include "runtime_status.h"

void install_builtin_extensions(void);
void init_builtin_extensions(void); /* ext/init_exts.c */

static void usage(void)
{
  fprintf(stderr,
          "Usage: slim [OPTION]... FILE\n"
          "     When FILE is  -, read standard input.\n"
          "options:\n"
          "  -I<path>            Adds <path> to the head of the load path list.\n"
          "  -O[<0-9>] (-O=-O1)  Optimization level. [DEFAULT:-O3]\n"
          "                      Intermediate instruction sequences are optimized.\n"
          "  -p[<0-3>] (-p=-p1)  Profiler level.\n"
          "  --nd                Change the execution mode from RunTime(RT) to ModelChecker(MC)\n"
          "  --translate         Change the execution mode to Output translated C from LMNtal\n"
          "  -t                  (RT) Show execution path\n"
          "                      (MC) Show state space\n"
          "  --hide-ruleset      Hide ruleset from result\n"
          "  --hl                (RT) Allow using hyperlink system\n"
          "  --show-proxy        Show proxy atoms\n"
          "  --show-chr          Show applied history in uniq rulesets (constraint handling rules)\n"
          "  --show-transition   (MC) Show transition information in state transition graph\n"
          "  --show-ends         (MC) Show all of terminated states\n"
          "  --show-hl           (RT) Show all hyperlinks details\n"
          "  --dump-dot          (RT) Print format: DOT language (LMNtal hierarchical graph)\n"
	        "                      (MC) Print format: DOT language (State Transition graph) \n"
          "  --dump-lavit        (MC) Print format: LaViT - LMNtal IDE (State Transition Graph)\n"
          "  --dump-inc          (MC) State Generation and Output of states at the same time\n"
          "  --nc <file>         (MC) Input <file> as a property automata (LTL2BA format)\n"
          "  --psym <file>       (MC) Input <file> as propositional symbol definitions\n"
          "  --ltl               (MC) Do LTL model checking (need --psym, --nc)\n"
          "  --ltl-all           (MC) Generate full state space and exhaustive search\n"
          "  --bfs               (MC) Use BFS strategy\n"
          "  --bfs-lsync         (MC) Use Layer Synchronized BFS strategy\n"
          "  --use-owcty         (MC) Use OWCTY algorithm  (LTL model checking)\n"
          "  --use-map           (MC) Use MAP algorithm    (LTL model checking)\n"
          "  --use-bledge        (MC) Use BLEDGE algorithm (LTL model checking)\n"
          "  --disable-map-h     (MC) No use MAP heuristics(LTL model checking)\n"
          "  --pscc-driven       (MC) Use SCC analysis of property automata (LTL model checking)\n"
          "  --use-Ncore=<N>     (MC) Use <N>threads\n"
          "  --delta-mem         (MC) Use delta membrane generator\n"
          "  --mem-enc           (MC) Use canonical membrane representation\n"
          "  --ltl-f <ltl>       (MC) Input <ltl> formula directly. (need LTL2BA env)\n"
          "  --version           Prints version and exits.\n"
          "  --help              This Help.\n"
          );
  exit(1);
}


void ver_print_with_esc_code(FILE *f, char *str, int color)
{
  esc_code_add(CODE__UNDER_LINE);
  esc_code_add(color);
  fprintf(f, "%s", str);
  esc_code_clear();
}

void slim_version(FILE *f)
{
  ver_print_with_esc_code(f, "S", CODE__FORECOLOR_LIGHTBLUE);
  fprintf(f, "lim ");
  ver_print_with_esc_code(f, "L", CODE__FORECOLOR_LIGHTBLUE);
  fprintf(f, "mntal ");
  ver_print_with_esc_code(f, "IM", CODE__FORECOLOR_LIGHTBLUE);
  fprintf(f, "plementation ");
  fprintf(f, "- version %s\n", SLIM_VERSION);
}

static int parse_options(int argc, char *argv[])
{
  int c, option_index;

  struct option long_options[] = {
    {"version"                , 0, 0, 1000},
    {"help"                   , 0, 0, 1001},
    {"show-proxy"             , 0, 0, 1002},
    {"hide-ruleset"           , 0, 0, 1003},
    {"show-chr"               , 0, 0, 1004},
    {"show-transition"        , 0, 0, 1005},
    {"show-ends"              , 0, 0, 1006},
    {"show-hl"                , 0, 0, 1007},//seiji
    {"dump-dot"               , 0, 0, 1100},
    {"dump-fsm"               , 0, 0, 1101},
    {"dump-lavit"             , 0, 0, 1102},
    {"dump-inc"               , 0, 0, 1103},
    {"dump-lmn"               , 0, 0, 1104},
    {"interactive"            , 0, 0, 1200},
    {"translate"              , 0, 0, 1300},
    {"hl"                     , 0, 0, 1350},//seiji
    {"ltl-all"                , 0, 0, 1400},
    {"ltl"                    , 0, 0, 1401},
    {"nd"                     , 0, 0, 1402},
    {"nc"                     , 1, 0, 1410},
    {"psym"                   , 1, 0, 1411},
    {"ltl-f"                  , 1, 0, 1412},
    {"pscc-driven"            , 0, 0, 1413},
    {"por"                    , 0, 0, 1420},
    {"bfs"                    , 0, 0, 1421},
    {"limited-step"           , 1, 0, 1422},
    {"search-ends"            , 0, 0, 1423},
    {"mem-enc"                , 0, 0, 2000},
    {"disable-compress"       , 0, 0, 2003},
    {"disable-compact"        , 0, 0, 2004},
    {"delta-mem"              , 0, 0, 2005},
    {"z-compress"             , 0, 0, 2007},
    {"use-owcty"              , 0, 0, 3000},
    {"use-map"                , 0, 0, 3001},
    {"use-bledge"             , 0, 0, 3002},
    {"bfs-lsync"              , 0, 0, 3003},
    {"disable-map-h"          , 0, 0, 3100},
    {"use-Ncore"              , 1, 0, 5000},
    {"cutoff-depth"           , 1, 0, 5001},
    {"disable-loadbalancer"   , 0, 0, 5015},
    {"opt-lock"               , 0, 0, 5025},
    {"disable-opt-hash"       , 0, 0, 5026},
    {"opt-hash-old"           , 0, 0, 5027},
    {"no-dump"                , 0, 0, 6000},
    {"benchmark-dump"         , 0, 0, 6001},
    {"property-dump"          , 0, 0, 6002},
    {"debug-memenc"           , 0, 0, 6006},
    {"debug-id"               , 0, 0, 6007},
    {"debug-delta"            , 0, 0, 6008},
    {"debug-hash"             , 0, 0, 6009},
    {"debug-isomor"           , 0, 0, 6010},
    {"debug-mc"               , 0, 0, 6011},
    {"prof-nomemeq"           , 0, 0, 6050},
    {0, 0, 0, 0}
  };

  while ((c = getopt_long(argc, argv, "+dvhtI:O::p::", long_options, &option_index)) != -1) {
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
    case 1001: /* help */     /* FALLTHROUGH */
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
    case 1007://seiji
      lmn_env.show_hyperlink = TRUE;
      break;
    case 1100:
      lmn_env.output_format = DOT;
      lmn_env.mc_dump_format = Dir_DOT;
      break;
    case 1101:
      lmn_env.mc_dump_format = FSM;
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
    case 1350://seiji
      lmn_env.hyperlink = TRUE;
      break;
    case 1400:
      lmn_env.ltl_all = TRUE; /* FALLTHROUGH */
      mc_data.do_exhaustive = TRUE;
    case 1401:
      lmn_env.ltl = TRUE;     /* FALLTHROUGH */
      mc_data.do_search = TRUE;
    case 1402:
      lmn_env.nd = TRUE;
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
    case 1420:
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
      /* FALLTHROUH */
    case 2004:
      lmn_env.compact_stack = FALSE;
      break;
    case 2005:
    	lmn_env.delta_mem = TRUE;
    	break;
    case 2007:
#ifdef HAVE_LIBZ
      lmn_env.z_compress = TRUE;
#else
      fprintf(stderr, "Sorry, z library cannot be found on your environment\n");
      fprintf(stderr, "if you installed z library, please re-configure & make slim\n");
      exit(EXIT_FAILURE);
#endif
      break;
    case 3000:
      mc_data.do_parallel = TRUE;
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_owcty = TRUE;
      break;
    case 3001:
      mc_data.do_parallel = TRUE;
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_map = TRUE;
      lmn_env.enable_map_heuristic = FALSE;
      break;
    case 3002:
      mc_data.do_parallel = TRUE;
      lmn_env.enable_parallel = TRUE;
      lmn_env.enable_bledge = TRUE; /* FALLTROUGH */
    case 3003:
      mc_data.do_parallel = TRUE;
      lmn_env.bfs = TRUE;
      lmn_env.bfs_layer_sync = TRUE;
      break;
    case 3100:
      lmn_env.enable_map_heuristic = FALSE;
      break;
#ifdef ENABLE_PARALLEL
    case 5000: /* parallel */
    {
      int core = atoi(optarg);
      if (core > 1) {
        lmn_env.core_num = core;
        mc_data.do_parallel = TRUE;
        lmn_thread_num = core;
      }
      lmn_env.enable_parallel  = TRUE;
      break;
    }
    case 5001:
      dfs_set_cutoff_depth((atoi(optarg) < 1) ? 1U
                                              : atoi(optarg));
      break;
    case 5015: /* optimize Load Balancing */
      lmn_env.optimize_loadbalancing = FALSE;
      break;
    case 5025: /* optimize lock under constructions.. */
      lmn_env.optimize_lock      = TRUE;
      break;
#else
    case 5000:
    case 5001:
    case 5015:
    case 5025:
      fprintf(stderr, "Sorry, parallel execution is not supported on your environment.\n");
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
      lmn_env.dump      = FALSE;
      lmn_env.end_dump  = FALSE;
      break;
    case 6002:
      lmn_env.property_dump = TRUE;
      break;
#ifdef DEBUG
    case 6006:
      lmn_env.debug_memenc = TRUE;
      break;
    case 6007:
      lmn_env.debug_id    = TRUE;
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
#else
    case 6006:
    case 6007:
    case 6008:
    case 6009:
    case 6010:
    case 6011:
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
  lmn_env.trace                  = FALSE;
  lmn_env.show_proxy             = FALSE;
  lmn_env.show_chr               = FALSE;
  lmn_env.show_ruleset           = TRUE;
  lmn_env.output_format          = DEFAULT;
  lmn_env.mc_dump_format         = CUI;
  lmn_env.sp_dump_format         = SP_NONE;
  lmn_env.nd                     = FALSE;
  lmn_env.ltl                    = FALSE;
  lmn_env.ltl_all                = FALSE;
  lmn_env.enable_por             = FALSE;
  lmn_env.show_transition        = FALSE;
  lmn_env.translate              = FALSE;
  lmn_env.optimization_level     = 3;
  lmn_env.profile_level          = 0;
  lmn_env.load_path_num          = 0;
  lmn_env.automata_file          = NULL;
  lmn_env.propositional_symbol   = NULL;
  lmn_env.ltl_exp                = NULL;
  lmn_env.bfs                    = FALSE;
  lmn_env.prop_scc_driven        = FALSE;
  lmn_env.depth_limits           = UINT_MAX;
  lmn_env.nd_search_end          = FALSE;
  lmn_env.mem_enc                = FALSE;
  lmn_env.compact_stack          = TRUE;
  lmn_env.delta_mem              = FALSE;
  lmn_env.dump                   = TRUE;
  lmn_env.end_dump               = FALSE;
  lmn_env.benchmark              = FALSE;
  lmn_env.property_dump          = FALSE;
  lmn_env.enable_compress_mem    = TRUE;
  lmn_env.z_compress             = FALSE;
  lmn_env.enable_parallel        = FALSE;
  lmn_env.core_num               = 1;
  lmn_env.optimize_lock          = FALSE;
  lmn_env.optimize_hash          = TRUE;
  lmn_env.optimize_loadbalancing = TRUE;

  /* only jni-interactive mode */
  lmn_env.interactive            = FALSE;
  lmn_env.normal_remain          = FALSE;
  lmn_env.normal_remaining       = FALSE;
  lmn_env.normal_cleaning        = FALSE;
  lmn_env.nd_remain              = FALSE;
  lmn_env.nd_remaining           = FALSE;
  lmn_env.nd_cleaning            = FALSE;

  lmn_env.enable_owcty           = FALSE;
  lmn_env.enable_map             = FALSE;
  lmn_env.enable_bledge          = FALSE;
  lmn_env.bfs_layer_sync         = FALSE;

  lmn_env.enable_map_heuristic   = TRUE;

#ifdef PROFILE
  lmn_env.optimize_hash_old      = FALSE;
  lmn_env.prof_no_memeq          = FALSE;
#endif

#ifdef DEBUG
  lmn_env.debug_memenc           = FALSE;
  lmn_env.debug_id               = FALSE;
  lmn_env.debug_delta            = FALSE;
  lmn_env.debug_hash             = FALSE;
  lmn_env.debug_isomor           = FALSE;
  lmn_env.debug_mc               = FALSE;
#endif


  /* for MC */
  mc_data.error_exist            = FALSE;
  mc_data.mc_exit                = FALSE;
  mc_data.is_format_states       = FALSE;
  mc_data.do_search              = FALSE;
  mc_data.do_exhaustive          = FALSE;
  mc_data.do_parallel            = FALSE;
  mc_data.has_property           = FALSE;
  mc_data.property_automata      = NULL;
  mc_data.propsyms               = NULL;
  mc_data.invalid_seeds          = NULL;
  mc_data.cycles                 = NULL;

  por_data.Stack_POR             = NULL;
  por_data.States_POR            = NULL;
  por_data.ample_candidate       = NULL;
  por_data.next_strans_id        = 0;
  por_data.strans_independency   = NULL;
  por_data.succ_strans           = NULL;
}

void init_default_system_ruleset();
void init_rules();
void destroy_rules();
void sym_tbl_destroy();
void sym_tbl_init();

/* 処理系内部の初期化処理 */
static void init_internal(void)
{
  lmn_profiler_init();
  sym_tbl_init();
  lmn_functor_tbl_init();
  init_rules();

  if(!lmn_env.translate){
    if (lmn_env.hyperlink) hyperlink_init();//seiji
    init_so_handles();
    init_default_system_ruleset();
    if (lmn_env.enable_por) init_por_vars();
    mpool_init();
    task_init();
    mem_isom_init();
/*    ext_init(); */
    sp_atom_init();
    ccallback_init();
    init_builtin_extensions();

    dumper_init();
    string_init();
    port_init();
  }
}

static void finalize(void)
{
  if(!lmn_env.translate){
    if (lmn_env.hyperlink) hyperlink_destroy();//seiji
    port_finalize();
    string_finalize();
    dumper_finalize();

    if (lmn_env.enable_por) free_por_vars();
    task_finalize();
    mem_isom_finalize();
/*    ext_finalize(); */
    ccallback_finalize();
    sp_atom_finalize();
    free_atom_memory_pools();
    finalize_so_handles();
  }

  lmn_profiler_finalize();
  destroy_rules();
  lmn_functor_tbl_destroy();
  sym_tbl_destroy();
}

int main(int argc, char *argv[])
{
  int optid;
  int i;

  init_env();
  optid = parse_options(argc, argv);
  init_internal();

  if (optid >= argc) {
    /** no input file */
    if (lmn_env.interactive) {
      run_jni_interactive(); /* interactive execution */
    } else {
      fprintf(stderr, "no input file\n");
    }
  } else {
    Vector *start_rulesets = vec_make(2);

    /** load input files */
    for(i = optid; i < argc; i++){
      FILE *in;
      LmnRuleSet t;
      char *f = argv[i];

      if (!strcmp("-", f)) { /* 標準入力からの読込み */
        in = stdin;
        t = load(stdin);
        vec_push(start_rulesets, (vec_data_t)t);
      } else {
        t = load_file(f);
        if (t) vec_push(start_rulesets, (vec_data_t)t);
      }
    }

    if(vec_num(start_rulesets) == 0){
      /** detected invalid file */
      fprintf(stderr, "bad input file.\n");
    }
    else {
      if (lmn_env.translate) {
        /** lmntalコードからCへの変換実行の場合 */
        /* TODO: 複数ファイル入力への対応 */
        if (!strcmp("-", argv[optid])) { /* argv[optid] is first input file name */
          translate(NULL);
        } else {
          translate(argv[optid]);
        }
      } else {
        /** Start Execution */
        if (lmn_env.profile_level >= 1) profile_start_slim();

        /** load directories(system & load path) */
        load_il_files(SLIM_LIB_DIR);
        for (i = lmn_env.load_path_num - 1; i >= 0; i--) {
          load_il_files(lmn_env.load_path[i]);
        }

        if (!lmn_env.nd) {
          /* execution as Runtime */
          lmn_run(start_rulesets);
        } else {
          /* execution as ModelChecker */
          Automata automata = NULL;
          PVector prop_defs = NULL;
          int ret = 1;

          if ((lmn_env.automata_file || lmn_env.ltl_exp)
              && lmn_env.propositional_symbol) {
            /* load property automata, definition of atomic propositional symbol */
            ret = mc_load_property(&automata, &prop_defs);
            if (!ret) {
              mc_data.has_property      = TRUE;
              mc_data.property_automata = automata;
              mc_data.propsyms          = prop_defs;
              if (lmn_env.prop_scc_driven) {
                automata_analysis(mc_data.property_automata);
              }
            } else {
              mc_explain_error(ret);
              exit(1);
            }
          }

          run_mc(start_rulesets);

          if (!ret) {
            automata_free(automata);
            propsyms_free(prop_defs);
          }
        }

        /** Finish Execution */
        if (lmn_env.profile_level >= 1) {
          profile_finish_slim();
          dump_profile_data(stderr);
        }
      }
      vec_free(start_rulesets);
    }
  }

  finalize();
  return 0;
}
