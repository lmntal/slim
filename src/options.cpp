#include "options.h"

#include <iostream>
#include <string>
#include <string_view>

#include "cxxopts/cxxopts.hpp"

#include "arch.h"
#include "config.h"
#include "lmntal.h"

void init_options() {
  // The trailing comment is for formatting purpose.
  slim_options.add_options()                     //
      ("h,help", "Prints this help message.")    //
      ("v,version", "Prints version and exits.") //
      ("d,dump", "Enable dump")                  //
      ("t,trace", "(RT) Show execution path\n"
                  "(MC) Show state space") //
      ("p,profiler-level", "Profiler level.",
       cxxopts::value<int>()->default_value("1")) //
      ("I,include", "Adds <path> to the head of the load path list.",
       cxxopts::value<std::string>()) //
      ("O,optimize-level", "Optimization level of the intermediate instruction.",
       cxxopts::value<int>()->default_value("3"))   //
      ("show-proxy", "Show proxy atoms.")           //
      ("hide-ruleset", "Hide ruleset from result.") //
      ("show-chr",
       "Show applied history in uniq rulesets (constraint handling rules).") //
      ("show-transition",
       "(MC) Show transition information in state transition graph.") //
      ("show-ends", "(MC) Show all of terminated states.")            //
      ("show-hl", "(RT) Show all hyperlinks details.")                //
      ("use-builtin-rule",
       "Load the rules builtin this application for arithmetic, nlmem, etc.") //
      ("dump-dot",
       "(RT) Print format: DOT language (LMNtal hierarchical graph),\n"
       "(MC) Print format: DOT language (State Transition graph).") //
      ("dump-fsm-lmn", "")                                          //
      ("dump-lavit",
       "(MC) Print format: LaViT - LMNtal IDE (State Transition Graph).") //
      ("dump-inc",
       "(MC) State Generation and Output of states at the same time.") //
      ("dump-lmn", "")                                                 //
      ("dump-json", "Print format: JSON")                              //
      ("dump-fsm-lmn-detail", "")                                      //
      ("dump-fsm-hl", "")                                              //
      ("show-laststep-only", "")                                       //
#ifdef HAVE_JNI_H
      ("interactive", "") //
#endif
      ("translate",
       "Change the execution mode to Output translated C from LMNtal.")    //
      ("hl", "(RT) Allow using hyperlink system.")                         //
      ("ltl-all", "(MC) Generate full state space and exhaustive search.") //
      ("ltl", "(MC) Do LTL model checking (need --psym, --nc).")           //
      ("nd",
       "Change the execution mode from RunTime(RT) to ModelChecker(MC).") //
      ("opt-min", "")                                                     //
      ("opt-max", "")                                                     //
      ("nc", "(MC) Input <file> as a property automata (LTL2BA format).",
       cxxopts::value<std::string>()) //
      ("psym", "(MC) Input <file> as propositional symbol definitions.",
       cxxopts::value<std::string>()) //
      ("ltl-f", "(MC) Input <ltl> formula directly. (need LTL2BA env).",
       cxxopts::value<std::string>()) //
      ("pscc-driven",
       "(MC) Use SCC analysis of property automata (LTL model checking).") //
      ("por-old", "")                                                      //
      ("por", "")                                                          //
      ("bfs", "(MC) Use BFS strategy.")                                    //
      ("limited-step", "(MC) Run only first <N> steps (BFS).",
       cxxopts::value<int>())                                    //
      ("search-ends", "")                                        //
      ("mem-enc", "(MC) Use canonical membrane representation.") //
      ("disable-compress", "")                                   //
      ("delta-mem", "(MC) Use delta membrane generator")         //
#ifdef HAVE_LIBZ
      ("z-compress", "") //
#endif
      ("d-compress", "")                                                   //
      ("r-compress", "")                                                   //
      ("use-owcty", "(MC) Use OWCTY algorithm  (LTL model checking).")     //
      ("use-map", "(MC) Use MAP algorithm    (LTL model checking).")       //
      ("use-bledge", "(MC) Use BLEDGE algorithm (LTL model checking).")    //
      ("bfs-lsync", "(MC) Use Layer Synchronized BFS strategy")            //
      ("use-mapndfs", "(MC) Use Map+NDFS algorithm (LTL model checking).") //
#ifndef MINIMAL_STATE
      ("use-mcndfs",
       "(MC) Use Multicore NDFS algorithm (LTL model checking).") //
#endif
      ("disable-map-h", "(MC) No use MAP heuristics(LTL model checking)");
#ifdef ENABLE_PARALLEL
  slim_options.add_options("parallel")("use-Ncore", "(MC) Use <N>threads", cxxopts::value<int>()) //
      ("cutoff-depth", "")                                                                        //
      ("independent", "")                                                                         //
      ("disable-loadbalancer", "")                                                                //
      ("opt-lock", "");
#endif

  slim_options.add_options()   //
      ("disable-opt-hash", "") //
      ("opt-hash-old", "")     //
      ("no-dump", "")          //
      ("benchmark-dump", "")   //
      ("property-dump", "");

#ifdef DEBUG
  slim_options.add_options("debug") //
      ("debug-id", "")              //
      ("debug-delta", "")           //
      ("debug-hash", "")            //
      ("debug-isomor", "")          //
      ("debug-mc", "")              //
      ("debug-por", "")             //
      ("show-rgraph", "")           //
      ("debug-tr-dep", "");         //
#endif

#ifdef PROFILE
  slim_options.add_options("profile")("prof-nomemeq", "");
#endif

  slim_options.add_options()                                  //
      ("visualize", "(MC) Output information for visualize.") //
      ("hash-compaction", "(MC) Use Hash Compaction.")        //
      ("hash-depth", "(MC) Set <N> Depth of Hash Function.",
       cxxopts::value<int>()->default_value("1")) //
      ("tree-compress", "(MC) Use Tree Compression with 2^N table size default(N=20).",
       cxxopts::value<int>()->default_value("20")) //
      ("run-test", "Run CUnit.")                   //
      ("history-management", "Optimize backtracking of findatom function by "
                             "using atoms for history management.") //
      ("shuffle-rule", "(RT) Apply rules randomly.")                //
      ("shuffle-atom", "(RT) Choose atoms to be applied randomly.") //
      ("shuffle",
       "(RT) Apply both shuffle-rule option and shuffle-atom options.") //
      ("interactive-debug", "")                                         //
      ("filenames", "The input file(s)", cxxopts::value<std::vector<std::string>>());

  slim_options.parse_positional("filenames");
}

auto check_parallel_options(cxxopts::ParseResult const &result) {
  auto check_have = [&](std::string_view name) { return result.count(name.data()) > 0 ? TRUE : FALSE; };

  if (result.count("use-Ncore") > 0) {
    auto ncore              = result["use-Ncore"].as<int>();
    ncore                   = std::max(ncore, 1);
    lmn_env.core_num        = ncore;
    lmn_env.enable_parallel = TRUE;
    env_set_threads_num(ncore);
  }

  if (result.count("cutoff-depth") > 0) {
    auto depth           = result["cutoff-depth"].as<int>();
    depth                = std::max(depth, 1);
    lmn_env.cutoff_depth = depth;
  }

  lmn_env.findatom_parallel_inde = check_have("independent");
  lmn_env.optimize_loadbalancing = check_have("disable-loadbalancer") == TRUE ? FALSE : TRUE;
  lmn_env.optimize_lock          = check_have("opt-lock");
}

auto check_debug_options(cxxopts::ParseResult const &result) {
#ifdef DEBUG
  auto check_have      = [&](std::string_view name) { return result.count(name.data()) > 0 ? TRUE : FALSE; };
  lmn_env.debug_id     = check_have("debug-id");
  lmn_env.debug_delta  = check_have("debug-delta");
  lmn_env.debug_hash   = check_have("debug-hash");
  lmn_env.debug_isomor = check_have("debug-isomor");
  lmn_env.debug_mc     = check_have("debug-mc");
  lmn_env.debug_por    = check_have("debug-por");

  lmn_env.show_reduced_graph = check_have("show-rgraph");
  lmn_env.show_transition    = check_have("show-rgraph");

  lmn_env.debug_por_dep = check_have("debug-tr-dep");
  lmn_env.enable_por    = check_have("enable-por");
#endif
}

// NOLINTNEXTLINE
auto parse_command_line(cxxopts::ParseResult const &result) -> CommandLineParseResult {
  auto check_have = [&](std::string_view name) { return result.count(name.data()) > 0 ? TRUE : FALSE; };

  if (result.count("help") > 0) {
    std::cout << slim_options.help() << std::endl;
    return CommandLineParseResult::EXIT;
  }

  if (result.count("version") > 0) {
    slim_version(stdout);
    return CommandLineParseResult::EXIT;
  }

  if (result.count("optimize-level") > 0) {
    auto opt                   = result["optimize-level"].as<int>();
    opt                        = std::clamp(opt, 0, 3);
    lmn_env.optimization_level = opt;
  }

  if (result.count("include") > 0) {
    auto include                               = result["include"].as<std::string>();
    lmn_env.load_path[lmn_env.load_path_num++] = strdup(include.c_str());
  }

  if (result.count("dump") > 0) {
    lmn_env.output_format = DEV;
  }

  lmn_env.trace = check_have("trace");

  if (result.count("profiler-level") > 0) {
    auto level            = result["profiler-level"].as<int>();
    level                 = std::clamp(level, 0, 3);
    lmn_env.profile_level = level;

#ifndef PROFILE
    if (lmn_env.profile_level > 2) {
      std::cerr << "please configure with --enable-profile" << std::endl;
      return CommandLineParseResult::ERROR;
    }
#endif
  }

  lmn_env.show_proxy         = check_have("show-proxy");
  lmn_env.show_ruleset       = check_have("show-ruleset") == TRUE ? FALSE : TRUE;
  lmn_env.show_chr           = check_have("show-chr");
  lmn_env.show_transition    = check_have("show-transition");
  lmn_env.end_dump           = check_have("show-ends");
  lmn_env.show_reduced_graph = check_have("show-reduced-graph");
  lmn_env.show_hyperlink     = check_have("show-hl");

  if (result.count("use-builtin-rule") > 0) {
    lmn_env.load_path[lmn_env.load_path_num++] = SLIM_LIB_DIR;
  }

  if (result.count("dump-dot") > 0) {
    lmn_env.output_format  = DOT;
    lmn_env.mc_dump_format = Dir_DOT;
  }

  if (result.count("dump-fsm-lmn") > 0) {
    lmn_env.mc_dump_format = LMN_FSM_GRAPH_MEM_NODE;
  }

  if (result.count("dump-lavit") > 0) {
    lmn_env.mc_dump_format = LaViT;
  }

  if (result.count("dump-inc") > 0) {
    lmn_env.sp_dump_format = INCREMENTAL;
  }

  if (result.count("dump-lmn") > 0) {
    lmn_env.sp_dump_format = LMN_SYNTAX;
  }

  if (result.count("dump-json") > 0) {
    lmn_env.output_format = JSON;
  }

  if (result.count("dump-fsm-lmn-detail") > 0) {
    lmn_env.mc_dump_format = LMN_FSM_GRAPH;
  }

  if (result.count("dump-fsm-hl") > 0) {
    lmn_env.mc_dump_format = LMN_FSM_GRAPH_HL_NODE;
  }

  lmn_env.trace              |= check_have("show-laststep-only");
  lmn_env.show_laststep_only = check_have("show-laststep-only");

  lmn_env.interactive = check_have("interactive");

  lmn_env.translate = check_have("translate");
  lmn_env.hyperlink = check_have("hl");

  lmn_env.ltl_all = check_have("ltl-all");
  lmn_env.ltl     = check_have("ltl-all") | check_have("ltl");
  lmn_env.nd      = check_have("ltl-all") | check_have("ltl") | check_have("nd");

  lmn_env.nd              |= check_have("opt-min") | check_have("opt-max");
  lmn_env.show_transition |= check_have("opt-min") | check_have("opt-max");
  if (result.count("opt-min") > 0) {
    lmn_env.opt_mode = OPT_MINIMIZE;
  } else if (result.count("opt-max") > 0) {
    lmn_env.opt_mode = OPT_MAXIMIZE;
  }

  if (result.count("nc") > 0) {
    auto file             = result["nc"].as<std::string>();
    lmn_env.automata_file = strdup(file.c_str());
  }

  if (result.count("psym") > 0) {
    auto file                    = result["psym"].as<std::string>();
    lmn_env.propositional_symbol = strdup(file.c_str());
  }

  if (result.count("ltl-f") > 0) {
    auto file       = result["ltl-f"].as<std::string>();
    lmn_env.ltl_exp = strdup(file.c_str());
  }

  lmn_env.prop_scc_driven = check_have("pscc-driven");

  lmn_env.enable_por_old = check_have("por-old");
  lmn_env.delta_mem      = check_have("por");
  lmn_env.enable_por     = check_have("por-old") | check_have("por");

  lmn_env.bfs = check_have("bfs");

  if (result.count("limited-step") > 0) {
    auto step              = result["limited-step"].as<int>();
    lmn_env.bfs_layer_sync = TRUE;
    lmn_env.depth_limits   = step;
  }

  lmn_env.nd_search_end = check_have("search-ends");

  lmn_env.mem_enc             = check_have("mem-enc");
  lmn_env.enable_compress_mem = check_have("disable-compress") == TRUE ? FALSE : TRUE;

  lmn_env.delta_mem |= check_have("delta-mem");

  lmn_env.z_compress = check_have("z-compress");
  lmn_env.d_compress = check_have("d-compress");
  lmn_env.r_compress = check_have("r-compress");

  lmn_env.enable_parallel |=
      check_have("use-owcty") | check_have("use-map") | check_have("use-bledge") | check_have("use-mapndfs");
  lmn_env.enable_owcty = check_have("use-owcty");

  lmn_env.enable_map           = check_have("use-map");
  lmn_env.enable_map_heuristic = check_have("use-map") == TRUE ? FALSE : TRUE;

  lmn_env.enable_bledge  = check_have("use-bledge");
  lmn_env.bfs            |= check_have("use-bledge") | check_have("bfs-lsync");
  lmn_env.bfs_layer_sync |= check_have("use-bledge") | check_have("bfs-lsync");

  lmn_env.enable_mapndfs = check_have("use-mapndfs");

#ifndef MINIMAL_STATE
  lmn_env.enable_parallel |= check_have("use-mcndfs");
  lmn_env.enable_mcndfs   = check_have("use-mcndfs");
#endif

  lmn_env.enable_map_heuristic |= check_have("disable-map-h") == TRUE ? FALSE : TRUE;

  check_parallel_options(result);

  lmn_env.optimize_hash = check_have("disable-opt-hash") == TRUE ? FALSE : TRUE;

  if (result.count("opt-hash-old") > 0) {
    lmn_env.optimize_hash     = FALSE;
    lmn_env.optimize_hash_old = TRUE;
  }

  lmn_env.dump = (check_have("no-dump") | check_have("benchmark-dump")) == TRUE ? FALSE : TRUE;

  lmn_env.benchmark = check_have("benchmark-dump");
  lmn_env.end_dump  |= check_have("benchmark-dump") == TRUE ? FALSE : TRUE;

  lmn_env.property_dump = check_have("property-dump");

  check_debug_options(result);

  lmn_env.enable_visualize = check_have("visualize");
  lmn_env.hash_compaction  = check_have("hash-compaction");
  lmn_env.hash_depth       = check_have("hash-depth") == TRUE ? std::max(1, result["hash-depth"].as<int>()) : FALSE;

  if (result.count("tree-compress") > 0) {
    lmn_env.hash_compaction = FALSE;
    lmn_env.tree_compress   = TRUE;

    int size = result["tree-compress"].as<int>();
    size     = std::max(15, size);

    lmn_env.tree_compress_table_size = size;
  }

  lmn_env.run_test           = check_have("run-test");
  lmn_env.history_management = check_have("history-management");

  lmn_env.shuffle_rule = check_have("shuffle-rule") | check_have("shuffle");
  lmn_env.shuffle_atom = check_have("shuffle-atom") | check_have("shuffle");

  lmn_env.interactive_debug = check_have("interactive-debug");

  return CommandLineParseResult::OK;
}