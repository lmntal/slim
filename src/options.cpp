#include "options.h"
#include "cxxopts/cxxopts.hpp"

#include <string>

cxxopts::Options slim_options{"slim", "An implementation of the SLIM language"};

void init_options() {                            // The trailing comment is for formatting purpose.
  slim_options.add_options()                     //
      ("v,version", "Prints version and exits.") //
      ("d,develop", "Enable develop mode")       //
      ("t,trace", "(RT) Show execution path\n"
                  "(MC) Show state space")                                                           //
      ("p,profiler-level", "Profiler level.", cxxopts::value<int>())                                 //
      ("I,include", "Adds <path> to the head of the load path list.", cxxopts::value<std::string>()) //
      ("O,optimize-level", "Optimization level of the intermediate instruction.",
       cxxopts::value<int>()->default_value("3"))                                                 //
      ("show-proxy", "Show proxy atoms.")                                                         //
      ("hide-ruleset", "Hide ruleset from result.")                                               //
      ("show-chr", "Show applied history in uniq rulesets (constraint handling rules).")          //
      ("show-transition", "(MC) Show transition information in state transition graph.")          //
      ("show-ends", "(MC) Show all of terminated states.")                                        //
      ("show-hl", "(RT) Show all hyperlinks details.")                                            //
      ("use-builtin-rule", "Load the rules builtin this application for arithmetic, nlmem, etc.") //
      ("dump-dot", "(RT) Print format: DOT language (LMNtal hierarchical graph),\n"
                   "(MC) Print format: DOT language (State Transition graph).")                          //
      ("dump-fsm-lmn", "")                                                                               //
      ("dump-lavit", "(MC) Print format: LaViT - LMNtal IDE (State Transition Graph).")                  //
      ("dump-inc", "(MC) State Generation and Output of states at the same time.")                       //
      ("dump-lmn", "")                                                                                   //
      ("dump-json", "Print format: JSON")                                                                //
      ("dump-fsm-lmn-detail", "")                                                                        //
      ("dump-fsm-hl", "")                                                                                //
      ("show-laststep-only", "")                                                                         //
      ("interactive", "")                                                                                //
      ("translate", "Change the execution mode to Output translated C from LMNtal.")                     //
      ("hl", "(RT) Allow using hyperlink system.")                                                       //
      ("ltl-all", "(MC) Generate full state space and exhaustive search.")                               //
      ("ltl", "(MC) Do LTL model checking (need --psym, --nc).")                                         //
      ("nd", "Change the execution mode from RunTime(RT) to ModelChecker(MC).")                          //
      ("opt-min", "")                                                                                    //
      ("opt-max", "")                                                                                    //
      ("nc", "(MC) Input <file> as a property automata (LTL2BA format).", cxxopts::value<std::string>()) //
      ("psym", "(MC) Input <file> as propositional symbol definitions.", cxxopts::value<std::string>())  //
      ("ltl-f", "(MC) Input <ltl> formula directly. (need LTL2BA env).", cxxopts::value<std::string>())  //
      ("pscc-driven", "(MC) Use SCC analysis of property automata (LTL model checking).")                //
      ("por-old", "")                                                                                    //
      ("por", "")                                                                                        //
      ("bfs", "(MC) Use BFS strategy.")                                                                  //
      ("limited-step", "(MC) Run only first <N> steps (BFS).", cxxopts::value<int>())                    //
      ("search-ends", "")                                                                                //
      ("mem-enc", "(MC) Use canonical membrane representation.")                                         //
      ("disable-compress", "")                                                                           //
      ("delta-mem", "(MC) Use delta membrane generator")                                                 //
      ("z-compress", "")                                                                                 //
      ("d-compress", "")                                                                                 //
      ("r-compress", "")                                                                                 //
      ("use-owcty", "(MC) Use OWCTY algorithm  (LTL model checking).")                                   //
      ("use-map", "(MC) Use MAP algorithm    (LTL model checking).")                                     //
      ("use-bledge", "(MC) Use BLEDGE algorithm (LTL model checking).")                                  //
      ("bfs-lsync", "(MC) Use Layer Synchronized BFS strategy")                                          //
      ("use-mapndfs", "(MC) Use Map+NDFS algorithm (LTL model checking).")                               //
#ifndef MINIMAL_STATE
      ("use-mcndfs", "(MC) Use Multicore NDFS algorithm (LTL model checking).") //
#endif
      ("disable-map-h", "(MC) No use MAP heuristics(LTL model checking)")           //
      ("use-Ncore", "(MC) Use <N>threads", cxxopts::value<int>())                   //
      ("cutoff-depth", "")                                                          //
      ("independent", "")                                                           //
      ("disable-loadbalancer", "")                                                  //
      ("opt-lock", "")                                                              //
      ("disable-opt-hash", "")                                                      //
      ("opt-hash-old", "")                                                          //
      ("no-dump", "")                                                               //
      ("benchmark-dump", "")                                                        //
      ("property-dump", "")                                                         //
      ("debug-id", "")                                                              //
      ("debug-delta", "")                                                           //
      ("debug-hash", "")                                                            //
      ("debug-isomor", "")                                                          //
      ("debug-mc", "")                                                              //
      ("debug-por", "")                                                             //
      ("show-rgraph", "")                                                           //
      ("debug-tr-dep", "")                                                          //
      ("prof-nomemeq", "")                                                          //
      ("visualize", "(MC) Output information for visualize.")                       //
      ("hash-compaction", "(MC) Use Hash Compaction.")                              //
      ("hash-depth", "(MC) Set <N> Depth of Hash Function.", cxxopts::value<int>()) //
      ("tree-compress", "(MC) Use Tree Compression with 2^N table size default(N=20).",
       cxxopts::value<int>()->default_value("20"))                                                                //
      ("run-test", "Run CUnit.")                                                                                  //
      ("history-management", "Optimize backtracking of findatom function by using atoms for history management.") //
      ("shuffle-rule", "(RT) Apply rules randomly.")                                                              //
      ("shuffle-atom", "(RT) Choose atoms to be applied randomly.")                                               //
      ("shuffle", "(RT) Apply both shuffle-rule option and shuffle-atom options.")                                //
      ("interactive-debug", "");
}