noinst_LIBRARIES = liblmn_verifier.a


AM_YFLAGS=-d -p $(patsubst %_parser.ypp,%,$<) -o $(patsubst %.ypp,%.cpp,$<)

PARSERS = nc_parser nc_lexer propsym_parser propsym_lexer
PARSER_HEADER = nc_parser.hpp propsym_parser.hpp
PARSER_SOURCE = nc_parser.cpp nc_lexer.cpp propsym_parser.cpp propsym_lexer.cpp

BUILT_SOURCES = $(PARSER_SOURCE)
CLEANFILES = $(PARSER_SOURCE) $(PARSER_HEADER)

liblmn_verifier_a_DEPENDENCIES =                         \
        ../../third_party/zdelta-2.1/libzd_in_slim.a
liblmn_verifier_a_CFLAGS =                               \
        -I../                                            \
        -I../../third_party/zdelta-2.1                   \
        $(CFLAGS)
        
liblmn_verifier_a_SOURCES =                              \
	automata.cpp               automata.h                    \
	binstr_compress.cpp        binstr_compress.h             \
	delta_membrane.cpp         delta_membrane.h              \
	dpor.cpp                 dpor.h                        \
	dpor_naive.cpp             dpor_naive.h                  \
	ltl2ba_adapter.cpp         ltl2ba_adapter.h              \
	mc.cpp                     mc.h                          \
	mc_explorer.cpp            mc_explorer.h                 \
	mc_generator.cpp           mc_generator.h                \
	mc_visualizer.cpp          mc_visualizer.h               \
	mc_worker.cpp              mc_worker.h                   \
	mem_encode.cpp             mem_encode.h                  \
	mhash.cpp                  mhash.h                       \
	nc_parser.ypp	             nc_lexer.cpp                    \
	propositional_symbol.cpp   propositional_symbol.h        \
	propsym_parser.ypp         propsym_lexer.cpp               \
	runtime_status.cpp         runtime_status.h              \
	state.cpp                  state.h          state.hpp             \
	statespace.cpp             statespace.h                  \
	tree_compress.cpp          tree_compress.h               \
	visitlog.cpp               visitlog.h                    \
	stack_macro.h                                            \
	convertedgraph.hpp	   graphinfo.hpp		\
	json.hpp		   json.cpp			\
	graphinfo.cpp		   collection.hpp		\
	util.hpp		   hash.hpp			\
	collection.cpp		   trie.hpp			\
	convertedgraph.cpp


EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)

nc_lexer.cpp: nc_lexer.cpp.re
	$(RE2C) -o nc_lexer.cpp nc_lexer.cpp.re

propsym_lexer.cpp: propsym_lexer.cpp.re
	$(RE2C) -c -o propsym_lexer.cpp propsym_lexer.cpp.re


.ypp.cpp:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<
