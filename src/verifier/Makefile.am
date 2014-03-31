noinst_LIBRARIES = liblmn_verifier.a

AM_LFLAGS=-P $(patsubst %_lexer.l,%,$<)  \
           --header-file=$(patsubst %.l,%.h,$<) -o $(patsubst %.l,%.c,$<)
AM_YFLAGS=-d -p $(patsubst %_parser.y,%,$<) -o $(patsubst %.y,%.c,$<)

PARSERS = nc_parser nc_lexer propsym_parser propsym_lexer
PARSER_HEADER = $(patsubst %, %.h, $(PARSERS))
PARSER_SOURCE = $(patsubst %, %.c, $(PARSERS))

BUILT_SOURCES = $(PARSER_SOURCE)
CLEANFILES = $(PARSER_SOURCE) $(PARSER_HEADER)

liblmn_verifier_a_DEPENDENCIES =                         \
        ../../third_party/zdelta-2.1/libzd_in_slim.a
liblmn_verifier_a_CFLAGS =                               \
        -I../                                            \
        -I../utility                                     \
        -I../../third_party/zdelta-2.1                   \
        $(CFLAGS)

liblmn_verifier_a_SOURCES =                              \
	nc_parser.y	             nc_lexer.l                    \
	propsym_parser.y         propsym_lexer.l               \
	propositional_symbol.c   propositional_symbol.h        \
	automata.c               automata.h                    \
	ltl2ba_adapter.c         ltl2ba_adapter.h              \
	mc.c                     mc.h                          \
	mc_worker.c              mc_worker.h                   \
	mc_generator.c           mc_generator.h                \
	mc_explorer.c            mc_explorer.h                 \
	mhash.c                  mhash.h                       \
	dpor.c                   dpor.h                        \
	dpor_naive.c             dpor_naive.h                  \
	state.c                  state.h                       \
	statespace.c             statespace.h                  \
	mem_encode.c             mem_encode.h                  \
	delta_membrane.c         delta_membrane.h              \
	binstr_compress.c        binstr_compress.h             \
        mc_visualizer.c          mc_visualizer.h



EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)

.l.c:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) $<
.y.c:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<
