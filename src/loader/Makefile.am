noinst_LIBRARIES = liblmn_loader.a

AM_LFLAGS=-P $(patsubst %_lexer.l,%,$<)  \
           --header-file=$(patsubst %.l,%.h,$<) -o $(patsubst %.l,%.c,$<)
AM_YFLAGS=-d -p $(patsubst %_parser.y,%,$<) -o $(patsubst %.y,%.c,$<)

PARSERS = il_lexer il_parser
PARSER_HEADER = $(patsubst %, %.h, $(PARSERS))
PARSER_SOURCE = $(patsubst %, %.c, $(PARSERS))

BUILT_SOURCES = $(PARSER_SOURCE)
CLEANFILES = $(PARSER_SOURCE) $(PARSER_HEADER)

liblmn_loader_a_CFLAGS =                               \
        -I../                                            \
        -I../utility                                     \
        -I../../third_party/zdelta-2.1                   \
        $(CFLAGS)

liblmn_loader_a_SOURCES = \
        load.c load.h \
        il_parser.y il_lexer.l \
        syntax.c syntax.h

EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)

.l.c:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) $<
.y.c:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<
