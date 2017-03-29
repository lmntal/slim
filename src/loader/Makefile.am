noinst_LIBRARIES = liblmn_loader.a

AM_LFLAGS=-P $(patsubst %_lexer.l,%,$<)  \
           --header-file=$(patsubst %.l,%.h,$<) -o $(patsubst %.l,%.c,$<)
AM_YFLAGS=-d -p $(patsubst %_parser.y,%,$<) -o $(patsubst %.y,%.c,$<)

PARSERS = il_lexer il_parser
PARSER_HEADER = $(patsubst %, %.h, $(PARSERS))
PARSER_SOURCE = $(patsubst %, %.c, $(PARSERS))

BUILT_SOURCES = $(PARSER_SOURCE) translate_generated.c interpret_generated.c
CLEANFILES = $(PARSER_SOURCE) $(PARSER_HEADER) translate_generated.c interpret_generated.c

liblmn_loader_a_CFLAGS =                               \
        -I../                                            \
        -I../../third_party/zdelta-2.1                   \
        $(CFLAGS)

liblmn_loader_a_SOURCES = \
        loader.h \
        load.c load.h \
        il_parser.y il_lexer.l \
        syntax.c syntax.h \
        translate.c translate.h \
        translate_generated.c translate_generator.rb \
        translate_generator.in \
        so.h \
        interpret_generated.c

EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)

translate_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@

interpret_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@ -i

.l.c:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) $<
.y.c:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<
