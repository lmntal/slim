noinst_LIBRARIES = liblmn_loader.a

AM_YFLAGS=-d -p $(patsubst %_parser.ypp,%,$<) -o $(patsubst %.ypp,%.cpp,$<)

PARSERS = il_lexer il_parser
PARSER_HEADER = il_parser.hpp
PARSER_SOURCE = il_parser.cpp il_lexer.cpp

BUILT_SOURCES = $(PARSER_SOURCE) translate_generated.c interpret_generated.c
CLEANFILES = $(PARSER_SOURCE) $(PARSER_HEADER) translate_generated.c interpret_generated.c

liblmn_loader_a_CFLAGS =                               \
        -I../                                            \
        -I../../third_party/zdelta-2.1                   \
        $(CFLAGS)

liblmn_loader_a_SOURCES = \
        loader.h \
        load.cpp load.h \
        il_parser.ypp il_lexer.cpp \
        syntax.cpp syntax.h \
        translate.cpp translate.h \
        translate_generated.c translate_generator.rb \
        translate_generator.in \
        so.h \
        interpret_generated.c

EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)

translate_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@

interpret_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@ -i

il_lexer.cpp: il_lexer.cpp.re
	re2c -o il_lexer.cpp il_lexer.cpp.re

.ypp.cpp:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<
