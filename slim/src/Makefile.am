bin_PROGRAMS = slim

slim_LDADD = -ldl

AM_LFLAGS=-P $(patsubst %_lexer.l,%,$<)  \
           --header-file=$(patsubst %.l,%.h,$<) -o $(patsubst %.l,%.c,$<)
AM_YFLAGS=-d -p $(patsubst %_parser.y,%,$<) -o $(patsubst %.y,%.c,$<)

PARSERS := il_parser il_lexer nc_parser nc_lexer propsym_parser propsym_lexer

# 事前に必要なファイル
BUILT_SOURCES = arch.h $(patsubst %, %.c, $(PARSERS))
CLEANFILES = arch.h $(patsubst %, %.h, $(PARSERS)) $(patsubst %, %.c, $(PARSERS))

# .yファイルは .lより先に書く
# この一覧は、ファイルを手動で列挙するにはどうしたらいい？
slim_SOURCES = main.c membrane.c rule.c alloc.c error.h error.c task.c memory_pool.c internal_hash.c \
	vector.c dumper.c system_ruleset.c atom.c \
	atom.h dumper.h instruction.h internal_hash.h lmntal.h membrane.h \
	memory_pool.h rule.h system_ruleset.h task.h vector.h st.c symbol.h symbol.c functor.h functor.c \
	syntax.h syntax.c il_parser.y il_lexer.l instruction.c load.h load.c st.h arch.h \
    mc.h mc.c mhash.h mhash.c runtime_status.h runtime_status.c translate.c translate.h \
	nc_parser.y nc_lexer.l automata.h automata.c \
	lmntal_system_adapter.h lmntal_system_adapter.c \
	propsym_parser.y propsym_lexer.l \
	propositional_symbol.h propositional_symbol.c ltl2ba_adapter.h ltl2ba_adapter.c \
	util.h process_util.h process_util.c

DISTCLEANFILES := arch.h ../lib/config.lmn

# Automakeは同じディレクトリにある複数のパーサーをコンパイルするため
# にylwrap を使おうとするが、必要ないので、ルールを上書きする
.l.c:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) $<
.y.c:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig

translate_generator.exe: translate_generator.cpp
	$(CXX) -Wall $< -o $@

translate_generated.c: translate_generator.in translate_generator.exe
	./translate_generator.exe <$< >$@
