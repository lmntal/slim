SUBDIRS = utility verifier

bin_PROGRAMS = slim

slim_LDADD =   -ldl                                    \
               -llmn_verifier                          \
               -llmn_utility                           \
               -lzd_in_slim
slim_LDFLAGS = -L./verifier                            \
               -L./utility                             \
               -L../third_party/zdelta-2.1
slim_CFLAGS =  -I./verifier                            \
               -I./utility
slim_DEPENDENCIES = ./verifier/liblmn_verifier.a       \
                    ./utility/liblmn_utility.a

if ENABLE_JNI
slim_LDADD += -ljvm
slim_LDFLAGS += -L../
slim_DEPENDENCIES += ../libjvm.a
endif

if ENABLE_TCMALLOC
slim_LDADD += -ltcmalloc_minimal_in_slim
slim_LDFLAGS += -L../third_party/google-perftools-1.8.3/.libs
slim_DEPENDENCIES += ../third_party/google-perftools-1.8.3/.libs/libtcmalloc_minimal_in_slim.a
endif

if ENABLE_CUNIT
slim_CFLAGS += -I$(CUNIT_HOME)/include
slim_LDFLAGS += -L$(CUNIT_HOME)/lib -lcunit
SUBDIRS += test
slim_LDADD += -lunit_test
slim_LDFLAGS += -L./test
slim_CFLAGS += -I./test
slim_DEPENDENCIES += ./test/libunit_test.a
endif

EXTRA_DIST = $(PARSER_SOURCE) $(PARSER_HEADER)
DISTCLEANFILES = arch.h ../lib/config.lmn


AM_LFLAGS=-P $(patsubst %_lexer.l,%,$<)  \
           --header-file=$(patsubst %.l,%.h,$<) -o $(patsubst %.l,%.c,$<)
AM_YFLAGS=-d -p $(patsubst %_parser.y,%,$<) -o $(patsubst %.y,%.c,$<)

PARSERS = il_parser il_lexer
PARSER_HEADER = $(patsubst %, %.h, $(PARSERS))
PARSER_SOURCE = $(patsubst %, %.c, $(PARSERS))

# 事前に必要なファイル
BUILT_SOURCES = arch.h                       \
                $(PARSER_SOURCE)             \
                translate_generated.c        \
                interpret_generated.c

CLEANFILES = arch.h $(PARSER_SOURCE) $(PARSER_HEADER) translate_generated.c interpret_generated.c

# .yファイルは .lより先に書く
# この一覧は、ファイルを手動で列挙するにはどうしたらいい？
slim_SOURCES =                                                     \
	main.c                          lmntal.h                         \
	alloc.c                         atom.h                           \
	membrane.c                      membrane.h                       \
	rule.c                          rule.h                           \
	error.c                         error.h                          \
	normal_thread.c			normal_thread.h			 \
	task.c                          task.h                           \
	memory_pool.c                   memory_pool.h                    \
	dumper.c                        dumper.h                         \
	system_ruleset.c                system_ruleset.h                 \
	instruction.c                   instruction.h                    \
	symbol.c                        symbol.h                         \
	functor.c                       functor.h                        \
	syntax.c                        syntax.h                         \
	react_context.c                 react_context.h                  \
	il_parser.y                     il_lexer.l                       \
	load.c                          load.h                           \
	env.c                           arch.h                           \
	runtime_status.c                runtime_status.h                 \
	lmntal_system_adapter.c         lmntal_system_adapter.h          \
	special_atom.c                  special_atom.h                   \
	memstack.c                      slim_header/memstack.h           \
	string.c                        slim_header/string.h             \
	port.c                          slim_header/port.h               \
	jni_lmntal.c                    jni_lmntal.h                     \
	ccallback.c                     ccallback.h                      \
	hyperlink.c                     hyperlink.h                      \
	translate.c                     translate.h                      \
	translate_generated.c           translate_generator.rb           \
	translate_generator.in          so.h                             \
	interpret_generated.c                                            \
	\
	lmntal_ext.h                                                     \
	ext/init_exts.c                                                  \
	ext/integer.c                                                    \
	ext/float.c                                                      \
	ext/atomic.c                                                     \
	ext/nlmem.c                                                      \
	ext/io.c                                                         \
	ext/array.c			ext/array.h                      \
	ext/initial_ruleset.c                                            \
	ext/nd_conf.c							 \
	ext/time.c
#	ext.h ext.c

slim_OBJS =

# Automakeは同じディレクトリにある複数のパーサーをコンパイルするため
# にylwrap を使おうとするが、必要ないので、ルールを上書きする
.l.c:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) $<
.y.c:
	$(YACC) $(YFLAGS) $(AM_YFLAGS) $<

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig

translate_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@

interpret_generated.c: translate_generator.in translate_generator.rb
	ruby --encoding=utf-8 ./translate_generator.rb <$< >$@ -i
