SUBDIRS = utility verifier vm loader element ffi

bin_PROGRAMS = slim

slim_LDADD =   -ldl                                    \
               -llmn_loader                            \
               -llmn_verifier                          \
               -llmn_utility                           \
               -lzd_in_slim                            \
               -llmn_ffi                               \
               -llmn_vm                                \
               -llmn_elm
slim_LDFLAGS = -L./verifier                            \
               -L./loader                              \
               -L./utility                             \
               -L./ffi                                 \
               -L./vm                                  \
               -L./element                             \
               -L../third_party/zdelta-2.1
slim_CFLAGS =  -I./utility                             \
               -I../third_party/zdelta-2.1             \
               -std=gnu99
slim_DEPENDENCIES = ./verifier/liblmn_verifier.a       \
                    ./utility/liblmn_utility.a         \
                    ./vm/liblmn_vm.a                   \
                    ./element/liblmn_elm.a             \
                    ./loader/liblmn_loader.a

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


# 事前に必要なファイル
BUILT_SOURCES = arch.h

CLEANFILES = arch.h $(PARSER_SOURCE) $(PARSER_HEADER)

# .yファイルは .lより先に書く
# この一覧は、ファイルを手動で列挙するにはどうしたらいい？
slim_SOURCES =                                                     \
	main.c                          lmntal.h                         \
	env.c                           arch.h                           \
	\
	lmntal_ext.h                                                     \
	ext/init_exts.c                                                  \
	ext/integer.c                                                    \
	ext/float.c                                                      \
	ext/atomic.c                                                     \
	ext/nlmem.c                                                      \
	ext/io.c                                                         \
	ext/array.c			ext/array.h                      \
	ext/atom.c                                                       \
	ext/react_rule.c                                                 \
	ext/initial_ruleset.c                                            \
	ext/nd_conf.c							 \
	ext/time.c
#	ext.h ext.c

slim_OBJS =

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig

ALL_HEADER_FILES := $(wildcard ./*/*.h) $(wildcard ./*.h)
ALL_SOURCE_FILES := $(wildcard ./*/*.c) $(wildcard ./*.c)
ALL_PARSER_FILES := $(wildcard ./*/*_parser.c) $(wildcard ./*_parser.c) $(wildcard ./*/*_parser.h) $(wildcard ./*_parser.h)
ALL_LEXER_FILES := $(wildcard ./*/*_lexer.c) $(wildcard ./*_lexer.c) $(wildcard ./*/*_lexer.h) $(wildcard ./*_lexer.h)
EXCLUDE_FILES := translate_generated.c interpret_generated.c
ALL_FILES := $(ALL_HEADER_FILES) $(ALL_SOURCE_FILES)
DOC_FILES := $(filter-out $(ALL_PARSER_FILES) $(ALL_LEXER_FILES) $(EXCLUDE_FILES), $(ALL_FILES))

cldoc:
	cldoc generate $(slim_CFLAGS) -- --report --output ../html $(DOC_FILES)
cldoc-static:
	cldoc generate $(slim_CFLAGS) -- --static --report --output ../html $(DOC_FILES)
