SUBDIRS = verifier vm loader element ffi

bin_PROGRAMS = slim

slim_LDADD =   -llmn_loader                            \
               -llmn_verifier                          \
               -llmn_vm                                \
               -llmn_ffi                               \
               -llmn_elm                               \
               -lzd_in_slim                            \
               -ldl
slim_LDFLAGS = -L./verifier                            \
               -L./loader                              \
               -L./ffi                                 \
               -L./vm                                  \
               -L./element                             \
               -L../third_party/zdelta-2.1
slim_CFLAGS =  -I../third_party/zdelta-2.1             \
               -DCOMMIT_ID=\"`git show -s --format=%h`\" \
               -std=gnu99

slim_DEPENDENCIES = ./verifier/liblmn_verifier.a       \
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
	ext/state_map.c							 \
	ext/time.c
#	ext.h ext.c

slim_OBJS =

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig
