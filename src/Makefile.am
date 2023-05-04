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
slim_CXXFLAGS = -std=c++20                             \
               -I../third_party/zdelta-2.1             \
               -DCOMMIT_ID=\"`git show -s --format=%h`\"
slim_CFLAGS = -std=gnu99                               \
              -I../third_party/zdelta-2.1              \
              -DCOMMIT_ID=\"`git show -s --format=%h`\"

slim_DEPENDENCIES = ./verifier/liblmn_verifier.a       \
                    ./vm/liblmn_vm.a                   \
                    ./element/liblmn_elm.a             \
                    ./loader/liblmn_loader.a

if ENABLE_JNI
slim_LDADD += -ljvm
slim_LDFLAGS += -L../
slim_DEPENDENCIES += ../libjvm.a
endif

if ENABLE_GPERFTOOLS
slim_LDADD += -ltcmalloc
else
if ENABLE_TCMALLOC
slim_LDADD += -ltcmalloc_minimal
endif
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
	main.cpp                          lmntal.h                         \
	env.cpp                           arch.h                           \
	options.cpp                       options.h                    \
	\
	lmntal_ext.h                                                     \
	ext/init_exts.cpp                                                  \
	ext/integer.cpp                                                    \
	ext/float.cpp                                                      \
	ext/nlmem.cpp                                                      \
	ext/array.cpp			ext/array.h                      \
	ext/atom.cpp                                                     \
	ext/react_rule.cpp                                               \
	ext/initial_ruleset.cpp                                          \
	ext/nd_conf.cpp							 \
	ext/set.cpp			ext/set.h			 \
	ext/state_map.cpp						 \
	ext/membrane.cpp      ext/membrane.h       \
	ext/zerostep.cpp \
	ext/time.cpp
#	ext.h ext.c

slim_OBJS =

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig
