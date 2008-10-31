bin_PROGRAMS = slim

slim_LDADD = -ldl

AM_YFLAGS = -d
# Flexが生成するヘッダファイルを指定する
AM_LFLAGS = --header-file=il_lex.h

# 事前に必要なファイル
BUILT_SOURCES = arch.h il_parser.h il_lex.h

# il_parser.y は il_parser.lより先に書く
# この一覧は、ファイルを手動で列挙するにはどうしたらいい？
slim_SOURCES = main.c membrane.c rule.c alloc.c error.c task.c memory_pool.c internal_hash.c \
	vector.c dumper.c system_ruleset.c atom.c \
	atom.h dumper.h instruction.h internal_hash.h lmntal.h membrane.h \
	memory_pool.h rule.h system_ruleset.h task.h vector.h st.c symbol.h symbol.c functor.h functor.c \
    syntax.h syntax.c il_parser.y il_lex.l instruction.c load.h load.c st.h arch.h \
    mc.h mc.c mhash.h mhash.c

DISTCLEANFILES := arch.h ../lib/config.lmn

# 字句解析機のヘッダファイルを生成
il_lex.h:
	$(LEX) $(LFLAGS) $(AM_LFLAGS) il_lex.l

arch.h ../lib/config.lmn: genconfig
	$(SHELL) ./genconfig
