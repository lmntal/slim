## Makefile.am for lib
## $Id: Makefile.am,v 1.8 2008/10/17 13:30:48 taisuke Exp $

LMN_CFLAGS = --slimcode -O3
LMN_FILES = $(wildcard *.lmn)
IL_FILES = $(patsubst %.lmn, %.il, $(wildcard *.lmn))

pkgdatadir = $(datadir)/@PACKAGE@/lib

EXTRA_DIST = $(LMN_FILES)
pkgdata_DATA = $(IL_FILES)
CLEANFILES = $(IL_FILES)

SUFFIXES = .lmn .il

LMNTAL="$(LMNTAL_HOME)/bin/lmntal"

.lmn.il:
	@# LMNtalコマンドはコンパイルに失敗しても常に0を返すので
	@# 標準エラー出力にERRORが含まれる場合、失敗にしている
	@if !(test -x $(LMNTAL)); then \
	  echo Cannot execute $(LMNTAL) ; false; \
	fi
	$(LMNTAL) $(LMN_CFLAGS) $< >$@ 2>__error_temp.txt
	!(grep -i error __error_temp.txt)

