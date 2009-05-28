## Makefile.am for lib
## $Id: Makefile.am,v 1.8 2008/10/17 13:30:48 taisuke Exp $

C_FILES := $(wildcard *.c)
SHARED_FILES := $(patsubst %.c, %.so, $(wildcard *.c))

pkgdatadir = $(datadir)/@PACKAGE@/ext

EXTRA_DIST := $(C_FILES)
pkgdata_DATA := $(SHARED_FILES)
CLEANFILES := $(SHARED_FILES)

.c.so:
	$(CC) $(CFLAGS) -shared -fPIC -o $@ $<
