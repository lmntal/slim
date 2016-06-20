## Makefile.am for SLIM
## $Id: Makefile.am,v 1.6 2008/09/30 05:19:47 taisuke Exp $

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = third_party src lib ext doc test
DIST_SUBDIRS = $(SUBDIRS)
DISTCLEANFILES = VERSION arch.h

cldoc:
	$(MAKE) cldoc -C src

cldoc-static:
	$(MAKE) cldoc-static -C src
