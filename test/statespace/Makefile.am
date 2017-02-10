
LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
                  $(top_srcdir)/test/tap-driver.sh

TESTS = \
	testsuite/basic/check.sh


check_DATA = \
	testsuite/basic/nd.il \
	testsuite/basic/nd2.il

CLEANFILES = $(check_DATA)

LMNC = $(LMNTAL_HOME)/bin/lmntal
LMNCFLAGS = --hl --slimcode -O3

%.il: %.lmn
	$(LMNC) $(LMNCFLAGS) $< >$@

