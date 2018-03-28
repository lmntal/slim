
LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
                  $(top_srcdir)/test/tap-driver.sh

TESTS = \
	testsuite/basic/check.sh \
	testsuite/hyperlink/check.sh


check_DATA = \
	testsuite/basic/nd.il \
	testsuite/basic/nd2.il \
	testsuite/hyperlink/cycle.il

CLEANFILES = $(check_DATA)

LMNC = $(LMNTAL_HOME)/bin/lmntal
LMNCFLAGS = --hl-opt --slimcode -O3

%.il: %.lmn
	$(LMNC) $(LMNCFLAGS) $< >$@

