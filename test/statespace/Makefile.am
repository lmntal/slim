LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
                  $(top_srcdir)/test/tap-driver.sh

TESTS = \
	testsuite/basic/check.sh \
	testsuite/hyperlink/check.sh \
	testsuite/advanced/check.sh

check_DATA = \
	testsuite/basic/nd.il \
	testsuite/basic/nd2.il \
	testsuite/hyperlink/cycle.il \
	../../benchmarkset/phi.il \
	../../benchmarkset/phiM.il \
	../../benchmarkset/bakery.il\
	../../benchmarkset/mutex.il\
	../../benchmarkset/rms.il\
	../../benchmarkset/edfs.il\
	../../benchmarkset/qlock.il\
	../../benchmarkset/lbully.il\
	../../benchmarkset/lring.il\
	../../benchmarkset/firewire.il\
	../../benchmarkset/abp.il\
	../../benchmarkset/swp.il\
	../../benchmarkset/byzantine.il\
	../../benchmarkset/jsp.il\
	../../benchmarkset/rabbit.il\
	../../benchmarkset/elevator.il\
	../../benchmarkset/bubble.il\
	../../benchmarkset/lambda.il\
	../../benchmarkset/sstd.il

CLEANFILES = $(check_DATA)

LMNC = $(LMNTAL_HOME)/bin/lmntal
LMNCFLAGS = --hl-opt --slimcode -O3

%.il: %.lmn
	$(LMNC) $(LMNCFLAGS) $< >$@

