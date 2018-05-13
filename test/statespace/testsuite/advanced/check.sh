#!/bin/sh
if [ $slim_CHECK_ND = yes ]; then
./check.pl \
	../../benchmarkset/phi 7297 1\
	../../benchmarkset/phiM 24484 1\
	../../benchmarkset/bakery 32924 51\
	../../benchmarkset/mutex 61454 0\
	../../benchmarkset/rms 53657 104\
	../../benchmarkset/edfs 77841 261\
	../../benchmarkset/qlock 27407 0\
	../../benchmarkset/lbully 159326 1\
	../../benchmarkset/lring 73918 0\
	../../benchmarkset/firewire 33792 1\
	../../benchmarkset/abp 140000 0\
	../../benchmarkset/swp 32911 1\
	../../benchmarkset/byzantine 9258 2\
	../../benchmarkset/jsp 121367 17\
	../../benchmarkset/rabbit 186608 45790\
	../../benchmarkset/elevator 24882 0\
	../../benchmarkset/bubble 40320 1\
	../../benchmarkset/lambda 63998 3\
	../../benchmarkset/sstd 44513 1
else
./check.pl 
fi
