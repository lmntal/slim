#!/bin/sh
echo "option: -O0"
./check_unix.pl $* - -O0
echo "option: -O1"
./check_unix.pl $* - -O1
echo "option: -O2"
./check_unix.pl $* - -O2
echo "option: -O3"
./check_unix.pl $* - -O3
