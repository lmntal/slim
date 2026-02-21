#!/bin/bash
cd /home/runner/work/slim/slim/test/system_check
# Inherit VERBOSE environment variable
export VERBOSE="${VERBOSE}"
export V="${V}"
./check.pl "$SLIM_BINARY" "/home/runner/work/slim/slim/test/system_check/testsuite/guard_ground/case12.il"
