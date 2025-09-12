#!/bin/sh

${SLIM_BINARY:-../../build/bin/slim} -I../../lib $slim_CHECK_OPTIONS testsuite/nlmem/nlmem.il
