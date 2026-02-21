#!/bin/bash
if [ "$slim_CHECK_ND" != "yes" ]; then
  echo "1..0 # SKIP statespace tests require slim_CHECK_ND=yes"
  exit 77
fi
exec /home/runner/work/slim/slim/test/statespace/testsuite/advanced/check.sh
