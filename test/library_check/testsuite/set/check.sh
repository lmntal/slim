#!/bin/sh

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Get the project root (script is in test/library_check/testsuite/set/)
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

${SLIM_BINARY:-"$PROJECT_ROOT/build/bin/slim"} -I"$PROJECT_ROOT/lib" $slim_CHECK_OPTIONS "$SCRIPT_DIR/set.il"
