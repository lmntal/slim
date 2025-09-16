#!/bin/sh

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

${SLIM_BINARY:-../../build/bin/slim} -I../../lib $slim_CHECK_OPTIONS "$SCRIPT_DIR/nlmem.il"
