#!/bin/sh

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Get the project root (script is in test/library_check/testsuite/statespace/)
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"

# Compile .lmn to .il if needed
LMN_FILE="$SCRIPT_DIR/statespace.lmn"
IL_FILE="$SCRIPT_DIR/statespace.il"
if [ ! -f "$IL_FILE" ] || [ "$LMN_FILE" -nt "$IL_FILE" ]; then
    "${LMNTAL_HOME:-$PROJECT_ROOT/../lmntal-compiler}/bin/lmntal" --slimcode "$LMN_FILE" > "$IL_FILE"
fi

# Run the test and output results
${SLIM_BINARY:-"$PROJECT_ROOT/build/bin/slim"} -I"$PROJECT_ROOT/lib" $slim_CHECK_OPTIONS "$IL_FILE"