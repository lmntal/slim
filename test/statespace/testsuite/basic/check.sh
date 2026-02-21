#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
LMNTAL="${LMNTAL_HOME:-$PROJECT_ROOT/../lmntal-compiler}/bin/lmntal"

for base in nd nd2; do
    LMN="$SCRIPT_DIR/$base.lmn"
    IL="$SCRIPT_DIR/$base.il"
    if [ ! -f "$IL" ] || [ "$LMN" -nt "$IL" ]; then
        "$LMNTAL" --slimcode "$LMN" > "$IL"
    fi
done

./check.pl \
	testsuite/basic/nd 9 1\
	testsuite/basic/nd2 10 5
