#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
LMNTAL="${LMNTAL_HOME:-$PROJECT_ROOT/../lmntal-compiler}/bin/lmntal"

for base in cycle mc-hl lambda_nd-hl7; do
    LMN="$SCRIPT_DIR/$base.lmn"
    IL="$SCRIPT_DIR/$base.il"
    if [ ! -f "$IL" ] || [ "$LMN" -nt "$IL" ]; then
        "$LMNTAL" --slimcode "$LMN" > "$IL"
    fi
done

./check.pl \
    testsuite/hyperlink/cycle 113 54\
    testsuite/hyperlink/mc-hl 45 12\
    testsuite/hyperlink/lambda_nd-hl7 2874 2
