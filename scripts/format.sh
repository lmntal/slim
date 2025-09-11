#!/bin/bash
# Code formatting script for SLIM

set -e

echo "Formatting C++ code with clang-format..."

# Check if clang-format is available
if ! command -v clang-format &> /dev/null; then
    echo "clang-format not found. Install with: sudo apt install clang-format"
    exit 1
fi

# Find and format all C++ source files
find src -name "*.cpp" -o -name "*.h" -o -name "*.hpp" | \
    grep -v -E "(build|third_party|generated)" | \
    xargs clang-format -i

echo "Code formatting complete!"

# Optional: Check for any changes
if git diff --quiet; then
    echo "No formatting changes needed."
else
    echo "Files have been formatted. Review changes with 'git diff'"
fi