#!/bin/bash
# Static analysis with clang-tidy

set -e

echo "Running static analysis with clang-tidy..."

# Check if clang-tidy is available
if ! command -v clang-tidy &> /dev/null; then
    echo "clang-tidy not found. Install with: sudo apt install clang-tidy"
    exit 1
fi

# Ensure we have a compile database
if [ ! -f "build-test/compile_commands.json" ]; then
    echo "Building with compile database..."
    mkdir -p build-test
    cd build-test
    cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    make -j$(nproc)
    cd ..
fi

# Run clang-tidy on source files
find src -name "*.cpp" | \
    grep -v -E "(build|third_party|generated)" | \
    xargs clang-tidy -p build-test

echo "Static analysis complete!"