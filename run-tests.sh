#!/bin/bash
# Simple script to run all tests from the project root directory
# Usage: ./run-tests.sh [ctest-arguments]
# Set VERBOSE=1 or V=1 environment variable for detailed test output

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"

# Check if build directory exists
if [ ! -d "$BUILD_DIR" ]; then
    echo "Error: Build directory '$BUILD_DIR' does not exist."
    echo "Please run './build-cmake.sh' first to build the project."
    exit 1
fi

# Change to build directory and run ctest with all arguments
echo "Running tests from $BUILD_DIR..."
cd "$BUILD_DIR"

# Set environment variables for verbose output if requested
if [ -n "$VERBOSE" ] || [ -n "$V" ]; then
    # Export variables so they're available to test processes
    export VERBOSE="$VERBOSE"
    export V="$V"
fi

exec ctest "$@"