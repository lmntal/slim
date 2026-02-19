#!/bin/bash
# Fix permissions on generated test scripts
# This should be run after rebuilds to ensure test scripts are executable

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/build"

if [ -d "$BUILD_DIR" ]; then
    echo "Fixing test script permissions..."
    find "$BUILD_DIR/test" -name "*_wrapper.sh" -exec chmod +x {} \; 2>/dev/null || true
    find "$BUILD_DIR/test" -name "*_run.sh" -exec chmod +x {} \; 2>/dev/null || true
    echo "Test script permissions fixed."
else
    echo "Build directory not found at $BUILD_DIR"
    exit 1
fi