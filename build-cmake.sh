#!/bin/bash
# CMake build script for SLIM
# Usage: ./build-cmake.sh [options]

set -e

# Default values
BUILD_TYPE="Release"
BUILD_DIR="build"
ENABLE_DEBUG="OFF"
ENABLE_DEVEL="OFF"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_TYPE="Debug"
            ENABLE_DEBUG="ON"
            shift
            ;;
        --devel)
            ENABLE_DEVEL="ON"
            shift
            ;;
        --build-dir)
            BUILD_DIR="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  --debug       Build in debug mode"
            echo "  --devel       Enable developer mode"
            echo "  --build-dir   Specify build directory (default: build)"
            echo "  --help        Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check required tools
command -v cmake >/dev/null 2>&1 || { echo "cmake is required but not installed. Aborting." >&2; exit 1; }
command -v bison >/dev/null 2>&1 || { echo "bison is required but not installed. Aborting." >&2; exit 1; }
command -v re2c >/dev/null 2>&1 || { echo "re2c is required but not installed. Aborting." >&2; exit 1; }
command -v ruby >/dev/null 2>&1 || { echo "ruby is required but not installed. Aborting." >&2; exit 1; }

# Check LMNTAL_HOME
if [ -z "$LMNTAL_HOME" ]; then
    echo "Warning: LMNTAL_HOME environment variable is not set."
    echo "This is required for compilation. Please set it to the LMNtal compiler directory."
fi

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure
echo "Configuring SLIM with CMake..."
cmake .. \
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
    -DENABLE_DEBUG="$ENABLE_DEBUG" \
    -DENABLE_DEVEL="$ENABLE_DEVEL" \
    -DCMAKE_INSTALL_PREFIX="$(pwd)"

# Build
echo "Building SLIM..."
make -j$(command -v nproc >/dev/null && nproc || sysctl -n hw.ncpu)

# Install (local installation to build directory - safe)
echo "Installing SLIM locally..."
make install

# Fix test script permissions
echo "Fixing test script permissions..."
find test -name "*_wrapper.sh" -exec chmod +x {} \; 2>/dev/null || true
find test -name "*_run.sh" -exec chmod +x {} \; 2>/dev/null || true

echo ""
echo "Build completed successfully!"
echo "Binary location: $BUILD_DIR/bin/slim"
echo ""
echo "To run tests:"
echo "  ./run-tests.sh                     # simplest"
echo "  ./run-tests.sh --output-on-failure # for detailed output"
echo "  slim_CHECK_ND=yes ./run-tests.sh   # include model checking tests"
echo ""
echo "To run with custom options:"
echo "  export slim_CHECK_OPTIONS='--your-options'"
echo "  make test"
echo ""
echo "Development tools:"
echo "  ./scripts/format.sh    # Format code with clang-format"
echo "  ./scripts/lint.sh      # Run static analysis"
echo "  cmake --preset debug   # Use CMake presets (if supported)"