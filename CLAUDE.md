# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SLIM (Slim LMNtal IMprementation) is a runtime and model checker for the LMNtal (pronounced "elemental") language. This is a C++ project that uses CMake for building and configuration.

## Essential Commands

### Environment Setup
```bash
export LMNTAL_HOME=/path/to/lmntal/compiler  # Required: Path to LMNtal Java compiler
```

### Building (CMake - Recommended)
```bash
./build-cmake.sh                # Simple CMake build
./build-cmake.sh --debug        # Debug build with debugging symbols
./build-cmake.sh --devel        # Developer build with extra warnings
```

### Manual CMake Build
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$(pwd)
make -j$(nproc)
make install
```


### Development Build Options (CMake)
```bash
cmake .. -DENABLE_DEBUG=ON      # Include DEBUG code (assert statements)
cmake .. -DENABLE_DEVEL=ON      # Development compilation options
cmake .. -DENABLE_PROFILE=ON    # Profiling options (affects runtime performance)
cmake .. -DENABLE_TCMALLOC=ON   # Use tcmalloc for better memory management
cmake .. -DENABLE_JNI=ON        # Enable JNI interactive mode
```


### Testing (CMake)
```bash
# From build directory
make test                       # Run all tests using CTest
ctest --output-on-failure       # Run tests with detailed output

# From project root (recommended)
./run-tests.sh                  # Run all 208 individual tests from root (simplest)
./run-tests.sh --output-on-failure # Run tests with detailed output from root
./run-tests.sh -R "system_basic_basic1" # Run specific individual test

# Alternative: Direct ctest (requires --test-dir build for cross-platform compatibility)
ctest --test-dir build          # Run all tests
ctest --test-dir build --output-on-failure # Run tests with detailed output

# If tests fail with "permission denied", fix script permissions
./fix-test-permissions.sh       # Fix test script permissions after rebuild

# Test configuration
export slim_CHECK_ND=yes        # Enable non-deterministic execution tests (time-intensive)
export slim_CHECK_OPTIONS="--history-management"  # Set runtime options for tests
```

## CTest Reference

### Common CTest Options
```bash
# Built-in help
ctest --help                    # Show all available options
ctest --help-full              # Show detailed help with examples

# Test selection
ctest -R "pattern"              # Run tests matching regex pattern
ctest -E "pattern"              # Exclude tests matching pattern  
ctest -L "label"                # Run tests with specific label

# Output control
ctest --output-on-failure       # Show output only for failed tests
ctest --verbose                 # Show all test output
ctest -V                        # Same as --verbose
ctest --quiet                   # Minimal output

# Execution control
ctest -j N                      # Run N tests in parallel
ctest --parallel N              # Same as -j N
ctest --timeout N               # Set timeout for tests (seconds)

# Rerun options
ctest --rerun-failed            # Only rerun previously failed tests
ctest --repeat N                # Run each test N times

# Information
ctest --show-only               # List all tests without running
ctest -N                        # Same as --show-only
```

### SLIM-Specific Examples
```bash
./run-tests.sh -R "system_basic"         # Run all basic system tests
./run-tests.sh -R "statespace"           # Run all statespace tests
./run-tests.sh -L "system"               # Run tests labeled as "system"
./run-tests.sh -E "statespace"           # Run all tests except statespace
./run-tests.sh -j 4                      # Run tests using 4 parallel jobs
./run-tests.sh -R "system_proccxt_case5" # Run specific individual test
./run-tests.sh --timeout 60              # Set 60-second timeout for all tests
```


### Running SLIM
```bash
lmntal --slimcode source.lmn > source.il   # Compile LMNtal to intermediate language
./bin/slim source.il                       # Execute with runtime
./bin/slim --help                          # Show available options
```

### Model Checking
```bash
./bin/slim --nd source.il                           # Single core non-deterministic execution
./bin/slim --nd --use-Ncore=12 source.il           # Multi-core execution
./bin/slim --nd --use-Ncore=12 --delta-mem source.il # Multi-core with optimization
```

## Code Architecture

### Core Components
- **src/vm/**: Virtual machine implementation - core runtime engine, memory management, atom/membrane handling, rule execution
- **src/verifier/**: Model checker implementation - state space exploration, DPOR algorithms, LTL model checking
- **src/loader/**: Intermediate language loader - parses .il files, translates to runtime structures

### Key Modules
- **src/vm/membrane.cpp**: Core data structure representing LMNtal membranes
- **src/vm/atom.cpp**: Atomic processes and their management
- **src/vm/rule.cpp**: Rule compilation and execution engine
- **src/verifier/state.cpp**: State representation for model checking
- **src/verifier/mc.cpp**: Main model checking coordination
- **src/loader/syntax.cpp**: IL syntax parsing and validation

### Test Structure
- **test/system_check/**: Runtime behavior tests using TAP protocol
- **test/statespace/**: Non-deterministic execution and state space tests
- **test/library_check/**: Built-in library functionality tests

## Development Guidelines

### Code Standards
- Follow C99 standard with autoconf extensions (inline, int32_t, etc.)
- Use `lmn` prefix for all externally visible functions, types, and macros
- Tab width: 2 spaces
- Treat compiler warnings as errors
- Always use `--enable-debug` during development

### Testing Protocol
- Use TAP (Test Anything Protocol) for all tests
- Test files: `<testname>.lmntest` contain program + expected output + ok/ng
- Add new tests to appropriate `test/*/Makefile.am` files
- Run tests frequently and keep them updated

### Code Formatting
- Currently no automatic formatter used
- Strongly recommend ClangFormat for new code: `clang-format -i filename.cpp`

### Required Dependencies  
- **CMake 3.16+** (build system)
- g++/clang++ (C++11), gcc/clang (C99)
- flex 2.5.35+, re2c 1.0.3+, bison 3.0+
- ruby 1.9.3+, cunit (optional)

## CMake Workflow

### Clean Build Commands
```bash
# From build directory
cd build && make clean          # Clean build files (keeps CMake cache)
cmake --build build --target clean  # Alternative clean from root

# Complete clean rebuild (recommended)
rm -rf build && ./build-cmake.sh    # Complete clean rebuild
```

### Build Troubleshooting
```bash
# If build issues occur, always try complete clean first
rm -rf build
./build-cmake.sh

# Check CMake configuration with verbose output
cd build && cmake .. -DCMAKE_VERBOSE_MAKEFILE=ON
```

### CMake-Only Build System
- **Clean CMake build**: No autotools dependencies required
- **All functionality**: Building, testing, and installation via CMake

## Important Notes

- LMNTAL_HOME environment variable is required for compilation (points to Java-based LMNtal compiler)
- When adding .h/.c files to src/, update appropriate CMakeLists.txt files
- Use dmalloc or valgrind for memory debugging during development
- Non-deterministic tests are time-intensive - use slim_CHECK_ND=yes sparingly
- CMake builds are out-of-source (build/ directory) - safe to delete anytime
- 26 generated "computed 'neq(10.4000,10.4000)' matches expected ' yes. '".  Similar for #27, #42, #16.  Please examine them.