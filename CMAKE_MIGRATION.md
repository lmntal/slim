# CMake Migration Guide for SLIM

This document describes the migration from autotools to CMake for the SLIM project.

## Overview

The SLIM project has been migrated from autotools (autoconf/automake) to CMake for improved cross-platform compatibility and easier maintenance.

## Quick Start

### Prerequisites

- CMake 3.16 or later
- C++11 compatible compiler (g++/clang++)
- C99 compatible compiler (gcc/clang)
- Bison 3.0+
- re2c
- flex
- Ruby (for code generation)
- Required libraries: pthread, zlib, libdl
- LMNTAL_HOME environment variable set to LMNtal compiler path

### Building

```bash
# Set up LMNtal compiler path
export LMNTAL_HOME=/path/to/lmntal/compiler

# Simple build
./build-cmake.sh

# Debug build
./build-cmake.sh --debug

# Developer build
./build-cmake.sh --devel

# Custom build directory
./build-cmake.sh --build-dir build-custom
```

### Manual CMake Usage

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
make install
```

## Configuration Options

CMake options that replace autotools configure flags:

| Autotools Flag | CMake Option | Description |
|----------------|--------------|-------------|
| `--enable-debug` | `-DENABLE_DEBUG=ON` | Enable debug mode |
| `--enable-devel` | `-DENABLE_DEVEL=ON` | Enable developer mode |
| `--enable-profile` | `-DENABLE_PROFILE=ON` | Enable profiling |
| `--enable-gprof` | `-DENABLE_GPROF=ON` | Enable GNU gprof |
| `--enable-jni` | `-DENABLE_JNI=ON` | Enable JNI support |
| `--enable-tcmalloc` | `-DENABLE_TCMALLOC=ON` | Use tcmalloc |
| `--enable-gperftools` | `-DENABLE_GPERFTOOLS=ON` | Use Google perftools |
| `--enable-opt-minmax` | `-DENABLE_OPT_MINMAX=ON` | Enable minmax optimization |
| `--enable-cunit` | `-DENABLE_CUNIT=ON` | Enable CUnit testing |
| `--enable-minimal-state` | `-DENABLE_MINIMAL_STATE=ON` | Enable minimal state |
| `--enable-firstclass-rule` | `-DENABLE_FIRSTCLASS_RULE=ON` | Enable first class rules |

### Example Configurations

```bash
# Debug build with developer options
cmake .. -DCMAKE_BUILD_TYPE=Debug -DENABLE_DEBUG=ON -DENABLE_DEVEL=ON

# Optimized build with tcmalloc
cmake .. -DCMAKE_BUILD_TYPE=Release -DENABLE_TCMALLOC=ON

# Profile build
cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_PROFILE=ON -DENABLE_GPROF=ON
```

## Testing

The testing system maintains compatibility with the original TAP-based tests:

```bash
# Run all tests
make test
# or
ctest --output-on-failure

# Run specific test category
ctest -R "system_"     # System tests
ctest -R "statespace_" # State space tests  
ctest -R "library_"    # Library tests

# Enable non-deterministic tests (time-intensive)
export slim_CHECK_ND=yes
make test
```

## Project Structure

The CMake build system preserves the original directory structure:

```
slim/
├── CMakeLists.txt              # Main CMake configuration
├── build-cmake.sh              # Convenient build script
├── src/
│   ├── CMakeLists.txt         # Main executable
│   ├── vm/CMakeLists.txt      # Virtual machine library
│   ├── verifier/CMakeLists.txt # Model checker library
│   ├── loader/CMakeLists.txt   # IL loader library
│   ├── element/CMakeLists.txt  # Element library
│   └── ffi/CMakeLists.txt     # FFI library
├── third_party/
│   └── zdelta-2.1/CMakeLists.txt # Compression library
├── test/
│   ├── CMakeLists.txt         # Test configuration
│   ├── system_check/          # System tests
│   ├── statespace/            # State space tests
│   └── library_check/         # Library tests
├── lib/CMakeLists.txt         # Library files
├── ext/CMakeLists.txt         # Extensions
└── doc/CMakeLists.txt         # Documentation
```

## Key Differences from Autotools

### Code Generation

- **Parsers**: Bison/re2c generated files are built in the binary directory
- **Ruby Generation**: translate_generated.cpp and interpret_generated.cpp are generated at build time
- **Config Files**: config.h is generated from config.h.cmake.in template

### Dependencies  

- **Library Detection**: Uses CMake's find_package() and find_library()
- **Feature Tests**: Atomic operations, thread-local storage, etc. tested at configure time
- **External Tools**: Bison, re2c, Ruby located automatically

### Installation

- **Prefix**: Uses standard CMAKE_INSTALL_PREFIX
- **Components**: Binary, libraries, documentation, man pages
- **Packaging**: Generates CMake package config files

## Migration Notes

### What Was Preserved

- All build options and their semantics
- Complete testing framework compatibility  
- Library structure and linking
- Code generation steps
- Installation layout

### What Changed

- Configuration syntax (CMake vs autotools)
- Generated file locations (build directory vs source)
- Some internal build logic simplified
- Better cross-platform support

### Potential Issues

1. **LMNTAL_HOME**: Still required as environment variable
2. **genconfig**: The arch.h generation may need adjustment for specific platforms
3. **Generated Files**: Build directory structure differs slightly
4. **Test Scripts**: Some test scripts may need path adjustments

## Troubleshooting

### Build Failures

1. **Missing tools**: Ensure bison, re2c, ruby are installed and in PATH
2. **LMNTAL_HOME**: Must be set to LMNtal compiler directory
3. **Generated files**: Clear build directory and reconfigure if parsers fail

### Test Failures

1. **Path issues**: Tests expect slim binary in build/src/
2. **Environment**: Set LMNTAL_HOME for compilation-dependent tests
3. **Permissions**: Ensure test scripts are executable

### macOS (M1–M4 / Apple Silicon)

1. **bison**: macOS ships bison 2.3 in `/usr/bin`, which fails the project's
   `Bison 3.0+` check. Install a current bison via Homebrew and put it ahead of
   `/usr/bin` on `PATH`:
   ```bash
   brew install cmake bison re2c ruby ant
   export PATH="$(brew --prefix bison)/bin:$PATH"
   ```
2. **OpenMP**: stock Apple Clang has no OpenMP support. `find_package(OpenMP)` is
   optional in `CMakeLists.txt`, so the build just proceeds without it unless you
   `brew install libomp` and point CMake at it.
3. **LMNTAL_HOME**: same requirement as Linux — build `lmntal-compiler` with
   `ant` first (requires a JDK) and point `LMNTAL_HOME` at it.
4. **libdl**: `CMakeLists.txt` does `find_library(dl REQUIRED)`. This normally
   resolves against the SDK's `libdl.tbd` stub, but if configure fails specifically
   on `dl`, that's the first place to look.

## Performance

The CMake build system typically provides:

- **Faster configuration** than autotools
- **Better dependency tracking** for incremental builds
- **Parallel builds** with better dependency resolution
- **Cross-platform compatibility**

## Future Enhancements

Potential improvements to consider:

1. **vcpkg/Conan integration** for better dependency management
2. **CPackage configuration** for distribution packages  
3. **Static analysis integration** (clang-tidy, cppcheck)
4. **Continuous integration** configuration
5. **Docker build environment** for consistent builds