# SLIM Testing Guide

## Overview

SLIM uses a modern, comprehensive testing framework built on CTest with enhanced categorization, parallel execution, and performance benchmarking.

## Test Categories

### 🔧 **System Tests** (`make test-quick`)
- **Location**: `test/system_check/testsuite/`
- **Purpose**: Core functionality testing
- **Format**: `.lmntest` files with program + expected output
- **Timeout**: 30 seconds per test
- **Parallelization**: Up to 8 tests simultaneously

### 📚 **Library Tests** (`make test-quick`) 
- **Location**: `test/library_check/testsuite/`
- **Purpose**: Built-in library functionality
- **Options**: Uses `--use-builtin-rule`
- **Timeout**: 45 seconds per test

### 🌌 **State Space Tests** (`make test-model-checking`)
- **Location**: `test/statespace/testsuite/`
- **Purpose**: Model checking and non-deterministic execution
- **Options**: Uses `--nd` flag
- **Timeout**: 5 minutes per test
- **Parallelization**: Sequential execution (resource intensive)

### ⚡ **Performance Benchmarks** (`make test-performance`)
- **Location**: `test/benchmarks/`
- **Purpose**: Performance regression testing
- **Output**: JSON reports with timing and memory statistics
- **Timeout**: 10 minutes per benchmark

## Running Tests

### **Quick Commands**
```bash
# All tests
make test

# Quick tests only (system + library)
make test-quick

# Model checking tests
make test-model-checking

# Performance benchmarks  
make test-performance

# Parallel execution
make test-parallel
```

### **Advanced CTest Commands**
```bash
# Run specific test patterns
ctest -R "basic"              # All tests with "basic" in name
ctest -L "system"             # All system tests
ctest -L "statespace"         # All model checking tests

# Parallel execution with custom job count
ctest --parallel 4

# Verbose output
ctest --verbose --output-on-failure

# Run only failed tests from last run
ctest --rerun-failed

# Generate XML report
ctest -T Test --output-log test_results.xml
```

### **Environment Configuration**
```bash
# Required environment variable
export LMNTAL_HOME=/path/to/lmntal/compiler

# Optional test customization
export slim_CHECK_OPTIONS="--history-management"
export slim_CHECK_ND="yes"    # Enable non-deterministic tests
```

## Test Development

### **Adding New Tests**

1. **System/Library Tests**:
   ```bash
   # Create test file: test/system_check/testsuite/mygroup/mytest.lmntest
   # Format:
   # Line 1: LMNtal program
   # Line 2: Expected output  
   # Line 3: "ok" or "ng"
   ```

2. **State Space Tests**:
   ```bash
   # Create: test/statespace/testsuite/mygroup/mytest.lmntest
   # Tests non-deterministic execution and model checking
   ```

3. **Performance Benchmarks**:
   ```bash
   # Create: test/benchmarks/mybenchmark.lmn
   # Standard LMNtal file for performance measurement
   ```

### **Test File Format (.lmntest)**
```
a(X), b(Y) :- X=Y, c(X).
c(hello).
ok
```

- **Line 1**: LMNtal program
- **Line 2**: Expected output
- **Line 3**: Test expectation (`ok` = output should match, `ng` = output should differ)

### **Auto-Discovery**
Tests are automatically discovered by CMake - no need to manually update test lists!

## Advanced Features

### **Test Coverage** (Debug builds)
```bash
# Build with coverage
cmake .. -DCMAKE_BUILD_TYPE=Debug -DENABLE_COVERAGE=ON
make
make coverage
```

### **Test Result Analysis**
```bash
# JSON output for programmatic analysis
./test/test_runner.py --test-file mytest.lmntest --json

# Benchmark comparison
make benchmark-compare
```

### **Continuous Integration**
Tests run automatically on:
- Every commit to main/develop branches
- Pull requests
- Multiple compiler configurations (GCC/Clang)
- Debug and Release builds

## Troubleshooting

### **Common Issues**

1. **LMNTAL_HOME not set**:
   ```bash
   export LMNTAL_HOME=/path/to/lmntal/compiler
   ```

2. **Tests timeout**:
   ```bash
   # Increase timeout for specific tests
   ctest --timeout 300
   ```

3. **Parallel test failures**:
   ```bash
   # Run tests sequentially
   ctest --parallel 1
   ```

4. **Test discovery issues**:
   ```bash
   # Regenerate test configuration
   rm -rf build && mkdir build && cd build && cmake ..
   ```

### **Performance Tips**

- **Quick feedback**: Use `make test-quick` during development
- **Parallel execution**: Use `make test-parallel` on multi-core systems  
- **Selective testing**: Use `ctest -R pattern` to run specific tests
- **Skip heavy tests**: Set `slim_CHECK_ND=""` to skip model checking tests

## Test Statistics

Current test suite:
- **175+ test cases** across 28 test suites
- **Auto-discovered** from directory structure
- **Parallel execution** support
- **Multiple output formats** (TAP, JSON, XML)
- **Performance tracking** with historical comparison