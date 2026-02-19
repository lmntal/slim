# Contributing to SLIM

Thank you for your interest in contributing to SLIM! This guide will help you get started.

## Development Setup

### Prerequisites
- Ubuntu 24.04 (recommended) or compatible Linux distribution
- CMake 3.16+
- C++11 compatible compiler (GCC/Clang)
- LMNtal compiler (set `LMNTAL_HOME` environment variable)

### Quick Start
```bash
# Clone the repository
git clone <repository-url>
cd slim-cmake

# Install dependencies
sudo apt install cmake bison flex re2c ruby build-essential

# Set up environment
export LMNTAL_HOME=/path/to/lmntal/compiler

# Build
./build-cmake.sh

# Test
./build-test/src/slim --help
cd build-test && make test
```

## Development Workflow

### Creating a New Feature
```bash
# Create feature branch
git checkout -b feature/your-feature-name

# Clean build for new branch
rm -rf build*
./build-cmake.sh

# Make your changes...
# Edit source files, update CMakeLists.txt if needed

# Test your changes
cd build-test && make && make test

# Format code (if available)
find src -name "*.cpp" -o -name "*.h" | xargs clang-format -i

# Commit and push
git add .
git commit -m "Add your feature description"
git push origin feature/your-feature-name
```

### Code Standards
- Follow existing code style (2-space indentation, similar to Google style)
- Use `clang-format` when available
- Add tests for new functionality
- Update documentation for user-facing changes
- All commits must pass CI checks

### Adding New Source Files
1. Add source files to appropriate directory (`src/vm/`, `src/verifier/`, etc.)
2. Update the corresponding `CMakeLists.txt`
3. Clean rebuild: `rm -rf build-test && ./build-cmake.sh`

### Before Submitting PR
- [ ] Code builds successfully
- [ ] All tests pass
- [ ] Code is formatted consistently
- [ ] Documentation updated if needed
- [ ] Commit messages are clear and descriptive

## Testing

### Running Tests
```bash
cd build-test
make test                    # All tests
ctest -R "system_"          # System tests only
ctest -R "statespace_"      # State space tests only
export slim_CHECK_ND=yes && make test  # Include non-deterministic tests
```

### Adding Tests
- System tests: Add `.lmntest` files to `test/system_check/testsuite/`
- Update appropriate `check.sh` scripts
- See `test/README.md` for detailed testing documentation

## Architecture Overview

### Core Components
- **VM** (`src/vm/`): Runtime virtual machine
- **Verifier** (`src/verifier/`): Model checker and state space exploration
- **Loader** (`src/loader/`): Intermediate language parser and loader
- **Element** (`src/element/`): Core data structures and utilities
- **Extensions** (`src/ext/`): Built-in rule implementations

### Build System
- **CMake-based**: Modern, cross-platform build system
- **Generated files**: Parsers and code generation handled automatically
- **Out-of-source builds**: All build artifacts in separate `build/` directories

## Getting Help

- Read `CLAUDE.md` for comprehensive development guide
- Check `CMAKE_MIGRATION.md` for build system details
- Look at existing code for patterns and conventions
- Open an issue for bugs or feature requests

## License

By contributing, you agree that your contributions will be licensed under the same license as the project.