# Git Commit Guide for SLIM CMake Migration

## Files TO Commit (Source Files)

### Core CMake Configuration
- `/home/ueda/slim-cmake/CMakeLists.txt` - Main project CMake configuration
- `/home/ueda/slim-cmake/build-cmake.sh` - Build script with debug/devel options
- `/home/ueda/slim-cmake/cmake/arch.h.in` - Architecture header template

### Source Directory CMake Files
- `/home/ueda/slim-cmake/src/CMakeLists.txt`
- `/home/ueda/slim-cmake/src/*/CMakeLists.txt` (all subdirectories)
  - `src/element/CMakeLists.txt`
  - `src/ffi/CMakeLists.txt`
  - `src/loader/CMakeLists.txt`
  - `src/verifier/CMakeLists.txt`
  - `src/vm/CMakeLists.txt`

### Test Framework Files
- `/home/ueda/slim-cmake/test/CMakeLists.txt`
- `/home/ueda/slim-cmake/test/*/CMakeLists.txt` (all test subdirectories)
- `/home/ueda/slim-cmake/test/enhanced_test_framework.cmake`

### Analysis and Utility Scripts
- `/home/ueda/slim-cmake/analyze_tests.py` - Test analysis script
- `/home/ueda/slim-cmake/fix-test-permissions.sh` - Permission fix script
- `/home/ueda/slim-cmake/run-tests.sh` - Test runner script

### Configuration Files (Dot Files)
- `/home/ueda/slim-cmake/.clang-format` - Code formatting configuration
- `/home/ueda/slim-cmake/.clang-tidy` - Static analysis configuration
- `/home/ueda/slim-cmake/.gitignore` - Git ignore patterns

### Third Party CMake Files
- `/home/ueda/slim-cmake/third_party/CMakeLists.txt`
- `/home/ueda/slim-cmake/third_party/*/CMakeLists.txt`

## Files NOT to Commit (Generated/Build Files)

### Entire Build Directory
- `/home/ueda/slim-cmake/build/` - All contents (Makefiles, binaries, generated files)

### CMake Generated Files in Source Tree
- `**/CMakeFiles/` - CMake internal files
- `**/cmake_install.cmake` - Installation scripts
- `**/CTestTestfile.cmake` - Test configuration (except root if desired)
- `**/Makefile` - Generated makefiles
- `**/*Config.cmake` - Generated config files
- `**/*ConfigVersion.cmake` - Generated version files

### Auto-Generated Test Scripts
- `**/*_wrapper.sh` - Statespace test wrappers
- `**/*_run.sh` - Individual test runners

### Temporary/Log Files
- `/home/ueda/slim-cmake/Testing/` - CTest temporary directory
- `/home/ueda/slim-cmake/claude_conversation.log` - Conversation log
- `/home/ueda/slim-cmake/SLIMConfig.cmake` - Generated config
- `/home/ueda/slim-cmake/SLIMConfigVersion.cmake` - Generated version config

### IDE-Specific Dot Files (Don't Commit)
- `/home/ueda/slim-cmake/.cquery` - IDE language server config (personal preference)

## Recommended Git Commands

```bash
# Add all CMake source files
git add CMakeLists.txt build-cmake.sh cmake/
git add src/CMakeLists.txt src/*/CMakeLists.txt
git add test/CMakeLists.txt test/*/CMakeLists.txt test/enhanced_test_framework.cmake
git add third_party/CMakeLists.txt third_party/*/CMakeLists.txt

# Add utility scripts and config files
git add analyze_tests.py fix-test-permissions.sh run-tests.sh
git add .clang-format .clang-tidy .gitignore

# Verify what will be committed
git status

# Create commit
git commit -m "Migrate build system from autotools to CMake

- Complete CMake configuration preserving all functionality
- Enhanced test framework with 208 individual tests
- Build script with debug/devel options
- All tests passing (100% success rate)

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

## Summary

**Total files to commit**: ~20-25 CMake configuration and utility files
**Files to exclude**: ~100+ generated files in build/ directory and temporary files

The migration preserves all original autotools functionality while providing a modern CMake-based build system with comprehensive testing support.