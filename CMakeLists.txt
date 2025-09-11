cmake_minimum_required(VERSION 3.16)
project(SLIM VERSION 2.8.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Get environment variables
set(LMNTAL_HOME $ENV{LMNTAL_HOME})

# Project information
set(SLIM_VERSION ${PROJECT_VERSION})
set(PACKAGE_NAME "slim")
set(PACKAGE_BUGREPORT "lmntal@ueda.info.waseda.ac.jp")

# Build type configuration
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type" FORCE)
endif()

# Options
option(ENABLE_DEBUG "Enable debug mode (affects performance)" OFF)
option(ENABLE_DEVEL "Enable developer mode" OFF)
option(ENABLE_PROFILE "Enable runtime profiler" OFF)
option(ENABLE_GPROF "Enable GNU gprof support" OFF)
option(ENABLE_JNI "Enable JNI interactive mode" OFF)
option(ENABLE_TCMALLOC "Use tcmalloc from Google perftools" OFF)
option(ENABLE_GPERFTOOLS "Use full Google perftools" OFF)
option(ENABLE_OPT_MINMAX "Enable minmax optimization mode" OFF)
option(ENABLE_CUNIT "Enable CUnit testing" OFF)
option(ENABLE_MINIMAL_STATE "Enable minimal state mode" OFF)
option(ENABLE_FIRSTCLASS_RULE "Enable first class rules" OFF)

# Find required programs and libraries
find_program(BISON_EXECUTABLE bison REQUIRED)
find_program(RE2C_EXECUTABLE re2c REQUIRED)
find_program(FLEX_EXECUTABLE flex REQUIRED)

# Check Bison version
execute_process(
    COMMAND ${BISON_EXECUTABLE} --version
    OUTPUT_VARIABLE BISON_VERSION_OUTPUT
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
if(BISON_VERSION_OUTPUT MATCHES "bison \\(GNU Bison\\) ([0-9]+)\\.([0-9]+)")
    set(BISON_VERSION_MAJOR ${CMAKE_MATCH_1})
    set(BISON_VERSION_MINOR ${CMAKE_MATCH_2})
    if(BISON_VERSION_MAJOR LESS 3)
        message(FATAL_ERROR "Bison 3.0 or later is required, found ${BISON_VERSION_MAJOR}.${BISON_VERSION_MINOR}")
    endif()
endif()

# Find Ruby for code generation
find_program(RUBY_EXECUTABLE ruby REQUIRED)

# Find required libraries
find_package(Threads REQUIRED)
if(Threads_FOUND)
    set(HAVE_LIBPTHREAD 1)
endif()
find_library(LIBM_LIBRARY m)
find_library(LIBZ_LIBRARY z)
find_library(LIBRT_LIBRARY rt)
find_library(LIBDL_LIBRARY dl REQUIRED)

# OpenMP support
find_package(OpenMP)
if(OpenMP_FOUND)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()

# Check for headers
include(CheckIncludeFiles)
check_include_files("sys/time.h" HAVE_SYS_TIME_H)
check_include_files("sched.h" HAVE_SCHED_H)
check_include_files("syscall.h" HAVE_SYSCALL_H)
check_include_files("omp.h" HAVE_OMP_H)
check_include_files("execinfo.h" HAVE_EXECINFO_H)

# Check for types
include(CheckTypeSize)
check_type_size("long" SIZEOF_LONG)
check_type_size("void*" SIZEOF_VOID_P)
check_type_size("double" SIZEOF_DOUBLE)
check_type_size("uintptr_t" SIZEOF_UINTPTR_T)

# Check for atomic operations
include(CheckCSourceRuns)
check_c_source_runs("
    int main(void) {
        int x = 0, y = 0, z = 0;
        __sync_bool_compare_and_swap(&x, y, z);
        return 0;
    }
" HAVE_ATOMIC_CAS)

check_c_source_runs("
    int main(void) {
        int x = 0, y = 0;
        __sync_add_and_fetch(&x, y);
        return 0;
    }
" HAVE_ATOMIC_ADD)

check_c_source_runs("
    __thread int x;
    int main(void) { return 0; }
" HAVE___THREAD)

# Get processor count
include(ProcessorCount)
ProcessorCount(PROCESSOR_COUNT)
if(NOT DEFINED PROCESSOR_COUNT OR PROCESSOR_COUNT EQUAL 0)
    set(PROCESSOR_COUNT 1)
endif()

# Configure compiler flags based on options
if(ENABLE_DEBUG)
    add_definitions(-DDEBUG=1)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O -g -fno-omit-frame-pointer -Wall")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O -g -fno-omit-frame-pointer -Wall")
else()
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()

if(ENABLE_DEVEL)
    add_definitions(-DDEVEL=1)
    if(CMAKE_COMPILER_IS_GNUCC)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ggdb -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare -Winline")
    endif()
endif()

if(ENABLE_PROFILE)
    add_definitions(-DPROFILE=1)
endif()

if(ENABLE_GPROF)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pg -g")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg -g")
endif()

if(ENABLE_JNI)
    add_definitions(-DUSE_JNI=1)
    find_package(JNI REQUIRED)
    include_directories(${JNI_INCLUDE_DIRS})
endif()

if(ENABLE_TCMALLOC OR ENABLE_GPERFTOOLS)
    find_library(TCMALLOC_LIBRARY tcmalloc REQUIRED)
    add_definitions(-DHAVE_TCMALLOC=1)
    if(CMAKE_COMPILER_IS_GNUCC)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-builtin-malloc -fno-builtin-calloc -fno-builtin-realloc -fno-builtin-free")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-builtin-malloc -fno-builtin-calloc -fno-builtin-realloc -fno-builtin-free")
    endif()
endif()

if(ENABLE_GPERFTOOLS)
    add_definitions(-DHAVE_GPERFTOOLS=1)
endif()

if(ENABLE_OPT_MINMAX)
    add_definitions(-DKWBT_OPT=1)
endif()

if(ENABLE_MINIMAL_STATE)
    add_definitions(-DMINIMAL_STATE=1)
endif()

if(ENABLE_FIRSTCLASS_RULE)
    add_definitions(-DUSE_FIRSTCLASS_RULE=1)
endif()

# Get Git commit hash
execute_process(
    COMMAND git show -s --format=%h
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE GIT_COMMIT_ID
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)
if(NOT GIT_COMMIT_ID)
    set(GIT_COMMIT_ID "unknown")
endif()

# Generate config header
configure_file(
    ${CMAKE_SOURCE_DIR}/src/config.h.cmake.in
    ${CMAKE_BINARY_DIR}/src/config.h
)

# Generate arch.h with CMake configuration
configure_file(
    ${CMAKE_SOURCE_DIR}/cmake/arch.h.in
    ${CMAKE_SOURCE_DIR}/src/arch.h
)

# Add subdirectories
add_subdirectory(third_party)
add_subdirectory(src)
add_subdirectory(lib)
add_subdirectory(ext)
add_subdirectory(doc)

# Testing
enable_testing()
add_subdirectory(test)

# Install rules
install(TARGETS slim
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Create package config
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/SLIMConfigVersion.cmake"
    VERSION ${SLIM_VERSION}
    COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(
    "${CMAKE_SOURCE_DIR}/cmake/SLIMConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/SLIMConfig.cmake"
    INSTALL_DESTINATION lib/cmake/SLIM
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/SLIMConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/SLIMConfigVersion.cmake"
    DESTINATION lib/cmake/SLIM
)

# Print configuration summary
message(STATUS "")
message(STATUS "Configuration Summary:")
message(STATUS "  Version: ${SLIM_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Debug mode: ${ENABLE_DEBUG}")
message(STATUS "  Developer mode: ${ENABLE_DEVEL}")
message(STATUS "  Profiling: ${ENABLE_PROFILE}")
message(STATUS "  JNI support: ${ENABLE_JNI}")
message(STATUS "  TCMalloc: ${ENABLE_TCMALLOC}")
message(STATUS "  Google perftools: ${ENABLE_GPERFTOOLS}")
message(STATUS "  Processor count: ${PROCESSOR_COUNT}")
message(STATUS "  Git commit: ${GIT_COMMIT_ID}")
message(STATUS "")