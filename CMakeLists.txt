cmake_minimum_required(VERSION 3.22)

# Project information
project(slim
    VERSION 2.6.0
    DESCRIPTION "SLIM LMNtal IMplementation"
    LANGUAGES C CXX)

set(SLIM_PACKAGE_NAME "SLIM")
set(SLIM_URL "https://github.com/lmntal/slim")
set(SLIM_BUGREPORT "lmntal@ueda.info.waseda.ac.jp")

# ================================

# Global configuration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(SLIM_DATA_ROOT_DIR "${CMAKE_INSTALL_PREFIX}/share/slim" CACHE PATH "Root directory of data files")
set(SLIM_LIB_DIR "${SLIM_DATA_ROOT_DIR}/lib" CACHE PATH "Directory of library files")
set(SLIM_EXT_DIR "${SLIM_DATA_ROOT_DIR}/ext" CACHE PATH "Directory of external files")

# ================================

# On macOS, search Homebrew for keg-only versions of Bison and Flex. Xcode does
# not provide new enough versions for us to use.
if (CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin")
    execute_process(
        COMMAND brew --prefix bison
        RESULT_VARIABLE BREW_BISON
        OUTPUT_VARIABLE BREW_BISON_PREFIX
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if (BREW_BISON EQUAL 0 AND EXISTS "${BREW_BISON_PREFIX}")
        message(STATUS "Found Bison keg installed by Homebrew at ${BREW_BISON_PREFIX}")
        set(BISON_EXECUTABLE "${BREW_BISON_PREFIX}/bin/bison")
    endif()

    execute_process(
        COMMAND brew --prefix flex
        RESULT_VARIABLE BREW_FLEX
        OUTPUT_VARIABLE BREW_FLEX_PREFIX
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if (BREW_FLEX EQUAL 0 AND EXISTS "${BREW_FLEX_PREFIX}")
        message(STATUS "Found Flex keg installed by Homebrew at ${BREW_FLEX_PREFIX}")
        set(FLEX_EXECUTABLE "${BREW_FLEX_PREFIX}/bin/flex")
    endif()
endif()

# Find necessary packages and executables
find_package(OpenMP REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(RE2C REQUIRED)
find_package(Threads REQUIRED) # Both pthread and win32 threads
find_package(ZLIB REQUIRED)
find_package(Doxygen)

find_program(RUBY_EXECUTABLE ruby REQUIRED)
if (RUBY_EXECUTABLE)
    message(STATUS "Ruby found: ${RUBY_EXECUTABLE}")
endif()

# ================================

# Configuration
option(devel "Enable develop mode" OFF)
option(gprof "Enable gprof" OFF)
option(gperftools "Enable gperftools" OFF)
option(profile "Enable profiling" OFF)
option(jni "Use jni" OFF)
option(tcmalloc "Use tcmalloc" OFF)
option(opt_minmax "Enable minmax optimization" OFF)
option(cunit "Enable cunit" OFF)
option(minimal_state "minimize the state instead of being unable to use the MCNDFS" OFF)
option(firstclass_rule "Enable firstclass rules" OFF)

if(devel)
    set(DEVEL 1)

    if(CMAKE_C_COMPILER_ID MATCHES "GNU")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ggdb -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare -Winline")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare -Winline")
    endif()
endif()

if(gprof)
    find_program(GPROF gprof)

    if(GPROF AND CMAKE_C_COMPILER_ID MATCHES "GNU")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pg")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg")
    endif()
endif()

if(profile)
    set(PROFILE 1)
endif()

if(jni)
    set(USE_JNI 1)
endif()

if(tcmalloc)
    set(HAVE_TCMALLOC 1)

    if(CMAKE_C_COMPILER_ID MATCHES "GNU")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-builtin-malloc -fno-builtin-calloc -fno-builtin-realloc -fno-builtin-free")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-builtin-malloc -fno-builtin-calloc -fno-builtin-realloc -fno-builtin-free")
    endif()
endif()

if(gperftools)
    set(HAVE_GPERFTOOLS 1)
endif()

if(opt_minmax)
    set(KWBT_OPT 1)
endif()

if(cunit)
    set(USE_CUNIT 1)

    if(NOT DEFINED ENV{CUNIT_HOME})
        # use default
        set(CUNIT_HOME "/usr/local")
    endif()
endif()

if(minimal_state)
    set(MINIMAL_STATE 1)
endif()

if(firstclass_rule)
    set(USE_FIRSTCLASS_RULE 1)
endif()

# ================================

# Checks including
include(CheckTypeSize)
include(ProcessorCount)

# Check processor count
ProcessorCount(N)

if(NOT N EQUAL 0)
    set(PROCESSOR_COUNT ${N})
endif()

# Check type size
check_type_size("void*" SIZEOF_VOID_P)
check_type_size("long" SIZEOF_LONG)
check_type_size("double" SIZEOF_DOUBLE)
check_type_size("uintptr_t" SIZEOF_UINTPTR_T)

if(ZLIB_FOUND)
    set(HAVE_LIBZ 1)
endif()

if(UNIX AND NOT APPLE)
    set(HAVE_LIBRT 1)
    set(TIME_WITH_SYS_TIME 1)
endif()

# ================================

# Include directories
add_subdirectory(src)
add_subdirectory(third_party)

# ================================

# Install
install(TARGETS slim
        RUNTIME DESTINATION bin)
install(DIRECTORY  "${CMAKE_SOURCE_DIR}/lib"
        DESTINATION ${SLIM_DATA_ROOT_DIR}
        FILES_MATCHING PATTERN "*.lmn")