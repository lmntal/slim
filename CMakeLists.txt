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

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_C_COMPILER clang)
    set(CMAKE_CXX_COMPILER clang++)
else()
    set(CMAKE_C_COMPILER gcc)
    set(CMAKE_CXX_COMPILER g++)
endif()

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(SLIM_DATA_ROOT_DIR "${CMAKE_INSTALL_PREFIX}/share/slim" CACHE PATH "Root directory of data files")
set(SLIM_LIB_DIR "${SLIM_DATA_ROOT_DIR}/lib" CACHE PATH "Directory of library files")
set(SLIM_EXT_DIR "${SLIM_DATA_ROOT_DIR}/ext" CACHE PATH "Directory of external files")

# ================================

# Find necessary packages
find_package(OpenMP REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(Doxygen REQUIRED)
find_package(RE2C REQUIRED)
find_package(Threads REQUIRED) # Both pthread and win32 threads
find_package(Ruby REQUIRED)
find_package(ZLIB REQUIRED)

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
include(CheckIncludeFile)
include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)

# Check header files
check_include_file("omp.h" HAVE_OMP_H)
check_include_file("inttypes.h" HAVE_INTTYPES_H)
check_include_file("sched.h" HAVE_SCHED_H)
check_include_file("syscall.h" HAVE_SYSCALL_H)
check_include_file("unistd.h" HAVE_UNISTD_H)
check_include_file("execinfo.h" HAVE_EXECINFO_H)

# Check library files
if(CMAKE_USE_PTHREADS_INIT)
    set(HAVE_LIBPTHREAD 1)
    set(HAVE_PTHREAD 1)
endif()

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

# Check atomic add
check_c_source_compiles("
int main(void)
{
    int x = 0, y = 0;
    __sync_add_and_fetch(&x, y);
    return 0;
}
" HAVE_ATOMIC_ADD)

# Check atomic sub
check_c_source_compiles("
int main(void)
{
    int x = 0, y = 0;
    __sync_sub_and_fetch(&x, y);
    return 0;
}
" HAVE_ATOMIC_SUB)

# Check atomic compare and swap
check_c_source_compiles("
int main(void)
{
    int x = 0, y = 0, z = 0;
    __sync_bool_compare_and_swap(&x, y, z);
    return 0;
}
" HAVE_ATOMIC_CAS)

# Check atomic and fetch
check_c_source_compiles("
int main(void)
{
    int x = 0, y = 0;
    __sync_and_and_fetch(&x, y);
    return 0;
}
" HAVE_ATOMIC_LOGICAL_AND)

# Check atomic or fetch
check_c_source_compiles("
int main(void)
{
    int x = 0, y = 0;
    __sync_or_and_fetch(&x, y);
    return 0;
}
" HAVE_ATOMIC_LOGICAL_OR)

# Check builtin barrier
check_c_source_compiles("
int main(void)
{
    __sync_synchronize();
    return 0;
}
" HAVE_BUILTIN_MBARRIER)

# Check pthread barrier
check_c_source_compiles("
#include <pthread.h>
int main(void)
{
    pthread_barrier_t barrier;
    pthread_barrier_init(&barrier, NULL, 1);
    pthread_barrier_wait(&barrier);
    pthread_barrier_destroy(&barrier);
    return 0;
}
" HAVE_PTHREAD_BARRIER)

# Check thread_local keyword
check_cxx_source_compiles("
int main(void)
{
    thread_local int x = 0;
    return 0;
}
" HAVE_TLS)

set(HAVE___THREAD ${HAVE_TLS})

if(ZLIB_FOUND)
    set(HAVE_LIBZ 1)
endif()

# ================================

# Include directories
add_subdirectory(src)
add_subdirectory(third_party)