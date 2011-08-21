#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# $Id: configure.ac,v 1.13 2008/10/05 11:22:41 riki Exp $

AC_PREREQ(2.61)
AC_INIT([SLIM], [2.1.7], [lmntal@ueda.info.waseda.ac.jp], [slim])
# AC_REVISION($Revision: 1.13 $)
# AC_COPYRIGHT (copyright-notice)
AC_CANONICAL_TARGET
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADER([src/config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE


# ----------------------------------------------------------------------
# Checks for programs.
#
AC_LANG(C)
AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_CC_C89
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_YACC
AC_PROG_LEX
AM_PROG_LEX

AC_SUBST(CFLAGS)
AM_CONDITIONAL(GCC, test "$GCC" = "yes")   # let the Makefile know if we're gcc

case "$target_os" in
  darwin*)
    CFLAGS="-Wall $CFLAGS"
    ;;
  *)
    CFLAGS="-Wall -export-dynamic $CFLAGS"
    ;;
esac

# ----------------------------------------------------------
# Setting Options
#

default_enable_devel=no
default_enable_debug=no
default_enable_gprof=no
default_enable_profile=no
default_enable_timeopt=yes
default_enable_jni=no
default_enable_tcmalloc=yes
case "$host" in
   *-mingw*) default_enable_tcmalloc=no;;
   *-cygwin*) default_enable_tcmalloc=no;;
esac

# developer mode : 開発時にはこのオプションを付けるようにする
AC_ARG_ENABLE([devel],
              [AS_HELP_STRING([--enable-devel],
                              [turn on settings for devel (default no)])],
              [],
              [enable_devel="$default_enable_devel"])

# debug mode
AC_ARG_ENABLE([debug],
              [AS_HELP_STRING([--enable-debug],
                              [turn on debugging (default no)])],
              [],
              [enable_debug="$default_enable_debug"])

AC_ARG_ENABLE([gprof],
              [AS_HELP_STRING([--enable-gprof],
                              [make it possible to use GNU gprof (default no)])],
              [],
              [enable_gprof="$default_enable_gprof"])

AC_ARG_ENABLE([profile],
              [AS_HELP_STRING([--enable-profile],
                              [turn on runtime profiler (default no)])],
              [],
              [enable_profile="$default_enable_profile"])

AC_ARG_ENABLE([time_opt],
              [AS_HELP_STRING([--enable-time-opt],
                              [turn on optimization for runtime performance (default yes)])],
              [],
              [enable_time_opt="$default_enable_timeopt"])

AC_ARG_ENABLE([jni],
              [AS_HELP_STRING([--enable-jni],
                              [turn on using jni interactive mode (default no)])],
              [],
              [enable_jni="$default_enable_jni"])

AC_ARG_ENABLE([tcmalloc],
              [AS_HELP_STRING([--enable-tcmalloc],
                              [turn on using google perftools tcmalloc (linux only)])],
              [],
              [enable_tcmalloc="$default_enable_tcmalloc"])


if test "$GCC" = "yes" && test "$enable_devel" = "yes"; then
  CFLAGS=$CFLAGS "-ggdb -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare -Winline"
# 現状コンパイルできないのでコメントアウト
#  CFLAGS="-std=c89 -pedantic $CFLAGS"
fi

# devel
if test "$enable_devel" = "yes"; then
  AC_MSG_RESULT([enable devel: yes])
  AC_DEFINE([DEVEL], 1,
            [enable devel])
else
  AC_MSG_RESULT([enable devel: no])
fi

# debug
if test "$enable_debug" = "yes"; then
  AC_MSG_RESULT([enable debug: yes])
  AC_DEFINE([DEBUG], 1,
            [enable debug])
else
  AC_MSG_RESULT([enable debug: no])
fi

# gprof
if test "$enable_gprof" = "yes" && test -x /usr/bin/gprof && test "$GCC" = "yes"; then
  AC_MSG_RESULT([enable GNU gprof: yes])
  CFLAGS="-pg -g $CFLAGS"
else
  AC_MSG_RESULT([enable GNU gprof: no])
fi

# profile
if test "$enable_profile" = "yes"; then
  AC_MSG_RESULT([enable profile: yes])
  AC_DEFINE([PROFILE], 1,
            [enable profile])
else
  AC_MSG_RESULT([enable profile: no])
fi

# optimization for runtime performance
if test "$enable_time_opt" = "no"; then
  AC_MSG_RESULT([enable time-opt: no])
else
  AC_MSG_RESULT([enable time-opt: yes])
	AC_DEFINE([TIME_OPT], 1,
	          [enable timeopt])
fi

# jni
if test "$enable_jni" = "yes"; then
  AC_MSG_RESULT([enable jni: yes])
  AC_DEFINE([USE_JNI], 1,
            [enable jni])
else
  AC_MSG_RESULT([enable jni: no])
fi

# tcmalloc
if test "$enable_tcmalloc" = "no"; then
  AC_MSG_RESULT([enable tcmalloc: no])
else
  AC_MSG_RESULT([enable tcmalloc: yes])
	AC_DEFINE([HAVE_TCMALLOC], 1,
	          [enable tcmalloc])
	THIRDPARTY_DIR="third_party"
	AC_SUBST(THIRDPARTY_DIR)
fi

AM_CONDITIONAL(ENABLE_JNI,      test "$enable_jni" = "yes")
AM_CONDITIONAL(ENABLE_TCMALLOC, test "$enable_tcmalloc" = "yes")
AM_WITH_DMALLOC

# ----------------------------------------------------------------------
# Checks for typedefs, structures, and compiler characteristics.
#

AC_PROG_CC
AC_C_INLINE
AC_C_CONST
AC_C_VOLATILE
AC_C_RESTRICT
AC_C_CHAR_UNSIGNED
#AC_C_VARARRAYS
#AC_C_TYPEOF
# AC_C_STRINGIZE

AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INTMAX_T
AC_TYPE_INTPTR_T
AC_TYPE_LONG_DOUBLE
# AC_TYPE_LONG_DOUBLE_WIDER
# AC_TYPE_LONG_LONG_INT
# AC_TYPE_MBSTATE_T
# AC_TYPE_SIZE_T
# AC_TYPE_SSIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINTMAX_T
# AC_TYPE_UINTPTR_T
# AC_TYPE_UNSIGNED_LONG_LONG_INT

AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(void*)


# ----------------------------------------------------------------------
# for SLIM
#

SLIM_VERSION=$PACKAGE_VERSION
AC_SUBST(SLIM_VERSION)
AC_DEFINE_UNQUOTED(SLIM_VERSION, "$SLIM_VERSION", [slim version])
ROOT_DIR=$rootdir
AC_SUBST(ROOT_DIR)
AC_DEFINE_UNQUOTED(ROOT_DIR, "$libdir", [root directory])
AC_SUBST(LIB_DIR)
AC_DEFINE_UNQUOTED(LIB_DIR, "${pkgdatadir}/liba", [library directory])
# AC_PROG_INSTALL

# Checks for libraries.
AC_CHECK_LIB(pthread, main)
AC_CHECK_LIB(z, main)
AC_CHECK_LIB(rt, main)

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
# AC_CHECK_HEADERS(sys/time.h sys/types.h glob.h dlfcn.h getopt.h sched.h unistd.h)
# AC_CHECK_HEADERS(stdint.h inttypes.h rpc/types.h)
# AC_CHECK_HEADERS(syslog.h crypt.h)
# AC_CHECK_HEADERS(pty.h util.h libutil.h sys/loadavg.h)
AC_CHECK_HEADERS(sched.h syscall.h)

# Checks for library functions.
AC_CHECK_FUNC([strchr])

# Checks for type __int64 (for jni)
AC_CHECK_TYPES([__int64])


# -----------------------------------------------------------------
# Builtin Keyword for Thread Local Storage
#

AC_MSG_CHECKING(for builtin __thread)
AC_CACHE_VAL(
  ac_cv_c___thread,
  AC_TRY_RUN(
    [
      __thread int x;
      int main(void)
      {
        return 0;
      }
    ],
    ac_cv_c___thread=yes,
    ac_cv_c___thread=no
  )
)
AC_MSG_RESULT($ac_cv_c___thread)
if test "$ac_cv_c___thread" = "yes"; then
  AC_DEFINE(HAVE___THREAD, [1], [Define to 1 if the system has the TLS keyword `__thread'.])
fi

# -----------------------------------------------------------------
# Builtin Functions for Atomic Operation
#

AC_MSG_CHECKING(for builtin compare_and_swap)
AC_CACHE_VAL(
  ac_cv_func_cas,
  AC_TRY_RUN(
    [
      int main(void)
      {
        int x = 0, y = 0, z = 0;
        __sync_bool_compare_and_swap_4(&x, y, z);
        return 0;
      }
    ],
    ac_cv_func_cas=yes,
    ac_cv_func_cas=no
  )
)
AC_MSG_RESULT($ac_cv_func_cas)
if test "$ac_cv_func_cas" = "yes"; then
  AC_DEFINE(HAVE_ATOMIC_CAS, [1], [Define to 1 if the system has the function `compare_and_swap'.])
fi

AC_MSG_CHECKING(for builtin atomic_add)
AC_CACHE_VAL(
  ac_cv_func_add,
  AC_TRY_RUN(
    [
      int main(void)
      {
        int x = 0, y = 0;
        __sync_add_and_fetch(&x, y);
        return 0;
      }
    ],
    ac_cv_func_add=yes, 
    ac_cv_func_add=no
  )
)
AC_MSG_RESULT($ac_cv_func_add)
if test "$ac_cv_func_add" = "yes"; then
  AC_DEFINE(HAVE_ATOMIC_ADD, [1], [Define to 1 if the system has the function `add_and_fetch'.])
fi

AC_MSG_CHECKING(for builtin atomic_sub)
AC_CACHE_VAL(
  ac_cv_func_sub,
  AC_TRY_RUN(
    [
      int main(void)
      {
        int x = 0, y = 0;
        __sync_sub_and_fetch(&x, y);
        return 0;
      }
    ],
    ac_cv_func_sub=yes,
    ac_cv_func_sub=no
  )
)
AC_MSG_RESULT($ac_cv_func_sub)
if test "$ac_cv_func_sub" = "yes"; then
  AC_DEFINE(HAVE_ATOMIC_SUB, [1], [Define to 1 if the system has the function `sub_and_fetch'.])
fi

AC_MSG_CHECKING(for builtin atomic_logical_and)
AC_CACHE_VAL(
  ac_cv_func_logical_and,
  AC_TRY_RUN(
    [
       int main(void)
       {
          int x = 2, y = 1;
          __sync_and_and_fetch(&x, y);
          return 0;
       }
    ],
    ac_cv_func_logical_and=yes,
    ac_cv_func_logical_and=no
  )
)
AC_MSG_RESULT($ac_cv_func_logical_and)
if test "$ac_cv_func_logical_and" = "yes"; then
  AC_DEFINE(HAVE_ATOMIC_LOGICAL_AND, [1], [Define to 1 if the system has the function `and_and_fetch'.])
fi

AC_MSG_CHECKING(for builtin atomic_logical_or)
AC_CACHE_VAL(
  ac_cv_func_logical_or,
  AC_TRY_RUN(
    [
      int main(void)
      {
        int x = 1, y = 0;
        __sync_or_and_fetch(&x, y);
        return 0;
      }
    ],
    ac_cv_func_logical_or=yes,
    ac_cv_func_logical_or=no
  )
)
AC_MSG_RESULT($ac_cv_func_logical_or)
if test "$ac_cv_func_logical_or" = "yes"; then
  AC_DEFINE(HAVE_ATOMIC_LOGICAL_OR, [1], [Define to 1 if the system has the function `or_and_fetch'.])
fi

AC_MSG_CHECKING(for pthread_barrier)
AC_CACHE_VAL(
  ac_cv_func_pthread_barrier,
  AC_TRY_RUN(
    [
      #include <pthread.h>
      int main(void)
      {
        pthread_barrier_t t;
        return 0;
      }
    ],
    ac_cv_func_pthread_barrier=yes,
    ac_cv_func_pthread_barrier=no
  )
)
AC_MSG_RESULT($ac_cv_func_pthread_barrier)
if test "$ac_cv_func_pthread_barrier" = "yes"; then
  AC_DEFINE(HAVE_PTHREAD_BARRIER, [1], [Define to 1 if the system has `pthread_barrier'.])
fi


# ---------------------------------------------------------------------
# Get Enable Number of Processor Core
#

AC_MSG_CHECKING(number of processor cores)
core_num=0
case "$host" in
  *-darwin*) 
    core_num=`hwprefs cpu_count`;;
  *)
    core_num=`cat /proc/cpuinfo | grep -c processor`;;
esac
if test ${core_num} -eq 0; then
  core_num=1
fi
AC_DEFINE_UNQUOTED(HAVE_PROCESSOR_CORES, $core_num,
                   [Define to Enable Processor Cores on your environment.])
AC_MSG_RESULT(${core_num})

# ---------------------------------------------------------------------
# for tcmalloc
#

# Export the version information (for tc_version and friends)
TC_VERSION_MAJOR=`expr "$PACKAGE_VERSION" : '\([[0-9]]*\)'`
TC_VERSION_MINOR=`expr "$PACKAGE_VERSION" : '[[^.]]*\.\([[0-9]]*\)'`
TC_VERSION_PATCH=`expr "$PACKAGE_VERSION" : '[[^.0-9]]*\([[^0-9]]*\)$'`
AC_SUBST(TC_VERSION_MAJOR)
AC_SUBST(TC_VERSION_MINOR)
AC_SUBST(TC_VERSION_PATCH)
AC_SUBST(PACKAGE_STRING)

# The user can choose not to compile in the heap-profiler, the
# heap-checker, or the cpu-profiler.  There's also the possibility
# for a 'fully minimal' compile, which leaves out the stacktrace
# code as well.  By default, we include all of these that the
# target system supports.
#default_enable_cpu_profiler=yes
#default_enable_heap_profiler=yes
#default_enable_heap_checker=yes
#default_enable_debugalloc=yes
#default_enable_minimal=no
need_nanosleep=yes   # Used later, to decide if to run ACX_NANOSLEEP
case "$host" in
   *-mingw*) default_enable_minimal=yes; default_enable_debugalloc=no;
             need_nanosleep=no;;
   *-cygwin*) default_enable_heap_checker=no; default_enable_cpu_profiler=no;;
   *-freebsd*) default_enable_heap_checker=no;;
   *-darwin*) default_enable_heap_checker=no;;
esac

#AC_ARG_ENABLE([cpu-profiler],
#              [AS_HELP_STRING([--disable-cpu-profiler],
#                              [do not build the cpu profiler])],
#              [],
#              [enable_cpu_profiler="$default_enable_cpu_profiler"])
#AC_ARG_ENABLE([heap-profiler],
#              [AS_HELP_STRING([--disable-heap-profiler],
#                              [do not build the heap profiler])],
#              [],
#              [enable_heap_profiler="$default_enable_heap_profiler"])
#AC_ARG_ENABLE([heap-checker],
#              [AS_HELP_STRING([--disable-heap-checker],
#                              [do not build the heap checker])],
#              [],
#              [enable_heap_checker="$default_enable_heap_checker"])
#AC_ARG_ENABLE([debugalloc],
#              [AS_HELP_STRING([--disable-debugalloc],
#                              [do not build versions of libs with debugalloc])],
#              [],
#              [enable_debugalloc="$default_enable_debugalloc"])
#AC_ARG_ENABLE([minimal],
#              [AS_HELP_STRING([--enable-minimal],
#                              [build only tcmalloc-minimal (and maybe tcmalloc-minimal-debug)])],
#              [],
#              [enable_minimal="$default_enable_minimal"])
#if test "$enable_minimal" = yes; then
  enable_cpu_profiler=no
  enable_heap_profiler=no
  enable_heap_checker=no
#fi
enable_debugalloc=no


# Checks for programs.
AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX
AM_CONDITIONAL(GCC, test "$GCC" = yes)   # let the Makefile know if we're gcc
AM_PROG_CC_C_O      # shrug: autogen.sh suddenly needs this for some reason

# Check if we have an objcopy installed that supports -W
AC_CHECK_TOOL([OBJCOPY], [objcopy], [])
AM_CONDITIONAL(HAVE_OBJCOPY_WEAKEN, ["$OBJCOPY" -W malloc /bin/ls /dev/null])

case $host_os in
  *mingw*)
    # Disabling fast install keeps libtool from creating wrapper scripts
    # around the executables it builds.  Such scripts have caused failures on
    # MinGW.  Using this option means an extra link step is executed during
    # "make install".
    AC_DISABLE_FAST_INSTALL
    ;;
   *)
    AC_ENABLE_FAST_INSTALL
    ;;
esac

AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)
AM_CONDITIONAL(USE_LIBTOOL, test "x$LIBTOOL" != "x")

AC_C_INLINE
AX_C___ATTRIBUTE__

# Check whether some low-level functions/files are available
AC_HEADER_STDC

# TODO(csilvers): we could remove a lot when WITH_CPU_PROFILER etc is "no".
AC_CHECK_TYPES([__int64])       # defined in some windows platforms
AC_CHECK_TYPES([struct mallinfo],,, [#include <malloc.h>])
AC_CHECK_TYPES([Elf32_Versym],,, [#include <elf.h>])   # for vdso_support.h
AC_CHECK_FUNCS(sbrk)            # for tcmalloc to get memory
AC_CHECK_FUNCS(geteuid)         # for turning off services when run as root
AC_CHECK_HEADERS(features.h)    # for vdso_support.h
AC_CHECK_HEADERS(malloc.h)      # some systems define stuff there, others not
AC_CHECK_HEADERS(glob.h)        # for heap-profile-table (cleaning up profiles)
AC_CHECK_HEADERS(execinfo.h)    # for stacktrace? and heapchecker_unittest
AC_CHECK_HEADERS(libunwind.h)   # for stacktrace
AC_CHECK_HEADERS(unwind.h)      # for stacktrace
AC_CHECK_HEADERS(sched.h)       # for being nice in our spinlock code
AC_CHECK_HEADERS(conflict-signal.h)      # defined on some windows platforms?
AC_CHECK_HEADERS(sys/prctl.h)   # for thread_lister (needed by leak-checker)
AC_CHECK_HEADERS(linux/ptrace.h)# also needed by leak-checker
AC_CHECK_HEADERS(sys/syscall.h)
AC_CHECK_HEADERS(sys/socket.h)  # optional; for forking out to symbolizer
AC_CHECK_HEADERS(sys/wait.h)    # optional; for forking out to symbolizer
AC_CHECK_HEADERS(poll.h)        # optional; for forking out to symbolizer
AC_CHECK_HEADERS(fcntl.h)       # for tcmalloc_unittest
AC_CHECK_HEADERS(grp.h)         # for heapchecker_unittest
AC_CHECK_HEADERS(pwd.h)         # for heapchecker_unittest
AC_CHECK_HEADERS(sys/resource.h)         # for memalign_unittest.cc
AC_CHECK_HEADERS(valgrind.h)    # we have a local copy if this isn't found
# We also need <ucontext.h>/<sys/ucontext.h>, but we get those from
# AC_PC_FROM_UCONTEXT, below.

# We override a lot of memory allocation routines, not all of which are
# standard.  For those the system doesn't declare, we'll declare ourselves.
AC_CHECK_DECLS([cfree,
                posix_memalign,
                memalign,
                valloc,
                pvalloc],,,
               [#define _XOPEN_SOURCE 600
                #include <stdlib.h>
                #include <malloc.h>])

if test "$ac_cv_type_struct_mallinfo" = yes; then
  AC_SUBST(ac_cv_have_struct_mallinfo, 1)   # google/tcmalloc.h needs this
else
  AC_SUBST(ac_cv_have_struct_mallinfo, 0)
fi

# We need to check for mmap.  cygwin supports mmap, but the autoconf
# test doesn't work on cygwin:
#    http://www.cygwin.com/ml/cygwin/2002-04/msg00412.html
# This workaround comes from
#    http://cygwin.com/ml/cygwin/2004-11/msg00138.html
case "$host" in
  *-*-cygwin*) ac_cv_func_mmap_fixed_mapped=yes
               AC_DEFINE(HAVE_MMAP, 1,
                         [Define to 1 if you have a working `mmap' system call.])
               ;;
            *) AC_FUNC_MMAP
               ;;
esac

# If AtomicWord != Atomic32, we need to define two versions of all the
# atomicops functions.  If they're the same, we want to define only one.
AC_MSG_CHECKING([if int32_t is the same type as intptr_t])
AC_TRY_COMPILE([#include <stdint.h>],
	       [int32_t v1 = 0; intptr_t v2 = 0; return (&v1 - &v2)],
               [AC_DEFINE(INT32_EQUALS_INTPTR, 1,
                          Define to 1 if int32_t is equivalent to intptr_t)
                AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])])

# We want to access the "PC" (Program Counter) register from a struct
# ucontext.  Every system has its own way of doing that.  We try all the
# possibilities we know about.  Note REG_PC should come first (REG_RIP
# is also defined on solaris, but does the wrong thing).  But don't
# bother if we're not doing cpu-profiling.
# [*] means that we've not actually tested one of these systems
if test "$enable_cpu_profiler" = yes; then
  AC_PC_FROM_UCONTEXT(AC_MSG_WARN(Could not find the PC.  Will not try to compile libprofiler...);
                      enable_cpu_profiler=no)
fi

# Some tests test the behavior of .so files, and only make sense for dynamic.
AM_CONDITIONAL(ENABLE_STATIC, test "$enable_static" = yes)

# We want to link in libunwind if it exists
AC_CHECK_LIB(unwind, backtrace, UNWIND_LIBS=-lunwind, UNWIND_LIBS=)
AC_SUBST(UNWIND_LIBS)

# On x86_64, instead of libunwind, we can choose to compile with frame-pointers
# (This isn't needed on i386, where -fno-omit-frame-pointer is the default).
AC_ARG_ENABLE(frame_pointers,
              AS_HELP_STRING([--enable-frame-pointers],
                             [On x86_64 systems, compile with -fno-omit-frame-pointer (see INSTALL)]),
	      , enable_frame_pointers=no)
AM_CONDITIONAL(ENABLE_FRAME_POINTERS, test "$enable_frame_pointers" = yes)

# Some x86_64 systems do not insert frame pointers by default (all
# i386 systems that I know of, do.  I don't know about non-x86 chips).
# We want to see if the current system is one of those.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM(, [return __x86_64__ == 1 ? 0 : 1])],
                  [is_x86_64=yes], [is_x86_64=no])
OLD_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -S -O2 -o fp.s"
# This test will always fail because we don't name our output file properly.
# We do our own determination of success/failure in the grep, below.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([int f(int x) {return x;}], [return f(0);])],
                  [:], [:])
AM_CONDITIONAL(X86_64_AND_NO_FP_BY_DEFAULT,
               test "$is_x86_64" = yes && ! grep 'mov.*rsp.*rbp' fp.s >/dev/null 2>&1)
rm fp.s
CFLAGS="$OLD_CFLAGS"


# Defines PRIuS
AC_COMPILER_CHARACTERISTICS

# Also make sure we get standard PRI... definitions, even with glibc.
# We have to use AH_VERBATIM because we need the #ifdef guard (gcc buglet)
AH_VERBATIM([__STDC_FORMAT_MACROS],
            [/* C99 says: define this to get the PRI... macros from stdint.h */
#ifndef __STDC_FORMAT_MACROS
# define __STDC_FORMAT_MACROS 1
#endif])

# Check if __builtin_stack_pointer() is available (for elfcore.h)
AC_MSG_CHECKING([for __builtin_stack_pointer()])
AC_LINK_IFELSE([AC_LANG_PROGRAM(, [void *sp = __builtin_stack_pointer()])],
               [AC_DEFINE(HAVE_BUILTIN_STACK_POINTER, 1,
                      Define to 1 if compiler supports __builtin_stack_pointer)
                AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])])

# Check if __environ is available (for GetenvBeforeMain)
AC_MSG_CHECKING([for __environ])
AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <unistd.h>],
                                [char **env = __environ])],
               [AC_DEFINE(HAVE___ENVIRON, 1,
                          [Define to 1 if compiler supports __environ])
                AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])])

# If we support __thread, that can speed up tcmalloc a bit.
# Note, however, that our code tickles a bug in gcc < 4.1.2
# involving TLS and -fPIC (which our libraries will use) on x86:
#   http://gcc.gnu.org/ml/gcc-bugs/2006-09/msg02275.html
AC_MSG_CHECKING([for __thread])
AC_LINK_IFELSE([AC_LANG_PROGRAM([#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__)) && ((__GNUC__ < 4) || (__GNUC__ == 4 && __GNUC_MINOR__ < 1) || (__GNUC__ == 4 && __GNUC_MINOR__ == 1 && __GNUC_PATCHLEVEL__ < 2))
#error gcc has this bug: http://gcc.gnu.org/ml/gcc-bugs/2006-09/msg02275.html
#endif], [static __thread int p = 0])],
               [AC_DEFINE(HAVE_TLS, 1,
                      Define to 1 if compiler supports __thread)
                AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([no])])

# Nanosleep requires extra libraries on some architectures (solaris).
# This sets NANOSLEEP_LIBS.  nanosleep doesn't exist on mingw, which
# is fine for us because we don't compile libspinlock, which uses it.
if test "$need_nanosleep" = yes; then
  ACX_NANOSLEEP
  AC_SUBST(NANOSLEEP_LIBS)
fi

# Solaris 10 6/06 has a bug where /usr/sfw/lib/libstdc++.la is empty.
# If so, we replace it with our own version.
LIBSTDCXX_LA_LINKER_FLAG=
if test -f /usr/sfw/lib/libstdc++.la && ! test -s /usr/sfw/lib/libstdc++.la
then
  LIBSTDCXX_LA_LINKER_FLAG='-L$(top_srcdir)/src/solaris'
fi
AC_SUBST(LIBSTDCXX_LA_LINKER_FLAG)

# We also need to check if the kernel supports __thread, which requires uname()
AC_CHECK_DECLS(uname,,, [#include <sys/utsname.h>])

# In fact, a lot of the code in this directory depends on pthreads
ACX_PTHREAD

# Find out what namespace 'normal' STL code lives in
AC_CXX_STL_NAMESPACE

# Figure out where libc has program_invocation_name
AC_PROGRAM_INVOCATION_NAME

# Make the install prefix available, to figure out where to look for pprof
AC_INSTALL_PREFIX

# For windows, this has a non-trivial value (__declspec(export)), but any
# system that uses configure wants this to be the empty string.
AC_DEFINE(PERFTOOLS_DLL_DECL,,
          [Always the empty-string on non-windows systems.
           On windows, should be "__declspec(dllexport)".
	   This way, when we compile the dll, we export our functions/classes.
	   It's safe to define this here because config.h is only used
	   internally, to compile the DLL, and every DLL source file
	   #includes "config.h" before anything else.])

# In theory, config.h files shouldn't need a header guard, but we do,
# because we (maybe) #include windows/mingw.h from within config.h,
# and it #includes other .h files.  These all have header guards, so
# the end result is if config.h is #included twice, its #undefs get
# evaluated twice, but all the ones in mingw.h/etc only get evaluated
# once, potentially causing trouble.  c.f.
#   http://code.google.com/p/google-perftools/issues/detail?id=246
AH_TOP([
#ifndef GOOGLE_PERFTOOLS_CONFIG_H_
#define GOOGLE_PERFTOOLS_CONFIG_H_
])

# MinGW uses autoconf, but also needs the windows shim routines
# (since it doesn't have its own support for, say, pthreads).
# This requires us to #include a special header file, and also to
# link in some windows versions of .o's instead of the unix versions.
AH_BOTTOM([
#ifdef __MINGW32__
#include "windows/mingw.h"
#endif

#endif  /* #ifndef GOOGLE_PERFTOOLS_CONFIG_H_ */
])
AM_CONDITIONAL(MINGW, expr $host : '.*-mingw' >/dev/null 2>&1)

# Redhat 7 (and below?) has sys/ucontext.h, but if you try to #include
# it directly, the compiler gets upset.  So we pretend we don't have
# it.
if cat /etc/redhat-release 2>/dev/null | grep "Red Hat Linux release 7" >/dev/null 2>&1; then
AC_DEFINE(HAVE_SYS_UCONTEXT_H, 0, [<sys/ucontext.h> is broken on redhat 7])
fi

# Export the --enable flags we set above.  We do this at the end so
# other configure rules can enable or disable targets based on what
# they find.
AM_CONDITIONAL(WITH_CPU_PROFILER, test "$enable_cpu_profiler" = yes)
AM_CONDITIONAL(WITH_HEAP_PROFILER, test "$enable_heap_profiler" = yes)
AM_CONDITIONAL(WITH_HEAP_CHECKER, test "$enable_heap_checker" = yes)
AM_CONDITIONAL(WITH_DEBUGALLOC, test "$enable_debugalloc" = yes)
# We make tcmalloc.so if either heap-profiler or heap-checker is asked for.
AM_CONDITIONAL(WITH_HEAP_PROFILER_OR_CHECKER,
               test "$enable_heap_profiler" = yes -o \
                    "$enable_heap_checker" = yes)
# If we don't use any profilers, we don't need stack traces (or pprof)
AM_CONDITIONAL(WITH_STACK_TRACE, test "$enable_cpu_profiler" = yes -o \
                                      "$enable_heap_profiler" = yes -o \
                                      "$enable_heap_checker" = yes)


# ----------------------------------------------------------------------
# Create output files
#
echo $SLIM_VERSION > VERSION
AC_OUTPUT(Makefile \
          src/Makefile \
          src/genconfig \
          src/verifier/Makefile \
	  src/utility/Makefile \
          lib/Makefile \
          ext/Makefile \
          test/Makefile \
          test/system_check/Makefile \
          third_party/Makefile \
          third_party/google-perftools-1.6/Makefile \
          third_party/google-perftools-1.6/src/google/tcmalloc.h \
          doc/Makefile \
          doc/slim.1)
