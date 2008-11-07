#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# $Id: configure.ac,v 1.13 2008/10/05 11:22:41 riki Exp $

AC_PREREQ(2.61)
AC_INIT([SLIM], [0.4.0], [lmntal@ueda.info.waseda.ac.jp], [slim])
# AC_REVISION($Revision: 1.13 $)
# AC_COPYRIGHT (copyright-notice)
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADER([src/config.h])
AM_INIT_AUTOMAKE

# ----------------------------------------------------------------------
# Checks for programs.
# 

AC_LANG(C)
AC_PROG_CC_STDC
AC_PROG_CC_C89
AC_PROG_CXX
AC_PROG_YACC
AC_PROG_LEX
AM_PROG_LEX

AC_SUBST(CFLAGS)
CFLAGS="-Wall $CFLAGS"


# 開発時にはこのオプションを付けるようにする
AC_ARG_ENABLE(devel,
[  --enable-devel            turn on settings for devel [[default no]]])

if test "$enable_devel" = "yes"; then
  AC_MSG_RESULT(enable devel: yes)
else
  AC_MSG_RESULT(enable devel: no)
fi

if test "$GCC" = "yes" && test "$enable_devel" = "yes"; then
  CFLAGS="-ggdb -Wall -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare -Winline"
# 現状コンパイルで着ないのでコメントアウト
#  CFLAGS="-std=c89 -pedantic $CFLAGS"
fi

# デバッグフラグON
AC_ARG_ENABLE(debug,
[  --enable-debug            turn on debugging [[default no]]])

if test "$enable_debug" = "yes"; then
  AC_MSG_RESULT(enable debug: yes)
  AC_DEFINE([DEBUG],,[elable debug])
else
  AC_MSG_RESULT(enable debug: no)
fi

# gprof
AC_ARG_ENABLE(gprof,
[  --enable-gprof            make it possible to use GNU gprof [[default no]]])
if test "$enable_gprof" = "yes" && test -x /usr/bin/gprof && test "$GCC" = "yes"; then
  AC_MSG_RESULT(enable GNU gprof: yes)
  CFLAGS="-pg -g $CFLAGS"
else
  AC_MSG_RESULT(enable GNU gprof: no)
fi

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
dnl AC_TYPE_LONG_LONG_INT
# AC_TYPE_MBSTATE_T
dnl AC_TYPE_SIZE_T
# AC_TYPE_SSIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINTMAX_T
# AC_TYPE_UINTPTR_T
dnl AC_TYPE_UNSIGNED_LONG_LONG_INT

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

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
dnl AC_CHECK_HEADERS(sys/time.h sys/types.h glob.h dlfcn.h getopt.h sched.h unistd.h)
dnl AC_CHECK_HEADERS(stdint.h inttypes.h rpc/types.h)
dnl AC_CHECK_HEADERS(syslog.h crypt.h)
dnl AC_CHECK_HEADERS(pty.h util.h libutil.h sys/loadavg.h)

# Checks for library functions.
AC_CHECK_FUNC([strchr])

# ----------------------------------------------------------------------
# Create output files
#
echo $SLIM_VERSION > VERSION
AC_OUTPUT(Makefile src/Makefile src/genconfig lib/Makefile test/Makefile test/system_check/Makefile doc/Makefile doc/slim.1)


