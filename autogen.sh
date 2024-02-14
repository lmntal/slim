#!/bin/sh

run ()
{
  echo "running: $*"
  eval $*

  if test $? != 0 ; then
    echo "error: while running '$*'"
    exit 1
  fi
}

# makeshift
autoheader=autoheader
autoreconf=autoreconf
autoconf --version | grep 2.69 >/dev/null
if test $? != 0 ; then
  echo "autoconf 2.69 is not installed."
  echo "Trying to use autoconf2.69 instead."
  autoheader=autoheader2.69
  autoreconf=autoreconf2.69
fi

run aclocal
run $autoheader
run automake --add-missing
run $autoreconf
