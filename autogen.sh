#!/usr/bin/env bash
# Generates the ./configure file.
set -e
set -x
case `uname` in Darwin*) glibtoolize --copy ;;
  *) libtoolize --copy ;; esac

aclocal
autoheader
automake --add-missing
autoconf

