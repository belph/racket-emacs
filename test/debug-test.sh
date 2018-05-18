#!/usr/bin/env bash

SCRIPT_DIR=$(realpath "$(dirname $0)")
ROOT_DIR=$(realpath "${SCRIPT_DIR}/..")

exec gdb -q -ex 'handle SIGSEGV nostop noprint' -ex 'b racket-rtutils.c:91' -ex 'r' --args \
     emacs -Q -batch \
     -L ":${SCRIPT_DIR}" \
     -L ":${SCRIPT_DIR}/src" \
     -L ":${ROOT_DIR}/src/.libs" \
     -L ":${ROOT_DIR}/lisp" \
     --module-assertions \
     -l ert -l src/basic-test \
     --eval '(ert-run-tests-batch-and-exit (quote (not (or (tag :expensive-test) (tag :unstable)))))'
