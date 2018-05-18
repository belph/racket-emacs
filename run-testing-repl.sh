#!/usr/bin/env bash

SCRIPT_DIR=$(realpath "$(dirname $0)")
ROOT_DIR="${SCRIPT_DIR}"

PROG="(progn (require (quote racket-emacs)) $@ (while t (print (eval (read)))))"

if [ ! -z "${USE_GDB}" ]; then
    gdb -q -ex 'handle SIGSEGV nostop noprint' -ex 'r' --args \
        emacs -Q -batch \
        -L ":${ROOT_DIR}/src/.libs" \
        -L ":${ROOT_DIR}/lisp" \
        --module-assertions \
        --eval "${PROG}"
else
    exec emacs -Q -batch \
         -L ":${ROOT_DIR}/src/.libs" \
         -L ":${ROOT_DIR}/lisp" \
         --module-assertions \
         --eval "${PROG}"
fi
