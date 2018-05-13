dnl Macro definitions for working with Racket

AC_DEFUN([CHECK_RACKET],[
  AC_CHECK_PROG([CHECK_RACKET_TMP], [racket], [yes], [no])
  AM_CONDITIONAL([FOUND_RACKET], [test "x$CHECK_RACKET_TMP" = "xyes"])	
  AM_COND_IF([FOUND_RACKET],, [AC_MSG_ERROR([required program 'racket' not found.])])
])

AC_DEFUN([RACKET_DEFDIR],[
  CHECK_RACKET()
  $1=$(racket -e '(require setup/dirs) (displayln (path->string (find-$2-dir)))')
])

AC_DEFUN([RACKET_CDEFDIR],[
  CHECK_RACKET()
  AC_DEFINE_UNQUOTED([$1], ["$(racket -e '(require setup/dirs) (displayln (path->string (find-$2-dir)))')"], [Main Racket collection directory])
])
