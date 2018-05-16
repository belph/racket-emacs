dnl Macro definitions for working with Racket

AC_DEFUN([CHECK_RACKET],[
  AC_CHECK_PROG([CHECK_RACKET_TMP], [racket], [yes], [no])
  AM_CONDITIONAL([FOUND_RACKET], [test "x$CHECK_RACKET_TMP" = "xyes"])	
  AM_COND_IF([FOUND_RACKET],, [AC_MSG_ERROR([required program 'racket' not found.])])
])

AC_DEFUN([RACKET_DEF],[
  $1=$(racket -e $2)
])

AC_DEFUN([RACKET_DEFDIR],[
  m4_define([rdefdir_cache_var], [racket_cv_defdir_$1])
  AC_CACHE_CHECK([for Racket '$2' directory],
    rdefdir_cache_var,
    [rdefdir_cache_var=$(racket -e '(require setup/dirs) (displayln (path->string (find-$2-dir)))')]
  )
dnl  $1=$(racket -e '(require setup/dirs) (displayln (path->string (find-$2-dir)))')
  $1=$m4_expand([rdefdir_cache_var])
])

AC_DEFUN([RACKET_CDEFDIR],[
  AC_DEFINE_UNQUOTED([$1], ["$(racket -e '(require setup/dirs) (displayln (path->string (find-$2-dir)))')"], [Main Racket collection directory])
])
