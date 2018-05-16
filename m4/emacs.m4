AC_DEFUN([CHECK_EMACS],[
  AC_CHECK_PROG([CHECK_EMACS_TMP], [emacs], [yes], [no])
  AM_CONDITIONAL([FOUND_EMACS], [test "x$CHECK_EMACS_TMP" = "xyes"])
  AM_COND_IF([FOUND_EMACS],, [AC_MSG_ERROR([required program 'emacs' not found.])])
])

AC_DEFUN([CHECK_EMACS_MODULE_SUPPORT],[
  AC_CACHE_CHECK([whether Emacs supports dynamic modules], [emacs_cv_modsupport], [
    AM_CONDITIONAL([EMACS_MODULE_SUPPORT_FOUND], [(emacs -batch --eval '(princ system-configuration-options)' | grep -q -- --with-modules)])
    AM_COND_IF([EMACS_MODULE_SUPPORT_FOUND], [
      emacs_cv_modsupport=yes
    ],[
      emacs_cv_modsupport=no
    ])
  ])
  EMACS_SUPPORTS_MODULES=$emacs_cv_modsupport
  AM_COND_IF([EMACS_MODULE_SUPPORT_FOUND],, [AC_MSG_ERROR([Emacs must be built with support for dynamic modules.])])
])
