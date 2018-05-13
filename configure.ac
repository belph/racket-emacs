AC_PREREQ([2.69])
AC_INIT([racket-emacs], [0.1], [philip@pblair.org])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_SRCDIR([src/mymod.c])
AC_CONFIG_MACRO_DIRS([m4])
COLOR_INIT()

dnl Set up Compilation Flags
RACKET_DEFDIR([RACKET_LIBDIR], [dll])
AC_MSG_NOTICE([Racket libraries located at: ${RACKET_LIBDIR}])
RACKET_DEFDIR([RACKET_INCDIR], [include])
AC_MSG_NOTICE([Racket headers located at: ${RACKET_INCDIR}])

LDFLAGS="$LDFLAGS -Wl,--no-as-needed,--enable-new-dtags,-rpath,${RACKET_LIBDIR}"
LDFLAGS="$LDFLAGS -L${RACKET_LIBDIR}"
CFLAGS="$CFLAGS -I${RACKET_INCDIR} -L${RACKET_LIBDIR}"
CPPFLAGS="$CPPFLAGS -I${RACKET_INCDIR} -L${RACKET_LIBDIR}"

dnl AC_MSG_NOTICE([LDFLAGS: ${LDFLAGS}])
dnl AC_MSG_NOTICE([CFLAGS: ${CFLAGS}])

dnl Prepare C project
AC_LANG(C)
AC_PROG_CC

dnl libraries
SECTION_TITLE([yellow], [Running libtool])
LT_INIT
AC_ENABLE_SHARED
AC_DISABLE_STATIC
AC_PROG_LIBTOOL(libtool)

SECTION_TITLE([yellow], [Checking for system libraries])

AC_SEARCH_LIBS([cos], [m], [], [
  AC_MSG_ERROR([unable to locate libm])
])
AC_SEARCH_LIBS([pthread_create], [pthread], [], [
  AC_MSG_ERROR([unable to locate libpthread])
])
AC_SEARCH_LIBS([dlopen], [dl], [], [
  AC_MSG_ERROR([unable to locate libdl])
])
dnl These are the ones less likely to be available
AC_SEARCH_LIBS([ffi_raw_call], [ffi], [], [
  AC_MSG_ERROR([unable to locate libffi. Please ensure that it is installed.])
])

AC_SEARCH_LIBS([scheme_main_setup], [racket3m], [], [
  AC_MSG_ERROR([unable to locate libracket3m.])
])

SECTION_TITLE([yellow], [Checking for required headers])
AC_CHECK_HEADERS([scheme.h], [], [
  AC_MSG_ERROR([unable to loacte Racket headers])
])
AC_CHECK_HEADERS([emacs-module.h], [
  EMACS_HEADER_SOURCE=system
], [
  NEEDS_EMACS_HEADER_TMP=yes
  EMACS_HEADER_SOURCE=AC_PACKAGE_NAME
])
AM_CONDITIONAL([NEEDS_EMACS_HEADER], [test "x$NEEDS_EMACS_HEADER_TMP" = "xyes"])

RACKET_CDEFDIR([RACKET_COLLECTS_DIR], [collects])
RACKET_DEFDIR([RACKET_CONFIG_DIR], [config])
AC_DEFINE_UNQUOTED([RACKET_CONFIG_PATH], ["$RACKET_CONFIG_DIR/config.rktd"], [Path to Racket runtime configuration file])

SECTION_TITLE([yellow], [Generating files])
dnl Create Makefile
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT

AS_ECHO(["
Finished Configuration."])

SECTION_TITLE([yellow], [Configuration Summary])
AS_ECHO(["  CFLAGS:.............  ${CFLAGS}
  LDFLAGS:............  ${LDFLAGS}
  Emacs header source:   ${EMACS_HEADER_SOURCE}
"])
AC_MSG_NOTICE([Run 'make' to build the library.])
