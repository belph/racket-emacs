AC_PREREQ([2.69])
AC_INIT([racket-emacs], [0.1], [philip@pblair.org])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS([include/re-config.h])
AC_CONFIG_SRCDIR([src/mymod.c])
AC_CONFIG_MACRO_DIRS([m4])
COLOR_INIT()

dnl Miscellaneous Prep
CHECK_RACKET()
CHECK_EMACS()

CHECK_EMACS_MODULE_SUPPORT()

AC_ARG_ENABLE([debug], [Turn on debug builds], [
  CFLAGS="$CFLAGS -g"
  AC_DEFINE(RE_DEBUG_BUILD, [1], [Whether this is a debug build.])
])
dnl From Emacs configure.ac:
AC_CACHE_CHECK([for 'find' args to delete a file],
  [emacs_cv_find_delete],
  [if touch conftest.tmp && find conftest.tmp -delete 2>/dev/null &&
      test ! -f conftest.tmp
   then emacs_cv_find_delete="-delete"
   else emacs_cv_find_delete="-exec rm -f {} ';'"
   fi])
FIND_DELETE=$emacs_cv_find_delete
AC_SUBST([FIND_DELETE])

dnl Also from Emacs configure.ac:
if test "${opsys}" = "mingw32"; then
  SEPCHAR=';'
else
  SEPCHAR=':'
fi
AC_DEFINE_UNQUOTED(SEPCHAR, ['$SEPCHAR'], [Character that separates PATH elements.])
dnl This is for MinGW, and is used in test/Makefile.in.
dnl The MSYS Bash has heuristics for replacing ':' with ';' when it
dnl decides that a command-line argument to be passed to a MinGW program
dnl is a PATH-style list of directories.  But that heuristics plays it
dnl safe, and only does the replacement when it is _absolutely_ sure it
dnl sees a colon-separated list of file names; e.g. ":." is left alone,
dnl which breaks in-tree builds.  So we do this manually instead.
dnl Note that we cannot rely on PATH_SEPARATOR, as that one will always
dnl be computed as ':' in MSYS Bash.
AC_SUBST(SEPCHAR)

dnl Set up Compilation Flags
RACKET_DEFDIR([RACKET_LIBDIR], [dll])
RACKET_DEFDIR([RACKET_INCDIR], [include])

LDFLAGS="$LDFLAGS -Wl,--no-as-needed,--enable-new-dtags,-rpath,${RACKET_LIBDIR}"
LDFLAGS="$LDFLAGS -L${RACKET_LIBDIR}"
CFLAGS="$CFLAGS -I${RACKET_INCDIR} -L${RACKET_LIBDIR}"
CPPFLAGS="$CPPFLAGS -I${RACKET_INCDIR} -L${RACKET_LIBDIR}"

dnl AC_MSG_NOTICE([LDFLAGS: ${LDFLAGS}])
dnl AC_MSG_NOTICE([CFLAGS: ${CFLAGS}])
dnl AC_ARG_VAR

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

AC_SEARCH_LIBS([hattrie_get], [hat-trie], [], [
  AC_MSG_ERROR([unable to locate libhat-trie. Please ensure that it is installed.])
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

AC_CHECK_HEADERS([hat-trie/hat-trie.h], [], [
  AC_MSG_ERROR([unable to locate libhat-trie headers])
])

RACKET_DEF([RACKET_DEFAULT_GC], ['(displayln (system-type (quote gc)))'])
AM_CONDITIONAL([RACKET_GC_IS3M_TST], [test "x$RACKET_DEFAULT_GC" = "x3m"])
AM_CONDITIONAL([RACKET_GC_ISCGC_TST], [test "x$RACKET_DEFAULT_GC" = "xcgc"])

RACKET_CDEFDIR([RACKET_COLLECTS_DIR], [collects])
RACKET_DEFDIR([RACKET_CONFIG_DIR], [config])
AC_DEFINE_UNQUOTED([RACKET_CONFIG_DIR], ["$RACKET_CONFIG_DIR"], [Racket runtime configuration directory])
AC_DEFINE_UNQUOTED([RACKET_CONFIG_PATH], ["$RACKET_CONFIG_DIR/config.rktd"], [Path to Racket runtime configuration file])
AM_COND_IF([RACKET_GC_IS3M_TST], [
  AC_DEFINE(RACKET_GC_IS3M, [1], [Whether the default GC is the 3m implementation])
  AC_DEFINE(MZ_PRECISE_GC, [1], [Indicates to Racket that the 3m garbage collector is in use.])
], [])
AM_COND_IF([RACKET_GC_ISCGC_TST], [
  AC_DEFINE(RACKET_GC_ISCGC, [1], [Whether the default GC is the CGC implementation])
], [])

CHECK_EMACS()
CHECK_EMACS_MODULE_SUPPORT()
EXTRA_EMACS_DIRS="\"$(pwd)/lisp\" \"$(pwd)/src/.libs\" \"$(pwd)/test/src\""
GET_EMACS_LOAD_PATH($EXTRA_EMACS_DIRS)

SECTION_TITLE([yellow], [Generating files])
dnl Create Makefile
AC_CONFIG_FILES([Makefile src/Makefile lisp/Makefile test/Makefile test/src/test-config.el test/.dir-locals.el])
AC_OUTPUT

AS_ECHO(["
Finished Configuration."])

SECTION_TITLE([yellow], [Configuration Summary])
AS_ECHO(["  CFLAGS:.............  ${CFLAGS}
  LDFLAGS:............  ${LDFLAGS}
  Emacs header source:   ${EMACS_HEADER_SOURCE}
  Default Racket GC:..   ${RACKET_DEFAULT_GC}
"])
AC_MSG_NOTICE([Run 'make' to build the library.])
