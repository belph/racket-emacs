AUTOMAKE_OPTIONS = foreign
lib_RACKETEMACS = libracketemacs.la
re_installdir=$(datarootdir)/emacs/site-lisp/racket-emacs
re_install_LTLIBRARIES = libracketemacs.la
libracketemacs_la_SOURCES = emacs-c-utils.c conv.c racket-rt.c ffi-utils.c racket-rtutils.c mymod.c
if NEEDS_EMACS_HEADER
libracketemacs_la_CFLAGS = -I$(top_srcdir)/include-emacs -I$(top_srcdir)/include
else
libracketemacs_la_CFLAGS = -I$(top_srcdir)/include
endif
libracketemacs_la_LDFLAGS = -version-info 0:0:0
