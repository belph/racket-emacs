AUTOMAKE_OPTIONS = foreign
lib_MYMOD = libmymod.la
lib_LTLIBRARIES = libmymod.la
libmymod_la_SOURCES = mymod.c
if NEEDS_EMACS_HEADER
libmymod_la_CFLAGS = -I$(top_srcdir)/include-emacs -I$(top_srcdir)/include
else
libmymod_la_CFLAGS = -I$(top_srcdir)/include
endif
libmymod_la_LDFLAGS = -version-info 0:0:0
