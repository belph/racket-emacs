# path to the emacs source dir
# (you can provide it here or on the command line)
#ROOT    =
CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS = -Wl,--no-as-needed -L/usr/racket/lib -lracket3m-6.12 -ldl -lm -lffi -lpthread

all: base.c mymod.so

base.c:
	raco ctool --c-mods base.c ++lib racket/base ++lib racket/base/lang/reader

# make shared library out of the object file
%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

# compile source file to object file
%.o: %.c
	$(CC) $(CFLAGS) -I/usr/racket/include/racket -I$(ROOT)/src -fPIC -c $<
