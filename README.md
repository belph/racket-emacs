# Racket Emacs Bindings

This project is an attempt to use Emacs' dynamic module support to facilitate
scripting Emacs using Racket. This is very much a work-in-progress!

## Building

Compilation requires a Racket installation and the system's installed Emacs 
to support dynamic modules (i.e. it was compiled with the `--with-modules` 
configure flag).

```bash
$ ./autogen.sh
$ ./configure
$ make
# Optionally:
$ make check
$ sudo make install
```

