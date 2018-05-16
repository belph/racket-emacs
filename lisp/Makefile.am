AUTOMAKE_OPTIONS = foreign
re_installdir=$(datarootdir)/emacs/site-lisp/racket-emacs
re_ELFILES := $(sort $(shell find ${srcdir} \
		-name "*.el" ! -name ".*" -print))

re_install_SCRIPTS = $(ELFILES)
