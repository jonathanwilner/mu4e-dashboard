.POSIX:

EMACS ?= emacs
EVAL := $(EMACS) --eval
ELISP = notmuch-dashboard.el

PKGDIR := .

# Additional emacs loadpath
LOADPATH        := -L $(PKGDIR) -l test/test-setup.el

all: lints $(ELISP:.el=.elc)

lints: checkdoc

checkdoc:
	for f in $(ELISP); do $(EMACS) -batch -Q $(LOADPATH) --eval "(checkdoc-file \"$$f\")"; done

check: all
	srcdir=$(PWD) $(EMACS) -batch -Q $(LOADPATH) -l test/regression-tests.el -f ert-run-tests-batch

clean:
	rm -f $(ELISP:.el=.elc)

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -Q $(LOADPATH) -L . -f batch-byte-compile $<
