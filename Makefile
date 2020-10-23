EMACS=emacs
export LC_ALL=C

all: clean compile test

test:
	${EMACS} -Q -batch -L . -l tests/org-journal-test -f ert-run-tests-batch-and-exit

compile:
	${EMACS} -Q -batch -L . -f batch-byte-compile tests/org-journal-test.el org-journal.el

clean:
	rm -f *.elc
