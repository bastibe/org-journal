EMACS=emacs

all: clean compile test

test:
	${EMACS} -Q -batch -L . -l org-journal-test -f ert-run-tests-batch-and-exit

compile:
	${EMACS} -Q -batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc
