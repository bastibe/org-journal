all: compile test

test:
	emacs -batch -L . -l org-journal-test -f ert-run-tests-batch-and-exit

compile:
	emacs -batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc
