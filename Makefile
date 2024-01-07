EMACS=emacs
export LC_ALL=C

all: clean compile gen-autoloads test

# Forcefully remove files ignored by Git.
clean:
	git clean -Xf

compile:
	${EMACS} -Q -batch -L . -f batch-byte-compile tests/org-journal-test.el org-journal.el

gen-autoloads:
	${EMACS} -Q -batch -L . -l org-journal.el --eval '`(generate-file-autoloads "org-journal-autoloads.el")`'

test:
	${EMACS} -Q -batch -L . -l tests/org-journal-test -f ert-run-tests-batch-and-exit
