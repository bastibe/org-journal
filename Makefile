EMACS = emacs
BATCH = $(EMACS) -Q -batch -L .

export LC_ALL=C

all: clean compile gen-autoloads test

# Forcefully remove files ignored by Git.
clean:
	git clean -Xf

compile:
	$(BATCH) -f batch-byte-compile org-journal.el

gen-autoloads:
	$(BATCH) \
		--eval "(require 'autoload)" \
		--eval '(setq make-backup-files nil)' \
		--eval "(setq generated-autoload-file (concat command-line-default-directory \"/\" \"org-journal-autoloads.el\"))" \
		-f batch-update-autoloads "."

test:
	$(BATCH) -l tests/org-journal-test -f ert-run-tests-batch-and-exit
