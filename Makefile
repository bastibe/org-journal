EMACS = emacs
EASK = eask
BATCH = $(EMACS) -Q -batch -L .

export LC_ALL=C

all: clean compile gen-autoloads test

# Forcefully remove files ignored by Git.
clean:
	git clean -Xf

compile:
	$(EASK) compile

gen-autoloads:
	$(EASK) generate autoloads

test:
	$(EASK) test ert ./tests/*.el
