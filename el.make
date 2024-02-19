##
# this file is part of el.make
#
# copyright: 2024- christian schwarzgruber (c.schwarzgruber.cs@gmail.com)
#
# version: 0.1.0
#
# source: https://github.com/casch-at/el.make
#
#
##
####
##
# el.make is free software: you can redistribute it and/or modify it
# under the terms of the gnu general public license as published by the free
# software foundation, either version 3 of the license, or
# (at your option) any later version.
#
# el.make is distributed in the hope that it will be useful,
# but without any warranty; without even the implied warranty of
# merchantability or fitness for a particular purpose. see the
# gnu general public license for more details.
#
# you should have received a copy of the gnu general public license along
# with el.make. if not, see <http://www.gnu.org/licenses/>.
##
####
##
#                     hier koennte ihre werbung stehen
#
#                                 el.make
#
#                       an emacs lisp `make` file
##

# todo(cschwarzgruber): hmm?
export LC_ALL = C

emacs = emacs
flags = -Q --batch --no-site-file
batch = $(emacs) $(flags)

## test directory
test-d = tests

## source directory
src-d = src

## all source files
el-f = $(wildcard $(src-d)/*.el)

## all test files
test-f = $(wildcard $(test-d)/*.el)

## all compiled files
elc-f = $(addprefix $(src-d)/, $(notdir $(el-f:.el=.elc)))

.phony: all compile test clean

all: compile test

compile: $(elc-f)

test: $(elc-f)
	@echo "running tests... "
	@$(batch) -L $(src-d) -l $(test-f) -f ert-run-tests-batch-and-exit

$(src-d)/%.elc: $(src-d)/%.el
	@echo "compiling $<..."
	@$(batch) -f batch-byte-compile $<

clean:
	rm -f $(elc-f)
