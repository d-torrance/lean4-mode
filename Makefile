EMACS ?= emacs

# Dependencies are installed into the project so that a checkout never
# disturbs the developer's own ~/.emacs.d.
export ELPA_DIR ?= $(CURDIR)/.elpa

# Point at a directory to test against a dependency version other than the
# newest, which is what `deps' installs.  Debian and Ubuntu ship the oldest
# magit-section this package supports, and it is worth not breaking:
#   make test SHADOW=/usr/share/emacs/site-lisp/elpa/magit-section-3.3.0
SHADOW ?=

BATCH = $(EMACS) -Q --batch -L . \
	  --eval '(setq package-user-dir (getenv "ELPA_DIR"))' \
	  --eval '(package-initialize)' \
	  $(if $(SHADOW),-L $(SHADOW))

EL    = $(wildcard lean4-*.el)
ELC   = $(EL:.el=.elc)
# The end-to-end suite is excluded here, not merely deselected: `make test'
# is meant to need no Lean toolchain and to finish in a moment, and loading
# that file at all pulls a server into the picture.
E2E   = test/lean4-e2e-test.el
TESTS = $(filter-out $(E2E),$(wildcard test/lean4-*-test.el))

.PHONY: all help deps compile checkdoc lint test e2e docs html abbreviations \
	clean distclean

all: compile checkdoc test

help:
	@echo 'Targets:'
	@echo '  deps      install dependencies into $$ELPA_DIR ($(ELPA_DIR))'
	@echo '  SHADOW=DIR  put DIR ahead of them, to test an older dependency'
	@echo '  compile   byte-compile all libraries; warnings are errors'
	@echo '  checkdoc  run checkdoc over all libraries'
	@echo '  lint      run package-lint over all libraries'
	@echo '  test      run the ERT suite (no language server required)'
	@echo '  e2e       run the end-to-end suite against a real "lake serve"'
	@echo '  docs      regenerate lean4-mode.texi and lean4-mode.info'
	@echo '  html      export the manual to public/ as a web page'
	@echo '  abbreviations  refresh data/abbreviations.json from vscode-lean4'
	@echo '  clean     remove byte-compiled files and the exported website'

deps:
	@$(EMACS) -Q --batch -L . \
	  --eval '(setq package-user-dir (getenv "ELPA_DIR"))' \
	  -l build.el -f lean4-build-deps

compile:
	@$(BATCH) -l build.el -f lean4-build-compile

checkdoc:
	@$(BATCH) -l build.el -f lean4-build-checkdoc

lint:
	@$(BATCH) -l build.el -f lean4-build-lint

# Both depend on `compile': a stale .elc silently shadows the .el it was
# built from, so tests would otherwise exercise the previous edit.
test: compile
	@$(BATCH) -L test $(patsubst %,-l %,$(TESTS)) \
	  -f ert-run-tests-batch-and-exit

# Kept out of `test' because it needs a Lean toolchain and takes minutes.
e2e: compile
	@$(BATCH) -L test -l $(E2E) \
	  --eval "(ert-run-tests-batch-and-exit '(tag :e2e))"

# The table stays checked in -- package managers need it, and a build that
# reaches the network is a build that fails offline -- but refreshing it
# should not be manual.  .github/workflows/update-abbr.yml does this on a
# schedule; this is the same thing by hand.
abbreviations:
	@$(BATCH) -l build.el -f lean4-build-abbreviations

# GNU- and NonGNU-Elpa accept Org files as package documentation but Melpa
# does not.  As long as lean4-mode is not distributed on GNU- or NonGNU-Elpa,
# it should ship with .texi and .info manuals.  Needs GNU Texinfo.
docs: lean4-mode.info

lean4-mode.info lean4-mode.texi: README.org
	$(EMACS) --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-info)'

# The same README the .texi comes from, exported to HTML instead, so the
# website says exactly what the shipped manual says.  Needs no Texinfo:
# ox-html is built into Emacs.  .github/workflows/pages.yml publishes the
# result; this is the same thing by hand, and `make html && xdg-open
# public/index.html' is how to look at a change before pushing it.
html:
	@$(EMACS) -Q --batch -l build.el -f lean4-build-html

clean:
	@rm -f $(ELC)
	@rm -rf public

distclean: clean
	@rm -rf $(ELPA_DIR)
