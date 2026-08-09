EMACS ?= emacs

# Dependencies are installed into the project so that a checkout never
# disturbs the developer's own ~/.emacs.d.
export ELPA_DIR ?= $(CURDIR)/.elpa

BATCH = $(EMACS) -Q --batch -L . \
	  --eval '(setq package-user-dir (getenv "ELPA_DIR"))' \
	  --eval '(package-initialize)'

EL    = $(wildcard lean4-*.el)
ELC   = $(EL:.el=.elc)
TESTS = $(wildcard test/lean4-*-test.el)

.PHONY: all help deps compile checkdoc lint test e2e docs clean distclean

all: compile checkdoc test

help:
	@echo 'Targets:'
	@echo '  deps      install dependencies into $$ELPA_DIR ($(ELPA_DIR))'
	@echo '  compile   byte-compile all libraries; warnings are errors'
	@echo '  checkdoc  run checkdoc over all libraries'
	@echo '  lint      run package-lint over all libraries'
	@echo '  test      run the ERT suite (no language server required)'
	@echo '  e2e       run the end-to-end suite against a real "lake serve"'
	@echo '  docs      regenerate lean4-mode.texi and lean4-mode.info'
	@echo '  clean     remove byte-compiled files'

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
	@$(BATCH) -L test -l test/lean4-e2e-test.el \
	  --eval "(ert-run-tests-batch-and-exit '(tag :e2e))"

# GNU- and NonGNU-Elpa accept Org files as package documentation but Melpa
# does not.  As long as lean4-mode is not distributed on GNU- or NonGNU-Elpa,
# it should ship with .texi and .info manuals.  Needs GNU Texinfo.
docs: lean4-mode.info

lean4-mode.info lean4-mode.texi: README.org
	$(EMACS) --batch \
		"--eval=(require 'ox-texinfo)" \
		'--eval=(find-file "$<")' \
		'--eval=(org-texinfo-export-to-info)'

clean:
	@rm -f $(ELC)

distclean: clean
	@rm -rf $(ELPA_DIR)
