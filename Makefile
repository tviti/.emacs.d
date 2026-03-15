EMACS ?= emacs

.PHONY: test test-pre test-post bootstrap clean

test: ## Run all tests (requires installed packages)
	$(EMACS) --batch -l test/test-config.el -f ert-run-tests-batch-and-exit

test-pre: ## Run pre-install tests only (syntax, structure)
	$(EMACS) --batch -l test/test-config.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag :post-install)))"

test-post: ## Run post-install tests only (byte-compile, load, symbols)
	$(EMACS) --batch -l test/test-config.el \
		--eval "(ert-run-tests-batch-and-exit '(tag :post-install))"

bootstrap: ## Install packages then run full test suite
	$(EMACS) --batch -l init.el --eval "(kill-emacs 0)"
	$(MAKE) test

clean: ## Remove byte-compiled files
	find . -name "*.elc" -delete
