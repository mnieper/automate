SCHEME=chibi-scheme -Ilib
REPL=$(SCHEME) -R
TEST=$(SCHEME) -ltests/testing.scm

repl:
	@$(SCHEME) repl.scm

test:
	$(TEST) tests/polynomial-tests.scm
	$(TEST)	tests/automate-tests.scm

.PHONY: repl tests
