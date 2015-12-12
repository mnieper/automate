SCHEME=chibi-scheme -Ilib
REPL=$(SCHEME) -R
TEST=$(SCHEME) -ltests/testing.scm

repl:
	$(REPL) -mautomate

test:
	$(TEST) tests/polynomial-tests.scm
	$(TEST)	tests/automate-tests.scm

.PHONY: repl tests
