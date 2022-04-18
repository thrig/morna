depend:
	@sbcl --version

test:
	@./runtests

.PHONY: depend test
