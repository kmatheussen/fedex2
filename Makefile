
GUILE = GUILE_AUTO_COMPILE=0 GUILE_WARN_DEPRECATED=detailed guile --fresh-auto-compile

test:
	$(GUILE) test.scm

bootstrapped:
	$(GUILE) bootstrap-from-bootstrapped.scm

bootstrapped-from-shen:
	$(GUILE) bootstrap-from-shen.scm

# Not very useful if shen is not used. Not very useful, even then, because variable names differs.
test-bootstrapped-validity:
	diff -u bootstrapped-from-shen.scm bootstrapped.scm
