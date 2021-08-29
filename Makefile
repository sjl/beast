.PHONY: pubdocs test-sbcl test-ccl test-ecl test-abcl test

heading_printer = $(shell which heading || echo 'true')
sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

# Documentation ---------------------------------------------------------------
$(apidoc): $(sourcefiles) docs/api.lisp
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidoc) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/docs.stevelosh.com pull -u
	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/beast
	hg -R ~/src/docs.stevelosh.com commit -Am 'beast: Update site.'
	hg -R ~/src/docs.stevelosh.com push

# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl:
	$(heading_printer) computer 'SBCL'
	time sbcl --load test/run.lisp

test-ccl:
	$(heading_printer) slant 'CCL'
	time ccl --load test/run.lisp

test-ecl:
	$(heading_printer) roman 'ECL'
	time ecl -load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	time abcl --load test/run.lisp
