.PHONY: pubdocs test-sbcl test-ccl test-ecl test vendor

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Documentation ---------------------------------------------------------------
$(apidoc): $(sourcefiles) docs/api.lisp package.lisp
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
	./test/header.sh computer 'SBCL'
	ros run -L sbcl --load test/test-run.lisp

test-ccl:
	./test/header.sh slant 'CCL'
	ros run -L ccl-bin --load test/test-run.lisp

test-ecl:
	./test/header.sh roman 'ECL'
	ros run -L ecl --load test/test-run.lisp

test-abcl:
	./test/header.sh broadway 'ABCL'
	abcl --load test/test-run.lisp
