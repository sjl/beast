.PHONY: pubdocs

quickutils.lisp: make-quickutils.lisp
	sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

$(apidoc): $(sourcefiles) docs/api.lisp package.lisp
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'


docs/build/index.html: $(docfiles) $(apidoc) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/beast
	hg -R ~/src/sjl.bitbucket.org commit -Am 'beast: Update site.'
	hg -R ~/src/sjl.bitbucket.org push

