.PHONY: pubdocs test-sbcl test-ccl test-ecl test

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


test: test-sbcl test-ccl test-ecl

test-sbcl:
	echo; figlet -kf computer 'SBCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L sbcl --load test-run.lisp

test-ccl:
	echo; figlet -kf slant 'CCL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ccl-bin --load test-run.lisp

test-ecl:
	echo; figlet -kf roman 'ECL' | sed -Ee 's/ +$$//' | tr -s '\n' | lolcat --freq=0.25; echo
	ros run -L ecl --load test-run.lisp
