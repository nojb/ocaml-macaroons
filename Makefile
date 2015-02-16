# set -x
# set -e

SODIUM ?= $(shell if ocamlfind query sodium >/dev/null 2>&1; then echo 1; fi)

OCAMLBUILD = ocamlbuild -use-ocamlfind -classic-display

.PHONY: prepare doc test build install uninstall clean

build:
ifeq ($(SODIUM),)
	$(OCAMLBUILD) lib/macaroons.{cma,cmxa,cmxs}
else
	$(OCAMLBUILD) lib/{sodium_,}macaroons.{cma,cmxa,cmxs}
endif

prepare: build test doc gh-pages
ifdef VERSION
	git diff --quiet && git diff --cached --quiet # make sure there are no uncommited changes
	git tag -f "v$(VERSION)"
	git push origin master
	git push --force origin "v$(VERSION)"
	opam-publish prepare "macaroons.$(VERSION)" \
		"https://github.com/nojb/ocaml-macaroons/archive/v$(VERSION).tar.gz"
else
	$(error VERSION is undefined)
endif

release: prepare
	opam-publish submit "./macaroons.$(VERSION)"

doc:
	$(OCAMLBUILD) -docflag -colorize-code doc/api.docdir/index.html
	cp doc/style.css api.docdir

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp api.docdir/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

test:
	$(OCAMLBUILD) lib_test/test.byte
	./test.byte

install: build
	ocamlfind install macaroons lib/META _build/lib/*macaroons.{mli,cmi,cmti,cma,cmxa,cmxs}

uninstall:
	ocamlfind remove macaroons

reinstall: uninstall install

clean:
	$(OCAMLBUILD) -clean
