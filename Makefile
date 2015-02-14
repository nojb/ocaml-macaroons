OCAMLBUILD=ocamlbuild -use-ocamlfind -classic-display

all:
	$(OCAMLBUILD) lib/macaroons.cma lib/macaroons.cmxa

clean:
	$(OCAMLBUILD) -clean

install:
	ocamlfind install macaroons lib/META \
	$(addprefix _build/lib/,macaroons.mli macaroons.cmi macaroons.cmti macaroons.cma macaroons.cmxa)

uninstall:
	ocamlfind remove macaroons

reinstall: uninstall install

doc: all
	$(OCAMLBUILD) lib/api.docdir/index.html

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

.PHONY: all clean install uninstall reinstall doc gh-pages
