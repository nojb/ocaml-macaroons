OCAMLBUILD = ocamlbuild -use-ocamlfind -classic-display

all: macaroons sodium_macaroons

OBJECTS = macaroons.cma macaroons.cmxa macaroons.cmxs

macaroons:
	$(OCAMLBUILD) $(addprefix lib/, $(OBJECTS))

sodium_macaroons:
	$(OCAMLBUILD) $(addprefix lib/sodium_, $(OBJECTS))

clean:
	$(OCAMLBUILD) -clean

PRODUCTS = \
	macaroons.mli macaroons.cmi macaroons.cmti macaroons.cma \
	macaroons.cmxa macaroons.cmxs

install: all
	ocamlfind install macaroons lib/META \
	$(addprefix _build/lib/,$(PRODUCTS)) \
	$(addprefix _build/lib/sodium_,$(PRODUCTS))

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
