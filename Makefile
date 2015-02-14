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

.PHONY: all clean install uninstall reinstall
