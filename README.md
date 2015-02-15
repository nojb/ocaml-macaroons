### Macaroons

A minimal implemention of [libmacaroons](https://github.com/rescrv/libmacaroons)
in pure OCaml.  See the [paper](http://research.google.com/pubs/pub41892.html)
for more information.

**Author:** Nicolas Ojeda Bar (n.oje.bar@gmail.com)

#### Dependencies

- [ocaml-sodium](https://github.com/dsheets/ocaml-sodium)
- [ocaml-base64](https://github.com/mirage/ocaml-base64)
- [ocaml-hex](https://github.com/mirage/ocaml-hex)

#### How to install

Make sure that you have installed [`libsodium`](https://github.com/jedisct1/libsodium).

Using OPAM:
```
opam install sodium macaroons
```

Manually:
```
git clone https://github.com/nojb/ocaml-macaroons
cd ocaml-macaroons
make
make install
```

#### Documentation

See the online version of the ocamldoc documentation
[here](https://nojb.github.io/ocaml-macaroons).
