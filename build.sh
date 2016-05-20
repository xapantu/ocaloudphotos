#!/usr/bin/sh
ocamlbuild -use-ocamlfind -plugin-tags "package(eliom.ocamlbuild)" \
                                  server/ocaloudcore.cma server/ocaloudcore.cmxa server/ocaloudcore.cmxs \
                                  client/ocaloudcore.js -use-menhir
