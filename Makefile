MODULES=main common client server
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
SERVER=server.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) -tag 'thread' $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'thread' -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'thread' -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN) $(CMD)
