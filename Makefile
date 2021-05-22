MODULES=main common client server world_manager input_handler author model
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
SERVER=server.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,sdl,bigarray,sdl.sdlimage,sdl.sdlttf,sdl.sdlmixer,yojson

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) -tag 'thread' $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'thread' -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'thread' -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN) $(CMD)

zip:
	zip clash-of-camls.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit *.md Makefile assets/*

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-thread -html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-thread -html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
