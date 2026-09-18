.PHONY: run build install-deps

run:
	dune exec dolphinc

build:
	dune build

install-deps:
	opam install . --deps-only
