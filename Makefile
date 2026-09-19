.PHONY: run build install-deps

# run: build
# 	_build/default/bin/main.exe
#
# run-phase-1: build
# 	_build/default/bin/main.exe compile --from-ast --phase 1 dolphin_web_ast-2026-8-5-15-22-26.json

run:
	dune exec dolphinc

build:
	dune build

install-deps:
	opam install . --deps-only
