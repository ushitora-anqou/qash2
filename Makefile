.PHONY: build
build:
	nix fmt
	dune build

.PHONY: test
test: build
	dune runtest
