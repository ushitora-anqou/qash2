.PHONY: build
build:
	direnv reload
	nix fmt
	dune build

.PHONY: test
test: build
	dune runtest
