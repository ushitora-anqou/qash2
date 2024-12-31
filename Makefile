.PHONY: build
build:
	nix fmt
	direnv reload
	dune build

.PHONY: test
test: build
	dune runtest
