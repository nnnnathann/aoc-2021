all: test build

.PHONY: test
test:
	cabal new-test

build:
	cabal new-build