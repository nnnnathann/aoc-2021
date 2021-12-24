all: test build

.PHONY: test
test:
	cabal new-test

.PHONY: dev
dev:
	nodemon --ext hs -x "cabal new-test --test-show-details=streaming --test-options=--format=failed-examples"

build:
	cabal new-build