.DEFAULT_GOAL := build

build:
	cabal build

install: build
	cabal install --overwrite-policy=always

clean:
	rm -rf dist-newstyle scala