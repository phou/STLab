.DEFAULT_GOAL := build

build:
	cabal build

install: build
	cabal install --overwrite-policy=always
	ln -s ~/.cabal/bin/HsScribble HsScribble

clean:
	rm -rf dist-newstyle scala HsScribble