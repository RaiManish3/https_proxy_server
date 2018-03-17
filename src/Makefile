SRC=src

.DEFAULT_GOAL := build

build: Server.hs
	@ghc -dynamic -threaded Server.hs -o pgm

.PHONY: clean

clean:
	@rm -rf pgm *.o *.hi
