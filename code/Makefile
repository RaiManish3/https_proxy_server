.DEFAULT_GOAL := build

build: Server.hs
	@ghc -dynamic -threaded Server.hs -o pgm && \
	./pgm +RTS -N4

.PHONY: clean

clean:
	@rm -rf pgm *.o *.hi
