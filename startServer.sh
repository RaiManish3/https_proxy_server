#!/bin/bash
make -C src/ > /dev/null 2>&1 &&
src/pgm +RTS -N4  ## run using 4 cores
