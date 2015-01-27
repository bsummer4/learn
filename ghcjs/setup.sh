#!/bin/sh

nix-env -i haskell-ghcjs-ghc7.8.4-0.1.0-shared
nix-env -i closure-compiler-20130603
make test
make clean
