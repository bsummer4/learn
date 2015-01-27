#!/bin/sh

nix-env -i haskell-ghcjs-ghc7.8.4-0.1.0-shared
make test
make clean