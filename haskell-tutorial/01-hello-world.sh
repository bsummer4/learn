#!/usr/bin/env bash

set -xe

# The stack tool takes care of installing Haskell libraries and executables.
echo brew update || true
echo brew install haskell-stack || true

# `resolver: lts-8.0` tells stack which set of libraries we want to work
# with. The Stackage project maintains a huge set of libraries that are
# guaranteed to work together well. This system eliminates any possibility of
# dependency conflicts, and makes builds completely reproducible.

cat >stack.yaml <<EOF
resolver: lts-8.0
packages: ['.']
EOF

# Set up basic project metadata.

cat >package.yaml <<EOF
name: haskell-tutorial
version: '0.1.0.0'
dependencies: ['base']
executables:
  haskell-tutorial:
    main: Main.hs
EOF

# Hello World in Haskell.
#
# The type `main :: IO ()` means that `main` is an "IO Machine" that may
# perform some side effects and then must output an empty tuple.
#
# Internally, `IO` machines are capable of invoking C procedures (through
# the FFI) and transforming data using Haskell functions. For example,
# `putStrLn` is implemented through a combination of pure Haskell functions and
# invocations of low-level IO routines from libc.
#
# Notice that the top-level entry point into a Haskell program is an `IO`
# machine.
cat >Main.hs <<EOF
main :: IO ()
main = putStrLn "Hello World"
EOF

stack setup
stack build
stack exec haskell-tutorial
