#!/usr/bin/env bash

set -xe

mkdir -p demo/do-notation
cd demo/do-notation

cat >stack.yaml <<EOF
resolver: lts-8.0
packages: ['.']
EOF

cat >package.yaml <<EOF
name: do-notation
version: '0.1.0.0'
dependencies: ['base']
executables:
  do-notation:
    main: Main.hs
EOF

# "do notation" is a simple system that takes imperative-looking code,
# and converts it into a bunch of nested calls to `bind` and `andThen`.
# The implementations of `andThen` and `bindIO` demonstrate the entire
# syntax of `do`.

cat >Main.hs <<'EOF'
module Main (main) where

import qualified Control.Monad

andThen :: (IO a, IO b) -> IO b
andThen(firstAction, secondAction) = do
    firstAction
    secondAction

bindIO :: (IO a, (a -> IO b)) -> IO b
bindIO(firstAction, secondAction) = do
    x <- firstAction
    secondAction(x)

joinIO :: (IO (IO a)) -> IO a
joinIO(x) = do
    Control.Monad.join(x)

pureIO :: a -> IO a
pureIO(x) = do
    pure(x)

helloWorld :: IO ()
helloWorld = do
    putStrLn("Hello World")

helloWorldTwice :: IO ()
helloWorldTwice = do
    helloWorld
    helloWorld

doTwice :: IO a -> IO a
doTwice action = do
    action
    action

doTimes :: (Int, IO ()) -> IO ()
doTimes (numTimes, action) =
    if numTimes <= 0
        then do
            pure ()
        else do
            action
            doTimes(numTimes - 1, action)

-- Again, I'm aliasing the generic function `read` to a simpler type to
-- avoid discussing methods.
readInt :: String -> Int
readInt = read

machineToReadInts :: String -> IO Int
machineToReadInts str = do
    pureIO(readInt(str))

-- `getLine` has type `IO String`
getInt :: IO Int
getInt = do
    bindIO(getLine, machineToReadInts)

main :: IO ()
main = do
    putStrLn "How many times should we say 'Hello World'?"
    nTimes <- getInt
    doTimes(nTimes, helloWorld)
EOF

stack setup
stack build
stack exec do-notation
