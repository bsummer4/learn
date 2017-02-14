module Main (main) where

import qualified Control.Monad

-- `pure`, `join`, `>>`, `>>=` are all methods on IO. However, I'm not ready
-- to talk about how methods work in Haskell, so I'm going to make my own
-- versions of these, with simpler types.

andThen :: (IO a, IO b) -> IO b
andThen(firstAction, secondAction) = firstAction Control.Monad.>> secondAction

joinIO :: (IO (IO a)) -> IO a
joinIO(x) = Control.Monad.join(x)

pureIO :: a -> IO a
pureIO(x) = pure(x)

bindIO :: (IO a, (a -> IO b)) -> IO b
bindIO(firstAction, secondAction) = firstAction >>= secondAction

helloWorld :: IO ()
helloWorld = putStrLn("Hello World")

helloWorldTwice :: IO ()
helloWorldTwice = andThen(helloWorld, helloWorld)

doTwice :: IO a -> IO a
doTwice action = andThen(action, action)

doNTimes :: (Int, IO ()) -> IO ()
doNTimes (numTimes, action) =
    if numTimes <= 0
        then pure ()
        else andThen(action, doNTimes(numTimes - 1, action))

-- Again, I'm aliasing the generic function `read` to a simpler type to
-- avoid discussing methods.
readInt :: String -> Int
readInt = read

machineToReadInts :: String -> IO Int
machineToReadInts str = pureIO(readInt(str))

-- `getLine` has type `IO String`
getInt :: IO Int
getInt = bindIO(getLine, machineToReadInts)

main :: IO ()
main =
  andThen(
    putStrLn "How many times should we say 'Hello World'?",
    bindIO(
      getInt,
      \n -> doNTimes(n, helloWorld)))
