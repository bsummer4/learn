module Main (main) where

import System.Posix.Process
import Control.Lens
import ClassyPrelude

main :: IO ()
main =
  forkProcess childProc >>= parentProc
 where
  childProc =
    putStrLn "Child process successfully forked."
  parentProc childPid = do
    putStrLn $ "Fork success! Child process has pid " <> tshow childPid
    getProcessStatus True False childPid >>= \case
      Nothing ->
        putStrLn "Something went horribly wrong."
      Just _  ->
        putStrLn "Child process exited."
