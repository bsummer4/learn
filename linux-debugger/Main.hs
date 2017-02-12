module Main (main) where

import Control.Lens
import ClassyPrelude
import System.Posix.Process

import qualified System.Linux.Ptrace as PT

main :: IO ()
main =
  forkProcess child >>= parent
 where
  child = do
    threadDelay 1000000 -- Wait a second
    putStrLn "Child process exit."
    return ()
  parent childPid = do
    patient <- PT.traceProcess childPid
    putStrLn "Child process paused."
    threadDelay 2000000 -- Wait two seconds
    putStrLn "Resuming child process"
    PT.continue patient
    PT.detach patient
    _ <- getProcessStatus True False childPid
    putStrLn "Child exited"
