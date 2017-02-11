module Main (main) where

import System.Posix.Process
import Control.Lens
import ClassyPrelude

import qualified Network.Socket            as S hiding (send, recv)
import qualified Network.Socket.ByteString as S (send, recv)


--------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  (pFd, cFd) <- S.socketPair S.AF_UNIX S.Stream S.defaultProtocol
  forkProcess (childProc cFd) >>= (parentProc pFd)
 where
  childProc sock = do
    putStrLn "[child] Child process successfully forked."
    putStrLn "[child] Child -> Parent"
    S.send sock "Hi!"
    putStrLn "[child] Send successful! Waiting for a reply."
    "Hi!" <- S.recv sock 1024
    putStrLn "[child] Received the message. Closing our side of the socket."
    putStrLn "[child] Exiting"

  parentProc sock childPid = do
    putStrLn $ "[parent] Fork success! Child process has pid " <> tshow childPid
    putStrLn $ "[parent] Waiting for message from child."
    "Hi!" <- S.recv sock 1024
    putStrLn $ "[parent] Received message from client! Sending a response."
    putStrLn $ "[parent] Parent -> Child"
    S.send sock "Hi!"
    putStrLn $ "[parent] Send successful."
    putStrLn $ "[parent] Waiting for client to exit."
    putStrLn $ "[parent] Closing our side of the socket."
    S.close sock

    getProcessStatus True False childPid >>= \case
      Nothing ->
        putStrLn "Something went horribly wrong."
      Just _  ->
        putStrLn "Child process exited."
