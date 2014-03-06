{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

broadcast ∷ Text → [WS.Connection] → IO ()
broadcast message clients = do
	T.putStrLn message
	forM_ clients $ \conn → WS.sendTextData conn message

application ∷ MVar [WS.Connection] → WS.PendingConnection → IO ()
application state pending = do
	conn ← WS.acceptRequest pending
	liftIO $ modifyMVar_ state $ return . (conn:)
	talk conn state

talk ∷ WS.Connection → MVar[WS.Connection] → IO ()
talk conn state = forever $ do
	msg ← WS.receiveData conn
	liftIO $ readMVar state >>= broadcast msg

main ∷ IO ()
main = do
	state ← newMVar([]∷[WS.Connection])
	WS.runServer "0.0.0.0" 9160 $ application state
