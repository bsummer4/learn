{-# LANGUAGE OverloadedStrings, UnicodeSyntax, ScopedTypeVariables #-}

import Control.Exception (finally)
import Data.Monoid (mappend)
import Control.Monad (forM_, forever)
import Control.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding as E
import qualified Network.WebSockets as WS


-- Message encoding.
encodeMsg ∷ (Text,Text) → Text
encodeMsg (user,msg) = user `mappend` ":" `mappend` msg

decodeMsg ∷ Text → Maybe (Text,Text)
decodeMsg str = case T.breakOn "::" str of
	(user,"") → Nothing
	(user,tail) → Just (user, T.drop 2 tail)


-- Server State
type User = Text
type Client = (User, WS.Connection)

removeClient ∷ User → [Client] → [Client]
removeClient user = filter ((/= user) . fst)


-- Server
broadcast ∷ Text → Text → [WS.Connection] → IO()
broadcast user message clients = do
	T.putStrLn message
	let e = encodeMsg (user,message)
	forM_ clients $ \conn → WS.sendTextData conn e

application ∷ MVar[Client] → WS.PendingConnection → IO()
application stateV pending = do
	conn ← WS.acceptRequest pending
	user∷Text ← WS.receiveData conn
	modifyMVar_ stateV $ return . ((user,conn):)
	let disconnect = modifyMVar_ stateV $ return . removeClient user
	flip finally disconnect $ forever $ do
		msg∷Text ← WS.receiveData conn
		readMVar stateV >>= broadcast user msg . map snd

main ∷ IO()
main = do
	state ← newMVar([]∷[Client])
	WS.runServer "0.0.0.0" 9160 $ application state
