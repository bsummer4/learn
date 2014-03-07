{-# LANGUAGE OverloadedStrings, UnicodeSyntax, ScopedTypeVariables #-}

import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding as E
import qualified Network.WebSockets as WS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)


-- State Manipulations
data Client = Client{cUser∷Text, cTopic∷Text, cConn∷WS.Connection}
type Connections = Map Text (Map Text WS.Connection)

fudgeUsername ∷ Text → Text
fudgeUsername n = T.append n "1"

removeClient ∷ Text → Text → Connections → Connections
removeClient topic user db = M.update newTopicT topic db where
	newTopicT topicT = Just $ M.delete user topicT

findConns ∷ Text → Connections → [WS.Connection]
findConns topic conns = M.elems $ fromMaybe M.empty $ M.lookup topic conns

-- Yeilds the username the client ended up with and the new connections db.
addClient ∷ Text → Text → WS.Connection → Connections → (Text,Connections)
addClient topic user conn db =
	(user', M.insert topic (M.insert user' conn topicT) db) where
		topicT = fromMaybe M.empty $ M.lookup topic db
		user' = mkUnique user
		mkUnique user =
			if not(M.member user topicT) then user else
				mkUnique(fudgeUsername user)


-- Server
broadcast ∷ Text → Text → Text → [WS.Connection] → IO()
broadcast topic user msg clients = do
	let payload = T.concat [topic, " : ", user, " : ", msg]
	T.putStrLn payload
	T.putStrLn $ T.concat ["send to ", T.pack $ show $ length clients]
	forM_ clients $ \conn → WS.sendTextData conn payload

application ∷ MVar Connections → WS.PendingConnection → IO()
application stateV pending = do
	conn ← WS.acceptRequest pending
	user ← WS.receiveData conn
	topic ← WS.receiveData conn
	user' ← modifyMVar stateV $ return . swap . addClient topic user conn
	let disconnect = modifyMVar_ stateV $ return . removeClient topic user'
	flip finally disconnect $ forever $ do
		msg∷Text ← WS.receiveData conn
		readMVar stateV >>= broadcast topic user' msg . findConns topic

main ∷ IO()
main = do
	state ← newMVar(M.empty∷Connections)
	WS.runServer "0.0.0.0" 9172 $ application state
