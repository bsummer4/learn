{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, PartialTypeSignatures     #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

module Raft where

import Control.Exception.Base (assert)
import Control.Monad

import           Data.Binary
import           Data.Foldable.Unicode
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Typeable
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Data.Vector.Binary    ()

import GHC.Generics (Generic)

import Prelude.Unicode hiding ((∈), (∉))


-- Types -----------------------------------------------------------------------

newtype Term = Term { unTerm ∷ Integer }
  deriving (Ord,Eq,Num,Typeable,Generic,Binary)

newtype LogIdx = LogIdx { unLogIdx ∷ Int }
  deriving (Ord,Eq,Num,Typeable,Generic,Binary)

data Entry e = Entry { entryValue ∷ !e
                     , entryTerm  ∷ !Term }
  deriving (Typeable,Generic)

data Log e = Log { entries   ∷ !(Vector (Entry e))
                 , commitIdx ∷ !LogIdx }

data ServerState pid = Follower
                     | Leader    { nextIndex      ∷ Map pid LogIdx
                                 , matchIndex     ∷ Map pid LogIdx }
                     | Candidate { votesResponded ∷ Set pid
                                 , votesGranted   ∷ Set pid }

data Server pid e = Server { srvTerm      ∷ Term
                           , srvPid       ∷ pid
                           , srvPeers     ∷ Set pid
                           , srvVotedFor  ∷ Maybe pid
                           , srvLog       ∷ Log e
                           , srvState     ∷ ServerState pid }

data Message body pid e = Message { msgBody   ∷ body pid e
                                  , msgTerm   ∷ Term
                                  , msgSource ∷ pid
                                  , msgTarget ∷ pid }
  deriving (Typeable,Generic)

data AppendEntries pid e = AppendEntries { mPrevLogIdx  ∷ LogIdx
                                         , mPrevLogTerm ∷ Term
                                         , mCommitIdx   ∷ LogIdx
                                         , mEntries     ∷ Vector(Entry e) }
  deriving (Typeable,Generic)

data AppendEntriesResp pid e = AppendEntriesResp
  deriving (Typeable,Generic)

data RequestVote pid e = RequestVote { mLastLogTerm ∷ Term
                                     , mLastLogIdx  ∷ LogIdx }
  deriving (Typeable,Generic)

data RequestVoteResp pid e = RequestVoteResp Bool
  deriving (Typeable,Generic)

data RPC n e = Append     (AppendEntries n e)
             | AppendResp (AppendEntriesResp n e)
             | Vote       (RequestVote n e)
             | VoteResp   (RequestVoteResp n e)
  deriving (Typeable,Generic)

data Event n e = RPC (Message RPC n e)
  deriving (Typeable,Generic)

-- The DeriveAnyClass extension would make this boilerplate unnecessary,
-- however, it conflicts with GeneralizedNewtypeDeriving, which we are using
-- for `Term` and `LogIdx`.
instance (Binary n, Binary e, Binary(rpc n e)) => Binary (Message rpc n e)
instance (Binary e)                            => Binary (Entry e)
instance (Binary n, Binary e)                  => Binary (RPC n e)
instance (Binary n, Binary e)                  => Binary (RequestVote n e)
instance (Binary n, Binary e)                  => Binary (RequestVoteResp n e)
instance (Binary n, Binary e)                  => Binary (AppendEntries n e)
instance (Binary n, Binary e)                  => Binary (AppendEntriesResp n e)
instance (Binary n, Binary e)                  => Binary (Event n e)


-- Utilities -------------------------------------------------------------------

assertM ∷ Monad m ⇒ Bool → m ()
assertM = flip assert $ return ()

isCandidate ∷ ServerState pid → Bool
isCandidate (Candidate _ _) = True
isCandidate _               = False

isQuorum ∷ Server pid e → Set pid → Bool
isQuorum Server{..} voteSet = 2*(Set.size voteSet) > Set.size srvPeers

lastElem ∷ Vector e → Maybe e
lastElem v = v V.!? (V.length v - 1)

lastTerm ∷ Log e → Maybe Term
lastTerm = fmap entryTerm . lastElem . entries

lastIndex ∷ Log e → LogIdx
lastIndex = LogIdx . (\x→x-1) . V.length . entries

emptyLog ∷ Log e
emptyLog = Log V.empty (-1)

emptyCandidate ∷ ServerState pid
emptyCandidate = Candidate Set.empty Set.empty

emptyLeader ∷ Set pid → ServerState pid
emptyLeader peers = Leader {
  nextIndex  = Map.fromSet (const 0) peers,
  matchIndex = Map.fromSet (const(-1)) peers }

initialState ∷ pid → Set pid → Server pid e
initialState pid peers = Server {
  srvTerm      = 0,
  srvPid       = pid,
  srvPeers     = peers,
  srvLog       = emptyLog,
  srvVotedFor  = Nothing,
  srvState     = Follower}


-- Transitions -----------------------------------------------------------------

restart ∷ Server pid e → Server pid e
restart Server{..} = (initialState srvPid srvPeers) {
  srvState    = Follower,
  srvTerm     = srvTerm,
  srvLog      = srvLog { commitIdx=(-1) },
  srvVotedFor = srvVotedFor}

timeout ∷ Eq (ServerState pid) ⇒ Server pid e → Maybe (Server pid e)
timeout Server{..} =
  case srvState of
    Leader{..} → Nothing
    _          → Just $ Server {
      srvTerm     = srvTerm + 1,
      srvPid      = srvPid,
      srvPeers    = srvPeers,
      srvVotedFor = Nothing, -- (Just srvPid) would be ok.
      srvLog      = srvLog,
      srvState    = emptyCandidate}

requestVote ∷ (Eq pid, Ord pid)
            ⇒ Server pid e → pid → Maybe (Message RequestVote pid e)
requestVote Server{..} target = do
  Candidate{..} ← return $ srvState
  guard (target ∉ votesResponded)
  req ← RequestVote <$> lastTerm srvLog <*> Just(lastIndex srvLog)
  return $ Message req srvTerm srvPid target

appendEntries ∷ (Eq pid, Ord pid)
              ⇒ Server pid e → pid → Maybe (Message AppendEntries pid e)
appendEntries Server{..} target = do
  guard $ srvPid ≠ target

  Leader{..}  ← return $ srvState

  nextIndexForTarget ← Map.lookup target nextIndex
  let numEntries = max 0 $ unLogIdx $ lastEntry - nextIndexForTarget
      lastEntry = min (LogIdx (V.length(entries srvLog)-1))
                      (nextIndexForTarget+1)

  assertM $ numEntries == 0 || numEntries == 1

  let mEntries   = V.slice (unLogIdx nextIndexForTarget) numEntries mEntries
      mCommitIdx = commitIdx srvLog `min` lastEntry
  mPrevLogIdx    ← (\x→x-1) <$> Map.lookup target nextIndex
  mPrevLogTerm   ← if mPrevLogIdx<0 then Just(-1) else
                     entryTerm <$> (entries srvLog) V.!? (unLogIdx mPrevLogIdx)

  return $ Message (AppendEntries{..}) srvTerm srvPid target

becomeLeader ∷ Server pid e → Maybe (Server pid e)
becomeLeader srv@Server{..} = do
  Candidate{..} ← return srvState
  guard $ isQuorum undefined votesGranted
  return $ undefined $ srv { srvState = Leader undefined undefined }

clientRequest ∷ Server pid e → e → Maybe (Server pid e)
clientRequest = undefined

advanceCommitIndex ∷ Server pid e → Maybe (Server pid e)
advanceCommitIndex = undefined

handleRequestVoteRequest ∷ Eq pid
                         ⇒ Server pid e → Message RequestVote pid e
                         → Server pid e
handleRequestVoteRequest srv@Server{..} Message{..} = fromMaybe srv $ do
  guard $ msgTerm ≤ srvTerm
  guard $ msgTarget ≡ undefined
  guard $ srvVotedFor≡Nothing ∨ srvVotedFor≡Just msgSource
  return undefined

handleRequestVoteResponse ∷ Server pid e → Message RequestVoteResp pid e
                          → Maybe (Server pid e)
handleRequestVoteResponse = undefined

handleAppendEntriesRequest ∷ Server pid e → Message AppendEntries pid e
                           → Maybe (Server pid e)
handleAppendEntriesRequest = undefined

handleAppendEntriesResponse ∷ Server pid e → Message AppendEntriesResp pid e
                            → Maybe (Server pid e)
handleAppendEntriesResponse = undefined

updateTerm ∷ Server pid e → Message m pid e → Maybe (Server pid e)
updateTerm = undefined

dropStaleResponse ∷ Server pid e → Message m pid e → Maybe (Message m pid e)
dropStaleResponse = undefined

receive ∷ Server pid e → Message RPC pid e → Maybe (Server pid e)
receive = undefined
