{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, PartialTypeSignatures     #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

module Raft where

import Control.Exception.Base (assert)
import Control.Monad

import           Data.Binary
import           Data.Foldable.Unicode
import qualified Data.List             as L
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Typeable
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Data.Vector.Binary    ()

import GHC.Generics (Generic)

import Prelude.Unicode hiding ((∈), (∉))


-- Types -----------------------------------------------------------------------

newtype Term = Term { unTerm ∷ Integer }
  deriving (Ord,Eq,Enum,Num,Typeable,Generic,Binary)

newtype LogIdx = LogIdx { unLogIdx ∷ Int }
  deriving (Ord,Eq,Enum,Num,Typeable,Generic,Binary)

data Entry e = Entry { entryValue ∷ !e
                     , entryTerm  ∷ !Term }
  deriving (Ord,Eq,Typeable,Generic)

data Log e = Log { entries   ∷ !(Vector (Entry e))
                 , commitIdx ∷ !LogIdx }

data ServerState n = Follower
                   | Leader    { nextIdx      ∷ Map n LogIdx
                               , matchIdx     ∷ Map n LogIdx }
                   | Candidate { votesResponded ∷ Set n
                               , votesGranted   ∷ Set n }
  deriving (Eq)

data Server n e = Server { srvTerm      ∷ Term
                         , srvId       ∷ n
                         , srvPeers     ∷ Set n
                         , srvVotedFor  ∷ Maybe n
                         , srvLog       ∷ Log e
                         , srvState     ∷ ServerState n }

data Message n e body = Message { msgBody   ∷ body n e
                                , msgTerm   ∷ Term
                                , msgSource ∷ n
                                , msgTarget ∷ n }
  deriving (Typeable,Generic)

data AppendEntries n e = AppendEntries { mPrevLogIdx  ∷ LogIdx
                                       , mPrevLogTerm ∷ Term
                                       , mCommitIdx   ∷ LogIdx
                                       , mEntries     ∷ Vector(Entry e) }
  deriving (Typeable,Generic)

data AppendEntriesResp n e = AppendEntriesResp { mMatchIdx ∷ Maybe LogIdx }
  deriving (Typeable,Generic)

data RequestVote n e = RequestVote { mLastLogTerm ∷ Term
                                   , mLastLogIdx  ∷ LogIdx }
  deriving (Typeable,Generic)

data RequestVoteResp n e = RequestVoteResp { mVoteGranted ∷ Bool }
  deriving (Typeable,Generic)

data RPC n e = Append     (AppendEntries n e)
             | AppendResp (AppendEntriesResp n e)
             | Vote       (RequestVote n e)
             | VoteResp   (RequestVoteResp n e)
  deriving (Typeable,Generic)

msgMap ∷ (x1 n e1 → y n e) → Message n e1 x1 → Message n e y
msgMap f m = m { msgBody = f(msgBody m) }

-- The DeriveAnyClass extension would make this boilerplate unnecessary,
-- however, it conflicts with GeneralizedNewtypeDeriving, which we are using
-- for `Term` and `LogIdx`.
instance (Binary n, Binary e, Binary(rpc n e)) => Binary (Message n e rpc)
instance (Binary e)                            => Binary (Entry e)
instance (Binary n, Binary e)                  => Binary (RPC n e)
instance (Binary n, Binary e)                  => Binary (RequestVote n e)
instance (Binary n, Binary e)                  => Binary (RequestVoteResp n e)
instance (Binary n, Binary e)                  => Binary (AppendEntries n e)
instance (Binary n, Binary e)                  => Binary (AppendEntriesResp n e)


-- Utilities -------------------------------------------------------------------

assertM ∷ Monad m ⇒ Bool → m ()
assertM = flip assert $ return ()

isCandidate, isFollower, isLeader ∷ Eq n ⇒ ServerState n → Bool
isCandidate (Candidate _ _) = True
isCandidate _               = False
isFollower                  = (≡Follower)
isLeader    (Leader _ _)    = True
isLeader    _               = False

isQuorum ∷ Server n e → Set n → Bool
isQuorum Server{..} voteSet = 2*(S.size voteSet) > S.size srvPeers

lastElem ∷ Vector e → Maybe e
lastElem v = v V.!? (V.length v - 1)

lastTerm ∷ Log e → Maybe Term
lastTerm = fmap entryTerm . lastElem . entries

lastIdx ∷ Log e → LogIdx
lastIdx = LogIdx . (\x→x-1) . V.length . entries

emptyLog ∷ Log e
emptyLog = Log V.empty (-1)

logSize ∷ Log e → Int
logSize = V.length . entries

dropLastLogEntry ∷ Log e → Log e
dropLastLogEntry (Log es c) = Log (vecUnSnoc es) c

vecUnSnoc ∷ V.Vector x → V.Vector x
vecUnSnoc v = V.take (V.length v - 1) v

emptyCandidate ∷ ServerState n
emptyCandidate = Candidate S.empty S.empty

emptyLeader ∷ Set n → ServerState n
emptyLeader peers = Leader {
  nextIdx  = M.fromSet (const 0) peers,
  matchIdx = M.fromSet (const(-1)) peers }

initialState ∷ n → Set n → Server n e
initialState nodeId peers = Server {
  srvTerm      = 0,
  srvId        = nodeId,
  srvPeers     = peers,
  srvLog       = emptyLog,
  srvVotedFor  = Nothing,
  srvState     = Follower}


-- Transitions -----------------------------------------------------------------

restart ∷ Server n e → Server n e
restart Server{..} = (initialState srvId srvPeers) {
  srvState    = Follower,
  srvTerm     = srvTerm,
  srvLog      = srvLog { commitIdx=(-1) },
  srvVotedFor = srvVotedFor}

timeout ∷ Eq (ServerState n) ⇒ Server n e → Maybe (Server n e)
timeout Server{..} =
  case srvState of
    Leader{..} → Nothing
    _          → Just $ Server {
      srvTerm     = srvTerm + 1,
      srvId       = srvId,
      srvPeers    = srvPeers,
      srvVotedFor = Nothing, -- (Just srvId) would be ok.
      srvLog      = srvLog,
      srvState    = emptyCandidate}

requestVote ∷ (Eq n, Ord n)
            ⇒ Server n e → n → Maybe (Message n e RequestVote)
requestVote Server{..} target = do
  Candidate{..} ← return $ srvState
  guard (target ∉ votesResponded)
  req ← RequestVote <$> lastTerm srvLog <*> Just(lastIdx srvLog)
  return $ Message req srvTerm srvId target

appendEntries ∷ (Eq n, Ord n)
              ⇒ Server n e → n → Maybe (Message n e AppendEntries)
appendEntries Server{..} target = do
  guard $ srvId ≠ target

  Leader{..}  ← return $ srvState

  nextIdxForTarget ← M.lookup target nextIdx
  let numEntries = max 0 $ unLogIdx $ lastEntry - nextIdxForTarget
      lastEntry = min (LogIdx (V.length(entries srvLog)-1))
                      (nextIdxForTarget+1)

  assertM $ numEntries == 0 ∨ numEntries == 1

  let mEntries   = V.slice (unLogIdx nextIdxForTarget) numEntries mEntries
      mCommitIdx = commitIdx srvLog `min` lastEntry
  mPrevLogIdx    ← (\x→x-1) <$> M.lookup target nextIdx
  mPrevLogTerm   ← if mPrevLogIdx<0 then Just(-1) else
                     entryTerm <$> (entries srvLog) V.!? (unLogIdx mPrevLogIdx)

  return $ Message (AppendEntries{..}) srvTerm srvId target

becomeLeader ∷ Server n e → Maybe (Server n e)
becomeLeader srv@Server{..} = do
  Candidate{..} ← return srvState
  guard $ isQuorum srv votesGranted
  let constMap keys v = M.fromSet (const v) keys
  return $ srv { srvState =
    Leader { nextIdx  = constMap srvPeers $ LogIdx(logSize srvLog)
           , matchIdx = constMap srvPeers $ LogIdx(-1) }}

clientRequest ∷ Server n e → e → Maybe (Server n e)
clientRequest srv@Server{..} e = do
  Leader{..} ← return srvState
  return $ srv {
    srvLog = srvLog {
      entries = V.snoc (entries srvLog) (Entry e srvTerm) }}

safeMaximum ∷ Ord a ⇒ [a] → Maybe a
safeMaximum l = if L.null l then Nothing
                            else Just (maximum l)

advanceCommitIdx ∷ Ord n ⇒ Server n e → Maybe (Server n e)
advanceCommitIdx srv@Server{..} = do
  Leader{..} ← return srvState

  let agree idx = S.insert srvId $ M.keysSet $ M.filter (≥idx) matchIdx
  let agreeIdxs = L.filter (isQuorum srv . agree) [0 .. lastIdx srvLog]
  let newCommitIdx = fromMaybe (commitIdx srvLog) $ do
          idx            ← safeMaximum agreeIdxs
          agreedUponTerm ← entryTerm <$> (entries srvLog) V.!? (unLogIdx idx)
          guard $ srvTerm ≡ agreedUponTerm
          return idx
  return $ srv {
    srvLog = srvLog {
      commitIdx = newCommitIdx }}

handleRequestVoteRequest ∷ Eq n
                         ⇒ Server n e
                         → Message n e RequestVote
                         → Maybe (Server n e, Message n e RequestVoteResp)
handleRequestVoteRequest srv@Server{..} Message{..} = do
  let RequestVote{..} = msgBody
  let candidate = msgSource
  lastLogTerm ← lastTerm srvLog
  let logOk = mLastLogTerm > lastLogTerm
            ∧ ( mLastLogTerm ≡ lastLogTerm
              ∨ mLastLogIdx  ≥ lastIdx srvLog )
      grant = msgTerm ≡ srvTerm
            ∧ logOk
            ∧ ( srvVotedFor ≡ Nothing
              ∨ srvVotedFor ≡ Just candidate )
  guard $ msgTerm ≤ srvTerm
  let newVotedFor = if grant then Just candidate
                             else srvVotedFor
  return ( srv { srvVotedFor = newVotedFor }
         , Message {
             msgTerm   = srvTerm,
             msgSource = srvId,
             msgTarget = candidate,
             msgBody   = RequestVoteResp grant })

handleRequestVoteResponse ∷ (Eq n, Ord n)
                          ⇒ Server n e → Message n e RequestVoteResp
                          → Maybe (Server n e)
handleRequestVoteResponse srv@Server{..} Message{..} = do
  let RequestVoteResp granted = msgBody
      voter = msgSource

  Candidate{..} ← return srvState
  guard $ msgTerm   ≡ srvTerm
        ∧ msgTarget ≡ srvId

  return $ srv {
    srvState = Candidate {
      votesResponded = S.insert voter votesResponded,
      votesGranted   = if granted then S.insert voter votesGranted
                                  else votesGranted }}

handleAppendEntriesRequest ∷ (Eq n, Eq e)
                           ⇒ Server n e → Message n e AppendEntries
                           → Maybe (Server n e, Message n e AppendEntriesResp)
handleAppendEntriesRequest srv@Server{..} msg@Message{..} = do
  let AppendEntries{..} = msgBody
      target = mPrevLogIdx + 1 -- The index that we're trying to write to.

  assertM $ V.length mEntries ≤ 1
  guard $ msgTarget ≡ srvId
        ∧ msgTerm ≤ srvTerm

  ourPrevLogTerm ← entryTerm <$> entries srvLog V.!? (unLogIdx mPrevLogIdx)

  let logOk = mPrevLogIdx ≡ (-1)
            ∨ ( mPrevLogIdx > (-1)
              ∧ mPrevLogIdx ≤ lastIdx srvLog
              ∧ mPrevLogTerm ≡ ourPrevLogTerm )

  let acceptableRequest = msgTerm≡srvTerm ∧ isFollower srvState ∧ logOk
      newLeader         = msgTerm≡srvTerm ∧ isCandidate srvState
      invalidRequest    = msgTerm < srvTerm ∨ ( msgTerm ≡ srvTerm
                                              ∧ isFollower srvState
                                              ∧ not logOk )

  let conflictsExist = fromMaybe False $ do
                         guard (target ≤ lastIdx srvLog)
                         targetEntry ← entries srvLog V.!? (unLogIdx target)
                         msgEntry    ← mEntries       V.!? 0
                         return (entryTerm targetEntry ≡ entryTerm msgEntry)

  let noNewEntries = fromMaybe False $ do
                       targetEntry ← entries srvLog V.!? (unLogIdx target)
                       msgEntry    ← mEntries       V.!? 0
                       return (targetEntry ≡ msgEntry)

  let canAppend = not(V.null mEntries) ∧ lastIdx srvLog ≡ mPrevLogIdx

  let response idx   = Message (AppendEntriesResp idx) srvTerm srvId msgSource
      rejectionResp  = response Nothing
      acceptanceResp = response $ Just(mPrevLogIdx + LogIdx(V.length(mEntries)))

  let loop srv'  = handleAppendEntriesRequest srv' msg
      conditions = ( invalidRequest,newLeader,acceptableRequest,noNewEntries
                   , conflictsExist, canAppend )
  case conditions of
    (True ,_,_,_,_,_) → return (srv,rejectionResp)
    (_,True ,_,_,_,_) → loop $ srv{srvState=Follower}
    (_,_,False,_,_,_) → Nothing
    (_,_,_,True ,_,_) → return (srv', acceptanceResp)
                          where srv' = srv {
                            srvLog = srvLog {
                              commitIdx = mCommitIdx }}
    (_,_,_,_,True ,_) → loop $ srv {srvLog = dropLastLogEntry srvLog}
    (_,_,_,_,_,False) → Nothing
    (_,_,_,_,_,True ) → loop $ srv {
                          srvLog = srvLog {
                            entries = (entries srvLog) V.++ mEntries }}

handleAppendEntriesResponse ∷ (Ord n, Eq n)
                            ⇒ Server n e → Message n e AppendEntriesResp
                            → Maybe (Server n e)
handleAppendEntriesResponse srv@Server{..} Message{..} = do
  guard $ msgTerm ≡ srvTerm
  Leader{..} ← return srvState

  let follower = msgSource
  let srvState' = case mMatchIdx msgBody of
        Just idx → Leader
            { nextIdx  = M.insert follower (idx+1) nextIdx
            , matchIdx = M.insert follower idx matchIdx }
        Nothing  → Leader
            { matchIdx = matchIdx
            , nextIdx  = M.adjust (max 0 . subtract 1) follower nextIdx }

  return $ srv { srvState = srvState' }

updateTerm ∷ Server n e → Message n e m → Maybe (Server n e)
updateTerm srv@Server{..} Message{..} = do
  guard $ msgTerm > srvTerm
  return $ srv { srvTerm     = msgTerm
               , srvState    = Follower
               , srvVotedFor = Nothing }

dropStaleResponse ∷ Server n e → Message n e m → Maybe (Message n e m)
dropStaleResponse Server{..} msg@Message{..} =
  if msgTerm < srvTerm
    then Nothing
    else Just msg

receive ∷ (Eq n, Ord n, Eq e)
        ⇒ Server n e → Message n e RPC
        → Maybe (Server n e, Maybe(Message n e RPC))
receive srv rpc = do
  srv ← return $ fromMaybe srv $ updateTerm srv rpc
  let msg body = rpc { msgBody=body }
  case msgBody rpc of
    Append m     → do
        (srv,resp) ← handleAppendEntriesRequest srv (msg m)
        return (srv,Just(msgMap AppendResp resp))

    AppendResp m → do
        m ← dropStaleResponse srv (msg m)
        srv ← handleAppendEntriesResponse srv m
        return (srv, Nothing)

    Vote m → do
        (srv',resp) ← handleRequestVoteRequest srv (msg m)
        return (srv', Just(msgMap VoteResp resp))

    VoteResp m   → do
        m ← dropStaleResponse srv (msg m)
        srv ← handleRequestVoteResponse srv m
        return (srv, Nothing)
