{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-

https://www.usenix.org/system/files/conference/atc14/atc14-paper-ongaro.pdf

-}

module Consensus.Raft (

) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Free
import Control.Monad (when)
import Data.Serialize
import Data.Map (Map)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold

import Network.Protocol

import qualified Consensus.Types as Consensus

----------------------------------------------------------------------

data RaftPersistentState s = RaftPersistentState
    {

    -- | Latest term server has seen (initialized to 0 on first boot,
    -- increases monotonically
      currentTerm :: Consensus.Term

    -- candidateId that received vote in current term (or Nothing if none)
    , votedFor :: Maybe Consensus.Identifier

    -- log entries; each entry contains command for state machine, and term
    -- when entry was received by leader (first index is 1)
    , log :: s
    }

data RaftVolatileState = RaftVolatileState
    {

    -- | Index of highest log entry known to be committed (initialized to 0,
    -- increases monotonically)
      commitIndex :: Consensus.Index

    -- | Index of highest log entry applied to state machine (initalized to 0,
    -- increases monotonically)
    , lastApplied :: Consensus.Index
    }

data RaftLeaderVolatileState = RaftLeaderVolatileState
    {

    -- For each server, index of the next log entry to send to that server
    -- (initialized to leader last log index + 1)
      nextIndex :: Map Consensus.Identifier Int

    -- For each server, index of highest log entry known to be replicated on
    -- server (initialized to 0, increases monotonically)
    , matchIndex :: Map Consensus.Identifier Int
    }

data Raft s = RaftLeader (RaftPersistentState s) RaftVolatileState RaftLeaderVolatileState
            | RaftFollower (RaftPersistentState s) RaftVolatileState
            | RaftCandidate (RaftPersistentState s) RaftVolatileState

pstate :: Raft s -> RaftPersistentState s
pstate (RaftLeader rps _ _) = rps
pstate (RaftFollower rps _) = rps
pstate (RaftCandidate rps _) = rps

----------------------------------------------------------------------
-- RPC
--

data AppendEntries s = AppendEntries
    {
    -- Leader's term
      aeTerm :: Consensus.Term

    -- So follower can redirect clients
    , leaderId :: Consensus.Identifier

    -- Index of log entry immediately preceding new ones
    , prevLogIndex :: Consensus.Index

    -- Term of prevLogIndex entry
    , prevLogTerm :: Consensus.Term

    -- Log entries to store (empty for heartbeat, may send more than one
    -- for efficiency
    -- , entries :: t (Consensus.Value s)
    , entries :: [Consensus.Value s]

    -- Leader's commitIndex
    , leaderCommit :: Consensus.Index
    }

instance ( Consensus.Store s
         , Serialize (Consensus.Value s)
         ) => Serialize (AppendEntries s) where
    put AppendEntries{..} = do
      put aeTerm
      put leaderId
      put prevLogIndex
      put prevLogTerm
      Fold.mapM_ put entries
      put leaderCommit
    get = AppendEntries <$> get <*> get <*> get <*> get <*> get <*> get

data AppendEntriesResponse = AppendEntriesResponse
    {
    -- currentTerm, for leader to update itself
      aerTerm :: Consensus.Term

    -- True if follower contained entry matching prevLogIndex and prevLogTerm
    , aerSuccess :: Bool
    }

instance Serialize AppendEntriesResponse where
    put AppendEntriesResponse{..} = do
      put aerTerm
      put aerSuccess
    get = AppendEntriesResponse <$> get <*> get

data RequestVote = RequestVote
    {
    -- Candidate's term
      rvTerm :: Consensus.Term

    -- Candidate requesting vote
    , candidateId :: Consensus.Identifier

    -- Index of candidate's last log entry
    , lastLogIndex :: Consensus.Index

    -- Term of candidate's last log entry
    , lastLogTerm :: Consensus.Term
    }

instance Serialize RequestVote where
    put RequestVote{..} = do
      put rvTerm
      put candidateId
      put lastLogIndex
      put lastLogTerm
    get = RequestVote <$> get <*> get <*> get <*> get

data RequestVoteResponse = RequestVoteResponse
    {
    -- currentTerm, for candidate to update itself
      rvrTerm :: Consensus.Term

    -- True means candidate received vote
    , voteGranted :: Bool
    }

instance Serialize RequestVoteResponse where
    put RequestVoteResponse{..} = do
      put rvrTerm
      put voteGranted
    get = RequestVoteResponse <$> get <*> get

----------------------------------------------------------------------

data RaftRequest s = AE (AppendEntries s)
                   | RV RequestVote

data RaftResponse = AER AppendEntriesResponse
                  | RVR RequestVoteResponse

instance (Consensus.Store s) => Protocol (Raft s) where
    type Request (Raft s) = RaftRequest s

    type Response (Raft s) = RaftResponse

    type Effects (Raft s) = Free (Consensus.LogStoreF [] (Consensus.Value s))

    step receiver (AE AppendEntries{..})
        -- Reply False if term < currentTerm
        | aeTerm < currentTerm =
              return (receiver, Just . AER$ AppendEntriesResponse currentTerm False)

        | otherwise = do

            -- Reply False if log doesn't contain an entry at prevLogIndex
            -- whose term matches prevLogTerm

            t <- fmap snd <$> Consensus.query' prevLogIndex

            if (t /= Just prevLogTerm)
              then return (receiver, Just . AER$ AppendEntriesResponse currentTerm False)
              else do

            -- If an existing entry conflicts with a new one (same index but
            -- different terms), delete the existing entry and all that
            -- follow it.
                  when (t /= Just aeTerm) $ Consensus.truncate' prevLogIndex

                  -- Append any new entries not already in the log
                  Consensus.store' (prevLogIndex+1) aeTerm entries
{-
        -- If leaderCommit > commitIndex, set commitIndex = min (leaderCommit, index of last new entry)

-}
                  return (receiver, Just . AER$ AppendEntriesResponse aeTerm True)
      where
        RaftPersistentState{..} = pstate receiver

        -- match prevLogIndex s Nothing = False
        -- match prevLogIndex s (Just (v, t)) = t == prevLogIndex

    -- Follower receiving RequestVote
    step receiver@(RaftFollower p@RaftPersistentState{..} vol) (RV RequestVote{..})
        -- Reply False if term < currentTerm
        | rvTerm < currentTerm
          = return (receiver, Just. RVR$ RequestVoteResponse currentTerm False)

        -- If votedFor is null or candidateId, and candidate's log is at
        -- least as up-to-date as receiver's log, grant vote
        | (votedFor == Nothing || votedFor == Just candidateId)
          && lastLogTerm <= currentTerm
          = return (RaftFollower granted vol, Just . RVR$ RequestVoteResponse rvTerm True)
      where
        granted = p { votedFor = Just candidateId }

    -- Leader or Candidate receiving RequestVote

    -- ??? If a server that is not a Follower receives a RequestVote, return False
    -- ??? what term to return? update volatile term?
    step receiver (RV _)
      = return (receiver, Just . RVR$ RequestVoteResponse (currentTerm (pstate receiver)) False)
