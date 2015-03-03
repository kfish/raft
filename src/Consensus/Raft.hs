{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-

https://www.usenix.org/system/files/conference/atc14/atc14-paper-ongaro.pdf

-}

module Consensus.Raft (
) where

import Data.Map (Map)
import Data.Traversable (Traversable)

import qualified Consensus.Types as Consensus

----------------------------------------------------------------------
-- Term

newtype Term = Term Int
    deriving (Eq, Ord)

class HasTerm a where
    termOf :: a -> Term

----------------------------------------------------------------------

newtype Index = Index Int

data RaftPersistentState s = RaftPersistentState
    {

    -- | Latest term server has seen (initialized to 0 on first boot,
    -- increases monotonically
      currentTerm :: Term

    -- candidateId that received vote in current term (or Nothing if none)
    , votedFor :: Maybe Consensus.Identifier

    -- log entries; each entry contains command for state machine, and term
    -- when entry was received by leader (first index is 1)
    , log :: Consensus.Store s => s
    }

data RaftVolatileState = RaftVolatileState
    {

    -- | Index of highest log entry known to be committed (initialized to 0,
    -- increases monotonically)
      commitIndex :: Int

    -- | Index of highest log entry applied to state machine (initalized to 0,
    -- increases monotonically)
    , lastApplied :: Int
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

data Raft a = RaftLeader (RaftPersistentState a) RaftVolatileState RaftLeaderVolatileState
            | RaftFollower (RaftPersistentState a) RaftVolatileState
            | RaftCandidate (RaftPersistentState a) RaftVolatileState

pstate :: Raft a -> RaftPersistentState a
pstate (RaftLeader rps _ _) = rps
pstate (RaftFollower rps _) = rps
pstate (RaftCandidate rps _) = rps

----------------------------------------------------------------------
-- RPC
--

data AppendEntries a = AppendEntries
    {
    -- Leader's term
      aeTerm :: Term

    -- So follower can redirect clients
    , leaderId :: Consensus.Identifier

    -- Index of log entry immediately preceding new ones
    , prevLogIndex :: Index

    -- Term of prevLogIndex entry
    , prevLogTerm :: Term

    -- Log entries to store (empty for heartbeat, may send more than one
    -- for efficiency
    , entries :: Traversable t => t a

    -- Leader's commitIndex
    , leaderCommit :: Index
    }

data AppendEntriesResponse = AppendEntriesResponse
    {
    -- currentTerm, for leader to update itself
      aerTerm :: Term

    -- True if follower contained entry matching prevLogIndex and prevLogTerm
    , aerSuccess :: Bool
    }

data RequestVote = RequestVote
    {
    -- Candidate's term
      rvTerm :: Term

    -- Candidate requesting vote
    , candidateId :: Consensus.Identifier

    -- Index of candidate's last log entry
    , lastLogIndex :: Index

    -- Term of candidate's last log entry
    , lastLogTerm :: Term
    }

data RequestVoteResponse = RequestVoteResponse
    {
    -- currentTerm, for candidate to update itself
      rvrTerm :: Term

    -- True means candidate received vote
    , voteGranted :: Bool
    }

----------------------------------------------------------------------

instance Consensus.Protocol (Raft a) where
    data Request (Raft a) = AE (AppendEntries a)
                          | RV RequestVote

    data Response (Raft a) = AER AppendEntriesResponse
                           | RVR RequestVoteResponse

    step raft (AE AppendEntries{..})
        -- Reply False if term < currentTerm
        | aeTerm < term = (raft, RVR$ RequestVoteResponse term False)

        -- Reply False if log doesn't contain an entry at prevLogIndex
        -- whose term matches prevLogTerm
        -- | 

        -- If an existing entry conflicts with a new one (same index but
        -- different terms), delete the existing entry and all that
        -- follow it.

        -- Append any new entries not already in the log

        -- If leaderCommit > commitIndex, set commitIndex = min (leaderCommit, index of last new entry)
      where
        term = currentTerm (pstate raft)

    step raft (RV RequestVote{..})
        -- Reply False if term < currentTerm
        | rvTerm < term = (raft, RVR$ RequestVoteResponse term False)

        -- If votedFor is null or candidateId, and candidate's log is at
        -- least as up-to-date as receiver's log, grant vote
        | (vf == Nothing || vf == Just candidateId) &&
          undefined
          = (raft, RVR$ RequestVoteResponse rvTerm True)
      where
        term = currentTerm (pstate raft)
        vf = votedFor (pstate raft)
