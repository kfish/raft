{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Raft.Store.Volatile (
    VolatileStore(..)
  , empty
) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as S
import Data.Functor.Identity

import Control.Monad.Free
import Control.Monad.State
import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Raft.Protocol as CS
import qualified Raft.Types as CS

----------------------------------------------------------------------

data VolatileStore = VolatileStore {
      tsInternal     :: Map CS.Index (Int, CS.Term)
    , tsLatestCommit :: CS.Index
    }

empty :: VolatileStore
empty = VolatileStore Map.empty 0

----------------------------------------------------------------------

runVolatileStore :: (MonadState VolatileStore m, Fold.Foldable t)
             => Free (CS.LogStoreF t Int) r -> m r
runVolatileStore (Pure r) = return r
runVolatileStore (Free x) = case x of
    CS.LogQuery ix cont -> do
        VolatileStore s c <- get
        let res = Map.lookup ix s
        runVolatileStore $ cont res
    CS.LogStore ix term xs next -> do
        modify $ \(VolatileStore s c) ->
            (\m -> VolatileStore m c) . fst $ Fold.foldl' (\(m, ixx) x -> (Map.insert ixx (x,term) m, ixx+1)) (s, ix) xs
        runVolatileStore next
    CS.LogCommit ix next -> do
        modify $ \ts -> ts { tsLatestCommit = ix }
        runVolatileStore next
    CS.LogTruncate ix next -> do
        modify $ \(VolatileStore s c) ->
            VolatileStore (fst (Map.split ix s)) (min ix c)
        runVolatileStore next

instance CS.Store VolatileStore where
    type Value VolatileStore = Int

instance CS.StoreIO VolatileStore where
    interpret cmds s = do
        (r, s') <- runStateT (runVolatileStore cmds) s
        return (s', r)
