{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TestStore (
    TestStore(..)
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

data TestStore = TestStore {
      tsInternal :: Map CS.Index (Int, CS.Term)
    , tsLatestCommit :: CS.Index
    }

empty :: TestStore
empty = TestStore Map.empty 0

----------------------------------------------------------------------

runTestStore :: (MonadState TestStore m, Fold.Foldable t)
             => Free (CS.LogStoreF t Int) r -> m r
runTestStore (Pure r) = return r
runTestStore (Free x) = case x of
    CS.LogQuery ix cont -> do
        TestStore s c <- get
        let res = Map.lookup ix s
        runTestStore $ cont res
    CS.LogStore ix term xs next -> do
        modify $ \(TestStore s c) ->
            (\m -> TestStore m c) . fst $ Fold.foldl' (\(m, ixx) x -> (Map.insert ixx (x,term) m, ixx+1)) (s, ix) xs
        runTestStore next
    CS.LogCommit ix next -> do
        modify $ \ts -> ts { tsLatestCommit = ix }
        runTestStore next
    CS.LogTruncate ix next -> do
        modify $ \(TestStore s c) ->
            TestStore (fst (Map.split ix s)) (min ix c)
        runTestStore next

instance CS.Store TestStore where
    type Value TestStore = Int

instance CS.StoreIO TestStore where
    interpret cmds s = do
        (r, s') <- runStateT (runTestStore cmds) s
        return (s', r)
