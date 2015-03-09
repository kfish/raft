{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TestStore (
    TestStore(..)
  , empty
  , testQuery
  , testStore
  , testCommit
  , testTruncate
) where

import Data.Functor.Identity

import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Consensus.Raft as CS
import qualified Consensus.Types as CS

----------------------------------------------------------------------

data TestStore = TestStore {
      tsInternal :: Map CS.Index (Int, CS.Term)
    , tsLatestCommit :: CS.Index
    }

instance CS.Store TestStore where
    type Value TestStore = Int

    query ix (TestStore s _) = return (Map.lookup ix s)

    store ix xs term (TestStore s c) =
        return . (\m -> TestStore m c) . fst $
        Fold.foldl' (\(m, ixx) x -> (Map.insert ixx (x,term) m, ixx+1))
          (s, ix) xs

    commit ix (TestStore s _) = return (TestStore s ix)

    truncate ix (TestStore s c) = return $ TestStore
        (fst (Map.split ix s)) (min ix c)

empty :: TestStore
empty = TestStore Map.empty 0

testQuery :: CS.Index -> TestStore -> Maybe (Int, CS.Term)
testQuery ix ts = runIdentity $ CS.query ix ts

testStore :: CS.Index -> [Int] -> CS.Term -> TestStore -> TestStore
testStore ix xs term ts = runIdentity $ CS.store ix xs term ts

testCommit :: CS.Index -> TestStore -> TestStore
testCommit ix ts = runIdentity $ CS.commit ix ts

testTruncate :: CS.Index -> TestStore -> TestStore
testTruncate ix ts = runIdentity $ CS.truncate ix ts
