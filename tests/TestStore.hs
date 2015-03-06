{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TestStore (
    TestStore(..)
) where

import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map

import Consensus.Raft
import Consensus.Types

data TestStore = TestStore {
      tsInternal :: Map Index (Int, Term)
    , tsLatestCommit :: Index
    }

instance Store TestStore where
    type Value TestStore = Int

    query ix (TestStore s _) = return (Map.lookup ix s)

    store ix xs term (TestStore s c) =
        return . (\m -> TestStore m c) . fst $
        Fold.foldl' (\(m, ixx) x -> (Map.insert ixx (x,term) m, ixx+1))
          (s, ix) xs

    commit ix (TestStore s _) = return (TestStore s ix)

    truncate ix (TestStore s c) = return $ TestStore
        (fst (Map.split ix s)) (min ix c)
