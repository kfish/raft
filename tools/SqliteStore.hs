{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module SqliteStore (
    SqliteStore(..)
  , empty
  , sqliteQuery
  , sqliteStore
  , sqliteCommit
  , sqliteTruncate
) where

import Data.Functor.Identity

import Control.Applicative ((<$>))

import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)

import qualified Database.SQLite.Simple as Sqlite

import qualified Consensus.Raft as CS
import qualified Consensus.Types as CS

----------------------------------------------------------------------

data SqliteStore = SqliteStore {
      ssConnection :: Sqlite.Connection
    , tsInternal :: Map CS.Index (Int, CS.Term)
    , tsLasqliteCommit :: CS.Index
    }

{-
ensureConnection :: SqliteStore -> IO SqliteStore
ensureConnection ss@SqliteStore{..}
  | ssConnection == Nothing = do
      conn <- Sqlite.open "test.db"
      return ss { ssConnection = conn }
  | otherwise = return ss
  -}

instance CS.Store SqliteStore where
    type Value SqliteStore = Int

    -- open = ensureConnection empty

    query ix (SqliteStore conn s _) = do
        listToMaybe <$> Sqlite.query_ conn
          ("SELECT * FROM kv WHERE key=(?)" (Sqlite.Only ix))

    store ix xs term (SqliteStore conn s c) =
        return . (\m -> SqliteStore conn m c) . fst $
        Fold.foldl' (\(m, ixx) x -> (Map.insert ixx (x,term) m, ixx+1))
          (s, ix) xs

    commit ix (SqliteStore conn s _) = return (SqliteStore conn s ix)

    truncate ix (SqliteStore conn s c) = return $ SqliteStore conn
        (fst (Map.split ix s)) (min ix c)

empty :: SqliteStore
empty = SqliteStore undefined Map.empty 0

sqliteQuery :: CS.Index -> SqliteStore -> Maybe (Int, CS.Term)
sqliteQuery ix ts = runIdentity $ CS.query ix ts

sqliteStore :: CS.Index -> [Int] -> CS.Term -> SqliteStore -> SqliteStore
sqliteStore ix xs term ts = runIdentity $ CS.store ix xs term ts

sqliteCommit :: CS.Index -> SqliteStore -> SqliteStore
sqliteCommit ix ts = runIdentity $ CS.commit ix ts

sqliteTruncate :: CS.Index -> SqliteStore -> SqliteStore
sqliteTruncate ix ts = runIdentity $ CS.truncate ix ts

