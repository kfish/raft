{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SqliteStore (
    SqliteStore(..)
  , open
) where

import Data.Functor.Identity

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (unless)
import Control.Monad.Free
import Control.Monad.State

import qualified Data.Foldable as Fold
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T

import qualified Database.SQLite.Simple as Sqlite

import qualified Consensus.Raft as CS
import qualified Consensus.Types as CS

----------------------------------------------------------------------

tableExists :: Sqlite.Connection -> String -> IO Bool
tableExists conn tblName = do
    r <- Sqlite.query conn
         "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
         (Sqlite.Only tblName)
    case r of
        [Sqlite.Only (_ :: String)] -> return True
        _ -> return False

createTables :: Sqlite.Connection -> IO ()
createTables conn = do
    schemaCreated <- tableExists conn "kv"
    unless schemaCreated $ do
        Sqlite.execute_ conn (Sqlite.Query $ T.concat
            [ "CREATE TABLE store ("
            , "ix    INTEGER PRIMARY KEY, "
            , "value INTEGER"
            , "term  INTEGER"
            , ")"
            ])
        Sqlite.execute_ conn (Sqlite.Query $ T.concat
            [ "CREATE TABLE meta ("
            , "latestCommit INTEGER"
            , ")"
            ])

----------------------------------------------------------------------

data SqliteStore = SqliteStore {
      ssConnection :: Sqlite.Connection
    }

type SqliteStoreM = StateT SqliteStore IO

open :: IO SqliteStore
open = do
      conn <- Sqlite.open "test.db"
      createTables conn
      return (SqliteStore conn)

runSqliteStore :: (Fold.Foldable t)
               => Free (CS.LogStoreF t Int) () -> SqliteStoreM ()
runSqliteStore (Pure r) = return r
runSqliteStore (Free x) = case x of
    CS.LogQuery ix cont -> do
        SqliteStore conn <- get
        res <- liftIO $ Sqlite.query conn "select from store (value, term) where ix = (?)" [ix]
        runSqliteStore $ cont (second CS.Term <$> listToMaybe res)
    CS.LogStore ix (CS.Term term) xs next -> do
        SqliteStore conn <- get
        liftIO $ mapM_ (\(ixx, x) ->
                           Sqlite.execute conn "insert into store (ix,value,term) values (?)" [ixx, x, term])
                       (zip [ix..] (Fold.toList xs))
        runSqliteStore next
    CS.LogCommit ix next -> do
        SqliteStore conn <- get
        liftIO $ Sqlite.execute conn "insert into meta (latestCommit) values (?)" [ix]
        runSqliteStore next
    CS.LogTruncate ix next -> do
        SqliteStore conn <- get
        liftIO $ do
            res <- Sqlite.query_ conn "select from meta (latestCommit)"
            let [c] = fromMaybe [0] (listToMaybe res)
            Sqlite.execute conn "delete from store where ix > (?)" [min ix c]
        runSqliteStore next
    CS.LogEnd -> return ()

instance CS.MonadStore SqliteStoreM where
    type ValueM SqliteStoreM = Int
    runLogStoreM cmds = runSqliteStore cmds
    valueAtM ix = do
        SqliteStore conn <- get
        res <- liftIO $ Sqlite.query conn "select from store (value, term) where ix = (?)" [ix]
        return (second CS.Term <$> listToMaybe res)

{-
ensureConnection :: SqliteStore -> IO SqliteStore
ensureConnection ss@SqliteStore{..}
  | ssConnection == Nothing = do
      conn <- Sqlite.open "test.db"
      return ss { ssConnection = conn }
  | otherwise = return ss
  -}

{-
instance CS.Store SqliteStore where
    type Value SqliteStore = Int

    -- open = ensureConnection empty

    query ix (SqliteStore conn s _) = return Nothing
{-
        listToMaybe <$> Sqlite.query_ conn
          ("SELECT * FROM kv WHERE key=(?)" (Sqlite.Only ix))
-}

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

-}
