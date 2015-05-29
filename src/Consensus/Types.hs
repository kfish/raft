{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Consensus.Types (
      Identifier
    , Index
    , Term(..)

    , Store(..)
    , StoreIO(..)

    , LogStoreF(..)
    , query'
    , store'
    , commit'
    , truncate'
) where

import Control.Applicative ((<$>))
import Control.Monad.Free
import Control.Monad.Trans (MonadIO)
import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Data.Serialize

import Network.Protocol

----------------------------------------------------------------------

type Identifier = Int

type Index = Int

----------------------------------------------------------------------
-- Term

newtype Term = Term Int
    deriving (Show, Eq, Ord)

instance Serialize Term where
    put (Term x) = put x
    get = Term <$> get

class HasTerm a where
    termOf :: a -> Term

----------------------------------------------------------------------

{-
class Node a where 

    identify :: a -> Identifier

    receive :: Monad m => a -> m (a, Request b)

    send :: Monad m => Response b -> a -> m a
    -}

data LogStoreF t entry next
    -- | Query the value at a given index
    = LogQuery Index (Maybe (entry, Term) -> next)
    -- | Store a value at a given index
    | LogStore Index Term (t entry) next
    | LogCommit Index next
    -- | Delete a given entry and all that follow it
    | LogTruncate Index next

instance Functor (LogStoreF t entry) where
    fmap f (LogQuery ix cont)              = LogQuery ix (f . cont)
    fmap f (LogStore ix term entries next) = LogStore ix term entries (f next)
    fmap f (LogCommit ix next)             = LogCommit ix (f next)
    fmap f (LogTruncate ix next)           = LogTruncate ix (f next)

query' :: MonadFree (LogStoreF t entry) m => Index -> m (Maybe (entry, Term))
query' ix = liftF (LogQuery ix id)

store' :: MonadFree (LogStoreF t entry) m => Index -> Term -> t entry -> m ()
store' ix term entries = liftF (LogStore ix term entries ())

commit' :: MonadFree (LogStoreF t entry) m => Index -> m ()
commit' ix = liftF (LogCommit ix ())

truncate' :: MonadFree (LogStoreF t entry) m => Index -> m ()
truncate' ix = liftF (LogTruncate ix ())

class Store s where
    type Value s :: *

class Store s => StoreIO s where
    interpret :: Free (LogStoreF [] (Value s)) r -> s -> IO (s, r)
