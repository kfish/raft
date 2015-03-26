{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Consensus.Types (
      Identifier
    , Index
    , Term(..)

    , Store(..)
) where

import Control.Applicative ((<$>))
import Control.Monad.Free
import Data.Serialize
import Data.Foldable (Foldable)

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

class Store s where

    type Value s :: *

    -- | Query the value at a given index
    query :: Monad m => Index -> s -> m (Maybe (Value s, Term))

    -- | Store a value at a given index
    store :: (Foldable t, Monad m) =>
        Int -> t (Value s) -> Term -> s -> m s

    commit :: Monad m => Index -> s -> m s

    -- | Delete a given entry and all that follow it
    truncate :: Monad m => Index -> s -> m s


data LogStoreF term entry next
    = LogQuery Index ((entry, term) -> next)
    | LogStore Index entry term next
    | LogCommit Index next
    | LogEnd

instance Functor (LogStoreF term entry) where
    fmap f (LogQuery ix cont)            = LogQuery ix (f . cont)
    fmap f (LogStore ix entry term next) = LogStore ix entry term (f next)
    fmap f LogEnd                        = LogEnd

query' :: MonadFree (LogStoreF term entry) m => Index -> m (entry, term)
query' ix = liftF (LogQuery ix id)

store' :: MonadFree (LogStoreF term entry) m => Index -> entry -> term -> m ()
store' ix entry term = liftF (LogStore ix entry term ())
