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


data StoreF k v next
    = Query k (v -> next)
    | Store k v next
    | End

instance Functor (StoreF k v) where
    fmap f (Query k cont)   = Query k (f . cont)
    fmap f (Store k v next) = Store k v (f next)
    fmap f End              = End

query' :: MonadFree (StoreF k v) m => k -> m v
query' k = liftF (Query k id)

store' :: MonadFree (StoreF k v) m => k -> v -> m ()
store' k v = liftF (Store k v ())
