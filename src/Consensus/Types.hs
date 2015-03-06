{-# LANGUAGE TypeFamilies #-}

module Consensus.Types (
      Identifier
    , Index
    , Term(..)

    , Store(..)
    , Protocol(..)
) where

import Data.Foldable (Foldable)

----------------------------------------------------------------------

type Identifier = Int

type Index = Int

----------------------------------------------------------------------
-- Term

newtype Term = Term Int
    deriving (Eq, Ord)

class HasTerm a where
    termOf :: a -> Term

----------------------------------------------------------------------

class Node a where 

    identify :: a -> Identifier

    receive :: Monad m => a -> m (a, Request b)

    send :: Monad m => Response b -> a -> m a

class Store s where

    type Value s :: *

    -- | Query the value at a given index
    query :: Monad m => Index -> s -> m (Maybe (Value s, Term))

    -- | Store a value at a given index
    store :: (Foldable t, Monad m) =>
        Int -> t (Value s) -> Term -> s -> m s

    commit :: Monad m => Int -> s -> m s

    -- | Delete a given entry and all that follow it
    truncate :: Monad m => Int -> s -> m s


class Protocol p where

    data Request p :: *

    data Response p :: *

    step :: p -> Request p -> (p, Response p)
