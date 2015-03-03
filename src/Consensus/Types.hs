{-# LANGUAGE TypeFamilies #-}

module Consensus.Types (
    Protocol(..)

    , Identifier
) where

type Identifier = Int

class Node a where 

    identify :: a -> Identifier

    receive :: Monad m => a -> m (a, Request b)

    send :: Monad m => Response b -> a -> m a

class Store s where

    data Value s :: *

    -- | Query the value at a given index
    query :: Monad m => Int -> s -> m (Value s)

    -- | Store a value at a given index
    store :: Monad m => Int -> Value s -> s -> m s

    -- | Delete a given entry and all that follow it
    truncate :: Monad m => Int -> s -> m s


class Protocol p where

    data Request p :: *

    data Response p :: *

    step :: p -> Request p -> (p, Response p)
